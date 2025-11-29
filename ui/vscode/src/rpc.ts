import { spawn, ChildProcess } from 'child_process';
import * as vscode from 'vscode';
import { getConfig } from './config';

interface JsonRpcRequest {
  jsonrpc: '2.0';
  method: string;
  params: object;
  id: number;
}

interface JsonRpcResponse {
  jsonrpc: '2.0';
  id: number;
  result?: unknown;
  error?: {
    code: number;
    message: string;
  };
}

type Callback = (error: Error | null, result: unknown) => void;

export class RpcClient {
  private process: ChildProcess | null = null;
  private requestId = 0;
  private pending: Map<number, Callback> = new Map();
  private buffer = '';
  private outputChannel: vscode.OutputChannel;
  private restartAttempts = 0;
  private maxRestartAttempts = 3;

  constructor() {
    this.outputChannel = vscode.window.createOutputChannel('Hemis');
  }

  async start(): Promise<boolean> {
    if (this.process) {
      return true;
    }

    const backend = getConfig().backend;
    if (!backend) {
      vscode.window.showErrorMessage('Hemis: Backend path not configured. Set hemis.backend in settings.');
      return false;
    }

    const env: Record<string, string | undefined> = { ...process.env };
    const dbPath = getConfig().databasePath;
    if (dbPath) {
      env['HEMIS_DB_PATH'] = dbPath;
    }

    try {
      this.process = spawn(backend, [], {
        stdio: ['pipe', 'pipe', 'pipe'],
        env,
      });

      this.process.stdout?.on('data', (data: Buffer) => {
        this.onData(data.toString());
      });

      this.process.stderr?.on('data', (data: Buffer) => {
        this.outputChannel.appendLine(`[stderr] ${data.toString()}`);
      });

      this.process.on('exit', (code) => {
        this.outputChannel.appendLine(`Backend exited with code ${code}`);
        this.onExit();
      });

      this.process.on('error', (err) => {
        this.outputChannel.appendLine(`Backend error: ${err.message}`);
        vscode.window.showErrorMessage(`Hemis backend error: ${err.message}`);
      });

      this.outputChannel.appendLine('Backend started');
      this.restartAttempts = 0;
      return true;
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      vscode.window.showErrorMessage(`Failed to start Hemis backend: ${message}`);
      return false;
    }
  }

  stop(): void {
    if (!this.process) {
      return;
    }

    // Clear pending requests
    for (const [id, callback] of this.pending) {
      callback(new Error('Backend stopped'), null);
    }
    this.pending.clear();
    this.buffer = '';

    // Send shutdown and kill
    try {
      const request: JsonRpcRequest = {
        jsonrpc: '2.0',
        method: 'shutdown',
        params: {},
        id: 0,
      };
      this.sendRaw(request);
    } catch {
      // Ignore errors during shutdown
    }

    setTimeout(() => {
      if (this.process) {
        this.process.kill();
        this.process = null;
      }
    }, 100);
  }

  async request<T>(method: string, params: object = {}): Promise<T> {
    if (!await this.start()) {
      throw new Error('Backend not running');
    }

    return new Promise((resolve, reject) => {
      const id = ++this.requestId;
      const request: JsonRpcRequest = {
        jsonrpc: '2.0',
        method,
        params,
        id,
      };

      this.pending.set(id, (error, result) => {
        if (error) {
          reject(error);
        } else {
          resolve(result as T);
        }
      });

      this.sendRaw(request);
      this.outputChannel.appendLine(`[request ${id}] ${method}`);
    });
  }

  isRunning(): boolean {
    return this.process !== null;
  }

  private sendRaw(request: JsonRpcRequest): void {
    if (!this.process?.stdin) {
      throw new Error('Backend not running');
    }

    const body = JSON.stringify(request);
    const message = `Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`;
    this.process.stdin.write(message);
  }

  private onData(data: string): void {
    this.buffer += data;

    while (true) {
      const headerEnd = this.buffer.indexOf('\r\n\r\n');
      if (headerEnd === -1) {
        break;
      }

      const header = this.buffer.substring(0, headerEnd);
      const match = header.match(/Content-Length:\s*(\d+)/i);
      if (!match) {
        // Invalid header, skip to next potential header
        this.buffer = this.buffer.substring(headerEnd + 4);
        continue;
      }

      const length = parseInt(match[1], 10);
      const bodyStart = headerEnd + 4;
      const bodyEnd = bodyStart + length;

      if (this.buffer.length < bodyEnd) {
        // Need more data
        break;
      }

      const body = this.buffer.substring(bodyStart, bodyEnd);
      this.buffer = this.buffer.substring(bodyEnd);

      try {
        const response: JsonRpcResponse = JSON.parse(body);
        this.handleResponse(response);
      } catch (err) {
        this.outputChannel.appendLine(`Failed to parse response: ${body}`);
      }
    }
  }

  private handleResponse(response: JsonRpcResponse): void {
    const callback = this.pending.get(response.id);
    if (!callback) {
      this.outputChannel.appendLine(`No callback for response ${response.id}`);
      return;
    }

    this.pending.delete(response.id);

    if (response.error) {
      callback(new Error(response.error.message), null);
    } else {
      callback(null, response.result);
    }
  }

  private onExit(): void {
    // Process any remaining buffered responses
    while (this.buffer.length > 0) {
      const headerEnd = this.buffer.indexOf('\r\n\r\n');
      if (headerEnd === -1) {
        break;
      }

      const header = this.buffer.substring(0, headerEnd);
      const match = header.match(/Content-Length:\s*(\d+)/i);
      if (!match) {
        break;
      }

      const length = parseInt(match[1], 10);
      const bodyStart = headerEnd + 4;
      const bodyEnd = bodyStart + length;

      if (this.buffer.length < bodyEnd) {
        break;
      }

      const body = this.buffer.substring(bodyStart, bodyEnd);
      this.buffer = this.buffer.substring(bodyEnd);

      try {
        const response: JsonRpcResponse = JSON.parse(body);
        this.handleResponse(response);
      } catch {
        break;
      }
    }

    this.process = null;
    this.buffer = '';

    // Fail remaining pending requests
    for (const [id, callback] of this.pending) {
      callback(new Error('Backend exited'), null);
    }
    this.pending.clear();

    // Auto-restart if we haven't exceeded attempts
    if (this.restartAttempts < this.maxRestartAttempts) {
      this.restartAttempts++;
      this.outputChannel.appendLine(`Attempting restart (${this.restartAttempts}/${this.maxRestartAttempts})`);
      setTimeout(() => this.start(), 1000);
    }
  }

  dispose(): void {
    this.stop();
    this.outputChannel.dispose();
  }
}

// Singleton instance
let client: RpcClient | null = null;

export function getRpcClient(): RpcClient {
  if (!client) {
    client = new RpcClient();
  }
  return client;
}

export function disposeRpcClient(): void {
  if (client) {
    client.dispose();
    client = null;
  }
}
