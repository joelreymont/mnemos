import { spawn } from 'child_process';
import * as net from 'net';
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { getConfig } from './config';
import { debug, debugVerbose } from './debug';

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
  private socket: net.Socket | null = null;
  private requestId = 0;
  private pending: Map<number, Callback> = new Map();
  private buffer = '';
  private outputChannel: vscode.OutputChannel;
  private connecting = false;

  constructor() {
    this.outputChannel = vscode.window.createOutputChannel('Hemis');
  }

  private getSocketPath(): string {
    return path.join(getConfig().hemisDir, 'hemis.sock');
  }

  private getLockPath(): string {
    return path.join(getConfig().hemisDir, 'hemis.lock');
  }

  private getLogPath(): string {
    return path.join(getConfig().hemisDir, 'hemis.log');
  }

  private socketExists(): boolean {
    return fs.existsSync(this.getSocketPath());
  }

  /**
   * Check log file for startup errors and return user-friendly message, or null.
   */
  private checkStartupError(): string | null {
    const logPath = this.getLogPath();
    if (!fs.existsSync(logPath)) {
      return null;
    }
    try {
      const content = fs.readFileSync(logPath, 'utf-8');
      const lines = content.split('\n');
      const tail = lines.slice(-10).join('\n');

      if (tail.includes('newer than this version') || tail.includes('schema version')) {
        return `Database schema is incompatible.

Your database was created by a newer version of Hemis.
Please upgrade Hemis or use a different database file.

To use a fresh database:
  rm ~/.hemis/hemis.db

Check ${logPath} for details.`;
      }
      if (tail.includes('Error:') || tail.includes('FATAL') || tail.includes('panic')) {
        return `Backend failed to start.

Check ${logPath} for details:
${tail}`;
      }
    } catch {
      // Ignore read errors
    }
    return null;
  }

  private readLockPid(): number | null {
    const lockPath = this.getLockPath();
    if (!fs.existsSync(lockPath)) {
      return null;
    }
    try {
      const content = fs.readFileSync(lockPath, 'utf8');
      const pid = parseInt(content.split('\n')[0], 10);
      return isNaN(pid) ? null : pid;
    } catch {
      return null;
    }
  }

  private processAlive(pid: number): boolean {
    try {
      process.kill(pid, 0);
      return true;
    } catch {
      return false;
    }
  }

  private async startServer(): Promise<void> {
    const config = getConfig();
    const backend = config.backend;
    if (!backend) {
      throw new Error('Backend path not configured. Set hemis.backend in settings.');
    }

    // Ensure hemis dir exists
    if (!fs.existsSync(config.hemisDir)) {
      fs.mkdirSync(config.hemisDir, { recursive: true });
    }

    const env: Record<string, string | undefined> = { ...process.env };
    env['HEMIS_DIR'] = config.hemisDir;
    if (config.databasePath) {
      env['HEMIS_DB_PATH'] = config.databasePath;
    }

    const logPath = this.getLogPath();
    const logStream = fs.createWriteStream(logPath, { flags: 'a' });

    const proc = spawn(backend, ['--serve'], {
      detached: true,
      stdio: ['ignore', logStream, logStream],
      env,
    });

    proc.unref();
    this.outputChannel.appendLine('Started Hemis server');
  }

  private async waitForSocket(timeoutMs: number): Promise<boolean> {
    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      if (this.socketExists()) {
        return true;
      }
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    return false;
  }

  private connectToSocket(): Promise<net.Socket> {
    return new Promise((resolve, reject) => {
      const socket = net.createConnection(this.getSocketPath());

      socket.on('connect', () => {
        resolve(socket);
      });

      socket.on('error', (err) => {
        reject(err);
      });
    });
  }

  async start(): Promise<boolean> {
    if (this.socket) {
      return true;
    }

    if (this.connecting) {
      // Wait for current connection attempt
      await new Promise(resolve => setTimeout(resolve, 100));
      return this.socket !== null;
    }

    this.connecting = true;
    debug('Connection: starting');

    try {
      const config = getConfig();

      // Try to connect to existing socket
      if (this.socketExists()) {
        debug('Connection: socket exists, attempting connect');
        try {
          const socket = await this.connectToSocket();
          this.setupSocket(socket);
          debug('Connection: connected to existing server');
          this.outputChannel.appendLine('Connected to existing Hemis server');
          return true;
        } catch {
          // Connection failed, might be stale socket
          const pid = this.readLockPid();
          if (pid && !this.processAlive(pid)) {
            // Stale socket, clean up
            debug(`Connection: stale socket (pid ${pid} dead), cleaning up`);
            this.outputChannel.appendLine('Cleaning up stale socket');
            try {
              fs.unlinkSync(this.getSocketPath());
            } catch { /* ignore */ }
            try {
              fs.unlinkSync(this.getLockPath());
            } catch { /* ignore */ }
          }
        }
      }

      // Need to start server
      if (!config.backend) {
        debug('Connection: no backend configured');
        vscode.window.showErrorMessage('Hemis: Backend path not configured. Set hemis.backend in settings.');
        return false;
      }

      debug(`Connection: starting server from ${config.backend}`);
      await this.startServer();

      // Wait for socket to appear
      if (!await this.waitForSocket(5000)) {
        debug('Connection: timeout waiting for socket');
        // Check log for startup errors (e.g., schema version mismatch)
        const startupErr = this.checkStartupError();
        if (startupErr) {
          throw new Error(startupErr);
        }
        throw new Error('Server failed to create socket');
      }

      // Small delay for server to be ready
      await new Promise(resolve => setTimeout(resolve, 100));

      // Connect
      const socket = await this.connectToSocket();
      this.setupSocket(socket);
      debug('Connection: connected to new server');
      this.outputChannel.appendLine('Connected to Hemis server');

      // Check version
      try {
        const version = await this.request<{ protocolVersion: number; gitHash: string }>('hemis/version');
        debug(`Connection: version ${version.protocolVersion} (${version.gitHash})`);
        this.outputChannel.appendLine(`Backend version: ${version.protocolVersion} (${version.gitHash})`);
      } catch (err) {
        debug(`Connection: version check failed: ${err}`);
        this.outputChannel.appendLine(`Version check failed: ${err}`);
      }

      return true;
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      debug(`Connection: failed - ${message}`);
      vscode.window.showErrorMessage(`Hemis: ${message}`);
      return false;
    } finally {
      this.connecting = false;
    }
  }

  private setupSocket(socket: net.Socket): void {
    this.socket = socket;

    socket.on('data', (data: Buffer) => {
      this.onData(data.toString());
    });

    socket.on('close', () => {
      this.outputChannel.appendLine('Disconnected from server');
      this.onDisconnect();
    });

    socket.on('error', (err) => {
      this.outputChannel.appendLine(`Socket error: ${err.message}`);
    });
  }

  stop(): void {
    if (!this.socket) {
      return;
    }

    // Clear pending requests
    for (const [, callback] of this.pending) {
      callback(new Error('Client stopped'), null);
    }
    this.pending.clear();
    this.buffer = '';

    // Just disconnect, don't shutdown the server (other clients may be using it)
    this.socket.end();
    this.socket = null;
  }

  async request<T>(method: string, params: object = {}): Promise<T> {
    if (!await this.start()) {
      throw new Error('Not connected to server');
    }

    return new Promise((resolve, reject) => {
      const id = ++this.requestId;
      const request: JsonRpcRequest = {
        jsonrpc: '2.0',
        method,
        params,
        id,
      };

      const startTime = Date.now();
      debug(`RPC>> ${method} [${id}]`);
      debugVerbose(`  params:`, params);

      this.pending.set(id, (error, result) => {
        const elapsed = Date.now() - startTime;
        if (error) {
          debug(`RPC<< ${method} [${id}] FAILED (${elapsed}ms): ${error.message}`);
          reject(error);
        } else {
          debug(`RPC<< ${method} [${id}] ok (${elapsed}ms)`);
          debugVerbose(`  result:`, result);
          resolve(result as T);
        }
      });

      this.sendRaw(request);
      this.outputChannel.appendLine(`[request ${id}] ${method}`);
    });
  }

  isRunning(): boolean {
    return this.socket !== null;
  }

  private sendRaw(request: JsonRpcRequest): void {
    if (!this.socket) {
      throw new Error('Not connected to server');
    }

    const body = JSON.stringify(request);
    const message = `Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`;
    this.socket.write(message);
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

  private onDisconnect(): void {
    this.socket = null;
    this.buffer = '';

    // Fail remaining pending requests
    for (const [, callback] of this.pending) {
      callback(new Error('Disconnected from server'), null);
    }
    this.pending.clear();
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
