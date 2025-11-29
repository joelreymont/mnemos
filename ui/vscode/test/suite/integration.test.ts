import * as assert from 'assert';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as net from 'net';
import { spawn } from 'child_process';

// Find the backend binary
function findBackend(): string | null {
  // Check environment variable first
  const envBackend = process.env['HEMIS_BACKEND'];
  if (envBackend && fs.existsSync(envBackend)) {
    return envBackend;
  }

  // Try relative paths from extension root
  const extensionRoot = path.resolve(__dirname, '../../../');
  const candidates = [
    path.join(extensionRoot, '../../target/debug/hemis'),
    path.join(extensionRoot, '../../target/release/hemis'),
  ];

  for (const candidate of candidates) {
    if (fs.existsSync(candidate)) {
      return candidate;
    }
  }

  return null;
}

// Create isolated test directory
function createTestDir(): string {
  const testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'hemis-vscode-test-'));
  return testDir;
}

// Clean up test directory
function cleanupTestDir(testDir: string): void {
  try {
    fs.rmSync(testDir, { recursive: true, force: true });
  } catch {
    // Ignore cleanup errors
  }
}

// Simple RPC client for testing (no VS Code dependencies)
class TestRpcClient {
  private socket: net.Socket | null = null;
  private requestId = 0;
  private pending: Map<number, { resolve: (v: unknown) => void; reject: (e: Error) => void }> = new Map();
  private buffer = '';

  constructor(
    private hemisDir: string,
    private backendPath: string
  ) {}

  private getSocketPath(): string {
    return path.join(this.hemisDir, 'hemis.sock');
  }

  private socketExists(): boolean {
    return fs.existsSync(this.getSocketPath());
  }

  async startServer(): Promise<void> {
    if (!fs.existsSync(this.hemisDir)) {
      fs.mkdirSync(this.hemisDir, { recursive: true });
    }

    const env: Record<string, string | undefined> = { ...process.env };
    env['HEMIS_DIR'] = this.hemisDir;
    env['HEMIS_DB_PATH'] = path.join(this.hemisDir, 'hemis.db');

    const logPath = path.join(this.hemisDir, 'hemis.log');
    const logFd = fs.openSync(logPath, 'a');

    const proc = spawn(this.backendPath, ['--serve'], {
      detached: true,
      stdio: ['ignore', logFd, logFd],
      env,
    });

    proc.unref();
    fs.closeSync(logFd);
  }

  async waitForSocket(timeoutMs: number): Promise<boolean> {
    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      if (this.socketExists()) {
        return true;
      }
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    return false;
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.socket = net.createConnection(this.getSocketPath());

      this.socket.on('connect', () => {
        resolve();
      });

      this.socket.on('data', (data: Buffer) => {
        this.onData(data.toString());
      });

      this.socket.on('error', (err) => {
        reject(err);
      });

      this.socket.on('close', () => {
        this.socket = null;
      });
    });
  }

  async start(): Promise<void> {
    if (this.socketExists()) {
      try {
        await this.connect();
        return;
      } catch {
        // Socket exists but can't connect, start server
      }
    }

    await this.startServer();
    if (!await this.waitForSocket(5000)) {
      throw new Error('Server failed to create socket');
    }
    await new Promise(resolve => setTimeout(resolve, 100));
    await this.connect();
  }

  async request<T>(method: string, params: object = {}): Promise<T> {
    if (!this.socket) {
      throw new Error('Not connected');
    }

    return new Promise((resolve, reject) => {
      const id = ++this.requestId;
      const request = {
        jsonrpc: '2.0',
        method,
        params,
        id,
      };

      this.pending.set(id, { resolve: resolve as (v: unknown) => void, reject });

      const body = JSON.stringify(request);
      const message = `Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`;
      this.socket!.write(message);
    });
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
        this.buffer = this.buffer.substring(headerEnd + 4);
        continue;
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
        const response = JSON.parse(body);
        const pending = this.pending.get(response.id);
        if (pending) {
          this.pending.delete(response.id);
          if (response.error) {
            pending.reject(new Error(response.error.message));
          } else {
            pending.resolve(response.result);
          }
        }
      } catch {
        // Ignore parse errors
      }
    }
  }

  disconnect(): void {
    if (this.socket) {
      this.socket.end();
      this.socket = null;
    }
  }

  async shutdown(): Promise<void> {
    if (this.socket) {
      try {
        await this.request('shutdown');
      } catch {
        // Ignore shutdown errors
      }
      this.disconnect();
    }
  }
}

suite('Integration Test Suite', () => {
  const backend = findBackend();
  let testDir: string;
  let client: TestRpcClient;

  setup(async function() {
    if (!backend) {
      this.skip();
      return;
    }

    testDir = createTestDir();
    client = new TestRpcClient(testDir, backend);
  });

  teardown(async () => {
    if (client) {
      await client.shutdown();
    }
    if (testDir) {
      // Wait a bit for processes to release files
      await new Promise(resolve => setTimeout(resolve, 200));
      cleanupTestDir(testDir);
    }
  });

  test('Backend binary exists', function() {
    if (!backend) {
      this.skip();
      return;
    }
    assert.ok(fs.existsSync(backend), `Backend should exist at ${backend}`);
  });

  test('RPC client can connect and get version', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    const version = await client.request<{ protocolVersion: number; gitHash: string }>('hemis/version');
    assert.ok(version, 'Should get version response');
    assert.ok(typeof version.protocolVersion === 'number', 'Protocol version should be a number');
    assert.ok(typeof version.gitHash === 'string', 'Git hash should be a string');
  });

  test('RPC client can get status', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    const status = await client.request<{ counts: { notes: number; files: number } }>('hemis/status');
    assert.ok(status, 'Should get status response');
    assert.ok(status.counts, 'Status should have counts');
    assert.strictEqual(typeof status.counts.notes, 'number', 'Notes count should be a number');
    assert.strictEqual(typeof status.counts.files, 'number', 'Files count should be a number');
  });

  test('RPC client can create and retrieve note', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    const createParams = {
      file: '/tmp/test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Test note from VS Code',
    };

    const created = await client.request<{ id: string; text: string }>('notes/create', createParams);
    assert.ok(created, 'Should create note');
    assert.ok(created.id, 'Created note should have id');
    assert.strictEqual(created.text, 'Test note from VS Code', 'Note text should match');

    const fetched = await client.request<{ id: string; text: string }>('notes/get', { id: created.id });
    assert.ok(fetched, 'Should fetch note');
    assert.strictEqual(fetched.id, created.id, 'Fetched note id should match');
    assert.strictEqual(fetched.text, created.text, 'Fetched note text should match');
  });

  test('RPC client can list notes for file', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    const testFile = '/tmp/list-test.rs';

    await client.request('notes/create', {
      file: testFile,
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'List test note',
    });

    const notes = await client.request<Array<{ id: string; text: string }>>('notes/list-for-file', {
      file: testFile,
      projectRoot: '/tmp',
      includeStale: true,
    });

    assert.ok(Array.isArray(notes), 'Notes should be an array');
    assert.ok(notes.length >= 1, 'Should have at least one note');
    assert.ok(notes.some(n => n.text === 'List test note'), 'Should find our note');
  });

  test('RPC client reconnects after disconnect', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(15000);

    await client.start();

    const version1 = await client.request<{ protocolVersion: number }>('hemis/version');
    assert.ok(version1.protocolVersion, 'Should get version');

    // Disconnect (but don't shutdown server)
    client.disconnect();

    // Wait a bit
    await new Promise(resolve => setTimeout(resolve, 500));

    // Reconnect
    await client.start();

    const version2 = await client.request<{ protocolVersion: number }>('hemis/version');
    assert.ok(version2.protocolVersion, 'Should get version after reconnect');
  });
});
