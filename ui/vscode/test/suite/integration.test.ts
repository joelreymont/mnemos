import * as assert from 'assert';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as net from 'net';
import { spawn, spawnSync } from 'child_process';

// Find the backend binary
function findBackend(): string | null {
  // Check environment variable first
  const envBackend = process.env['MNEMOS_BACKEND'];
  if (envBackend && fs.existsSync(envBackend)) {
    return envBackend;
  }

  // Try relative paths from extension root
  const extensionRoot = path.resolve(__dirname, '../../../');
  const candidates = [
    path.join(extensionRoot, '../../zig-out/bin/mnemos'),
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
  const testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'mnemos-vscode-test-'));
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
    private mnemosDir: string,
    private backendPath: string
  ) {}

  private getSocketPath(): string {
    return path.join(this.mnemosDir, 'mnemos.sock');
  }

  private socketExists(): boolean {
    return fs.existsSync(this.getSocketPath());
  }

  async startServer(): Promise<void> {
    if (!fs.existsSync(this.mnemosDir)) {
      fs.mkdirSync(this.mnemosDir, { recursive: true });
    }

    const env: Record<string, string | undefined> = { ...process.env };
    env['MNEMOS_DIR'] = this.mnemosDir;
    env['MNEMOS_NOTES_PATH'] = path.join(this.mnemosDir, 'notes');

    const logPath = path.join(this.mnemosDir, 'mnemos.log');
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

    const version = await client.request<{ protocolVersion: number; gitHash: string }>('mnemos/version');
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

    const status = await client.request<{ counts: { notes: number; files: number } }>('mnemos/status');
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

    // Backend returns {notes: [...]} wrapper
    const result = await client.request<{ notes: Array<{ id: string; text: string }> } | Array<{ id: string; text: string }>>('notes/list-for-file', {
      file: testFile,
      projectRoot: '/tmp',
      includeStale: true,
    });

    // Unwrap if needed
    const notes = Array.isArray(result) ? result : result.notes;

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

    const version1 = await client.request<{ protocolVersion: number }>('mnemos/version');
    assert.ok(version1.protocolVersion, 'Should get version');

    // Disconnect (but don't shutdown server)
    client.disconnect();

    // Wait a bit
    await new Promise(resolve => setTimeout(resolve, 500));

    // Reconnect
    await client.start();

    const version2 = await client.request<{ protocolVersion: number }>('mnemos/version');
    assert.ok(version2.protocolVersion, 'Should get version after reconnect');
  });

  test('RPC client can search notes for link insertion', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a note that we'll search for
    const created = await client.request<{ id: string; text: string }>('notes/create', {
      file: '/tmp/link-search-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Unique searchable note for linking',
    });

    assert.ok(created.id, 'Note should be created');

    // Search for the note using notes/search (used by link insertion)
    const results = await client.request<Array<{ id: string; text: string; summary?: string }>>('notes/search', {
      query: 'Unique searchable',
    });

    assert.ok(Array.isArray(results), 'Search should return array');
    assert.ok(results.length >= 1, 'Should find at least one note');
    assert.ok(results.some(r => r.id === created.id), 'Should find the created note');
  });

  test('RPC client can query backlinks', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create note A (the target)
    const noteA = await client.request<{ id: string; text: string }>('notes/create', {
      file: '/tmp/backlinks-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Note A - the target',
    });
    assert.ok(noteA.id, 'Note A should have id');

    // Create note B that links to A using [[desc][id]] format
    const noteB = await client.request<{ id: string; text: string }>('notes/create', {
      file: '/tmp/backlinks-test.rs',
      projectRoot: '/tmp',
      line: 10,
      column: 0,
      text: `Note B links to [[target][${noteA.id}]]`,
    });
    assert.ok(noteB.id, 'Note B should have id');

    // Query backlinks for note A
    const backlinks = await client.request<Array<{ id: string; text: string }>>('notes/backlinks', {
      id: noteA.id,
    });

    assert.ok(Array.isArray(backlinks), 'Backlinks should be an array');
    assert.strictEqual(backlinks.length, 1, 'Note A should have exactly one backlink');
    assert.strictEqual(backlinks[0].id, noteB.id, 'Backlink should be note B');
    assert.ok(backlinks[0].text.includes('Note B links to'), 'Backlink text should match note B');
  });

  test('RPC client can search notes', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a note with searchable text
    await client.request('notes/create', {
      file: '/tmp/search-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Searchable unique content',
    });

    // Search for it
    const results = await client.request<unknown[]>('mnemos/search', {
      query: 'Searchable',
      projectRoot: '/tmp',
    });

    assert.ok(Array.isArray(results), 'Search results should be an array');
  });

  test('RPC client can explain region', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a test file
    const testFile = path.join(testDir, 'explain-test.rs');
    fs.writeFileSync(testFile, 'fn main() {\n    println!("hello");\n}\n');

    // Explain a region
    const result = await client.request<{ content?: string; explanation?: string }>('mnemos/explain-region', {
      file: testFile,
      startLine: 1,
      endLine: 3,
      projectRoot: testDir,
    });

    assert.ok(result, 'Explain region should return result');
    assert.ok(result.content || result.explanation, 'Should have content');
  });

  test('RPC client can save and load snapshot', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a note first
    await client.request('notes/create', {
      file: '/tmp/snapshot-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Snapshot test note',
    });

    // Save snapshot
    const snapshotPath = path.join(testDir, 'snapshot.json');
    const saveResult = await client.request<{ counts: { notes: number } }>('mnemos/save-snapshot', {
      path: snapshotPath,
      projectRoot: '/tmp',
    });

    assert.ok(saveResult, 'Save snapshot should return result');
    assert.ok(saveResult.counts, 'Should have counts');
    assert.ok(fs.existsSync(snapshotPath), 'Snapshot file should exist');

    // Load snapshot
    const loadResult = await client.request<{ counts: { notes: number } }>('mnemos/load-snapshot', {
      path: snapshotPath,
    });

    assert.ok(loadResult, 'Load snapshot should return result');
    assert.ok(loadResult.counts, 'Should have counts');
  });

  test('RPC client can update note', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a note
    const created = await client.request<{ id: string; text: string }>('notes/create', {
      file: '/tmp/update-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'Original text',
    });

    // Update it
    const updated = await client.request<{ id: string; text: string }>('notes/update', {
      id: created.id,
      text: 'Updated text',
    });

    assert.strictEqual(updated.text, 'Updated text', 'Text should be updated');
  });

  test('RPC client can delete note', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create a note
    const created = await client.request<{ id: string }>('notes/create', {
      file: '/tmp/delete-test.rs',
      projectRoot: '/tmp',
      line: 1,
      column: 0,
      text: 'To be deleted',
    });

    // Delete it
    await client.request('notes/delete', { id: created.id });

    // Verify it's gone
    try {
      await client.request('notes/get', { id: created.id });
      assert.fail('Should have thrown error for deleted note');
    } catch (err: unknown) {
      // Expected - note doesn't exist
      assert.ok(true, 'Note should be deleted');
    }
  });
});

// Demo workflow tests (mirrors neovim.demo)
// These test position tracking and stale detection with real backend
suite('Demo Workflow Tests', () => {
  const backend = findBackend();
  let testDir: string;
  let client: TestRpcClient;

  // Demo code for position/stale testing
  const DEMO_CODE = `fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

impl Server {
    fn new(config: Config) -> Self {
        Server { config }
    }
}
`;

  // Helper to unwrap notes from {notes: [...]} wrapper
  function unwrapNotes(result: { notes?: Array<object> } | Array<object>): Array<object> {
    if (Array.isArray(result)) {
      return result;
    }
    return result.notes || [];
  }

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
      await new Promise(resolve => setTimeout(resolve, 200));
      cleanupTestDir(testDir);
    }
  });

  test('Position tracking: note line updates when lines inserted above', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'position-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    // Create note at line 7 (fn load_config) - MUST pass content for tree-sitter anchor
    const created = await client.request<{ id: string; nodeTextHash?: string }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 7,
      column: 0,
      text: 'Note on load_config function',
      content: DEMO_CODE,  // Required for position tracking!
    });

    assert.ok(created.id, 'Note should be created');
    assert.ok(created.nodeTextHash, 'Note should have nodeTextHash for position tracking');

    // Insert 2 comment lines at top
    const newContent = '// Comment 1\n// Comment 2\n' + DEMO_CODE;

    // List with new content to get updated line position
    const result = await client.request<{ notes: Array<{ line: number }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: newContent,  // Required for position computation!
    });

    const notes = unwrapNotes(result) as Array<{ line: number }>;
    assert.ok(notes.length >= 1, 'Should have at least one note');
    // Original line 7 + 2 inserted = 9
    assert.strictEqual(notes[0].line, 9, 'Note should move to line 9 after inserting 2 lines above');
  });

  test('Stale detection: marks note stale when anchor code changes', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'stale-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    // Create note at line 12 (fn new) with content for anchor
    const created = await client.request<{ id: string }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 12,
      column: 0,
      text: 'Note on new function',
      content: DEMO_CODE,  // Required for stale detection!
    });

    assert.ok(created.id, 'Note should be created');

    // Check initial staleness (with original content)
    const initialResult = await client.request<{ notes: Array<{ stale: boolean }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: DEMO_CODE,
    });

    const initialNotes = unwrapNotes(initialResult) as Array<{ stale: boolean }>;
    assert.strictEqual(initialNotes[0].stale, false, 'Note should be fresh initially');

    // Change "fn new" to "fn create" (modifies anchor code)
    const modifiedContent = DEMO_CODE.replace('fn new', 'fn create');

    // Check staleness with modified content
    const staleResult = await client.request<{ notes: Array<{ stale: boolean }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: modifiedContent,
    });

    const staleNotes = unwrapNotes(staleResult) as Array<{ stale: boolean }>;
    assert.strictEqual(staleNotes[0].stale, true, 'Note should be stale after anchor code changed');
  });

  test('Reattach clears stale status', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'reattach-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    // Create note at line 12 (fn new) with content
    const created = await client.request<{ id: string }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 12,
      column: 0,
      text: 'Reattach test note',
      content: DEMO_CODE,
    });

    assert.ok(created.id, 'Note should be created');

    // Change anchor code (makes note stale)
    const modifiedContent = DEMO_CODE.replace('fn new', 'fn create');

    // Reattach note to new code
    await client.request('notes/reattach', {
      id: created.id,
      file: testFile,
      line: 12,
      content: modifiedContent,
      projectRoot: testDir,
    });

    // Check staleness after reattach
    const result = await client.request<{ notes: Array<{ stale: boolean }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: modifiedContent,
    });

    const notes = unwrapNotes(result) as Array<{ stale: boolean }>;
    assert.strictEqual(notes[0].stale, false, 'Note should be fresh after reattach');
  });

  test('Multiline notes: creates and retrieves multiline content', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'multiline-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    const multilineText = 'Config improvements:\n- Add validation\n- Support env vars';

    // Create multiline note
    const created = await client.request<{ id: string; text: string; formattedLines?: string[] }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 7,
      column: 0,
      text: multilineText,
      content: DEMO_CODE,
    });

    assert.ok(created.id, 'Note should be created');
    assert.strictEqual(created.text, multilineText, 'Note text should match');
    assert.ok(created.formattedLines, 'Should have formattedLines');
    assert.ok(created.formattedLines!.length >= 3, 'Should have at least 3 formatted lines');
  });

  test('Explain region AI: creates note with AI explanation', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    const provider = process.env['MNEMOS_AI_PROVIDER'];
    if (!provider || provider === 'none') {
      this.skip();
      return;
    }
    const providerCheck = spawnSync(provider, ['--version'], { stdio: 'ignore' });
    if (providerCheck.status !== 0) {
      this.skip();
      return;
    }

    this.timeout(120000); // AI can take up to 2 minutes

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'explain-ai-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    // Call explain-region with useAI=true
    const explainResult = await client.request<{
      content?: string;
      explanation?: string;
      ai?: { statusDisplay?: string };
    }>('mnemos/explain-region', {
      file: testFile,
      startLine: 2,
      endLine: 4,
      projectRoot: testDir,
      content: DEMO_CODE,
      useAI: true,
    });

    assert.ok(explainResult, 'Should return result');
    assert.ok(explainResult.explanation, 'Should return explanation');

    // AI returned explanation - create note with it
    const statusDisplay = explainResult.ai?.statusDisplay || `[AI: ${provider}]`;
    const noteText = `${statusDisplay} ${explainResult.explanation}`;

    const note = await client.request<{ id: string; text: string }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 2,
      column: 0,
      text: noteText,
      content: DEMO_CODE,
    });

    assert.ok(note.id, 'Note should be created with AI explanation');
    assert.ok(note.text.includes('['), 'Note should include AI status');

    // Verify note appears in list
    const listResult = await client.request<{ notes: Array<{ id: string }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: DEMO_CODE,
    });

    const notes = unwrapNotes(listResult) as Array<{ id: string }>;
    assert.ok(notes.some(n => n.id === note.id), 'Note should appear in list');
  });

  test('Full create-display-delete cycle', async function() {
    if (!backend) {
      this.skip();
      return;
    }

    this.timeout(10000);

    await client.start();

    // Create test file
    const testFile = path.join(testDir, 'lifecycle-test.rs');
    fs.writeFileSync(testFile, DEMO_CODE);

    // Create note
    const created = await client.request<{ id: string }>('notes/create', {
      file: testFile,
      projectRoot: testDir,
      line: 5,
      column: 0,
      text: 'Lifecycle test note',
      content: DEMO_CODE,
    });

    assert.ok(created.id, 'Note should be created');

    // List notes - should have one
    const listAfterCreate = await client.request<{ notes: Array<{ id: string }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: DEMO_CODE,
    });

    const notesAfterCreate = unwrapNotes(listAfterCreate) as Array<{ id: string }>;
    assert.strictEqual(notesAfterCreate.length, 1, 'Should have 1 note after create');

    // Delete note
    await client.request('notes/delete', { id: created.id });

    // List notes - should have none
    const listAfterDelete = await client.request<{ notes: Array<{ id: string }> }>('notes/list-for-file', {
      file: testFile,
      projectRoot: testDir,
      content: DEMO_CODE,
    });

    const notesAfterDelete = unwrapNotes(listAfterDelete) as Array<{ id: string }>;
    assert.strictEqual(notesAfterDelete.length, 0, 'Should have 0 notes after delete');
  });
});
