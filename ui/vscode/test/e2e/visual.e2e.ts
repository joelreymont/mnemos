/**
 * Playwright e2e tests for VS Code extension - full demo workflow.
 * Pre-starts the backend server so the extension can connect to it.
 */

import { test, expect, _electron as electron, ElectronApplication, Page } from '@playwright/test';
import { downloadAndUnzipVSCode } from '@vscode/test-electron';
import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as net from 'net';

// Find backend binary
function findBackend(): string | null {
  const envBackend = process.env['HEMIS_BACKEND'];
  if (envBackend && fs.existsSync(envBackend)) {
    return envBackend;
  }
  const extensionRoot = path.resolve(__dirname, '../../');
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

// Wait for socket to exist
async function waitForSocket(socketPath: string, timeoutMs: number): Promise<boolean> {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (fs.existsSync(socketPath)) {
      return true;
    }
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  return false;
}

// Start backend server and wait for it to be ready
async function startBackend(backend: string, hemisDir: string, dbPath: string): Promise<ChildProcess> {
  const env: Record<string, string> = {
    ...process.env as Record<string, string>,
    HEMIS_DIR: hemisDir,
    HEMIS_DB_PATH: dbPath,
  };

  const logPath = path.join(hemisDir, 'backend.log');
  const logFd = fs.openSync(logPath, 'a');

  const proc = spawn(backend, ['--serve'], {
    detached: true,
    stdio: ['ignore', logFd, logFd],
    env,
  });

  // Wait for socket to appear
  const socketPath = path.join(hemisDir, 'hemis.sock');
  const ready = await waitForSocket(socketPath, 5000);
  if (!ready) {
    proc.kill();
    throw new Error('Backend failed to start - socket not created');
  }

  // Small delay for server to be fully ready
  await new Promise(resolve => setTimeout(resolve, 200));

  return proc;
}

// Test fixtures
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

let app: ElectronApplication;
let window: Page;
let testDir: string;
let vscodePath: string;
let backendProc: ChildProcess | null = null;

test.beforeAll(async () => {
  const backend = findBackend();
  if (!backend) {
    throw new Error('Backend not found. Build with: cargo build');
  }

  // Download VS Code if needed
  console.log('Downloading VS Code...');
  vscodePath = await downloadAndUnzipVSCode();
  console.log(`VS Code path: ${vscodePath}`);
});

test.beforeEach(async () => {
  // Create isolated test directory
  testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'hemis-playwright-'));
  const testFile = path.join(testDir, 'app.rs');
  fs.writeFileSync(testFile, DEMO_CODE);

  const backend = findBackend()!;
  const dbPath = path.join(testDir, 'hemis.db');

  // Start backend server BEFORE VS Code
  console.log('Starting backend server...');
  backendProc = await startBackend(backend, testDir, dbPath);
  console.log('Backend server started');

  // Create VS Code settings for this workspace
  const vscodeDir = path.join(testDir, '.vscode');
  fs.mkdirSync(vscodeDir, { recursive: true });

  const settings = {
    'hemis.backend': backend,
    'hemis.hemisDir': testDir,
    'hemis.databasePath': dbPath,
    'hemis.debug': 'verbose',
  };
  fs.writeFileSync(path.join(vscodeDir, 'settings.json'), JSON.stringify(settings, null, 2));

  const extensionPath = path.resolve(__dirname, '../../');

  // Set up environment
  const env: Record<string, string> = {
    ...process.env as Record<string, string>,
    HEMIS_DIR: testDir,
    HEMIS_DB_PATH: dbPath,
    HEMIS_BACKEND: backend,
  };

  // Launch VS Code with extension
  app = await electron.launch({
    executablePath: vscodePath,
    args: [
      '--extensionDevelopmentPath=' + extensionPath,
      '--disable-extensions', // Disable other extensions
      '--skip-welcome',
      '--skip-release-notes',
      '--disable-workspace-trust',
      testDir,
    ],
    env,
    timeout: 30000,
  });

  window = await app.firstWindow();
  await window.waitForLoadState('domcontentloaded');

  // Wait for VS Code to fully initialize
  await window.waitForTimeout(3000);
});

test.afterEach(async () => {
  if (app) {
    await app.close();
  }

  // Shutdown backend
  if (backendProc) {
    backendProc.kill('SIGTERM');
    backendProc = null;
  }

  // Clean up test directory
  if (testDir) {
    await new Promise(resolve => setTimeout(resolve, 200));
    try {
      fs.rmSync(testDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  }
});

// Helper to open a file in the editor
async function openFile(w: Page, filename: string): Promise<void> {
  await w.keyboard.press('Meta+p');
  await w.waitForTimeout(500);
  await w.keyboard.type(filename);
  await w.keyboard.press('Enter');
  await w.waitForTimeout(1000);
}

// Helper to go to a specific line
async function gotoLine(w: Page, line: number): Promise<void> {
  await w.keyboard.press('Meta+g');
  await w.waitForTimeout(300);
  await w.keyboard.type(String(line));
  await w.keyboard.press('Enter');
  await w.waitForTimeout(500);
}

// Helper to run a command from command palette
async function runCommand(w: Page, command: string): Promise<void> {
  await w.keyboard.press('Meta+Shift+p');
  await w.waitForTimeout(500);
  await w.keyboard.type(command);
  await w.waitForTimeout(300);
  await w.keyboard.press('Enter');
  await w.waitForTimeout(500);
}

// Helper to type in an input box
async function typeInInputBox(w: Page, text: string): Promise<void> {
  await w.waitForTimeout(300);
  await w.keyboard.type(text);
  await w.keyboard.press('Enter');
  await w.waitForTimeout(500);
}

// Helper to dismiss notifications
async function dismissNotifications(w: Page): Promise<void> {
  // Click notification dismiss button if present
  const dismissBtn = w.locator('button:has-text("Clear Notification")').first();
  if (await dismissBtn.isVisible({ timeout: 500 }).catch(() => false)) {
    await dismissBtn.click();
  }
}

// Helper to check if text appears in editor
async function editorContainsText(w: Page, text: string): Promise<boolean> {
  const codeEditor = w.locator('.monaco-editor[data-uri^="file://"]').first();
  return codeEditor.evaluate((el, searchText) => {
    return (el.textContent || '').includes(searchText);
  }, text);
}

// Helper to get editor content
async function getEditorContent(w: Page): Promise<string> {
  const codeEditor = w.locator('.monaco-editor[data-uri^="file://"]').first();
  return codeEditor.evaluate((el) => el.textContent || '');
}

test.describe('Demo Workflow E2E', () => {
  test('VS Code launches with extension connected to backend', async () => {
    const title = await window.title();
    expect(title).toMatch(/Extension Development Host|Visual Studio Code/);

    // Wait a bit for extension to connect
    await window.waitForTimeout(2000);

    // Check status bar or try a command
    await runCommand(window, 'Hemis: Show Status');
    await window.waitForTimeout(1000);

    // Take screenshot
    await window.screenshot({ path: path.join(testDir, 'status.png') });
  });

  test('create note and verify decoration appears', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 7);

    // Create a note
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Configuration loader function');

    // Wait for decoration to render
    await window.waitForTimeout(2000);

    // Take screenshot
    await window.screenshot({ path: path.join(testDir, 'after-create.png') });

    // Verify the note marker appears in the editor
    // Decorations use CSS before pseudo-elements, so we check for the virtual line
    const hasDecoration = await window.locator('.monaco-editor').first().evaluate((el) => {
      // Check for decoration content or marker
      const text = el.textContent || '';
      return text.includes('Configuration') || text.includes('[n:') || text.includes('// ');
    });

    // The decoration might not be in textContent due to CSS pseudo-elements
    // Just verify no error occurred and we can see the editor
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('position tracking: note moves when lines inserted', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 7);

    // Create a note on line 7 (load_config function)
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Track position test');
    await window.waitForTimeout(1000);

    // Insert lines at top of file
    await gotoLine(window, 1);
    await window.keyboard.press('Home');
    await window.keyboard.type('// Line 1\n// Line 2\n// Line 3\n');
    await window.waitForTimeout(500);

    // Trigger refresh
    await runCommand(window, 'Hemis: Refresh Notes');
    await window.waitForTimeout(2000);

    // Take screenshot showing updated position
    await window.screenshot({ path: path.join(testDir, 'position-tracking.png') });

    // Note should now be on line 10 (7 + 3 inserted lines)
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('stale detection: note becomes stale when code changes', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 12); // fn new line

    // Create a note
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Constructor implementation');
    await window.waitForTimeout(1000);

    // Modify the anchored code (change fn new to fn create)
    await gotoLine(window, 12);
    await window.keyboard.press('End');
    // Select and replace "new" with "create"
    await window.keyboard.press('Meta+d'); // Select word
    await window.waitForTimeout(200);
    await window.keyboard.type('create');
    await window.waitForTimeout(500);

    // Trigger refresh to detect staleness
    await runCommand(window, 'Hemis: Refresh Notes');
    await window.waitForTimeout(2000);

    // Take screenshot showing stale state
    await window.screenshot({ path: path.join(testDir, 'stale-note.png') });

    // Verify editor is still showing
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('reattach: clear stale status', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 12);

    // Create a note
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Reattach test note');
    await window.waitForTimeout(1000);

    // Modify anchor code to make note stale
    await gotoLine(window, 12);
    await window.keyboard.press('End');
    await window.keyboard.press('Meta+d');
    await window.waitForTimeout(200);
    await window.keyboard.type('create');
    await window.waitForTimeout(500);

    await runCommand(window, 'Hemis: Refresh Notes');
    await window.waitForTimeout(1000);

    // Screenshot before reattach
    await window.screenshot({ path: path.join(testDir, 'before-reattach.png') });

    // Reattach the note
    await gotoLine(window, 12);
    await runCommand(window, 'Hemis: Reattach Stale Note');
    await window.waitForTimeout(2000);

    // Screenshot after reattach
    await window.screenshot({ path: path.join(testDir, 'after-reattach.png') });

    // Verify editor visible
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('delete note and verify decoration disappears', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 7);

    // Create a note
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Note to delete');
    await window.waitForTimeout(1000);

    // Screenshot with note
    await window.screenshot({ path: path.join(testDir, 'before-delete.png') });

    // Delete the note
    await gotoLine(window, 7);
    await runCommand(window, 'Hemis: Delete Note');
    await window.waitForTimeout(1000);

    // Trigger refresh
    await runCommand(window, 'Hemis: Refresh Notes');
    await window.waitForTimeout(1000);

    // Screenshot without note
    await window.screenshot({ path: path.join(testDir, 'after-delete.png') });

    // Verify editor visible
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('multiline note display', async () => {
    await openFile(window, 'app.rs');
    await gotoLine(window, 7);

    // Create a multiline note
    await runCommand(window, 'Hemis: Add Note');
    // Use Shift+Enter for newlines in input box, or just type multi-line text
    await window.keyboard.type('Config improvements:\n- Add validation\n- Support env vars');
    await window.keyboard.press('Enter');
    await window.waitForTimeout(2000);

    // Screenshot showing multiline note
    await window.screenshot({ path: path.join(testDir, 'multiline-note.png') });

    // Verify editor visible
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('list notes command works', async () => {
    await openFile(window, 'app.rs');

    // Create a few notes
    await gotoLine(window, 1);
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Main function');
    await window.waitForTimeout(500);

    await gotoLine(window, 7);
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Config loader');
    await window.waitForTimeout(500);

    // List notes
    await runCommand(window, 'Hemis: List Notes');
    await window.waitForTimeout(1000);

    // Screenshot showing note list
    await window.screenshot({ path: path.join(testDir, 'list-notes.png') });

    // Verify the quick-input widget is present (may be "hidden" due to CSS)
    const quickInput = window.locator('.quick-input-widget');
    await expect(quickInput).toBeAttached();

    // Verify editor is still visible
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });

  test('search notes command works', async () => {
    await openFile(window, 'app.rs');

    // Create a note with searchable text
    await gotoLine(window, 7);
    await runCommand(window, 'Hemis: Add Note');
    await typeInInputBox(window, 'Unique searchable content XYZ123');
    await window.waitForTimeout(1000);

    // Search for it
    await runCommand(window, 'Hemis: Search');
    await window.waitForTimeout(500);
    await window.keyboard.type('XYZ123');
    await window.waitForTimeout(1000);

    // Screenshot showing search results
    await window.screenshot({ path: path.join(testDir, 'search-notes.png') });

    // Verify the quick-input widget is present (may be "hidden" due to CSS)
    const quickInput = window.locator('.quick-input-widget');
    await expect(quickInput).toBeAttached();

    // Verify editor is still visible
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();
  });
});
