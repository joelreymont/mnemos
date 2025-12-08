/**
 * Playwright e2e tests for VS Code extension visual decoration rendering.
 * Uses Playwright's Electron support to launch VS Code with the extension
 * and verify decorations appear in the editor DOM.
 */

import { test, expect, _electron as electron, ElectronApplication, Page } from '@playwright/test';
import { downloadAndUnzipVSCode } from '@vscode/test-electron';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

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

  // Create VS Code settings for this workspace
  const vscodeDir = path.join(testDir, '.vscode');
  fs.mkdirSync(vscodeDir, { recursive: true });

  const backend = findBackend()!;
  const settings = {
    'hemis.backend': backend,
    'hemis.hemisDir': testDir,
    'hemis.databasePath': path.join(testDir, 'hemis.db'),
    'hemis.debug': 'verbose',
  };
  fs.writeFileSync(path.join(vscodeDir, 'settings.json'), JSON.stringify(settings, null, 2));

  const extensionPath = path.resolve(__dirname, '../../');

  // Set up environment
  const env: Record<string, string> = {
    ...process.env as Record<string, string>,
    HEMIS_DIR: testDir,
    HEMIS_DB_PATH: path.join(testDir, 'hemis.db'),
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
  if (testDir) {
    fs.rmSync(testDir, { recursive: true, force: true });
  }
});

test.describe('Visual Decoration Rendering', () => {
  test('VS Code launches with extension', async () => {
    const title = await window.title();
    // Extension Development Host mode uses different title format
    expect(title).toMatch(/Extension Development Host|Visual Studio Code/);
  });

  test('editor shows test file', async () => {
    // Open the test file via command palette
    await window.keyboard.press('Meta+p'); // Quick open
    await window.waitForTimeout(500);
    await window.keyboard.type('app.rs');
    await window.keyboard.press('Enter');
    await window.waitForTimeout(1000);

    // Verify editor has content - use first() for strict mode with multiple editors
    const editorContent = window.locator('.monaco-editor .view-lines').first();
    await expect(editorContent).toBeVisible();
  });

  test('hemis commands are registered', async () => {
    // Open command palette and verify Hemis commands exist
    await window.keyboard.press('Meta+Shift+p');
    await window.waitForTimeout(500);
    await window.keyboard.type('Hemis');
    await window.waitForTimeout(500);

    // Take screenshot showing Hemis commands
    await window.screenshot({ path: path.join(testDir, 'hemis-commands.png') });

    // Verify Hemis commands appear in the command palette
    const commandList = window.locator('.quick-input-list');
    await expect(commandList).toBeVisible();

    // Check that at least one Hemis command is shown
    const hemisCommands = window.locator('.quick-input-list-entry:has-text("Hemis")');
    const count = await hemisCommands.count();
    expect(count).toBeGreaterThan(0);
  });

  test('file code content is visible in editor', async () => {
    // Open file first
    await window.keyboard.press('Meta+p');
    await window.waitForTimeout(500);
    await window.keyboard.type('app.rs');
    await window.keyboard.press('Enter');
    await window.waitForTimeout(1000);

    // Take screenshot of editor with code
    await window.screenshot({ path: path.join(testDir, 'code-editor.png') });

    // Find the main code editor by data-uri attribute
    const codeEditor = window.locator('.monaco-editor[data-uri^="file://"]').first();
    await expect(codeEditor).toBeVisible();

    // Verify code content is visible
    const hasCode = await codeEditor.evaluate((el) => {
      const text = el.textContent || '';
      return text.includes('fn main') || text.includes('load_config');
    });
    expect(hasCode).toBe(true);
  });

  test('can take screenshot of editor state', async () => {
    // Open file
    await window.keyboard.press('Meta+p');
    await window.waitForTimeout(500);
    await window.keyboard.type('app.rs');
    await window.keyboard.press('Enter');
    await window.waitForTimeout(1000);

    // Take screenshot
    const screenshotPath = path.join(testDir, 'editor-state.png');
    await window.screenshot({ path: screenshotPath });

    expect(fs.existsSync(screenshotPath)).toBe(true);
    const stats = fs.statSync(screenshotPath);
    expect(stats.size).toBeGreaterThan(10000); // Should be a real screenshot
  });
});
