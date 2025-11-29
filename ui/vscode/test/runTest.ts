import * as path from 'path';
import * as fs from 'fs';
import { runTests } from '@vscode/test-electron';

function findBackend(): string | undefined {
  // Check environment variable first
  const envBackend = process.env['HEMIS_BACKEND'];
  if (envBackend && fs.existsSync(envBackend)) {
    return envBackend;
  }

  // Try relative paths from extension root
  const extensionRoot = path.resolve(__dirname, '../../');
  const candidates = [
    path.join(extensionRoot, '../../target/debug/hemis'),
    path.join(extensionRoot, '../../target/release/hemis'),
  ];

  for (const candidate of candidates) {
    const resolved = path.resolve(candidate);
    if (fs.existsSync(resolved)) {
      return resolved;
    }
  }

  return undefined;
}

async function main() {
  try {
    // The folder containing the Extension Manifest package.json
    // Passed to `--extensionDevelopmentPath`
    const extensionDevelopmentPath = path.resolve(__dirname, '../../');

    // The path to test runner
    // Passed to --extensionTestsPath
    const extensionTestsPath = path.resolve(__dirname, './suite/index');

    // Find backend for integration tests
    const backend = findBackend();
    const env: Record<string, string> = { ...process.env as Record<string, string> };
    if (backend) {
      env['HEMIS_BACKEND'] = backend;
      console.log(`Using backend: ${backend}`);
    } else {
      console.log('Backend not found, integration tests will be skipped');
    }

    // Download VS Code, unzip it and run the integration test
    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      launchArgs: ['--disable-extensions'],
      extensionTestsEnv: env,
    });
  } catch (err) {
    console.error('Failed to run tests');
    process.exit(1);
  }
}

main();
