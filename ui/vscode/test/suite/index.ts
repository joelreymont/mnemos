import * as path from 'path';
import Mocha from 'mocha';
import { glob } from 'glob';
import * as vscode from 'vscode';

export async function run(): Promise<void> {
  // Create the mocha test
  const mocha = new Mocha({
    ui: 'tdd',
    color: true,
  });

  const testsRoot = path.resolve(__dirname, '.');

  const files = await glob('**/**.test.js', { cwd: testsRoot });

  // Add files to the test suite
  files.forEach((f) => mocha.addFile(path.resolve(testsRoot, f)));

  return new Promise((resolve, reject) => {
    try {
      // Run the mocha test
      mocha.run((failures) => {
        if (failures > 0) {
          reject(new Error(`${failures} tests failed.`));
        } else {
          resolve();
        }
        const exitCode = failures > 0 ? 1 : 0;
        void vscode.commands.executeCommand('workbench.action.quit').then(
          () => process.exit(exitCode),
          () => {
            void vscode.commands.executeCommand('workbench.action.closeWindow').then(
              () => process.exit(exitCode),
              () => process.exit(exitCode),
            );
          },
        );
      });
    } catch (err) {
      console.error(err);
      reject(err);
    }
  });
}
