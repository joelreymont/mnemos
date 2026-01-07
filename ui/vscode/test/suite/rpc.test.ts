import * as assert from 'assert';
import * as vscode from 'vscode';

suite('RPC Client Test Suite', () => {
  vscode.window.showInformationMessage('Start RPC tests.');

  test('RPC client should be importable', () => {
    // Basic sanity check that the module loads
    const rpc = require('../../src/rpc');
    assert.ok(rpc.getRpcClient);
    assert.ok(rpc.disposeRpcClient);
  });

  test('Config should have defaults', () => {
    const config = require('../../src/config');
    const mnemosConfig = config.getConfig();
    assert.strictEqual(typeof mnemosConfig.autoRefresh, 'boolean');
    assert.strictEqual(typeof mnemosConfig.displayStyle, 'string');
  });
});
