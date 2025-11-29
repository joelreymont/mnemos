import * as vscode from 'vscode';
import { getRpcClient, disposeRpcClient } from './rpc';
import { registerCommands } from './commands';
import { refreshNotes, disposeDecorations, refreshAllEditors } from './decorations';
import { getConfig } from './config';
import { NotesTreeDataProvider } from './providers/notesView';

let notesProvider: NotesTreeDataProvider | null = null;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  console.log('Hemis extension activating...');

  // Register commands
  registerCommands(context);

  // Register tree view
  notesProvider = new NotesTreeDataProvider();
  context.subscriptions.push(
    vscode.window.registerTreeDataProvider('hemisNotes', notesProvider)
  );

  // Start backend
  const client = getRpcClient();
  const started = await client.start();

  if (started) {
    console.log('Hemis backend started successfully');
  } else {
    console.log('Hemis backend failed to start - commands will try to start it on demand');
  }

  // Auto-refresh on file open
  if (getConfig().autoRefresh) {
    context.subscriptions.push(
      vscode.window.onDidChangeActiveTextEditor(async (editor) => {
        if (editor) {
          await refreshNotes(editor);
          notesProvider?.refresh();
        }
      })
    );

    // Refresh on document save
    context.subscriptions.push(
      vscode.workspace.onDidSaveTextDocument(async (document) => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document === document) {
          await refreshNotes(editor);
          notesProvider?.refresh();
        }
      })
    );

    // Initial refresh for current editor
    if (vscode.window.activeTextEditor) {
      await refreshNotes(vscode.window.activeTextEditor);
      notesProvider?.refresh();
    }
  }

  // Configuration change handler
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration('hemis')) {
        // Restart backend if config changed
        disposeRpcClient();
        getRpcClient().start();
        refreshAllEditors();
      }
    })
  );

  console.log('Hemis extension activated');
}

export function deactivate(): void {
  console.log('Hemis extension deactivating...');
  disposeRpcClient();
  disposeDecorations();
  console.log('Hemis extension deactivated');
}
