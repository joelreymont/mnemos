import * as vscode from 'vscode';
import * as fs from 'fs';
import { getRpcClient, disposeRpcClient } from './rpc';
import { registerCommands } from './commands';
import { refreshNotes, disposeDecorations, refreshAllEditors, updateNotePosition } from './decorations';
import { getConfig } from './config';
import { NotesTreeDataProvider } from './providers/notesView';
import { debug, disposeDebug } from './debug';
import { getEventClient, disposeEventClient, NotePositionChangedEvent, NoteCreatedEvent, NoteDeletedEvent, NoteUpdatedEvent } from './events';

/**
 * Resolve a path to its canonical form (resolving symlinks).
 * On macOS, /tmp is a symlink to /private/tmp.
 * Falls back to the original path if resolution fails.
 */
function resolveRealPath(filePath: string): string {
  try {
    return fs.realpathSync(filePath);
  } catch {
    return filePath;
  }
}

/**
 * Check if two file paths refer to the same file, handling symlinks.
 * Server sends canonicalized paths (e.g., /private/tmp/foo on macOS)
 * but editors may have non-canonicalized names (e.g., /tmp/foo).
 */
function pathsMatch(editorPath: string, eventPath: string): boolean {
  // Fast path: direct comparison
  if (editorPath === eventPath) {
    return true;
  }
  // Resolve symlinks and compare
  return resolveRealPath(editorPath) === resolveRealPath(eventPath);
}

let notesProvider: NotesTreeDataProvider | null = null;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  console.log('Mnemos extension activating...');
  debug('Extension activating');

  // Register commands
  registerCommands(context);

  // Register tree view
  notesProvider = new NotesTreeDataProvider();
  context.subscriptions.push(
    vscode.window.registerTreeDataProvider('mnemosNotes', notesProvider)
  );

  // Start backend
  const client = getRpcClient();
  const started = await client.start();

  if (started) {
    console.log('Mnemos backend started successfully');
  } else {
    console.log('Mnemos backend failed to start - commands will try to start it on demand');
  }

  // Start event client for push notifications
  const eventClient = getEventClient();
  eventClient.start();

  // Handle note position changes (from buffer-update events)
  eventClient.on('note-position-changed', (event: NotePositionChangedEvent) => {
    debug(`Event: note-position-changed ${event.id} ${event.old_line}->${event.new_line}`);
    updateNotePosition(event.file, event.id, event.new_line, event.stale);
  });

  // Handle note CRUD events
  eventClient.on('note-created', (event: NoteCreatedEvent) => {
    debug(`Event: note-created ${event.id} at ${event.file}:${event.line}`);
    // Refresh the file where note was created
    refreshFileDecorations(event.file);
    notesProvider?.refresh();
  });

  eventClient.on('note-updated', (event: NoteUpdatedEvent) => {
    debug(`Event: note-updated ${event.id}`);
    // Full refresh since we don't know which file
    refreshAllEditors();
    notesProvider?.refresh();
  });

  eventClient.on('note-deleted', (event: NoteDeletedEvent) => {
    debug(`Event: note-deleted ${event.id}`);
    // Full refresh since we don't know which file
    refreshAllEditors();
    notesProvider?.refresh();
  });

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
      if (e.affectsConfiguration('mnemos')) {
        // Restart backend if config changed
        disposeRpcClient();
        getRpcClient().start();
        refreshAllEditors();
      }
    })
  );

  console.log('Mnemos extension activated');
}

export function deactivate(): void {
  console.log('Mnemos extension deactivating...');
  debug('Extension deactivating');
  disposeEventClient();
  disposeRpcClient();
  disposeDecorations();
  disposeDebug();
  console.log('Mnemos extension deactivated');
}

// Helper to refresh decorations for a specific file
async function refreshFileDecorations(file: string): Promise<void> {
  for (const editor of vscode.window.visibleTextEditors) {
    if (pathsMatch(editor.document.uri.fsPath, file)) {
      await refreshNotes(editor);
      return;
    }
  }
}
