import * as vscode from 'vscode';
import * as path from 'path';
import {
  createNote,
  deleteNote,
  updateNote,
  listNotes,
  indexFile,
  indexProject,
  search,
  getStatus,
  getNoteAtCursor,
  getProjectRoot,
  Note,
  SearchHit,
} from './notes';
import { refreshNotes, refreshAllEditors } from './decorations';

// Simple node path extraction (placeholder until Tree-sitter is integrated)
function getNodePath(): string[] {
  // For now, return empty - Tree-sitter integration would provide real node paths
  return [];
}

export async function addNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }

  const text = await vscode.window.showInputBox({
    prompt: 'Enter note text',
    placeHolder: 'Note about this code...',
  });

  if (!text) {
    return;
  }

  const document = editor.document;
  const position = editor.selection.active;

  try {
    await createNote({
      file: document.uri.fsPath,
      projectRoot,
      line: position.line + 1, // 1-indexed
      column: position.character,
      text,
      nodePath: getNodePath(),
    });

    await refreshNotes(editor);
    vscode.window.showInformationMessage('Note created');
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to create note: ${message}`);
  }
}

export async function deleteNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursor(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  const confirm = await vscode.window.showWarningMessage(
    `Delete note "${note.text.substring(0, 50)}..."?`,
    { modal: true },
    'Delete'
  );

  if (confirm !== 'Delete') {
    return;
  }

  try {
    await deleteNote(note.id);
    await refreshNotes(editor);
    vscode.window.showInformationMessage('Note deleted');
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to delete note: ${message}`);
  }
}

export async function editNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursor(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  const newText = await vscode.window.showInputBox({
    prompt: 'Edit note text',
    value: note.text,
  });

  if (!newText || newText === note.text) {
    return;
  }

  try {
    await updateNote({ id: note.id, text: newText });
    await refreshNotes(editor);
    vscode.window.showInformationMessage('Note updated');
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to update note: ${message}`);
  }
}

export async function refreshNotesCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    return;
  }

  await refreshNotes(editor);
}

export async function listNotesCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const file = editor.document.uri.fsPath;
  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }

  try {
    const notes = await listNotes(file, projectRoot);

    if (notes.length === 0) {
      vscode.window.showInformationMessage('No notes in this file');
      return;
    }

    const items = notes.map((note) => ({
      label: `L${note.line}: ${note.text.split('\n')[0].substring(0, 50)}`,
      description: note.stale ? '[STALE]' : '',
      detail: `ID: ${note.id.substring(0, 8)}`,
      note,
    }));

    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a note to jump to',
    });

    if (selected) {
      const line = Math.max(0, selected.note.line - 1);
      const position = new vscode.Position(line, selected.note.column);
      editor.selection = new vscode.Selection(position, position);
      editor.revealRange(new vscode.Range(position, position));
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to list notes: ${message}`);
  }
}

export async function indexFileCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const file = editor.document.uri.fsPath;
  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }
  const content = editor.document.getText();

  try {
    await indexFile(file, projectRoot, content);
    vscode.window.showInformationMessage(`Indexed: ${path.basename(file)}`);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to index file: ${message}`);
  }
}

export async function indexProjectCommand(): Promise<void> {
  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Indexing project...',
      cancellable: false,
    },
    async () => {
      try {
        const result = await indexProject(projectRoot);
        vscode.window.showInformationMessage(
          `Indexed ${result.indexed} files`
        );
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        vscode.window.showErrorMessage(`Failed to index project: ${message}`);
      }
    }
  );
}

export async function searchCommand(): Promise<void> {
  const query = await vscode.window.showInputBox({
    prompt: 'Search notes and files',
    placeHolder: 'Enter search query...',
  });

  if (!query) {
    return;
  }

  try {
    const projectRoot = getProjectRoot();
    const hits = await search(query, projectRoot || undefined);

    if (hits.length === 0) {
      vscode.window.showInformationMessage('No results found');
      return;
    }

    const items = hits.map((hit: SearchHit) => ({
      label: hit.kind === 'note' ? `[Note] ${hit.noteSummary || ''}` : `[File] ${path.basename(hit.file)}`,
      description: `Score: ${hit.score.toFixed(2)}`,
      detail: hit.line ? `${hit.file}:${hit.line}` : hit.file,
      hit,
    }));

    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a result to open',
    });

    if (selected) {
      const uri = vscode.Uri.file(selected.hit.file);
      const document = await vscode.workspace.openTextDocument(uri);
      const editor = await vscode.window.showTextDocument(document);

      if (selected.hit.line) {
        const line = Math.max(0, selected.hit.line - 1);
        const position = new vscode.Position(line, 0);
        editor.selection = new vscode.Selection(position, position);
        editor.revealRange(new vscode.Range(position, position));
      }
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Search failed: ${message}`);
  }
}

export async function insertLinkCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const query = await vscode.window.showInputBox({
    prompt: 'Search for a note to link',
    placeHolder: 'Enter search query...',
  });

  if (!query) {
    return;
  }

  try {
    const projectRoot = getProjectRoot();
    const hits = await search(query, projectRoot || undefined);
    const noteHits = hits.filter((h: SearchHit) => h.kind === 'note');

    if (noteHits.length === 0) {
      vscode.window.showInformationMessage('No notes found');
      return;
    }

    const items = noteHits.map((hit: SearchHit) => ({
      label: hit.noteSummary || `Note ${hit.noteId}`,
      description: path.basename(hit.file),
      detail: `Line ${hit.line}`,
      hit,
    }));

    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a note to link',
    });

    if (selected && selected.hit.noteId) {
      const description = selected.hit.noteSummary?.substring(0, 30) || 'note';
      const link = `[[${description}][${selected.hit.noteId}]]`;
      editor.edit((editBuilder) => {
        editBuilder.insert(editor.selection.active, link);
      });
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to search notes: ${message}`);
  }
}

export async function statusCommand(): Promise<void> {
  try {
    const status = await getStatus();
    const message = [
      `Hemis Status: ${status.ok ? 'OK' : 'Error'}`,
      `Project: ${status.projectRoot || 'None'}`,
      `Notes: ${status.counts.notes}`,
      `Files: ${status.counts.files}`,
      `Embeddings: ${status.counts.embeddings}`,
    ].join('\n');

    vscode.window.showInformationMessage(message, { modal: true });
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to get status: ${message}`);
  }
}

export function registerCommands(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.commands.registerCommand('hemis.addNote', addNoteCommand),
    vscode.commands.registerCommand('hemis.deleteNote', deleteNoteCommand),
    vscode.commands.registerCommand('hemis.editNote', editNoteCommand),
    vscode.commands.registerCommand('hemis.refreshNotes', refreshNotesCommand),
    vscode.commands.registerCommand('hemis.listNotes', listNotesCommand),
    vscode.commands.registerCommand('hemis.indexFile', indexFileCommand),
    vscode.commands.registerCommand('hemis.indexProject', indexProjectCommand),
    vscode.commands.registerCommand('hemis.search', searchCommand),
    vscode.commands.registerCommand('hemis.insertLink', insertLinkCommand),
    vscode.commands.registerCommand('hemis.status', statusCommand)
  );
}
