import * as vscode from 'vscode';
import * as path from 'path';
import {
  createNote,
  deleteNote,
  updateNote,
  listNotes,
  indexProject,
  indexProjectWithAI,
  search,
  getStatus,
  getBacklinks,
  shutdown,
  explainRegion,
  getNoteAtCursor,
  getNoteAtCursorWithPicker,
  getProjectRoot,
  getProjectMeta,
  reattachNote,
  Note,
  SearchHit,
} from './notes';
import { refreshNotes } from './decorations';

// Node path is now computed server-side when content is provided
// No local tree-sitter needed

// Selected note state
let selectedNote: Note | null = null;
let statusBarItem: vscode.StatusBarItem | null = null;

export function getSelectedNote(): Note | null {
  return selectedNote;
}

export function setSelectedNote(note: Note | null): void {
  selectedNote = note;
  updateStatusBar();
}

function updateStatusBar(): void {
  if (!statusBarItem) {
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    statusBarItem.command = 'hemis.clearSelection';
  }
  if (selectedNote) {
    const shortId = selectedNote.shortId || selectedNote.id.substring(0, 8);
    const summary = selectedNote.summary.length > 30
      ? selectedNote.summary.substring(0, 30) + '...'
      : selectedNote.summary;
    statusBarItem.text = `$(bookmark) ${shortId}: ${summary}`;
    statusBarItem.tooltip = `Selected note: ${selectedNote.summary}\nClick to clear selection`;
    statusBarItem.show();
  } else {
    statusBarItem.hide();
  }
}

// Jump to a note's location (used by tree view click)
export async function jumpToNoteCommand(note: Note): Promise<void> {
  if (!note || !note.file) {
    return;
  }

  try {
    const uri = vscode.Uri.file(note.file);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document);

    // Use displayLine if available (server-computed position), otherwise fall back to stored line
    const line = Math.max(0, (note.displayLine ?? note.line) - 1);
    const position = new vscode.Position(line, note.column);
    editor.selection = new vscode.Selection(position, position);
    editor.revealRange(new vscode.Range(position, position), vscode.TextEditorRevealType.InCenter);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to jump to note: ${message}`);
  }
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
      content: document.getText(), // Server computes nodePath and hash from content
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

  const note = await getNoteAtCursorWithPicker(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  const confirm = await vscode.window.showWarningMessage(
    `Delete note "${note.summary}"?`,
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

export async function reattachNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursorWithPicker(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  if (!note.stale) {
    vscode.window.showInformationMessage('Note is not stale');
    return;
  }

  const document = editor.document;
  const position = editor.selection.active;
  const file = document.uri.fsPath;
  const line = position.line + 1; // 1-indexed
  const column = position.character;
  const content = document.getText();

  try {
    await reattachNote(note.id, file, line, column, content);
    await refreshNotes(editor);
    vscode.window.showInformationMessage('Note reattached');
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to reattach note: ${message}`);
  }
}

export async function editNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursorWithPicker(editor);
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

export async function editNoteBufferCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursorWithPicker(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  // Store note info for later
  const noteId = note.id;
  const originalText = note.text;
  const originalFile = editor.document.uri;

  // Create an editable document
  const editDoc = await vscode.workspace.openTextDocument({
    content: note.text,
    language: 'markdown',
  });

  await vscode.window.showTextDocument(editDoc, {
    viewColumn: vscode.ViewColumn.Beside,
    preview: false,
  });

  // Watch for save
  const saveDisposable = vscode.workspace.onDidSaveTextDocument(async (savedDoc) => {
    if (savedDoc === editDoc) {
      const newText = savedDoc.getText(); // Backend trims
      if (newText.trim() && newText !== originalText) {
        try {
          await updateNote({ id: noteId, text: newText });
          vscode.window.showInformationMessage('Note saved');
          // Refresh the original file's notes
          const originalEditor = vscode.window.visibleTextEditors.find(
            e => e.document.uri.toString() === originalFile.toString()
          );
          if (originalEditor) {
            await refreshNotes(originalEditor);
          }
        } catch (err) {
          const message = err instanceof Error ? err.message : String(err);
          vscode.window.showErrorMessage(`Failed to save note: ${message}`);
        }
      }
    }
  });

  // Clean up when document is closed
  const closeDisposable = vscode.workspace.onDidCloseTextDocument((closedDoc) => {
    if (closedDoc === editDoc) {
      saveDisposable.dispose();
      closeDisposable.dispose();
    }
  });
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
      label: `L${note.line}: ${note.summary}`,
      description: note.displayMarker || (note.stale ? '[STALE]' : ''),
      detail: `ID: ${note.shortId}`,
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
      // Use backend displayLabel if available, otherwise build locally
      label: hit.displayLabel || (hit.kind === 'note' ? `[Note] ${hit.noteSummary || ''}` : `[File] ${path.basename(hit.file)}`),
      description: `Score: ${hit.score.toFixed(2)}`,
      // Use backend displayDetail if available, otherwise build locally
      detail: hit.displayDetail || (hit.line ? `${hit.file}:${hit.line}` : hit.file),
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
    // Use backend statusDisplay if available, otherwise build locally
    const message = status.statusDisplay || [
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

export async function backlinksCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursorWithPicker(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  try {
    const backlinks = await getBacklinks(note.id);

    if (backlinks.length === 0) {
      vscode.window.showInformationMessage('No backlinks found');
      return;
    }

    const items = backlinks.map((n) => ({
      label: n.summary,
      description: n.displayMarker || path.basename(n.file),
      detail: `Line ${n.line}`,
      note: n,
    }));

    const selected = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a note that links here',
    });

    if (selected) {
      const uri = vscode.Uri.file(selected.note.file);
      const document = await vscode.workspace.openTextDocument(uri);
      const openedEditor = await vscode.window.showTextDocument(document);

      const line = Math.max(0, selected.note.line - 1);
      const position = new vscode.Position(line, selected.note.column);
      openedEditor.selection = new vscode.Selection(position, position);
      openedEditor.revealRange(new vscode.Range(position, position));
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to get backlinks: ${message}`);
  }
}

export async function viewNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const note = await getNoteAtCursorWithPicker(editor);
  if (!note) {
    vscode.window.showInformationMessage('No note at cursor position');
    return;
  }

  // Create a virtual document to show the note
  const content = [
    `# Note`,
    ``,
    `**File:** ${note.file}:${note.line}`,
    // Use backend formattedCreatedAt/formattedUpdatedAt if available
    `**Created:** ${note.formattedCreatedAt || new Date(Number(note.createdAt) * 1000).toLocaleString()}`,
    `**Updated:** ${note.formattedUpdatedAt || new Date(Number(note.updatedAt) * 1000).toLocaleString()}`,
    note.stale ? `**Status:** STALE` : '',
    ``,
    `---`,
    ``,
    note.text,
  ].filter(Boolean).join('\n');

  const doc = await vscode.workspace.openTextDocument({
    content,
    language: 'markdown',
  });
  await vscode.window.showTextDocument(doc, { preview: true });
}

export async function helpCommand(): Promise<void> {
  const commands = [
    '**Hemis Commands**',
    '',
    '| Command | Keybinding | Description |',
    '|---------|------------|-------------|',
    '| Add Note | Cmd+Shift+H N | Create a note at cursor |',
    '| Edit Note | Cmd+Shift+H E | Edit note at cursor |',
    '| Edit Note (Buffer) | | Edit note in side buffer |',
    '| Delete Note | Cmd+Shift+H D | Delete note at cursor |',
    '| List Notes | Cmd+Shift+H L | List notes in file |',
    '| View Note | Cmd+Shift+H V | View full note text |',
    '| Backlinks | Cmd+Shift+H B | Show notes linking here |',
    '| Search | Cmd+Shift+H S | Search notes and files |',
    '| Insert Link | Cmd+Shift+H K | Insert note link |',
    '| Index Project | Cmd+Shift+H P | Index all project files |',
    '| Explain Region | | Get code snippet for LLM |',
    '| Status | | Show database counts |',
    '| Shutdown | | Stop backend server |',
  ].join('\n');

  const doc = await vscode.workspace.openTextDocument({
    content: commands,
    language: 'markdown',
  });
  await vscode.window.showTextDocument(doc, { preview: true });
}

export async function shutdownCommand(): Promise<void> {
  try {
    await shutdown();
    vscode.window.showInformationMessage('Hemis backend shutdown');
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to shutdown: ${message}`);
  }
}

export async function explainRegionCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const selection = editor.selection;
  if (selection.isEmpty) {
    vscode.window.showInformationMessage('Select a region first');
    return;
  }

  const file = editor.document.uri.fsPath;
  const startLine = selection.start.line + 1;
  const endLine = selection.end.line + 1;

  try {
    const result = await explainRegion(file, startLine, endLine);

    // Copy to clipboard
    await vscode.env.clipboard.writeText(result.content);
    vscode.window.showInformationMessage(
      `Copied ${endLine - startLine + 1} lines to clipboard (LLM-ready)`
    );
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to explain region: ${message}`);
  }
}

export async function explainRegionAICommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage('No active editor');
    return;
  }

  const selection = editor.selection;
  if (selection.isEmpty) {
    vscode.window.showInformationMessage('Select a region first');
    return;
  }

  const file = editor.document.uri.fsPath;
  const startLine = selection.start.line + 1;
  const endLine = selection.end.line + 1;
  const content = editor.document.getText();
  const projectRoot = getProjectRoot();

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Getting AI explanation...',
      cancellable: false,
    },
    async () => {
      try {
        const result = await explainRegion(file, startLine, endLine, true, content, projectRoot || undefined);

        if (result.explanation) {
          // Show in a document
          const lines = [
            `# Explanation for ${path.basename(file)}:${startLine}-${endLine}`,
            '',
          ];

          if (result.ai) {
            if (result.ai.statusDisplay) {
              lines.push(`*${result.ai.statusDisplay}*`);
            } else if (result.ai.provider) {
              const suffix = result.ai.hadContext ? ' + project context' : '';
              lines.push(`*[AI: ${result.ai.provider}${suffix}]*`);
            } else if (result.ai.error) {
              lines.push(`*[AI error: ${result.ai.error}]*`);
            }
            lines.push('');
          }

          lines.push(result.explanation);

          const doc = await vscode.workspace.openTextDocument({
            content: lines.join('\n'),
            language: 'markdown',
          });
          await vscode.window.showTextDocument(doc, { preview: true });
        } else {
          // Fall back to clipboard
          await vscode.env.clipboard.writeText(result.content);
          let msg = `Copied ${endLine - startLine + 1} lines to clipboard`;
          if (result.ai?.error) {
            msg += ` (AI failed: ${result.ai.error})`;
          }
          vscode.window.showInformationMessage(msg);
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        vscode.window.showErrorMessage(`Failed to explain region: ${message}`);
      }
    }
  );
}

export async function indexProjectAICommand(): Promise<void> {
  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Indexing project with AI analysis...',
      cancellable: false,
    },
    async () => {
      try {
        const result = await indexProjectWithAI(projectRoot, true);
        const msg = result.statusMessage || (() => {
          let m = `Indexed ${result.indexed} files`;
          if (result.ai) {
            if (result.ai.analyzed) {
              m += `, analyzed with ${result.ai.provider}`;
            } else if (result.ai.error) {
              m += ` (AI failed: ${result.ai.error})`;
            }
          }
          return m;
        })();
        vscode.window.showInformationMessage(msg);
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        vscode.window.showErrorMessage(`Failed to index project: ${message}`);
      }
    }
  );
}

export async function projectMetaCommand(): Promise<void> {
  const projectRoot = getProjectRoot();
  if (!projectRoot) {
    vscode.window.showErrorMessage('No workspace folder open');
    return;
  }

  try {
    const meta = await getProjectMeta(projectRoot);

    const lines = [
      `# Project: ${meta.projectRoot}`,
      '',
      `**Indexed:** ${meta.indexed ? 'Yes' : 'No'}`,
    ];

    if (meta.formattedIndexedAt) {
      lines.push(`  - Last indexed: ${meta.formattedIndexedAt}`);
    } else if (meta.indexedAt) {
      lines.push(`  - Last indexed: ${new Date(meta.indexedAt * 1000).toLocaleString()}`);
    }

    lines.push('');

    const analysisStatus = meta.analysisStatusDisplay || (() => {
      if (meta.analyzed) {
        return meta.analysisStale ? 'Stale (commit changed)' : 'Up to date';
      } else if (meta.hasAnalysisFile) {
        return 'Has file but not tracked';
      } else {
        return 'Not analyzed';
      }
    })();

    lines.push(`**AI Analysis:** ${analysisStatus}`);
    if (meta.analysisProvider) {
      lines.push(`  - Provider: ${meta.analysisProvider}`);
    }

    lines.push('');
    lines.push(`**AI Available:** ${meta.aiAvailable ? 'Yes' : 'No'}`);

    const doc = await vscode.workspace.openTextDocument({
      content: lines.join('\n'),
      language: 'markdown',
    });
    await vscode.window.showTextDocument(doc, { preview: true });
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to get project meta: ${message}`);
  }
}

// Select a note at cursor or from list
export async function selectNoteCommand(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No active editor');
    return;
  }

  try {
    // First try to get note at cursor position
    const note = await getNoteAtCursorWithPicker(editor);
    if (note) {
      setSelectedNote(note);
      vscode.window.showInformationMessage(`Selected note: ${note.summary}`);
      return;
    }

    // If no note at cursor, show picker for notes in file
    const file = editor.document.uri.fsPath;
    const content = editor.document.getText();
    const notes = await listNotes(file, content);

    if (!notes || notes.length === 0) {
      vscode.window.showInformationMessage('No notes in this file');
      return;
    }

    // Show quick pick
    const items = notes.map(n => ({
      label: n.displayLabel || `[Note] ${n.summary}`,
      detail: n.displayDetail || `Line ${n.line}`,
      note: n
    }));

    const picked = await vscode.window.showQuickPick(items, {
      placeHolder: 'Select a note',
    });

    if (picked) {
      setSelectedNote(picked.note);
      vscode.window.showInformationMessage(`Selected note: ${picked.note.summary}`);
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    vscode.window.showErrorMessage(`Failed to select note: ${message}`);
  }
}

// Clear the selected note
export function clearSelectionCommand(): void {
  if (selectedNote) {
    vscode.window.showInformationMessage('Note selection cleared');
  }
  setSelectedNote(null);
}

export function registerCommands(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.commands.registerCommand('hemis.addNote', addNoteCommand),
    vscode.commands.registerCommand('hemis.deleteNote', deleteNoteCommand),
    vscode.commands.registerCommand('hemis.reattachNote', reattachNoteCommand),
    vscode.commands.registerCommand('hemis.editNote', editNoteCommand),
    vscode.commands.registerCommand('hemis.refreshNotes', refreshNotesCommand),
    vscode.commands.registerCommand('hemis.listNotes', listNotesCommand),
    vscode.commands.registerCommand('hemis.indexProject', indexProjectCommand),
    vscode.commands.registerCommand('hemis.indexProjectAI', indexProjectAICommand),
    vscode.commands.registerCommand('hemis.search', searchCommand),
    vscode.commands.registerCommand('hemis.insertLink', insertLinkCommand),
    vscode.commands.registerCommand('hemis.status', statusCommand),
    vscode.commands.registerCommand('hemis.projectMeta', projectMetaCommand),
    vscode.commands.registerCommand('hemis.backlinks', backlinksCommand),
    vscode.commands.registerCommand('hemis.viewNote', viewNoteCommand),
    vscode.commands.registerCommand('hemis.editNoteBuffer', editNoteBufferCommand),
    vscode.commands.registerCommand('hemis.help', helpCommand),
    vscode.commands.registerCommand('hemis.shutdown', shutdownCommand),
    vscode.commands.registerCommand('hemis.explainRegion', explainRegionCommand),
    vscode.commands.registerCommand('hemis.explainRegionAI', explainRegionAICommand),
    vscode.commands.registerCommand('hemis.jumpToNote', jumpToNoteCommand),
    vscode.commands.registerCommand('hemis.selectNote', selectNoteCommand),
    vscode.commands.registerCommand('hemis.clearSelection', clearSelectionCommand)
  );
}
