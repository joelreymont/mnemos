import * as vscode from 'vscode';
import { Note, listNotes, getProjectRoot } from './notes';
import { getConfig } from './config';
import { debug, debugVerbose } from './debug';

// Decoration type for note markers
const noteDecorationType = vscode.window.createTextEditorDecorationType({
  isWholeLine: true,
  before: {
    margin: '0 0 0 0',
  },
});

const staleNoteDecorationType = vscode.window.createTextEditorDecorationType({
  isWholeLine: true,
  before: {
    margin: '0 0 0 0',
  },
});

// Track decorations per editor (exported for testing)
export const editorDecorations: Map<string, vscode.DecorationOptions[]> = new Map();

// Format note text for display
// Server provides formattedLines when content is sent
export function formatNoteText(note: Note, style: 'full' | 'minimal'): string {
  if (style === 'minimal') {
    return `[n:${note.shortId}]`;
  }

  // Server provides formattedLines; fallback to raw text if missing
  if (note.formattedLines && note.formattedLines.length > 0) {
    return note.formattedLines.join('\n');
  }
  return `// ${note.text}`;
}

export function renderNotes(editor: vscode.TextEditor, notes: Note[]): void {
  const config = getConfig();
  const decorations: vscode.DecorationOptions[] = [];
  const staleDecorations: vscode.DecorationOptions[] = [];
  debug(`renderNotes: ${notes.length} notes for ${editor.document.fileName}`);

  for (const note of notes) {
    // Server updates note.line with computed position when content is provided
    const line = Math.max(0, note.line - 1); // Convert to 0-indexed
    const range = new vscode.Range(line, 0, line, 0);

    // Server computes staleness when content is provided
    const isStale = note.stale ?? false;

    const text = formatNoteText(note, config.displayStyle);
    const color = isStale ? '#808080' : '#4682B4';

    const decoration: vscode.DecorationOptions = {
      range,
      renderOptions: {
        before: {
          contentText: text,
          color,
          fontStyle: 'italic',
          textDecoration: 'none; display: block; margin-bottom: 2px;',
        },
      },
      hoverMessage: new vscode.MarkdownString(
        `**Note** (${note.shortId})${isStale ? ' [STALE]' : ''}\n\n${note.text}`
      ),
    };

    if (isStale) {
      staleDecorations.push(decoration);
    } else {
      decorations.push(decoration);
    }
  }

  editor.setDecorations(noteDecorationType, decorations);
  editor.setDecorations(staleNoteDecorationType, staleDecorations);

  // Store for later reference
  editorDecorations.set(editor.document.uri.toString(), [...decorations, ...staleDecorations]);
}

export function clearNotes(editor: vscode.TextEditor): void {
  editor.setDecorations(noteDecorationType, []);
  editor.setDecorations(staleNoteDecorationType, []);
  editorDecorations.delete(editor.document.uri.toString());
}

export async function refreshNotes(editor: vscode.TextEditor): Promise<void> {
  const file = editor.document.uri.fsPath;
  const projectRoot = getProjectRoot();
  const content = editor.document.getText();

  debug(`refreshNotes: file=${file}`);

  if (!projectRoot) {
    debug('refreshNotes: no project root, clearing');
    clearNotes(editor);
    return;
  }

  try {
    // Send content so server computes displayLine positions
    const notes = await listNotes(file, projectRoot, true, content);
    debug(`refreshNotes: received ${notes.length} notes`);
    debugVerbose('refreshNotes: notes', notes.map(n => ({ id: n.shortId, line: n.line })));
    renderNotes(editor, notes);
  } catch (err) {
    // Backend might not be running, silently ignore
    debug(`refreshNotes: error - ${err}`);
    clearNotes(editor);
  }
}

export async function refreshAllEditors(): Promise<void> {
  for (const editor of vscode.window.visibleTextEditors) {
    await refreshNotes(editor);
  }
}

export function disposeDecorations(): void {
  noteDecorationType.dispose();
  staleNoteDecorationType.dispose();
  editorDecorations.clear();
}
