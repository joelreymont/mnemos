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

// Export for testing
export function getCommentPrefix(languageId: string): string {
  const commentPrefixes: Record<string, string> = {
    rust: '//',
    typescript: '//',
    javascript: '//',
    python: '#',
    go: '//',
    java: '//',
    c: '//',
    cpp: '//',
    csharp: '//',
    ruby: '#',
    shell: '#',
    bash: '#',
    lua: '--',
    sql: '--',
    haskell: '--',
    elm: '--',
    html: '<!--',
    css: '/*',
    scss: '//',
    yaml: '#',
    toml: '#',
  };
  return commentPrefixes[languageId] || '//';
}

// Export for testing
export function formatNoteText(note: Note, languageId: string, style: 'full' | 'minimal'): string {
  if (style === 'minimal') {
    return `[n:${note.id.substring(0, 8)}]`;
  }

  const prefix = getCommentPrefix(languageId);
  const lines = note.text.split('\n');
  const staleMarker = note.stale ? ' [STALE]' : '';

  if (lines.length === 1) {
    return `${prefix} ${lines[0]}${staleMarker}`;
  }

  return lines.map((line, i) => {
    if (i === lines.length - 1) {
      return `${prefix} ${line}${staleMarker}`;
    }
    return `${prefix} ${line}`;
  }).join('\n');
}

export function renderNotes(editor: vscode.TextEditor, notes: Note[]): void {
  const config = getConfig();
  const languageId = editor.document.languageId;
  const decorations: vscode.DecorationOptions[] = [];
  const staleDecorations: vscode.DecorationOptions[] = [];
  debug(`renderNotes: ${notes.length} notes for ${editor.document.fileName}`);

  for (const note of notes) {
    // Use server-computed displayLine when available, otherwise stored line
    const displayLine = note.displayLine ?? note.line;
    const line = Math.max(0, displayLine - 1); // Convert to 0-indexed
    const range = new vscode.Range(line, 0, line, 0);

    // Use server-computed staleness when available
    const isStale = note.computedStale ?? note.stale ?? false;

    const text = formatNoteText({ ...note, stale: isStale }, languageId, config.displayStyle);
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
        `**Note** (${note.id.substring(0, 8)})${isStale ? ' [STALE]' : ''}\n\n${note.text}`
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
    debugVerbose('refreshNotes: notes', notes.map(n => ({ id: n.id.substring(0, 8), line: n.line, displayLine: n.displayLine })));
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
