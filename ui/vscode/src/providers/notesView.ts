import * as vscode from 'vscode';
import { listNotes, Note, getProjectRoot } from '../notes';

export class NotesTreeDataProvider implements vscode.TreeDataProvider<NoteItem> {
  private _onDidChangeTreeData: vscode.EventEmitter<NoteItem | undefined | null | void> =
    new vscode.EventEmitter<NoteItem | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<NoteItem | undefined | null | void> =
    this._onDidChangeTreeData.event;

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  getTreeItem(element: NoteItem): vscode.TreeItem {
    return element;
  }

  async getChildren(element?: NoteItem): Promise<NoteItem[]> {
    if (element) {
      // No nested items
      return [];
    }

    // Get notes for current file
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
      return [];
    }

    const file = editor.document.uri.fsPath;
    const projectRoot = getProjectRoot();

    if (!projectRoot) {
      return [];
    }

    try {
      const notes = await listNotes(file, projectRoot);
      return notes.map((note) => new NoteItem(note));
    } catch {
      // Backend might not be running
      return [];
    }
  }
}

export class NoteItem extends vscode.TreeItem {
  constructor(public readonly note: Note) {
    super(note.summary, vscode.TreeItemCollapsibleState.None);

    // Backend guarantees displayMarker
    this.description = note.displayMarker;

    // Backend guarantees hoverText
    this.tooltip = new vscode.MarkdownString(note.hoverText);

    // Click to jump to note
    this.command = {
      command: 'hemis.jumpToNote',
      title: 'Jump to Note',
      arguments: [note],
    };

    // Context menu
    this.contextValue = 'note';

    // Backend guarantees iconHint
    if (note.iconHint === 'stale') {
      this.iconPath = new vscode.ThemeIcon('warning', new vscode.ThemeColor('list.warningForeground'));
    } else {
      this.iconPath = new vscode.ThemeIcon('note');
    }
  }
}
