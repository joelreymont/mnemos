import * as vscode from 'vscode';
import * as path from 'path';
import { listNotes, Note } from '../notes';

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

    try {
      const notes = await listNotes(file);
      return notes.map((note) => new NoteItem(note));
    } catch {
      // Backend might not be running
      return [];
    }
  }
}

export class NoteItem extends vscode.TreeItem {
  constructor(public readonly note: Note) {
    super(note.text.split('\n')[0].substring(0, 50), vscode.TreeItemCollapsibleState.None);

    this.description = `L${note.line}`;
    this.tooltip = new vscode.MarkdownString(
      `**${note.id.substring(0, 8)}**${note.stale ? ' [STALE]' : ''}\n\n${note.text}`
    );

    // Click to jump to note
    this.command = {
      command: 'hemis.jumpToNote',
      title: 'Jump to Note',
      arguments: [note],
    };

    // Context menu
    this.contextValue = 'note';

    // Icon
    if (note.stale) {
      this.iconPath = new vscode.ThemeIcon('warning', new vscode.ThemeColor('list.warningForeground'));
    } else {
      this.iconPath = new vscode.ThemeIcon('note');
    }
  }
}
