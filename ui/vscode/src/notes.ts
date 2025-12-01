import * as vscode from 'vscode';
import { getRpcClient } from './rpc';

export interface Note {
  id: string;
  file: string;
  line: number;
  column: number;
  text: string;
  nodePath: string[];
  commit?: string;
  blob?: string;
  stale?: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateNoteParams {
  file: string;
  projectRoot: string;
  line: number;
  column: number;
  text: string;
  nodePath: string[];
}

export interface UpdateNoteParams {
  id: string;
  text: string;
}

export async function createNote(params: CreateNoteParams): Promise<Note> {
  const client = getRpcClient();
  return client.request<Note>('notes/create', params);
}

export async function getNote(id: string): Promise<Note> {
  const client = getRpcClient();
  return client.request<Note>('notes/get', { id });
}

export async function updateNote(params: UpdateNoteParams): Promise<Note> {
  const client = getRpcClient();
  return client.request<Note>('notes/update', params);
}

export async function deleteNote(id: string): Promise<void> {
  const client = getRpcClient();
  await client.request<void>('notes/delete', { id });
}

export async function listNotes(file: string, projectRoot: string, includeStale = true): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/list-for-file', { file, projectRoot, includeStale });
}

export async function listNotesByNode(
  file: string,
  projectRoot: string,
  nodePath: string[],
  includeStale = true
): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/list-by-node', { file, projectRoot, nodePath, includeStale });
}

export async function indexFile(file: string, projectRoot: string, content: string): Promise<void> {
  const client = getRpcClient();
  await client.request<void>('index/add-file', { file, projectRoot, content });
}

export async function indexProject(projectRoot: string): Promise<{ indexed: number }> {
  const client = getRpcClient();
  return client.request<{ indexed: number }>('hemis/index-project', { projectRoot });
}

export interface SearchHit {
  kind: 'file' | 'note';
  file: string;
  line?: number;
  score: number;
  snippet?: string;
  noteId?: string;
  noteSummary?: string;
}

export async function search(query: string, projectRoot?: string, includeNotes = true): Promise<SearchHit[]> {
  const client = getRpcClient();
  return client.request<SearchHit[]>('hemis/search', { query, projectRoot, includeNotes });
}

export interface Status {
  ok: boolean;
  projectRoot: string | null;
  counts: {
    notes: number;
    files: number;
    edges: number;
    embeddings: number;
  };
}

export async function getStatus(): Promise<Status> {
  const client = getRpcClient();
  return client.request<Status>('hemis/status', {});
}

// Helper to get project root from workspace
export function getProjectRoot(): string | null {
  const workspaceFolders = vscode.workspace.workspaceFolders;
  if (!workspaceFolders || workspaceFolders.length === 0) {
    return null;
  }
  return workspaceFolders[0].uri.fsPath;
}

// Helper to get note at current cursor position
export async function getNoteAtCursor(
  editor: vscode.TextEditor
): Promise<Note | null> {
  const document = editor.document;
  const position = editor.selection.active;
  const file = document.uri.fsPath;
  const line = position.line + 1; // 1-indexed
  const projectRoot = getProjectRoot();

  if (!projectRoot) {
    return null;
  }

  const notes = await listNotes(file, projectRoot);

  // Find note closest to cursor line
  for (const note of notes) {
    if (note.line === line) {
      return note;
    }
  }

  return null;
}
