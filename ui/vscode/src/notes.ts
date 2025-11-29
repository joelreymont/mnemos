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

export async function listNotes(file: string, includeStale = true): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/list', { file, includeStale });
}

export async function listNotesByNode(
  file: string,
  nodePath: string[],
  includeStale = true
): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/list-by-node', { file, nodePath, includeStale });
}

export async function indexFile(file: string): Promise<void> {
  const client = getRpcClient();
  await client.request<void>('index/add-file', { file });
}

export async function indexProject(root: string): Promise<{ filesIndexed: number }> {
  const client = getRpcClient();
  return client.request<{ filesIndexed: number }>('hemis/index-project', { root });
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

export async function search(query: string, includeNotes = true): Promise<SearchHit[]> {
  const client = getRpcClient();
  return client.request<SearchHit[]>('hemis/search', { query, includeNotes });
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

// Helper to get note at current cursor position
export async function getNoteAtCursor(
  editor: vscode.TextEditor
): Promise<Note | null> {
  const document = editor.document;
  const position = editor.selection.active;
  const file = document.uri.fsPath;
  const line = position.line + 1; // 1-indexed

  const notes = await listNotes(file);

  // Find note closest to cursor line
  for (const note of notes) {
    if (note.line === line) {
      return note;
    }
  }

  return null;
}
