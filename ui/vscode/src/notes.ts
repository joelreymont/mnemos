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
  displayLine?: number; // Server-computed display position
  computedStale?: boolean; // Server-computed staleness
  createdAt: string;
  updatedAt: string;
}

export interface CreateNoteParams {
  file: string;
  projectRoot: string;
  line: number;
  column: number;
  text: string;
  content?: string; // Buffer content for server-side hash computation
  nodePath?: string[]; // Optional fallback if content not provided
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

export async function listNotes(
  file: string,
  projectRoot: string,
  includeStale = true,
  content?: string
): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/list-for-file', { file, projectRoot, includeStale, content });
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

export async function getBacklinks(id: string): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/backlinks', { id });
}

export async function shutdown(): Promise<void> {
  const client = getRpcClient();
  await client.request<void>('shutdown', {});
}

export interface ExplainResult {
  file: string;
  startLine: number;
  endLine: number;
  content: string;
  explanation?: string;
  ai?: {
    provider?: string;
    hadContext?: boolean;
    error?: string;
  };
}

export async function explainRegion(
  file: string,
  startLine: number,
  endLine: number,
  useAI = false,
  content?: string,
  projectRoot?: string
): Promise<ExplainResult> {
  const client = getRpcClient();
  const params: Record<string, unknown> = { file, startLine, endLine };
  if (useAI) {
    params.useAI = true;
  }
  if (content) {
    params.content = content;
  }
  if (projectRoot) {
    params.projectRoot = projectRoot;
  }
  return client.request<ExplainResult>('hemis/explain-region', params);
}

export interface IndexProjectResult {
  ok: boolean;
  indexed: number;
  skipped: number;
  projectRoot: string;
  ai?: {
    analyzed?: boolean;
    provider?: string;
    error?: string;
  };
}

export async function indexProjectWithAI(projectRoot: string, includeAI = false): Promise<IndexProjectResult> {
  const client = getRpcClient();
  const params: Record<string, unknown> = { projectRoot };
  if (includeAI) {
    params.includeAI = true;
  }
  return client.request<IndexProjectResult>('hemis/index-project', params);
}

export interface ProjectMeta {
  projectRoot: string;
  indexed: boolean;
  indexedAt?: number;
  indexedCommit?: string;
  analyzed: boolean;
  analyzedAt?: number;
  analysisCommit?: string;
  analysisProvider?: string;
  analysisStale: boolean;
  hasAnalysisFile: boolean;
  aiAvailable: boolean;
  currentCommit?: string;
}

export async function getProjectMeta(projectRoot: string): Promise<ProjectMeta> {
  const client = getRpcClient();
  return client.request<ProjectMeta>('hemis/project-meta', { projectRoot });
}

export interface SnapshotResult {
  ok: boolean;
  counts?: {
    notes: number;
    files: number;
    embeddings: number;
    edges: number;
  };
}

export async function saveSnapshot(path: string, projectRoot?: string): Promise<SnapshotResult> {
  const client = getRpcClient();
  return client.request<SnapshotResult>('hemis/save-snapshot', { path, projectRoot });
}

export async function loadSnapshot(path: string): Promise<SnapshotResult> {
  const client = getRpcClient();
  return client.request<SnapshotResult>('hemis/load-snapshot', { path });
}

// Send buffer update for real-time position tracking
export async function bufferUpdate(file: string, content: string): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/buffer-update', { file, content });
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
  const content = document.getText();

  if (!projectRoot) {
    return null;
  }

  // Send content so server computes displayLine
  const notes = await listNotes(file, projectRoot, true, content);

  // Find note at cursor line using displayLine (server-computed) or stored line
  for (const note of notes) {
    const displayLine = note.displayLine ?? note.line;
    if (displayLine === line) {
      return note;
    }
  }

  return null;
}
