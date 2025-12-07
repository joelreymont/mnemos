import * as vscode from 'vscode';
import { getRpcClient } from './rpc';

export interface Note {
  id: string;
  shortId: string; // Server-computed first 8 chars of ID
  file: string;
  line: number;
  column: number;
  text: string;
  summary: string; // Server-computed first line, truncated to 60 chars
  nodePath: string[];
  commit?: string;
  blob?: string;
  stale?: boolean; // Server computes when content is provided
  formattedLines?: string[]; // Server-computed formatted lines with comment prefix
  displayLine?: number; // Server-computed display line (may differ from stored line)
  createdAt: string;
  updatedAt: string;
  formattedCreatedAt?: string; // Server-computed human-readable timestamp
  formattedUpdatedAt?: string; // Server-computed human-readable timestamp
  hoverText?: string; // Server-computed ready-to-display hover content (markdown)
  displayMarker?: string; // Server-computed minimal marker like "[n:abc123]"
  iconHint?: 'fresh' | 'stale'; // Server-computed icon hint for color selection
}

export interface CreateNoteParams {
  file: string;
  projectRoot?: string; // Server auto-computes from file if not provided
  line: number;
  column: number;
  text: string;
  content?: string; // Buffer content - server computes anchor position, nodePath, and hash
  nodePath?: string[]; // Deprecated: only used if content not provided (backwards compat)
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
  projectRoot?: string, // Server auto-computes from file if not provided
  includeStale = true,
  content?: string
): Promise<Note[]> {
  const client = getRpcClient();
  const params: Record<string, unknown> = { file, includeStale };
  if (projectRoot) {
    params.projectRoot = projectRoot;
  }
  if (content) {
    params.content = content;
  }
  return client.request<Note[]>('notes/list-for-file', params);
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
  displayLabel?: string; // Server-computed label (e.g., "[Note] Summary")
  displayDetail?: string; // Server-computed detail (e.g., "path/file.rs:42")
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
  statusDisplay?: string; // Server-computed formatted status string
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
    statusDisplay?: string; // Server-computed AI status string (e.g., "[AI: claude + project context]")
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
  statusMessage?: string; // Server-computed status message (e.g., "Project indexed: 42 files, analyzed with claude")
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
  formattedIndexedAt?: string; // Server-computed human-readable timestamp
  indexedCommit?: string;
  analyzed: boolean;
  analyzedAt?: number;
  formattedAnalyzedAt?: string; // Server-computed human-readable timestamp
  analysisCommit?: string;
  analysisProvider?: string;
  analysisStale: boolean;
  analysisStatusDisplay?: string; // Server-computed status (e.g., "Up to date", "Stale (commit changed)")
  hasAnalysisFile: boolean;
  aiAvailable: boolean;
  currentCommit?: string;
}

export async function getProjectMeta(projectRoot: string): Promise<ProjectMeta> {
  const client = getRpcClient();
  return client.request<ProjectMeta>('hemis/project-meta', { projectRoot });
}

// Send buffer update for real-time position tracking
export async function bufferUpdate(file: string, content: string): Promise<Note[]> {
  const client = getRpcClient();
  return client.request<Note[]>('notes/buffer-update', { file, content });
}

// Reattach a stale note to the current cursor position
export async function reattachNote(
  id: string,
  file: string,
  line: number,
  column: number,
  content: string
): Promise<Note> {
  const client = getRpcClient();
  return client.request<Note>('notes/reattach', {
    id,
    file,
    line,
    column,
    content,
  });
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
// Uses server-side notes/get-at-position for efficiency
export async function getNoteAtCursor(
  editor: vscode.TextEditor
): Promise<Note | null> {
  const client = getRpcClient();
  const document = editor.document;
  const position = editor.selection.active;
  const file = document.uri.fsPath;
  const line = position.line + 1; // 1-indexed
  const content = document.getText();

  // Server computes display positions and returns the note at the line, or null
  return client.request<Note | null>('notes/get-at-position', { file, line, content });
}
