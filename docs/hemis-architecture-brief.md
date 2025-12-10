# Hemis Architecture Brief

This document explains how Hemis works for AI assistants or developers who have no prior knowledge of the project and cannot access the repository.

## What is Hemis?

Hemis is a **"second brain for your code"** - a system that lets developers attach persistent, searchable notes to specific locations in source code. The key innovation is that notes survive refactoring because they're anchored to **AST nodes** (via Tree-sitter) rather than fragile line numbers.

**Core value proposition:**
- Notes attached to functions, classes, or blocks persist when code moves
- Multiple editors (Emacs, Neovim, VS Code) share the same notes database
- AI can auto-generate explanations for code regions
- Full-text and semantic search across notes and code

## System Architecture

```
┌─────────────┐                           ┌──────────────────────────┐
│   Emacs     │──────┐                    │                          │
│ (hemis.el)  │      │   Unix Socket      │    Backend Server        │
└─────────────┘      │   ~/.hemis/        │    (Single Rust Process) │
                     │   hemis.sock       │                          │
┌─────────────┐      │                    │  ┌────────────────────┐  │
│  Neovim     │──────┼───────────────────►│  │  JSON-RPC 2.0      │  │
│ (Lua plugin)│      │                    │  │  Request Handler   │  │
└─────────────┘      │                    │  └─────────┬──────────┘  │
                     │                    │            │             │
┌─────────────┐      │                    │  ┌─────────▼──────────┐  │
│  VS Code    │──────┘                    │  │  Tree-sitter       │  │
│ (extension) │                           │  │  AST Parsing       │  │
└─────────────┘                           │  └─────────┬──────────┘  │
       ▲                                  │            │             │
       │                                  │  ┌─────────▼──────────┐  │
       │  Events Socket                   │  │  SQLite Storage    │  │
       │  ~/.hemis/events.sock            │  │  ~/.hemis/hemis.db │  │
       │                                  │  └────────────────────┘  │
       │                                  │                          │
       └──────────────────────────────────┤  ┌────────────────────┐  │
                                          │  │  AI CLI Integration│  │
                                          │  │  (Codex/Claude)    │  │
                                          │  └────────────────────┘  │
                                          └──────────────────────────┘
```

## Backend Server

The backend is a **single Rust process** that serves all editor clients. It uses **reference counting** - when the last client disconnects, it waits 30 seconds then shuts down gracefully.

### Communication Protocol

**JSON-RPC 2.0** over Unix domain sockets with newline-delimited JSON framing.

Request format:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "notes/create",
  "params": {
    "file": "/path/to/file.rs",
    "projectRoot": "/path/to",
    "line": 42,
    "column": 4,
    "text": "This function handles authentication",
    "content": "fn authenticate(user: &User) { ... }"
  }
}
```

Response format:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "shortId": "abc123",
    "file": "/path/to/file.rs",
    "line": 42,
    "column": 4,
    "text": "This function handles authentication",
    "summary": "This function handles authentication",
    "stale": false,
    "formattedLines": ["    // This function handles authentication"],
    "displayMarker": "[n:abc123]",
    "displayLabel": "[Note] This function handles authentication"
  }
}
```

### RPC Methods

**Notes operations:**
| Method | Purpose |
|--------|---------|
| `notes/create` | Create a note at a code location |
| `notes/get` | Fetch a single note by ID |
| `notes/update` | Update note text/tags (saves version history) |
| `notes/delete` | Delete a note |
| `notes/list-for-file` | Get all notes in a file with staleness info |
| `notes/list-project` | Get all notes in a project |
| `notes/list-by-node` | Filter notes by AST node path |
| `notes/search` | Full-text search across notes |
| `notes/backlinks` | Find notes that link to a given note |
| `notes/reattach` | Re-anchor a stale note to new location |
| `notes/buffer-update` | Update note positions after buffer edit |

**Indexing operations:**
| Method | Purpose |
|--------|---------|
| `index/add-file` | Index file content for text search |
| `index/search` | Search indexed file content |

**Project operations:**
| Method | Purpose |
|--------|---------|
| `hemis/search` | Unified search (text + semantic if embeddings exist) |
| `hemis/index-project` | Index all files in project (async supported) |
| `hemis/status` | Get counts (notes, files, embeddings) |
| `hemis/explain-region` | Get AI explanation for code region |
| `hemis/project-meta` | Get project indexing state |

## Note Anchoring (Tree-sitter Integration)

When a note is created, the backend:

1. **Parses the file** with Tree-sitter to build an AST
2. **Finds the AST node** at the given (line, column)
3. **Computes anchor position** - adjusts to the start of the containing significant node (function, class, statement)
4. **Extracts node path** - a sequence of node types from root to target (e.g., `["source_file", "function_item", "block", "let_declaration"]`)
5. **Computes node text hash** - SHA256 of the node's source text

This creates a **stable anchor** that can be re-located even if the code moves.

### Staleness Detection

Notes track git state:
- `commit_sha` - git commit when note was created
- `blob_sha` - git blob hash of the file content
- `node_text_hash` - SHA256 of the anchored node's text

When fetching notes, the backend compares stored hashes with current state:
- If `blob_sha` differs → file content changed
- If `node_text_hash` differs → the specific code block changed
- Either case marks `stale: true`

Stale notes are displayed differently (grayed out) and can be manually re-anchored.

## Storage (SQLite)

All data stored in `~/.hemis/hemis.db`.

**Notes table:**
```sql
CREATE TABLE notes (
  id TEXT PRIMARY KEY,           -- UUID
  file TEXT NOT NULL,            -- Canonical file path
  project_root TEXT NOT NULL,    -- Project root directory
  line INTEGER NOT NULL,         -- 1-indexed line number
  column INTEGER NOT NULL,       -- 0-indexed column
  node_path TEXT,                -- JSON array of node types
  node_text_hash TEXT,           -- SHA256 of node text
  tags TEXT,                     -- JSON array of tags
  text TEXT NOT NULL,            -- Note content
  summary TEXT,                  -- Short summary (first line)
  commit_sha TEXT,               -- Git commit hash
  blob_sha TEXT,                 -- Git blob hash
  created_at INTEGER,            -- Unix timestamp
  updated_at INTEGER             -- Unix timestamp
);
```

**Files table (for text search):**
```sql
CREATE TABLE files (
  file TEXT PRIMARY KEY,
  project_root TEXT NOT NULL,
  content TEXT,                  -- Full file content
  updated_at INTEGER
);
```

**Note versions table (history):**
```sql
CREATE TABLE note_versions (
  note_id TEXT,
  version INTEGER,
  text TEXT,
  line INTEGER,
  column INTEGER,
  node_path TEXT,
  commit_sha TEXT,
  blob_sha TEXT,
  reason TEXT,                   -- Why version was created
  created_at INTEGER,
  PRIMARY KEY (note_id, version)
);
```

## Event System

The backend broadcasts events to all connected clients via a separate socket (`~/.hemis/events.sock`).

Event types:
```
NoteCreated { id, file, line, project_root }
NoteUpdated { id }
NoteDeleted { id }
NotePositionChanged { id, file, old_line, new_line, stale }
AiComplete { note_id, success, provider }
IndexComplete { project, files_indexed }
FileIndexed { file, project }
```

Events are JSON lines. UIs subscribe to this socket and refresh their displays when relevant events arrive.

## UI Plugin Architecture

All UI plugins follow the same pattern:

1. **On startup:** Connect to backend socket (start backend if needed)
2. **On file open:** Call `notes/list-for-file` to get notes, render as overlays/virtual text
3. **On user action:** Send RPC request, handle response, update display
4. **On event:** Refresh affected buffers

### Display Rendering

The backend pre-computes display fields so UIs don't need to format:

- `formattedLines` - Comment-prefixed lines with correct indentation
- `displayMarker` - Short marker like `[n:abc123]`
- `displayLabel` - For lists: `[Note] Summary text`
- `hoverText` - Markdown for hover tooltips
- `iconHint` - `"fresh"` or `"stale"` for icon selection

**Indentation:** `formattedLines` include leading spaces matching the note's column position, so notes visually align with the code they annotate:

```rust
fn main() {
    let x = 1;
    // [Note] This variable stores the counter  <- indented to match
}
```

### Neovim Rendering

Uses Neovim's extmarks with `virt_lines` (virtual lines above actual line):

```lua
vim.api.nvim_buf_set_extmark(buf, ns_id, line - 1, 0, {
  virt_lines = {
    {{ "    // Note text here", "HemisNote" }}
  },
  virt_lines_above = true
})
```

### Emacs Rendering

Uses overlays positioned at the anchor line:

```elisp
(let ((ov (make-overlay start end)))
  (overlay-put ov 'before-string
    (propertize "// Note text\n" 'face 'hemis-note-face)))
```

### VS Code Rendering

Uses decoration API with `before` text decorations.

## AI Integration

The backend can call local AI CLI tools (Codex or Claude) to generate note content.

**Flow:**
1. User selects code region and triggers "explain"
2. UI sends `hemis/explain-region` with file path and line range
3. Backend extracts code snippet with context (±20 lines)
4. Backend spawns AI CLI subprocess with the snippet
5. AI returns explanation
6. Backend creates note with AI-generated content
7. `NoteCreated` event triggers UI refresh

**Rate limiting:** Max 2 concurrent AI calls, 10 calls/minute to prevent runaway costs.

**Provider selection:**
- Default: `codex` CLI on PATH
- Override: `HEMIS_AI_PROVIDER=claude` environment variable
- Disable: `HEMIS_AI_PROVIDER=none`

## Path Handling

**Critical detail:** The backend canonicalizes all paths to resolve symlinks. On macOS, `/tmp` is a symlink to `/private/tmp`. The backend stores `/private/tmp/...` but UIs may use `/tmp/...`.

UIs must either:
1. Canonicalize paths before comparing, or
2. Use `vim.fn.resolve()` / `file-truename` when matching event paths to buffers

## Lifecycle Management

**Starting the backend:**
1. Check for existing socket at `~/.hemis/hemis.sock`
2. If socket exists, try to connect (backend already running)
3. If connection fails, check lock file `~/.hemis/hemis.lock` for stale PID
4. Kill stale process if needed
5. Spawn new backend process

**Shutdown:**
- Backend tracks connected clients with reference count
- When count reaches 0, start 30-second grace timer
- If no new connections in 30 seconds, exit cleanly
- New connection cancels shutdown timer

## Summary

Hemis is a client-server system where:

- **Backend** (Rust): Single process handling JSON-RPC, SQLite storage, Tree-sitter parsing, AI integration
- **UI plugins** (Emacs/Neovim/VS Code): Thin clients that render notes and send commands
- **Protocol**: JSON-RPC 2.0 over Unix sockets + event broadcasting
- **Anchoring**: Notes attached to AST nodes via Tree-sitter, survive refactoring
- **Staleness**: Git hashes detect when code changes, mark notes for review
- **AI**: Optional local CLI integration for auto-generating explanations

The architecture enables multiple editors to share the same notes database with real-time synchronization via events.
