# Mnemos Protocol v1 (JSON-RPC 2.0)

Transport: **Unix domain socket** at `~/.mnemos/mnemos.sock`. Uses LSP-style `Content-Length` framing.
Protocol: **JSON-RPC 2.0**.

## Note Object

```jsonc
{
  "id": "uuid-string",
  "shortId": "8-char-prefix",
  "file": "/abs/path/to/file",
  "projectRoot": "/abs/path/to/project",
  "commitSha": "git commit hex (optional)",
  "blobSha": "git blob hash (optional)",
  "nodeTextHash": "sha256 of anchor node text",
  "stale": false,
  "line": 42,
  "column": 0,
  "nodePath": "function_definition.parameters",
  "tags": ["todo", "refactor"],
  "text": "Full note body",
  "summary": "Short preview",
  "createdAt": 1732000000,
  "updatedAt": 1732000000
}
```

## Methods

### Notes

| Method | Description |
|--------|-------------|
| `notes/create` | Create a new note at cursor position |
| `notes/get` | Fetch a single note by ID |
| `notes/update` | Update note text/tags |
| `notes/delete` | Delete a note |
| `notes/list-for-file` | List notes for a file (with position tracking) |
| `notes/list-project` | List all notes in a project |
| `notes/list-by-node` | List notes for a specific node path |
| `notes/search` | Text search across notes |
| `notes/backlinks` | Find notes that link to a given note |
| `notes/reattach` | Reattach a stale note to a new position |
| `notes/buffer-update` | Update positions for real-time tracking |
| `notes/get-at-position` | Get note at specific line/column |
| `notes/anchor` | Get anchor info for a position |
| `notes/link-suggestions` | Suggest notes to link to |
| `notes/history` | Get version history for a note |
| `notes/get-version` | Get specific version of a note |
| `notes/restore-version` | Restore note to previous version |
| `notes/explain-and-create` | AI explain and create note |

### Index

| Method | Description |
|--------|-------------|
| `index/add-file` | Index a file's content |
| `index/search` | Search indexed file content |

### Mnemos

| Method | Description |
|--------|-------------|
| `mnemos/status` | Get backend status (note/file counts) |
| `mnemos/search` | Combined search (notes + files) |
| `mnemos/index-project` | Index all project files |
| `mnemos/explain-region` | Get code context for a region |
| `mnemos/project-meta` | Get project metadata |
| `mnemos/open-project` | Set active project root |
| `mnemos/save-snapshot` | Save database snapshot |
| `mnemos/load-snapshot` | Load database snapshot |
| `mnemos/display-config` | Get display configuration |
| `mnemos/note-templates` | Get note templates |
| `mnemos/suggest-tags` | Suggest tags for a note |
| `mnemos/graph` | Get note link graph |
| `mnemos/tasks` | List background tasks |
| `mnemos/task-status` | Get task status |
| `mnemos/task-list` | List all tasks |
| `mnemos/code-references` | Find code references |
| `mnemos/file-context` | Get file context |
| `mnemos/buffer-context` | Get buffer context |
| `mnemos/summarize-file` | AI summarize file |

## Example: Create Note

Request:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "notes/create",
  "params": {
    "file": "/path/to/file.rs",
    "line": 42,
    "column": 0,
    "text": "TODO: refactor this function",
    "content": "... file content for anchor computation ..."
  }
}
```

Response:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "shortId": "550e8400",
    "file": "/path/to/file.rs",
    "line": 42,
    "column": 0,
    "text": "TODO: refactor this function",
    "stale": false,
    "createdAt": 1732000000,
    "updatedAt": 1732000000
  }
}
```

## Example: List Notes for File

Request:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "notes/list-for-file",
  "params": {
    "file": "/path/to/file.rs",
    "content": "... current buffer content ...",
    "includeStale": true
  }
}
```

The `content` parameter enables real-time position tracking. The server returns notes with updated `line` values based on current buffer state.

## Staleness

Notes track the code they're attached to via `nodeTextHash` (SHA256 of the anchor node's text). When the hash changes, `stale: true` is set. Use `notes/reattach` to re-anchor a stale note to a new position.

## Note Links

Notes can link to other notes using `[[description][uuid]]` syntax. The `notes/backlinks` method finds notes that link to a given note ID.
