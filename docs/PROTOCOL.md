# Hemis Protocol v2 (JSON-RPC 2.0)

Transport: **stdio** (UTF-8). Accepts either newline-delimited JSON objects or LSP-style `Content-Length` framed messages. Responses mirror the framing used by the client.  
Protocol: **JSON-RPC 2.0**.

## Note Object

```jsonc
{
  "id": "string",
  "file": "/abs/path/to/file",
  "projectRoot": "/abs/path/to/project",
  "commitSha": "git commit hex (optional)",
  "blobSha": "git blob hash (optional)",
  "stale": false,
  "line": 42,
  "column": 0,
  "nodePath": ["function_definition", 0, "parameters", 1],   // optional
  "tags": ["todo", "refactor"],                              // optional
  "text": "Full note body",
  "summary": "Short preview",
  "createdAt": 1732000000,
  "updatedAt": 1732000000
}
```

## Methods

- `notes/list-for-file` → list notes for a file
- `notes/list-project` → list notes for a project
- `notes/create` → create a new note
- `notes/update` → update an existing note
- `notes/delete` → delete a note
- `notes/get` → fetch a single note by id
- `notes/search` → naive text search across notes
- `notes/list-by-node` → list notes for a specific nodePath
- `index/add-file` → store file content (for search)
- `index/search` → naive text search across indexed files
- `shutdown` → cleanly terminate the backend
- `hemis/open-project` → select a project root
- `hemis/list-files` → recursively list project files (ignores common dirs)
- `hemis/get-file` → fetch file contents
- `hemis/explain-region` → return a snippet for a file range (LLM-ready hook)
- `hemis/search` → semantic/text search (scores; blends notes when includeNotes=true, accepts optional `vector` for semantic query)
- `hemis/save-snapshot` → write a snapshot summary (version, counts, timestamps)
- `hemis/load-snapshot` → load a snapshot file
- `hemis/index-project` → index all project files under `projectRoot`

Notes attach to files, git versions, node paths, and tags. Backend returns JSON objects.

### Index

- `index/add-file` stores a file’s content for text search.
- `index/search` searches indexed files for a substring and returns hits with file/line/column/text.

### Notes
Notes are stored as Markdown and may include external links as well as references to other notes/files.
Links to other notes use the format `[[DESCRIPTION][ID]]`; the ID stays stable even if the description changes.
A double `[[` in the Emacs UI triggers a note search to help insert these links.

#### `notes/create`

Request params:

```json
{
  "file": "/abs/path/file.rs",
  "projectRoot": "/abs/path",
  "line": 10,
  "column": 2,
  "nodePath": ["function_item", "parameters"], // optional
  "commit": "HEAD sha",                        // optional (auto-detected if omitted)
  "blob": "blob sha",                          // optional (auto-detected if omitted)
  "text": "note text",
  "tags": ["todo", "rust"]                     // optional
}
```

Response: note object.

#### `notes/list-for-file`

```json
{
  "file": "/abs/path/file.rs",
  "projectRoot": "/abs/path",
  "commit": "HEAD sha",     // optional filter; when present, stale notes are excluded
  "blob": "blob sha",       // optional filter
  "includeStale": false     // optional; include notes from other commits/blobs (stale=true)
}
```

Response: array of note objects.

#### `notes/list-by-node`

Same filters as `list-for-file`, plus:

```json
{ "nodePath": ["function_item", "parameters"] }
```

#### `notes/get`

```json
{ "id": "note-id" }
```

Response: note object.

#### `notes/delete`

```json
{ "id": "note-id" }
```

Response:

```json
{ "ok": true }
```

#### `notes/update`

```json
{ "id": "note-id", "text": "new text", "tags": ["tag"] }
```

Response: updated note object.
