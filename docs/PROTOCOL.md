# Hemis Protocol v2 (JSON-RPC 2.0)

Transport: **stdio** (one JSON object per line, UTF-8).  
Protocol: **JSON-RPC 2.0**.

## Note Object

```jsonc
{
  "id": "string",
  "file": "/abs/path/to/file",
  "projectRoot": "/abs/path/to/project",
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
- `notes/search` → naive text search across notes
- `shutdown` → cleanly terminate the backend
