# 11. Snapshots

## 11.1 Why Snapshots

Large projects can take time to index. Snapshots allow:

- Fast reopen.
- Rollbacks for experiments.
- Stable context for reproducible sessions.

## 11.2 Contents

A snapshot might include:

- Project config.
- Index version.
- Summary of major subsystems.
- Pointers to embeddings and notes.
- Checksums for model + source tree to detect drift.

## 11.3 Format

Typically JSON or a compact binary format:

```json
{
  "version": 1,
  "project_id": "my-cool-project",
  "created_at": "2025-11-16T12:34:56Z",
  "subsystems": [
    {"name": "parser", "summary": "..."},
    {"name": "backend", "summary": "..."}
  ]
}
```
