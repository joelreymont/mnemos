# 4. Indexing and Project State

Mnemos builds a multi-layer index over your project.

## 4.1 Components

- **File scanner** — discovers files to index based on config.
- **AST engine** — Tree-sitter based parsers per language.
- **Symbol graph** — global representation of definitions and references.
- **Embedding index** — vector representations for search and clustering.
- **Notes and backlinks** — user-authored annotations and cross-links.

## 4.2 Pipeline

1. Discover files under the project root.
2. For each file:
   - Parse into AST.
   - Extract declarations, definitions, and references.
3. Update symbol graph:
   - Nodes: symbols, files, notes.
   - Edges: “defines”, “refers-to”, “explains”, “duplicates”, etc.
4. Generate embeddings:
   - Functions and methods.
   - Important comments/docstrings.
   - Notes and higher-level summaries.
5. Persist index state:
   - Lightweight per-file caches.
   - Global symbol/embedding databases.
   - Note graph and backlinks.

## 4.3 Project Snapshots

Mnemos may periodically write snapshots:

- Project metadata.
- Index version and layout.
- Summaries of key areas.
- Checkpoints for faster resume.

Snapshots allow:
- Quickly reopening large projects.
- Migrating between machines or model versions with less work.
