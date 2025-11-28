# Hemis Emacs UI

- Backend: Rust JSON-RPC server over stdio (`backend/` bin in the Cargo workspace).
- Client: `hemis.el` provides notes overlays and a notes list; uses built-in Emacs `jsonrpc`.
- Doom: module stubs live in `emacs/doom/tools/hemis/`.

Quick start:

```elisp
(add-to-list 'load-path "/path/to/hemis/emacs")
(require 'hemis)
;; Point to the built Rust backend (or set env HEMIS_BACKEND).
(setq hemis-backend (expand-file-name "target/debug/backend" "/path/to/hemis"))
```

Use `M-x hemis-notes-mode` in a buffer to show sticky notes; `C-c h a` to add a note, `C-c h r` to refresh, `C-c h l` to list notes. `M-x hemis-shutdown` stops the backend.
Indexing/search: `C-c h i` to index the current file; `C-c h s` to search indexed files in the current project. Backend stores notes/files in SQLite (`HEMIS_DB_PATH` env var can override the default location).
Project ops: `M-x hemis-open-project` to set a project root; `M-x hemis-list-files` to browse backend-visible files; `M-x hemis-view-file` to fetch a file through the backend.
Explain/snapshots: select a region and `M-x hemis-explain-region`; `M-x hemis-save-snapshot` / `M-x hemis-load-snapshot` to write/load snapshot summaries.

## Testing

Run Rust backend + Emacs ERT suite from the repo root:

```bash
./scripts/run-rust-tests.sh
```

Run only the Emacs ERT suite:

```bash
HEMIS_BACKEND=/path/to/target/debug/backend \
emacs -Q --batch \
  -L emacs \
  -l hemis.el \
  -L emacs/tests \
  -l hemis-test.el \
  -f ert-run-tests-batch-and-exit
```

Tree-sitter tests skip automatically if the runtime lacks `treesit` or `rust-ts-mode`.
When `hemis-auto-install-treesit-grammars` is non-nil (default), Hemis will attempt
to install the Rust Tree-sitter grammar automatically if missing.

## Semantic search

Set `HEMIS_EMBED_URL` to point at an embedding HTTP endpoint (POST body `{ "text": "..." }`, response `{ "vector": [f32...] }`). `hemis/search` blends semantic hits when `vector` is provided; Emacs search displays scores alongside results.
