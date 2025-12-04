# Hemis Emacs UI

- Backend: Rust JSON-RPC server over stdio (`hemis` binary in the Cargo workspace).
- Client: `hemis.el` provides notes overlays and a notes list; uses built-in Emacs `jsonrpc`.
- Doom: module stubs live in `ui/emacs/doom/tools/hemis/`.

Quick start:

```elisp
(add-to-list 'load-path "/path/to/hemis/ui/emacs")
(require 'hemis)
;; Point to the built Rust backend (or set env HEMIS_BACKEND).
(setq hemis-backend (expand-file-name "target/debug/hemis" "/path/to/hemis"))
```

Use `M-x hemis-notes-mode` in a buffer to show sticky notes; `C-c h a` to add a note, `C-c h r` to refresh, `C-c h l` to list notes. `M-x hemis-shutdown` stops the backend.
Indexing/search: `C-c h i` to index the current file; `C-c h s` to search indexed files in the current project. Backend stores notes/files in SQLite (`HEMIS_DB_PATH` env var can override the default location).
Project ops: `M-x hemis-open-project` to set a project root.
Explain/snapshots: select a region and `M-x hemis-explain-region`; `M-x hemis-save-snapshot` / `M-x hemis-load-snapshot` to write/load snapshot summaries.

## Testing

Run Rust backend + Emacs ERT suite from the repo root:

```bash
./scripts/run-rust-tests.sh
```

Run only the Emacs ERT suite:

```bash
HEMIS_BACKEND=/path/to/target/debug/hemis \
emacs -Q --batch \
  -L ui/emacs \
  -l hemis.el \
  -L ui/emacs/tests \
  -l hemis-test.el \
  -f ert-run-tests-batch-and-exit
```

Tree-sitter tests skip automatically if the runtime lacks `treesit` or `rust-ts-mode`.
When `hemis-auto-install-treesit-grammars` is non-nil (default), Hemis will attempt
to install the Rust Tree-sitter grammar automatically if missing.

## Semantic search

Set `HEMIS_EMBED_URL` to point at an embedding HTTP endpoint (POST body `{ "text": "..." }`, response `{ "vector": [f32...] }`). `hemis/search` blends semantic hits when `vector` is provided; Emacs search displays scores alongside results.

Progress/status
---------------
- `C-c h p` indexes the whole project and reports counts in the minibuffer.
- Snapshot saves/loads report to the minibuffer; semantic search results show kind/score/location.
