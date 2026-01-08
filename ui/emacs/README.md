# Mnemos Emacs UI

- Backend: Rust JSON-RPC server over stdio (`mnemos` binary in the Cargo workspace).
- Client: `mnemos.el` provides notes overlays and a notes list; uses built-in Emacs `jsonrpc`.
- Doom: module stubs live in `ui/emacs/doom/tools/mnemos/`.

Quick start:

```elisp
(add-to-list 'load-path "/path/to/mnemos/ui/emacs")
(require 'mnemos)
;; Point to the built Rust backend (or set env MNEMOS_BACKEND).
(setq mnemos-backend (expand-file-name "target/debug/mnemos" "/path/to/mnemos"))
```

Use `M-x mnemos-notes-mode` in a buffer to show sticky notes; `C-c m a` to add a note, `C-c m r` to refresh, `C-c m l` to list notes. `M-x mnemos-shutdown` stops the backend.
Search: `C-c m s` to search indexed files in the current project. Backend stores notes/files in SQLite (`MNEMOS_DB_PATH` env var can override the default location).
Project ops: `M-x mnemos-open-project` to set a project root; `C-c m p` to index the project.
Explain: select a region and `M-x mnemos-explain-region`.

## Testing

Run Rust backend + Emacs ERT suite from the repo root:

```bash
./scripts/run-rust-tests.sh
```

Run only the Emacs ERT suite:

```bash
MNEMOS_BACKEND=/path/to/target/debug/mnemos \
emacs -Q --batch \
  -L ui/emacs \
  -l mnemos.el \
  -L ui/emacs/tests \
  -l mnemos-test.el \
  -f ert-run-tests-batch-and-exit
```

## Semantic search

Set `MNEMOS_EMBED_URL` to point at an embedding HTTP endpoint (POST body `{ "text": "..." }`, response `{ "vector": [f32...] }`). `mnemos/search` blends semantic hits when `vector` is provided; Emacs search displays scores alongside results.

Progress/status
---------------
- `C-c m p` indexes the whole project and reports counts in the minibuffer.
- Semantic search results show kind/score/location.
