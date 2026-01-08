# Mnemos Emacs UI

- Backend: Zig JSON-RPC server (`mnemos` binary).
- Client: `mnemos.el` provides notes overlays and a notes list; uses built-in Emacs `jsonrpc`.
- Doom: module stubs live in `ui/emacs/doom/tools/mnemos/`.

Quick start:

```elisp
(add-to-list 'load-path "/path/to/mnemos/ui/emacs")
(require 'mnemos)
;; Point to the built backend (or set env MNEMOS_BACKEND).
(setq mnemos-backend (expand-file-name "zig-out/bin/mnemos" "/path/to/mnemos"))
```

Use `M-x mnemos-notes-mode` in a buffer to show sticky notes; `C-c m a` to add a note, `C-c m r` to refresh, `C-c m l` to list notes. `M-x mnemos-shutdown` stops the backend.
Search: `C-c m s` to search notes and files via ripgrep. Notes are stored as Markdown in `<project>/.mnemos/notes` (override with `MNEMOS_NOTES_PATH`).
Project ops: `M-x mnemos-open-project` to set a project root; `C-c m p` is legacy (search uses ripgrep).
Explain: select a region and `M-x mnemos-explain-region`.

## Testing

Run backend tests:

```bash
zig build test
```

Run the Emacs ERT suite:

```bash
MNEMOS_BACKEND=/path/to/zig-out/bin/mnemos \
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
- `C-c m p` is legacy; project search uses ripgrep directly.
- Semantic search results show kind/score/location.
