# Hemis Emacs UI

- Backend: Common Lisp JSON-RPC server over stdio at `hemis-project/backend/hemis.lisp` (run with `sbcl --script`).
- Client: `hemis.el` provides notes overlays and a notes list; uses built-in Emacs `jsonrpc`.
- Doom: module stubs live in `emacs/doom/tools/hemis/`.

Quick start:

```elisp
(add-to-list 'load-path "/path/to/hemis/emacs")
(require 'hemis)
;; Optional: adjust if you use a different Lisp or backend script path
(setq hemis-executable "sbcl"
      hemis-backend-script (expand-file-name "hemis-project/backend/hemis.lisp" "/path/to/hemis"))
```

Use `M-x hemis-notes-mode` in a buffer to show sticky notes; `C-c h a` to add a note, `C-c h r` to refresh, `C-c h l` to list notes. `M-x hemis-shutdown` stops the backend.
