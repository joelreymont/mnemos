# Doom Emacs + Mnemos guide

This walks you through wiring Mnemos into Doom Emacs, then exercising the full flow: open a Rust file, add two notes, edit one to link to the other, and search for the note.

## Prereqs
- Rust toolchain (for `cargo build`).
- Emacs 29+ with built-in Tree-sitter; Git available in `PATH`.
- Network access for Tree-sitter grammar install (Mnemos can auto-install Rust grammar).

## Build the backend
From the repo root:
```bash
cargo build -p backend
```
This produces `target/debug/mnemos`. The backend stores data in SQLite; override with `MNEMOS_DB_PATH` if you want a custom location.

## Doom module install
1) Symlink the module:
```bash
mkdir -p ~/.config/doom/modules/tools
ln -s /Users/joel/Work/mnemos/ui/emacs/doom/tools/mnemos ~/.config/doom/modules/tools/mnemos
```
2) Enable in `~/config/doom/init.el` under `:tools`:
```lisp
mnemos
```
3) Point Doom to the local frontend in `~/.config/doom/packages.el`:
```lisp
(package! mnemos
  :recipe (:local-repo "/Users/joel/Work/mnemos/ui/emacs"
           :files ("mnemos.el")))
```
4) Configure backend path in `~/.config/doom/config.el`:
```lisp
(setq mnemos-backend "/Users/joel/Work/mnemos/target/debug/mnemos"
      mnemos-auto-install-treesit-grammars t) ;; installs Rust grammar if missing
;; Database defaults to ~/.mnemos/mnemos.db
;; Uncomment to use a different location:
;; (setq mnemos-backend-env '("MNEMOS_DB_PATH=/path/to/custom.db"))
```
5) `doom sync` and restart Emacs.

> If `doom sync` shows "Skipping mnemos because it is out-of-tree", add the `package! mnemos` stanza in your personal `~/.doom.d/packages.el` as shown above and rerun `doom sync -u`—this tells Doom where to fetch Mnemos from.

## Key bindings (notes mode)
- `C-c m a` add note at point (multiline prompt; RET inserts newline, `C-c C-c` saves)
- `C-c m r` refresh notes overlays
- `C-c m l` list notes for buffer
- `C-c m i` index current file; `C-c m p` index project
- `C-c m s` search indexed files/notes
- `C-c m k` insert note link (`[[DESC][ID]]`); typing `[[` in notes mode also triggers search

## End-to-end workflow
1) `M-x mnemos-open-project` → select `/Users/joel/Work/mnemos`.
2) Open `backend/src/lib.rs` (or any Rust file in this repo). Mnemos starts the backend and, if needed, auto-installs the Rust Tree-sitter grammar.
3) Index the file for search: `C-c m i`.
4) Create Note A at a line of interest: `C-c m a` → enter text (e.g., “Parser entry”).
5) Move to another relevant line and create Note B: `C-c m a` → enter text (e.g., “Search pipeline”).
6) Link Note B to Note A:
   - Run `C-c m l` to open `*Mnemos Notes*`, note the IDs (first column), e.g., `n1` for Note A, `n2` for Note B.
   - Use `C-c m k` inside Note B's text to search for Note A by summary and insert `[[Parser entry][n1]]`, **or** run `M-:`:
     ```lisp
     (mnemos--request "notes/update"
                     '((id . "n2")
                       (text . "Search pipeline links to [[Parser entry][n1]]")))
     ```
   - `C-c m r` to refresh overlays.
7) Search for the linked note: `C-c m s` → query "Parser". In `*Mnemos Search*`, note hits show `kind` "note"; `RET` jumps to the location.
8) View a note as Markdown: in `*Mnemos Notes*`, press `v` on a note line to open it in `*Mnemos Note*` with `markdown-mode` (falls back to `text-mode` if Markdown is unavailable).
9) Auto-enable: Mnemos now enables `mnemos-notes-mode` automatically in programming buffers via `mnemos-notes-global-mode`. Toggle globally with `M-x mnemos-notes-global-mode` if you need to disable/re-enable.

## Troubleshooting
- Backend path: ensure `mnemos-backend` points to the built binary; check `*Mnemos Log*` for process output.
- Tree-sitter: if Rust grammar fails to install automatically, re-run `M-:` `(mnemos--ensure-rust-grammar t)` in a Rust buffer; `treesit-extra-load-path` should include `~/.emacs.d/tree-sitter/`.
- Database: set `MNEMOS_DB_PATH` in `mnemos-backend-env` to separate per-project data.
