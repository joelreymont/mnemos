# Doom Emacs + Hemis guide

This walks you through wiring Hemis into Doom Emacs, then exercising the full flow: open a Rust file, add two notes, edit one to link to the other, and search for the note.

## Prereqs
- Rust toolchain (for `cargo build`).
- Emacs 29+ with built-in Tree-sitter; Git available in `PATH`.
- Network access for Tree-sitter grammar install (Hemis can auto-install Rust grammar).

## Build the backend
From the repo root:
```bash
cargo build -p backend
```
This produces `target/debug/backend`. The backend stores data in SQLite; override with `HEMIS_DB_PATH` if you want a custom location.

## Doom module install
1) Symlink the module:
```bash
mkdir -p ~/.config/doom/modules/tools
ln -s /Users/joel/Work/hemis/emacs/doom/tools/hemis ~/.config/doom/modules/tools/hemis
```
2) Enable in `~/config/doom/init.el` under `:tools`:
```lisp
hemis
```
3) Point Doom to the local frontend in `~/.config/doom/packages.el`:
```lisp
(package! hemis
  :recipe (:local-repo "/Users/joel/Work/hemis/emacs"
           :files ("hemis.el")))
```
4) Configure backend path and DB in `~/.config/doom/config.el`:
```lisp
(setq hemis-backend "/Users/joel/Work/hemis/target/debug/backend"
      hemis-backend-env '("HEMIS_DB_PATH=/Users/joel/Work/hemis/hemis-notes.db")
      hemis-auto-install-treesit-grammars t) ;; installs Rust grammar if missing
```
5) `doom sync` and restart Emacs.

> If `doom sync` shows “Skipping hemis because it is out-of-tree”, add the `package! hemis` stanza in your personal `~/.doom.d/packages.el` as shown above and rerun `doom sync -u`—this tells Doom where to fetch Hemis from.

## Key bindings (notes mode)
- `C-c h a` add note at point (multiline prompt; RET inserts newline, `C-c C-c` saves)
- `C-c h r` refresh notes overlays
- `C-c h l` list notes for buffer
- `C-c h i` index current file; `C-c h p` index project
- `C-c h s` search indexed files/notes
- `C-c h k` insert note link (`[[DESC][ID]]`); typing `[[` in notes mode also triggers search

## End-to-end workflow
1) `M-x hemis-open-project` → select `/Users/joel/Work/hemis`.
2) Open `backend/src/lib.rs` (or any Rust file in this repo). Hemis starts the backend and, if needed, auto-installs the Rust Tree-sitter grammar.
3) Index the file for search: `C-c h i`.
4) Create Note A at a line of interest: `C-c h a` → enter text (e.g., “Parser entry”).
5) Move to another relevant line and create Note B: `C-c h a` → enter text (e.g., “Search pipeline”).
6) Link Note B to Note A:
   - Run `C-c h l` to open `*Hemis Notes*`, note the IDs (first column), e.g., `n1` for Note A, `n2` for Note B.
   - Use `C-c h k` inside Note B’s text to search for Note A by summary and insert `[[Parser entry][n1]]`, **or** run `M-:`:
     ```lisp
     (hemis--request "notes/update"
                     '((id . "n2")
                       (text . "Search pipeline links to [[Parser entry][n1]]")))
     ```
   - `C-c h r` to refresh overlays.
7) Search for the linked note: `C-c h s` → query “Parser”. In `*Hemis Search*`, note hits show `kind` “note”; `RET` jumps to the location.
8) View a note as Markdown: in `*Hemis Notes*`, press `v` on a note line to open it in `*Hemis Note*` with `markdown-mode` (falls back to `text-mode` if Markdown is unavailable).
9) Auto-enable: Hemis now enables `hemis-notes-mode` automatically in programming buffers via `hemis-notes-global-mode`. Toggle globally with `M-x hemis-notes-global-mode` if you need to disable/re-enable.

## Troubleshooting
- Backend path: ensure `hemis-backend` points to the built binary; check `*Hemis Log*` for process output.
- Tree-sitter: if Rust grammar fails to install automatically, re-run `M-:` `(hemis--ensure-rust-grammar t)` in a Rust buffer; `treesit-extra-load-path` should include `~/.emacs.d/tree-sitter/`.
- Database: set `HEMIS_DB_PATH` in `hemis-backend-env` to separate per-project data.
