# Hemis Architecture

## Overview
- Rust backend (Cargo workspace) provides a JSON-RPC 2.0 server over stdio (`backend` bin).
- SQLite storage for notes/files/embeddings/snapshots (default `hemis-notes.db`).
- Emacs frontend (`emacs/hemis.el`) speaks JSON-RPC over stdio, renders notes as comment-style overlays, and auto-installs the Rust Tree-sitter grammar when needed.

## Workspace layout
- `backend` (bin): JSON-RPC server, framing, routing, snapshot/status, project/file/index/search endpoints, integrates crates below.
- `notes`: note model + SQLite access (create/get/update/delete/list/search), stores `commit_sha`/`blob_sha` but now always returns notes; marks `stale` when commit/blob mismatch.
- `storage`: SQLite helpers, migrations, preload/warmup, schema (notes/files/embeddings/edges).
- `index`: text indexing/search; semantic stubs integrate embeddings when available.
- `git`: git info helper using libgit2/CLI fallback for commit/blob capture.
- `rpc`: request/response helpers, framing, parse tests.
- `embedder`: optional HTTP embedder integration stub (HEMIS_EMBED_URL).

## Notes and staleness
- Notes carry `file`, `projectRoot`, `line`, `column`, `nodePath`, `commit_sha`, `blob_sha`, timestamps.
- Backend now **always returns notes** even when commit/blob differ; `stale` boolean marks mismatches. `includeStale` is kept for API compatibility but server-side filtering by commit/blob was removed to allow re-anchoring.
- Emacs includes `includeStale: true` in note fetches and re-anchors overlays to the line start of the node; multiple notes at a position are rendered as a comment block above the node.

## Emacs frontend
- `hemis-notes-mode` auto-enables in `prog-mode`; keymap under `C-c h …`.
- Overlays: full note text as comment block above the node (no underlines/boxes); re-anchored to Tree-sitter node start or line start fallback.
- Backend lifecycle: kills any running `hemis-backend*` processes before start/reload; keymaps self-heal on reload; `hemis-reload` shuts down backend before reloading.
- Treesit: auto-install Rust grammar when missing; remaps rust-ts-mode to rust.

## Testing
- Rust: `scripts/run-rust-tests.sh` runs cargo tests and snapshot/integration suites.
- Emacs: same script runs ERT against the Rust backend (batch).
- Protocol snapshots live in `backend/tests/snapshots`; note/list/search behaviors are snapshot/flow-tested including stale flags.
