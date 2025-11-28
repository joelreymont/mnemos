# Hemis – Second Brain for Your Codebase

This repo is transitioning to a Rust backend; the Lisp backend lives on the `lisp` branch. Current layout:

- `backend/`: Rust JSON-RPC backend over stdio (Content-Length framing), wired to crates under `crates/`
- `crates/`: feature crates (notes, index/search, git integration, storage, rpc)
- `../emacs/`: Emacs UI client (`hemis.el`) plus Doom module that talks to the backend (set `hemis-backend` to the Rust binary)

Protocol details: see `../docs/PROTOCOL.md`.  
Tests: `./scripts/run-rust-tests.sh` runs `cargo test` and Emacs ERT against the Rust backend.
