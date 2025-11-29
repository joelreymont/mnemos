# Hemis: A Second Brain for Your Code

Hemis attaches persistent notes to code locations, anchored to Tree-sitter nodes. Notes survive refactoring and are searchable across your project.

## Features

- Notes anchored to AST nodes (functions, classes, etc.)
- Git-aware staleness detection
- Full-text and semantic search
- Note linking with `[[description][id]]` syntax
- Project-wide indexing

## Architecture

```
┌─────────────┐     stdio/JSON-RPC     ┌─────────────┐
│   Emacs     │◄──────────────────────►│             │
│   hemis.el  │                        │             │
└─────────────┘                        │             │
                                       │   Backend   │
┌─────────────┐     stdio/JSON-RPC     │   (Rust)    │
│   Neovim    │◄──────────────────────►│             │
│   hemis.lua │                        │             │
└─────────────┘                        │             │
                                       │             │
┌─────────────┐     stdio/JSON-RPC     │             │
│   VS Code   │◄──────────────────────►│             │
│   extension │                        └──────┬──────┘
└─────────────┘                               │
                                              ▼
                                       ┌─────────────┐
                                       │   SQLite    │
                                       │   Database  │
                                       └─────────────┘
```

Each editor runs its own backend process, but they share the same database file at `~/.hemis/hemis.db`. Notes created in one editor appear in the other after refresh.

## Database Location

By default, Hemis stores notes in `~/.hemis/hemis.db`. This enables sharing notes across Emacs, Neovim, and VS Code simultaneously.

To use a different location (e.g., for project-specific databases):

```bash
export HEMIS_DB_PATH=/path/to/custom.db
```

Or configure it in your editor settings.

## Quick Start

### Build the Backend

```bash
git clone https://github.com/joelreymont/hemis
cd hemis
cargo build --release
```

The binary is at `target/release/hemis`.

### Installation

See the setup guide for your editor:

- **Doom Emacs**: [docs/DOOM-EMACS.md](docs/DOOM-EMACS.md)
- **Neovim (LazyVim)**: [docs/NEOVIM.md](docs/NEOVIM.md)
- **VS Code**: [ui/vscode/README.md](ui/vscode/README.md)
- **Vanilla Emacs**: [ui/emacs/README.md](ui/emacs/README.md)

## Key Bindings

All editors use a consistent `<prefix> h` pattern:

| Action | Emacs | Neovim | VS Code |
|--------|-------|--------|---------|
| Add note | `C-c h a` | `<leader>ha` | `Ctrl+Shift+H A` |
| List notes | `C-c h l` | `<leader>hl` | `Ctrl+Shift+H L` |
| Refresh | `C-c h r` | `<leader>hr` | `Ctrl+Shift+H R` |
| Search | `C-c h s` | `<leader>hs` | `Ctrl+Shift+H S` |
| Index file | `C-c h i` | `<leader>hi` | `Ctrl+Shift+H I` |
| Index project | `C-c h p` | `<leader>hp` | `Ctrl+Shift+H P` |
| Insert link | `C-c h k` | `<leader>hk` | `Ctrl+Shift+H K` |
| Help | `C-c h ?` | `<leader>h?` | - |

## Testing

```bash
# Run all tests (Rust + Emacs ERT)
./scripts/run-rust-tests.sh

# Rust tests only
cargo test

# Neovim tests (requires plenary.nvim)
nvim --headless -c "PlenaryBustedDirectory ui/neovim/tests/ {minimal_init = 'ui/neovim/tests/minimal_init.lua'}"
```

## Project Structure

```
hemis/
  backend/              # Rust JSON-RPC server
    src/
    crates/             # notes, index, storage, git, rpc
    tests/
  ui/
    emacs/              # Emacs client (hemis.el)
    neovim/             # Neovim client (hemis.lua)
    vscode/             # VS Code extension
  docs/
    PROTOCOL.md         # JSON-RPC protocol spec
    ARCHITECTURE.md     # System design
    DOOM-EMACS.md       # Doom Emacs setup
    NEOVIM.md           # Neovim/LazyVim setup
```

## Protocol

The backend speaks JSON-RPC 2.0 over stdio with Content-Length framing (LSP-style). See [docs/PROTOCOL.md](docs/PROTOCOL.md) for the full API.

## License

MIT
