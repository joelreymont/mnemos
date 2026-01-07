# Mnemos: A Second Brain for Your Code

Mnemos attaches persistent notes to code locations, anchored to Tree-sitter nodes. Notes survive refactoring and are searchable across your project.

## Features

- Notes anchored to AST nodes (functions, classes, etc.)
- Git-aware staleness detection with reattachment
- Full-text search across notes and code
- Note linking with `[[description][id]]` syntax and backlinks
- AI-powered code explanations (optional)
- Project-wide indexing
- Selected note model for precise operations

## Architecture

```
┌─────────────┐                        ┌─────────────────────────────┐
│   Emacs     │     Unix Socket        │                             │
│   mnemos.el │◄──────────────────────►│                             │
└─────────────┘                        │                             │
               ~/.mnemos/mnemos.sock   │   Backend Server            │
┌─────────────┐     Unix Socket        │   (Single Process)          │
│   Neovim    │◄──────────────────────►│                             │
│   mnemos.lua│                        │   - Reference counting      │
└─────────────┘                        │   - Grace period shutdown   │
                                       │   - Version checking        │
┌─────────────┐     Unix Socket        │                             │
│   VS Code   │◄──────────────────────►│                             │
│   extension │                        └──────────────┬──────────────┘
└─────────────┘                                       │
                                                      ▼
                                               ┌─────────────┐
                                               │   SQLite    │
                                               │  ~/.mnemos/ │
                                               │  mnemos.db  │
                                               └─────────────┘
```

All editors connect to a single backend process via Unix domain socket. The first editor to need the backend starts it; subsequent editors connect. Backend shuts down after 30s with no connections.

## Database Location

By default, Mnemos stores notes in `~/.mnemos/mnemos.db`. This enables sharing notes across Emacs, Neovim, and VS Code simultaneously.

To use a different location (e.g., for project-specific databases):

```bash
export MNEMOS_DB_PATH=/path/to/custom.db
```

## Quick Start

### Build the Backend

```bash
git clone https://github.com/joelreymont/mnemos
cd mnemos
cargo build --release
```

The binary is at `target/release/mnemos`.

### Installation

See the setup guide for your editor:

- **Doom Emacs**: [docs/DOOM-EMACS.md](docs/DOOM-EMACS.md)
- **Neovim (LazyVim)**: [docs/NEOVIM.md](docs/NEOVIM.md)
- **VS Code**: [ui/vscode/README.md](ui/vscode/README.md)
- **Vanilla Emacs**: [ui/emacs/README.md](ui/emacs/README.md)

## Key Bindings

All editors use a consistent `<prefix> h` pattern:

| Action | Emacs | Neovim | Description |
|--------|-------|--------|-------------|
| Add note | `C-c h a` | `<leader>ha` | Add note at cursor |
| Add note (multiline) | - | `<leader>hA` | Add multiline note |
| Select note | `C-c h s` | `<leader>hs` | Select note at cursor |
| List notes | `C-c h l` | `<leader>hl` | List notes in file |
| Refresh | `C-c h r` | `<leader>hr` | Refresh note display |
| Edit note | `C-c h e` | `<leader>he` | Edit selected note |
| Edit note (buffer) | `C-c h E` | `<leader>hE` | Edit in full buffer |
| Delete note | `C-c h d` | `<leader>hd` | Delete selected note |
| Reattach note | `C-c h R` | `<leader>hR` | Reattach stale note |
| Search file | - | `<leader>hf` | Search notes in file |
| Search project | `C-c h f` | `<leader>hF` | Search project |
| Index file | - | `<leader>hP` | Index current file |
| Index project | `C-c h p` | `<leader>hp` | Index entire project |
| Insert link | `C-c h k` | `<leader>hk` | Insert note link |
| Show backlinks | `C-c h b` | `<leader>hb` | Show notes linking here |
| Explain region | `C-c h x` | `<leader>hx` | Explain code (visual) |
| Explain (detailed) | `C-c h X` | `<leader>hX` | Detailed AI explanation |
| Status | `C-c h S` | `<leader>ht` | Show backend status |
| Help | `C-c h ?` | `<leader>h?` | Show keybindings |

## AI Features

Mnemos can use AI to generate code explanations. Set the provider:

```bash
export MNEMOS_AI_PROVIDER=claude  # or codex, none
```

Use `<leader>hx` (visual mode) to explain selected code.

## Testing

```bash
# Rust tests
cargo test

# Neovim tests (requires plenary.nvim)
cd ui/neovim && nvim --headless -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"

# Emacs tests
emacs -Q --batch -L ui/emacs -l mnemos.el \
  -L ui/emacs/tests -l mnemos-test.el -f ert-run-tests-batch-and-exit
```

## Project Structure

```
mnemos/
  backend/              # Rust JSON-RPC server
    src/
    crates/
      git/              # Git operations (libgit2/CLI)
      index/            # Text indexing and search
      notes/            # Note model and SQLite access
      rpc/              # JSON-RPC framing
      storage/          # SQLite helpers, migrations
      treesitter/       # Tree-sitter parsing
  ui/
    emacs/              # Emacs client (mnemos.el)
    neovim/             # Neovim client (lua)
    vscode/             # VS Code extension
  docs/
    PROTOCOL.md         # JSON-RPC protocol spec
    ARCHITECTURE.md     # System design
    DOOM-EMACS.md       # Doom Emacs setup
    NEOVIM.md           # Neovim/LazyVim setup
```

## Protocol

The backend speaks JSON-RPC 2.0 over Unix domain socket (`~/.mnemos/mnemos.sock`). See [docs/PROTOCOL.md](docs/PROTOCOL.md) for the full API.

## License

MIT
