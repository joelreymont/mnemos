![Mnemos](assets/mnemos.jpg)

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

## Why Markdown Storage?

Mnemos stores your notes as plain markdown files in `.mnemos/notes/`. This design choice provides significant benefits:

- **Human-readable** - Notes stored as plain markdown files you can read and edit anywhere
- **Git-friendly** - Track note history, diff changes, and collaborate with your team
- **No lock-in** - Your notes are just files you own, not trapped in a database
- **Portable** - Share the `.mnemos/` folder with your team via git or any file sync
- **Editor-agnostic** - Edit notes directly in any text editor, IDE, or even GitHub

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
                                           ┌───────────────────┐
                                           │  .mnemos/notes/   │
                                           │    *.md files     │
                                           │  (plain markdown) │
                                           └───────────────────┘
```

All editors connect to a single backend process via Unix domain socket. The first editor to need the backend starts it; subsequent editors connect. Backend shuts down after 30s with no connections.

## Notes Storage Location

Mnemos stores notes as markdown files in `.mnemos/notes/` within your project directory. Each note is a standalone `.md` file with YAML frontmatter containing metadata (anchor information, timestamps, links).

```
your-project/
  .mnemos/
    notes/
      abc123.md    # Individual note files
      def456.md    # Named by unique ID
      ...
```

This enables:
- **Version control**: Commit `.mnemos/` alongside your code
- **Team collaboration**: Share notes via git push/pull
- **Backup**: Notes are backed up with your normal file backups
- **Direct editing**: Open any `.md` file in your favorite editor

To use a different location:

```bash
export MNEMOS_NOTES_PATH=/path/to/custom/notes/
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
      notes/            # Note model and markdown storage
      rpc/              # JSON-RPC framing
      storage/          # Markdown file helpers
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
