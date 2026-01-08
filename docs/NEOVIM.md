# Neovim + Mnemos Setup Guide

This guide walks you through setting up Mnemos with Neovim/LazyVim.

## Prerequisites

- Neovim 0.9+ (for native Tree-sitter and extmarks)
- Rust toolchain (to build the backend)
- Git in PATH

## Build the Backend

```bash
cd /path/to/mnemos
cargo build --release
```

This produces `target/release/mnemos`.

## LazyVim Installation

Add to `~/.config/nvim/lua/plugins/mnemos.lua`:

```lua
return {
  {
    dir = "/path/to/mnemos/ui/neovim",
    dependencies = {},
    opts = {
      backend = "/path/to/mnemos/target/release/mnemos",
      -- Database defaults to ~/.mnemos/mnemos.db
      -- Uncomment to use a different location:
      -- backend_env = { MNEMOS_DB_PATH = "/path/to/custom.db" },
      auto_refresh = true,
      keymaps = true,
      keymap_prefix = "<leader>m",
      display_style = "full",  -- "full" or "minimal"
    },
    keys = {
      { "<leader>ma", "<cmd>MnemosAddNote<cr>", desc = "Mnemos: Add note" },
      { "<leader>mA", "<cmd>MnemosAddNoteMultiline<cr>", desc = "Mnemos: Add note (multiline)" },
      { "<leader>ml", "<cmd>MnemosListNotes<cr>", desc = "Mnemos: List notes" },
      { "<leader>mr", "<cmd>MnemosRefresh<cr>", desc = "Mnemos: Refresh" },
      { "<leader>ms", "<cmd>MnemosSearch<cr>", desc = "Mnemos: Search" },
      { "<leader>mi", "<cmd>MnemosIndexFile<cr>", desc = "Mnemos: Index file" },
      { "<leader>mp", "<cmd>MnemosIndexProject<cr>", desc = "Mnemos: Index project" },
      { "<leader>mk", "<cmd>MnemosInsertLink<cr>", desc = "Mnemos: Insert link" },
      { "<leader>m?", "<cmd>MnemosHelp<cr>", desc = "Mnemos: Help" },
    },
  },
}
```

Restart Neovim or run `:Lazy sync`.

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `<leader>ma` | `:MnemosAddNote` | Add note at cursor |
| `<leader>mA` | `:MnemosAddNoteMultiline` | Add note (floating window) |
| `<leader>ml` | `:MnemosListNotes` | List notes for current file |
| `<leader>mr` | `:MnemosRefresh` | Refresh note display |
| `<leader>md` | `:MnemosDeleteNote` | Delete note at cursor |
| `<leader>me` | `:MnemosEditNote` | Edit note at cursor |
| `<leader>ms` | `:MnemosSearch` | Search notes and files |
| `<leader>mi` | `:MnemosIndexFile` | Index current file |
| `<leader>mp` | `:MnemosIndexProject` | Index all project files |
| `<leader>mk` | `:MnemosInsertLink` | Insert link to another note |
| `<leader>m?` | `:MnemosHelp` | Show keybindings |

## Commands

```vim
:MnemosAddNote          " Add note at cursor (single line)
:MnemosAddNoteMultiline " Add note in floating window
:MnemosListNotes        " Open notes list buffer
:MnemosRefresh          " Refresh note display
:MnemosDeleteNote       " Delete note at cursor
:MnemosEditNote         " Edit note at cursor
:MnemosSearch           " Search notes and indexed files
:MnemosIndexFile        " Index current file for search
:MnemosIndexProject     " Index all project files
:MnemosInsertLink       " Insert [[desc][id]] link
:MnemosStatus           " Show backend status
:MnemosShutdown         " Stop backend process
:MnemosHelp             " Show keybindings
```

## Note Display

Notes appear as comment blocks above their anchored line:

```rust
// This is a note about the function
// It can have multiple lines
fn my_function() {
    // ...
}
```

Set `display_style = "minimal"` for a compact indicator:

```rust
fn my_function() {  [n:a1b2c3d4]
    // ...
}
```

## Notes List Buffer

`:MnemosListNotes` opens a split showing all notes for the current file:

```
Mnemos notes for main.rs

  0 [a1b2c3d4] L10,C0
    This is the first note

  1 [e5f6g7h8] L25,C4
    Another note here
```

- `j/k` - Navigate
- `Enter` - Jump to note location
- `q` - Close buffer

## Workflow

1. Open a source file
2. Index it: `<leader>mi`
3. Add notes at interesting locations: `<leader>ma`
4. View all notes: `<leader>ml`
5. Search across project: `<leader>ms`
6. Link notes together: `<leader>mk`

## Multi-Editor Support

Mnemos works with Emacs, Neovim, and VS Code. Each editor runs its own backend process, but they all share the same SQLite database at `~/.mnemos/mnemos.db` by default. Notes created in one editor appear in the others after refresh.

To use a project-specific database, set the environment variable:
- Neovim: `backend_env = { MNEMOS_DB_PATH = "/path/to/project.db" }`
- Emacs: `mnemos-backend-env '("MNEMOS_DB_PATH=/path/to/project.db")`
- Shell: `export MNEMOS_DB_PATH=/path/to/project.db`

## Troubleshooting

### Backend not starting

Check that the binary exists and is executable:

```bash
ls -la /path/to/mnemos/target/release/mnemos
```

Check `:messages` for error details.

### Notes not displaying

1. Ensure the file is saved (notes attach to file paths)
2. Run `:MnemosRefresh` to reload notes
3. Check `:MnemosStatus` for backend health

### Tree-sitter not working

Mnemos uses Tree-sitter for node anchoring. Install the parser for your language:

```vim
:TSInstall rust
```

## Testing

Run tests with plenary.nvim:

```bash
nvim --headless -u ui/neovim/tests/minimal_init.lua \
  -c "PlenaryBustedDirectory ui/neovim/tests/ {minimal_init = 'ui/neovim/tests/minimal_init.lua'}"
```
