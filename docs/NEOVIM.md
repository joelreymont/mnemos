# Neovim + Hemis Setup Guide

This guide walks you through setting up Hemis with Neovim/LazyVim.

## Prerequisites

- Neovim 0.9+ (for native Tree-sitter and extmarks)
- Rust toolchain (to build the backend)
- Git in PATH

## Build the Backend

```bash
cd /path/to/hemis
cargo build --release
```

This produces `target/release/hemis`.

## LazyVim Installation

Add to `~/.config/nvim/lua/plugins/hemis.lua`:

```lua
return {
  {
    dir = "/path/to/hemis/ui/neovim",
    dependencies = {},
    opts = {
      backend = "/path/to/hemis/target/release/hemis",
      -- Database defaults to ~/.hemis/hemis.db
      -- Uncomment to use a different location:
      -- backend_env = { HEMIS_DB_PATH = "/path/to/custom.db" },
      auto_refresh = true,
      keymaps = true,
      keymap_prefix = "<leader>h",
      display_style = "full",  -- "full" or "minimal"
    },
    keys = {
      { "<leader>ha", "<cmd>HemisAddNote<cr>", desc = "Hemis: Add note" },
      { "<leader>hA", "<cmd>HemisAddNoteMultiline<cr>", desc = "Hemis: Add note (multiline)" },
      { "<leader>hl", "<cmd>HemisListNotes<cr>", desc = "Hemis: List notes" },
      { "<leader>hr", "<cmd>HemisRefresh<cr>", desc = "Hemis: Refresh" },
      { "<leader>hs", "<cmd>HemisSearch<cr>", desc = "Hemis: Search" },
      { "<leader>hi", "<cmd>HemisIndexFile<cr>", desc = "Hemis: Index file" },
      { "<leader>hp", "<cmd>HemisIndexProject<cr>", desc = "Hemis: Index project" },
      { "<leader>hk", "<cmd>HemisInsertLink<cr>", desc = "Hemis: Insert link" },
      { "<leader>h?", "<cmd>HemisHelp<cr>", desc = "Hemis: Help" },
    },
  },
}
```

Restart Neovim or run `:Lazy sync`.

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `<leader>ha` | `:HemisAddNote` | Add note at cursor |
| `<leader>hA` | `:HemisAddNoteMultiline` | Add note (floating window) |
| `<leader>hl` | `:HemisListNotes` | List notes for current file |
| `<leader>hr` | `:HemisRefresh` | Refresh note display |
| `<leader>hd` | `:HemisDeleteNote` | Delete note at cursor |
| `<leader>he` | `:HemisEditNote` | Edit note at cursor |
| `<leader>hs` | `:HemisSearch` | Search notes and files |
| `<leader>hi` | `:HemisIndexFile` | Index current file |
| `<leader>hp` | `:HemisIndexProject` | Index all project files |
| `<leader>hk` | `:HemisInsertLink` | Insert link to another note |
| `<leader>h?` | `:HemisHelp` | Show keybindings |

## Commands

```vim
:HemisAddNote          " Add note at cursor (single line)
:HemisAddNoteMultiline " Add note in floating window
:HemisListNotes        " Open notes list buffer
:HemisRefresh          " Refresh note display
:HemisDeleteNote       " Delete note at cursor
:HemisEditNote         " Edit note at cursor
:HemisSearch           " Search notes and indexed files
:HemisIndexFile        " Index current file for search
:HemisIndexProject     " Index all project files
:HemisInsertLink       " Insert [[desc][id]] link
:HemisStatus           " Show backend status
:HemisShutdown         " Stop backend process
:HemisHelp             " Show keybindings
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

`:HemisListNotes` opens a split showing all notes for the current file:

```
Hemis notes for main.rs

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
2. Index it: `<leader>hi`
3. Add notes at interesting locations: `<leader>ha`
4. View all notes: `<leader>hl`
5. Search across project: `<leader>hs`
6. Link notes together: `<leader>hk`

## Multi-Editor Support

Hemis works with Emacs, Neovim, and VS Code. Each editor runs its own backend process, but they all share the same SQLite database at `~/.hemis/hemis.db` by default. Notes created in one editor appear in the others after refresh.

To use a project-specific database, set the environment variable:
- Neovim: `backend_env = { HEMIS_DB_PATH = "/path/to/project.db" }`
- Emacs: `hemis-backend-env '("HEMIS_DB_PATH=/path/to/project.db")`
- Shell: `export HEMIS_DB_PATH=/path/to/project.db`

## Troubleshooting

### Backend not starting

Check that the binary exists and is executable:

```bash
ls -la /path/to/hemis/target/release/hemis
```

Check `:messages` for error details.

### Notes not displaying

1. Ensure the file is saved (notes attach to file paths)
2. Run `:HemisRefresh` to reload notes
3. Check `:HemisStatus` for backend health

### Tree-sitter not working

Hemis uses Tree-sitter for node anchoring. Install the parser for your language:

```vim
:TSInstall rust
```

## Testing

Run tests with plenary.nvim:

```bash
nvim --headless -u ui/neovim/tests/minimal_init.lua \
  -c "PlenaryBustedDirectory ui/neovim/tests/ {minimal_init = 'ui/neovim/tests/minimal_init.lua'}"
```
