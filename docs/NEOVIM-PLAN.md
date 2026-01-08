# Neovim UI Implementation Plan

## Overview

Add a Neovim frontend for Mnemos using LazyVim conventions. The plugin will communicate with the same Rust backend over stdio JSON-RPC, providing feature parity with the Emacs UI.

## Directory Structure

```
ui/neovim/
  lua/
    mnemos/
      init.lua           # Plugin setup, public API
      rpc.lua            # JSON-RPC client (stdio, Content-Length framing)
      notes.lua          # Note CRUD operations
      display.lua        # Virtual text/lines rendering
      treesitter.lua     # Node path extraction
      commands.lua       # User commands
      telescope.lua      # Telescope integration (search, note linking)
      config.lua         # Configuration with defaults
  plugin/
    mnemos.lua           # Lazy-load entry point
  README.md
```

## Implementation Phases

### Phase 1: RPC Client

**Files**: `lua/mnemos/rpc.lua`, `lua/mnemos/config.lua`

**Features**:
- Start backend process with `vim.fn.jobstart()`
- Content-Length framing (send and receive)
- Request queue with callbacks
- Auto-restart on crash
- Shutdown on Neovim exit

**Key functions**:
```lua
M.start()              -- Start backend process
M.stop()               -- Stop backend process
M.request(method, params, callback)  -- Async request
M.request_sync(method, params)       -- Sync request (for simple cases)
```

**Config**:
```lua
{
  backend = nil,  -- Path to mnemos binary (auto-detect from plugin dir)
  backend_env = { "MNEMOS_DB_PATH=..." },
  log_level = "warn",
}
```

### Phase 2: Basic Notes

**Files**: `lua/mnemos/notes.lua`, `lua/mnemos/display.lua`

**Features**:
- Create note at cursor position
- List notes for current buffer
- Display notes as virtual lines (comment-style blocks above the line)
- Delete note
- Update note text

**Display approach**:
- Use `nvim_buf_set_extmark()` with `virt_lines` to show note text above the anchored line
- Format as comment blocks matching file type (// for Rust, # for Python, etc.)
- Store extmark IDs to clear/update on refresh

**Key functions**:
```lua
M.create(text, opts)      -- Create note at cursor
M.delete(id)              -- Delete note by ID
M.update(id, text)        -- Update note text
M.list_for_buffer()       -- Get notes for current buffer
M.refresh()               -- Refresh note display
M.get(id)                 -- Get single note
```

### Phase 3: Tree-sitter Integration

**Files**: `lua/mnemos/treesitter.lua`

**Features**:
- Get Tree-sitter node at cursor
- Build node path (list of node types from root)
- Re-anchor notes to node start on display
- Fallback to line start when Tree-sitter unavailable

**Key functions**:
```lua
M.get_node_path()         -- Get node path at cursor as list
M.get_node_start()        -- Get start position of current node
M.is_available()          -- Check if Tree-sitter is available for buffer
```

**Node path format**: Same as Emacs - `["function_item", "parameters", "identifier"]`

### Phase 4: Commands and Keybindings

**Files**: `lua/mnemos/commands.lua`, `plugin/mnemos.lua`

**User commands**:
```
:MnemosAddNote          -- Add note at cursor (opens input)
:MnemosListNotes        -- Open notes list for buffer
:MnemosRefresh          -- Refresh note display
:MnemosDeleteNote       -- Delete note at cursor
:MnemosEditNote         -- Edit note at cursor
:MnemosSearch <query>   -- Search notes/files
:MnemosIndexFile        -- Index current file
:MnemosIndexProject     -- Index all project files
:MnemosInsertLink       -- Insert note link
:MnemosStatus           -- Show backend status
:MnemosShutdown         -- Stop backend
```

**Default keybindings** (under `<leader>m`):
```
<leader>ma  -- Add note
<leader>ml  -- List notes
<leader>mr  -- Refresh notes
<leader>md  -- Delete note at cursor
<leader>me  -- Edit note at cursor
<leader>ms  -- Search
<leader>mi  -- Index file
<leader>mp  -- Index project
<leader>mk  -- Insert note link
<leader>m?  -- Help (show keybindings)
```

### Phase 5: Telescope Integration

**Files**: `lua/mnemos/telescope.lua`

**Features**:
- Notes picker (list all notes, preview, jump to location)
- Search picker (search notes and files, show scores)
- Note link picker (search notes, insert `[[desc][id]]` link)

**Pickers**:
```lua
M.notes()           -- Browse all notes
M.search(opts)      -- Search notes/files
M.note_links()      -- Insert note link
```

### Phase 6: Notes List Buffer

**Features**:
- Dedicated buffer showing notes for current file
- Navigation with j/k/n/p
- Enter to jump to note location
- d to delete note
- e to edit note
- v to view full note in floating window
- q to close

**Buffer format**:
```
Mnemos notes for src/main.rs

  0 [a1b2c3d4] L10,C0
    This is the first note
    with multiple lines

  1 [e5f6g7h8] L25,C4
    Another note here
```

### Phase 7: Note Editing

**Features**:
- Floating window for multi-line note input
- Markdown preview (optional, with render-markdown.nvim)
- `<C-c><C-c>` to save, `<C-c><C-k>` to cancel (Emacs-compatible)
- Or `<CR>` in normal mode to save, `q` to cancel

### Phase 8: Polish

**Features**:
- Stale note indicators (different highlight when commit/blob mismatch)
- Auto-refresh on BufEnter
- Lazy loading (only start backend when needed)
- Health check (`:checkhealth mnemos`)
- Which-key integration for keybinding hints

## Testing Strategy

**Unit tests** (using plenary.nvim):
- RPC framing (Content-Length encode/decode)
- Node path extraction
- Note display formatting

**Integration tests**:
- Full RPC flow against real backend
- Note CRUD operations
- Search functionality

**Test file**: `ui/neovim/tests/mnemos_spec.lua`

## Dependencies

**Required**:
- Neovim 0.9+ (for native Tree-sitter, extmarks with virt_lines)
- mnemos backend binary

**Optional**:
- telescope.nvim (for search/linking UI)
- which-key.nvim (for keybinding hints)
- render-markdown.nvim (for note preview)

## Configuration Example

```lua
-- In LazyVim: lua/plugins/mnemos.lua
return {
  {
    dir = "~/Work/mnemos/ui/neovim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
    },
    opts = {
      backend = "~/Work/mnemos/target/debug/mnemos",
      backend_env = {
        "MNEMOS_DB_PATH=~/Work/mnemos/mnemos.db",
      },
      auto_refresh = true,
      keymaps = {
        prefix = "<leader>m",
      },
    },
    keys = {
      { "<leader>ma", "<cmd>MnemosAddNote<cr>", desc = "Add note" },
      { "<leader>ml", "<cmd>MnemosListNotes<cr>", desc = "List notes" },
      { "<leader>ms", "<cmd>MnemosSearch<cr>", desc = "Search" },
    },
  },
}
```

## Differences from Emacs UI

| Feature | Emacs | Neovim |
|---------|-------|--------|
| Note display | Overlays with before-string | Extmarks with virt_lines |
| Note input | Minibuffer with custom keymap | Floating window |
| Search UI | Custom buffer | Telescope picker |
| Linking trigger | `[[` post-self-insert hook | `[[` via abbreviation or keymap |
| Project root | project.el / override | Git root / configured |

## Implementation Order

1. **Week 1**: RPC client + basic note create/list/display
2. **Week 2**: Tree-sitter integration + note anchoring
3. **Week 3**: Commands, keybindings, notes list buffer
4. **Week 4**: Telescope integration, note linking
5. **Week 5**: Polish, testing, documentation

## Open Questions

1. **Linking trigger**: Should `[[` auto-trigger like Emacs, or use explicit `<leader>mk`?
   - Recommendation: Start with explicit keymap, add `[[` as optional iabbrev

2. **Note display style**: Full comment block vs. single-line indicator?
   - Recommendation: Match Emacs (full comment block) but add config option for minimal mode

3. **Floating window vs. split**: For note editing and viewing?
   - Recommendation: Floating by default, configurable
