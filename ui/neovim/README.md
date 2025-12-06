# Hemis Neovim UI

Neovim plugin for Hemis - a second brain for your code.

## Features

- Notes anchored to Tree-sitter nodes
- Notes displayed as virtual text (comment blocks)
- Note linking with `[[desc][id]]` syntax
- Project-wide search
- Stale note detection

## Requirements

- Neovim 0.9+
- hemis backend binary

## Quick Start

```lua
-- In lazy.nvim
{
  dir = "/path/to/hemis/ui/neovim",
  opts = {
    backend = "/path/to/hemis/target/release/hemis",
  },
}
```

See [docs/NEOVIM.md](../../docs/NEOVIM.md) for full setup instructions.

## Development

```bash
# Build backend
cargo build

# Run tests
nvim --headless -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"
```

## Structure

```
lua/hemis/
  init.lua        # Main module, setup()
  config.lua      # Configuration
  rpc.lua         # JSON-RPC client
  notes.lua       # Note operations (server handles anchor/nodePath computation)
  display.lua     # Virtual text rendering
  commands.lua    # Commands and keymaps
plugin/
  hemis.lua       # Entry point
tests/
  hemis_spec.lua  # Integration tests
```
