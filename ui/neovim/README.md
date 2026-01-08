# Mnemos Neovim UI

Neovim plugin for Mnemos - a second brain for your code.

## Features

- Notes anchored to Tree-sitter nodes
- Notes displayed as virtual text (comment blocks)
- Note linking with `[[desc][id]]` syntax
- Project-wide search via ripgrep
- Stale note detection

## Requirements

- Neovim 0.9+
- mnemos backend binary

## Quick Start

```lua
-- In lazy.nvim
{
  dir = "/path/to/mnemos/ui/neovim",
  opts = {
    backend = "/path/to/mnemos/zig-out/bin/mnemos",
  },
}
```

See [docs/NEOVIM.md](../../docs/NEOVIM.md) for full setup instructions.

## Development

```bash
# Build backend
zig build

# Run tests
nvim --headless -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"
```

## Structure

```
lua/mnemos/
  init.lua        # Main module, setup()
  config.lua      # Configuration
  rpc.lua         # JSON-RPC client
  notes.lua       # Note operations (server handles anchor/nodePath computation)
  display.lua     # Virtual text rendering
  commands.lua    # Commands and keymaps
plugin/
  mnemos.lua       # Entry point
tests/
  mnemos_spec.lua  # Integration tests
```
