# Mnemos VS Code Extension

A second brain for your code - attach persistent notes to AST nodes.

## Features

- Notes anchored to code locations
- Git-aware staleness detection
- Full-text and semantic search
- Note linking with `[[description][id]]` syntax
- Notes panel in Explorer sidebar

## Requirements

- VS Code 1.80+
- Mnemos backend binary

## Installation

1. Build the backend:
   ```bash
   cd /path/to/mnemos
   cargo build --release
   ```

2. Configure the extension in VS Code settings:
   ```json
   {
     "mnemos.backend": "/path/to/mnemos/target/release/mnemos"
   }
   ```

   The database defaults to `~/.mnemos/mnemos.db`. Override with:
   ```json
   {
     "mnemos.databasePath": "/path/to/custom.db"
   }
   ```

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `Ctrl+Shift+H A` | Add note | Add note at cursor |
| `Ctrl+Shift+H L` | List notes | Show notes in Quick Pick |
| `Ctrl+Shift+H R` | Refresh | Refresh note display |
| `Ctrl+Shift+H D` | Delete | Delete note at cursor |
| `Ctrl+Shift+H E` | Edit | Edit note at cursor |
| `Ctrl+Shift+H S` | Search | Search notes and files |
| `Ctrl+Shift+H I` | Index | Index current file |
| `Ctrl+Shift+H P` | Index project | Index workspace |
| `Ctrl+Shift+H K` | Insert link | Insert note link |

## Commands

- `Mnemos: Add Note` - Add a note at the cursor position
- `Mnemos: Delete Note` - Delete the note at cursor
- `Mnemos: Edit Note` - Edit the note at cursor
- `Mnemos: Refresh Notes` - Refresh note decorations
- `Mnemos: List Notes` - Show notes for current file
- `Mnemos: Search` - Search notes and indexed files
- `Mnemos: Index Current File` - Index current file for search
- `Mnemos: Index Project` - Index all project files
- `Mnemos: Insert Note Link` - Insert a link to another note
- `Mnemos: Show Status` - Display backend status

## Development

```bash
# Install dependencies
npm install

# Compile
npm run compile

# Watch for changes
npm run watch

# Run tests
npm test
```

## Multi-Editor Support

This extension shares its database with the Emacs and Neovim Mnemos clients. Notes created in one editor appear in the others after refresh.
