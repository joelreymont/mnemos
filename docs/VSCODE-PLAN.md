# VS Code Extension Implementation Plan

## Overview

Create a VS Code extension for Hemis that communicates with the same Rust backend, providing feature parity with Emacs and Neovim UIs.

## Directory Structure

```
ui/vscode/
  src/
    extension.ts        # Extension entry point, activation
    rpc.ts              # JSON-RPC client (stdio, Content-Length framing)
    notes.ts            # Note operations
    decorations.ts      # Text decorations for note display
    treesitter.ts       # Tree-sitter integration via wasm
    commands.ts         # Command handlers
    providers/
      notesView.ts      # TreeView for notes list
      hoverProvider.ts  # Hover to show note details
      linkProvider.ts   # Document links for [[id]] references
    config.ts           # Configuration
  test/
    suite/
      rpc.test.ts       # RPC client tests
      notes.test.ts     # Note operations tests
      integration.test.ts  # Full flow tests
    runTest.ts          # Test runner
  package.json          # Extension manifest
  tsconfig.json
  README.md
  CHANGELOG.md
```

## Implementation Phases

### Phase 1: Extension Scaffold and RPC Client

**Files**: `src/extension.ts`, `src/rpc.ts`, `src/config.ts`

**Features**:
- Extension activation on supported file types
- Backend process spawn with stdio
- Content-Length framing (send/receive)
- Request/response with Promises
- Auto-restart on crash
- Graceful shutdown on deactivate

**Configuration** (package.json contributes.configuration):
```json
{
  "hemis.backend": {
    "type": "string",
    "description": "Path to hemis backend binary"
  },
  "hemis.databasePath": {
    "type": "string",
    "description": "Path to SQLite database (defaults to ~/.hemis/hemis.db)"
  },
  "hemis.autoRefresh": {
    "type": "boolean",
    "default": true
  },
  "hemis.displayStyle": {
    "type": "string",
    "enum": ["full", "minimal"],
    "default": "full"
  }
}
```

**Note**: The database defaults to `~/.hemis/hemis.db`, enabling sharing notes with Emacs and Neovim.

### Phase 2: Basic Notes CRUD

**Files**: `src/notes.ts`, `src/commands.ts`

**Commands** (package.json contributes.commands):
- `hemis.addNote` - Add note at cursor
- `hemis.deleteNote` - Delete note at cursor
- `hemis.editNote` - Edit note at cursor
- `hemis.refreshNotes` - Refresh display
- `hemis.listNotes` - Show notes panel

**Features**:
- Create note at cursor position
- Delete note by ID
- Update note text
- List notes for current file

### Phase 3: Note Display with Decorations

**Files**: `src/decorations.ts`

**Approach**:
- Use `vscode.TextEditorDecorationType` for note markers
- Show notes as "before" decorations (text above line)
- Different decoration for stale notes
- CodeLens for note indicators (alternative approach)

**Decoration types**:
```typescript
const noteDecoration = vscode.window.createTextEditorDecorationType({
  before: {
    contentText: '// note text here',
    color: '#4682B4',
    fontStyle: 'italic',
  },
  isWholeLine: true,
});
```

### Phase 4: Tree-sitter Integration

**Files**: `src/treesitter.ts`

**Approach**:
- Use `web-tree-sitter` (wasm-based Tree-sitter)
- Load language grammars dynamically
- Get node at cursor position
- Build node path
- Cache parsed trees

**Node path format**: Same as Emacs/Neovim - `["function_declaration", "parameters"]`

### Phase 5: Notes Panel (TreeView)

**Files**: `src/providers/notesView.ts`

**Features**:
- TreeView showing all notes for current file
- Group by file in project view
- Click to navigate to note location
- Context menu: Edit, Delete
- Stale note indicator

**TreeView structure**:
```
HEMIS NOTES
  ├── main.rs
  │   ├── [a1b2] L10 - Note about function
  │   └── [c3d4] L25 - Another note
  └── lib.rs
      └── [e5f6] L5 - Module description
```

### Phase 6: Hover and Link Providers

**Files**: `src/providers/hoverProvider.ts`, `src/providers/linkProvider.ts`

**Hover**:
- Show full note text on hover over marked line
- Markdown rendering for note content

**Links**:
- Make `[[desc][id]]` clickable
- Navigate to linked note on click
- Show link preview on hover

### Phase 7: Search and Indexing

**Commands**:
- `hemis.indexFile` - Index current file
- `hemis.indexProject` - Index workspace
- `hemis.search` - Search notes and files

**Search UI**:
- Quick pick for search results
- Show score, file, line
- Preview on hover

### Phase 8: Note Linking

**Commands**:
- `hemis.insertLink` - Insert link to another note

**Features**:
- Quick pick to search notes
- Insert `[[description][id]]` format
- Auto-complete for `[[` (CompletionProvider)

## Testing Strategy

**Unit tests** (Mocha + VS Code test API):
- RPC framing encode/decode
- Note operations (mocked RPC)
- Decoration calculation

**Integration tests**:
- Full RPC flow against real backend
- Note CRUD operations
- Search functionality

**Test file structure**:
```
test/
  suite/
    rpc.test.ts
    notes.test.ts
    decorations.test.ts
    integration.test.ts
  runTest.ts
  index.ts
```

## Dependencies

**Runtime**:
- `web-tree-sitter` - Tree-sitter for wasm
- `tree-sitter-{lang}` - Language grammars

**Development**:
- `@types/vscode`
- `typescript`
- `@vscode/test-electron`
- `mocha`
- `glob`

## Package.json Key Sections

```json
{
  "name": "hemis",
  "displayName": "Hemis",
  "description": "A second brain for your code",
  "version": "0.1.0",
  "engines": { "vscode": "^1.80.0" },
  "categories": ["Other"],
  "activationEvents": [
    "onLanguage:rust",
    "onLanguage:typescript",
    "onLanguage:javascript",
    "onLanguage:python",
    "onCommand:hemis.addNote"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "commands": [...],
    "configuration": {...},
    "views": {
      "explorer": [{
        "id": "hemisNotes",
        "name": "Hemis Notes"
      }]
    },
    "keybindings": [
      { "command": "hemis.addNote", "key": "ctrl+shift+h a" },
      { "command": "hemis.listNotes", "key": "ctrl+shift+h l" }
    ]
  }
}
```

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `Ctrl+Shift+H A` | Add note | Add note at cursor |
| `Ctrl+Shift+H L` | List notes | Open notes panel |
| `Ctrl+Shift+H R` | Refresh | Refresh note display |
| `Ctrl+Shift+H D` | Delete | Delete note at cursor |
| `Ctrl+Shift+H E` | Edit | Edit note at cursor |
| `Ctrl+Shift+H S` | Search | Search notes/files |
| `Ctrl+Shift+H I` | Index | Index current file |
| `Ctrl+Shift+H P` | Index project | Index workspace |
| `Ctrl+Shift+H K` | Insert link | Insert note link |

## Implementation TODO

### Phase 1: Scaffold (Week 1)
- [ ] Initialize extension with `yo code`
- [ ] Set up TypeScript configuration
- [ ] Implement `src/config.ts` with settings
- [ ] Implement `src/rpc.ts` with:
  - [ ] Process spawn with stdio
  - [ ] Content-Length framing
  - [ ] Request queue with Promises
  - [ ] Auto-reconnect logic
- [ ] Add basic activation in `extension.ts`
- [ ] Write RPC unit tests

### Phase 2: Basic Notes (Week 2)
- [ ] Implement `src/notes.ts` with CRUD operations
- [ ] Implement `src/commands.ts` with:
  - [ ] addNote (with input box)
  - [ ] deleteNote
  - [ ] editNote (with editor)
  - [ ] refreshNotes
- [ ] Register commands in package.json
- [ ] Write notes unit tests

### Phase 3: Display (Week 2-3)
- [ ] Implement `src/decorations.ts`
- [ ] Create decoration types for notes
- [ ] Handle multi-line notes
- [ ] Add stale note styling
- [ ] Refresh on document change
- [ ] Write decoration tests

### Phase 4: Tree-sitter (Week 3)
- [ ] Set up web-tree-sitter
- [ ] Load Rust grammar
- [ ] Implement node path extraction
- [ ] Cache parsed trees
- [ ] Write Tree-sitter tests

### Phase 5: Notes Panel (Week 4)
- [ ] Implement TreeDataProvider
- [ ] Create TreeView in explorer
- [ ] Add click-to-navigate
- [ ] Add context menu
- [ ] Write TreeView tests

### Phase 6: Hover/Links (Week 4)
- [ ] Implement HoverProvider
- [ ] Implement DocumentLinkProvider
- [ ] Handle [[id]] link clicks
- [ ] Write provider tests

### Phase 7: Search (Week 5)
- [ ] Implement search QuickPick
- [ ] Add index commands
- [ ] Show search results with preview
- [ ] Write search tests

### Phase 8: Polish (Week 5)
- [ ] Add status bar item
- [ ] Add welcome walkthrough
- [ ] Write README.md
- [ ] Create CHANGELOG.md
- [ ] Package for marketplace

## Differences from Emacs/Neovim

| Feature | Emacs | Neovim | VS Code |
|---------|-------|--------|---------|
| Note display | Overlays | Extmarks | Decorations |
| Note input | Minibuffer | Floating window | Input box / Editor |
| Notes list | Custom buffer | Split buffer | TreeView panel |
| Search | Custom buffer | QuickFix / Telescope | QuickPick |
| Linking | `[[` trigger | Keymap | CompletionProvider |

## Open Questions

1. **Tree-sitter loading**: Bundle grammars or download on demand?
   - Recommendation: Bundle common grammars, download others

2. **Note display**: Decorations vs CodeLens?
   - Recommendation: Decorations for consistency with Emacs/Neovim

3. **WebView for notes**: Use WebView for rich note editing?
   - Recommendation: Start simple with input box, add WebView later

4. **Multi-root workspaces**: How to handle project root?
   - Recommendation: Use workspace folder of current file
