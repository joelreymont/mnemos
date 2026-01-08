# Mnemos Architecture

## Overview

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

Files:
  ~/.mnemos/
    mnemos.sock     # Unix domain socket
    mnemos.lock     # PID file for startup coordination
    mnemos.log      # Server logs (optional)
    events.sock     # Event notifications socket
  <project>/.mnemos/
    notes/          # Markdown note files
```

## Key Concepts

- **Single Backend Process**: All editors connect to one backend via Unix socket
- **Reference Counting**: Backend tracks connected clients, shuts down after 30s idle
- **Selected Note Model**: Operations (edit, delete, backlinks) work on explicitly selected note
- **Position Tracking**: Notes track positions in real-time as buffer changes
- **Staleness Detection**: Notes become stale when anchor code changes (via nodeTextHash)

## Workspace Layout

```
src/
  main.zig        # Entry point, CLI, socket setup
  server.zig      # Unix socket server and event loop
  rpc.zig         # JSON-RPC dispatch
  storage.zig     # Markdown note storage + indexing
  ai.zig          # AI provider integration
  treesitter.zig  # Tree-sitter integration
  git.zig         # libgit2 bindings
ui/
  neovim/         # Lua plugin
  emacs/          # Elisp package
  vscode/         # TypeScript extension
```

## Note Anchoring

Notes attach to Tree-sitter AST nodes:

1. **Creation**: When creating a note, the backend finds the smallest AST node containing the cursor
2. **nodeTextHash**: SHA256 hash of the node's source text, used for staleness detection
3. **nodePath**: Path through AST (e.g., `function_definition.parameters`)
4. **Position Tracking**: When `content` is provided in requests, backend recomputes line numbers

## Staleness and Reattachment

```
Note created at fn foo() → nodeTextHash = sha256("fn foo() { ... }")
                         ↓
Code edited: fn foo() → fn bar()
                         ↓
nodeTextHash mismatch → stale: true
                         ↓
User reattaches → new nodeTextHash computed
```

## UI Frontends

### Neovim (lua)

- `mnemos/rpc.lua`: Socket connection, request/response handling
- `mnemos/notes.lua`: Note operations (create, list, search, etc.)
- `mnemos/commands.lua`: User commands, keybindings, display
- `mnemos/display.lua`: Virtual text rendering for notes

Selected note stored in `M.selected_note`, operations use this state.

### Emacs (elisp)

- `mnemos.el`: All-in-one package
- Uses overlays for note display
- `mnemos-notes-mode` auto-enables in `prog-mode`
- Keymap under `C-c m ...`

### VS Code (TypeScript)

- Extension in `ui/vscode/`
- Uses decorations for note display

## AI Integration

Optional AI features via `MNEMOS_AI_PROVIDER` environment variable:

- `claude`: Anthropic Claude API
- `codex`: OpenAI Codex via CLI
- `none`: Disabled (default)

AI features:
- `mnemos/explain-region`: Generate explanation for code region
- `notes/explain-and-create`: AI-generated note from code

## Event System

Backend emits events via `~/.mnemos/events.sock`:

- `note-created`: New note created
- `note-updated`: Note text/tags changed
- `note-deleted`: Note removed
- `index-complete`: Project indexing finished (legacy)

Used by demo automation to wait for async operations.

## Testing

```bash
# Backend tests
zig build test

# Neovim tests (plenary.nvim)
cd ui/neovim && nvim --headless -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"

# Emacs tests (ERT)
emacs -Q --batch -L ui/emacs -l mnemos.el \
  -L ui/emacs/tests -l mnemos-test.el -f ert-run-tests-batch-and-exit

# VSCode tests
cd ui/vscode && npm test
```
