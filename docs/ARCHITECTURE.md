# Hemis Architecture

## Overview

```
┌─────────────┐                        ┌─────────────────────────────┐
│   Emacs     │     Unix Socket        │                             │
│   hemis.el  │◄──────────────────────►│                             │
└─────────────┘                        │                             │
               ~/.hemis/hemis.sock     │   Backend Server            │
┌─────────────┐     Unix Socket        │   (Single Process)          │
│   Neovim    │◄──────────────────────►│                             │
│   hemis.lua │                        │   - Reference counting      │
└─────────────┘                        │   - Grace period shutdown   │
                                       │   - Version checking        │
┌─────────────┐     Unix Socket        │                             │
│   VS Code   │◄──────────────────────►│                             │
│   extension │                        └──────────────┬──────────────┘
└─────────────┘                                       │
                                                      ▼
                                               ┌─────────────┐
                                               │   SQLite    │
                                               │  ~/.hemis/  │
                                               │  hemis.db   │
                                               └─────────────┘

Files:
  ~/.hemis/
    hemis.db        # Database
    hemis.sock      # Unix domain socket
    hemis.lock      # PID file for startup coordination
    hemis.log       # Server logs (optional)
    events.sock     # Event notifications socket
```

## Key Concepts

- **Single Backend Process**: All editors connect to one backend via Unix socket
- **Reference Counting**: Backend tracks connected clients, shuts down after 30s idle
- **Selected Note Model**: Operations (edit, delete, backlinks) work on explicitly selected note
- **Position Tracking**: Notes track positions in real-time as buffer changes
- **Staleness Detection**: Notes become stale when anchor code changes (via nodeTextHash)

## Workspace Layout

```
backend/
  src/lib.rs          # Main RPC dispatcher
  src/main.rs         # Server entry point
  src/ai_cli.rs       # AI provider integration
  crates/
    git/              # Git operations (libgit2 with CLI fallback)
    index/            # FTS5 text indexing and search
    notes/            # Note model, SQLite CRUD, position tracking
    rpc/              # JSON-RPC framing and parsing
    storage/          # SQLite connection, migrations, schema
    treesitter/       # Tree-sitter parsing for anchor detection
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

- `hemis/rpc.lua`: Socket connection, request/response handling
- `hemis/notes.lua`: Note operations (create, list, search, etc.)
- `hemis/commands.lua`: User commands, keybindings, display
- `hemis/display.lua`: Virtual text rendering for notes

Selected note stored in `M.selected_note`, operations use this state.

### Emacs (elisp)

- `hemis.el`: All-in-one package
- Uses overlays for note display
- `hemis-notes-mode` auto-enables in `prog-mode`
- Keymap under `C-c h ...`

### VS Code (TypeScript)

- Extension in `ui/vscode/`
- Uses decorations for note display

## AI Integration

Optional AI features via `HEMIS_AI_PROVIDER` environment variable:

- `claude`: Anthropic Claude API
- `codex`: OpenAI Codex via CLI
- `none`: Disabled (default)

AI features:
- `hemis/explain-region`: Generate explanation for code region
- `notes/explain-and-create`: AI-generated note from code

## Event System

Backend emits events via `~/.hemis/events.sock`:

- `note-created`: New note created
- `note-updated`: Note text/tags changed
- `note-deleted`: Note removed
- `index-complete`: Project indexing finished

Used by demo automation to wait for async operations.

## Testing

```bash
# Rust unit and integration tests
cargo test

# Neovim tests (plenary.nvim)
cd ui/neovim && nvim --headless -u tests/minimal_init.lua \
  -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"

# Emacs tests (ERT)
emacs -Q --batch -L ui/emacs -l hemis.el \
  -L ui/emacs/tests -l hemis-test.el -f ert-run-tests-batch-and-exit

# VSCode tests
cd ui/vscode && npm test
```
