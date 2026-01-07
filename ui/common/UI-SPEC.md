# Mnemos UI Specification

Cross-editor abstraction layer for Mnemos UI implementations.

## Overview

This document defines the common patterns and contracts that all Mnemos UI implementations (Neovim, Emacs, VS Code, etc.) must follow. It serves as:
1. A guide for implementing new UIs
2. A consistency reference for existing UIs
3. Documentation of the RPC contract

## Core Abstractions

### Buffer Parameters

All file-scoped RPC calls require a standard set of buffer parameters.

```typescript
interface BufferParams {
  file: string;           // Absolute path to the file
  projectRoot: string;    // Git root or working directory
  commit?: string;        // HEAD commit SHA (for staleness)
  blob?: string;          // File blob SHA (for staleness)
  content?: string;       // Current buffer content (for position tracking)
  includeStale?: boolean; // Whether to include stale notes (default: true)
}
```

**Implementation Requirements:**
- `file`: Must be absolute path, not relative
- `projectRoot`: Prefer git root, fallback to cwd
- `commit`/`blob`: Optional but recommended for staleness detection
- `content`: Include when server needs to compute positions (list, create, reattach)

### Git Operations

UIs must extract git information for staleness tracking:

```typescript
interface GitInfo {
  commit: string | null;  // git rev-parse HEAD
  blob: string | null;    // git hash-object <file> or git ls-files -s
}
```

**Caching Strategy:**
- Cache per buffer/file
- Invalidate on file save
- Return null gracefully for non-git files

### Project Root Detection

```typescript
function getProjectRoot(): string {
  // 1. Try git rev-parse --show-toplevel
  // 2. Fallback to editor's project detection
  // 3. Fallback to current working directory
}
```

## RPC Methods

### Note Operations

| Method | Required Params | Optional Params | Returns |
|--------|-----------------|-----------------|---------|
| `notes/create` | file, projectRoot, line, column, text | content, tags, nodePath, nodeTextHash | Note |
| `notes/get` | id | - | Note |
| `notes/update` | id, text | tags | Note |
| `notes/delete` | id | - | {ok: true} |
| `notes/list-for-file` | file, projectRoot | content, includeStale | Note[] |
| `notes/list-project` | projectRoot | limit, offset | Note[] |
| `notes/search` | query, projectRoot | limit | Note[] |
| `notes/backlinks` | id | - | Note[] |
| `notes/reattach` | id, file, line, column | content | Note |
| `notes/buffer-update` | file, content | - | {positions: ...} |

### Project Operations

| Method | Required Params | Optional Params | Returns |
|--------|-----------------|-----------------|---------|
| `mnemos/index-project` | projectRoot | includeAI | {indexed, ai?} |
| `mnemos/search` | query, projectRoot | includeNotes, limit | SearchResult[] |
| `mnemos/status` | - | projectRoot | {ok, counts} |
| `mnemos/project-meta` | projectRoot | - | ProjectMeta |
| `mnemos/explain-region` | file, startLine, endLine, projectRoot | content, useAI, detailed | ExplainResult |

## Display Patterns

### Note Rendering

Notes should be displayed inline with the code, following these rules:

1. **Position**: Use `displayLine` from server if available, else stored `line`
2. **Grouping**: Multiple notes on same line should be grouped
3. **Staleness**: Stale notes should be visually distinguished
4. **Format**: Render as comments using buffer's comment syntax

```
// [note-id] Note text here
//           Wrapped continuation if needed
```

### Virtual Text / Overlays

| Editor | Mechanism | Positioning |
|--------|-----------|-------------|
| Neovim | `nvim_buf_set_extmark` with `virt_lines_above` | At line, column 0 |
| Emacs | Overlays with `before-string` | At line-beginning-position |
| VS Code | TextEditorDecorationType | Above line |

## Command Structure

All UI commands follow a consistent pattern:

```
Command Flow:
1. Capture context (cursor position, buffer info)
2. Gather user input if needed (note text, confirmation)
3. Build RPC params using buffer_params()
4. Call RPC method
5. Handle response (refresh display, show notification)
```

### Standard Commands

| Command | Action | User Input | RPC Method |
|---------|--------|------------|------------|
| add-note | Create note at cursor | Note text | notes/create |
| edit-note | Edit note at cursor | Modified text | notes/update |
| delete-note | Delete note at cursor | Confirmation | notes/delete |
| list-notes | Show notes for file | - | notes/list-for-file |
| refresh | Update display | - | notes/list-for-file |
| search | Search notes | Query | notes/search |
| backlinks | Show backlinks | - | notes/backlinks |
| reattach | Reattach stale note | Confirmation | notes/reattach |
| index-project | Index project files | - | mnemos/index-project |
| status | Show backend status | - | mnemos/status |

## Keybinding Conventions

Default prefix patterns (user-configurable):

| Editor | Prefix | Example |
|--------|--------|---------|
| Neovim | `<leader>h` | `<leader>ha` for add-note |
| Emacs | `C-c h` | `C-c h a` for add-note |
| VS Code | `Cmd+Shift+H` | `Cmd+Shift+H n` for add-note |

## Error Handling

UIs should handle these error conditions:

1. **Backend not running**: Start backend or show helpful message
2. **RPC timeout**: Retry once, then show error
3. **Invalid response**: Log error, don't crash UI
4. **Stale socket**: Clean up and reconnect

## Testing Patterns

Shared test fixtures for all UIs:

```json
{
  "create_note_success": {
    "request": {"method": "notes/create", "params": {...}},
    "response": {"id": "...", "text": "..."}
  },
  "create_note_error": {
    "request": {"method": "notes/create", "params": {...}},
    "error": {"code": -32000, "message": "..."}
  }
}
```

## Adding a New UI

1. Implement `buffer_params()` function following the spec
2. Implement git info extraction with caching
3. Implement RPC client (Unix socket, JSON-RPC 2.0)
4. Implement display layer using editor's decoration API
5. Wire up commands following the command structure
6. Add tests using shared fixtures
