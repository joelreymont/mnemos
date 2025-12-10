# AI Notification Timing Bug - Comprehensive Technical Brief

## Project Background

**Hemis** is a code annotation system that lets developers attach persistent notes to specific lines of code. Notes are anchored to AST nodes via tree-sitter, so they survive code refactoring. The system has:

- **Backend**: A Rust JSON-RPC server that manages notes in SQLite, handles tree-sitter parsing, and integrates with AI providers (Claude, etc.)
- **UI Clients**: Neovim, VSCode, Emacs plugins that display notes as virtual text/overlays
- **Event System**: A Unix socket that broadcasts real-time events (note-created, note-updated, etc.) to all connected clients

## Neovim Plugin Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Neovim Plugin                            │
├─────────────────────────────────────────────────────────────────┤
│  init.lua          - Plugin setup, event handler registration  │
│  commands.lua      - User commands (add_note, explain_region)  │
│  display.lua       - Renders notes as extmarks (virtual text)  │
│  notes.lua         - RPC wrappers for backend calls            │
│  rpc.lua           - JSON-RPC client over Unix socket          │
│  events.lua        - Event socket client for real-time updates │
└─────────────────────────────────────────────────────────────────┘
           │                              │
           │ JSON-RPC                     │ Events (JSON-lines)
           ▼                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Rust Backend                               │
├─────────────────────────────────────────────────────────────────┤
│  RPC Socket: ~/.hemis/rpc.sock                                  │
│  Event Socket: ~/.hemis/events.sock                             │
│  Database: ~/.hemis/hemis.db (SQLite)                           │
└─────────────────────────────────────────────────────────────────┘
```

### How Notes Are Displayed

Notes are rendered using Neovim's **extmarks** API. An extmark is metadata attached to a buffer position that can include virtual text (text that appears in the buffer but isn't part of the file content).

```lua
-- Simplified display.render_notes()
function display.render_notes(buf, notes)
  -- Clear old extmarks
  vim.api.nvim_buf_clear_namespace(buf, namespace_id, 0, -1)

  for _, note in ipairs(notes) do
    -- Create extmark with virtual lines (the note text)
    vim.api.nvim_buf_set_extmark(buf, namespace_id, note.line - 1, 0, {
      virt_lines = format_as_virtual_lines(note.formattedLines),
      virt_lines_above = true,
    })
  end
end
```

### The `formattedLines` Field

Each note has a `formattedLines` array computed by the backend. This contains the note text formatted as code comments with proper indentation:

```json
{
  "id": "abc123",
  "file": "/tmp/app.rs",
  "line": 16,
  "column": 4,
  "text": "This function initializes the server",
  "formattedLines": [
    "    // This function initializes the server"
  ]
}
```

The indentation (4 spaces) matches the code's column position so the note aligns with the code it annotates.

## The Feature: AI-Powered Code Explanation

The `explain_region` command:
1. User selects code in visual mode
2. User presses `<leader>hx`
3. Plugin shows "AI thinking..." status message
4. Backend sends selected code to AI provider (Claude)
5. AI returns explanation
6. Plugin creates a note with the explanation
7. Note appears as virtual text above the selected code
8. Status message is cleared

## The Bug

**The "AI thinking..." message disappears 1-8 seconds BEFORE the note appears.**

The delay correlates with note length:
- Short explanation (~50 chars): ~1 second gap
- Long explanation (~500 chars): ~7-8 second gap

This is jarring UX - the user sees the message disappear and thinks something failed, then the note suddenly appears seconds later.

## Current Implementation

### explain_region() - The Main Function

```lua
function M.explain_region()
  -- 1. Exit visual mode and get selection bounds
  local start_line, end_line = get_visual_selection()
  local anchor = get_cursor_position()
  local source_buf = vim.api.nvim_get_current_buf()
  local file = vim.fn.expand("%:p")

  -- 2. Show persistent status message
  vim.api.nvim_echo({ { "AI thinking...", "Comment" } }, false, {})
  vim.cmd("redraw")

  -- 3. Call backend for AI explanation (ASYNC)
  notes.explain_region(file, start_line, end_line, function(err, result)
    if err then
      vim.api.nvim_echo({ { "" } }, false, {})  -- Clear message
      vim.notify("Failed: " .. err.message, vim.log.levels.ERROR)
      return
    end

    local explanation_text = result.ai.statusDisplay .. " " .. result.explanation

    -- 4. Create note with the explanation (ASYNC)
    notes.create(explanation_text, {
      anchor = anchor,
      source_buf = source_buf,
    }, function(create_err, created_note)
      if create_err then
        vim.api.nvim_echo({ { "" } }, false, {})
        vim.notify("Failed to create note", vim.log.levels.ERROR)
        return
      end

      -- 5. Fetch all notes for buffer and render them
      M.refresh_sync(5000)

      -- 6. Yield to UI, then clear message
      vim.schedule(function()
        vim.cmd("redraw")
        vim.schedule(function()
          vim.api.nvim_echo({ { "" } }, false, {})  -- Clear message
        end)
      end)
    end)
  end)
end
```

### refresh_sync() - Synchronous Note Fetching

```lua
function M.refresh_sync(timeout_ms)
  timeout_ms = timeout_ms or 5000
  local done = false

  -- Make async RPC call
  notes.list_for_buffer(function(err, result)
    if not err then
      local notes_list = result.notes or result
      M.buffer_notes = notes_list
      display.render_notes(nil, M.buffer_notes)  -- Create extmarks
    end
    done = true
  end)

  -- Block until callback fires (with event loop processing)
  local uv = vim.uv or vim.loop
  local start = uv.now()
  while not done and (uv.now() - start) < timeout_ms do
    uv.run("nowait")  -- Process socket I/O
    vim.wait(50, function() return done end, 10)
  end
end
```

### Event System - Parallel Refresh Path

The plugin also listens for server-sent events. When a note is created, the server broadcasts a `note-created` event:

```lua
-- In init.lua
M.events.on("note-created", function(event)
  local bufnr = find_buffer_by_path(event.file)
  if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
    M.commands.refresh()  -- ASYNC refresh
  end
end)
```

This means there are **two refresh paths**:
1. `refresh_sync()` called explicitly in `explain_region` callback
2. `refresh()` triggered by `note-created` event

## What We've Tried (All Failed)

### Attempt 1: vim.notify() → nvim_echo()
**Theory**: `vim.notify()` auto-dismisses messages
**Change**: Used `nvim_echo()` which is more persistent
**Result**: No effect - message still disappears early

### Attempt 2: refresh_sync() with vim.wait()
**Theory**: Need to block until RPC completes
**Change**: Added synchronous wait loop with libuv event processing
**Result**: No effect - callback fires but note doesn't appear

### Attempt 3: Poll for Extmarks
**Theory**: Extmarks might not exist yet when we clear message
**Change**: Added polling loop to check `nvim_buf_get_extmarks()`:
```lua
function M.wait_for_extmarks(buf, timeout_ms)
  local start = uv.now()
  while (uv.now() - start) < timeout_ms do
    local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, {})
    if #marks > 0 then
      vim.cmd("redraw")
      return true
    end
    uv.run("nowait")
    vim.wait(50, function() ... end, 10)
  end
  return false
end
```
**Result**: No effect - extmarks exist but note not visible

### Attempt 4: Double vim.schedule()
**Theory** (from GPT-5.1 oracle): Extmarks exist in memory before UI renders them. Need to yield control so UI can flush.
**Change**:
```lua
M.refresh_sync(5000)
vim.schedule(function()
  vim.cmd("redraw")
  vim.schedule(function()
    vim.api.nvim_echo({ { "" } }, false, {})
  end)
end)
```
**Result**: No effect - delay still correlates with note length

## Key Observations

1. **Delay scales with content length** - This rules out constant-time UI issues
2. **Extmarks exist before note appears** - Polling confirms they're created
3. **Backend returns `formattedLines` synchronously** - Verified in Rust code:
   ```rust
   // In RPC handler for notes/create
   match notes::create(db, params) {
       Ok(mut note) => {
           events::emit(Event::NoteCreated { ... });
           display::ensure_formatted_lines(&mut note, None);  // Sync!
           Response::result_from(id, note)  // formattedLines included
       }
   }
   ```
4. **Two refresh paths exist** - RPC callback and event handler both trigger refresh

## Backend Event Emission

When a note is created, the backend:
1. Inserts note into SQLite
2. Emits `note-created` event (async, non-blocking)
3. Computes `formattedLines` (sync)
4. Returns RPC response with complete note

```rust
// Simplified backend flow
fn handle_notes_create(params: CreateParams) -> Response {
    let note = db.insert_note(params)?;

    // Fire-and-forget event broadcast
    events::emit(Event::NoteCreated {
        id: note.id.clone(),
        file: params.file.to_string(),
        line: note.line,
    });

    // Compute formatted lines (BLOCKING)
    display::ensure_formatted_lines(&mut note, None);

    // Return complete note
    Response::success(note)
}
```

## Hypotheses

### Hypothesis 1: Race Condition Between Refresh Paths
The event-triggered `refresh()` runs asynchronously and might:
- Overwrite the display after `refresh_sync()` completes
- Clear extmarks and re-render with stale data
- Cause the double-render to confuse the UI

### Hypothesis 2: Neovim Extmark Rendering Pipeline
Even though extmarks exist in memory:
- Neovim might batch extmark rendering
- Virtual lines might require multiple redraw cycles
- Longer content = more virtual lines = longer render time

### Hypothesis 3: Something Outside Our Code Clears the Message
- Another plugin or autocmd
- Neovim's cmdheight=0 behavior
- Status line refresh

### Hypothesis 4: The Callback Isn't Actually Firing When We Think
- RPC response parsing delay
- libuv socket buffering
- JSON parsing overhead scales with response size

## Questions for Investigation

1. **Is the RPC callback firing at the right time?**
   - Add logging with timestamps to verify callback timing
   - Compare callback timestamp to note-created event timestamp

2. **Is the event-triggered refresh interfering?**
   - Temporarily disable the event handler
   - Check if timing improves

3. **Are extmarks actually being rendered?**
   - Add delay after refresh_sync and before clearing message
   - Check if notes appear during that delay

4. **Is something else clearing the message?**
   - Search for other nvim_echo or message-clearing code
   - Check autocmds that fire on BufEnter, etc.

## Reproduction Steps

1. Open a Rust file in Neovim with Hemis plugin
2. Select 3-4 lines of code in visual mode
3. Press `<leader>hx` to trigger explain_region
4. Observe: "AI thinking..." appears
5. Wait for AI response (~3-5 seconds)
6. Observe: Message disappears
7. Wait 1-8 more seconds (depending on explanation length)
8. Observe: Note finally appears

## Environment

- Neovim 0.10+
- macOS (Darwin 25.1.0)
- Hemis backend (Rust, built with cargo)
- AI provider: Claude (via HEMIS_AI_PROVIDER=claude)
- Demo runner: hemis-demo (Swift)

## Files Involved

| File | Purpose |
|------|---------|
| `ui/neovim/lua/hemis/commands.lua` | explain_region(), refresh_sync(), wait_for_extmarks() |
| `ui/neovim/lua/hemis/display.lua` | render_notes(), extmark creation |
| `ui/neovim/lua/hemis/init.lua` | Event handler registration |
| `ui/neovim/lua/hemis/events.lua` | Unix socket event client |
| `ui/neovim/lua/hemis/notes.lua` | RPC wrappers |
| `ui/neovim/lua/hemis/rpc.lua` | JSON-RPC client |
| `backend/src/lib.rs` | RPC handlers |
| `backend/src/display.rs` | formattedLines computation |
| `backend/src/events.rs` | Event broadcaster |
