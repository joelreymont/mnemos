-- Commands and keybindings for Mnemos
local notes = require("mnemos.notes")
local display = require("mnemos.display")
local rpc = require("mnemos.rpc")
local config = require("mnemos.config")
local events = require("mnemos.events")

local M = {}

-- Cached notes for current buffer
M.buffer_notes = {}

-- Selected note state (global, persists across buffers)
M.selected_note = nil

-- Flag to prevent race between explain_region's refresh_sync and event-triggered refresh
M.explain_region_in_progress = false

-- Generation counter to invalidate queued timer callbacks
M._status_generation = 0

-- Signal picker ready only after verifying conditions are met.
-- This replaces blind "schedule N times" with actual verification.
-- For dressing.nvim/Telescope pickers: we need insert mode in a floating window.
local function signal_picker_ready(event_name, max_attempts)
  max_attempts = max_attempts or 20
  local attempts = 0

  local function check_and_signal()
    attempts = attempts + 1

    -- Must be scheduled to escape any textlock
    vim.schedule(function()
      local mode = vim.fn.mode()
      local win = vim.api.nvim_get_current_win()
      local win_config = vim.api.nvim_win_get_config(win)

      -- For dressing/Telescope: insert mode in a floating window
      local is_insert = mode:match("^[iI]") ~= nil
      local is_floating = win_config.relative ~= nil and win_config.relative ~= ""

      -- Debug: log state on each attempt
      if os.getenv("MNEMOS_DEBUG") then
        vim.notify(string.format("[picker-ready] attempt=%d mode=%s floating=%s win=%d",
          attempts, mode, tostring(is_floating), win), vim.log.levels.DEBUG)
      end

      if is_insert and is_floating then
        -- Ready! Signal automation
        vim.cmd("redraw")
        vim.api.nvim_exec_autocmds("User", { pattern = event_name })
      elseif attempts < max_attempts then
        -- Not ready yet, try again next tick
        vim.schedule(check_and_signal)
      else
        -- Give up after max attempts, fire anyway (fallback)
        vim.cmd("redraw")
        vim.api.nvim_exec_autocmds("User", { pattern = event_name })
      end
    end)
  end

  check_and_signal()
end

-- Refresh notes display (async)
function M.refresh()
  notes.list_for_buffer(function(err, result)
    if err then
      vim.notify("Failed to fetch notes: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    -- Backend returns {notes: [...]} wrapper
    local notes_list = result
    if result and result.notes then
      notes_list = result.notes
    end

    M.buffer_notes = notes_list or {}
    local selected_id = M.selected_note and M.selected_note.id or nil
    display.render_notes(nil, M.buffer_notes, selected_id)
    display.cache_notes(nil, M.buffer_notes)
    -- Force redraw to ensure extmarks appear immediately
    vim.cmd("redraw")
  end)
end

-- Synchronous refresh - waits for notes to be fetched and rendered
-- Use after operations that need immediate display (e.g., explain_region)
function M.refresh_sync(timeout_ms, debug_t0)
  timeout_ms = timeout_ms or 5000
  local done = false
  local refresh_err = nil

  notes.list_for_buffer(function(err, result)
    if debug_t0 then
      vim.notify(string.format("[DEBUG %.3fs] refresh_sync callback fired (notes fetched)", os.clock() - debug_t0), vim.log.levels.INFO)
    end

    if err then
      refresh_err = err
    else
      -- Backend returns {notes: [...]} wrapper
      local notes_list = result
      if result and result.notes then
        notes_list = result.notes
      end
      M.buffer_notes = notes_list or {}

      if debug_t0 then
        vim.notify(string.format("[DEBUG %.3fs] Calling display.render_notes (in refresh_sync callback)", os.clock() - debug_t0), vim.log.levels.INFO)
      end

      local selected_id = M.selected_note and M.selected_note.id or nil
      display.render_notes(nil, M.buffer_notes, selected_id)
      display.cache_notes(nil, M.buffer_notes)

      if debug_t0 then
        vim.notify(string.format("[DEBUG %.3fs] display.render_notes completed", os.clock() - debug_t0), vim.log.levels.INFO)
      end
    end
    done = true
  end)

  -- Wait with libuv event processing to ensure socket callbacks fire
  local uv = vim.uv or vim.loop
  local start = uv.now()
  while not done and (uv.now() - start) < timeout_ms do
    -- Process libuv events (socket reads) then vim scheduled callbacks
    uv.run("nowait")
    vim.wait(50, function() return done end, 10)
  end

  if refresh_err then
    vim.notify("Failed to fetch notes: " .. (refresh_err.message or "unknown"), vim.log.levels.ERROR)
  end
end

-- Wait for extmarks to actually exist in buffer (for sync with visual display)
-- Returns true if extmarks found, false on timeout
function M.wait_for_extmarks(buf, timeout_ms)
  buf = buf or vim.api.nvim_get_current_buf()
  timeout_ms = timeout_ms or 3000
  local ns = display.ns_id
  local uv = vim.uv or vim.loop
  local start = uv.now()

  while (uv.now() - start) < timeout_ms do
    local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, {})
    if #marks > 0 then
      -- Force redraw to ensure extmarks are visually displayed
      vim.cmd("redraw")
      return true
    end
    -- Process events and wait a bit
    uv.run("nowait")
    vim.wait(50, function()
      local m = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, {})
      return #m > 0
    end, 10)
  end
  return false
end

-- Get raw cursor position (server handles anchor adjustment)
local function get_cursor_position()
  local cursor = vim.api.nvim_win_get_cursor(0)
  return { line = cursor[1], column = cursor[2] }
end

-- Add note at cursor
function M.add_note()
  -- Capture position BEFORE opening input UI (which changes buffer context)
  -- Server computes anchor, nodePath, nodeTextHash from content
  local anchor = get_cursor_position()
  local source_buf = vim.api.nvim_get_current_buf()

  vim.ui.input({ prompt = "Note: " }, function(text)
    if not text or text == "" then
      return
    end

    -- Backend trims, but skip if only whitespace
    if not text:match("%S") then
      return
    end

    -- Pass captured position to create (server handles tree-sitter)
    notes.create(text, {
      anchor = anchor,
      source_buf = source_buf,
    }, function(err, _)
      if not err then
        M.refresh()
      end
    end)
  end)
end

-- Add multi-line note
function M.add_note_multiline()
  -- Capture position BEFORE opening input UI (which changes buffer context)
  -- Server computes anchor, nodePath, nodeTextHash from content
  local anchor = get_cursor_position()
  local source_buf = vim.api.nvim_get_current_buf()

  -- Create a scratch buffer for note input
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * 0.6)
  local height = 10

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = " New Note (<CR> to save, q or <Esc> to cancel) ",
    title_pos = "center",
  })

  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "markdown"

  -- Save callback
  local function save_note()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(lines, "\n")

    vim.api.nvim_win_close(win, true)

    -- Backend trims, but skip if only whitespace
    if text:match("%S") then
      -- Pass captured position to create (server handles tree-sitter)
      notes.create(text, {
        anchor = anchor,
        source_buf = source_buf,
      }, function(err, _)
        if not err then
          M.refresh()
        end
      end)
    end
  end

  -- Cancel callback
  local function cancel()
    vim.api.nvim_win_close(win, true)
    vim.notify("Note cancelled", vim.log.levels.INFO)
  end

  -- Keymaps for the note buffer
  vim.keymap.set("n", "q", cancel, { buffer = buf })
  vim.keymap.set("n", "<Esc>", cancel, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-c><C-c>", save_note, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-c><C-k>", cancel, { buffer = buf })
  vim.keymap.set("n", "<CR>", save_note, { buffer = buf })

  vim.cmd("startinsert")
end

-- Delete selected note
function M.delete_note()
  if not M.selected_note then
    vim.notify("No note selected", vim.log.levels.WARN)
    return
  end

  local note = M.selected_note
  vim.ui.select({ "Yes", "No" }, { prompt = "Delete note?" }, function(choice)
    if choice == "Yes" then
      notes.delete(note.id, function(err, _)
        if not err then
          M.selected_note = nil
          M.refresh()
        end
      end)
    end
  end)
end

-- Edit note at cursor (convenience - edit what you're looking at)
-- Uses cursor position to find note, not selection
function M.edit_note()
  local cursor_note = display.get_note_at_cursor(M.buffer_notes)
  if not cursor_note then
    vim.notify("No note at cursor", vim.log.levels.WARN)
    return
  end

  local note = cursor_note
  -- Capture file path in case we need it for insert-link
  local file = vim.fn.expand("%:p")

  -- Create scratch buffer with existing text
  local buf = vim.api.nvim_create_buf(false, true)
  local width = math.floor(vim.o.columns * 0.6)
  local height = 10

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = " Edit Note (<CR> to save, q or <Esc> to cancel) ",
    title_pos = "center",
  })

  local lines = vim.split(note.text or "", "\n")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "markdown"
  -- Store source file for insert_link to use
  vim.b[buf].mnemos_source_file = file

  local function save()
    local new_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(new_lines, "\n")

    vim.api.nvim_win_close(win, true)

    -- Backend trims, but skip if only whitespace
    if text:match("%S") then
      notes.update(note.id, text, {}, function(err, _)
        if not err then
          M.refresh()
        end
      end)
    end
  end

  local function cancel()
    vim.api.nvim_win_close(win, true)
  end

  vim.keymap.set("n", "q", cancel, { buffer = buf })
  vim.keymap.set("n", "<Esc>", cancel, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-c><C-c>", save, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-c><C-k>", cancel, { buffer = buf })
  vim.keymap.set("n", "<CR>", save, { buffer = buf })

  -- Add insert-link keymap for the edit buffer (uses same prefix as main keymaps)
  -- Pass captured file since edit buffer has no associated file
  local prefix = config.get("keymap_prefix") or "<leader>h"
  vim.keymap.set({ "n", "i" }, prefix .. "k", function()
    M.insert_link({ file = file })
  end, { buffer = buf, desc = "Mnemos: Insert link" })
end

-- Edit note at cursor in a full split buffer (for longer notes like AI explanations)
function M.edit_note_buffer()
  local cursor_note = display.get_note_at_cursor(M.buffer_notes)
  if not cursor_note then
    vim.notify("No note at cursor", vim.log.levels.WARN)
    return
  end

  local note = cursor_note
  local note_id = note.id
  -- Capture file path in case we need it for insert-link
  local file = vim.fn.expand("%:p")

  -- Create a new buffer in a horizontal split
  vim.cmd("new")
  local buf = vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()

  -- Set buffer properties
  vim.bo[buf].buftype = "acwrite"
  vim.bo[buf].filetype = "markdown"
  vim.bo[buf].swapfile = false
  vim.api.nvim_buf_set_name(buf, "mnemos://note/" .. note_id)

  -- Insert note content
  local lines = vim.split(note.text or "", "\n")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modified = false

  -- Set up autocmd to save on :w
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    buffer = buf,
    callback = function()
      local new_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
      local text = table.concat(new_lines, "\n")

      -- Backend trims, but skip if only whitespace
      if text:match("%S") then
        -- Mark as saved immediately so 'q' keymap works right after :w
        -- (optimistic update - if save fails, user sees error)
        vim.bo[buf].modified = false
        notes.update(note_id, text, {}, function(err, _)
          if err then
            vim.notify("Failed to save note: " .. (err.message or "unknown"), vim.log.levels.ERROR)
            -- Re-mark as modified since save failed
            vim.bo[buf].modified = true
          else
            vim.notify("Note saved", vim.log.levels.INFO)
            -- Refresh notes display in source buffer
            M.refresh()
          end
        end)
      end
    end,
  })

  -- Add keymaps
  vim.keymap.set("n", "q", function()
    if vim.bo[buf].modified then
      vim.ui.select({ "Save and close", "Discard changes", "Cancel" }, {
        prompt = "Buffer has unsaved changes:",
      }, function(choice)
        if choice == "Save and close" then
          vim.cmd("write")
          vim.api.nvim_win_close(win, true)
        elseif choice == "Discard changes" then
          vim.bo[buf].modified = false
          vim.api.nvim_win_close(win, true)
        end
      end)
    else
      vim.api.nvim_win_close(win, true)
    end
  end, { buffer = buf, desc = "Close buffer" })

  -- Add insert-link keymap
  local prefix = config.get("keymap_prefix") or "<leader>h"
  vim.keymap.set({ "n", "i" }, prefix .. "k", function()
    M.insert_link({ file = file })
  end, { buffer = buf, desc = "Mnemos: Insert link" })

  vim.notify("Editing note in buffer. :w to save, q to close.", vim.log.levels.INFO)
end

-- List notes in a buffer using vim.ui.select (floating picker via dressing.nvim)
function M.list_notes()
  notes.list_for_buffer(function(err, result)
    if err then
      vim.notify("Failed to fetch notes", vim.log.levels.ERROR)
      return
    end

    -- Backend returns {notes: [...], contentHash: ...} wrapper when content is sent
    local notes_list = result
    if result and result.notes then
      notes_list = result.notes
    end

    if not notes_list or #notes_list == 0 then
      vim.notify("No notes for this file", vim.log.levels.INFO)
      return
    end

    -- Build picker items
    local items = {}
    for _, note in ipairs(notes_list) do
      local short_id = note.shortId or (note.id or ""):sub(1, 8)
      local summary = note.summary or (note.text or ""):sub(1, 50)
      local stale = note.stale and " [STALE]" or ""
      table.insert(items, {
        label = string.format("[%s] %s%s", short_id, summary, stale),
        note = note,
      })
    end

    -- Format function for vim.ui.select
    local function format_item(item)
      return item.label
    end

    -- Schedule picker creation so we're back in the main loop (outside RPC callback)
    vim.schedule(function()
      vim.ui.select(items, {
        prompt = "Notes in " .. vim.fn.expand("%:t") .. ":",
        format_item = format_item,
      }, function(selected)
        if selected then
          -- Jump to note location
          local note = selected.note
          if note and note.line then
            vim.schedule(function()
              vim.api.nvim_win_set_cursor(0, { note.line, (note.column or 1) - 1 })
              -- Select this note
              M.set_selected_note(note)
            end)
          else
            vim.notify("Selected note has no line info", vim.log.levels.WARN)
          end
        end
      end)

      -- Verify picker is ready (insert mode + floating window) before signaling
      signal_picker_ready("MnemosListNotesPickerReady")
    end)
  end)
end

-- Search notes in current file
-- Uses vim.fn.input() for reliable remote-send automation
function M.search_file()
  local ok, query = pcall(vim.fn.input, "Search notes in file: ")
  if not ok or not query or query == "" then
    vim.cmd("echo ''")
    return
  end
  vim.cmd("echo ''")

  -- Filter buffer_notes by query
  local matches = {}
  for _, note in ipairs(M.buffer_notes or {}) do
    local text = (note.text or "") .. " " .. (note.summary or "")
    if text:lower():find(query:lower(), 1, true) then
      table.insert(matches, note)
    end
  end

  if #matches == 0 then
    vim.notify("No notes match: " .. query, vim.log.levels.INFO)
    return
  end

  -- Show picker
  local items = {}
  for _, note in ipairs(matches) do
    local short_id = note.shortId or (note.id or ""):sub(1, 8)
    local summary = note.summary or (note.text or ""):sub(1, 50)
    table.insert(items, {
      label = string.format("[%s] %s", short_id, summary),
      note = note,
    })
  end

  -- Use vim.defer_fn to ensure clean context for picker
  vim.defer_fn(function()
    vim.ui.select(items, {
      prompt = "Notes matching '" .. query .. "':",
      format_item = function(item) return item.label end,
    }, function(selected)
      if selected then
        vim.schedule(function()
          vim.api.nvim_win_set_cursor(0, { selected.note.line, (selected.note.column or 1) - 1 })
          M.set_selected_note(selected.note)
        end)
      end
    end)

    signal_picker_ready("MnemosSearchFilePickerReady")
  end, 50)
end

-- Search notes and files in project
-- Uses vim.fn.input() for reliable remote-send automation
function M.search_project()
  local ok, query = pcall(vim.fn.input, "Search project: ")
  if not ok or not query or query == "" then
    vim.cmd("echo ''")
    return
  end
  vim.cmd("echo ''")

  notes.search_project(query, { include_notes = true }, function(err, result)
    if err then
      vim.notify("Search failed", vim.log.levels.ERROR)
      return
    end

    if not result or #result == 0 then
      vim.notify("No results", vim.log.levels.INFO)
      return
    end

    -- Show results in floating picker
    local items = {}
    for _, hit in ipairs(result) do
      local filename = hit.file or ""
      local basename = vim.fn.fnamemodify(filename, ":t")
      local line = hit.line or 1
      local text = hit.text or hit.display_label or ""
      text = text:gsub("^%s+", ""):gsub("%s+$", "")
      table.insert(items, {
        label = string.format("%s:%d %s", basename, line, text),
        file = hit.file,
        line = line,
        col = hit.column or 0,
      })
    end

    -- Use vim.defer_fn to ensure clean context for picker
    vim.defer_fn(function()
      vim.ui.select(items, {
        prompt = "Search results for '" .. query .. "':",
        format_item = function(item) return item.label end,
      }, function(selected)
        if selected then
          vim.schedule(function()
            if selected.file then
              vim.cmd("edit " .. vim.fn.fnameescape(selected.file))
            end
            vim.api.nvim_win_set_cursor(0, { selected.line, selected.col })
          end)
        end
      end)

      signal_picker_ready("MnemosSearchProjectPickerReady")
    end, 50)
  end)
end

-- Insert note link
-- Prompts for search query, shows picker with matching notes, inserts link to selected note
function M.insert_link()
  -- Use vim.fn.input for search query (works with remote-send, no textlock issues)
  local query = vim.fn.input("Link search query: ")
  if query == "" then
    return
  end

  -- Get file for projectRoot - use source file if in edit buffer
  local file = vim.b.mnemos_source_file or vim.fn.expand("%:p")

  -- Search notes in project (pass file explicitly)
  notes.search_project(query, { include_notes = true, file = file }, function(err, result)
    if err then
      vim.notify("Search failed: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    -- Filter to only note results
    local note_hits = {}
    for _, hit in ipairs(result or {}) do
      if hit.kind == "note" and hit.noteId then
        table.insert(note_hits, hit)
      end
    end

    if #note_hits == 0 then
      vim.notify("No notes found for: " .. query, vim.log.levels.INFO)
      return
    end

    -- Build picker items
    local items = {}
    for _, hit in ipairs(note_hits) do
      local file = hit.file and vim.fn.fnamemodify(hit.file, ":t") or "unknown"
      local summary = hit.noteSummary or hit.text or ""
      table.insert(items, {
        label = string.format("%s:%d - %s", file, hit.line or 0, summary:sub(1, 50)),
        hit = hit,
      })
    end

    local function format_item(item)
      return item.label
    end

    -- Show picker
    vim.schedule(function()
      vim.ui.select(items, {
        prompt = string.format("Insert link to (%d notes):", #items),
        format_item = format_item,
      }, function(selected)
        if selected then
          local hit = selected.hit
          local desc = hit.noteSummary or hit.text or "note"
          desc = desc:sub(1, 40)
          -- Use full noteId (36-char UUID) for backlinks to work
          local id = hit.noteId or ""
          local link = string.format("[[%s][%s]]", desc, id)
          vim.schedule(function()
            vim.api.nvim_put({ link }, "c", true, true)
          end)
        end
      end)

      signal_picker_ready("MnemosInsertLinkPickerReady")
    end)
  end)
end

-- Follow link under cursor - navigate to linked note
function M.follow_link()
  -- Get current line and cursor position
  local line = vim.api.nvim_get_current_line()
  local col = vim.api.nvim_win_get_cursor(0)[2] + 1 -- 1-indexed

  -- Find all links in line: [[desc][uuid]]
  -- Pattern matches: [[anything][36-char-uuid]]
  local link_pattern = "%[%[.-%]%[([%x%-]+)%]%]"

  -- Find which link the cursor is on
  local link_start = 1
  local found_id = nil
  while true do
    local s, e, id = line:find(link_pattern, link_start)
    if not s then break end
    -- Check if cursor is within this link
    if col >= s and col <= e then
      found_id = id
      break
    end
    link_start = e + 1
  end

  if not found_id then
    vim.notify("No link under cursor", vim.log.levels.INFO)
    return
  end

  -- Get the note by ID
  notes.get(found_id, function(err, note)
    if err then
      vim.notify("Failed to get note: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end
    if not note then
      vim.notify("Note not found: " .. found_id, vim.log.levels.WARN)
      return
    end
    if not note.file then
      vim.notify("Note has no file location", vim.log.levels.WARN)
      return
    end

    vim.schedule(function()
      -- Push current position to jumplist before navigating
      vim.cmd("normal! m'")

      -- Open file and jump to line
      vim.cmd("edit " .. vim.fn.fnameescape(note.file))
      if note.line then
        vim.api.nvim_win_set_cursor(0, { note.line, (note.column or 1) - 1 })
      end
      vim.notify("Followed link to: " .. (note.summary or note.id), vim.log.levels.INFO)
    end)
  end)
end

-- Navigate back after following link (uses Vim jumplist)
function M.navigate_back()
  vim.cmd("normal! \15") -- Ctrl-O
end

-- Index current file only
function M.index_file()
  notes.index_file()
end

-- Index project
function M.index_project()
  notes.index_project(false)
end

-- Index project with AI analysis
function M.index_project_ai()
  notes.index_project(true)
end

-- Show status
function M.status()
  notes.status(function(err, result)
    if err then
      vim.notify("Backend not running", vim.log.levels.WARN)
      return
    end

    -- Backend guarantees statusDisplay
    vim.notify(result.statusDisplay, vim.log.levels.INFO)
  end)
end

-- Reattach stale selected note to current cursor position
function M.reattach_note()
  if not M.selected_note then
    vim.notify("No note selected", vim.log.levels.WARN)
    return
  end

  local note = M.selected_note
  local anchor = get_cursor_position()

  -- Server computes staleness when content is provided
  if not note.stale then
    vim.notify("Note is not stale", vim.log.levels.INFO)
    return
  end

  vim.ui.select({ "Yes", "No" }, { prompt = "Reattach note to current position?" }, function(choice)
    if choice == "Yes" then
      notes.reattach(note.id, {
        anchor = anchor,
      }, function(err, _)
        if not err then
          M.refresh()
        end
      end)
    end
  end)
end

-- Show backlinks for selected note (uses picker like list_notes)
function M.show_backlinks()
  if not M.selected_note then
    vim.notify("No note selected", vim.log.levels.WARN)
    return
  end

  local note = M.selected_note

  notes.backlinks(note.id, function(err, result)
    if err then
      vim.notify("Failed to fetch backlinks: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    if not result or #result == 0 then
      vim.notify("No backlinks for this note", vim.log.levels.INFO)
      return
    end

    -- Build picker items
    local items = {}
    for _, n in ipairs(result) do
      local short_id = n.shortId or (n.id or ""):sub(1, 8)
      local summary = n.summary or (n.text or ""):sub(1, 50)
      local file = n.file and vim.fn.fnamemodify(n.file, ":t") or "unknown"
      table.insert(items, {
        label = string.format("[%s] %s:%d - %s", short_id, file, n.line or 0, summary),
        note = n,
      })
    end

    local function format_item(item)
      return item.label
    end

    -- Schedule picker creation so we're back in the main loop
    vim.schedule(function()
      local marker = note.display_marker or note.shortId or (note.id or ""):sub(1, 8)
      vim.ui.select(items, {
        prompt = string.format("Backlinks to %s (%d):", marker, #items),
        format_item = format_item,
      }, function(selected)
        if selected then
          local n = selected.note
          if n and n.file then
            vim.schedule(function()
              -- Open file if different
              local current_file = vim.fn.expand("%:p")
              if n.file ~= current_file then
                vim.cmd("edit " .. vim.fn.fnameescape(n.file))
              end
              -- Jump to note location
              if n.line then
                vim.api.nvim_win_set_cursor(0, { n.line, (n.column or 1) - 1 })
              end
              -- Select this note
              M.set_selected_note(n)
            end)
          end
        end
      end)

      signal_picker_ready("MnemosBacklinksPickerReady")
    end)
  end)
end

-- Resolve <leader> in prefix to human-readable key name
local function resolve_prefix(prefix)
  if not prefix:find("<leader>") then
    return prefix
  end
  local leader = vim.g.mapleader or "\\"
  local leader_name
  if leader == " " then
    leader_name = "SPC "
  elseif leader == "\\" then
    leader_name = "\\"
  else
    leader_name = leader
  end
  return prefix:gsub("<leader>", leader_name)
end

-- Show help in styled floating window (like which-key)
function M.help()
  local prefix = resolve_prefix(config.get("keymap_prefix") or "<leader>h")

  -- Define keybindings: { key, description }
  local bindings = {
    { "a", "Add note at cursor" },
    { "A", "Add multi-line note" },
    { "l", "List notes for file" },
    { "r", "Refresh notes display" },
    { "R", "Reattach selected note" },
    { "d", "Delete selected note" },
    { "e", "Edit note at cursor" },
    { "E", "Edit note at cursor (buffer)" },
    { "f", "Search notes in file" },
    { "F", "Search project (notes/files)" },
    { "p", "Index project" },
    { "P", "Index file" },
    { "k", "Insert link to selected note" },
    { "b", "Show backlinks for selected note" },
    { "s", "Select note" },
    { "t", "Status" },
    { "q", "Shutdown backend" },
    { "x", "Explain region (visual)" },
    { "X", "Explain region detailed (visual)" },
    { "?", "Show this help" },
  }

  -- Build lines for display
  local lines = { "Mnemos Keybindings", "" }
  for _, b in ipairs(bindings) do
    table.insert(lines, string.format("  %s%s  %s", prefix, b[1], b[2]))
  end

  -- Calculate window size
  local max_width = 0
  for _, line in ipairs(lines) do
    max_width = math.max(max_width, #line)
  end
  local width = math.min(max_width + 4, 60)
  local height = #lines

  -- Create buffer
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_set_option_value("modifiable", false, { buf = buf })
  vim.api.nvim_set_option_value("bufhidden", "wipe", { buf = buf })

  -- Calculate position (centered)
  local uis = vim.api.nvim_list_uis()
  local ui_height = uis[1] and uis[1].height or 24
  local ui_width = uis[1] and uis[1].width or 80
  local row = math.floor((ui_height - height) / 2)
  local col = math.floor((ui_width - width) / 2)

  -- Create floating window
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
    title = " Mnemos ",
    title_pos = "center",
  })

  -- Apply highlights
  vim.api.nvim_set_option_value("winhl", "Normal:NormalFloat,FloatBorder:FloatBorder", { win = win })

  -- Highlight title
  vim.api.nvim_buf_add_highlight(buf, -1, "Title", 0, 0, -1)

  -- Highlight keys (the prefix + key part)
  local prefix_len = #prefix
  for i = 3, #lines do  -- Skip title and blank line
    -- Highlight the key portion
    vim.api.nvim_buf_add_highlight(buf, -1, "Special", i - 1, 2, 2 + prefix_len + 1)
  end

  -- Close on any key
  vim.keymap.set("n", "<Esc>", function()
    vim.api.nvim_win_close(win, true)
  end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function()
    vim.api.nvim_win_close(win, true)
  end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "<CR>", function()
    vim.api.nvim_win_close(win, true)
  end, { buffer = buf, nowait = true })
end

-- Shutdown backend
function M.shutdown()
  rpc.stop()
  vim.notify("Mnemos backend stopped", vim.log.levels.INFO)
end

-- Get selected note
function M.get_selected_note()
  return M.selected_note
end

-- Set selected note and update display
function M.set_selected_note(note)
  M.selected_note = note
  -- Trigger status line refresh
  vim.cmd("redrawstatus")
  -- Re-render notes to update highlight
  local selected_id = note and note.id or nil
  display.render_notes(vim.api.nvim_get_current_buf(), M.buffer_notes, selected_id)
end

-- Select note at cursor or from picker
function M.select_note()
  -- First try notes at cursor (handles multi-note disambiguation)
  local notes_at_cursor = display.get_notes_at_cursor(M.buffer_notes)

  if #notes_at_cursor == 1 then
    -- Single note at cursor, select it directly
    local note = notes_at_cursor[1]
    M.set_selected_note(note)
    local short_id = note.shortId or (note.id or ""):sub(1, 8)
    vim.notify("Selected note: " .. short_id .. " - " .. (note.summary or ""):sub(1, 40), vim.log.levels.INFO)
    return
  end

  if #notes_at_cursor > 1 then
    -- Multiple notes at cursor, show picker for just those
    local items = {}
    for _, n in ipairs(notes_at_cursor) do
      local short_id = n.shortId or (n.id or ""):sub(1, 8)
      local summary = n.summary or (n.text or ""):sub(1, 40)
      local stale = n.stale and " [STALE]" or ""
      table.insert(items, string.format("[%s] %s%s", short_id, summary, stale))
    end

    vim.ui.select(items, { prompt = "Multiple notes on this line:" }, function(_, idx)
      if idx then
        local selected = notes_at_cursor[idx]
        M.set_selected_note(selected)
        local short_id = selected.shortId or (selected.id or ""):sub(1, 8)
        vim.notify("Selected note: " .. short_id, vim.log.levels.INFO)
      end
    end)
    return
  end

  -- No note at cursor, show picker for all notes in file
  if not M.buffer_notes or #M.buffer_notes == 0 then
    vim.notify("No notes in this file", vim.log.levels.INFO)
    return
  end

  local items = {}
  for _, n in ipairs(M.buffer_notes) do
    local short_id = n.shortId or (n.id or ""):sub(1, 8)
    local summary = n.summary or (n.text or ""):sub(1, 40)
    table.insert(items, string.format("[%s] %s", short_id, summary))
  end

  vim.ui.select(items, { prompt = "Select note:" }, function(_, idx)
    if idx then
      local selected = M.buffer_notes[idx]
      M.set_selected_note(selected)
      local short_id = selected.shortId or (selected.id or ""):sub(1, 8)
      vim.notify("Selected note: " .. short_id, vim.log.levels.INFO)
    end
  end)
end

-- Clear note selection
function M.clear_selection()
  if M.selected_note then
    vim.notify("Note selection cleared", vim.log.levels.INFO)
  end
  M.set_selected_note(nil)
end

-- Explain region using AI and create a note
-- Ask AI about selected region with custom prompt
function M.ask_ai()
  -- Exit visual mode first to set '< and '> marks
  local mode = vim.fn.mode()
  if mode:match("[vV]") then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  end

  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")
  local start_line = start_pos[2]
  local end_line = end_pos[2]

  if start_line == 0 or end_line == 0 then
    vim.notify("Select a region first", vim.log.levels.WARN)
    return
  end

  -- Prompt for AI instruction with default
  local ok, prompt = pcall(vim.fn.input, {
    prompt = "Ask AI: ",
    default = "explain this code",
  })
  if not ok or not prompt or prompt == "" then
    vim.cmd("echo ''")
    return
  end
  vim.cmd("echo ''")

  -- Set flag to prevent race with event-triggered refresh
  M.explain_region_in_progress = true

  -- Capture context before async operations
  vim.api.nvim_win_set_cursor(0, { start_line, 0 })
  local anchor = get_cursor_position()
  local source_buf = vim.api.nvim_get_current_buf()
  local file = vim.fn.expand("%:p")

  -- Track timing for display
  local start_time = vim.uv.now()

  -- Increment generation to invalidate any queued timer callbacks from previous runs
  M._status_generation = M._status_generation + 1
  local current_generation = M._status_generation

  -- Show persistent status in echo area with timer
  local status_timer = vim.uv.new_timer()
  local function update_status()
    -- Check both timer reference AND generation (generation invalidates queued callbacks)
    if M._pending_status_timer ~= status_timer then return end
    if M._status_generation ~= current_generation then return end
    local elapsed = math.floor((vim.uv.now() - start_time) / 1000)
    vim.api.nvim_echo({ { string.format("AI thinking... %ds", elapsed), "Comment" } }, false, {})
  end

  -- Helper to stop the status timer and clear message
  local function stop_and_clear()
    -- Increment generation FIRST to invalidate any queued timer callbacks
    M._status_generation = M._status_generation + 1
    M._pending_status_timer = nil
    if status_timer then
      status_timer:stop()
      status_timer:close()
      status_timer = nil
    end
    vim.cmd("redraw!")
    vim.api.nvim_echo({ { "" } }, false, {})
  end

  -- Store timer reference so update_status can check if it's still active
  M._pending_status_timer = status_timer

  update_status()
  vim.cmd("redraw")

  -- Start timer to update elapsed time every second
  status_timer:start(1000, 1000, vim.schedule_wrap(update_status))

  -- Request AI response with custom prompt
  notes.explain_region(file, start_line, end_line, prompt, function(err, result)
    if err then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("AI request failed: " .. (err.message or vim.inspect(err)), vim.log.levels.ERROR)
      return
    end

    if not result or not result.explanation then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("No AI response", vim.log.levels.WARN)
      return
    end

    -- Backend guarantees ai.statusDisplay when AI is used
    local text = string.format("%s %s", result.ai.statusDisplay, result.explanation)

    -- Create note with AI response
    notes.create(text, {
      anchor = anchor,
      source_buf = source_buf,
    }, function(create_err, _)
      if create_err then
        M._pending_status_timer = nil
        stop_and_clear()
        M.explain_region_in_progress = false
        vim.notify("Failed to create note: " .. (create_err.message or "unknown"), vim.log.levels.ERROR)
        return
      end

      -- Event handler will handle refresh and message clearing
      -- Add fallback timeout in case event doesn't arrive
      vim.defer_fn(function()
        if M._pending_status_timer == status_timer then
          -- Event handler didn't clean up, do it ourselves
          M._pending_status_timer = nil
          M.explain_region_in_progress = false
          M.refresh_sync(5000)
          stop_and_clear()
        end
      end, 5000)
    end)
  end)
end

-- Show project metadata
function M.project_meta()
  notes.project_meta(function(err, result)
    if err then
      vim.notify("Failed to get project meta: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    local buf = vim.api.nvim_create_buf(false, true)
    local lines = {}
    table.insert(lines, string.format("Project: %s", result.projectRoot or "unknown"))
    table.insert(lines, "")
    table.insert(lines, string.format("Indexed: %s", result.indexed and "Yes" or "No"))
    if result.formattedIndexedAt then
      table.insert(lines, string.format("  Last indexed: %s", result.formattedIndexedAt))
    elseif result.indexedAt then
      table.insert(lines, string.format("  Last indexed: %s", os.date("%Y-%m-%d %H:%M:%S", result.indexedAt)))
    end
    table.insert(lines, "")
    table.insert(lines, string.format("AI Analysis: %s", result.analysisStatusDisplay))
    if result.analysisProvider then
      table.insert(lines, string.format("  Provider: %s", result.analysisProvider))
    end
    table.insert(lines, "")
    table.insert(lines, string.format("AI Available: %s", result.aiAvailable and "Yes" or "No"))

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.api.nvim_buf_set_option(buf, "modifiable", false)
    vim.cmd("split")
    vim.api.nvim_win_set_buf(0, buf)
  end)
end

-- Setup user commands
function M.setup_commands()
  vim.api.nvim_create_user_command("MnemosAddNote", M.add_note, {})
  vim.api.nvim_create_user_command("MnemosAddNoteMultiline", M.add_note_multiline, {})
  vim.api.nvim_create_user_command("MnemosListNotes", M.list_notes, {})
  vim.api.nvim_create_user_command("MnemosRefresh", M.refresh, {})
  vim.api.nvim_create_user_command("MnemosDeleteNote", M.delete_note, {})
  vim.api.nvim_create_user_command("MnemosEditNote", M.edit_note, {})
  vim.api.nvim_create_user_command("MnemosEditNoteBuffer", M.edit_note_buffer, {})
  vim.api.nvim_create_user_command("MnemosSearchFile", M.search_file, {})
  vim.api.nvim_create_user_command("MnemosSearchProject", M.search_project, {})
  vim.api.nvim_create_user_command("MnemosIndexFile", M.index_file, {})
  vim.api.nvim_create_user_command("MnemosIndexProject", M.index_project, {})
  vim.api.nvim_create_user_command("MnemosInsertLink", M.insert_link, {})
  vim.api.nvim_create_user_command("MnemosFollowLink", M.follow_link, {})
  vim.api.nvim_create_user_command("MnemosNavigateBack", M.navigate_back, {})
  vim.api.nvim_create_user_command("MnemosBacklinks", M.show_backlinks, {})
  vim.api.nvim_create_user_command("MnemosReattachNote", M.reattach_note, {})
  vim.api.nvim_create_user_command("MnemosStatus", M.status, {})
  vim.api.nvim_create_user_command("MnemosHelp", M.help, {})
  vim.api.nvim_create_user_command("MnemosShutdown", M.shutdown, {})
  vim.api.nvim_create_user_command("MnemosAskAI", M.ask_ai, { range = true })
  vim.api.nvim_create_user_command("MnemosIndexProjectAI", M.index_project_ai, {})
  vim.api.nvim_create_user_command("MnemosProjectMeta", M.project_meta, {})
  vim.api.nvim_create_user_command("MnemosSelectNote", M.select_note, {})
  vim.api.nvim_create_user_command("MnemosClearSelection", M.clear_selection, {})
end

-- Setup keymaps
function M.setup_keymaps()
  if not config.get("keymaps") then
    return
  end

  local prefix = config.get("keymap_prefix") or "<leader>h"

  local mappings = {
    { "n", M.add_note, "Add note" },
    { "N", M.add_note_multiline, "Add note (multiline)" },
    { "l", M.list_notes, "List notes" },
    { "r", M.refresh, "Refresh notes" },
    { "R", M.reattach_note, "Reattach note" },
    { "d", M.delete_note, "Delete note" },
    { "e", M.edit_note, "Edit note" },
    { "E", M.edit_note_buffer, "Edit note (buffer)" },
    { "f", M.search_file, "Search notes in file" },
    { "F", M.search_project, "Search project" },
    { "p", M.index_project, "Index project" },
    { "P", M.index_file, "Index file" },
    { "i", M.insert_link, "Insert link" },
    { "]", M.follow_link, "Follow link" },
    { "[", M.navigate_back, "Navigate back" },
    { "b", M.show_backlinks, "Show backlinks" },
    { "s", M.select_note, "Select note" },
    { "t", M.status, "Status" },
    { "q", M.shutdown, "Shutdown backend" },
    { "?", M.help, "Help" },
  }

  for _, m in ipairs(mappings) do
    vim.keymap.set("n", prefix .. m[1], m[2], { desc = "Mnemos: " .. m[3] })
  end

  -- Visual mode mapping for Ask AI
  vim.keymap.set("v", prefix .. "a", M.ask_ai, { desc = "Mnemos: Ask AI" })

  -- Clear selection with prefix + Escape
  vim.keymap.set("n", prefix .. "<Esc>", M.clear_selection, { desc = "Mnemos: Clear selection" })
end

-- Debounce timer for buffer updates
local buffer_update_timer = nil
local BUFFER_UPDATE_DEBOUNCE_MS = 200

-- Send buffer update to server for real-time position tracking
local function send_buffer_update()
  if buffer_update_timer then
    buffer_update_timer:stop()
    buffer_update_timer = nil
  end

  -- Only update if we have notes cached for this buffer
  if not M.buffer_notes or #M.buffer_notes == 0 then
    return
  end

  notes.buffer_update(function(err, result)
    if err then
      -- Silently ignore errors (don't spam user during typing)
      return
    end

    if result and #result > 0 then
      -- Update cached notes with new positions
      M.buffer_notes = result
      -- Re-render with server-computed positions
      display.render_notes(nil, M.buffer_notes)
      display.cache_notes(nil, M.buffer_notes)
    end
  end)
end

-- Schedule debounced buffer update
local function schedule_buffer_update()
  if buffer_update_timer then
    buffer_update_timer:stop()
  end

  buffer_update_timer = vim.defer_fn(send_buffer_update, BUFFER_UPDATE_DEBOUNCE_MS)
end

-- Setup autocommands
function M.setup_autocommands()
  local group = vim.api.nvim_create_augroup("mnemos", { clear = true })

  if config.get("auto_refresh") then
    vim.api.nvim_create_autocmd("BufEnter", {
      group = group,
      pattern = "*",
      callback = function()
        if vim.bo.buftype == "" and vim.fn.expand("%:p") ~= "" then
          -- Delay to let buffer load
          vim.defer_fn(M.refresh, 100)
        end
      end,
    })
  end

  -- Real-time position tracking: update note positions as user types
  if config.get("realtime_tracking") ~= false then
    vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
      group = group,
      pattern = "*",
      callback = function()
        if vim.bo.buftype == "" and vim.fn.expand("%:p") ~= "" then
          schedule_buffer_update()
        end
      end,
    })
  end

  -- Cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
      rpc.stop()
    end,
  })
end

return M
