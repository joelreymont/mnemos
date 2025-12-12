-- Commands and keybindings for Hemis
local notes = require("hemis.notes")
local display = require("hemis.display")
local rpc = require("hemis.rpc")
local config = require("hemis.config")
local events = require("hemis.events")

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
      if os.getenv("HEMIS_DEBUG") then
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

-- Delete note at cursor
function M.delete_note()
  display.get_note_at_cursor_with_picker(M.buffer_notes, nil, function(note)
    if not note then
      vim.notify("No note at cursor", vim.log.levels.WARN)
      return
    end

    vim.ui.select({ "Yes", "No" }, { prompt = "Delete note?" }, function(choice)
      if choice == "Yes" then
        notes.delete(note.id, function(err, _)
          if not err then
            M.refresh()
          end
        end)
      end
    end)
  end)
end

-- Edit note at cursor
function M.edit_note()
  -- Capture file path before picker (in case we open an edit buffer)
  local file = vim.fn.expand("%:p")

  display.get_note_at_cursor_with_picker(M.buffer_notes, nil, function(note)
    if not note then
      vim.notify("No note at cursor", vim.log.levels.WARN)
      return
    end

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
    end, { buffer = buf, desc = "Hemis: Insert link" })
  end)
end

-- Edit note in a full split buffer (for longer notes like AI explanations)
function M.edit_note_buffer()
  -- Capture file path before picker
  local file = vim.fn.expand("%:p")

  display.get_note_at_cursor_with_picker(M.buffer_notes, nil, function(note)
    if not note then
      vim.notify("No note at cursor", vim.log.levels.WARN)
      return
    end

    local note_id = note.id

    -- Create a new buffer in a horizontal split
    vim.cmd("new")
    local buf = vim.api.nvim_get_current_buf()
    local win = vim.api.nvim_get_current_win()

    -- Set buffer properties
    vim.bo[buf].buftype = "acwrite"
    vim.bo[buf].filetype = "markdown"
    vim.bo[buf].swapfile = false
    vim.api.nvim_buf_set_name(buf, "hemis://note/" .. note_id)

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
    end, { buffer = buf, desc = "Hemis: Insert link" })

    vim.notify("Editing note in buffer. :w to save, q to close.", vim.log.levels.INFO)
  end)
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
      signal_picker_ready("HemisListNotesPickerReady")
    end)
  end)
end

-- Search notes in current file
function M.search_file()
  vim.ui.input({ prompt = "Search notes in file: " }, function(query)
    if not query or query == "" then
      return
    end

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

    -- Schedule once, then verify picker readiness via polling
    vim.schedule(function()
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

      -- Verify picker is ready (insert mode + floating window) before signaling
      signal_picker_ready("HemisSearchFilePickerReady")
    end)
  end)
end

-- Search notes and files in project
-- IMPORTANT: Uses vim.fn.input() instead of vim.ui.input() to avoid textlock issues.
-- vim.ui.input's callback holds textlock, and any RPC call inside it causes
-- remote-send keystrokes to be dropped from the typeahead queue.
-- vim.fn.input() is blocking but releases textlock when it returns.
function M.search_project()
  -- Use blocking vim.fn.input() - textlock is released when it returns
  local ok, query = pcall(vim.fn.input, "Search project: ")
  if not ok or not query or query == "" then
    -- Clear command line
    vim.cmd("echo ''")
    return
  end
  -- Clear command line
  vim.cmd("echo ''")

  -- Now we're in a clean state with no textlock
  -- Use async RPC with vim.schedule for the picker
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
      -- Get relative filename for display
      local filename = hit.file or ""
      local basename = vim.fn.fnamemodify(filename, ":t")
      local line = hit.line or 1
      -- Use text field (actual line content) for display
      local text = hit.text or hit.display_label or ""
      -- Trim whitespace from text
      text = text:gsub("^%s+", ""):gsub("%s+$", "")
      table.insert(items, {
        label = string.format("%s:%d %s", basename, line, text),
        file = hit.file,
        line = line,
        col = hit.column or 0,
      })
    end

    -- vim.schedule to escape fast-event context from RPC callback
    vim.schedule(function()
      vim.ui.select(items, {
        prompt = "Search results for '" .. query .. "':",
        format_item = function(item) return item.label end,
      }, function(selected)
        if selected then
          vim.schedule(function()
            -- Open the file and go to line
            if selected.file then
              vim.cmd("edit " .. vim.fn.fnameescape(selected.file))
            end
            vim.api.nvim_win_set_cursor(0, { selected.line, selected.col })
          end)
        end
      end)

      -- Signal picker is ready for automation
      signal_picker_ready("HemisSearchProjectPickerReady")
    end)
  end)
end

-- Insert note link
-- opts.file can override the current file (useful from edit buffers with no file)
function M.insert_link(opts)
  opts = opts or {}
  vim.ui.input({ prompt = "Search notes: " }, function(query)
    if not query or query == "" then
      return
    end

    notes.search(query, { file = opts.file }, function(err, result)
      if err or not result or #result == 0 then
        vim.notify("No notes found", vim.log.levels.WARN)
        return
      end

      local items = {}
      for _, note in ipairs(result) do
        local desc = note.summary or (note.text or ""):sub(1, 40)
        local short_id = note.shortId or (note.id or ""):sub(1, 8)
        table.insert(items, string.format("%s (%s)", desc, short_id))
      end

      -- Use schedule_clean instead of double vim.schedule to ensure we're in a clean
      -- event context. The RPC callback arrives via libuv fast-event, and vim.ui.input
      -- holds textlock. schedule_clean uses timer + vim.schedule to escape both.
      schedule_clean(function()
        vim.ui.select(items, { prompt = "Select note:" }, function(choice, idx)
          if choice and idx then
            local note = result[idx]
            local desc = note.summary or (note.text or ""):sub(1, 40)
            local short_id = note.shortId or (note.id or ""):sub(1, 8)
            local link = string.format("[[%s][%s]]", desc, short_id)
            vim.api.nvim_put({ link }, "c", true, true)
          end
        end)

        -- Verify picker is ready (insert mode + floating window) before signaling
        signal_picker_ready("HemisInsertLinkPickerReady")
      end, 100)
    end)
  end)
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

-- Reattach stale note at cursor to current position
function M.reattach_note()
  -- Capture cursor position before picker
  local anchor = get_cursor_position()

  display.get_note_at_cursor_with_picker(M.buffer_notes, nil, function(note)
    if not note then
      vim.notify("No note at cursor", vim.log.levels.WARN)
      return
    end

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
  end)
end

-- Show backlinks for note at cursor
function M.show_backlinks()
  display.get_note_at_cursor_with_picker(M.buffer_notes, nil, function(note)
    if not note then
      vim.notify("No note at cursor", vim.log.levels.WARN)
      return
    end

    notes.backlinks(note.id, function(err, result)
    if err then
      vim.notify("Failed to fetch backlinks: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    if not result or #result == 0 then
      vim.notify("No backlinks for this note", vim.log.levels.INFO)
      return
    end

    -- Create backlinks buffer
    local buf = vim.api.nvim_create_buf(false, true)
    -- Backend guarantees display_marker is always present
    local marker = note.display_marker

    local lines = { string.format("Backlinks to note %s", marker), string.format("(%d notes link to this note)", #result), "" }

    for i, n in ipairs(result) do
      -- Backend guarantees display_marker and summary are always present
      local n_marker = n.display_marker
      table.insert(lines, string.format("  %d %s L%d,C%d", i - 1, n_marker, n.line or 0, n.column or 0))

      local text = n.summary
      for line in text:gmatch("[^\n]+") do
        table.insert(lines, "    " .. line)
      end
      table.insert(lines, "")
    end

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].modifiable = false
    vim.api.nvim_buf_set_name(buf, "Hemis Backlinks")

    -- Open in split
    vim.cmd("split")
    vim.api.nvim_win_set_buf(0, buf)

    -- Store notes for navigation
    vim.b[buf].hemis_notes = result

    -- Keymaps
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf })
    vim.keymap.set("n", "<CR>", function()
      M.visit_note_from_list(buf)
    end, { buffer = buf })
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
    { "R", "Reattach stale note" },
    { "d", "Delete note at cursor" },
    { "e", "Edit note at cursor" },
    { "E", "Edit note (buffer)" },
    { "f", "Search notes in file" },
    { "F", "Search project (notes/files)" },
    { "p", "Index project" },
    { "P", "Index file" },
    { "k", "Insert note link" },
    { "b", "Show backlinks" },
    { "s", "Select note" },
    { "t", "Status" },
    { "q", "Shutdown backend" },
    { "x", "Explain region (visual)" },
    { "X", "Explain region detailed (visual)" },
    { "?", "Show this help" },
  }

  -- Build lines for display
  local lines = { "Hemis Keybindings", "" }
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
  local ui = vim.api.nvim_list_uis()[1]
  local row = math.floor((ui.height - height) / 2)
  local col = math.floor((ui.width - width) / 2)

  -- Create floating window
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    row = row,
    col = col,
    style = "minimal",
    border = "rounded",
    title = " Hemis ",
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
  vim.notify("Hemis backend stopped", vim.log.levels.INFO)
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
function M.explain_region()
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

  -- Request AI explanation
  notes.explain_region(file, start_line, end_line, true, false, function(err, result)
    if err then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("Failed to explain region: " .. (err.message or vim.inspect(err)), vim.log.levels.ERROR)
      return
    end

    if not result or not result.explanation then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("No AI explanation available", vim.log.levels.WARN)
      return
    end

    -- Backend guarantees ai.statusDisplay when AI is used
    local text = string.format("%s %s", result.ai.statusDisplay, result.explanation)

    -- Timer reference already stored at start of function

    -- Create note with explanation
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

-- Explain region using AI with detailed/comprehensive explanation
function M.explain_region_full()
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
    vim.api.nvim_echo({ { string.format("AI thinking deeply... %ds", elapsed), "Comment" } }, false, {})
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

  -- Request detailed AI explanation
  notes.explain_region(file, start_line, end_line, true, true, function(err, result)
    if err then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("Failed to explain region: " .. (err.message or vim.inspect(err)), vim.log.levels.ERROR)
      return
    end

    if not result or not result.explanation then
      stop_and_clear()
      M.explain_region_in_progress = false
      vim.notify("No AI explanation available", vim.log.levels.WARN)
      return
    end

    -- Backend guarantees ai.statusDisplay when AI is used
    local text = string.format("%s %s", result.ai.statusDisplay, result.explanation)

    -- Timer reference already stored at start of function

    -- Create note with explanation
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
  vim.api.nvim_create_user_command("HemisAddNote", M.add_note, {})
  vim.api.nvim_create_user_command("HemisAddNoteMultiline", M.add_note_multiline, {})
  vim.api.nvim_create_user_command("HemisListNotes", M.list_notes, {})
  vim.api.nvim_create_user_command("HemisRefresh", M.refresh, {})
  vim.api.nvim_create_user_command("HemisDeleteNote", M.delete_note, {})
  vim.api.nvim_create_user_command("HemisEditNote", M.edit_note, {})
  vim.api.nvim_create_user_command("HemisEditNoteBuffer", M.edit_note_buffer, {})
  vim.api.nvim_create_user_command("HemisSearchFile", M.search_file, {})
  vim.api.nvim_create_user_command("HemisSearchProject", M.search_project, {})
  vim.api.nvim_create_user_command("HemisIndexFile", M.index_file, {})
  vim.api.nvim_create_user_command("HemisIndexProject", M.index_project, {})
  vim.api.nvim_create_user_command("HemisInsertLink", M.insert_link, {})
  vim.api.nvim_create_user_command("HemisBacklinks", M.show_backlinks, {})
  vim.api.nvim_create_user_command("HemisReattachNote", M.reattach_note, {})
  vim.api.nvim_create_user_command("HemisStatus", M.status, {})
  vim.api.nvim_create_user_command("HemisHelp", M.help, {})
  vim.api.nvim_create_user_command("HemisShutdown", M.shutdown, {})
  vim.api.nvim_create_user_command("HemisExplainRegion", M.explain_region, { range = true })
  vim.api.nvim_create_user_command("HemisExplainRegionFull", M.explain_region_full, { range = true })
  vim.api.nvim_create_user_command("HemisIndexProjectAI", M.index_project_ai, {})
  vim.api.nvim_create_user_command("HemisProjectMeta", M.project_meta, {})
  vim.api.nvim_create_user_command("HemisSelectNote", M.select_note, {})
  vim.api.nvim_create_user_command("HemisClearSelection", M.clear_selection, {})
end

-- Setup keymaps
function M.setup_keymaps()
  if not config.get("keymaps") then
    return
  end

  local prefix = config.get("keymap_prefix") or "<leader>h"

  local mappings = {
    { "a", M.add_note, "Add note" },
    { "A", M.add_note_multiline, "Add note (multiline)" },
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
    { "k", M.insert_link, "Insert link" },
    { "b", M.show_backlinks, "Show backlinks" },
    { "s", M.select_note, "Select note" },
    { "t", M.status, "Status" },
    { "q", M.shutdown, "Shutdown backend" },
    { "?", M.help, "Help" },
  }

  for _, m in ipairs(mappings) do
    vim.keymap.set("n", prefix .. m[1], m[2], { desc = "Hemis: " .. m[3] })
  end

  -- Visual mode mappings for explain region
  vim.keymap.set("v", prefix .. "x", M.explain_region, { desc = "Hemis: Explain region (AI)" })
  vim.keymap.set("v", prefix .. "X", M.explain_region_full, { desc = "Hemis: Explain region (AI detailed)" })

  -- Clear selection with prefix + Escape
  vim.keymap.set("n", prefix .. "<Esc>", M.clear_selection, { desc = "Hemis: Clear selection" })
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
  local group = vim.api.nvim_create_augroup("hemis", { clear = true })

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
