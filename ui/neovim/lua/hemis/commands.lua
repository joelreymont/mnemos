-- Commands and keybindings for Hemis
local notes = require("hemis.notes")
local display = require("hemis.display")
local rpc = require("hemis.rpc")
local config = require("hemis.config")

local M = {}

-- Cached notes for current buffer
M.buffer_notes = {}

-- Refresh notes display
function M.refresh()
  notes.list_for_buffer(function(err, result)
    if err then
      vim.notify("Failed to fetch notes: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    M.buffer_notes = result or {}
    display.render_notes(nil, M.buffer_notes)
  end)
end

-- Add note at cursor
function M.add_note()
  -- Capture position BEFORE opening input UI (which changes buffer context)
  local ts = require("hemis.treesitter")
  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local node_text_hash = ts.get_node_text_hash()
  local source_buf = vim.api.nvim_get_current_buf()

  vim.ui.input({ prompt = "Note: " }, function(text)
    if not text or text == "" then
      return
    end

    -- Trim whitespace from input
    text = text:gsub("^%s+", ""):gsub("%s+$", "")
    if text == "" then
      return
    end

    -- Pass captured position to create
    notes.create(text, {
      anchor = anchor,
      node_path = node_path,
      node_text_hash = node_text_hash,
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
  local ts = require("hemis.treesitter")
  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local node_text_hash = ts.get_node_text_hash()
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
    title = " New Note (C-c C-c to save, C-c C-k to cancel) ",
    title_pos = "center",
  })

  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "markdown"

  -- Save callback
  local function save_note()
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(lines, "\n"):gsub("^%s+", ""):gsub("%s+$", "")

    vim.api.nvim_win_close(win, true)

    if text ~= "" then
      -- Pass captured position to create
      notes.create(text, {
        anchor = anchor,
        node_path = node_path,
        node_text_hash = node_text_hash,
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
  local note = display.get_note_at_cursor(M.buffer_notes)
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
end

-- Get project root from current buffer's file path
local function get_project_root()
  local file = vim.fn.expand("%:p")
  local dir = vim.fn.fnamemodify(file, ":h")

  local result = vim.fn.systemlist({ "git", "-C", dir, "rev-parse", "--show-toplevel" })
  if result[1] and not result[1]:match("^fatal") then
    return result[1]
  end

  return vim.fn.getcwd()
end

-- Edit note at cursor
function M.edit_note()
  local note = display.get_note_at_cursor(M.buffer_notes)
  if not note then
    vim.notify("No note at cursor", vim.log.levels.WARN)
    return
  end

  -- Capture project root before creating edit buffer (edit buffer has no file)
  local project_root = get_project_root()

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
    title = " Edit Note (C-c C-c to save, C-c C-k to cancel) ",
    title_pos = "center",
  })

  local lines = vim.split(note.text or "", "\n")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "markdown"

  local function save()
    local new_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local text = table.concat(new_lines, "\n"):gsub("^%s+", ""):gsub("%s+$", "")

    vim.api.nvim_win_close(win, true)

    if text ~= "" then
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
  -- Pass captured project_root since edit buffer has no associated file
  local prefix = config.get("keymap_prefix") or "<leader>h"
  vim.keymap.set({ "n", "i" }, prefix .. "k", function()
    M.insert_link({ project_root = project_root })
  end, { buffer = buf, desc = "Hemis: Insert link" })
end

-- List notes in a buffer
function M.list_notes()
  notes.list_for_buffer(function(err, result)
    if err then
      vim.notify("Failed to fetch notes", vim.log.levels.ERROR)
      return
    end

    if not result or #result == 0 then
      vim.notify("No notes for this file", vim.log.levels.INFO)
      return
    end

    -- Create notes list buffer
    local buf = vim.api.nvim_create_buf(false, true)
    local file = vim.fn.expand("%:t")

    local lines = { "Hemis notes for " .. file, "" }

    for i, note in ipairs(result) do
      local short_id = (note.id or ""):sub(1, 8)
      local stale = note.stale and " [stale]" or ""
      table.insert(lines, string.format("  %d [%s] L%d,C%d%s", i - 1, short_id, note.line or 0, note.column or 0, stale))

      local text = note.text or note.summary or ""
      for line in text:gmatch("[^\n]+") do
        table.insert(lines, "    " .. line)
      end
      table.insert(lines, "")
    end

    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].modifiable = false
    vim.api.nvim_buf_set_name(buf, "Hemis Notes")

    -- Open in split
    vim.cmd("split")
    vim.api.nvim_win_set_buf(0, buf)

    -- Store notes for navigation
    vim.b[buf].hemis_notes = result
    vim.b[buf].hemis_file = vim.fn.expand("%:p")

    -- Keymaps
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf })
    vim.keymap.set("n", "<CR>", function()
      M.visit_note_from_list(buf)
    end, { buffer = buf })
  end)
end

-- Visit note from list buffer
function M.visit_note_from_list(buf)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1]

  local notes_list = vim.b[buf].hemis_notes

  if not notes_list then
    return
  end

  -- Find which note we're on (rough heuristic: look for note index pattern)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local note_idx = nil

  for i = line, 1, -1 do
    local match = lines[i]:match("^%s*(%d+)%s+%[")
    if match then
      note_idx = tonumber(match) + 1
      break
    end
  end

  if note_idx and notes_list[note_idx] then
    local note = notes_list[note_idx]
    -- Use file from note itself (works for both notes list and backlinks)
    local file = note.file or vim.b[buf].hemis_file
    if not file then
      vim.notify("No file for this note", vim.log.levels.WARN)
      return
    end
    vim.cmd("close")
    vim.cmd("edit " .. file)
    vim.api.nvim_win_set_cursor(0, { note.line or 1, note.column or 0 })
    vim.cmd("normal! zz")
  end
end

-- Search notes
function M.search()
  vim.ui.input({ prompt = "Search: " }, function(query)
    if not query or query == "" then
      return
    end

    notes.search_project(query, { include_notes = true }, function(err, result)
      if err then
        vim.notify("Search failed", vim.log.levels.ERROR)
        return
      end

      if not result or #result == 0 then
        vim.notify("No results", vim.log.levels.INFO)
        return
      end

      -- Show results in quickfix
      local items = {}
      for _, hit in ipairs(result) do
        table.insert(items, {
          filename = hit.file,
          lnum = hit.line or 1,
          col = hit.column or 0,
          text = string.format("[%s] %s", hit.kind or "file", hit.text or hit.summary or ""),
        })
      end

      vim.fn.setqflist(items)
      vim.cmd("copen")
    end)
  end)
end

-- Insert note link
-- opts.project_root can override the auto-detected project root (useful from edit buffers)
function M.insert_link(opts)
  opts = opts or {}
  vim.ui.input({ prompt = "Search notes: " }, function(query)
    if not query or query == "" then
      return
    end

    notes.search(query, { project_root = opts.project_root }, function(err, result)
      if err or not result or #result == 0 then
        vim.notify("No notes found", vim.log.levels.WARN)
        return
      end

      local items = {}
      for _, note in ipairs(result) do
        local desc = note.summary or (note.text or ""):sub(1, 40)
        local short_id = (note.id or ""):sub(1, 8)
        table.insert(items, string.format("%s (%s)", desc, short_id))
      end

      vim.ui.select(items, { prompt = "Select note:" }, function(choice, idx)
        if choice and idx then
          local note = result[idx]
          local desc = note.summary or (note.text or ""):sub(1, 40)
          local link = string.format("[[%s][%s]]", desc, note.id)
          vim.api.nvim_put({ link }, "c", true, true)
        end
      end)
    end)
  end)
end

-- Index current file
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

    local counts = result.counts or {}
    local msg = string.format(
      "Hemis: %d notes, %d files, %d embeddings",
      counts.notes or 0,
      counts.files or 0,
      counts.embeddings or 0
    )
    vim.notify(msg, vim.log.levels.INFO)
  end)
end

-- Reattach stale note at cursor to current position
function M.reattach_note()
  local note = display.get_note_at_cursor(M.buffer_notes)
  if not note then
    vim.notify("No note at cursor", vim.log.levels.WARN)
    return
  end

  -- Check staleness from server-computed value or stored stale flag
  local is_stale = note.computedStale or note.stale or false

  if not is_stale then
    vim.notify("Note is not stale", vim.log.levels.INFO)
    return
  end

  -- Capture position before any async operations
  local ts = require("hemis.treesitter")
  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local node_text_hash = ts.get_node_text_hash()

  vim.ui.select({ "Yes", "No" }, { prompt = "Reattach note to current position?" }, function(choice)
    if choice == "Yes" then
      notes.reattach(note.id, {
        anchor = anchor,
        node_path = node_path,
        node_text_hash = node_text_hash,
      }, function(err, _)
        if not err then
          M.refresh()
        end
      end)
    end
  end)
end

-- Show backlinks for note at cursor
function M.show_backlinks()
  local note = display.get_note_at_cursor(M.buffer_notes)
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
    local short_id = (note.id or ""):sub(1, 8)

    local lines = { string.format("Backlinks to note %s", short_id), string.format("(%d notes link to this note)", #result), "" }

    for i, n in ipairs(result) do
      local n_short_id = (n.id or ""):sub(1, 8)
      table.insert(lines, string.format("  %d [%s] L%d,C%d", i - 1, n_short_id, n.line or 0, n.column or 0))

      local text = n.text or n.summary or ""
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

-- Show help
function M.help()
  local prefix = resolve_prefix(config.get("keymap_prefix") or "<leader>h")
  local help = {
    "Hemis Keybindings",
    "",
    prefix .. "a  - Add note at cursor",
    prefix .. "A  - Add multi-line note",
    prefix .. "l  - List notes for file",
    prefix .. "r  - Refresh notes display",
    prefix .. "R  - Reattach stale note",
    prefix .. "d  - Delete note at cursor",
    prefix .. "e  - Edit note at cursor",
    prefix .. "s  - Search notes/files",
    prefix .. "i  - Index current file",
    prefix .. "p  - Index project",
    prefix .. "k  - Insert note link",
    prefix .. "b  - Show backlinks",
    prefix .. "f  - List project files",
    prefix .. "S  - Save snapshot",
    prefix .. "L  - Load snapshot",
    prefix .. "x  - Explain region (visual)",
    prefix .. "?  - Show this help",
    "",
    ":HemisStatus       - Show backend status",
    ":HemisShutdown     - Stop backend",
    ":HemisViewFile     - View file content",
    ":HemisExplainRegion - Copy region for LLM",
    ":HemisSaveSnapshot - Save notes backup",
    ":HemisLoadSnapshot - Restore notes",
  }

  vim.notify(table.concat(help, "\n"), vim.log.levels.INFO)
end

-- Shutdown backend
function M.shutdown()
  rpc.stop()
  vim.notify("Hemis backend stopped", vim.log.levels.INFO)
end

-- View file content (via backend)
function M.view_file()
  vim.ui.input({ prompt = "File path: " }, function(file)
    if not file or file == "" then
      return
    end

    notes.get_file(file, function(err, result)
      if err then
        vim.notify("Failed to get file: " .. (err.message or "unknown"), vim.log.levels.ERROR)
        return
      end

      -- Create buffer with content
      local buf = vim.api.nvim_create_buf(false, true)
      local lines = vim.split(result.content or "", "\n")
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      vim.bo[buf].buftype = "nofile"
      vim.bo[buf].modifiable = false

      -- Set filetype based on extension
      local ext = file:match("%.(%w+)$")
      if ext then
        vim.bo[buf].filetype = ext
      end

      vim.cmd("split")
      vim.api.nvim_win_set_buf(0, buf)
    end)
  end)
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

  -- Capture tree-sitter info at the start of the selection
  local ts = require("hemis.treesitter")
  vim.api.nvim_win_set_cursor(0, { start_line, 0 })
  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local node_text_hash = ts.get_node_text_hash()
  local source_buf = vim.api.nvim_get_current_buf()
  local file = vim.fn.expand("%:p")

  -- Use polling approach so UI stays responsive
  local done = false
  local ai_err = nil
  local ai_result = nil

  notes.explain_region(file, start_line, end_line, true, false, function(err, result)
    ai_err = err
    ai_result = result
    done = true
  end)

  -- Poll with status updates until AI responds
  local elapsed = 0
  while not done and elapsed < 120000 do
    local msg = string.format("AI thinking... (%ds)", math.floor(elapsed / 1000))
    vim.api.nvim_echo({{msg, "Comment"}}, false, {})
    vim.cmd("redraw")
    vim.wait(500, function() return done end)
    elapsed = elapsed + 500
  end
  vim.api.nvim_echo({{""}}, false, {})

  if ai_err then
    vim.notify("Failed to explain region: " .. (ai_err.message or vim.inspect(ai_err)), vim.log.levels.ERROR)
    return
  end

  if not ai_result or not ai_result.explanation then
    vim.notify("No AI explanation available", vim.log.levels.WARN)
    return
  end


  -- Create note synchronously using coroutine-style wait
  local create_done = false
  local create_err = nil

  local provider = ai_result.ai and ai_result.ai.provider or "AI"
  local text = string.format("[%s] %s", provider, ai_result.explanation)

  notes.create(text, {
    anchor = anchor,
    node_path = node_path,
    node_text_hash = node_text_hash,
    source_buf = source_buf,
  }, function(err, _)
    create_err = err
    create_done = true
  end)

  vim.wait(5000, function() return create_done end)

  if create_err then
    vim.notify("Failed to create note: " .. (create_err.message or "unknown"), vim.log.levels.ERROR)
    return
  end

  M.refresh()
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

  -- Capture tree-sitter info at the start of the selection
  local ts = require("hemis.treesitter")
  vim.api.nvim_win_set_cursor(0, { start_line, 0 })
  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local node_text_hash = ts.get_node_text_hash()
  local source_buf = vim.api.nvim_get_current_buf()
  local file = vim.fn.expand("%:p")

  -- Use polling approach so UI stays responsive
  local done = false
  local ai_err = nil
  local ai_result = nil

  notes.explain_region(file, start_line, end_line, true, true, function(err, result)
    ai_err = err
    ai_result = result
    done = true
  end)

  -- Poll with status updates until AI responds
  local elapsed = 0
  while not done and elapsed < 120000 do
    local msg = string.format("AI thinking deeply... (%ds)", math.floor(elapsed / 1000))
    vim.api.nvim_echo({{msg, "Comment"}}, false, {})
    vim.cmd("redraw")
    vim.wait(500, function() return done end)
    elapsed = elapsed + 500
  end
  vim.api.nvim_echo({{""}}, false, {})

  if ai_err then
    vim.notify("Failed to explain region: " .. (ai_err.message or vim.inspect(ai_err)), vim.log.levels.ERROR)
    return
  end

  if not ai_result or not ai_result.explanation then
    vim.notify("No AI explanation available", vim.log.levels.WARN)
    return
  end

  -- Create note synchronously using coroutine-style wait
  local create_done = false
  local create_err = nil

  local provider = ai_result.ai and ai_result.ai.provider or "AI"
  local text = string.format("[%s detailed] %s", provider, ai_result.explanation)

  notes.create(text, {
    anchor = anchor,
    node_path = node_path,
    node_text_hash = node_text_hash,
    source_buf = source_buf,
  }, function(err, _)
    create_err = err
    create_done = true
  end)

  vim.wait(5000, function() return create_done end)

  if create_err then
    vim.notify("Failed to create note: " .. (create_err.message or "unknown"), vim.log.levels.ERROR)
    return
  end

  M.refresh()
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
    if result.indexedAt then
      table.insert(lines, string.format("  Last indexed: %s", os.date("%Y-%m-%d %H:%M:%S", result.indexedAt)))
    end
    table.insert(lines, "")
    local analysis_status
    if result.analyzed then
      analysis_status = result.analysisStale and "Stale (commit changed)" or "Up to date"
    elseif result.hasAnalysisFile then
      analysis_status = "Has file but not tracked"
    else
      analysis_status = "Not analyzed"
    end
    table.insert(lines, string.format("AI Analysis: %s", analysis_status))
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

-- Save snapshot
function M.save_snapshot()
  vim.ui.input({ prompt = "Snapshot path: ", default = "hemis-snapshot.json" }, function(path)
    if not path or path == "" then
      return
    end

    notes.save_snapshot(path, function(err, result)
      if err then
        vim.notify("Failed to save snapshot: " .. (err.message or "unknown"), vim.log.levels.ERROR)
        return
      end

      local counts = result.counts or {}
      vim.notify(
        string.format("Snapshot saved: %d notes, %d files", counts.notes or 0, counts.files or 0),
        vim.log.levels.INFO
      )
    end)
  end)
end

-- Load snapshot
function M.load_snapshot()
  vim.ui.input({ prompt = "Snapshot path: " }, function(path)
    if not path or path == "" then
      return
    end

    vim.ui.select({ "Yes", "No" }, { prompt = "Loading will replace all data. Continue?" }, function(choice)
      if choice ~= "Yes" then
        return
      end

      notes.load_snapshot(path, function(err, result)
        if err then
          vim.notify("Failed to load snapshot: " .. (err.message or "unknown"), vim.log.levels.ERROR)
          return
        end

        local counts = result.counts or {}
        vim.notify(
          string.format("Snapshot loaded: %d notes, %d files", counts.notes or 0, counts.files or 0),
          vim.log.levels.INFO
        )

        -- Refresh current buffer
        M.refresh()
      end)
    end)
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
  vim.api.nvim_create_user_command("HemisSearch", M.search, {})
  vim.api.nvim_create_user_command("HemisIndexFile", M.index_file, {})
  vim.api.nvim_create_user_command("HemisIndexProject", M.index_project, {})
  vim.api.nvim_create_user_command("HemisInsertLink", M.insert_link, {})
  vim.api.nvim_create_user_command("HemisBacklinks", M.show_backlinks, {})
  vim.api.nvim_create_user_command("HemisReattachNote", M.reattach_note, {})
  vim.api.nvim_create_user_command("HemisStatus", M.status, {})
  vim.api.nvim_create_user_command("HemisHelp", M.help, {})
  vim.api.nvim_create_user_command("HemisShutdown", M.shutdown, {})
  vim.api.nvim_create_user_command("HemisViewFile", M.view_file, {})
  vim.api.nvim_create_user_command("HemisExplainRegion", M.explain_region, { range = true })
  vim.api.nvim_create_user_command("HemisExplainRegionFull", M.explain_region_full, { range = true })
  vim.api.nvim_create_user_command("HemisIndexProjectAI", M.index_project_ai, {})
  vim.api.nvim_create_user_command("HemisProjectMeta", M.project_meta, {})
  vim.api.nvim_create_user_command("HemisSaveSnapshot", M.save_snapshot, {})
  vim.api.nvim_create_user_command("HemisLoadSnapshot", M.load_snapshot, {})
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
    { "s", M.search, "Search" },
    { "i", M.index_file, "Index file" },
    { "p", M.index_project, "Index project" },
    { "k", M.insert_link, "Insert link" },
    { "b", M.show_backlinks, "Show backlinks" },
    { "S", M.save_snapshot, "Save snapshot" },
    { "L", M.load_snapshot, "Load snapshot" },
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

  -- Clear git cache on buffer write (blob SHA changes)
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = group,
    pattern = "*",
    callback = function(ev)
      notes.clear_git_cache(ev.buf)
    end,
  })

  -- Cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
      rpc.stop()
    end,
  })
end

return M
