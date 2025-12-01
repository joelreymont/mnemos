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
    vim.notify(string.format("%d notes loaded", #M.buffer_notes), vim.log.levels.INFO)
  end)
end

-- Add note at cursor
function M.add_note()
  vim.ui.input({ prompt = "Note: " }, function(text)
    if not text or text == "" then
      return
    end

    notes.create(text, {}, function(err, _)
      if not err then
        M.refresh()
      end
    end)
  end)
end

-- Add multi-line note
function M.add_note_multiline()
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
      notes.create(text, {}, function(err, _)
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

-- Edit note at cursor
function M.edit_note()
  local note = display.get_note_at_cursor(M.buffer_notes)
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
function M.insert_link()
  vim.ui.input({ prompt = "Search notes: " }, function(query)
    if not query or query == "" then
      return
    end

    notes.search(query, function(err, result)
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
  notes.index_project()
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

-- Show help
function M.help()
  local prefix = config.get("keymap_prefix") or "<leader>h"
  local help = {
    "Hemis Keybindings",
    "",
    prefix .. "a  - Add note at cursor",
    prefix .. "A  - Add multi-line note",
    prefix .. "l  - List notes for file",
    prefix .. "r  - Refresh notes display",
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
    ":HemisListFiles    - Browse project files",
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

-- List project files
function M.list_files()
  notes.list_files(function(err, result)
    if err then
      vim.notify("Failed to list files: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    if not result or #result == 0 then
      vim.notify("No files found", vim.log.levels.INFO)
      return
    end

    local items = {}
    for _, f in ipairs(result) do
      table.insert(items, string.format("%s (%d bytes)", f.file, f.size or 0))
    end

    vim.ui.select(items, { prompt = "Select file:" }, function(choice, idx)
      if choice and idx then
        local file = result[idx].file
        vim.cmd("edit " .. file)
      end
    end)
  end)
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

-- Explain region (copy visual selection for LLM)
function M.explain_region()
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")
  local start_line = start_pos[2]
  local end_line = end_pos[2]

  if start_line == 0 or end_line == 0 then
    vim.notify("Select a region first", vim.log.levels.WARN)
    return
  end

  local file = vim.fn.expand("%:p")

  notes.explain_region(file, start_line, end_line, function(err, result)
    if err then
      vim.notify("Failed to explain region: " .. (err.message or "unknown"), vim.log.levels.ERROR)
      return
    end

    -- Copy to clipboard
    vim.fn.setreg("+", result.content or "")
    vim.fn.setreg("*", result.content or "")
    vim.notify(
      string.format("Copied %d lines to clipboard (LLM-ready)", end_line - start_line + 1),
      vim.log.levels.INFO
    )
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
  vim.api.nvim_create_user_command("HemisStatus", M.status, {})
  vim.api.nvim_create_user_command("HemisHelp", M.help, {})
  vim.api.nvim_create_user_command("HemisShutdown", M.shutdown, {})
  vim.api.nvim_create_user_command("HemisListFiles", M.list_files, {})
  vim.api.nvim_create_user_command("HemisViewFile", M.view_file, {})
  vim.api.nvim_create_user_command("HemisExplainRegion", M.explain_region, { range = true })
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
    { "d", M.delete_note, "Delete note" },
    { "e", M.edit_note, "Edit note" },
    { "s", M.search, "Search" },
    { "i", M.index_file, "Index file" },
    { "p", M.index_project, "Index project" },
    { "k", M.insert_link, "Insert link" },
    { "b", M.show_backlinks, "Show backlinks" },
    { "f", M.list_files, "List files" },
    { "S", M.save_snapshot, "Save snapshot" },
    { "L", M.load_snapshot, "Load snapshot" },
    { "?", M.help, "Help" },
  }

  for _, m in ipairs(mappings) do
    vim.keymap.set("n", prefix .. m[1], m[2], { desc = "Hemis: " .. m[3] })
  end

  -- Visual mode mapping for explain region
  vim.keymap.set("v", prefix .. "x", M.explain_region, { desc = "Hemis: Explain region" })
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

  -- Cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
      rpc.stop()
    end,
  })
end

return M
