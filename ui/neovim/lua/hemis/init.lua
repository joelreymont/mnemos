-- Hemis: A Second Brain for Your Code
-- Neovim plugin for code-anchored notes

local M = {}

-- Re-export submodules
M.rpc = require("hemis.rpc")
M.notes = require("hemis.notes")
M.display = require("hemis.display")
-- NOTE: treesitter module removed - server now handles anchor position, nodePath, and nodeTextHash
M.commands = require("hemis.commands")
M.config = require("hemis.config")
M.events = require("hemis.events")

-- Find buffer by file path, handling path canonicalization
-- Server sends canonicalized paths (e.g., /private/tmp/foo on macOS)
-- But buffers may have non-canonicalized names (e.g., /tmp/foo)
local function find_buffer_by_path(file_path)
  -- Try direct lookup first (fast path)
  local bufnr = vim.fn.bufnr(file_path)
  if bufnr ~= -1 then
    return bufnr
  end

  -- Canonicalize the event path for comparison
  local canonical_event = vim.fn.resolve(file_path)

  -- Check all loaded buffers for matching canonical path
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local buf_name = vim.api.nvim_buf_get_name(buf)
      if buf_name ~= "" then
        local canonical_buf = vim.fn.resolve(buf_name)
        if canonical_buf == canonical_event or canonical_buf == file_path then
          return buf
        end
      end
    end
  end

  return -1
end

-- Setup function (called by lazy.nvim)
function M.setup(opts)
  -- Initialize configuration
  M.config.setup(opts)

  -- Setup commands
  M.commands.setup_commands()

  -- Setup keymaps
  M.commands.setup_keymaps()

  -- Setup autocommands
  M.commands.setup_autocommands()

  -- Start event client for push notifications
  M.events.start()

  -- Register event handlers
  M.events.on("note-position-changed", function(event)
    -- Update display for the affected file
    local bufnr = find_buffer_by_path(event.file)
    if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
      M.display.update_note_position(bufnr, event.id, event.new_line, event.stale)
    end
  end)

  M.events.on("note-created", function(event)
    local bufnr = find_buffer_by_path(event.file)
    if bufnr == -1 or not vim.api.nvim_buf_is_loaded(bufnr) then
      return
    end

    -- Check if this is from explain_region (has pending timer)
    local pending_timer = M.commands._pending_status_timer
    if pending_timer then
      -- explain_region case: we handle refresh and cleanup here
      -- This runs outside the blocking RPC callback chain

      -- Increment generation FIRST to invalidate any queued timer callbacks
      M.commands._status_generation = (M.commands._status_generation or 0) + 1
      M.commands._pending_status_timer = nil
      M.commands.explain_region_in_progress = false

      -- Stop timer and clear message IMMEDIATELY when note arrives
      pending_timer:stop()
      pending_timer:close()
      vim.cmd("redraw!")
      vim.api.nvim_echo({ { "" } }, false, {})

      -- Fetch notes and render (timer already stopped)
      M.notes.list_for_buffer(function(err, result)
        if not err then
          local notes_list = result
          if result and result.notes then
            notes_list = result.notes
          end
          M.commands.buffer_notes = notes_list or {}
          M.display.render_notes(bufnr, M.commands.buffer_notes)
          M.display.cache_notes(bufnr, M.commands.buffer_notes)
        end
      end)
    else
      -- Normal case: just refresh
      M.commands.refresh()
    end
  end)

  M.events.on("note-deleted", function(_event)
    -- Full refresh since we don't know which buffer
    M.commands.refresh()
  end)

  M.events.on("note-updated", function(_event)
    -- Full refresh since we don't know which buffer
    M.commands.refresh()
  end)
end

-- Convenience exports
M.add_note = M.commands.add_note
M.add_note_multiline = M.commands.add_note_multiline
M.list_notes = M.commands.list_notes
M.refresh = M.commands.refresh
M.delete_note = M.commands.delete_note
M.edit_note = M.commands.edit_note
M.search = M.commands.search
M.index_project = M.commands.index_project
M.insert_link = M.commands.insert_link
M.status = M.commands.status
M.help = M.commands.help
M.shutdown = M.commands.shutdown
M.select_note = M.commands.select_note
M.clear_selection = M.commands.clear_selection
M.get_selected_note = M.commands.get_selected_note

-- Start backend (usually auto-started on first request)
M.start = M.rpc.start

-- Stop everything
function M.stop()
  M.events.stop()
  M.rpc.stop()
end

return M
