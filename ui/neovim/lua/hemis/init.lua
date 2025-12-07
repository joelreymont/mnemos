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
    local bufnr = vim.fn.bufnr(event.file)
    if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
      M.display.update_note_position(bufnr, event.id, event.new_line, event.stale)
    end
  end)

  M.events.on("note-created", function(event)
    -- Refresh display for the file where note was created
    local bufnr = vim.fn.bufnr(event.file)
    if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
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
