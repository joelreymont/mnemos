-- Hemis: A Second Brain for Your Code
-- Neovim plugin for code-anchored notes

local M = {}

-- Re-export submodules
M.rpc = require("hemis.rpc")
M.notes = require("hemis.notes")
M.display = require("hemis.display")
M.treesitter = require("hemis.treesitter")
M.commands = require("hemis.commands")
M.config = require("hemis.config")

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

-- Start backend (usually auto-started on first request)
M.start = M.rpc.start
M.stop = M.rpc.stop

return M
