-- Test runner for Hemis Neovim tests using plenary.busted
-- Usage: nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua"

local plugin_root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h")
local test_root = plugin_root .. "/tests"

-- Add paths
vim.opt.runtimepath:prepend(plugin_root)
package.path = test_root .. "/?.lua;" .. package.path

-- Use plenary.busted for proper async test handling
local ok, plenary_busted = pcall(require, "plenary.busted")
if not ok then
  print("ERROR: plenary.nvim not found. Install it to run tests.")
  print("Try: git clone https://github.com/nvim-lua/plenary.nvim ~/.local/share/nvim/site/pack/plugins/start/plenary.nvim")
  vim.cmd("cquit 1")
  return
end

-- Run tests using PlenaryBustedDirectory which handles async properly
-- This processes libuv events and vim.wait/vim.schedule correctly
vim.cmd(string.format("PlenaryBustedDirectory %s {minimal_init = '%s/minimal_init.lua'}", test_root, test_root))
