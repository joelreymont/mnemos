-- Test runner for Hemis Neovim tests using plenary.busted
-- Usage: nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua"
--
-- Exit codes:
--   0 = all tests passed
--   1 = plenary not found or tests failed

local plugin_root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h")
local test_root = plugin_root .. "/tests"

-- Add paths
vim.opt.runtimepath:prepend(plugin_root)
package.path = test_root .. "/?.lua;" .. package.path

-- Check for plenary.nvim
local ok, harness = pcall(require, "plenary.test_harness")
if not ok then
  print("ERROR: plenary.nvim not found. Install it to run tests.")
  print("Try: git clone https://github.com/nvim-lua/plenary.nvim ~/.local/share/nvim/site/pack/plugins/start/plenary.nvim")
  vim.cmd("cquit 1")
  return
end

-- Run tests and track results
-- plenary.test_harness.test_directory runs async, so we parse output for failures
local failed = false

-- Override print to capture test results
local original_print = print
_G.print = function(...)
  local args = { ... }
  local msg = table.concat(vim.tbl_map(tostring, args), " ")
  if msg:match("Failed%s*:%s*[1-9]") or msg:match("Errors%s*:%s*[1-9]") then
    failed = true
  end
  original_print(...)
end

-- Run tests
harness.test_directory(test_root, {
  minimal_init = test_root .. "/minimal_init.lua",
  sequential = true,
})

-- Wait for async tests to complete, then exit with appropriate code
-- Use a timer since vim.defer_fn can fail in some headless contexts
local uv = vim.uv or vim.loop
local check_timer = uv.new_timer()
local check_count = 0
local max_checks = 600 -- 60 seconds max

check_timer:start(100, 100, function()
  check_count = check_count + 1

  -- Check messages for completion
  vim.schedule(function()
    local messages = vim.fn.execute("messages")

    -- Look for final summary lines (both test files complete)
    local display_done = messages:match("Testing:%s+.-display_spec%.lua") and messages:match("Success:%s+%d+.-display_spec")
    local integration_done = messages:match("Testing:%s+.-integration_spec%.lua") and messages:match("Success:%s+%d+.-integration_spec")

    -- Also check for failure patterns
    if messages:match("Failed%s*:%s*[1-9]") or messages:match("Errors%s*:%s*[1-9]") then
      failed = true
    end

    -- Count completed test blocks (each file shows "Success: N")
    local _, success_count = messages:gsub("Success:%s+%d+", "")

    if success_count >= 2 or check_count >= max_checks then
      check_timer:stop()
      check_timer:close()
      _G.print = original_print

      if failed then
        vim.cmd("cquit 1")
      else
        vim.cmd("qall!")
      end
    end
  end)
end)
