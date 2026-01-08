-- Test runner for Mnemos Neovim tests using plenary.busted
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

-- Count spec files to know when all tests are done
local spec_files = vim.fn.glob(test_root .. "/*_spec.lua", false, true)
local spec_count = #spec_files

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
local check_interval_ms = 100
local max_duration_ms = tonumber(vim.env.MNEMOS_TEST_TIMEOUT_MS) or 300000
local max_checks = math.floor(max_duration_ms / check_interval_ms)

check_timer:start(check_interval_ms, check_interval_ms, function()
  check_count = check_count + 1

  -- Check messages for completion
  vim.schedule(function()
    local messages = vim.fn.execute("messages")

    -- Check for failure patterns
    if messages:match("Failed%s*:%s*[1-9]") or messages:match("Errors%s*:%s*[1-9]") then
      failed = true
    end

    -- Count completed test files (each shows "Success: N" or "Failed: N")
    local _, success_count = messages:gsub("Success:%s+%d+", "")
    local _, failed_count = messages:gsub("Failed%s*:%s*%d+", "")
    local completed_count = success_count + failed_count

    if completed_count >= spec_count or check_count >= max_checks then
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
