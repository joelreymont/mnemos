-- Simple test runner for Hemis Neovim tests
-- Usage: nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua" -c "qa!"

local function run_tests()
  local plugin_root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h")
  local test_root = plugin_root .. "/tests"

  -- Add paths
  vim.opt.runtimepath:prepend(plugin_root)
  package.path = test_root .. "/?.lua;" .. package.path

  -- Load helpers
  local helpers = require("helpers")

  -- Collect test results
  local passed = 0
  local failed = 0
  local skipped = 0
  local errors = {}

  -- Simple describe/it implementation
  local current_describe = ""

  _G.describe = function(name, fn)
    current_describe = name
    print("\n" .. name)
    fn()
  end

  _G.it = function(name, fn)
    local full_name = current_describe .. " > " .. name
    local ok, err = pcall(fn)
    if ok then
      passed = passed + 1
      print("  [PASS] " .. name)
    elseif type(err) == "string" and err:match("^.*PENDING:") then
      skipped = skipped + 1
      print("  [SKIP] " .. name)
    else
      failed = failed + 1
      print("  [FAIL] " .. name)
      print("         " .. tostring(err))
      table.insert(errors, { name = full_name, error = err })
    end
  end

  _G.before_each = function(fn)
    -- Store for use by it()
    _G._before_each = fn
  end

  _G.after_each = function(fn)
    _G._after_each = fn
  end

  -- Override it to call before/after
  local original_it = _G.it
  _G.it = function(name, fn)
    original_it(name, function()
      if _G._before_each then
        _G._before_each()
      end
      fn()
      if _G._after_each then
        _G._after_each()
      end
    end)
  end

  -- Skip marker for tests that can't run
  _G.pending = function(msg)
    error("PENDING: " .. (msg or "test skipped"))
  end

  -- Simple assertions
  _G.assert = setmetatable({}, {
    __call = function(_, v, msg)
      if not v then
        error(msg or "assertion failed")
      end
    end,
    __index = {
      equals = function(expected, actual)
        if expected ~= actual then
          error(string.format("Expected %s but got %s", vim.inspect(expected), vim.inspect(actual)))
        end
      end,
      truthy = function(v, msg)
        if not v then
          error(msg or "expected truthy value")
        end
      end,
      is_nil = function(v)
        if v ~= nil then
          error(string.format("expected nil but got %s", vim.inspect(v)))
        end
      end,
      is_not_nil = function(v)
        if v == nil then
          error("expected non-nil value")
        end
      end,
      is_true = function(v)
        if v ~= true then
          error(string.format("expected true but got %s", vim.inspect(v)))
        end
      end,
      is_false = function(v)
        if v ~= false then
          error(string.format("expected false but got %s", vim.inspect(v)))
        end
      end,
      is_boolean = function(v)
        if type(v) ~= "boolean" then
          error(string.format("expected boolean but got %s", type(v)))
        end
      end,
      is_number = function(v)
        if type(v) ~= "number" then
          error(string.format("expected number but got %s", type(v)))
        end
      end,
      is_string = function(v)
        if type(v) ~= "string" then
          error(string.format("expected string but got %s", type(v)))
        end
      end,
      is_table = function(v)
        if type(v) ~= "table" then
          error(string.format("expected table but got %s", type(v)))
        end
      end,
    },
  })

  -- Run test files
  local test_files = vim.fn.glob(test_root .. "/*_spec.lua", false, true)

  for _, file in ipairs(test_files) do
    print("\n=== Running: " .. vim.fn.fnamemodify(file, ":t") .. " ===")
    _G._before_each = nil
    _G._after_each = nil
    local ok, err = pcall(dofile, file)
    if not ok then
      print("ERROR loading test file: " .. tostring(err))
      failed = failed + 1
    end
  end

  -- Summary
  print("\n" .. string.rep("=", 50))
  print(string.format("Results: %d passed, %d failed, %d skipped", passed, failed, skipped))

  if #errors > 0 then
    print("\nFailures:")
    for i, e in ipairs(errors) do
      print(string.format("  %d) %s", i, e.name))
      print("     " .. tostring(e.error))
    end
  end

  -- Exit with appropriate code
  if failed > 0 then
    vim.cmd("cquit 1")
  else
    vim.cmd("quit")
  end
end

-- Run after a short delay to let Neovim initialize
vim.defer_fn(run_tests, 100)
