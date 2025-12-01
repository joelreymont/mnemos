-- Test helpers for Hemis Neovim plugin
local M = {}

-- Namespace for hemis extmarks
local NS_NAME = "hemis"

-- Create a test buffer with content
function M.setup_test_buffer(content)
  content = content or { "fn main() {", "    let x = 1;", "}" }
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, content)
  vim.bo[buf].filetype = "rust"
  return buf
end

-- Clean up test state
function M.cleanup()
  -- Stop any RPC connections to avoid state leaking between tests
  local ok, rpc = pcall(require, "hemis.rpc")
  if ok and rpc.stop then
    rpc.stop()
  end

  -- Close all buffers except current
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
  end
end

-- Capture the current display state for verification
-- Returns detailed virt_lines/virt_text structure for snapshot testing
function M.capture_display_state(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local display = require("hemis.display")
  local ns = display.ns_id
  local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, { details = true })

  local extmarks = {}
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    local virt_text = ""
    local virt_lines_raw = {}  -- Raw virt_lines structure for detailed testing
    local hl_groups = {}       -- All highlight groups used

    if details.virt_lines then
      -- Note overlays use virt_lines (block above)
      for _, vline in ipairs(details.virt_lines) do
        local line_parts = {}
        for _, chunk in ipairs(vline) do
          virt_text = virt_text .. (chunk[1] or "")
          table.insert(line_parts, { text = chunk[1], hl = chunk[2] })
          if chunk[2] then
            hl_groups[chunk[2]] = true
          end
        end
        table.insert(virt_lines_raw, line_parts)
        virt_text = virt_text .. "\n"
      end
    elseif details.virt_text then
      -- Inline virtual text (minimal style)
      for _, chunk in ipairs(details.virt_text) do
        virt_text = virt_text .. (chunk[1] or "")
        if chunk[2] then
          hl_groups[chunk[2]] = true
        end
      end
    end

    -- Convert hl_groups to list
    local hl_list = {}
    for hl, _ in pairs(hl_groups) do
      table.insert(hl_list, hl)
    end

    table.insert(extmarks, {
      id = mark[1],
      line = mark[2] + 1, -- Convert to 1-indexed
      col = mark[3],
      text = virt_text,
      virt_lines = virt_lines_raw,     -- Detailed virt_lines structure
      virt_lines_above = details.virt_lines_above,
      virt_text_pos = details.virt_text_pos,
      hl_groups = hl_list,             -- List of highlight groups used
    })
  end

  return {
    buffer = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"),
    extmarks = extmarks,
    cursor = vim.api.nvim_win_get_cursor(0),
    bufname = vim.api.nvim_buf_get_name(buf),
  }
end

-- Assert that an extmark exists at line with text containing pattern
function M.assert_extmark_at_line(state, line, pattern)
  for _, mark in ipairs(state.extmarks) do
    if mark.line == line and string.find(mark.text, pattern) then
      return true
    end
  end
  error(string.format("No extmark at line %d matching '%s'", line, pattern))
end

-- Assert buffer contains text
function M.assert_buffer_contains(buf, pattern)
  buf = buf or vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local content = table.concat(lines, "\n")
  if not string.find(content, pattern) then
    error(string.format("Buffer does not contain '%s'\nContent: %s", pattern, content))
  end
  return true
end

-- Mock RPC for unit tests
function M.mock_rpc(responses)
  local rpc = require("hemis.rpc")
  local original_request = rpc.request

  rpc.request = function(method, params, callback)
    local response = responses[method]
    if type(response) == "function" then
      response = response(params)
    end
    if callback then
      vim.schedule(function()
        callback(nil, response)
      end)
    end
    return response
  end

  return function()
    rpc.request = original_request
  end
end

-- Wait for async operations (simple delay)
function M.wait(ms)
  ms = ms or 100
  vim.wait(ms)
end

-- Wait for a condition with libuv event processing
-- This is needed for socket callbacks to fire in headless mode
function M.wait_for(condition_fn, timeout_ms)
  timeout_ms = timeout_ms or 5000
  local uv = vim.uv or vim.loop
  local start = uv.now()
  while not condition_fn() and (uv.now() - start) < timeout_ms do
    -- Process any pending libuv events
    uv.run("nowait")
    -- Also give vim a chance to process
    vim.wait(10, condition_fn, 10)
  end
  return condition_fn()
end

--------------------------------------------------------------------------------
-- Snapshot Testing
--------------------------------------------------------------------------------

-- Directory for snapshot files
local SNAPSHOT_DIR = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h") .. "/snapshots"

-- Serialize display state to string for comparison
function M.serialize_state(state)
  local lines = {}
  table.insert(lines, "=== Buffer Content ===")
  table.insert(lines, state.buffer)
  table.insert(lines, "")
  table.insert(lines, "=== Extmarks ===")
  for i, mark in ipairs(state.extmarks) do
    table.insert(lines, string.format("[%d] Line %d: %s", i, mark.line, mark.text:gsub("\n", "\\n")))
  end
  table.insert(lines, "")
  table.insert(lines, string.format("=== Cursor: %d,%d ===", state.cursor[1], state.cursor[2]))
  return table.concat(lines, "\n")
end

-- Get snapshot file path for a test
function M.snapshot_path(name)
  vim.fn.mkdir(SNAPSHOT_DIR, "p")
  return SNAPSHOT_DIR .. "/" .. name .. ".snap"
end

-- Load existing snapshot or return nil
function M.load_snapshot(name)
  local path = M.snapshot_path(name)
  local f = io.open(path, "r")
  if not f then
    return nil
  end
  local content = f:read("*a")
  f:close()
  return content
end

-- Save snapshot
function M.save_snapshot(name, content)
  local path = M.snapshot_path(name)
  local f = io.open(path, "w")
  if not f then
    error("Cannot write snapshot: " .. path)
  end
  f:write(content)
  f:close()
end

-- Assert state matches snapshot
-- Set HEMIS_UPDATE_SNAPSHOTS=1 to update/create snapshots
-- Without this env var, missing snapshots FAIL (must be committed first)
function M.assert_snapshot(name, state)
  local serialized = M.serialize_state(state)
  local existing = M.load_snapshot(name)

  if vim.env.HEMIS_UPDATE_SNAPSHOTS == "1" then
    -- Update mode - create or overwrite snapshot
    M.save_snapshot(name, serialized)
    if existing == nil then
      print("Created snapshot: " .. name)
    else
      print("Updated snapshot: " .. name)
    end
    return true
  end

  if existing == nil then
    -- Snapshot missing and not in update mode - FAIL
    error("Snapshot missing: " .. name .. "\nRun with HEMIS_UPDATE_SNAPSHOTS=1 to create it")
  end

  -- Compare
  if serialized ~= existing then
    local diff_lines = {}
    table.insert(diff_lines, "Snapshot mismatch for: " .. name)
    table.insert(diff_lines, "")
    table.insert(diff_lines, "Expected:")
    table.insert(diff_lines, existing)
    table.insert(diff_lines, "")
    table.insert(diff_lines, "Actual:")
    table.insert(diff_lines, serialized)
    table.insert(diff_lines, "")
    table.insert(diff_lines, "Run with HEMIS_UPDATE_SNAPSHOTS=1 to update")
    error(table.concat(diff_lines, "\n"))
  end

  return true
end

--------------------------------------------------------------------------------
-- Screen Capture (TUI state)
--------------------------------------------------------------------------------

-- Capture what would be rendered on screen
-- Note: This requires Neovim UI protocol, works in headless mode
function M.capture_screen()
  local lines = {}
  local win = vim.api.nvim_get_current_win()
  local buf = vim.api.nvim_win_get_buf(win)
  local win_height = vim.api.nvim_win_get_height(win)
  local win_width = vim.api.nvim_win_get_width(win)

  -- Get visible line range
  local top_line = vim.fn.line("w0")
  local bot_line = vim.fn.line("w$")

  -- Capture each visible line
  for lnum = top_line, bot_line do
    local line = vim.fn.getline(lnum) or ""
    -- Truncate to window width
    if #line > win_width then
      line = line:sub(1, win_width)
    end
    table.insert(lines, string.format("%3d| %s", lnum, line))
  end

  return {
    lines = lines,
    cursor = vim.api.nvim_win_get_cursor(win),
    mode = vim.api.nvim_get_mode().mode,
    cmdline = vim.fn.getcmdline(),
  }
end

-- Serialize screen state
function M.serialize_screen(screen)
  local result = {}
  table.insert(result, "=== Screen ===")
  for _, line in ipairs(screen.lines) do
    table.insert(result, line)
  end
  table.insert(result, "")
  table.insert(result, string.format("Cursor: %d,%d  Mode: %s", screen.cursor[1], screen.cursor[2], screen.mode))
  if screen.cmdline and screen.cmdline ~= "" then
    table.insert(result, "Cmdline: " .. screen.cmdline)
  end
  return table.concat(result, "\n")
end

-- Assert screen matches snapshot
-- Set HEMIS_UPDATE_SNAPSHOTS=1 to update/create snapshots
-- Without this env var, missing snapshots FAIL (must be committed first)
function M.assert_screen_snapshot(name)
  local screen = M.capture_screen()
  local serialized = M.serialize_screen(screen)
  local existing = M.load_snapshot(name .. ".screen")

  if vim.env.HEMIS_UPDATE_SNAPSHOTS == "1" then
    -- Update mode - create or overwrite snapshot
    M.save_snapshot(name .. ".screen", serialized)
    if existing == nil then
      print("Created screen snapshot: " .. name)
    else
      print("Updated screen snapshot: " .. name)
    end
    return true
  end

  if existing == nil then
    -- Snapshot missing and not in update mode - FAIL
    error("Screen snapshot missing: " .. name .. "\nRun with HEMIS_UPDATE_SNAPSHOTS=1 to create it")
  end

  -- Compare
  if serialized ~= existing then
    error("Screen snapshot mismatch for: " .. name .. "\n\nExpected:\n" .. existing .. "\n\nActual:\n" .. serialized .. "\n\nRun with HEMIS_UPDATE_SNAPSHOTS=1 to update")
  end

  return true
end

return M
