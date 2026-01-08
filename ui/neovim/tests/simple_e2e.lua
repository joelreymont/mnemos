-- Simple standalone e2e test for explain-region
-- Run: MNEMOS_AI_PROVIDER=claude nvim --headless -u tests/minimal_init.lua -l tests/simple_e2e.lua

-- Set up paths
local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h:h")
vim.opt.rtp:prepend(root)

-- Find plenary
local plenary_paths = {
  vim.fn.stdpath("data") .. "/lazy/plenary.nvim",
  vim.fn.stdpath("data") .. "/site/pack/packer/start/plenary.nvim",
}
for _, path in ipairs(plenary_paths) do
  if vim.fn.isdirectory(path) == 1 then
    vim.opt.rtp:prepend(path)
    break
  end
end

print("=== Mnemos E2E Test ===")

-- Check AI provider
local ai_provider = vim.env.MNEMOS_AI_PROVIDER
if not ai_provider then
  print("ERROR: MNEMOS_AI_PROVIDER not set")
  vim.cmd("cquit 1")
  return
end
print("AI Provider: " .. ai_provider)

-- Find backend
local backend_paths = {
  root .. "/../../target/release/mnemos",
  root .. "/../../target/debug/mnemos",
}
local backend = nil
for _, path in ipairs(backend_paths) do
  local expanded = vim.fn.fnamemodify(path, ":p")
  if vim.fn.executable(expanded) == 1 then
    backend = expanded
    break
  end
end

if not backend then
  print("ERROR: Backend not found")
  vim.cmd("cquit 1")
  return
end
print("Backend: " .. backend)

-- Create temp directory
local test_dir = vim.fn.tempname() .. "_mnemos_e2e"
vim.fn.mkdir(test_dir, "p")
print("Test dir: " .. test_dir)

-- Create test file
local test_file = test_dir .. "/test.rs"
local code = [[fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}
]]
local f = io.open(test_file, "w")
if f then
  f:write(code)
  f:close()
end
print("Test file: " .. test_file)

-- Configure mnemos
local config = require("mnemos.config")
config.setup({
  backend = backend,
  mnemos_dir = test_dir,
  auto_refresh = false,
  keymaps = false,
  log_level = "debug",
})

-- Start RPC
local rpc = require("mnemos.rpc")
rpc.stop() -- Clean up any existing connection

print("Starting RPC connection...")
local connected = false
local connection_error = nil

rpc.start(function(ok)
  if ok then
    connected = true
    print("Connected to backend!")
  else
    connection_error = "Failed to connect"
    print("ERROR: " .. connection_error)
  end
end)

-- Wait for connection (up to 10 seconds)
local start_time = vim.uv.now()
while not connected and not connection_error and (vim.uv.now() - start_time) < 10000 do
  vim.uv.run("nowait")
  vim.wait(100, function() return connected or connection_error ~= nil end, 50)
end

if connection_error then
  print("FAILED: " .. connection_error)
  -- Check log file
  local log_file = test_dir .. "/mnemos.log"
  local lf = io.open(log_file, "r")
  if lf then
    print("=== mnemos.log ===")
    print(lf:read("*a"))
    lf:close()
  end
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

if not connected then
  print("TIMEOUT waiting for connection")
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

-- Test explain-region RPC
print("\nTesting mnemos/explain-region with AI...")
local result = nil
local err = nil
local done = false

rpc.request("mnemos/explain-region", {
  file = test_file,
  startLine = 2,
  endLine = 4,
  projectRoot = test_dir,
  useAI = true,
}, function(e, r)
  err = e
  result = r
  done = true
end)

-- Wait for AI response (up to 60 seconds)
start_time = vim.uv.now()
while not done and (vim.uv.now() - start_time) < 60000 do
  vim.uv.run("nowait")
  vim.wait(500, function() return done end, 100)
  if not done and (vim.uv.now() - start_time) % 10000 < 500 then
    print("  Still waiting for AI response... " .. math.floor((vim.uv.now() - start_time) / 1000) .. "s")
  end
end

if not done then
  print("TIMEOUT waiting for AI response")
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

if err then
  print("ERROR from RPC: " .. vim.inspect(err))
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

if not result then
  print("ERROR: No result returned")
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

print("\n=== RESULT ===")
print("Has explanation: " .. tostring(result.explanation ~= nil))
if result.explanation then
  print("Explanation length: " .. #result.explanation)
  print("Preview: " .. result.explanation:sub(1, 100) .. "...")
end
if result.ai then
  print("AI status: " .. tostring(result.ai.statusDisplay))
end

-- Create note from explanation
print("\nCreating note...")
local note_done = false
local note_result = nil
local note_err = nil

local note_text = (result.ai and result.ai.statusDisplay or "[AI]") .. " " .. result.explanation
rpc.request("notes/create", {
  file = test_file,
  line = 2,
  column = 0,
  text = note_text,
  projectRoot = test_dir,
}, function(e, r)
  note_err = e
  note_result = r
  note_done = true
end)

-- Wait for note creation
start_time = vim.uv.now()
while not note_done and (vim.uv.now() - start_time) < 5000 do
  vim.uv.run("nowait")
  vim.wait(100, function() return note_done end, 50)
end

if note_err then
  print("ERROR creating note: " .. vim.inspect(note_err))
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

print("Note created! ID: " .. tostring(note_result and note_result.id))

-- Verify note exists
print("\nVerifying note...")
local list_done = false
local notes = nil

-- Small delay to allow backend to persist
vim.wait(500, function() return false end, 100)

rpc.request("notes/list-for-file", {
  file = test_file,
  projectRoot = test_dir,
  includeStale = true,
}, function(e, r)
  if e then
    print("List error: " .. vim.inspect(e))
  end
  notes = r
  print("Raw list result: " .. vim.inspect(r))
  list_done = true
end)

start_time = vim.uv.now()
while not list_done and (vim.uv.now() - start_time) < 5000 do
  vim.uv.run("nowait")
  vim.wait(100, function() return list_done end, 50)
end

-- Handle both {notes: [...]} and direct array response
local note_list = notes
if notes and notes.notes then
  note_list = notes.notes
end

if note_list and #note_list > 0 then
  print("Found " .. #note_list .. " note(s) in file")
  print("\n=== SUCCESS ===")
  print("Full explain-region flow works!")
else
  print("ERROR: No notes found")
  rpc.stop()
  vim.fn.delete(test_dir, "rf")
  vim.cmd("cquit 1")
  return
end

-- Cleanup
rpc.stop()
vim.fn.delete(test_dir, "rf")
print("\nCleanup complete.")
vim.cmd("qall!")
