-- JSON-RPC client for Hemis backend
local config = require("hemis.config")

local M = {}

-- State
M.job_id = nil
M.request_id = 0
M.pending = {} -- id -> callback
M.buffer = ""

-- Logging
local function log(level, msg)
  local levels = { debug = 1, info = 2, warn = 3, error = 4 }
  local cfg_level = levels[config.get("log_level")] or 3
  if levels[level] >= cfg_level then
    vim.notify("[hemis] " .. msg, vim.log.levels[level:upper()])
  end
end

-- Encode a request with Content-Length framing
local function encode_request(method, params, id)
  local body = vim.json.encode({
    jsonrpc = "2.0",
    method = method,
    params = params or vim.empty_dict(),
    id = id,
  })
  return string.format("Content-Length: %d\r\n\r\n%s", #body, body)
end

-- Parse Content-Length framed response from buffer
local function parse_response(buf)
  local header_end = buf:find("\r\n\r\n", 1, true)
  if not header_end then
    return nil, buf
  end

  local header = buf:sub(1, header_end - 1)
  local length = header:match("Content%-Length:%s*(%d+)")
  if not length then
    return nil, buf
  end

  length = tonumber(length)
  local body_start = header_end + 4
  local body_end = body_start + length - 1

  if #buf < body_end then
    return nil, buf -- Need more data
  end

  local body = buf:sub(body_start, body_end)
  local remaining = buf:sub(body_end + 1)

  local ok, decoded = pcall(vim.json.decode, body)
  if not ok then
    log("error", "Failed to decode JSON: " .. body)
    return nil, remaining
  end

  return decoded, remaining
end

-- Handle incoming data from backend
local function on_stdout(_, data, _)
  if not data then
    return
  end

  -- Concatenate data chunks
  for _, chunk in ipairs(data) do
    M.buffer = M.buffer .. chunk
  end

  -- Parse all complete responses
  while true do
    local response, remaining = parse_response(M.buffer)
    if not response then
      break
    end
    M.buffer = remaining

    -- Find and call the callback
    local id = response.id
    if id and M.pending[id] then
      local callback = M.pending[id]
      M.pending[id] = nil
      if response.error then
        callback(response.error, nil)
      else
        callback(nil, response.result)
      end
    end
  end
end

local function on_stderr(_, data, _)
  if data then
    for _, line in ipairs(data) do
      if line and line ~= "" then
        log("debug", "backend stderr: " .. line)
      end
    end
  end
end

local function on_exit(_, code, _)
  log("info", "Backend exited with code " .. tostring(code))
  M.job_id = nil
  M.buffer = ""
  -- Fail all pending requests
  for id, callback in pairs(M.pending) do
    callback({ code = -1, message = "Backend process exited" }, nil)
    M.pending[id] = nil
  end
end

-- Start the backend process
function M.start()
  if M.job_id then
    return true
  end

  local backend = config.get("backend")
  if not backend then
    log("error", "No backend configured. Set hemis.backend or build the Rust backend.")
    return false
  end

  if vim.fn.executable(backend) ~= 1 then
    log("error", "Backend not found or not executable: " .. backend)
    return false
  end

  local env_config = config.get("backend_env") or {}
  local env_dict = {}

  -- Convert to dictionary format for jobstart
  for k, v in pairs(env_config) do
    if type(k) == "number" then
      -- Array format: {"KEY=VALUE", ...}
      local key, val = v:match("([^=]+)=(.*)")
      if key then
        env_dict[key] = val
      end
    else
      -- Map format: {KEY = "VALUE", ...}
      env_dict[k] = v
    end
  end

  -- Merge with current environment
  local full_env = vim.tbl_extend("force", vim.fn.environ(), env_dict)

  M.job_id = vim.fn.jobstart({ backend }, {
    on_stdout = on_stdout,
    on_stderr = on_stderr,
    on_exit = on_exit,
    env = full_env,
    stdin = "pipe",
    stdout_buffered = false,
  })

  if M.job_id <= 0 then
    log("error", "Failed to start backend process")
    M.job_id = nil
    return false
  end

  log("info", "Backend started (pid " .. M.job_id .. ")")
  return true
end

-- Stop the backend process
function M.stop()
  if not M.job_id then
    return
  end

  -- Send shutdown request
  M.request("shutdown", {}, function() end)

  -- Give it a moment, then force kill
  vim.defer_fn(function()
    if M.job_id then
      vim.fn.jobstop(M.job_id)
      M.job_id = nil
    end
  end, 100)
end

-- Send an async request
function M.request(method, params, callback)
  if not M.start() then
    if callback then
      callback({ code = -1, message = "Backend not running" }, nil)
    end
    return
  end

  M.request_id = M.request_id + 1
  local id = M.request_id

  if callback then
    M.pending[id] = callback
  end

  local msg = encode_request(method, params, id)
  vim.fn.chansend(M.job_id, msg)

  log("debug", "Sent request " .. id .. ": " .. method)
end

-- Send a synchronous request (blocks, use sparingly)
function M.request_sync(method, params, timeout_ms)
  timeout_ms = timeout_ms or 5000

  local result = nil
  local err = nil
  local done = false

  M.request(method, params, function(e, r)
    err = e
    result = r
    done = true
  end)

  local start = vim.loop.now()
  while not done and (vim.loop.now() - start) < timeout_ms do
    vim.wait(10, function()
      return done
    end)
  end

  if not done then
    return { code = -1, message = "Request timed out" }, nil
  end

  return err, result
end

-- Check if backend is running
function M.is_running()
  return M.job_id ~= nil
end

return M
