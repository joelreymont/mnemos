-- JSON-RPC client for Hemis backend (Unix socket mode)
local config = require("hemis.config")

local M = {}

-- Helper: schedule a function to run after a delay using libuv timer
-- Works better than vim.defer_fn in some headless/test contexts
-- Signature matches vim.defer_fn: (fn, delay_ms)
local function schedule_after(fn, delay_ms)
  local uv = vim.uv or vim.loop
  local timer = uv.new_timer()
  timer:start(delay_ms, 0, function()
    timer:stop()
    timer:close()
    vim.schedule(fn)
  end)
end

-- Expected protocol version (bump when backend protocol changes)
local EXPECTED_PROTOCOL_VERSION = 1

-- State
M.socket = nil -- vim.uv TCP handle (works for Unix sockets too)
M.request_id = 0
M.pending = {} -- id -> callback
M.buffer = ""
M.connected = false
M.connecting = false

-- Paths (must be async-safe - no vim.fn.* in callbacks)
local function get_hemis_dir()
  local custom = config.get("hemis_dir")
  if custom then
    -- Expand ~ manually for async safety
    if custom:sub(1, 1) == "~" then
      return os.getenv("HOME") .. custom:sub(2)
    end
    return custom
  end
  -- Check HEMIS_DIR env var (used by demo automation)
  local env_dir = os.getenv("HEMIS_DIR")
  if env_dir then
    return env_dir
  end
  return os.getenv("HOME") .. "/.hemis"
end

local function get_socket_path()
  return get_hemis_dir() .. "/hemis.sock"
end

local function get_lock_path()
  return get_hemis_dir() .. "/hemis.lock"
end

local function get_log_path()
  return get_hemis_dir() .. "/hemis.log"
end

-- Async-safe helpers (no vim.fn.* - safe to call in libuv callbacks)
local function mkdir_p(dir)
  os.execute(string.format('mkdir -p "%s"', dir))
end

local function file_exists(path)
  local stat = vim.uv.fs_stat(path)
  return stat ~= nil
end

local function is_executable(path)
  local stat = vim.uv.fs_stat(path)
  if not stat then return false end
  -- Check if it's a file and has user execute bit (mode & 0100)
  return stat.type == "file" and math.floor(stat.mode / 64) % 2 == 1
end

local function shell_escape(s)
  return "'" .. s:gsub("'", "'\\''") .. "'"
end

-- Read last N lines from log file for startup error messages
local function read_log_tail(max_lines)
  local log_path = get_log_path()
  local f = io.open(log_path, "r")
  if not f then
    return nil
  end
  local content = f:read("*a")
  f:close()
  if not content or content == "" then
    return nil
  end
  -- Get last N lines
  local lines = {}
  for line in content:gmatch("[^\n]+") do
    table.insert(lines, line)
  end
  local start_idx = math.max(1, #lines - max_lines + 1)
  local result = {}
  for i = start_idx, #lines do
    table.insert(result, lines[i])
  end
  return table.concat(result, "\n")
end

-- Check log for schema/startup errors and return user-friendly message
local function check_startup_error()
  local tail = read_log_tail(10)
  if not tail then
    return nil
  end
  -- Check for schema version error
  if tail:match("newer than this version") or tail:match("schema version") then
    return "Database schema is incompatible.\n\n"
      .. "Your database was created by a newer version of Hemis.\n"
      .. "Please upgrade Hemis or use a different database file.\n\n"
      .. "To use a fresh database:\n"
      .. "  rm ~/.hemis/hemis.db\n\n"
      .. "Check ~/.hemis/hemis.log for details."
  end
  -- Check for other fatal errors
  if tail:match("Error:") or tail:match("FATAL") or tail:match("panic") then
    return "Backend failed to start.\n\nCheck ~/.hemis/hemis.log for details:\n" .. tail
  end
  return nil
end

-- Logging (schedule to avoid "fast event context" errors in libuv callbacks)
local function log(level, msg)
  local levels = { debug = 1, info = 2, warn = 3, error = 4 }
  local cfg_level = levels[config.get("log_level")] or 3
  if levels[level] >= cfg_level then
    vim.schedule(function()
      vim.notify("[hemis] " .. msg, vim.log.levels[level:upper()])
    end)
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

-- Handle incoming data from socket
local function on_data(data)
  if not data then
    return
  end

  M.buffer = M.buffer .. data

  -- Parse all complete responses
  while true do
    local response, remaining = parse_response(M.buffer)
    if not response then
      break
    end
    M.buffer = remaining

    -- Find and call the callback
    -- Must use vim.schedule() because we're in a libuv callback (fast event context)
    -- and the callbacks may call vim APIs that require the main loop
    local id = response.id
    if id and M.pending[id] then
      local callback = M.pending[id]
      M.pending[id] = nil
      vim.schedule(function()
        if response.error then
          callback(response.error, nil)
        else
          callback(nil, response.result)
        end
      end)
    end
  end
end

-- Handle socket close
local function on_close()
  log("info", "Socket closed")
  M.connected = false
  M.socket = nil
  M.buffer = ""

  -- Fail any pending requests (must use vim.schedule for same reason as on_data)
  for id, callback in pairs(M.pending) do
    vim.schedule(function()
      callback({ code = -1, message = "Socket closed" }, nil)
    end)
    M.pending[id] = nil
  end
end

-- Check if a process with given PID is alive
local function process_alive(pid)
  local ok = os.execute("kill -0 " .. pid .. " 2>/dev/null")
  return ok == 0 or ok == true
end

-- Read PID from lock file
local function read_lock_pid()
  local lock_path = get_lock_path()
  local file = io.open(lock_path, "r")
  if not file then
    return nil
  end
  local content = file:read("*l")
  file:close()
  if content then
    return tonumber(content)
  end
  return nil
end

-- Try to acquire lock file (returns true if acquired)
local function try_acquire_lock()
  local hemis_dir = get_hemis_dir()
  -- Ensure directory exists before trying to create lock
  mkdir_p(hemis_dir)
  local lock_path = get_lock_path()
  -- Use O_CREAT | O_EXCL via shell
  local cmd = string.format('set -C; echo %d > "%s" 2>/dev/null', vim.uv.os_getpid(), lock_path)
  local ok = os.execute(cmd)
  return ok == 0 or ok == true
end

-- Remove lock file
local function remove_lock()
  local lock_path = get_lock_path()
  os.remove(lock_path)
end

-- Start the backend server process
local function start_server()
  local backend = config.get("backend")
  if not backend then
    log("error", "No backend configured")
    return false
  end

  if not is_executable(backend) then
    log("error", "Backend not found or not executable: " .. backend)
    return false
  end

  local hemis_dir = get_hemis_dir()

  -- Ensure hemis directory exists
  mkdir_p(hemis_dir)

  -- Build command with optional --config flag
  local config_path = config.get("config_path")
  local config_arg = ""
  if config_path then
    config_arg = " --config " .. shell_escape(config_path)
  end

  -- Start server in background (detached)
  -- Must set HEMIS_DIR so backend uses the same socket path we're expecting
  -- Use env command to ensure the env var is properly set
  local cmd = string.format(
    "env HEMIS_DIR=%s %s --serve%s >> %s/hemis.log 2>&1 &",
    shell_escape(hemis_dir),
    shell_escape(backend),
    config_arg,
    shell_escape(hemis_dir)
  )
  log("debug", "Starting server: " .. cmd)
  os.execute(cmd)

  log("info", "Started backend server")
  return true
end

-- Wait for socket to appear
local function wait_for_socket(timeout_ms, callback)
  local socket_path = get_socket_path()
  local uv = vim.uv or vim.loop
  local start = uv.now()
  local check_interval = 100

  local timer = uv.new_timer()
  local done = false
  local function check()
    if done then
      return
    end
    if file_exists(socket_path) then
      done = true
      timer:stop()
      timer:close()
      callback(true)
      return
    end

    if (uv.now() - start) >= timeout_ms then
      done = true
      timer:stop()
      timer:close()
      callback(false)
      return
    end
  end

  timer:start(0, check_interval, vim.schedule_wrap(check))
end

-- Connect to the socket
local function connect_socket(callback)
  local socket_path = get_socket_path()

  M.socket = vim.uv.new_pipe(false)
  M.socket:connect(socket_path, function(err)
    if err then
      M.socket:close()
      M.socket = nil
      callback(err)
      return
    end

    M.connected = true
    M.buffer = ""

    -- Start reading
    M.socket:read_start(function(read_err, data)
      if read_err then
        log("error", "Socket read error: " .. read_err)
        on_close()
        return
      end
      if data then
        on_data(data)
      else
        -- EOF
        on_close()
      end
    end)

    callback(nil)
  end)
end

-- Ensure we're connected to the backend
function M.ensure_connected(callback)
  if M.connected and M.socket then
    callback(nil)
    return
  end

  if M.connecting then
    -- Already trying to connect, queue this callback
    schedule_after(function()
      M.ensure_connected(callback)
    end, 100)
    return
  end

  M.connecting = true

  local socket_path = get_socket_path()

  -- Check if socket exists
  if file_exists(socket_path) then
    -- Try to connect
    connect_socket(function(err)
      if not err then
        M.connecting = false
        callback(nil)
        return
      end

      -- Connection failed, socket might be stale
      log("debug", "Socket exists but connection failed, checking if stale")
      local pid = read_lock_pid()
      if pid and not process_alive(pid) then
        -- Stale, clean up
        log("info", "Removing stale socket and lock")
        os.remove(socket_path)
        remove_lock()
      end

      -- Need to start server
      M.ensure_connected_start_server(callback)
    end)
    return
  end

  -- Socket doesn't exist, need to start server
  M.ensure_connected_start_server(callback)
end

-- Helper: start server and connect
function M.ensure_connected_start_server(callback)
  -- Try to acquire lock
  if try_acquire_lock() then
    -- We acquired the lock, start the server
    if not start_server() then
      remove_lock()
      M.connecting = false
      callback("Failed to start server")
      return
    end

    -- Wait for socket to appear
    wait_for_socket(5000, function(appeared)
      if not appeared then
        remove_lock()
        M.connecting = false
        -- Check log for startup errors (e.g., schema version mismatch)
        local startup_err = check_startup_error()
        if startup_err then
          vim.schedule(function()
            vim.notify("[hemis] " .. startup_err, vim.log.levels.ERROR)
          end)
          callback("Backend startup failed - check log")
        else
          callback("Server failed to create socket")
        end
        return
      end

      -- Give it a moment
      schedule_after(function()
        connect_socket(function(err)
          M.connecting = false
          callback(err)
        end)
      end, 100)
    end)
  else
    -- Lock exists, someone else is starting the server
    log("debug", "Lock exists, waiting for server")
    wait_for_socket(5000, function(appeared)
      if not appeared then
        M.connecting = false
        callback("Timeout waiting for server")
        return
      end

      schedule_after(function()
        connect_socket(function(err)
          M.connecting = false
          callback(err)
        end)
      end, 100)
    end)
  end
end

-- Start/connect to the backend
function M.start(callback)
  M.ensure_connected(function(err)
    if err then
      log("error", "Failed to connect: " .. tostring(err))
      if callback then
        callback(false)
      end
      return
    end

    -- Check version
    M.request("hemis/version", {}, function(ver_err, result)
      if ver_err then
        log("warn", "Version check failed: " .. vim.inspect(ver_err))
        if callback then
          callback(true) -- Connected, but version unknown
        end
        return
      end

      if result and result.protocolVersion then
        if result.protocolVersion > EXPECTED_PROTOCOL_VERSION then
          log("warn", "Backend is newer than expected. Consider updating the plugin.")
        elseif result.protocolVersion < EXPECTED_PROTOCOL_VERSION then
          vim.notify(
            "[hemis] Backend is outdated. Please restart to update.\n"
              .. "Run: pkill -f 'hemis --serve' && hemis --serve",
            vim.log.levels.WARN
          )
        end
        log("info", string.format("Connected to backend v%d (%s)", result.protocolVersion, result.gitHash or "?"))
      end

      if callback then
        callback(true)
      end
    end)
  end)
end

-- Disconnect from the backend (does NOT shutdown server)
function M.stop()
  if M.socket then
    pcall(function() M.socket:close() end)
    M.socket = nil
  end
  M.connected = false
  M.connecting = false  -- Reset connecting flag
  M.buffer = ""
  M.pending = {}
end

-- Send an async request
function M.request(method, params, callback)
  M.ensure_connected(function(err)
    if err then
      if callback then
        callback({ code = -1, message = "Not connected: " .. tostring(err) }, nil)
      end
      return
    end

    M.request_id = M.request_id + 1
    local id = M.request_id

    if callback then
      M.pending[id] = callback
    end

    local msg = encode_request(method, params, id)
    M.socket:write(msg, function(write_err)
      if write_err then
        log("error", "Socket write error: " .. write_err)
        if M.pending[id] then
          M.pending[id]({ code = -1, message = "Write failed" }, nil)
          M.pending[id] = nil
        end
      end
    end)

    log("debug", "Sent request " .. id .. ": " .. method)
  end)
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

-- Check if connected to backend
function M.is_running()
  return M.connected and M.socket ~= nil
end

-- For backward compatibility with tests that use stdio mode
M.job_id = nil -- Deprecated, kept for compatibility

return M
