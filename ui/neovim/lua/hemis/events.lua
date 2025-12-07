-- Event client for Hemis backend (Unix socket push notifications)
local config = require("hemis.config")

local M = {}

-- State
M.socket = nil
M.buffer = ""
M.connected = false
M.connecting = false
M.stopped = false
M.reconnect_timer = nil

-- Event handlers (set by init.lua)
M.handlers = {}

-- Paths
local function get_hemis_dir()
  local custom = config.get("hemis_dir")
  if custom then
    return vim.fn.expand(custom)
  end
  return vim.fn.expand("~/.hemis")
end

local function get_events_socket_path()
  return get_hemis_dir() .. "/events.sock"
end

-- Logging
local function log(level, msg)
  local levels = { debug = 1, info = 2, warn = 3, error = 4 }
  local cfg_level = levels[config.get("log_level")] or 3
  if levels[level] >= cfg_level then
    vim.schedule(function()
      vim.notify("[hemis-events] " .. msg, vim.log.levels[level:upper()])
    end)
  end
end

-- Parse JSON-lines from buffer
local function parse_events(buf)
  local events = {}
  local remaining = buf

  while true do
    local newline_pos = remaining:find("\n", 1, true)
    if not newline_pos then
      break
    end

    local line = remaining:sub(1, newline_pos - 1)
    remaining = remaining:sub(newline_pos + 1)

    if #line > 0 then
      local ok, event = pcall(vim.json.decode, line)
      if ok and event and event.type then
        table.insert(events, event)
      else
        log("debug", "Failed to parse event: " .. line)
      end
    end
  end

  return events, remaining
end

-- Handle incoming data
local function on_data(data)
  M.buffer = M.buffer .. data

  local events, remaining = parse_events(M.buffer)
  M.buffer = remaining

  for _, event in ipairs(events) do
    log("debug", "Event: " .. event.type)

    -- Call registered handlers
    local handler = M.handlers[event.type]
    if handler then
      vim.schedule(function()
        handler(event)
      end)
    end

    -- Also call catch-all handler if registered
    if M.handlers["*"] then
      vim.schedule(function()
        M.handlers["*"](event)
      end)
    end
  end
end

-- Schedule reconnect
local function schedule_reconnect()
  if M.stopped or M.reconnect_timer then
    return
  end

  local uv = vim.uv or vim.loop
  M.reconnect_timer = uv.new_timer()
  M.reconnect_timer:start(2000, 0, function()
    M.reconnect_timer:stop()
    M.reconnect_timer:close()
    M.reconnect_timer = nil
    M.connect()
  end)
end

-- Connect to events socket
function M.connect()
  if M.stopped or M.connecting or M.connected then
    return
  end

  local socket_path = get_events_socket_path()

  -- Check if socket exists
  if vim.fn.filereadable(socket_path) == 0 then
    schedule_reconnect()
    return
  end

  M.connecting = true
  log("debug", "Connecting to " .. socket_path)

  local uv = vim.uv or vim.loop
  M.socket = uv.new_pipe(false)

  M.socket:connect(socket_path, function(err)
    M.connecting = false

    if err then
      log("debug", "Connect error: " .. err)
      if M.socket then
        M.socket:close()
        M.socket = nil
      end
      schedule_reconnect()
      return
    end

    M.connected = true
    log("debug", "Connected to events socket")

    -- Start reading
    M.socket:read_start(function(read_err, data)
      if read_err then
        log("debug", "Read error: " .. read_err)
        M.disconnect()
        schedule_reconnect()
        return
      end

      if data then
        on_data(data)
      else
        -- EOF
        log("debug", "Events socket closed")
        M.disconnect()
        schedule_reconnect()
      end
    end)
  end)
end

-- Disconnect
function M.disconnect()
  M.connected = false
  M.buffer = ""

  if M.socket then
    if not M.socket:is_closing() then
      M.socket:close()
    end
    M.socket = nil
  end
end

-- Start event client
function M.start()
  M.stopped = false
  M.connect()
end

-- Stop event client
function M.stop()
  M.stopped = true

  if M.reconnect_timer then
    M.reconnect_timer:stop()
    M.reconnect_timer:close()
    M.reconnect_timer = nil
  end

  M.disconnect()
end

-- Register an event handler
-- event_type: "note-created", "note-position-changed", etc. or "*" for all
function M.on(event_type, handler)
  M.handlers[event_type] = handler
end

-- Check if connected
function M.is_connected()
  return M.connected
end

return M
