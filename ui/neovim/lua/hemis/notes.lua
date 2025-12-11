-- Note operations for Hemis
local rpc = require("hemis.rpc")
-- NOTE: treesitter module no longer needed - server computes anchor position from content
-- NOTE: git info (commit/blob) no longer needed - server auto-computes from file
-- NOTE: projectRoot no longer needed - server computes from file via git::find_root()

local M = {}

-- Get raw cursor position (1-indexed line, 0-indexed column)
-- Server handles anchor adjustment when content is provided
local function get_cursor_position()
  local cursor = vim.api.nvim_win_get_cursor(0)
  return {
    line = cursor[1],     -- 1-indexed
    column = cursor[2],   -- 0-indexed
  }
end

-- Get current file path (canonicalized to resolve symlinks on macOS)
local function get_current_file()
  local path = vim.fn.expand("%:p")
  -- Resolve symlinks using realpath (macOS: /var -> /private/var)
  local resolved = vim.fn.resolve(path)
  return resolved
end

-- Get buffer content as a single string
local function get_buffer_content(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  return table.concat(lines, "\n")
end

-- Build params for current buffer
-- Server auto-computes projectRoot, commit, blob from file
local function buffer_params(include_content)
  local file = get_current_file()

  local params = {
    file = file,
    includeStale = true,
  }

  -- Include content for server-side position tracking
  if include_content then
    params.content = get_buffer_content()
  end

  return params
end

-- Create a note at cursor
-- Server computes anchor position, nodeTextHash, and nodePath from content
function M.create(text, opts, callback)
  opts = opts or {}

  -- Use pre-captured cursor position if provided, otherwise capture now
  local anchor = opts.anchor or get_cursor_position()
  -- Send content so server can compute anchor, hash, and nodePath
  local params = buffer_params(true) -- include content

  params.line = anchor.line
  params.column = anchor.column
  params.text = text
  params.tags = opts.tags

  rpc.request("notes/create", params, function(err, result)
    if callback then
      callback(err, result)
    elseif err then
      vim.notify("Failed to create note: " .. (err.message or "unknown error"), vim.log.levels.ERROR)
    end
  end)
end

-- List notes for current buffer
-- Server updates note.line and note.stale with computed values when content is provided
function M.list_for_buffer(callback, include_content)
  -- Default to including content for position tracking
  if include_content == nil then
    include_content = true
  end
  local params = buffer_params(include_content)
  rpc.request("notes/list-for-file", params, callback)
end

-- Get a single note by ID
function M.get(id, callback)
  rpc.request("notes/get", { id = id }, callback)
end

-- Update a note
function M.update(id, text, opts, callback)
  opts = opts or {}
  local params = {
    id = id,
    text = text,
  }
  if opts.tags then
    params.tags = opts.tags
  end

  rpc.request("notes/update", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("Note updated", vim.log.levels.INFO)
    else
      vim.notify("Failed to update note: " .. (err.message or "unknown error"), vim.log.levels.ERROR)
    end
  end)
end

-- Delete a note
function M.delete(id, callback)
  rpc.request("notes/delete", { id = id }, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("Note deleted", vim.log.levels.INFO)
    else
      vim.notify("Failed to delete note: " .. (err.message or "unknown error"), vim.log.levels.ERROR)
    end
  end)
end

-- Search notes
-- Server computes projectRoot from file
function M.search(query, opts, callback)
  -- Support old signature: M.search(query, callback)
  if type(opts) == "function" then
    callback = opts
    opts = {}
  end
  opts = opts or {}
  local params = {
    query = query,
    file = opts.file or get_current_file(), -- Server computes projectRoot from file
  }
  rpc.request("notes/search", params, callback)
end

-- List notes by node path
function M.list_by_node(node_path, callback)
  local params = buffer_params()
  params.nodePath = node_path
  rpc.request("notes/list-by-node", params, callback)
end

-- Index current file only
function M.index_file(callback)
  local file = get_current_file()
  local content = get_buffer_content()
  local params = {
    file = file,
    content = content,
  }
  rpc.request("index/add-file", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("Indexed: " .. vim.fn.fnamemodify(file, ":t"), vim.log.levels.INFO)
    end
  end)
end

-- Index project
-- Server computes projectRoot from file via git::find_root()
-- If include_ai is true, also run AI analysis
function M.index_project(include_ai, callback)
  local params = { file = get_current_file() } -- Server computes projectRoot
  if include_ai then
    params.includeAI = true
  end
  rpc.request("hemis/index-project", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err and result then
      -- Backend guarantees statusMessage
      vim.notify(result.statusMessage, vim.log.levels.INFO)
    end
  end)
end

-- Search files and notes
-- Server computes projectRoot from file
function M.search_project(query, opts, callback)
  opts = opts or {}
  local params = {
    query = query,
    file = get_current_file(), -- Server computes projectRoot
    includeNotes = opts.include_notes ~= false,
  }
  rpc.request("hemis/search", params, callback)
end

-- Get backend status
function M.status(callback)
  rpc.request("hemis/status", {}, callback)
end

-- Get backlinks (notes that link to this note)
function M.backlinks(id, callback)
  rpc.request("notes/backlinks", { id = id }, callback)
end

-- Explain region (for LLM context)
-- Server computes projectRoot from file
-- If use_ai is true, will use AI to generate explanation
-- If detailed is true, will generate a comprehensive explanation
function M.explain_region(file, start_line, end_line, use_ai, detailed, callback)
  local bufnr = vim.fn.bufnr(file)
  local content = nil
  if bufnr ~= -1 then
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    content = table.concat(lines, "\n")
  end
  local params = {
    file = file,
    startLine = start_line,
    endLine = end_line,
    -- Server computes projectRoot from file
  }
  if content then
    params.content = content
  end
  if use_ai then
    params.useAI = true
  end
  if detailed then
    params.detailed = true
  end
  rpc.request("hemis/explain-region", params, callback)
end

-- Get project metadata (indexing and AI analysis status)
-- Server computes projectRoot from file
function M.project_meta(callback)
  rpc.request("hemis/project-meta", { file = get_current_file() }, callback)
end

-- Reattach a stale note to a new position
-- Server computes anchor position and nodeTextHash from content
function M.reattach(id, opts, callback)
  opts = opts or {}

  -- Use pre-captured cursor position if provided, otherwise capture now
  local anchor = opts.anchor or get_cursor_position()
  local file = get_current_file()

  -- Send content so server can compute anchor and hash
  local content = get_buffer_content()

  local params = {
    id = id,
    file = file,
    line = anchor.line,
    column = anchor.column,
    content = content,
  }

  rpc.request("notes/reattach", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("Note reattached", vim.log.levels.INFO)
    else
      vim.notify("Failed to reattach note: " .. (err.message or "unknown error"), vim.log.levels.ERROR)
    end
  end)
end

-- Send buffer update for real-time position tracking
-- Called on TextChanged with debouncing (see commands.lua)
function M.buffer_update(callback)
  local file = get_current_file()
  local content = get_buffer_content()

  rpc.request("notes/buffer-update", {
    file = file,
    content = content,
  }, callback)
end

return M
