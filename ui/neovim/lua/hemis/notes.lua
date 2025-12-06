-- Note operations for Hemis
local rpc = require("hemis.rpc")
-- NOTE: treesitter module no longer needed - server computes anchor position from content
-- NOTE: git info (commit/blob) no longer needed - server auto-computes from file

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

-- Cache for project root per directory (still needed for project-level operations)
local project_root_cache = {}

-- Get project root (git root or cwd) - cached per directory
-- Exported for use by commands.lua (e.g., edit buffers with no file)
function M.get_project_root()
  local file = vim.fn.expand("%:p")
  local dir = vim.fn.fnamemodify(file, ":h")

  -- Check cache first
  if project_root_cache[dir] then
    return project_root_cache[dir]
  end

  local result = vim.fn.systemlist({ "git", "-C", dir, "rev-parse", "--show-toplevel" })
  local root
  if result[1] and not result[1]:match("^fatal") then
    root = result[1]
  else
    root = vim.fn.getcwd()
  end

  -- Cache the result
  project_root_cache[dir] = root
  return root
end

-- Clear all caches (for testing)
function M.clear_all_caches()
  project_root_cache = {}
end

-- Get buffer content as a single string
local function get_buffer_content(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  return table.concat(lines, "\n")
end

-- Build params for current buffer
-- Server auto-computes projectRoot, commit, blob from file when not provided
local function buffer_params(include_content)
  local file = vim.fn.expand("%:p")

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
-- Server computes displayLine and computedStale from content when provided
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
-- opts.project_root can override the auto-detected project root
function M.search(query, opts, callback)
  -- Support old signature: M.search(query, callback)
  if type(opts) == "function" then
    callback = opts
    opts = {}
  end
  opts = opts or {}
  local params = {
    query = query,
    projectRoot = opts.project_root or M.get_project_root(),
  }
  rpc.request("notes/search", params, callback)
end

-- List notes by node path
function M.list_by_node(node_path, callback)
  local params = buffer_params()
  params.nodePath = node_path
  rpc.request("notes/list-by-node", params, callback)
end

-- Index project
-- If include_ai is true, also run AI analysis
function M.index_project(include_ai, callback)
  local root = M.get_project_root()
  local params = { projectRoot = root }
  if include_ai then
    params.includeAI = true
  end
  rpc.request("hemis/index-project", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err and result then
      local msg = string.format("Project indexed: %d files", result.indexed or 0)
      if result.ai then
        if result.ai.analyzed then
          msg = msg .. string.format(", AI analyzed with %s", result.ai.provider or "?")
        elseif result.ai.error then
          msg = msg .. string.format(" (AI failed: %s)", result.ai.error)
        end
      end
      vim.notify(msg, vim.log.levels.INFO)
    end
  end)
end

-- Search files and notes
function M.search_project(query, opts, callback)
  opts = opts or {}
  local params = {
    query = query,
    projectRoot = M.get_project_root(),
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
    projectRoot = M.get_project_root(),
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
function M.project_meta(callback)
  local root = M.get_project_root()
  rpc.request("hemis/project-meta", { projectRoot = root }, callback)
end

-- Reattach a stale note to a new position
-- Server computes anchor position and nodeTextHash from content
function M.reattach(id, opts, callback)
  opts = opts or {}

  -- Use pre-captured cursor position if provided, otherwise capture now
  local anchor = opts.anchor or get_cursor_position()
  local file = vim.fn.expand("%:p")

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
  local file = vim.fn.expand("%:p")
  local content = get_buffer_content()

  rpc.request("notes/buffer-update", {
    file = file,
    content = content,
  }, callback)
end

return M
