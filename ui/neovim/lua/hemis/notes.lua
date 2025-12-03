-- Note operations for Hemis
local rpc = require("hemis.rpc")
local ts = require("hemis.treesitter")

local M = {}

-- Cache for git info per buffer (cleared on buffer write)
local git_cache = {}

-- Get git info for file (cached per buffer)
local function get_git_info(file, bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Check cache first
  if git_cache[bufnr] and git_cache[bufnr].file == file then
    return git_cache[bufnr].commit, git_cache[bufnr].blob
  end

  local dir = vim.fn.fnamemodify(file, ":h")

  -- Get commit SHA
  local commit_result = vim.fn.systemlist({ "git", "-C", dir, "rev-parse", "HEAD" })
  local commit = (commit_result[1] and not commit_result[1]:match("^fatal")) and commit_result[1] or nil

  -- Get blob SHA
  local blob_result = vim.fn.systemlist({ "git", "-C", dir, "ls-files", "-s", file })
  local blob = nil
  if blob_result[1] then
    blob = blob_result[1]:match("^%d+%s+(%x+)")
  end

  -- Cache the result
  git_cache[bufnr] = { file = file, commit = commit, blob = blob }

  return commit, blob
end

-- Cache for project root per directory
local project_root_cache = {}

-- Get project root (git root or cwd) - cached per directory
local function get_project_root()
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

-- Clear git cache for a buffer (call on BufWritePost)
function M.clear_git_cache(bufnr)
  git_cache[bufnr] = nil
end

-- Clear all caches (for testing)
function M.clear_all_caches()
  git_cache = {}
  project_root_cache = {}
end

-- Get buffer content as a single string
local function get_buffer_content(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  return table.concat(lines, "\n")
end

-- Build params for current buffer
local function buffer_params(include_content)
  local file = vim.fn.expand("%:p")
  local commit, blob = get_git_info(file)

  local params = {
    file = file,
    projectRoot = get_project_root(),
    commit = commit,
    blob = blob,
    includeStale = true,
  }

  -- Include content for server-side position tracking
  if include_content then
    params.content = get_buffer_content()
  end

  return params
end

-- Create a note at cursor
-- Server computes nodeTextHash and nodePath from content when provided
function M.create(text, opts, callback)
  opts = opts or {}

  -- Use pre-captured position if provided, otherwise capture now
  local anchor = opts.anchor or ts.get_anchor_position()
  -- Send content so server can compute hash/nodePath (fallback to UI-computed if no content)
  local params = buffer_params(true) -- include content

  params.line = anchor.line
  params.column = anchor.column
  params.text = text
  params.tags = opts.tags

  -- Only send UI-computed values as fallback (server prefers to compute from content)
  if not params.content then
    params.nodePath = opts.node_path or ts.get_node_path()
    params.nodeTextHash = opts.node_text_hash or ts.get_node_text_hash()
  end

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
    projectRoot = opts.project_root or get_project_root(),
  }
  rpc.request("notes/search", params, callback)
end

-- List notes by node path
function M.list_by_node(node_path, callback)
  local params = buffer_params()
  params.nodePath = node_path
  rpc.request("notes/list-by-node", params, callback)
end

-- Index current file
function M.index_file(callback)
  local file = vim.fn.expand("%:p")
  local content = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")

  rpc.request("index/add-file", {
    file = file,
    projectRoot = get_project_root(),
    content = content,
  }, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("File indexed", vim.log.levels.INFO)
    end
  end)
end

-- Index project
-- If include_ai is true, also run AI analysis
function M.index_project(include_ai, callback)
  local root = get_project_root()
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
    projectRoot = get_project_root(),
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

-- List project files
function M.list_files(callback)
  local root = get_project_root()
  rpc.request("hemis/list-files", { projectRoot = root }, function(err, result)
    callback(err, result, root)
  end)
end

-- Get file content
function M.get_file(file, callback)
  rpc.request("hemis/get-file", { file = file }, callback)
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
    projectRoot = get_project_root(),
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
  local root = get_project_root()
  rpc.request("hemis/project-meta", { projectRoot = root }, callback)
end

-- Save snapshot
function M.save_snapshot(path, callback)
  local root = get_project_root()
  rpc.request("hemis/save-snapshot", {
    path = path,
    projectRoot = root,
  }, callback)
end

-- Load snapshot
function M.load_snapshot(path, callback)
  rpc.request("hemis/load-snapshot", { path = path }, callback)
end

-- Reattach a stale note to a new position
function M.reattach(id, opts, callback)
  opts = opts or {}

  -- Use pre-captured position if provided, otherwise capture now
  local ts = require("hemis.treesitter")
  local anchor = opts.anchor or ts.get_anchor_position()
  local file = vim.fn.expand("%:p")

  -- Send content so server can compute hash (fallback to UI-computed if needed)
  local content = get_buffer_content()

  local params = {
    id = id,
    file = file,
    line = anchor.line,
    column = anchor.column,
    content = content,
  }

  -- Only send UI-computed values as fallback
  if not content then
    params.nodePath = opts.node_path or ts.get_node_path()
    params.nodeTextHash = opts.node_text_hash or ts.get_node_text_hash()
  end

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
