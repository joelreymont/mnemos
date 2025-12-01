-- Note operations for Hemis
local rpc = require("hemis.rpc")
local ts = require("hemis.treesitter")

local M = {}

-- Get git info for current file
local function get_git_info(file)
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

  return commit, blob
end

-- Get project root (git root or cwd)
local function get_project_root()
  local file = vim.fn.expand("%:p")
  local dir = vim.fn.fnamemodify(file, ":h")

  local result = vim.fn.systemlist({ "git", "-C", dir, "rev-parse", "--show-toplevel" })
  if result[1] and not result[1]:match("^fatal") then
    return result[1]
  end

  return vim.fn.getcwd()
end

-- Build params for current buffer
local function buffer_params()
  local file = vim.fn.expand("%:p")
  local commit, blob = get_git_info(file)

  return {
    file = file,
    projectRoot = get_project_root(),
    commit = commit,
    blob = blob,
    includeStale = true,
  }
end

-- Create a note at cursor
function M.create(text, opts, callback)
  opts = opts or {}

  local anchor = ts.get_anchor_position()
  local node_path = ts.get_node_path()
  local params = buffer_params()

  params.line = anchor.line
  params.column = anchor.column
  params.text = text
  params.tags = opts.tags
  params.nodePath = node_path

  rpc.request("notes/create", params, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err then
      vim.notify("Note created", vim.log.levels.INFO)
    else
      vim.notify("Failed to create note: " .. (err.message or "unknown error"), vim.log.levels.ERROR)
    end
  end)
end

-- List notes for current buffer
function M.list_for_buffer(callback)
  local params = buffer_params()
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
function M.search(query, callback)
  local params = {
    query = query,
    projectRoot = get_project_root(),
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
function M.index_project(callback)
  local root = get_project_root()
  rpc.request("hemis/index-project", { projectRoot = root }, function(err, result)
    if callback then
      callback(err, result)
    end
    if not err and result then
      vim.notify(string.format("Project indexed: %d files", result.indexed or 0), vim.log.levels.INFO)
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
  rpc.request("hemis/list-files", { projectRoot = root }, callback)
end

-- Get file content
function M.get_file(file, callback)
  rpc.request("hemis/get-file", { file = file }, callback)
end

-- Explain region (for LLM context)
function M.explain_region(file, start_line, end_line, callback)
  rpc.request("hemis/explain-region", {
    file = file,
    startLine = start_line,
    endLine = end_line,
  }, callback)
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

return M
