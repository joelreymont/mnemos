-- Tree-sitter integration for Hemis
local M = {}

-- Check if Tree-sitter is available for current buffer
function M.is_available()
  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if not ok then
    -- Try native treesitter
    local buf = vim.api.nvim_get_current_buf()
    local lang = vim.treesitter.language.get_lang(vim.bo[buf].filetype)
    if lang then
      local has_parser = pcall(vim.treesitter.get_parser, buf, lang)
      return has_parser
    end
    return false
  end
  return parsers.has_parser()
end

-- Get the Tree-sitter node at cursor
function M.get_node_at_cursor()
  if not M.is_available() then
    return nil
  end

  local ok, node = pcall(vim.treesitter.get_node)
  if ok and node then
    return node
  end
  return nil
end

-- Build node path from root to current node
function M.get_node_path(max_depth)
  max_depth = max_depth or 10

  local node = M.get_node_at_cursor()
  if not node then
    return nil
  end

  local path = {}
  local current = node
  local depth = 0

  while current and depth < max_depth do
    table.insert(path, 1, current:type())
    current = current:parent()
    depth = depth + 1
  end

  return path
end

-- Get the start position of the current node
function M.get_node_start()
  local node = M.get_node_at_cursor()
  if not node then
    return nil
  end

  local row, col = node:start()
  return { line = row + 1, column = col } -- 1-indexed line
end

-- Find the named node containing the cursor (skip anonymous nodes)
function M.get_named_node_at_cursor()
  local node = M.get_node_at_cursor()
  if not node then
    return nil
  end

  -- Walk up to find a named node
  while node and not node:named() do
    node = node:parent()
  end

  return node
end

-- Get anchor position: start of the containing named node
function M.get_anchor_position()
  local node = M.get_named_node_at_cursor()
  if node then
    local row, col = node:start()
    return { line = row + 1, column = col }
  end

  -- Fallback: current line start
  local cursor = vim.api.nvim_win_get_cursor(0)
  return { line = cursor[1], column = 0 }
end

return M
