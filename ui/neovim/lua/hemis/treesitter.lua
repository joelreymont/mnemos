-- Tree-sitter integration for Hemis
local M = {}

-- Check if Tree-sitter is available for current buffer
function M.is_available()
  local buf = vim.api.nvim_get_current_buf()
  local lang = vim.treesitter.language.get_lang(vim.bo[buf].filetype)
  if not lang then
    return false
  end
  -- Try to get parser - this works with both nvim-treesitter and native
  local has_parser = pcall(vim.treesitter.get_parser, buf, lang)
  return has_parser
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

-- Get anchor position: prefer cursor line, use node start only if on same line
function M.get_anchor_position()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = cursor[1]

  local node = M.get_named_node_at_cursor()
  if node then
    local row, col = node:start()
    local node_line = row + 1 -- 1-indexed

    -- Only use node start if it's on the same line as cursor
    -- This prevents anchoring to a parent node that starts many lines above
    if node_line == cursor_line then
      return { line = node_line, column = col }
    end
  end

  -- Use cursor position (more predictable for users)
  return { line = cursor_line, column = 0 }
end

return M
