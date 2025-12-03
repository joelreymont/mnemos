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
-- Uses first non-whitespace column to find actual code, not parent scope
function M.get_node_at_cursor()
  if not M.is_available() then
    return nil
  end

  local buf = vim.api.nvim_get_current_buf()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local row = cursor[1] - 1 -- 0-indexed
  local col = cursor[2]

  -- If cursor is at column 0 or in leading whitespace, find first non-whitespace
  -- This ensures we anchor to actual code, not parent scope containing whitespace
  local lines = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)
  if #lines > 0 then
    local first_nonws = lines[1]:find("%S")
    if first_nonws and col < first_nonws - 1 then
      col = first_nonws - 1 -- Use first non-whitespace column
    end
  end

  -- Use explicit buffer and position for reliability
  local ok, node = pcall(vim.treesitter.get_node, { bufnr = buf, pos = { row, col } })
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

-- Node types that are too small/generic to hash uniquely
-- These are identifiers and small fragments that likely appear multiple times
local IDENTIFIER_TYPES = {
  identifier = true,
  type_identifier = true,
  field_identifier = true,
  property_identifier = true,
  shorthand_property_identifier = true,
  shorthand_property_identifier_pattern = true,
  -- Common in various languages
  name = true,
  variable_name = true,
  simple_identifier = true,
  -- Parameters and arguments - too granular, want function-level
  parameter = true,
  parameters = true,
  formal_parameters = true,
  argument = true,
  arguments = true,
  -- Type annotations - too granular
  type_annotation = true,
  return_type = true,
  primitive_type = true,
  generic_type = true,
  scoped_type_identifier = true,
  -- Rust-specific small nodes
  self_parameter = true,
  visibility_modifier = true,
}

-- Node types that are too large to be useful for tracking
-- (changes to unrelated code would mark notes as stale)
local TOO_LARGE_TYPES = {
  source_file = true,
  program = true,
  module = true,
  chunk = true, -- Lua
}

-- Walk up from a node to find a semantically meaningful parent
-- Returns the first ancestor that is neither too small (identifier) nor too large (file)
local function find_significant_node(node)
  if not node then
    return nil
  end

  local current = node
  local original_line = node:start()

  while current do
    local node_type = current:type()
    local start_row = current:start()

    -- Stop if we've gone too far up (different line)
    if start_row ~= original_line then
      -- Return the last node that was on our line
      return node
    end

    -- If this node type is too large, return previous node
    if TOO_LARGE_TYPES[node_type] then
      return node
    end

    -- If this is not an identifier type, it's significant
    if not IDENTIFIER_TYPES[node_type] then
      return current
    end

    -- Keep track of last valid node and continue up
    node = current
    current = current:parent()
  end

  -- If we reach root without finding significant node, return original
  return node
end

-- Get the source text of the significant node at cursor
-- For multi-line nodes, returns only the first line to avoid false staleness
-- when content inside the node changes (e.g., adding code inside a function)
function M.get_node_text()
  local node = M.get_named_node_at_cursor()
  if not node then
    return nil
  end

  -- Walk up to find a significant node (matches get_node_at_position behavior)
  node = find_significant_node(node)
  if not node then
    return nil
  end

  local buf = vim.api.nvim_get_current_buf()
  local text = vim.treesitter.get_node_text(node, buf)

  -- For multi-line nodes, use only the first line as the hash basis
  -- This prevents false staleness when content inside the node changes
  local first_line = text:match("^[^\n]+")
  return first_line or text
end

-- Compute SHA256 hash of significant node's source text
-- This hash is used for note position tracking and stale detection
function M.get_node_text_hash()
  local text = M.get_node_text()
  if not text then
    return nil
  end

  -- Use vim.fn.sha256 to compute hash
  return vim.fn.sha256(text)
end

-- Get node at specific line/column position
-- Returns a semantically meaningful node (not just an identifier)
-- If column is 0 or in whitespace, finds first non-whitespace column on the line
-- to avoid anchoring to parent nodes that contain the whitespace
function M.get_node_at_position(buf, line, column)
  buf = buf or vim.api.nvim_get_current_buf()
  local lang = vim.treesitter.language.get_lang(vim.bo[buf].filetype)
  if not lang then
    return nil
  end

  local ok, parser = pcall(vim.treesitter.get_parser, buf, lang)
  if not ok or not parser then
    return nil
  end

  -- Force re-parse to get current buffer state (not cached stale tree)
  local tree = parser:parse(true)[1]
  if not tree then
    return nil
  end

  -- Tree-sitter uses 0-indexed line/column
  local row = line - 1
  local col = column or 0

  -- If column is 0 or not specified, find first non-whitespace column
  -- This prevents anchoring to parent nodes that contain leading whitespace
  if col == 0 then
    local lines = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)
    if #lines > 0 then
      local first_nonws = lines[1]:find("%S")
      if first_nonws then
        col = first_nonws - 1 -- Convert to 0-indexed
      end
    end
  end

  local node = tree:root():named_descendant_for_range(row, col, row, col)

  -- Walk up to find a significant node (not just an identifier)
  return find_significant_node(node)
end

-- Get text of node at specific position
-- For multi-line nodes, returns only the first line (matches get_node_text behavior)
function M.get_text_at_position(buf, line, column)
  local node = M.get_node_at_position(buf, line, column)
  if not node then
    return nil
  end

  buf = buf or vim.api.nvim_get_current_buf()
  local text = vim.treesitter.get_node_text(node, buf)

  -- Use only first line for multi-line nodes
  local first_line = text:match("^[^\n]+")
  return first_line or text
end

-- Compute hash of node at specific position
function M.get_hash_at_position(buf, line, column)
  local text = M.get_text_at_position(buf, line, column)
  if not text then
    return nil
  end
  return vim.fn.sha256(text)
end

return M
