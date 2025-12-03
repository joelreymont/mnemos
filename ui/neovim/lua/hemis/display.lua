-- Note display module for Hemis
local config = require("hemis.config")
local ts = require("hemis.treesitter")

local M = {}

-- Namespace for extmarks
M.ns_id = vim.api.nvim_create_namespace("hemis_notes")

-- Per-render cache for position hashes (cleared at start of each render_notes call)
-- Key: "bufnr:line:column" -> hash
local position_hash_cache = {}

-- Per-render cache for parsed tree (cleared at start of each render_notes call)
-- Key: bufnr -> { tree = tree, root = root, changedtick = tick }
local tree_cache = {}

-- Highlight groups
local function setup_highlights()
  vim.api.nvim_set_hl(0, "HemisNote", { fg = "#4682B4", italic = true, default = true })
  vim.api.nvim_set_hl(0, "HemisNoteStale", { fg = "#CC6666", italic = true, default = true })
  vim.api.nvim_set_hl(0, "HemisNoteMarker", { fg = "#4682B4", bold = true, default = true })
end

setup_highlights()

-- Get comment prefix for current filetype
local function get_comment_prefix()
  local cs = vim.bo.commentstring
  if cs and cs ~= "" then
    -- Extract comment prefix from commentstring (e.g., "// %s" -> "//")
    local prefix = cs:match("^(.-)%%s") or cs:match("^(.-) ") or cs
    prefix = prefix:gsub("%s+$", "")
    if prefix ~= "" then
      return prefix .. " "
    end
  end

  -- Fallback based on filetype
  local ft = vim.bo.filetype
  local prefixes = {
    lua = "-- ",
    python = "# ",
    rust = "// ",
    go = "// ",
    javascript = "// ",
    typescript = "// ",
    c = "// ",
    cpp = "// ",
    java = "// ",
    sh = "# ",
    bash = "# ",
    zsh = "# ",
    ruby = "# ",
    vim = '" ',
  }
  return prefixes[ft] or "// "
end

-- Format note text as comment lines
local function format_note_lines(note, prefix)
  local lines = {}
  local text = note.text or note.summary or ""
  local hl = note.stale and "HemisNoteStale" or "HemisNote"

  for line in text:gmatch("[^\n]+") do
    -- Skip lines that are only whitespace (prevents extra blank virtual lines)
    if not line:match("^%s*$") then
      table.insert(lines, { { prefix .. line, hl } })
    end
  end

  return lines
end

-- Clear all note extmarks in buffer
function M.clear(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(bufnr, M.ns_id, 0, -1)
end

-- Get the indentation of a line in a buffer
local function get_line_indent(bufnr, line_nr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, line_nr, line_nr + 1, false)
  if #lines == 0 then
    return ""
  end
  local line_text = lines[1]
  local indent = line_text:match("^(%s*)") or ""
  return indent
end

-- Render a single note
function M.render_note(bufnr, note)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  local line = (note.line or 1) - 1 -- Convert to 0-indexed
  if line < 0 then
    line = 0
  end

  local max_lines = vim.api.nvim_buf_line_count(bufnr)
  if line >= max_lines then
    line = max_lines - 1
  end

  -- Get indentation to align note with code
  local indent = get_line_indent(bufnr, line)

  local prefix = get_comment_prefix()
  local virt_lines = format_note_lines(note, indent .. prefix)

  if #virt_lines == 0 then
    return nil
  end

  local style = config.get("display_style") or "full"

  if style == "minimal" then
    -- Single line indicator
    local short_id = (note.id or ""):sub(1, 8)
    local hl = note.stale and "HemisNoteStale" or "HemisNoteMarker"
    return vim.api.nvim_buf_set_extmark(bufnr, M.ns_id, line, 0, {
      virt_text = { { "[n:" .. short_id .. "]", hl } },
      virt_text_pos = "eol",
      hl_mode = "combine",
    })
  else
    -- Full comment block above the line
    return vim.api.nvim_buf_set_extmark(bufnr, M.ns_id, line, 0, {
      virt_lines = virt_lines,
      virt_lines_above = true,
      hl_mode = "combine",
    })
  end
end

-- Node types that are too small/generic to hash uniquely (mirrors treesitter.lua)
local IDENTIFIER_TYPES = {
  identifier = true,
  type_identifier = true,
  field_identifier = true,
  property_identifier = true,
  shorthand_property_identifier = true,
  shorthand_property_identifier_pattern = true,
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
  -- Comments - not code, shouldn't anchor notes
  line_comment = true,
  block_comment = true,
  comment = true,
}

-- Node types that are too large (root-level or container nodes)
local TOO_LARGE_TYPES = {
  source_file = true,
  program = true,
  module = true,
  chunk = true, -- Lua
  -- Container types - too broad, contains multiple items
  impl_item = true,
  trait_item = true,
  mod_item = true,
  declaration_list = true,
  block = true,
}

-- Check if a node type is a significant (hashable) type
-- Must be not too small (identifiers) and not too large (root nodes)
local function is_significant_type(node_type)
  return not IDENTIFIER_TYPES[node_type] and not TOO_LARGE_TYPES[node_type]
end

-- Get or create cached tree for buffer (avoids re-parsing per note)
local function get_cached_tree(bufnr)
  if tree_cache[bufnr] then
    return tree_cache[bufnr].tree, tree_cache[bufnr].root
  end

  local lang = vim.treesitter.language.get_lang(vim.bo[bufnr].filetype)
  if not lang then
    return nil, nil
  end

  local ok, parser = pcall(vim.treesitter.get_parser, bufnr, lang)
  if not ok or not parser then
    return nil, nil
  end

  -- Force re-parse to get current buffer state
  local tree = parser:parse(true)[1]
  if not tree then
    return nil, nil
  end

  local root = tree:root()
  tree_cache[bufnr] = { tree = tree, root = root }
  return tree, root
end

-- Search the tree-sitter tree within a line range for a node with exact matching hash
-- Returns: exact_match_line, closest_significant_line
-- - exact_match_line: line where hash matches exactly (nil if not found)
-- - closest_significant_line: line of significant node closest to target_line (for stale fallback)
-- Single traversal for efficiency
local function find_node_in_range(bufnr, target_hash, min_line, max_line, target_line)
  local tree, root = get_cached_tree(bufnr)
  if not tree or not root then
    return nil, nil
  end

  local exact_match = nil
  local closest_line = nil
  local closest_distance = math.huge

  -- Traverse tree looking for matching node and tracking closest significant node
  local function search_node(node)
    local start_row, _, end_row, _ = node:range()
    local node_start_line = start_row + 1 -- Convert to 1-indexed
    local node_end_line = end_row + 1

    -- Skip subtrees entirely outside the search range
    if node_end_line < min_line or node_start_line > max_line then
      return
    end

    -- Check this node if it's within range and significant
    if node_start_line >= min_line and node_start_line <= max_line and node:named() then
      local node_type = node:type()
      if is_significant_type(node_type) then
        -- Track closest significant node to target_line (for stale fallback)
        -- Prefer nodes AFTER target_line (code usually moves down when lines inserted)
        local distance = math.abs(node_start_line - target_line)
        -- Bias: nodes after target get slight preference (subtract 0.5 from distance)
        if node_start_line > target_line then
          distance = distance - 0.5
        end
        if distance < closest_distance then
          closest_distance = distance
          closest_line = node_start_line
        end

        -- Check for exact hash match
        local text = vim.treesitter.get_node_text(node, bufnr)
        if text then
          local first_line = text:match("^[^\n]+")
          local hash = vim.fn.sha256(first_line or text)
          if hash == target_hash then
            exact_match = node_start_line
            return -- Found exact match, but continue for closest tracking
          end
        end
      end
    end

    -- Recurse into children
    for child in node:iter_children() do
      search_node(child)
      if exact_match then
        return -- Early exit once exact match found
      end
    end
  end

  search_node(root)
  return exact_match, closest_line
end

-- Get hash at position with per-render caching
-- Avoids recomputing sha256 for the same position within a render pass
local function get_cached_hash_at_position(bufnr, line, column)
  local cache_key = bufnr .. ":" .. line .. ":" .. (column or 0)
  if position_hash_cache[cache_key] ~= nil then
    return position_hash_cache[cache_key]
  end
  local hash = ts.get_hash_at_position(bufnr, line, column or 0)
  position_hash_cache[cache_key] = hash or false -- false = no node at position
  return hash
end

-- Debug log file
local function debug_log(msg)
  local f = io.open("/tmp/hemis-debug.log", "a")
  if f then
    f:write(os.date("%H:%M:%S") .. " " .. msg .. "\n")
    f:close()
  end
end

-- Find the display position for a note by searching for its code
-- When code moves (e.g., line inserted above), the note should follow it
-- Returns: display_line, is_stale
--
-- Staleness logic:
-- 1. Exact hash at stored position → stay at stored position (fresh)
-- 2. Exact hash found elsewhere → FOLLOW to new position (fresh) - code moved unchanged
-- 3. No exact hash found → stale (code was modified or deleted)
--
-- Performance: O(1) in common case (hash matches at stored position)
local function find_note_position(bufnr, note)
  -- If note has no stored hash, use stored position and backend's stale flag
  if not note.nodeTextHash then
    debug_log("Note has NO hash, stored line=" .. note.line)
    return note.line, note.stale
  end

  debug_log("Note has hash, stored line=" .. note.line .. ", hash=" .. note.nodeTextHash:sub(1,8))

  -- Debug: show actual buffer content at stored line
  local lines = vim.api.nvim_buf_get_lines(bufnr, note.line - 1, note.line, false)
  debug_log("Buffer line " .. note.line .. ": " .. (lines[1] and lines[1]:sub(1, 50) or "nil"))

  -- Debug: show what node is at stored position
  local node = ts.get_node_at_position(bufnr, note.line, note.column or 0)
  if node then
    debug_log("Node at stored pos: type=" .. node:type() .. ", row=" .. (node:start() + 1))
    local node_text = vim.treesitter.get_node_text(node, bufnr)
    local first_line = node_text:match("^[^\n]+")
    debug_log("Node text: " .. (first_line and first_line:sub(1, 50) or "nil"))
  else
    debug_log("Node at stored pos: nil")
  end

  -- Fast path: check stored position first (O(1) - no tree traversal, cached hash)
  local stored_hash = get_cached_hash_at_position(bufnr, note.line, note.column or 0)
  debug_log("Hash at stored pos: " .. (stored_hash and stored_hash:sub(1,8) or "nil"))

  if stored_hash == note.nodeTextHash then
    debug_log("Hash matches at stored position")
    return note.line, false -- Exact match at stored position
  end

  -- Hash doesn't match at stored position (or no node there)
  -- Search for exact hash elsewhere - note should FOLLOW its code when it moves
  local search_radius = 20
  local line_count = vim.api.nvim_buf_line_count(bufnr)
  local min_line = math.max(1, note.line - search_radius)
  local max_line = math.min(line_count, note.line + search_radius)

  local exact_match_line, closest_line = find_node_in_range(
    bufnr, note.nodeTextHash, min_line, max_line, note.line
  )

  if exact_match_line then
    debug_log("Found hash at line " .. exact_match_line .. " (was " .. note.line .. ")")
    return exact_match_line, false -- Exact hash found elsewhere - follow the code
  end

  -- No exact hash found - code was modified, note is stale
  -- Use closest significant node as display position (code moved then modified)
  if closest_line then
    debug_log("STALE at line " .. closest_line .. " (closest node, was " .. note.line .. ")")
    return closest_line, true
  end

  -- Fallback to stored position if no significant nodes found
  debug_log("STALE at stored line " .. note.line .. " (no nodes found)")
  return note.line, true
end

-- Render all notes for buffer
function M.render_notes(bufnr, notes)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Clear per-render caches (new render = fresh caches)
  position_hash_cache = {}
  tree_cache = {}

  -- Clear existing marks
  M.clear(bufnr)

  if not notes or #notes == 0 then
    return
  end

  -- Compute display position and staleness for each note
  -- Prefer server-computed displayLine/computedStale when available
  -- Fall back to client-side computation for backwards compatibility
  local display_notes = {}
  for _, note in ipairs(notes) do
    local display_line, is_stale
    if note.displayLine then
      -- Server computed position (when content was sent)
      display_line = note.displayLine
      is_stale = note.computedStale or note.stale or false
    else
      -- Client-side fallback (tree-sitter based)
      display_line, is_stale = find_note_position(bufnr, note)
    end
    table.insert(display_notes, {
      note = note,
      display_line = display_line,
      is_stale = is_stale,
    })
  end

  -- Group by display line (not stored line)
  local by_line = {}
  for _, dn in ipairs(display_notes) do
    local line = dn.display_line
    by_line[line] = by_line[line] or {}
    table.insert(by_line[line], dn)
  end

  -- Render grouped notes
  for line, line_dns in pairs(by_line) do
    -- Combine all notes at this display line
    local combined_text = {}
    local is_stale = false

    for _, dn in ipairs(line_dns) do
      local text = dn.note.text or dn.note.summary or ""
      table.insert(combined_text, text)
      if dn.is_stale then
        is_stale = true
      end
    end

    local combined_note = {
      line = line,
      text = table.concat(combined_text, "\n---\n"),
      stale = is_stale,
      id = line_dns[1].note.id,
    }

    M.render_note(bufnr, combined_note)
  end
end

-- Get note at cursor position (if any)
-- Uses display position (where note is rendered) not stored position
function M.get_note_at_cursor(notes, bufnr)
  if not notes or #notes == 0 then
    return nil
  end

  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = cursor[1]

  -- Find note whose display position matches cursor
  -- Prefer server-computed displayLine when available
  for _, note in ipairs(notes) do
    local display_line
    if note.displayLine then
      display_line = note.displayLine
    else
      display_line, _ = find_note_position(bufnr, note)
    end
    if display_line == cursor_line then
      return note
    end
  end

  return nil
end

-- Export find_note_position for use by commands (e.g., reattach)
M.find_note_position = find_note_position

return M
