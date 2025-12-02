-- Note display module for Hemis
local config = require("hemis.config")
local ts = require("hemis.treesitter")

local M = {}

-- Namespace for extmarks
M.ns_id = vim.api.nvim_create_namespace("hemis_notes")

-- Highlight groups
local function setup_highlights()
  vim.api.nvim_set_hl(0, "HemisNote", { fg = "#4682B4", italic = true, default = true })
  vim.api.nvim_set_hl(0, "HemisNoteStale", { fg = "#B47846", italic = true, default = true })
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
}

-- Node types that are too large (root-level nodes that always exist)
local TOO_LARGE_TYPES = {
  source_file = true,
  program = true,
  module = true,
  chunk = true, -- Lua
}

-- Check if a node type is a significant (hashable) type
-- Must be not too small (identifiers) and not too large (root nodes)
local function is_significant_type(node_type)
  return not IDENTIFIER_TYPES[node_type] and not TOO_LARGE_TYPES[node_type]
end

-- Search the tree-sitter tree within a line range for:
-- 1. A node with exact matching hash (returns its line)
-- 2. Any significant node (tracks existence)
--
-- Returns: exact_match_line (or nil), any_node_exists (boolean)
-- Single traversal for efficiency - only called when node not at stored position
local function find_node_in_range(bufnr, target_hash, min_line, max_line)
  local lang = vim.treesitter.language.get_lang(vim.bo[bufnr].filetype)
  if not lang then
    return nil, false
  end

  local ok, parser = pcall(vim.treesitter.get_parser, bufnr, lang)
  if not ok or not parser then
    return nil, false
  end

  local tree = parser:parse()[1]
  if not tree then
    return nil, false
  end

  local any_node_found = false

  -- Traverse tree looking for matching node
  -- Track any significant node, return immediately on exact hash match
  local function search_node(node)
    local start_row = node:start()
    local line = start_row + 1 -- Convert to 1-indexed

    -- Only check significant named nodes within the search range
    if line >= min_line and line <= max_line and node:named() then
      local node_type = node:type()
      if is_significant_type(node_type) then
        any_node_found = true -- Track that we found at least one node

        local text = vim.treesitter.get_node_text(node, bufnr)
        if text then
          local hash = vim.fn.sha256(text)
          if hash == target_hash then
            return line -- Exact match - return immediately
          end
        end
      end
    end

    -- Recurse into children
    for child in node:iter_children() do
      local result = search_node(child)
      if result then
        return result -- Propagate exact match
      end
    end

    return nil
  end

  local exact_match = search_node(tree:root())
  return exact_match, any_node_found
end

-- Find the display position for a note by searching for its code
-- When code moves (e.g., line inserted above), the note should follow it
-- Returns: display_line, is_stale
--
-- Staleness logic (optimized for efficiency):
-- 1. Exact hash at stored position → stay at stored position (fresh)
-- 2. Exact hash found elsewhere → FOLLOW to new position (fresh) - code moved unchanged
-- 3. No exact hash, but node at stored position → stay (fresh) - code modified in place
-- 4. No exact hash, no node at stored, but nodes in range → stay (fresh)
-- 5. No nodes in range → stale (code was deleted)
--
-- Performance: O(1) in common case (hash matches at stored position)
local function find_note_position(bufnr, note)
  -- If note has no stored hash, use stored position and backend's stale flag
  if not note.nodeTextHash then
    return note.line, note.stale
  end

  -- Fast path: check stored position first (O(1) - no tree traversal)
  local stored_hash = ts.get_hash_at_position(bufnr, note.line, note.column or 0)
  if stored_hash == note.nodeTextHash then
    return note.line, false -- Exact match at stored position
  end

  -- Hash doesn't match at stored position (or no node there)
  -- Search for exact hash elsewhere - note should FOLLOW its code when it moves
  local search_radius = 20
  local line_count = vim.api.nvim_buf_line_count(bufnr)
  local min_line = math.max(1, note.line - search_radius)
  local max_line = math.min(line_count, note.line + search_radius)

  local exact_match_line, any_node_exists = find_node_in_range(bufnr, note.nodeTextHash, min_line, max_line)

  if exact_match_line then
    return exact_match_line, false -- Exact hash found elsewhere - follow the code
  end

  -- No exact hash found anywhere
  -- If there's still a node at stored position, stay there (code was modified)
  if stored_hash then
    return note.line, false -- Node exists but was modified, stay fresh
  end

  -- No node at stored position either
  -- But if there are any nodes in range, stay fresh (code exists nearby)
  if any_node_exists then
    return note.line, false -- Code exists nearby, stays fresh at stored position
  end

  return note.line, true -- No code in range, truly stale
end

-- Render all notes for buffer
function M.render_notes(bufnr, notes)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Clear existing marks
  M.clear(bufnr)

  if not notes or #notes == 0 then
    return
  end

  -- Compute display position and staleness for each note
  -- Notes follow their code when it moves (hash-based tracking)
  local display_notes = {}
  for _, note in ipairs(notes) do
    local display_line, is_stale = find_note_position(bufnr, note)
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
  for _, note in ipairs(notes) do
    local display_line, _ = find_note_position(bufnr, note)
    if display_line == cursor_line then
      return note
    end
  end

  return nil
end

-- Export find_note_position for use by commands (e.g., reattach)
M.find_note_position = find_note_position

return M
