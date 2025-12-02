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

-- Check if a note is stale using tree-sitter based comparison
-- Returns true if the node's text hash has changed since the note was created
local function is_note_stale(bufnr, note)
  -- If note has no stored hash, fall back to backend's stale flag
  if not note.nodeTextHash then
    return note.stale
  end

  -- Compute current hash at the note's position
  local current_hash = ts.get_hash_at_position(bufnr, note.line, note.column)

  -- If we can't get current hash (no parser, invalid position), use backend's flag
  if not current_hash then
    return note.stale
  end

  -- Compare stored hash with current hash
  return note.nodeTextHash ~= current_hash
end

-- Render all notes for buffer
function M.render_notes(bufnr, notes)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Clear existing marks
  M.clear(bufnr)

  if not notes or #notes == 0 then
    return
  end

  -- Group notes by line
  local by_line = {}
  for _, note in ipairs(notes) do
    local line = note.line or 1
    by_line[line] = by_line[line] or {}
    table.insert(by_line[line], note)
  end

  -- Render grouped notes
  for line, line_notes in pairs(by_line) do
    -- Combine all notes at this line
    local combined_text = {}
    local is_stale = false

    for _, note in ipairs(line_notes) do
      local text = note.text or note.summary or ""
      table.insert(combined_text, text)
      -- Use tree-sitter based staleness check
      if is_note_stale(bufnr, note) then
        is_stale = true
      end
    end

    local combined_note = {
      line = line,
      text = table.concat(combined_text, "\n---\n"),
      stale = is_stale,
      id = line_notes[1].id,
    }

    M.render_note(bufnr, combined_note)
  end
end

-- Get note at cursor position (if any)
function M.get_note_at_cursor(notes)
  if not notes or #notes == 0 then
    return nil
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = cursor[1]

  -- Find note closest to cursor line
  for _, note in ipairs(notes) do
    if note.line == cursor_line then
      return note
    end
  end

  return nil
end

return M
