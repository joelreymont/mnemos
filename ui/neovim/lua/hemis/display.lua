-- Note display module for Hemis
--
-- Note positions are computed server-side when content is provided.
-- This module renders notes at server-computed displayLine positions.
-- Server provides formattedLines with comment prefix and text wrapping.
local config = require("hemis.config")

local M = {}

-- Namespace for extmarks
M.ns_id = vim.api.nvim_create_namespace("hemis_notes")

-- Highlight groups
local function setup_highlights()
  vim.api.nvim_set_hl(0, "HemisNote", { fg = "#4682B4", italic = true, default = true })
  vim.api.nvim_set_hl(0, "HemisNoteStale", { fg = "#808080", italic = true, default = true })
  vim.api.nvim_set_hl(0, "HemisNoteMarker", { fg = "#4682B4", bold = true, default = true })
end

setup_highlights()

-- Format note text as virtual lines for display
-- Server MUST provide formattedLines (includes comment prefix and wrapping)
local function format_note_lines(note)
  local lines = {}
  local hl = note.stale and "HemisNoteStale" or "HemisNote"

  if not note.formattedLines or #note.formattedLines == 0 then
    -- Server bug: should always provide formattedLines when content is sent
    vim.notify("[hemis] Note missing formattedLines - check server version", vim.log.levels.WARN)
    return { { { "// " .. (note.text or ""):sub(1, 50), hl } } }
  end

  for _, formatted_line in ipairs(note.formattedLines) do
    table.insert(lines, { { formatted_line, hl } })
  end
  return lines
end

-- Clear all note extmarks in buffer
function M.clear(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(bufnr, M.ns_id, 0, -1)
end

-- Render a single note at its display position
function M.render_note(bufnr, note, display_line, is_stale)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  local line = (display_line or note.line or 1) - 1 -- Convert to 0-indexed
  if line < 0 then
    line = 0
  end

  local max_lines = vim.api.nvim_buf_line_count(bufnr)
  if line >= max_lines then
    line = max_lines - 1
  end

  -- Override staleness if provided
  local note_with_stale = note
  if is_stale ~= nil then
    note_with_stale = vim.tbl_extend("force", note, { stale = is_stale })
  end

  local virt_lines = format_note_lines(note_with_stale)

  if #virt_lines == 0 then
    return nil
  end

  local style = config.get("display_style") or "full"

  if style == "minimal" then
    -- Single line indicator at end of line
    local short_id = (note.id or ""):sub(1, 8)
    local hl = note_with_stale.stale and "HemisNoteStale" or "HemisNoteMarker"
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

-- Get display position for a note
-- Server computes position and staleness when content is provided,
-- updating the note's line and stale fields directly
local function get_note_display_position(note)
  return note.line, note.stale or false
end

-- Render all notes for buffer
function M.render_notes(bufnr, notes)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Clear existing marks
  M.clear(bufnr)

  if not notes or #notes == 0 then
    return
  end

  -- Render each note at its server-computed display position
  for _, note in ipairs(notes) do
    local display_line, is_stale = get_note_display_position(note)
    M.render_note(bufnr, note, display_line, is_stale)
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
    local display_line = get_note_display_position(note)
    if display_line == cursor_line then
      return note
    end
  end

  return nil
end

return M
