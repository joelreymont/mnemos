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
  vim.api.nvim_set_hl(0, "HemisNoteSelected", { fg = "#FFD700", italic = true, bold = true, default = true })
end

setup_highlights()

-- Format note text as virtual lines for display
-- Server provides formattedLines and iconHint
-- is_selected: whether this note is currently selected
local function format_note_lines(note, is_selected)
  local lines = {}

  -- Determine highlight: selected > stale > normal
  local hl
  if is_selected then
    hl = "HemisNoteSelected"
  elseif (note.iconHint or note.icon_hint) == "stale" or note.stale then
    hl = "HemisNoteStale"
  else
    hl = "HemisNote"
  end

  -- Backend returns formattedLines (camelCase)
  local formatted = note.formattedLines or note.formatted_lines
  if not formatted then
    -- Fallback: wrap note text manually if backend didn't provide formattedLines
    local text = note.text or ""
    formatted = { "// " .. text:sub(1, 80) }
  end

  for _, formatted_line in ipairs(formatted) do
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
-- is_selected: whether this note is currently selected (for highlight)
function M.render_note(bufnr, note, display_line, is_stale, is_selected)
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

  local virt_lines = format_note_lines(note_with_stale, is_selected)

  if #virt_lines == 0 then
    return nil
  end

  local style = config.get("display_style") or "full"

  if style == "minimal" then
    -- Single line indicator at end of line
    -- Backend uses camelCase: displayMarker
    local marker = note.displayMarker or note.display_marker or "[note]"
    local hl
    if is_selected then
      hl = "HemisNoteSelected"
    elseif (note_with_stale.iconHint or note_with_stale.icon_hint) == "stale" or note_with_stale.stale then
      hl = "HemisNoteStale"
    else
      hl = "HemisNoteMarker"
    end
    return vim.api.nvim_buf_set_extmark(bufnr, M.ns_id, line, 0, {
      virt_text = { { marker, hl } },
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
-- Server computes position and staleness when content is provided.
-- Uses displayLine if server computed it, otherwise falls back to stored line.
-- Uses computedStale if server computed it, otherwise falls back to stored stale.
local function get_note_display_position(note)
  -- Server-computed displayLine takes precedence over stored line
  local display_line = note.displayLine or note.display_line or note.line
  -- Server-computed staleness takes precedence over stored stale flag
  local is_stale
  if note.computedStale ~= nil then
    is_stale = note.computedStale
  elseif note.computed_stale ~= nil then
    is_stale = note.computed_stale
  else
    -- Check iconHint for stale indication, then fall back to stale flag
    is_stale = (note.iconHint or note.icon_hint) == "stale" or note.stale or false
  end
  return display_line, is_stale
end

-- Render all notes for buffer
-- selected_note_id: ID of the currently selected note (for highlight)
function M.render_notes(bufnr, notes, selected_note_id)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  -- Clear existing marks
  M.clear(bufnr)

  if not notes or #notes == 0 then
    return
  end

  -- Render each note at its server-computed display position
  for _, note in ipairs(notes) do
    local display_line, is_stale = get_note_display_position(note)
    local is_selected = selected_note_id ~= nil and note.id == selected_note_id
    M.render_note(bufnr, note, display_line, is_stale, is_selected)
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

-- Get all notes at cursor position (for multi-note disambiguation)
function M.get_notes_at_cursor(notes, bufnr)
  if not notes or #notes == 0 then
    return {}
  end

  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = cursor[1]

  -- Find all notes whose display position matches cursor
  local result = {}
  for _, note in ipairs(notes) do
    local display_line = get_note_display_position(note)
    if display_line == cursor_line then
      table.insert(result, note)
    end
  end

  return result
end

-- Get note at cursor with picker if multiple notes on same line
function M.get_note_at_cursor_with_picker(notes, bufnr, callback)
  local notes_at_cursor = M.get_notes_at_cursor(notes, bufnr)

  if #notes_at_cursor == 0 then
    callback(nil)
    return
  end

  if #notes_at_cursor == 1 then
    callback(notes_at_cursor[1])
    return
  end

  -- Multiple notes - show picker
  -- Backend guarantees display_label is always present
  local items = {}
  for _, note in ipairs(notes_at_cursor) do
    table.insert(items, note.display_label)
  end

  vim.ui.select(items, { prompt = "Multiple notes on this line:" }, function(_, idx)
    if idx then
      callback(notes_at_cursor[idx])
    else
      callback(nil)
    end
  end)
end

-- Note cache for incremental updates: bufnr -> (id -> note)
M.note_cache = {}

-- Cache notes for a buffer (called during refresh)
function M.cache_notes(bufnr, notes)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local cache = {}
  for _, note in ipairs(notes or {}) do
    cache[note.id] = note
  end
  M.note_cache[bufnr] = cache
end

-- Clear cache for a buffer
function M.clear_cache(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  M.note_cache[bufnr] = nil
end

-- Update a single note's position (called from event handler)
-- selected_note_id: ID of currently selected note for highlight
function M.update_note_position(bufnr, note_id, new_line, stale, selected_note_id)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  local cache = M.note_cache[bufnr]
  if not cache then
    -- No cache, skip (will be populated on next refresh)
    return
  end

  local note = cache[note_id]
  if not note then
    -- Note not in cache, skip
    return
  end

  -- Update note in cache
  note.line = new_line
  note.stale = stale

  -- Re-render all notes for this buffer
  local notes = {}
  for _, n in pairs(cache) do
    table.insert(notes, n)
  end
  M.render_notes(bufnr, notes, selected_note_id)
end

return M
