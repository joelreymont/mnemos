-- Display tests for Hemis Neovim plugin
-- Tests verify that what user sees matches expected state

local helpers = require("tests.helpers")

describe("hemis display", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  describe("render_notes", function()
    it("creates extmark at correct line for single note", function()
      local display = require("hemis.display")
      local notes = {
        { id = "1", line = 1, column = 0, text = "Test note content" },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      assert.equals(1, state.extmarks[1].line)
    end)

    it("shows note text in virtual lines", function()
      local display = require("hemis.display")
      local notes = {
        { id = "1", line = 2, column = 0, text = "Important: check this" },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      helpers.assert_extmark_at_line(state, 2, "Important")
    end)

    it("renders multiple notes at different lines", function()
      local display = require("hemis.display")
      local notes = {
        { id = "1", line = 1, column = 0, text = "First note" },
        { id = "2", line = 3, column = 0, text = "Second note" },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(2, #state.extmarks)
    end)

    it("marks stale notes with HemisNoteStale highlight", function()
      local display = require("hemis.display")
      local notes = {
        { id = "1", line = 1, column = 0, text = "Stale note", iconHint = "stale" },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      local mark = state.extmarks[1]
      -- Stale notes use HemisNoteStale highlight group
      local has_stale_hl = false
      for _, hl in ipairs(mark.hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale_hl = true
        end
      end
      assert.truthy(has_stale_hl, "Should use HemisNoteStale highlight")
    end)

    it("clears previous notes before rendering new ones", function()
      local display = require("hemis.display")

      -- Render first set
      display.render_notes(buf, {
        { id = "1", line = 1, column = 0, text = "Old note" },
      })
      helpers.wait()

      -- Render second set (should replace, not add)
      display.render_notes(buf, {
        { id = "2", line = 2, column = 0, text = "New note" },
      })
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      assert.equals(2, state.extmarks[1].line)
    end)
  end)

  describe("get_note_at_cursor", function()
    it("returns note when cursor is on note line", function()
      local display = require("hemis.display")
      local notes = {
        { id = "note-123", line = 2, column = 0, text = "Target note" },
      }

      -- Set cursor to line 2
      vim.api.nvim_win_set_cursor(0, { 2, 0 })

      local note = display.get_note_at_cursor(notes)
      assert.is_not_nil(note)
      assert.equals("note-123", note.id)
    end)

    it("returns nil when cursor is not on note line", function()
      local display = require("hemis.display")
      local notes = {
        { id = "note-123", line = 2, column = 0, text = "Target note" },
      }

      -- Set cursor to line 1 (no note)
      vim.api.nvim_win_set_cursor(0, { 1, 0 })

      local note = display.get_note_at_cursor(notes)
      assert.is_nil(note)
    end)
  end)
end)

describe("hemis display virt_lines details", function()
  -- Tests verify the exact virt_lines payload structure users see

  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
    vim.bo[buf].filetype = "rust"
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("virt_lines has comment prefix from commentstring", function()
    local display = require("hemis.display")
    vim.bo[buf].commentstring = "// %s"
    local notes = {
      { id = "1", line = 1, column = 0, text = "Test note" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(1, #state.extmarks)
    local mark = state.extmarks[1]
    -- Check virt_lines contains comment prefix
    assert.truthy(#mark.virt_lines > 0)
    local first_line = mark.virt_lines[1][1]
    assert.truthy(string.find(first_line.text, "^// "), "Should have // prefix")
  end)

  it("virt_lines uses HemisNote highlight for fresh notes", function()
    local display = require("hemis.display")
    local notes = {
      { id = "1", line = 1, column = 0, text = "Fresh note", stale = false },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Check highlight group
    local has_hemis_note = false
    for _, hl in ipairs(mark.hl_groups) do
      if hl == "HemisNote" then
        has_hemis_note = true
      end
    end
    assert.truthy(has_hemis_note, "Should use HemisNote highlight")
  end)

  it("virt_lines uses HemisNoteStale highlight for stale notes", function()
    local display = require("hemis.display")
    local notes = {
      { id = "1", line = 1, column = 0, text = "Stale note", iconHint = "stale" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Check highlight group
    local has_stale = false
    for _, hl in ipairs(mark.hl_groups) do
      if hl == "HemisNoteStale" then
        has_stale = true
      end
    end
    assert.truthy(has_stale, "Should use HemisNoteStale highlight")
  end)

  it("virt_lines_above is true for full display style", function()
    local config = require("hemis.config")
    -- Use config.setup to properly initialize config state
    config.setup({ display_style = "full" })
    local display = require("hemis.display")
    local notes = {
      { id = "1", line = 2, column = 0, text = "Above note" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    assert.truthy(mark.virt_lines_above, "Should render above the line")
  end)

  it("minimal style uses virt_text at eol with displayMarker", function()
    local config = require("hemis.config")
    -- Use config.setup to properly initialize, then restore at end
    local original_style = config.get("display_style")
    config.setup({ display_style = "minimal" })
    local display = require("hemis.display")
    -- Backend provides displayMarker for minimal display
    local notes = {
      { id = "abcd1234-5678", line = 1, column = 0, text = "Minimal note", displayMarker = "[n:abcd1234]" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Minimal style uses virt_text not virt_lines
    assert.equals("eol", mark.virt_text_pos)
    -- Should have [n:xxxx] format (first 8 chars of id)
    assert.truthy(string.find(mark.text, "%[n:abcd1234%]"), "Should have [n:id] format")
    -- Restore original config
    config.setup({ display_style = original_style or "full" })
  end)

  it("multiple notes on same line render as separate extmarks", function()
    local display = require("hemis.display")
    -- Backend provides formattedLines for each note
    local notes = {
      { id = "1", line = 1, column = 0, text = "First note", formattedLines = { "// First note" } },
      { id = "2", line = 1, column = 5, text = "Second note", formattedLines = { "// Second note" } },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    -- Notes render as separate extmarks (backend handles combining if needed)
    assert.equals(2, #state.extmarks)
    -- Both notes should be present
    local all_text = state.extmarks[1].text .. state.extmarks[2].text
    assert.truthy(string.find(all_text, "First note"))
    assert.truthy(string.find(all_text, "Second note"))
  end)

  it("multiline note renders each line as separate virt_line", function()
    local display = require("hemis.display")
    -- Backend provides formattedLines with each line pre-formatted
    local notes = {
      {
        id = "1",
        line = 1,
        column = 0,
        text = "Line one\nLine two\nLine three",
        formattedLines = { "// Line one", "// Line two", "// Line three" },
      },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Should have multiple virt_lines (one per formattedLine)
    assert.truthy(#mark.virt_lines >= 3, "Should have at least 3 virt_lines")
    -- Each line should have comment prefix
    assert.truthy(string.find(mark.virt_lines[1][1].text, "Line one"))
    assert.truthy(string.find(mark.virt_lines[2][1].text, "Line two"))
    assert.truthy(string.find(mark.virt_lines[3][1].text, "Line three"))
  end)
end)

describe("hemis display snapshots", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("snapshot: single note display", function()
    local display = require("hemis.display")
    -- Backend provides formattedLines with proper indentation
    local notes = {
      {
        id = "test-001",
        line = 2,
        column = 4,
        text = "Remember to add error handling",
        formattedLines = { "    // Remember to add error handling" },
      },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("single_note_display", state)
  end)

  it("snapshot: multiple notes display", function()
    local display = require("hemis.display")
    -- Backend provides formattedLines for each note
    local notes = {
      { id = "test-001", line = 1, column = 0, text = "Entry point", formattedLines = { "// Entry point" } },
      { id = "test-002", line = 2, column = 4, text = "Variable declaration", formattedLines = { "    // Variable declaration" } },
      { id = "test-003", line = 3, column = 0, text = "End of function", iconHint = "stale", formattedLines = { "// End of function" } },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("multiple_notes_display", state)
  end)

  it("snapshot: stale note indicator", function()
    local display = require("hemis.display")
    -- Backend marks stale notes with iconHint
    local notes = {
      { id = "stale-001", line = 1, column = 0, text = "This note is stale", iconHint = "stale", formattedLines = { "// This note is stale" } },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("stale_note_indicator", state)
  end)

  it("snapshot: multiline note text", function()
    local display = require("hemis.display")
    -- Backend provides formattedLines with each line pre-wrapped
    local notes = {
      {
        id = "multi-001",
        line = 1,
        column = 0,
        text = "This is a multiline note:\n- Point one\n- Point two\n- Point three",
        formattedLines = {
          "// This is a multiline note:",
          "// - Point one",
          "// - Point two",
          "// - Point three",
        },
      },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("multiline_note_text", state)
  end)
end)

describe("hemis commands", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  describe("list_notes", function()
    it("shows picker with notes", function()
      local commands = require("hemis.commands")
      local restore = helpers.mock_rpc({
        ["notes/list-for-file"] = {
          { id = "1", shortId = "note1", line = 1, column = 0, text = "Listed note one" },
          { id = "2", shortId = "note2", line = 2, column = 0, text = "Listed note two" },
        },
      })

      -- Capture vim.ui.select call
      local select_items = nil
      local select_opts = nil
      local original_select = vim.ui.select
      vim.ui.select = function(items, opts, on_choice)
        select_items = items
        select_opts = opts
        -- Don't call on_choice to avoid blocking
      end

      commands.list_notes()
      helpers.wait(200)

      vim.ui.select = original_select
      restore()

      -- Verify picker was called with correct items
      assert.truthy(select_items, "vim.ui.select was not called")
      assert.equals(2, #select_items)
      assert.truthy(string.find(select_items[1].label, "Listed note one"))
      assert.truthy(string.find(select_items[2].label, "Listed note two"))
    end)
  end)

  describe("help", function()
    it("shows keybinding information", function()
      local commands = require("hemis.commands")

      commands.help()
      helpers.wait()

      -- Help creates a floating window with a buffer containing keybindings
      -- Find the help buffer by checking for "Hemis Keybindings" header
      local found = false
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_valid(b) then
          local lines = vim.api.nvim_buf_get_lines(b, 0, -1, false)
          local content = table.concat(lines, "\n")
          if string.find(content, "Hemis Keybindings") and string.find(content, "Add note") then
            found = true
            break
          end
        end
      end
      assert.truthy(found, "Help buffer not found with keybinding content")
    end)
  end)

  describe("status", function()
    it("shows note and file counts", function()
      local commands = require("hemis.commands")
      -- Backend provides statusDisplay for formatted output
      local restore = helpers.mock_rpc({
        ["hemis/status"] = {
          counts = { notes = 5, files = 10, embeddings = 0 },
          statusDisplay = "Hemis: 5 notes across 10 files",
        },
      })

      local messages = {}
      local original_notify = vim.notify
      vim.notify = function(msg, ...)
        table.insert(messages, msg)
        return original_notify(msg, ...)
      end

      commands.status()
      helpers.wait()

      vim.notify = original_notify
      restore()

      local found = false
      for _, msg in ipairs(messages) do
        if string.find(msg, "5 notes") and string.find(msg, "10 files") then
          found = true
        end
      end
      assert.truthy(found, "Status message not shown correctly")
    end)
  end)

  describe("edit_note_buffer", function()
    it("opens note in a new split buffer", function()
      local commands = require("hemis.commands")
      local display = require("hemis.display")

      -- Mock RPC for update
      local update_called = false
      local restore = helpers.mock_rpc({
        ["notes/update"] = function(params)
          update_called = true
          return { id = params.id, text = params.text }
        end,
        ["notes/list-for-file"] = {},
      })

      -- Set up buffer_notes so get_note_at_cursor works
      commands.buffer_notes = {
        { id = "test-note-id", line = 1, column = 0, text = "Original note text\nSecond line" },
      }

      -- Mock get_note_at_cursor to return our note
      local orig_get_note = display.get_note_at_cursor
      display.get_note_at_cursor = function(notes)
        return notes and notes[1]
      end

      -- Count windows before
      local win_count_before = #vim.api.nvim_list_wins()

      commands.edit_note_buffer()
      helpers.wait(100)

      -- Should have opened a new window
      local win_count_after = #vim.api.nvim_list_wins()
      assert.truthy(win_count_after > win_count_before, "Should open new window")

      -- Find the hemis note buffer
      local note_buf = nil
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        local name = vim.api.nvim_buf_get_name(b)
        if string.find(name, "hemis://note/") then
          note_buf = b
          break
        end
      end

      assert.is_not_nil(note_buf, "Should create note buffer")

      -- Verify buffer content
      local lines = vim.api.nvim_buf_get_lines(note_buf, 0, -1, false)
      assert.equals("Original note text", lines[1])
      assert.equals("Second line", lines[2])

      -- Verify buffer options
      assert.equals("acwrite", vim.bo[note_buf].buftype)
      assert.equals("markdown", vim.bo[note_buf].filetype)

      -- Cleanup
      display.get_note_at_cursor = orig_get_note
      restore()
      -- Close the note buffer window
      for _, w in ipairs(vim.api.nvim_list_wins()) do
        local b = vim.api.nvim_win_get_buf(w)
        local name = vim.api.nvim_buf_get_name(b)
        if string.find(name, "hemis://note/") then
          vim.api.nvim_win_close(w, true)
        end
      end
    end)
  end)
end)

-- Tests using the demo sample source code
-- These tests verify note position tracking and stale detection
describe("hemis display with demo source", function()
  -- Demo source code from demo.json
  local DEMO_SOURCE = [[
fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}

struct Config {
    port: u16,
}

impl Default for Config {
    fn default() -> Self {
        Self { port: 8080 }
    }
}
]]

  local buf

  before_each(function()
    -- Split preserving empty lines (unlike [^\n]+ which skips them)
    local lines = {}
    for line in (DEMO_SOURCE .. "\n"):gmatch("(.-)\n") do
      table.insert(lines, line)
    end
    -- Remove first empty line if DEMO_SOURCE starts with newline
    if lines[1] == "" then
      table.remove(lines, 1)
    end
    buf = helpers.setup_test_buffer(lines)
    vim.bo[buf].filetype = "rust"
  end)

  after_each(function()
    helpers.cleanup()
  end)

  describe("note position tracking", function()
    it("displays note at stored line when no hash available", function()
      -- Notes without nodeTextHash display at their stored line
      local display = require("hemis.display")
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method for Server" },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      assert.equals(16, state.extmarks[1].line)
    end)

    it("displays note as fresh when server marks it fresh", function()
      -- Note with computedStale = false should be displayed as fresh
      local display = require("hemis.display")

      -- Server computed staleness (hash matches at stored position)
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", computedStale = false },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Should NOT use HemisNoteStale since server says not stale
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when server marks computedStale = false")
    end)

    it("finds note at new position when server provides displayLine", function()
      -- Server computes displayLine when code moves
      -- Note stored at line 16, but server computed displayLine = 18
      local display = require("hemis.display")

      -- Server has computed that code moved to line 18
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", displayLine = 18 },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Note should display at line 18 (server-computed displayLine)
      assert.equals(18, state.extmarks[1].line, "Note should display at server-computed displayLine")
      -- Should be fresh since server didn't set computedStale
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when server says so")
    end)

    it("marks note as stale when server sets computedStale", function()
      -- Server detects code was modified and sets computedStale = true
      local display = require("hemis.display")

      -- Server has computed that code was modified
      local notes = {
        { id = "note-1", line = 17, column = 8, text = "Constructor body", displayLine = 17, computedStale = true },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Note should be stale - server says so
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_true(has_stale, "Note should be stale when server sets computedStale")
    end)

    it("falls back to stored stale flag when server doesn't compute", function()
      -- When server doesn't provide displayLine/computedStale, use stored stale flag
      local display = require("hemis.display")

      -- Note without server-computed displayLine, using stored stale flag
      local notes = {
        { id = "note-1", line = 17, column = 8, text = "Constructor body", stale = true },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Should use the stored stale flag
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_true(has_stale, "Note should be stale when stored flag says so")
    end)

    it("prefers computedStale over stored stale flag", function()
      -- Server-computed staleness takes precedence over stored flag
      local display = require("hemis.display")

      -- Note has stored stale=true but server says computedStale=false
      local notes = {
        { id = "note-1", line = 1, column = 0, text = "Note text", stale = true, displayLine = 1, computedStale = false },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Should use computedStale (false), not stored stale (true)
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "computedStale should override stored stale flag")
    end)
  end)

  describe("get_note_at_cursor with position tracking", function()
    it("finds note at server-computed displayLine, not stored position", function()
      -- get_note_at_cursor should use server-provided displayLine
      local display = require("hemis.display")

      -- Note stored at line 16, but server computed displayLine = 18
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", displayLine = 18 },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      -- Set cursor to line 18 (displayLine)
      vim.api.nvim_win_set_cursor(0, { 18, 4 })

      local note = display.get_note_at_cursor(notes)
      assert.is_not_nil(note, "Should find note at displayLine")
      assert.equals("note-1", note.id)

      -- Setting cursor to line 16 (stored position) should NOT find note
      vim.api.nvim_win_set_cursor(0, { 16, 4 })
      local no_note = display.get_note_at_cursor(notes)
      assert.is_nil(no_note, "Should not find note at stored position when displayLine differs")
    end)
  end)
end)

describe("hemis selected note highlighting", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
    vim.bo[buf].filetype = "rust"
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("uses HemisNoteSelected highlight for selected note", function()
    local display = require("hemis.display")
    local notes = {
      { id = "note-1", line = 1, column = 0, text = "First note" },
      { id = "note-2", line = 2, column = 0, text = "Second note" },
    }

    -- Render with note-1 selected
    display.render_notes(buf, notes, "note-1")
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(2, #state.extmarks)

    -- Find the note at line 1 (selected)
    local selected_mark = nil
    local unselected_mark = nil
    for _, mark in ipairs(state.extmarks) do
      if mark.line == 1 then
        selected_mark = mark
      else
        unselected_mark = mark
      end
    end

    -- Selected note should use HemisNoteSelected
    local has_selected_hl = false
    for _, hl in ipairs(selected_mark.hl_groups) do
      if hl == "HemisNoteSelected" then
        has_selected_hl = true
      end
    end
    assert.truthy(has_selected_hl, "Selected note should use HemisNoteSelected highlight")

    -- Unselected note should use HemisNote
    local has_normal_hl = false
    for _, hl in ipairs(unselected_mark.hl_groups) do
      if hl == "HemisNote" then
        has_normal_hl = true
      end
    end
    assert.truthy(has_normal_hl, "Unselected note should use HemisNote highlight")
  end)

  it("selected highlight takes precedence over stale", function()
    local display = require("hemis.display")
    local notes = {
      { id = "note-1", line = 1, column = 0, text = "Stale but selected", iconHint = "stale" },
    }

    -- Render with note-1 selected (even though it's stale)
    display.render_notes(buf, notes, "note-1")
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(1, #state.extmarks)

    -- Should use HemisNoteSelected, not HemisNoteStale
    local has_selected_hl = false
    local has_stale_hl = false
    for _, hl in ipairs(state.extmarks[1].hl_groups) do
      if hl == "HemisNoteSelected" then
        has_selected_hl = true
      end
      if hl == "HemisNoteStale" then
        has_stale_hl = true
      end
    end
    assert.truthy(has_selected_hl, "Selected takes precedence - should use HemisNoteSelected")
    assert.is_false(has_stale_hl, "Selected takes precedence - should not use HemisNoteStale")
  end)

  it("no selected highlight when selected_note_id is nil", function()
    local display = require("hemis.display")
    local notes = {
      { id = "note-1", line = 1, column = 0, text = "Normal note" },
    }

    -- Render without selection
    display.render_notes(buf, notes, nil)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(1, #state.extmarks)

    -- Should use HemisNote, not HemisNoteSelected
    local has_selected_hl = false
    local has_normal_hl = false
    for _, hl in ipairs(state.extmarks[1].hl_groups) do
      if hl == "HemisNoteSelected" then
        has_selected_hl = true
      end
      if hl == "HemisNote" then
        has_normal_hl = true
      end
    end
    assert.is_false(has_selected_hl, "Should not use HemisNoteSelected when no selection")
    assert.truthy(has_normal_hl, "Should use HemisNote when no selection")
  end)
end)

-- Tests for follow_link link pattern parsing
describe("hemis follow_link", function()
  it("link pattern matches [[desc][uuid]] format", function()
    local link_pattern = "%[%[.-%]%[([%x%-]+)%]%]"
    local test_uuid = "12345678-1234-1234-1234-123456789abc"

    -- Test: link in middle of line
    local line1 = "See [[target note][" .. test_uuid .. "]] for details"
    local s, e, id = line1:find(link_pattern)
    assert.is_not_nil(s)
    assert.equals(test_uuid, id)

    -- Test: link at start of line
    local line2 = "[[first][" .. test_uuid .. "]] is important"
    s, e, id = line2:find(link_pattern)
    assert.is_not_nil(s)
    assert.equals(test_uuid, id)

    -- Test: multiple links - finds first
    local uuid2 = "abcdef01-2345-6789-abcd-ef0123456789"
    local line3 = "[[one][" .. test_uuid .. "]] and [[two][" .. uuid2 .. "]]"
    s, e, id = line3:find(link_pattern)
    assert.equals(test_uuid, id)
    -- Find second link
    s, e, id = line3:find(link_pattern, e + 1)
    assert.equals(uuid2, id)

    -- Test: no link
    local line4 = "No links here"
    s, e, id = line4:find(link_pattern)
    assert.is_nil(s)
  end)

  it("finds correct link when cursor is on it", function()
    local link_pattern = "%[%[.-%]%[([%x%-]+)%]%]"
    local uuid1 = "11111111-1111-1111-1111-111111111111"
    local uuid2 = "22222222-2222-2222-2222-222222222222"
    -- [[first][uuid1]] = 47 chars, " middle " = 8 chars, [[second][uuid2]] = 48 chars
    local line = "[[first][" .. uuid1 .. "]] middle [[second][" .. uuid2 .. "]]"

    -- Helper to find link at column (1-indexed)
    local function find_link_at_col(line_str, col)
      local link_start = 1
      while true do
        local s, e, id = line_str:find(link_pattern, link_start)
        if not s then return nil end
        if col >= s and col <= e then
          return id
        end
        link_start = e + 1
      end
    end

    -- Cursor on first link (col 5 is inside [[first][...]])
    assert.equals(uuid1, find_link_at_col(line, 5))

    -- Cursor on second link (col 60 is inside [[second][...]])
    assert.equals(uuid2, find_link_at_col(line, 60))

    -- Cursor between links (col 50 is in " middle ")
    assert.is_nil(find_link_at_col(line, 50))
  end)

  it("handles empty description in link", function()
    local link_pattern = "%[%[.-%]%[([%x%-]+)%]%]"
    local uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

    -- Empty description
    local line = "[[" .. "][" .. uuid .. "]]"
    local s, e, id = line:find(link_pattern)
    assert.is_not_nil(s)
    assert.equals(uuid, id)
  end)

  it("handles special characters in description", function()
    local link_pattern = "%[%[.-%]%[([%x%-]+)%]%]"
    local uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

    -- Description with special chars
    local line = "[[foo: bar (baz)][" .. uuid .. "]]"
    local s, e, id = line:find(link_pattern)
    assert.is_not_nil(s)
    assert.equals(uuid, id)
  end)
end)
