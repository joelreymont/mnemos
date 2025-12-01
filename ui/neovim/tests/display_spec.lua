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
        { id = "1", line = 1, column = 0, text = "Stale note", stale = true },
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
      { id = "1", line = 1, column = 0, text = "Stale note", stale = true },
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
    config.options.display_style = "full"
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

  it("minimal style uses virt_text at eol with [n:xxxx] format", function()
    local config = require("hemis.config")
    config.options.display_style = "minimal"
    local display = require("hemis.display")
    local notes = {
      { id = "abcd1234-5678", line = 1, column = 0, text = "Minimal note" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Minimal style uses virt_text not virt_lines
    assert.equals("eol", mark.virt_text_pos)
    -- Should have [n:xxxx] format (first 8 chars of id)
    assert.truthy(string.find(mark.text, "%[n:abcd1234%]"), "Should have [n:id] format")
    -- Reset config
    config.options.display_style = "full"
  end)

  it("multiple notes on same line combine with --- separator", function()
    local display = require("hemis.display")
    local notes = {
      { id = "1", line = 1, column = 0, text = "First note" },
      { id = "2", line = 1, column = 5, text = "Second note" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    -- Should be combined into single extmark
    assert.equals(1, #state.extmarks)
    -- Combined text should have separator
    assert.truthy(string.find(state.extmarks[1].text, "First note"))
    assert.truthy(string.find(state.extmarks[1].text, "Second note"))
    assert.truthy(string.find(state.extmarks[1].text, "---"))
  end)

  it("multiline note renders each line as separate virt_line", function()
    local display = require("hemis.display")
    local notes = {
      { id = "1", line = 1, column = 0, text = "Line one\nLine two\nLine three" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    local mark = state.extmarks[1]
    -- Should have multiple virt_lines (plus empty line at end)
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
    local notes = {
      { id = "test-001", line = 2, column = 4, text = "Remember to add error handling" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("single_note_display", state)
  end)

  it("snapshot: multiple notes display", function()
    local display = require("hemis.display")
    local notes = {
      { id = "test-001", line = 1, column = 0, text = "Entry point" },
      { id = "test-002", line = 2, column = 4, text = "Variable declaration", stale = false },
      { id = "test-003", line = 3, column = 0, text = "End of function", stale = true },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("multiple_notes_display", state)
  end)

  it("snapshot: stale note indicator", function()
    local display = require("hemis.display")
    local notes = {
      { id = "stale-001", line = 1, column = 0, text = "This note is stale", stale = true },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    helpers.assert_snapshot("stale_note_indicator", state)
  end)

  it("snapshot: multiline note text", function()
    local display = require("hemis.display")
    local notes = {
      {
        id = "multi-001",
        line = 1,
        column = 0,
        text = "This is a multiline note:\n- Point one\n- Point two\n- Point three",
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
    it("creates buffer showing notes", function()
      local commands = require("hemis.commands")
      local restore = helpers.mock_rpc({
        ["notes/list-for-file"] = {
          { id = "1", line = 1, column = 0, text = "Listed note one" },
          { id = "2", line = 2, column = 0, text = "Listed note two" },
        },
      })

      commands.list_notes()
      helpers.wait(200)

      -- Find the Hemis Notes buffer
      local found = false
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        local name = vim.api.nvim_buf_get_name(b)
        if string.find(name, "Hemis Notes") then
          found = true
          helpers.assert_buffer_contains(b, "Listed note one")
          helpers.assert_buffer_contains(b, "Listed note two")
        end
      end

      restore()
      assert.truthy(found, "Hemis Notes buffer not found")
    end)
  end)

  describe("help", function()
    it("shows keybinding information", function()
      local commands = require("hemis.commands")

      -- Capture notifications
      local messages = {}
      local original_notify = vim.notify
      vim.notify = function(msg, ...)
        table.insert(messages, msg)
        return original_notify(msg, ...)
      end

      commands.help()

      vim.notify = original_notify

      -- Check help content was shown
      local found = false
      for _, msg in ipairs(messages) do
        if string.find(msg, "Hemis") and string.find(msg, "Add note") then
          found = true
        end
      end
      assert.truthy(found, "Help message not shown")
    end)
  end)

  describe("status", function()
    it("shows note and file counts", function()
      local commands = require("hemis.commands")
      local restore = helpers.mock_rpc({
        ["hemis/status"] = {
          counts = { notes = 5, files = 10, embeddings = 0 },
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
end)
