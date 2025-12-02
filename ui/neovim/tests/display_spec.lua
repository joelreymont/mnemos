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

  it("minimal style uses virt_text at eol with [n:xxxx] format", function()
    local config = require("hemis.config")
    -- Use config.setup to properly initialize, then restore at end
    local original_style = config.get("display_style")
    config.setup({ display_style = "minimal" })
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
    -- Restore original config
    config.setup({ display_style = original_style or "full" })
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

    it("displays note as fresh when hash matches stored line", function()
      -- Note with matching hash at stored position should be fresh
      local display = require("hemis.display")
      local ts = require("hemis.treesitter")

      -- Get the actual hash at line 16 (fn new)
      local hash = ts.get_hash_at_position(buf, 16, 4)

      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", nodeTextHash = hash },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Should NOT use HemisNoteStale since hash matches
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when hash matches")
    end)

    it("finds note at new position when code moves down", function()
      -- Simulate inserting lines above the note's original position
      -- Note was at line 16 (fn new), but code moved to line 18 after inserting 2 lines above
      local display = require("hemis.display")
      local ts = require("hemis.treesitter")

      -- Get the hash of "fn new" at line 16 (inside impl Server block)
      local hash = ts.get_hash_at_position(buf, 16, 4)
      assert.is_not_nil(hash, "Should get hash at line 16")

      -- Insert 2 comment lines after line 15 (impl Server {), before fn new
      -- Line 15 = "impl Server {"
      -- Line 16 = "    fn new(...)"
      vim.api.nvim_buf_set_lines(buf, 15, 15, false, {
        "    // Added comment line 1",
        "    // Added comment line 2",
      })
      helpers.wait()

      -- Now fn new is at line 18, but note still has line = 16
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", nodeTextHash = hash },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Note should display at line 18 (where the code actually is now)
      assert.equals(18, state.extmarks[1].line, "Note should follow code to new position")
      -- Should still be fresh since hash was found
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when code just moved")
    end)

    it("marks note as stale when tree-sitter node content changes", function()
      -- Changing "Self { config }" to "Server { config }" changes the hash
      -- But the tree-sitter node still exists, so note stays fresh
      -- Line 17 = "        Self { config }"
      local display = require("hemis.display")
      local ts = require("hemis.treesitter")

      -- Get the original hash at line 17 (Self { config })
      local original_hash = ts.get_hash_at_position(buf, 17, 8)
      assert.is_not_nil(original_hash, "Should get hash at line 17")

      -- Modify line 17: change Self to Server
      local line_content = vim.api.nvim_buf_get_lines(buf, 16, 17, false)[1]
      assert.truthy(line_content:find("Self"), "Line 17 should contain Self")
      local modified_content = line_content:gsub("Self", "Server")
      vim.api.nvim_buf_set_lines(buf, 16, 17, false, { modified_content })
      helpers.wait()

      -- Note still has the old hash
      local notes = {
        { id = "note-1", line = 17, column = 8, text = "Constructor body", nodeTextHash = original_hash },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Note should NOT be stale - there's still a tree-sitter node at the position
      -- The hash changed but the code still exists, so it's still relevant
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when node still exists (even if modified)")
    end)

    it("keeps note fresh when node removed but other code exists nearby", function()
      -- Delete the line entirely - but there's still other code in range
      local display = require("hemis.display")
      local ts = require("hemis.treesitter")

      -- Get the original hash at line 17
      local original_hash = ts.get_hash_at_position(buf, 17, 8)
      assert.is_not_nil(original_hash, "Should get hash at line 17")

      -- Delete line 17 entirely
      vim.api.nvim_buf_set_lines(buf, 16, 17, false, {})
      helpers.wait()

      -- Note still references line 17 with old hash
      local notes = {
        { id = "note-1", line = 17, column = 8, text = "Constructor body", nodeTextHash = original_hash },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(buf)
      assert.equals(1, #state.extmarks)
      -- Should NOT be stale - there's still other code in the search range
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.is_false(has_stale, "Note should be fresh when other code exists in range")
    end)

    it("marks note as stale when all code in range is removed", function()
      -- Use a buffer without tree-sitter support (plain text)
      local display = require("hemis.display")

      local plain_buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_lines(plain_buf, 0, -1, false, { "just some text", "no tree-sitter" })
      vim.bo[plain_buf].filetype = "text" -- No tree-sitter parser for plain text
      helpers.wait()

      -- Note references line 1 with a hash - but no tree-sitter, so no nodes to find
      local notes = {
        { id = "note-1", line = 1, column = 0, text = "Note on deleted code", nodeTextHash = "abc123nonexistent" },
      }

      display.render_notes(plain_buf, notes)
      helpers.wait()

      local state = helpers.capture_display_state(plain_buf)
      assert.equals(1, #state.extmarks)
      -- Should be stale since there's no tree-sitter parser
      local has_stale = false
      for _, hl in ipairs(state.extmarks[1].hl_groups) do
        if hl == "HemisNoteStale" then
          has_stale = true
        end
      end
      assert.truthy(has_stale, "Note should be stale when no tree-sitter available")
      vim.api.nvim_buf_delete(plain_buf, { force = true })
    end)
  end)

  describe("get_note_at_cursor with position tracking", function()
    it("finds note at display position, not stored position", function()
      -- When code moves, get_note_at_cursor should use display position
      local display = require("hemis.display")
      local ts = require("hemis.treesitter")

      -- Get hash at line 16 (fn new)
      local hash = ts.get_hash_at_position(buf, 16, 4)
      assert.is_not_nil(hash, "Should get hash at line 16")

      -- Insert lines after line 15 (impl Server {) to move code down
      vim.api.nvim_buf_set_lines(buf, 15, 15, false, {
        "    // Comment 1",
        "    // Comment 2",
      })
      helpers.wait()

      -- Note stored at line 16, but code is now at line 18
      local notes = {
        { id = "note-1", line = 16, column = 4, text = "Factory method", nodeTextHash = hash },
      }

      display.render_notes(buf, notes)
      helpers.wait()

      -- Set cursor to line 18 (where code actually is)
      vim.api.nvim_win_set_cursor(0, { 18, 4 })

      local note = display.get_note_at_cursor(notes)
      assert.is_not_nil(note, "Should find note at display position")
      assert.equals("note-1", note.id)

      -- Setting cursor to line 16 (stored position) should NOT find note
      vim.api.nvim_win_set_cursor(0, { 16, 4 })
      local no_note = display.get_note_at_cursor(notes)
      assert.is_nil(no_note, "Should not find note at old stored position")
    end)
  end)
end)
