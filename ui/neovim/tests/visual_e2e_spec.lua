-- Visual E2E tests for Hemis Neovim plugin
-- These tests verify that notes ACTUALLY APPEAR VISUALLY (extmarks exist)
-- Run with: nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedFile tests/visual_e2e_spec.lua"
--
-- REQUIRES:
-- - Backend built: cargo build -p backend

local helpers = require("tests.helpers")

-- Helper to set up test environment with real backend
local function setup_e2e_env()
  local test_dir = vim.fn.tempname() .. "_visual_e2e_" .. math.random(10000)
  local test_file = test_dir .. "/app.rs"
  vim.fn.mkdir(test_dir, "p")

  -- Write test file - MUST MATCH config.demo exactly for goto-symbol tests
  local code = [[fn main() {
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
  local f = io.open(test_file, "w")
  if f then
    f:write(code)
    f:close()
  end

  -- Stop any existing RPC connection
  local rpc = require("hemis.rpc")
  rpc.stop()

  -- Configure hemis
  local config = require("hemis.config")
  config.setup({
    backend = vim.g.hemis_test_backend,
    hemis_dir = test_dir,
    auto_refresh = false,
    keymaps = false,
  })

  -- Open the file in a buffer
  vim.cmd("edit " .. test_file)
  local buf = vim.api.nvim_get_current_buf()
  vim.bo[buf].filetype = "rust"

  return {
    dir = test_dir,
    file = test_file,
    buf = buf,
    rpc = rpc,
    cleanup = function()
      rpc.stop()
      pcall(function()
        vim.cmd("bdelete! " .. buf)
      end)
      vim.fn.delete(test_dir, "rf")
    end,
  }
end

-- Helper to verify extmarks exist with content
local function verify_extmarks_visible(buf, expected_text)
  local display = require("hemis.display")
  local ns = display.ns_id
  local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, { details = true })

  if #marks == 0 then
    return false, "No extmarks found"
  end

  -- Check if any mark has the expected text
  local found_text = false
  local all_text = ""
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.virt_lines then
      for _, vline in ipairs(details.virt_lines) do
        for _, chunk in ipairs(vline) do
          all_text = all_text .. (chunk[1] or "")
        end
      end
    elseif details.virt_text then
      for _, chunk in ipairs(details.virt_text) do
        all_text = all_text .. (chunk[1] or "")
      end
    end
  end

  if expected_text and not string.find(all_text, expected_text, 1, true) then
    return false, "Expected text '" .. expected_text .. "' not found in extmarks: " .. all_text
  end

  return true, nil
end

describe("visual e2e: notes appear as extmarks", function()
  after_each(function()
    helpers.cleanup()
  end)

  it("VISUAL: create note and verify extmark appears", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil
    local create_err = nil

    -- Connect to backend
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      -- Create a note
      env.rpc.request("notes/create", {
        file = env.file,
        line = 7,
        column = 0,
        text = "Visual test note content",
        projectRoot = env.dir,
      }, function(err, res)
        create_err = err
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)

    assert.truthy(connected, "Should connect to backend")
    assert.is_nil(create_err, "Note creation should not error: " .. vim.inspect(create_err))
    assert.is_not_nil(note_id, "Note should have been created with ID")

    -- Now list notes and render them
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Notes list should not be empty")

    -- CRITICAL: Render the notes to display
    display.render_notes(env.buf, notes)
    helpers.wait(100) -- Brief wait for rendering

    -- VERIFY: Check that extmarks actually exist
    local visible, err_msg = verify_extmarks_visible(env.buf, "Visual test note")
    assert.truthy(visible, "VISUAL VERIFICATION FAILED: " .. (err_msg or "unknown"))

    env.cleanup()
  end)

  it("VISUAL: note displays at correct line after render", function()
    -- This test verifies extmarks are placed at the line position returned by backend
    -- Position tracking logic is tested in Rust unit tests (treesitter/src/position.rs)
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note at line 3 (inside main())
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      env.rpc.request("notes/create", {
        file = env.file,
        line = 3,
        column = 4,
        text = "Note at line 3",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List notes
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Notes list should not be empty")

    -- Render and verify extmark at backend-specified position
    display.render_notes(env.buf, notes)
    helpers.wait(100)

    -- Verify extmark exists at the note's line
    local ns = display.ns_id
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "VISUAL VERIFICATION FAILED: No extmarks found")

    -- Verify extmark is at the correct line (0-indexed in extmarks)
    local expected_line = notes[1].line
    local mark_line = marks[1][2] + 1 -- Convert to 1-indexed
    assert.equals(expected_line, mark_line, "VISUAL VERIFICATION FAILED: Extmark should be at line " .. expected_line .. ", was at " .. mark_line)

    env.cleanup()
  end)

  it("VISUAL: stale note shows with stale highlight", function()
    -- This test verifies that notes with stale=true render with HemisNoteStale highlight
    -- We directly test the display module with a mock stale note
    local env = setup_e2e_env()
    local display = require("hemis.display")

    -- Create a mock note that is stale (simulates what backend would return)
    local stale_note = {
      id = "mock-stale-note-id",
      shortId = "mock1234",
      file = env.file,
      projectRoot = env.dir,
      line = 3,
      column = 4,
      text = "This note is stale",
      summary = "This note is stale",
      stale = true,  -- Key field - note is stale
      iconHint = "stale",
      formattedLines = { "// This note is stale" },
      displayMarker = "[n:mock1234]",
      hoverText = "**Note** (mock1234) [STALE]\n\nThis note is stale",
      displayLabel = "[Note] This note is stale",
      displayDetail = env.file .. ":3",
      createdAt = os.time(),
      updatedAt = os.time(),
      formattedCreatedAt = os.date("%Y-%m-%d %H:%M:%S"),
      formattedUpdatedAt = os.date("%Y-%m-%d %H:%M:%S"),
    }

    -- Render the stale note
    display.render_notes(env.buf, { stale_note })
    helpers.wait(100)

    -- Verify extmark exists with stale highlight
    local ns = display.ns_id
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "VISUAL VERIFICATION FAILED: No extmarks found")

    -- Check for HemisNoteStale highlight
    local has_stale_hl = false
    local details = marks[1][4] or {}
    if details.virt_lines then
      for _, vline in ipairs(details.virt_lines) do
        for _, chunk in ipairs(vline) do
          if chunk[2] and string.find(chunk[2], "Stale", 1, true) then
            has_stale_hl = true
          end
        end
      end
    end

    assert.truthy(has_stale_hl, "VISUAL VERIFICATION FAILED: Stale note should have Stale highlight group")

    env.cleanup()
  end)

  it("VISUAL: delete note removes extmark", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      env.rpc.request("notes/create", {
        file = env.file,
        line = 7,
        column = 0,
        text = "Note to delete",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List and render to verify note appears
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    display.render_notes(env.buf, notes)
    helpers.wait(100)

    -- Verify extmark exists before delete
    local ns = display.ns_id
    local marks_before = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, {})
    assert.truthy(#marks_before > 0, "Should have extmark before delete")

    -- Delete the note
    local delete_done = false
    env.rpc.request("notes/delete", {
      id = note_id,
    }, function(err, res)
      delete_done = true
    end)

    helpers.wait_for(function() return delete_done end, 5000)

    -- List again and re-render
    list_done = false
    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    display.render_notes(env.buf, notes)
    helpers.wait(100)

    -- VERIFY: Extmark should be gone
    local marks_after = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, {})
    assert.equals(0, #marks_after, "VISUAL VERIFICATION FAILED: Extmark should be removed after delete")

    env.cleanup()
  end)

  it("VISUAL: multiline note renders multiple virt_lines", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create multiline note
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      env.rpc.request("notes/create", {
        file = env.file,
        line = 7,
        column = 0,
        text = "Line one of note\nLine two of note\nLine three of note",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List and render
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")

    display.render_notes(env.buf, notes)
    helpers.wait(100)

    -- VERIFY: Extmark should have multiple virt_lines
    local ns = display.ns_id
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "VISUAL VERIFICATION FAILED: No extmarks found")

    local details = marks[1][4] or {}
    local virt_lines = details.virt_lines or {}

    -- Should have at least 3 virtual lines for 3-line note
    assert.truthy(#virt_lines >= 3, "VISUAL VERIFICATION FAILED: Should have at least 3 virt_lines, got " .. #virt_lines)

    -- Verify content
    local all_text = ""
    for _, vline in ipairs(virt_lines) do
      for _, chunk in ipairs(vline) do
        all_text = all_text .. (chunk[1] or "")
      end
    end

    assert.truthy(string.find(all_text, "Line one", 1, true), "Should contain 'Line one'")
    assert.truthy(string.find(all_text, "Line two", 1, true), "Should contain 'Line two'")
    assert.truthy(string.find(all_text, "Line three", 1, true), "Should contain 'Line three'")

    env.cleanup()
  end)

  it("VISUAL: note indentation matches column position", function()
    -- This test verifies that formattedLines have correct indentation
    -- based on the note's column (matching tree-sitter node alignment)
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note at column 4 (inside function body)
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      -- Create note at line 3, column 4 (indented code inside main())
      env.rpc.request("notes/create", {
        file = env.file,
        line = 3,
        column = 4,
        text = "Indented note",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List notes and check formattedLines
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Notes list should not be empty")

    -- Verify formattedLines have 4-space indent (column 4)
    local note = notes[1]
    local formatted = note.formattedLines
    assert.is_not_nil(formatted, "Note should have formattedLines")
    assert.truthy(#formatted > 0, "formattedLines should not be empty")

    -- First line should start with 4 spaces + comment prefix
    local first_line = formatted[1]
    assert.truthy(string.find(first_line, "^    // "), "INDENT VERIFICATION FAILED: formattedLines should start with 4-space indent, got: '" .. first_line .. "'")

    -- Render and verify extmark contains indented text
    display.render_notes(env.buf, notes)
    helpers.wait(100)

    local ns = display.ns_id
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "VISUAL VERIFICATION FAILED: No extmarks found")

    -- Check extmark text includes the indentation
    local all_text = ""
    local details = marks[1][4] or {}
    if details.virt_lines then
      for _, vline in ipairs(details.virt_lines) do
        for _, chunk in ipairs(vline) do
          all_text = all_text .. (chunk[1] or "")
        end
      end
    end

    assert.truthy(string.find(all_text, "    // Indented note", 1, true), "VISUAL VERIFICATION FAILED: Extmark should contain indented text")

    env.cleanup()
  end)

  it("VISUAL: note at column 0 has no indentation", function()
    -- This test verifies notes at column 0 have no leading spaces
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note at column 0 (top-level)
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      -- Create note at line 7, column 0 (fn load_config at top level)
      env.rpc.request("notes/create", {
        file = env.file,
        line = 7,
        column = 0,
        text = "Top level note",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List notes and check formattedLines
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Notes list should not be empty")

    -- Verify formattedLines have NO indent (column 0)
    local note = notes[1]
    local formatted = note.formattedLines
    assert.is_not_nil(formatted, "Note should have formattedLines")
    assert.truthy(#formatted > 0, "formattedLines should not be empty")

    -- First line should start with "// " (no leading spaces)
    local first_line = formatted[1]
    assert.truthy(string.find(first_line, "^// "), "INDENT VERIFICATION FAILED: Column 0 note should have no indent, got: '" .. first_line .. "'")
    assert.falsy(string.find(first_line, "^%s+// "), "INDENT VERIFICATION FAILED: Column 0 note should NOT start with spaces")

    env.cleanup()
  end)

  it("VISUAL: nested note has deeper indentation", function()
    -- This test verifies notes at column 8 (nested block) have 8-space indent
    local env = setup_e2e_env()
    local display = require("hemis.display")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note at column 8 (deeply nested)
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      -- Create note at line 13, column 8 (inside impl Server, inside fn new)
      env.rpc.request("notes/create", {
        file = env.file,
        line = 13,
        column = 8,
        text = "Deeply nested note",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List notes and check formattedLines
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Notes list should not be empty")

    -- Verify formattedLines have 8-space indent (column 8)
    local note = notes[1]
    local formatted = note.formattedLines
    assert.is_not_nil(formatted, "Note should have formattedLines")
    assert.truthy(#formatted > 0, "formattedLines should not be empty")

    -- First line should start with 8 spaces + comment prefix
    local first_line = formatted[1]
    assert.truthy(string.find(first_line, "^        // "), "INDENT VERIFICATION FAILED: formattedLines should start with 8-space indent, got: '" .. first_line .. "'")

    env.cleanup()
  end)
end)

-- Test index/search symbol lookup
describe("visual e2e: index/search symbol lookup", function()
  after_each(function()
    helpers.cleanup()
  end)

  it("index/search returns correct line for fn definition", function()
    local env = setup_e2e_env()

    local connected = false
    local index_done = false
    local search_done = false
    local search_result = nil
    local search_err = nil

    -- Read file content for indexing
    local file_content = table.concat(vim.api.nvim_buf_get_lines(env.buf, 0, -1, false), "\n")

    -- Connect to backend
    env.rpc.start(function(ok)
      if not ok then
        index_done = true
        search_done = true
        return
      end
      connected = true

      -- First index the file with content
      env.rpc.request("index/add-file", {
        file = env.file,
        projectRoot = env.dir,
        content = file_content,
      }, function(err, res)
        index_done = true

        if err then
          search_done = true
          return
        end

        -- Now search for "fn load_config"
        env.rpc.request("index/search", {
          query = "fn load_config",
          projectRoot = env.dir,
        }, function(search_error, result)
          search_err = search_error
          search_result = result
          search_done = true
        end)
      end)
    end)

    helpers.wait_for(function() return search_done end, 10000)

    assert.truthy(connected, "Should connect to backend")
    assert.truthy(index_done, "Should complete indexing")
    assert.is_nil(search_err, "Search should not error: " .. vim.inspect(search_err))
    assert.is_not_nil(search_result, "Search should return results")

    -- Result should be an array with at least one hit
    assert.truthy(#search_result > 0, "Should have at least one search result")

    local first_hit = search_result[1]
    -- Compare resolved paths (macOS /var -> /private/var symlink)
    assert.equals(vim.fn.resolve(env.file), vim.fn.resolve(first_hit.file), "Result should be in test file")
    assert.equals(7, first_hit.line, "fn load_config should be on line 7")
    assert.truthy(string.find(first_hit.text, "fn load_config", 1, true), "Result text should contain query")

    env.cleanup()
  end)

  it("index/search returns correct line for struct definition", function()
    local env = setup_e2e_env()

    local connected = false
    local search_done = false
    local search_result = nil

    -- Read file content for indexing
    local file_content = table.concat(vim.api.nvim_buf_get_lines(env.buf, 0, -1, false), "\n")

    env.rpc.start(function(ok)
      if not ok then
        search_done = true
        return
      end
      connected = true

      -- Index the file with content
      env.rpc.request("index/add-file", {
        file = env.file,
        projectRoot = env.dir,
        content = file_content,
      }, function()
        -- Search for "impl Server"
        env.rpc.request("index/search", {
          query = "impl Server",
          projectRoot = env.dir,
        }, function(err, result)
          search_result = result
          search_done = true
        end)
      end)
    end)

    helpers.wait_for(function() return search_done end, 10000)

    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(search_result, "Search should return results")
    assert.truthy(#search_result > 0, "Should have at least one search result")

    local first_hit = search_result[1]
    assert.equals(15, first_hit.line, "impl Server should be on line 15")

    env.cleanup()
  end)

  it("index/search returns correct line for fn new inside impl", function()
    local env = setup_e2e_env()

    local connected = false
    local search_done = false
    local search_result = nil

    -- Read file content for indexing
    local file_content = table.concat(vim.api.nvim_buf_get_lines(env.buf, 0, -1, false), "\n")

    env.rpc.start(function(ok)
      if not ok then
        search_done = true
        return
      end
      connected = true

      env.rpc.request("index/add-file", {
        file = env.file,
        projectRoot = env.dir,
        content = file_content,
      }, function()
        env.rpc.request("index/search", {
          query = "fn new",
          projectRoot = env.dir,
        }, function(err, result)
          search_result = result
          search_done = true
        end)
      end)
    end)

    helpers.wait_for(function() return search_done end, 10000)

    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(search_result, "Search should return results")
    assert.truthy(#search_result > 0, "Should have at least one search result")

    local first_hit = search_result[1]
    assert.equals(16, first_hit.line, "fn new should be on line 16")

    env.cleanup()
  end)
end)

-- Test wait_for_extmarks() synchronization
describe("visual e2e: wait_for_extmarks sync", function()
  after_each(function()
    helpers.cleanup()
  end)

  it("wait_for_extmarks returns true when extmarks exist", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Create and render a note (via mock data, not RPC)
    local mock_note = {
      id = "sync-test-note",
      shortId = "sync1234",
      file = env.file,
      projectRoot = env.dir,
      line = 3,
      column = 0,
      text = "Sync test note",
      summary = "Sync test note",
      stale = false,
      iconHint = "fresh",
      formattedLines = { "// Sync test note" },
      displayMarker = "[n:sync1234]",
      hoverText = "**Note**\n\nSync test note",
      displayLabel = "[Note] Sync test note",
      displayDetail = env.file .. ":3",
      createdAt = os.time(),
      updatedAt = os.time(),
      formattedCreatedAt = os.date("%Y-%m-%d %H:%M:%S"),
      formattedUpdatedAt = os.date("%Y-%m-%d %H:%M:%S"),
    }

    -- Render extmarks
    display.render_notes(env.buf, { mock_note })
    helpers.wait(50)

    -- wait_for_extmarks should return true immediately since extmarks exist
    local result = commands.wait_for_extmarks(env.buf, 1000)
    assert.truthy(result, "wait_for_extmarks should return true when extmarks exist")

    env.cleanup()
  end)

  it("wait_for_extmarks returns false on timeout when no extmarks", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Clear any existing extmarks
    local ns = display.ns_id
    vim.api.nvim_buf_clear_namespace(env.buf, ns, 0, -1)

    -- wait_for_extmarks should return false (timeout) since no extmarks
    local start = vim.uv and vim.uv.now() or vim.loop.now()
    local result = commands.wait_for_extmarks(env.buf, 200)  -- Short timeout
    local elapsed = (vim.uv and vim.uv.now() or vim.loop.now()) - start

    assert.falsy(result, "wait_for_extmarks should return false when no extmarks")
    assert.truthy(elapsed >= 150, "Should wait at least close to timeout duration")

    env.cleanup()
  end)

  it("wait_for_extmarks detects extmarks created during wait", function()
    local env = setup_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Clear any existing extmarks
    local ns = display.ns_id
    vim.api.nvim_buf_clear_namespace(env.buf, ns, 0, -1)

    -- Schedule extmark creation after 100ms
    vim.defer_fn(function()
      local mock_note = {
        id = "delayed-note",
        shortId = "delay123",
        file = env.file,
        projectRoot = env.dir,
        line = 5,
        column = 0,
        text = "Delayed note",
        summary = "Delayed note",
        stale = false,
        iconHint = "fresh",
        formattedLines = { "// Delayed note" },
        displayMarker = "[n:delay123]",
        hoverText = "**Note**\n\nDelayed note",
        displayLabel = "[Note] Delayed note",
        displayDetail = env.file .. ":5",
        createdAt = os.time(),
        updatedAt = os.time(),
        formattedCreatedAt = os.date("%Y-%m-%d %H:%M:%S"),
        formattedUpdatedAt = os.date("%Y-%m-%d %H:%M:%S"),
      }
      display.render_notes(env.buf, { mock_note })
    end, 100)

    -- wait_for_extmarks should eventually detect the delayed extmarks
    local result = commands.wait_for_extmarks(env.buf, 2000)
    assert.truthy(result, "wait_for_extmarks should detect extmarks created during wait")

    -- Verify extmarks actually exist
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, {})
    assert.truthy(#marks > 0, "Extmarks should exist after wait")

    env.cleanup()
  end)
end)

-- goto-symbol e2e tests (simulating demo driver behavior)
-- These tests verify the FULL goto-symbol flow: index -> search -> navigate
describe("visual e2e: goto-symbol (demo driver flow)", function()
  after_each(function()
    helpers.cleanup()
  end)

  -- Helper to simulate goto-symbol: index file, search, navigate to line
  local function goto_symbol(env, symbol_query)
    local file_content = table.concat(vim.api.nvim_buf_get_lines(env.buf, 0, -1, false), "\n")

    local index_done = false
    local search_result = nil
    local search_err = nil

    -- Step 1: Index the file (like hemis plugin does on BufEnter)
    env.rpc.request("index/add-file", {
      file = env.file,
      projectRoot = env.dir,
      content = file_content,
    }, function(err, res)
      if err then
        index_done = true
        search_err = err
        return
      end

      -- Step 2: Search for symbol (like demo driver does)
      env.rpc.request("index/search", {
        query = symbol_query,
        projectRoot = env.dir,  -- Demo uses non-canonical path
      }, function(search_error, result)
        search_err = search_error
        search_result = result
        index_done = true
      end)
    end)

    helpers.wait_for(function() return index_done end, 10000)

    if search_err then
      return nil, "Search error: " .. vim.inspect(search_err)
    end

    if not search_result or #search_result == 0 then
      return nil, "No results for: " .. symbol_query
    end

    -- Step 3: Navigate to the line (like demo driver does with goto keyActions)
    local hit = search_result[1]
    vim.api.nvim_win_set_cursor(0, { hit.line, 0 })

    return hit, nil
  end

  it("GOTO-SYMBOL E2E: fn load_config navigates to correct line", function()
    local env = setup_e2e_env()

    local connected = false
    env.rpc.start(function(ok)
      connected = ok
    end)
    helpers.wait_for(function() return connected end, 10000)
    assert.truthy(connected, "Should connect to backend")

    local hit, err = goto_symbol(env, "fn load_config")
    assert.is_nil(err, err)
    assert.is_not_nil(hit)

    -- Verify cursor is at correct line (line 7 in the full demo file)
    local cursor = vim.api.nvim_win_get_cursor(0)
    assert.equals(7, cursor[1], "Cursor should be at line 7 (fn load_config)")
    assert.equals(7, hit.line, "Search hit should report line 7")

    env.cleanup()
  end)

  it("GOTO-SYMBOL E2E: impl Server navigates to correct line", function()
    local env = setup_e2e_env()

    local connected = false
    env.rpc.start(function(ok)
      connected = ok
    end)
    helpers.wait_for(function() return connected end, 10000)
    assert.truthy(connected, "Should connect to backend")

    local hit, err = goto_symbol(env, "impl Server")
    assert.is_nil(err, err)
    assert.is_not_nil(hit)

    -- impl Server is at line 15 in the full demo file
    local cursor = vim.api.nvim_win_get_cursor(0)
    assert.equals(15, cursor[1], "Cursor should be at line 15 (impl Server)")
    assert.equals(15, hit.line, "Search hit should report line 15")

    env.cleanup()
  end)

  it("GOTO-SYMBOL E2E: fn new navigates to correct line", function()
    local env = setup_e2e_env()

    local connected = false
    env.rpc.start(function(ok)
      connected = ok
    end)
    helpers.wait_for(function() return connected end, 10000)
    assert.truthy(connected, "Should connect to backend")

    local hit, err = goto_symbol(env, "fn new")
    assert.is_nil(err, err)
    assert.is_not_nil(hit)

    -- fn new is at line 16 in the full demo file
    local cursor = vim.api.nvim_win_get_cursor(0)
    assert.equals(16, cursor[1], "Cursor should be at line 16 (fn new)")
    assert.equals(16, hit.line, "Search hit should report line 16")

    env.cleanup()
  end)

  it("GOTO-SYMBOL E2E: handles symlink path canonicalization", function()
    -- Use /tmp which is symlinked to /private/tmp on macOS
    -- This simulates the demo driver scenario exactly
    local test_dir = "/tmp/hemis_goto_symbol_e2e_" .. math.random(100000)
    local test_file = test_dir .. "/app.rs"
    vim.fn.mkdir(test_dir, "p")

    -- MUST MATCH config.demo exactly
    local code = [[fn main() {
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
    local f = io.open(test_file, "w")
    if f then
      f:write(code)
      f:close()
    end

    -- Configure with /tmp path (non-canonical)
    local rpc = require("hemis.rpc")
    rpc.stop()

    local config = require("hemis.config")
    config.setup({
      backend = vim.g.hemis_test_backend,
      hemis_dir = test_dir,
      auto_refresh = false,
      keymaps = false,
    })

    vim.cmd("edit " .. test_file)
    local buf = vim.api.nvim_get_current_buf()
    vim.bo[buf].filetype = "rust"

    local connected = false
    rpc.start(function(ok)
      connected = ok
    end)
    helpers.wait_for(function() return connected end, 10000)
    assert.truthy(connected, "Should connect to backend")

    -- Check path situation
    local resolved = vim.fn.resolve(test_file)
    local is_symlinked = resolved ~= test_file

    -- Index with canonical path (backend will canonicalize)
    local file_content = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")

    local search_done = false
    local search_result = nil

    rpc.request("index/add-file", {
      file = test_file,  -- /tmp/... (non-canonical)
      projectRoot = test_dir,  -- /tmp/... (non-canonical)
      content = file_content,
    }, function(err, res)
      -- Search with non-canonical path (like demo driver does)
      rpc.request("index/search", {
        query = "fn load_config",
        projectRoot = test_dir,  -- /tmp/... (non-canonical)
      }, function(search_err, result)
        search_result = result
        search_done = true
      end)
    end)

    helpers.wait_for(function() return search_done end, 10000)

    assert.is_not_nil(search_result, "Search should return results")
    assert.truthy(#search_result > 0, "Should find fn load_config")
    assert.equals(7, search_result[1].line, "fn load_config should be on line 7")

    if is_symlinked then
      -- This is the key test: search with /tmp/... finds file indexed with /private/tmp/...
      assert.truthy(string.find(test_file, "^/tmp/"), "Test file should use /tmp path")
      -- The fix canonicalizes projectRoot in index/search, so this should work
    end

    -- Cleanup
    rpc.stop()
    pcall(function() vim.cmd("bdelete! " .. buf) end)
    vim.fn.delete(test_dir, "rf")
  end)
end)

-- AI tests (require HEMIS_AI_PROVIDER)
-- These are TRUE e2e tests that use the actual commands and event system
describe("visual e2e: AI features", function()
  local ai_available = vim.env.HEMIS_AI_PROVIDER ~= nil

  if not ai_available then
    it("requires AI provider (HEMIS_AI_PROVIDER)", function()
      pending("Set HEMIS_AI_PROVIDER=claude to run AI tests")
    end)
    return
  end

  -- Full e2e setup: initializes hemis with events, uses real user flow
  local function setup_ai_e2e_env()
    -- Use /tmp to test path canonicalization (macOS: /tmp -> /private/tmp)
    local test_dir = "/tmp/hemis_ai_e2e_" .. math.random(100000)
    local test_file = test_dir .. "/app.rs"
    vim.fn.mkdir(test_dir, "p")

    -- Write test file
    local code = [[fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }
}
]]
    local f = io.open(test_file, "w")
    if f then
      f:write(code)
      f:close()
    end

    -- Stop any existing connections
    local rpc = require("hemis.rpc")
    local events = require("hemis.events")
    rpc.stop()
    events.stop()

    -- Configure hemis first (but don't call full setup yet - backend not running)
    local config = require("hemis.config")
    config.setup({
      backend = vim.g.hemis_test_backend,
      hemis_dir = test_dir,
      auto_refresh = false,
      keymaps = false,
    })

    -- Open the file (using /tmp path, not /private/tmp)
    vim.cmd("edit " .. test_file)
    local buf = vim.api.nvim_get_current_buf()
    vim.bo[buf].filetype = "rust"

    -- Start RPC and wait for connection (starts backend)
    local rpc_connected = false
    rpc.start(function(ok)
      rpc_connected = ok
    end)
    helpers.wait_for(function() return rpc_connected end, 10000)

    -- Now do full hemis setup which starts events and registers handlers
    -- Backend is running so events socket should exist
    local hemis = require("hemis")
    hemis.setup({
      backend = vim.g.hemis_test_backend,
      hemis_dir = test_dir,
      auto_refresh = false,
      keymaps = false,
    })

    -- Wait for events socket to connect (with retries)
    local events_connected = helpers.wait_for(function()
      -- Process libuv events to allow reconnect timer to fire
      local uv = vim.uv or vim.loop
      uv.run("nowait")
      return events.is_connected()
    end, 10000)

    if not events_connected then
      -- Fallback: tests will still work via the refresh fallback we added
      vim.notify("Events not connected - tests will use fallback refresh", vim.log.levels.WARN)
    end

    return {
      dir = test_dir,
      file = test_file,
      buf = buf,
      rpc = rpc,
      events = events,
      hemis = hemis,
      cleanup = function()
        events.stop()
        rpc.stop()
        pcall(function()
          vim.cmd("bdelete! " .. buf)
        end)
        vim.fn.delete(test_dir, "rf")
      end,
    }
  end

  after_each(function()
    helpers.cleanup()
  end)

  it("TRUE E2E: explain_region command creates note via event system", function()
    local env = setup_ai_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Note: events may not connect in headless mode, but the fallback refresh will work

    -- Make visual selection (lines 7-9: load_config function)
    vim.api.nvim_win_set_cursor(0, { 7, 0 })
    vim.cmd("normal! V2j")  -- Visual line mode, select 3 lines
    vim.cmd("normal! \\<Esc>")  -- Exit visual to set marks

    -- Set visual marks manually for headless mode
    vim.fn.setpos("'<", { 0, 7, 1, 0 })
    vim.fn.setpos("'>", { 0, 9, 1, 0 })

    -- Call the ACTUAL command (not raw RPC)
    -- This uses the real flow: explain -> create note -> event -> refresh
    commands.explain_region()

    -- Wait for AI + note creation + event-triggered refresh
    -- The event system should trigger commands.refresh() which renders extmarks
    -- AI timing: ~4-6s for short summary
    local ns = display.ns_id
    local marks_appeared = helpers.wait_for(function()
      local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
      return #marks > 0
    end, 30000)

    assert.truthy(marks_appeared, "TRUE E2E FAILED: Note should appear via event system (path canonicalization issue?)")

    -- Verify extmark content includes AI marker
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    local all_text = ""
    for _, mark in ipairs(marks) do
      local details = mark[4] or {}
      if details.virt_lines then
        for _, vline in ipairs(details.virt_lines) do
          for _, chunk in ipairs(vline) do
            all_text = all_text .. (chunk[1] or "")
          end
        end
      end
    end
    assert.truthy(string.find(all_text, "AI", 1, true), "Note should contain AI marker")

    env.cleanup()
  end)

  it("TRUE E2E: explain_region_full command creates detailed note via event system", function()
    local env = setup_ai_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Note: events may not connect in headless mode, but the fallback refresh will work

    -- Make visual selection (lines 11-14: Server impl)
    vim.api.nvim_win_set_cursor(0, { 11, 0 })
    vim.cmd("normal! V3j")  -- Visual line mode, select 4 lines
    vim.cmd("normal! \\<Esc>")

    -- Set visual marks manually for headless mode
    vim.fn.setpos("'<", { 0, 11, 1, 0 })
    vim.fn.setpos("'>", { 0, 14, 1, 0 })

    -- Call the ACTUAL detailed command
    commands.explain_region_full()

    -- Wait for AI + note creation + event-triggered refresh
    -- AI timing: ~20-30s for detailed explanation
    local ns = display.ns_id
    local marks_appeared = helpers.wait_for(function()
      local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
      return #marks > 0
    end, 60000)

    assert.truthy(marks_appeared, "TRUE E2E FAILED: Detailed note should appear via event system")

    -- Verify it's a multiline note (detailed explanations are longer)
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    local details = marks[1][4] or {}
    local virt_lines = details.virt_lines or {}
    assert.truthy(#virt_lines >= 1, "Detailed explanation should have virtual lines")

    env.cleanup()
  end)

  it("TRUE E2E: event system handles path canonicalization (/tmp vs /private/tmp)", function()
    -- This test specifically verifies the path canonicalization fix
    -- On macOS, /tmp is a symlink to /private/tmp
    -- The backend canonicalizes paths, but buffers use the original path
    local env = setup_ai_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    -- Verify we're testing with a symlinked path
    local resolved = vim.fn.resolve(env.file)
    local is_symlinked = resolved ~= env.file
    if is_symlinked then
      -- Buffer has /tmp/... but backend will send /private/tmp/...
      assert.truthy(string.find(env.file, "^/tmp/"), "Test file should be in /tmp")
      assert.truthy(string.find(resolved, "^/private/tmp/"), "Resolved path should be /private/tmp")
    end

    -- The fix in init.lua should handle this mismatch
    vim.fn.setpos("'<", { 0, 1, 1, 0 })
    vim.fn.setpos("'>", { 0, 4, 1, 0 })
    commands.explain_region()

    -- AI timing: ~4-6s for short summary
    local ns = display.ns_id
    local marks_appeared = helpers.wait_for(function()
      local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
      return #marks > 0
    end, 30000)

    if is_symlinked then
      assert.truthy(marks_appeared, "PATH CANONICALIZATION FIX FAILED: Event with /private/tmp path should match /tmp buffer")
    else
      assert.truthy(marks_appeared, "Note should appear (non-macOS or non-symlinked path)")
    end

    env.cleanup()
  end)
end)

-- Selected note highlighting E2E tests
describe("visual e2e: selected note highlighting", function()
  it("VISUAL: selected note shows with HemisNoteSelected highlight", function()
    if not vim.g.hemis_test_backend then
      pending("Requires backend")
      return
    end
    local env = setup_e2e_env()
    local display = require("hemis.display")
    local commands = require("hemis.commands")

    local connected = false
    local create_done = false
    local note_id = nil

    -- Connect and create note
    env.rpc.start(function(ok)
      if not ok then
        create_done = true
        return
      end
      connected = true

      env.rpc.request("notes/create", {
        file = env.file,
        line = 7,
        column = 0,
        text = "Note to be selected",
        projectRoot = env.dir,
      }, function(err, res)
        if res then
          note_id = res.id
        end
        create_done = true
      end)
    end)

    helpers.wait_for(function() return create_done end, 10000)
    assert.truthy(connected, "Should connect to backend")
    assert.is_not_nil(note_id, "Note should have been created")

    -- List notes and render
    local list_done = false
    local notes = nil

    env.rpc.request("notes/list-for-file", {
      file = env.file,
      projectRoot = env.dir,
    }, function(err, res)
      if not err then
        notes = helpers.unwrap_notes(res)
      end
      list_done = true
    end)

    helpers.wait_for(function() return list_done end, 5000)
    assert.is_not_nil(notes, "Should have received notes list")
    assert.truthy(#notes > 0, "Should have at least one note")

    -- Set the buffer_notes and render without selection first
    commands.buffer_notes = notes
    display.render_notes(env.buf, notes, nil)
    helpers.wait(100)

    -- Verify note renders with HemisNote (not selected)
    local ns = display.ns_id
    local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "Should have at least one extmark")
    local details = marks[1][4] or {}
    local virt_lines = details.virt_lines or {}
    local hl_before = virt_lines[1] and virt_lines[1][1] and virt_lines[1][1][2]
    assert.equals("HemisNote", hl_before, "Before selection should use HemisNote")

    -- Now set the note as selected and re-render
    commands.set_selected_note(notes[1])
    helpers.wait(100)

    -- Verify note now renders with HemisNoteSelected
    marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    assert.truthy(#marks > 0, "Should still have extmark after selection")
    details = marks[1][4] or {}
    virt_lines = details.virt_lines or {}
    local hl_after = virt_lines[1] and virt_lines[1][1] and virt_lines[1][1][2]
    assert.equals("HemisNoteSelected", hl_after, "VISUAL VERIFICATION FAILED: After selection should use HemisNoteSelected")

    -- Clear selection
    commands.set_selected_note(nil)
    helpers.wait(100)

    -- Verify back to HemisNote
    marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
    details = marks[1][4] or {}
    virt_lines = details.virt_lines or {}
    local hl_cleared = virt_lines[1] and virt_lines[1][1] and virt_lines[1][1][2]
    assert.equals("HemisNote", hl_cleared, "After clear selection should use HemisNote again")

    env.cleanup()
  end)
end)
