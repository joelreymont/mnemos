-- Hemis integration tests
-- Run with: nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedDirectory tests/"
--
-- Tests reuse a single backend server for efficiency.
-- Uses an isolated hemis_dir to avoid conflicting with user's Hemis.

local hemis = require("hemis")

-- Shared test state
local test_dir = vim.fn.tempname() .. "_hemis_test"
local test_db = test_dir .. "/hemis.db"
local backend_path = vim.g.hemis_test_backend
local backend_started = false
local setup_done = false

-- Start backend once for all tests (lazy initialization)
local function ensure_backend()
  if backend_started then
    return true
  end

  if not backend_path then
    return false
  end

  if not setup_done then
    -- Create isolated test directory
    vim.fn.mkdir(test_dir, "p")

    hemis.setup({
      backend = backend_path,
      hemis_dir = test_dir,
      backend_env = { HEMIS_DB_PATH = test_db },
      auto_refresh = false,
      keymaps = false,
    })
    setup_done = true
  end

  -- Start connects to socket (starting server if needed)
  local done = false
  local ok = false
  hemis.rpc.start(function(success)
    ok = success
    done = true
  end)

  vim.wait(5000, function()
    return done
  end)

  if ok then
    backend_started = true
  end
  return ok
end

-- Clean up notes between tests (without restarting backend)
local function clear_notes()
  if not backend_started then
    return
  end

  -- List all notes and delete them
  local done = false
  hemis.rpc.request("notes/list", { file = "", includeStale = true }, function(err, notes)
    if not err and notes then
      for _, note in ipairs(notes) do
        hemis.rpc.request("notes/delete", { id = note.id }, function() end)
      end
    end
    done = true
  end)
  vim.wait(1000, function() return done end)
end

describe("hemis", function()
  describe("config", function()
    it("should have defaults", function()
      assert.is_not_nil(hemis.config.defaults)
      assert.is_not_nil(hemis.config.defaults.log_level)
    end)

    it("should merge options", function()
      -- Ensure setup is done
      ensure_backend()
      hemis.config.setup({ log_level = "debug" })
      assert.equals("debug", hemis.config.get("log_level"))
    end)
  end)

  describe("rpc", function()
    it("should start backend", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      local ok = ensure_backend()
      assert.is_true(ok, "Backend should start")
      assert.is_true(hemis.rpc.is_running())
    end)

    it("should send request and receive response", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      ensure_backend()

      local done = false
      local result = nil
      local err = nil

      hemis.rpc.request("hemis/status", {}, function(e, r)
        err = e
        result = r
        done = true
      end)

      vim.wait(2000, function()
        return done
      end)

      assert.is_true(done, "Request should complete")
      assert.is_nil(err)
      assert.is_not_nil(result)
      assert.is_not_nil(result.counts)
      assert.is_number(result.counts.notes)
    end)
  end)

  describe("notes", function()
    before_each(function()
      -- Ensure backend is running and clear notes for isolation
      ensure_backend()
      clear_notes()
    end)

    it("should create and retrieve note", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      -- Create a temp file
      local test_file = vim.fn.tempname() .. ".rs"
      vim.fn.writefile({ "fn main() {}", "    println!(\"hello\");", "}" }, test_file)

      vim.cmd("edit " .. test_file)
      vim.api.nvim_win_set_cursor(0, { 2, 4 })

      local created = nil
      local create_done = false

      hemis.notes.create("Test note from Neovim", {}, function(err, result)
        assert.is_nil(err)
        created = result
        create_done = true
      end)

      vim.wait(2000, function()
        return create_done
      end)

      assert.is_true(create_done, "Create should complete")
      assert.is_not_nil(created)
      assert.is_not_nil(created.id)
      assert.equals("Test note from Neovim", created.text)

      -- Fetch it back
      local fetched = nil
      local fetch_done = false

      hemis.notes.get(created.id, function(err, result)
        assert.is_nil(err)
        fetched = result
        fetch_done = true
      end)

      vim.wait(2000, function()
        return fetch_done
      end)

      assert.is_true(fetch_done, "Fetch should complete")
      assert.is_not_nil(fetched)
      assert.equals(created.id, fetched.id)

      -- Cleanup
      vim.cmd("bdelete!")
      os.remove(test_file)
    end)

    it("should list notes for buffer", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      local test_file = vim.fn.tempname() .. ".rs"
      vim.fn.writefile({ "fn test() {}" }, test_file)
      vim.cmd("edit " .. test_file)

      -- Create a note
      local create_done = false
      hemis.notes.create("List test note", {}, function()
        create_done = true
      end)
      vim.wait(2000, function()
        return create_done
      end)

      -- List notes
      local notes_list = nil
      local list_done = false

      hemis.notes.list_for_buffer(function(err, result)
        assert.is_nil(err)
        notes_list = result
        list_done = true
      end)

      vim.wait(2000, function()
        return list_done
      end)

      assert.is_true(list_done, "List should complete")
      assert.is_not_nil(notes_list)
      assert.is_true(#notes_list >= 1, "Should have at least one note")

      vim.cmd("bdelete!")
      os.remove(test_file)
    end)
  end)

  describe("treesitter", function()
    it("should check availability", function()
      local available = hemis.treesitter.is_available()
      -- May or may not be available depending on parser
      assert.is_boolean(available)
    end)

    it("should get anchor position", function()
      local test_file = vim.fn.tempname() .. ".lua"
      vim.fn.writefile({ "local x = 1", "print(x)" }, test_file)
      vim.cmd("edit " .. test_file)
      vim.api.nvim_win_set_cursor(0, { 1, 6 })

      local anchor = hemis.treesitter.get_anchor_position()
      assert.is_not_nil(anchor)
      assert.is_number(anchor.line)
      assert.is_number(anchor.column)

      vim.cmd("bdelete!")
      os.remove(test_file)
    end)
  end)

  describe("backlinks", function()
    before_each(function()
      ensure_backend()
      clear_notes()
    end)

    it("should return notes that link to target", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      local test_file = vim.fn.tempname() .. ".rs"
      vim.fn.writefile({ "fn main() {}" }, test_file)
      vim.cmd("edit " .. test_file)

      -- Create target note A
      local note_a = nil
      local create_a_done = false
      hemis.notes.create("Note A - target", {}, function(err, result)
        assert.is_nil(err)
        note_a = result
        create_a_done = true
      end)
      vim.wait(2000, function() return create_a_done end)
      assert.is_not_nil(note_a)

      -- Create note B that links to A
      local note_b = nil
      local create_b_done = false
      hemis.notes.create(string.format("Note B links to [[target][%s]]", note_a.id), {}, function(err, result)
        assert.is_nil(err)
        note_b = result
        create_b_done = true
      end)
      vim.wait(2000, function() return create_b_done end)
      assert.is_not_nil(note_b)

      -- Query backlinks for A
      local backlinks = nil
      local backlinks_done = false
      hemis.notes.backlinks(note_a.id, function(err, result)
        assert.is_nil(err)
        backlinks = result
        backlinks_done = true
      end)
      vim.wait(2000, function() return backlinks_done end)

      assert.is_true(backlinks_done, "Backlinks should complete")
      assert.is_not_nil(backlinks)
      assert.equals(1, #backlinks, "Should have one backlink")
      assert.equals(note_b.id, backlinks[1].id, "Backlink should be note B")

      vim.cmd("bdelete!")
      os.remove(test_file)
    end)
  end)

  describe("display", function()
    it("should format comment prefix", function()
      -- Create a Rust buffer
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_set_current_buf(buf)
      vim.bo[buf].filetype = "rust"

      -- The display module should use // for Rust
      -- (Internal function, test indirectly via render)
      hemis.display.clear(buf)
      -- Should not error
      assert.is_true(true)

      vim.api.nvim_buf_delete(buf, { force = true })
    end)

    it("should render notes as virtual lines", function()
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_set_current_buf(buf)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "line 1", "line 2", "line 3" })
      vim.bo[buf].filetype = "lua"

      local notes = {
        { id = "abc123", line = 2, column = 0, text = "Test note" },
      }

      hemis.display.render_notes(buf, notes)

      -- Check extmarks were created
      local marks = vim.api.nvim_buf_get_extmarks(buf, hemis.display.ns_id, 0, -1, {})
      assert.is_true(#marks >= 1, "Should have at least one extmark")

      hemis.display.clear(buf)
      marks = vim.api.nvim_buf_get_extmarks(buf, hemis.display.ns_id, 0, -1, {})
      assert.equals(0, #marks, "Should have no extmarks after clear")

      vim.api.nvim_buf_delete(buf, { force = true })
    end)
  end)

  -- Final cleanup (runs after the last test due to test order)
  describe("cleanup", function()
    it("should shutdown backend", function()
      if backend_started then
        -- Send shutdown to server
        local done = false
        hemis.rpc.request("shutdown", {}, function()
          done = true
        end)
        vim.wait(1000, function()
          return done
        end)

        -- Disconnect
        hemis.rpc.stop()
        vim.wait(200)
      end

      -- Clean up test directory
      vim.fn.delete(test_dir, "rf")

      -- Always pass - this is just cleanup
      assert.is_true(true)
    end)
  end)
end)
