-- Hemis integration tests
-- Run with: nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedDirectory tests/"

local hemis = require("hemis")

describe("hemis", function()
  local test_db = vim.fn.tempname() .. ".db"
  local backend_path = vim.g.hemis_test_backend

  before_each(function()
    -- Setup with test database
    hemis.setup({
      backend = backend_path,
      backend_env = { "HEMIS_DB_PATH=" .. test_db },
      auto_refresh = false,
      keymaps = false,
    })
  end)

  after_each(function()
    -- Shutdown backend
    hemis.rpc.stop()
    -- Wait for shutdown
    vim.wait(100)
    -- Clean up test database
    os.remove(test_db)
  end)

  describe("config", function()
    it("should have defaults", function()
      assert.is_not_nil(hemis.config.defaults)
      assert.is_not_nil(hemis.config.defaults.log_level)
    end)

    it("should merge options", function()
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

      local started = hemis.rpc.start()
      assert.is_true(started)
      assert.is_true(hemis.rpc.is_running())
    end)

    it("should send request and receive response", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      hemis.rpc.start()

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
    it("should create and retrieve note", function()
      if not backend_path then
        pending("Backend not found")
        return
      end

      hemis.rpc.start()

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

      hemis.rpc.start()

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
end)
