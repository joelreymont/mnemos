-- Integration tests for Hemis Neovim plugin
-- Tests all features through actual backend interaction
-- Run with backend: nvim --headless -c "let g:hemis_test_backend='/path/to/hemis'" -c "set rtp+=." -c "lua require('tests.run')"

local helpers = require("tests.helpers")

-- Skip all backend tests if no backend configured
if not vim.g.hemis_test_backend then
  describe("hemis integration (SKIPPED - no backend)", function()
    it("requires g:hemis_test_backend to be set", function()
      pending("Set g:hemis_test_backend to run integration tests")
    end)
  end)
  return
end

-- Helper to get test environment (called inside each test)
local function get_test_env()
  local test_dir = vim.fn.tempname() .. "_hemis_int_" .. math.random(10000)
  local test_file = test_dir .. "/test.rs"
  vim.fn.mkdir(test_dir, "p")
  local f = io.open(test_file, "w")
  if f then
    f:write("fn main() {\n    println!(\"hello\");\n}\n")
    f:close()
  end

  -- Stop any existing RPC connection before reconfiguring
  local rpc = require("hemis.rpc")
  rpc.stop()

  local config = require("hemis.config")
  config.setup({
    backend = vim.g.hemis_test_backend,
    hemis_dir = test_dir,
    auto_refresh = false,
    keymaps = false,
  })

  return {
    dir = test_dir,
    file = test_file,
    rpc = rpc,
    cleanup = function()
      -- Stop the RPC connection first
      rpc.stop()
      -- Remove any lock file
      os.remove(test_dir .. "/hemis.lock")
      -- Delete the test directory
      vim.fn.delete(test_dir, "rf")
    end,
  }
end

describe("hemis integration", function()
  after_each(function()
    helpers.cleanup()
  end)

  describe("notes/create", function()
    it("creates a note and returns id", function()
      local env = get_test_env()
      local done = false
      local result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Test note content",
          projectRoot = env.dir,
        }, function(err, res)
          result = res
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(result, "Should return result")
      assert.is_not_nil(result.id, "Should have note id")
    end)
  end)

  describe("notes/list-for-file", function()
    it("lists notes for a file", function()
      local env = get_test_env()
      local done = false
      local result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- First create a note
        env.rpc.request("notes/create", {
          file = env.file,
          line = 2,
          column = 4,
          text = "Note for listing",
          projectRoot = env.dir,
        }, function(err, res)
          -- Then list notes
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
          }, function(err2, notes)
            result = notes
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(result, "Should return notes")
      assert.is_table(result)
      assert.truthy(#result >= 1, "Should have at least one note")
    end)
  end)

  describe("notes/update", function()
    it("updates note text", function()
      local env = get_test_env()
      local done = false
      local updated = nil
      local connect_ok = false
      local create_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create note first
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Original text",
          projectRoot = env.dir,
        }, function(err, res)
          if not res then
            done = true
            return
          end
          create_ok = true
          -- Update it
          env.rpc.request("notes/update", {
            id = res.id,
            text = "Updated text",
          }, function(err2, res2)
            updated = res2
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.truthy(create_ok, "Note creation failed")
      assert.is_not_nil(updated, "Should return updated note")
      assert.equals("Updated text", updated.text)
    end)
  end)

  describe("notes/delete", function()
    it("deletes a note", function()
      local env = get_test_env()
      local done = false
      local delete_ok = false
      local connect_ok = false
      local create_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create note first
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "To be deleted",
          projectRoot = env.dir,
        }, function(err, res)
          if not res then
            done = true
            return
          end
          create_ok = true
          -- Delete it
          env.rpc.request("notes/delete", { id = res.id }, function(err2, res2)
            delete_ok = not err2
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.truthy(create_ok, "Note creation failed")
      assert.is_true(delete_ok, "Delete should succeed")
    end)
  end)

  describe("notes/backlinks", function()
    it("finds notes linking to target", function()
      local env = get_test_env()
      local done = false
      local backlinks = nil
      local connect_ok = false
      local target_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create target note
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Target note",
          projectRoot = env.dir,
        }, function(err, target)
          if not target then
            done = true
            return
          end
          target_ok = true
          -- Create linking note (use full UUID for proper link detection)
          env.rpc.request("notes/create", {
            file = env.file,
            line = 2,
            column = 0,
            text = "Links to [[target][" .. target.id .. "]]",
            projectRoot = env.dir,
          }, function(err2, linking)
            -- Get backlinks
            env.rpc.request("notes/backlinks", {
              id = target.id,
              projectRoot = env.dir,
            }, function(err3, res)
              backlinks = res
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.truthy(target_ok, "Target note creation failed")
      assert.is_not_nil(backlinks, "Should return backlinks")
      assert.is_table(backlinks)
    end)
  end)

  describe("hemis/status", function()
    it("returns note and file counts", function()
      local env = get_test_env()
      local done = false
      local status = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("hemis/status", { projectRoot = env.dir }, function(err, res)
          status = res
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(status, "Should return status")
      assert.is_not_nil(status.counts, "Should have counts")
    end)
  end)

  describe("hemis/list-files", function()
    it("lists project files", function()
      local env = get_test_env()
      local done = false
      local files = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("hemis/list-files", { projectRoot = env.dir }, function(err, res)
          files = res
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(files, "Should return files")
      assert.is_table(files)
    end)
  end)

  describe("hemis/save-snapshot and load-snapshot", function()
    it("saves and loads snapshots", function()
      local env = get_test_env()
      local done = false
      local save_ok = false
      local load_ok = false
      local connect_ok = false
      local snapshot_path = env.dir .. "/snapshot.json"

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create a note first
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Snapshot test note",
          projectRoot = env.dir,
        }, function(err, res)
          -- Save snapshot
          env.rpc.request("hemis/save-snapshot", {
            path = snapshot_path,
            projectRoot = env.dir,
          }, function(err2, res2)
            save_ok = not err2
            -- Load snapshot
            env.rpc.request("hemis/load-snapshot", {
              path = snapshot_path,
            }, function(err3, res3)
              load_ok = not err3
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_true(save_ok, "Save should succeed")
      assert.is_true(load_ok, "Load should succeed")
    end)
  end)

  describe("hemis/search", function()
    it("searches notes and files", function()
      local env = get_test_env()
      local done = false
      local results = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create a note first
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Searchable note content",
          projectRoot = env.dir,
        }, function(err, res)
          -- Search for it
          env.rpc.request("hemis/search", {
            query = "Searchable",
            projectRoot = env.dir,
          }, function(err2, res2)
            results = res2
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(results, "Should return results")
      assert.is_table(results)
    end)
  end)

  describe("index/add-file", function()
    it("indexes a file for search", function()
      local env = get_test_env()
      local done = false
      local index_ok = false
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("index/add-file", {
          file = env.file,
          projectRoot = env.dir,
        }, function(err, res)
          index_ok = not err
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_true(index_ok, "Index file should succeed")
    end)
  end)

  describe("hemis/index-project", function()
    it("indexes entire project", function()
      local env = get_test_env()
      local done = false
      local result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("hemis/index-project", {
          projectRoot = env.dir,
        }, function(err, res)
          result = res
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(result, "Should return result")
      assert.is_not_nil(result.indexed, "Should have indexed count")
    end)
  end)

  describe("hemis/explain-region", function()
    it("returns code snippet for LLM context", function()
      local env = get_test_env()
      local done = false
      local result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("hemis/explain-region", {
          file = env.file,
          startLine = 1,
          endLine = 3,
          projectRoot = env.dir,
        }, function(err, res)
          result = res
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(result, "Should return result")
      -- Should contain the code content
      assert.is_not_nil(result.content or result.explanation, "Should have content")
    end)
  end)
end)

describe("hemis display integration", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("renders notes from backend data", function()
    local display = require("hemis.display")
    -- Simulate backend response
    local notes = {
      { id = "int-001", line = 1, column = 0, text = "Integration test note" },
    }

    display.render_notes(buf, notes)
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(1, #state.extmarks)
    helpers.assert_extmark_at_line(state, 1, "Integration test note")
  end)

  it("updates display when notes change", function()
    local display = require("hemis.display")

    -- Initial notes
    display.render_notes(buf, {
      { id = "1", line = 1, column = 0, text = "First version" },
    })
    helpers.wait()

    -- Updated notes (simulating refresh)
    display.render_notes(buf, {
      { id = "1", line = 1, column = 0, text = "Updated version" },
    })
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(1, #state.extmarks)
    helpers.assert_extmark_at_line(state, 1, "Updated version")
  end)

  it("clears display when no notes", function()
    local display = require("hemis.display")

    -- Add notes first
    display.render_notes(buf, {
      { id = "1", line = 1, column = 0, text = "Temporary note" },
    })
    helpers.wait()

    -- Clear
    display.render_notes(buf, {})
    helpers.wait()

    local state = helpers.capture_display_state(buf)
    assert.equals(0, #state.extmarks)
  end)
end)
