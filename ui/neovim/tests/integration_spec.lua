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

    it("creates note at specified line and returns correct line", function()
      -- Verify note is created at the exact line specified, not line 1
      local env = get_test_env()
      local done = false
      local result = nil
      local connect_ok = false
      local target_line = 5

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        env.rpc.request("notes/create", {
          file = env.file,
          line = target_line,
          column = 0,
          text = "Note at line 5",
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
      assert.equals(target_line, result.line, "Note should be at line " .. target_line)
    end)

    it("lists notes with correct line positions", function()
      -- Create notes at different lines and verify list returns correct lines
      local env = get_test_env()
      local done = false
      local list_result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create note at line 2
        env.rpc.request("notes/create", {
          file = env.file,
          line = 2,
          column = 0,
          text = "Note at line 2",
          projectRoot = env.dir,
        }, function(err1, res1)
          -- Create note at line 5
          env.rpc.request("notes/create", {
            file = env.file,
            line = 5,
            column = 0,
            text = "Note at line 5",
            projectRoot = env.dir,
          }, function(err2, res2)
            -- List notes
            env.rpc.request("notes/list-for-file", {
              file = env.file,
              projectRoot = env.dir,
            }, function(err3, res3)
              list_result = res3
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(list_result, "Should return list")
      assert.equals(2, #list_result, "Should have 2 notes")

      -- Find notes by line
      local lines = {}
      for _, note in ipairs(list_result) do
        lines[note.line] = note.text
      end
      assert.equals("Note at line 2", lines[2], "Line 2 note should exist")
      assert.equals("Note at line 5", lines[5], "Line 5 note should exist")
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

    it("finds backlinks after update adds link", function()
      -- Test update path: create A, create B (no link), update B to link to A, check backlinks
      local env = get_test_env()
      local done = false
      local backlinks = nil
      local connect_ok = false
      local target_id = nil
      local linking_id = nil

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create target note A
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Target note A",
          projectRoot = env.dir,
        }, function(err, target)
          if not target then
            done = true
            return
          end
          target_id = target.id

          -- Create linking note B (no link yet)
          env.rpc.request("notes/create", {
            file = env.file,
            line = 2,
            column = 0,
            text = "Note B - no link",
            projectRoot = env.dir,
          }, function(err2, linking)
            if not linking then
              done = true
              return
            end
            linking_id = linking.id

            -- Update B to add link to A
            env.rpc.request("notes/update", {
              id = linking.id,
              text = "Note B - see [[A][" .. target.id .. "]]",
            }, function(err3, updated)
              -- Get backlinks for A
              env.rpc.request("notes/backlinks", {
                id = target.id,
              }, function(err4, res)
                backlinks = res
                done = true
              end)
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(target_id, "Target note should be created")
      assert.is_not_nil(linking_id, "Linking note should be created")
      assert.is_not_nil(backlinks, "Should return backlinks")
      assert.is_table(backlinks)
      assert.equals(1, #backlinks, "Should have exactly one backlink after update")
      assert.equals(linking_id, backlinks[1].id, "Backlink should be the linking note")
    end)
  end)

  describe("notes/reattach", function()
    it("reattaches note to new position", function()
      local env = get_test_env()
      local done = false
      local reattached = nil
      local connect_ok = false
      local create_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create note at line 1
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Note to reattach",
          projectRoot = env.dir,
        }, function(err, res)
          if not res then
            done = true
            return
          end
          create_ok = true
          -- Reattach to line 3, column 4
          env.rpc.request("notes/reattach", {
            id = res.id,
            file = env.file,
            line = 3,
            column = 4,
            nodePath = {"fn", "main"},
          }, function(err2, res2)
            reattached = res2
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.truthy(create_ok, "Note creation failed")
      assert.is_not_nil(reattached, "Should return reattached note")
      assert.equals(3, reattached.line, "Line should be updated to 3")
      assert.equals(4, reattached.column, "Column should be updated to 4")
      assert.is_false(reattached.stale, "Reattached note should not be stale")
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

    it("searches case-insensitively", function()
      -- Searching "config" should find "Config struct improvements"
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

        -- Create a note with uppercase text
        env.rpc.request("notes/create", {
          file = env.file,
          line = 1,
          column = 0,
          text = "Config struct improvements",
          projectRoot = env.dir,
        }, function(err, res)
          -- Search with lowercase
          env.rpc.request("hemis/search", {
            query = "config",
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
      assert.truthy(#results >= 1, "Should find note with lowercase search")
      assert.truthy(results[1].text:find("Config"), "Should find the Config note")
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

    it("returns correct line range content", function()
      -- Verify explain-region extracts the exact lines requested
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
          startLine = 2,
          endLine = 2,
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
      assert.is_not_nil(result.content, "Should have content")
      -- Line 2 of test file is '    println!("hello");'
      assert.truthy(result.content:match("println"), "Content should contain line 2")
      -- Should NOT contain 'fn main' which is line 1
      assert.falsy(result.content:match("fn main"), "Content should not contain line 1")
    end)
  end)

  describe("explain-region buffer preservation", function()
    -- Test that explain-region command does not modify buffer content
    it("preserves buffer content after explain-region RPC", function()
      local env = get_test_env()
      local done = false
      local connect_ok = false

      -- Read original file content
      local original_content = nil
      local f = io.open(env.file, "r")
      if f then
        original_content = f:read("*all")
        f:close()
      end
      assert.is_not_nil(original_content, "Should read original file")

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Call explain-region
        env.rpc.request("hemis/explain-region", {
          file = env.file,
          startLine = 1,
          endLine = 3,
          projectRoot = env.dir,
        }, function(err, res)
          done = true
        end)
      end)

      helpers.wait_for(function() return done end, 5000)

      -- Verify file content unchanged
      local after_content = nil
      f = io.open(env.file, "r")
      if f then
        after_content = f:read("*all")
        f:close()
      end

      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.equals(original_content, after_content, "File content should be unchanged after explain-region")
    end)
  end)

  describe("note creation verification", function()
    -- Test that notes are actually persisted in the database
    it("note exists in database after create", function()
      local env = get_test_env()
      local done = false
      local create_result = nil
      local list_result = nil
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create a note
        env.rpc.request("notes/create", {
          file = env.file,
          line = 2,
          column = 0,
          text = "Persisted note test",
          projectRoot = env.dir,
        }, function(err, res)
          create_result = res
          -- Now list notes to verify it was persisted
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
          }, function(err2, res2)
            list_result = res2
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()
      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(create_result, "Should return create result")
      assert.is_not_nil(create_result.id, "Should have note id")
      assert.is_not_nil(list_result, "Should return list result")
      assert.truthy(#list_result > 0, "Should have at least one note")
      -- Find our note in the list
      local found = false
      for _, note in ipairs(list_result) do
        if note.id == create_result.id then
          found = true
          assert.equals("Persisted note test", note.text, "Note text should match")
          assert.equals(2, note.line, "Note line should match")
        end
      end
      assert.truthy(found, "Created note should be in list")
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
