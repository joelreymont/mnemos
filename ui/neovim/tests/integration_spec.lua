-- Integration tests for Mnemos Neovim plugin
-- Tests all features through actual backend interaction
-- Run with backend: nvim --headless -c "let g:mnemos_test_backend='/path/to/mnemos'" -c "set rtp+=." -c "lua require('tests.run')"

local helpers = require("tests.helpers")

-- Skip all backend tests if no backend configured
if not vim.g.mnemos_test_backend then
  describe("mnemos integration (SKIPPED - no backend)", function()
    it("requires g:mnemos_test_backend to be set", function()
      pending("Set g:mnemos_test_backend to run integration tests")
    end)
  end)
  return
end

-- Helper to get test environment (called inside each test)
local function get_test_env()
  local test_dir = vim.fn.tempname() .. "_mnemos_int_" .. math.random(10000)
  local test_file = test_dir .. "/test.rs"
  vim.fn.mkdir(test_dir, "p")
  local f = io.open(test_file, "w")
  if f then
    f:write("fn main() {\n    println!(\"hello\");\n}\n")
    f:close()
  end

  -- Stop any existing RPC connection before reconfiguring
  local rpc = require("mnemos.rpc")
  rpc.stop()

  local config = require("mnemos.config")
  config.setup({
    backend = vim.g.mnemos_test_backend,
    mnemos_dir = test_dir,
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
      os.remove(test_dir .. "/mnemos.lock")
      -- Delete the test directory
      vim.fn.delete(test_dir, "rf")
    end,
  }
end

describe("mnemos integration", function()
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
              list_result = helpers.unwrap_notes(res3)
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
            result = helpers.unwrap_notes(notes)
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

  describe("mnemos/status", function()
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

        env.rpc.request("mnemos/status", { projectRoot = env.dir }, function(err, res)
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

  describe("mnemos/save-snapshot and load-snapshot", function()
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
          env.rpc.request("mnemos/save-snapshot", {
            path = snapshot_path,
            projectRoot = env.dir,
          }, function(err2, res2)
            save_ok = not err2
            -- Load snapshot
            env.rpc.request("mnemos/load-snapshot", {
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

  describe("mnemos/search", function()
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
          env.rpc.request("mnemos/search", {
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
          env.rpc.request("mnemos/search", {
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

  describe("mnemos/index-project", function()
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

        env.rpc.request("mnemos/index-project", {
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

  describe("mnemos/explain-region", function()
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

        env.rpc.request("mnemos/explain-region", {
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

        env.rpc.request("mnemos/explain-region", {
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
        env.rpc.request("mnemos/explain-region", {
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
            list_result = helpers.unwrap_notes(res2)
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

describe("mnemos display integration", function()
  local buf

  before_each(function()
    buf = helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("renders notes from backend data", function()
    local display = require("mnemos.display")
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
    local display = require("mnemos.display")

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
    local display = require("mnemos.display")

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

-- Full demo workflow tests (mirrors neovim.demo)
-- These test the complete user experience with real backend
describe("demo workflow", function()
  -- Skip if no backend
  if not vim.g.mnemos_test_backend then
    return
  end

  -- Demo file with meaningful code for position/stale testing
  local DEMO_CODE = [[fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

impl Server {
    fn new(config: Config) -> Self {
        Server { config }
    }
}
]]

  local function get_demo_env()
    local test_dir = vim.fn.tempname() .. "_mnemos_demo_" .. math.random(10000)
    local test_file = test_dir .. "/app.rs"
    vim.fn.mkdir(test_dir, "p")

    local f = io.open(test_file, "w")
    if f then
      f:write(DEMO_CODE)
      f:close()
    end

    local rpc = require("mnemos.rpc")
    rpc.stop()

    local config = require("mnemos.config")
    config.setup({
      backend = vim.g.mnemos_test_backend,
      mnemos_dir = test_dir,
      auto_refresh = false,
      keymaps = false,
    })

    vim.cmd("edit " .. test_file)
    local buf = vim.api.nvim_get_current_buf()

    return {
      dir = test_dir,
      file = test_file,
      buf = buf,
      rpc = rpc,
      cleanup = function()
        rpc.stop()
        pcall(vim.api.nvim_buf_delete, buf, { force = true })
        os.remove(test_dir .. "/mnemos.lock")
        vim.fn.delete(test_dir, "rf")
      end,
    }
  end

  after_each(function()
    helpers.cleanup()
  end)

  describe("position tracking", function()
    it("note line updates when lines inserted above", function()
      local env = get_demo_env()
      local done = false
      local connect_ok = false
      local note_id = nil
      local original_line = 7  -- "fn load_config" line
      local line_after = nil

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at line 7 (fn load_config) - MUST pass content for tree-sitter anchor
        env.rpc.request("notes/create", {
          file = env.file,
          line = original_line,
          column = 0,
          text = "Note on load_config function",
          projectRoot = env.dir,
          content = DEMO_CODE,  -- Required for position tracking!
        }, function(err, res)
          if not res then done = true return end
          note_id = res.id

          -- Insert 2 comment lines at top of file
          local new_content = "// Comment 1\n// Comment 2\n" .. DEMO_CODE

          -- List with new content to get updated line position
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = new_content,  -- Required for position computation!
          }, function(err2, notes_wrapped)
            local notes = helpers.unwrap_notes(notes_wrapped)
            if notes and #notes > 0 then
              line_after = notes[1].line
            end
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)
      env.cleanup()

      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(note_id, "Note should be created")
      assert.is_not_nil(line_after, "Should have line after")
      -- Original line 7 + 2 inserted = 9
      assert.equals(9, line_after, "Note should move to line 9 after inserting 2 lines above")
    end)

    it("displays note at updated line position after lines inserted", function()
      local env = get_demo_env()
      local display = require("mnemos.display")
      local done = false
      local connect_ok = false
      local extmark_line = nil

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at line 7 (fn load_config)
        env.rpc.request("notes/create", {
          file = env.file,
          line = 7,
          column = 0,
          text = "Position display test",
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end

          -- Insert 2 comment lines at top
          local new_content = "// Comment 1\n// Comment 2\n" .. DEMO_CODE

          -- Update buffer content to match
          vim.api.nvim_buf_set_lines(env.buf, 0, -1, false, vim.split(new_content, "\n"))

          -- List with new content and render
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = new_content,
          }, function(err2, notes_wrapped)
            local notes = helpers.unwrap_notes(notes_wrapped)
            if notes then
              display.render_notes(env.buf, notes)
              helpers.wait(50)
            end
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)

      local state = helpers.capture_display_state(env.buf)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.equals(1, #state.extmarks, "Should have 1 extmark")
      -- Note moved from line 7 to line 9 (7 + 2 inserted lines)
      assert.equals(9, state.extmarks[1].line, "Extmark should be at updated line 9")
    end)
  end)

  describe("stale detection", function()
    it("marks note stale when anchor code changes", function()
      local env = get_demo_env()
      local done = false
      local connect_ok = false
      local stale_before = nil
      local stale_after = nil

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at "fn new" line (line 12) with content for anchor
        env.rpc.request("notes/create", {
          file = env.file,
          line = 12,
          column = 0,
          text = "Note on new function",
          projectRoot = env.dir,
          content = DEMO_CODE,  -- Required for stale detection!
        }, function(err, res)
          if not res then done = true return end

          -- Check initial staleness (with original content)
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = DEMO_CODE,
          }, function(err2, notes_before_wrapped)
            local notes_before = helpers.unwrap_notes(notes_before_wrapped)
            if notes_before and #notes_before > 0 then
              stale_before = notes_before[1].stale
            end

            -- Change "fn new" to "fn create" (modifies anchor code)
            local modified_content = DEMO_CODE:gsub("fn new", "fn create")

            -- Check staleness with modified content
            env.rpc.request("notes/list-for-file", {
              file = env.file,
              projectRoot = env.dir,
              content = modified_content,
            }, function(err3, notes_after_wrapped)
              local notes_after = helpers.unwrap_notes(notes_after_wrapped)
              if notes_after and #notes_after > 0 then
                stale_after = notes_after[1].stale
              end
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.is_false(stale_before, "Note should be fresh initially")
      assert.is_true(stale_after, "Note should be stale after anchor changed")
    end)

    it("displays stale note with MnemosNoteStale highlight", function()
      local env = get_demo_env()
      local display = require("mnemos.display")
      local done = false
      local connect_ok = false

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at fn new (line 12) with content
        env.rpc.request("notes/create", {
          file = env.file,
          line = 12,
          column = 0,
          text = "Stale display test",
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end

          -- Make stale by changing anchor code
          local modified_content = DEMO_CODE:gsub("fn new", "fn create")

          -- List with modified content to get stale note
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = modified_content,
          }, function(err2, notes_wrapped)
            local notes = helpers.unwrap_notes(notes_wrapped)
            if notes then
              display.render_notes(env.buf, notes)
              helpers.wait(50)
            end
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)

      local state = helpers.capture_display_state(env.buf)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.truthy(#state.extmarks > 0, "Should have extmark")

      -- Check for stale highlight
      local has_stale_hl = false
      for _, mark in ipairs(state.extmarks) do
        for _, hl in ipairs(mark.hl_groups or {}) do
          if hl:find("Stale") then
            has_stale_hl = true
            break
          end
        end
      end
      assert.truthy(has_stale_hl, "Stale note should use MnemosNoteStale highlight")
    end)

    it("reattach clears stale status", function()
      local env = get_demo_env()
      local done = false
      local connect_ok = false
      local note_id = nil
      local stale_after_reattach = nil

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at fn new (line 12) with content
        env.rpc.request("notes/create", {
          file = env.file,
          line = 12,
          column = 0,
          text = "Reattach test",
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end
          note_id = res.id

          -- Change anchor code (makes note stale)
          local modified_content = DEMO_CODE:gsub("fn new", "fn create")

          -- Reattach note to new code (with new content)
          env.rpc.request("notes/reattach", {
            id = note_id,
            file = env.file,
            line = 12,
            content = modified_content,
            projectRoot = env.dir,
          }, function(err2, res2)
            -- Check staleness after reattach
            env.rpc.request("notes/list-for-file", {
              file = env.file,
              projectRoot = env.dir,
              content = modified_content,
            }, function(err3, notes_wrapped)
              local notes = helpers.unwrap_notes(notes_wrapped)
              if notes and #notes > 0 then
                stale_after_reattach = notes[1].stale
              end
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.is_not_nil(note_id)
      assert.is_false(stale_after_reattach, "Note should be fresh after reattach")
    end)

    it("displays fresh highlight after reattach clears stale", function()
      local env = get_demo_env()
      local display = require("mnemos.display")
      local done = false
      local connect_ok = false
      local note_id = nil

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note at fn new (line 12) with content
        env.rpc.request("notes/create", {
          file = env.file,
          line = 12,
          column = 0,
          text = "Reattach display test",
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end
          note_id = res.id

          -- Change anchor code (makes note stale)
          local modified_content = DEMO_CODE:gsub("fn new", "fn create")

          -- Reattach note to new code
          env.rpc.request("notes/reattach", {
            id = note_id,
            file = env.file,
            line = 12,
            content = modified_content,
            projectRoot = env.dir,
          }, function(err2, res2)
            -- List and render after reattach
            env.rpc.request("notes/list-for-file", {
              file = env.file,
              projectRoot = env.dir,
              content = modified_content,
            }, function(err3, notes_wrapped)
              local notes = helpers.unwrap_notes(notes_wrapped)
              if notes then
                display.render_notes(env.buf, notes)
                helpers.wait(50)
              end
              done = true
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)

      local state = helpers.capture_display_state(env.buf)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.truthy(#state.extmarks > 0, "Should have extmark")

      -- Check that stale highlight is NOT present (should use MnemosNote, not MnemosNoteStale)
      local has_stale_hl = false
      for _, mark in ipairs(state.extmarks) do
        for _, hl in ipairs(mark.hl_groups or {}) do
          if hl:find("Stale") then
            has_stale_hl = true
            break
          end
        end
      end
      assert.is_false(has_stale_hl, "Reattached note should NOT use MnemosNoteStale highlight")
    end)
  end)

  describe("multiline notes", function()
    it("creates and displays multiline note", function()
      local env = get_demo_env()
      local display = require("mnemos.display")
      local done = false
      local connect_ok = false
      local multiline_text = "Config improvements:\n- Add validation\n- Support env vars"

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        env.rpc.request("notes/create", {
          file = env.file,
          line = 7,
          column = 0,
          text = multiline_text,
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end

          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = DEMO_CODE,
          }, function(err2, notes_wrapped)
            local notes = helpers.unwrap_notes(notes_wrapped)
            if notes then
              display.render_notes(env.buf, notes)
              helpers.wait(50)
            end
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)

      local state = helpers.capture_display_state(env.buf)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.truthy(#state.extmarks > 0, "Should have extmark")

      -- Verify multiline content
      local text = state.extmarks[1].text
      assert.truthy(text:find("Config improvements"), "Should show first line")
      assert.truthy(text:find("Add validation"), "Should show second line")
      assert.truthy(text:find("Support env vars"), "Should show third line")
    end)
  end)

  describe("full create-display-delete cycle", function()
    it("note appears after create and disappears after delete", function()
      local env = get_demo_env()
      local display = require("mnemos.display")
      local done = false
      local connect_ok = false
      local note_id = nil
      local extmarks_after_create = 0
      local extmarks_after_delete = 0

      env.rpc.start(function(ok)
        if not ok then done = true return end
        connect_ok = true

        -- Create note
        env.rpc.request("notes/create", {
          file = env.file,
          line = 5,
          column = 0,
          text = "Lifecycle test note",
          projectRoot = env.dir,
          content = DEMO_CODE,
        }, function(err, res)
          if not res then done = true return end
          note_id = res.id

          -- List and render
          env.rpc.request("notes/list-for-file", {
            file = env.file,
            projectRoot = env.dir,
            content = DEMO_CODE,
          }, function(err2, notes_wrapped)
            local notes = helpers.unwrap_notes(notes_wrapped)
            display.render_notes(env.buf, notes or {})
            helpers.wait(50)
            extmarks_after_create = #helpers.capture_display_state(env.buf).extmarks

            -- Delete note
            env.rpc.request("notes/delete", {
              id = note_id,
              projectRoot = env.dir,
            }, function(err3, res3)
              -- List and render again
              env.rpc.request("notes/list-for-file", {
                file = env.file,
                projectRoot = env.dir,
                content = DEMO_CODE,
              }, function(err4, notes2_wrapped)
                local notes2 = helpers.unwrap_notes(notes2_wrapped)
                display.render_notes(env.buf, notes2 or {})
                helpers.wait(50)
                extmarks_after_delete = #helpers.capture_display_state(env.buf).extmarks
                done = true
              end)
            end)
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 10000)
      env.cleanup()

      assert.truthy(connect_ok)
      assert.equals(1, extmarks_after_create, "Should have 1 extmark after create")
      assert.equals(0, extmarks_after_delete, "Should have 0 extmarks after delete")
    end)
  end)

  describe("commands.follow_link", function()
    it("parses link and gets note by id", function()
      local env = get_test_env()
      local done = false
      local connect_ok = false
      local note_id = nil
      local fetched_note = nil

      env.rpc.start(function(ok)
        if not ok then
          done = true
          return
        end
        connect_ok = true

        -- Create target note
        env.rpc.request("notes/create", {
          file = env.file,
          line = 2,
          column = 4,
          text = "Target note for follow",
          projectRoot = env.dir,
        }, function(err, res)
          if not res then
            done = true
            return
          end
          note_id = res.id

          -- Now fetch the note by ID (what follow_link does internally)
          env.rpc.request("notes/get", {
            id = note_id,
          }, function(err2, note)
            fetched_note = note
            done = true
          end)
        end)
      end)

      helpers.wait_for(function() return done end, 5000)
      env.cleanup()

      assert.truthy(connect_ok, "Backend connection failed")
      assert.is_not_nil(note_id, "Note should be created")
      assert.is_not_nil(fetched_note, "Should fetch note by ID")
      assert.equals(note_id, fetched_note.id)
      assert.equals(env.file, fetched_note.file)
      assert.equals(2, fetched_note.line)
    end)
  end)
end)
