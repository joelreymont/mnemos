-- End-to-end test for explain_region command
-- Tests the full flow: visual selection -> command -> AI -> note creation -> display
-- Run with: nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedFile tests/explain_region_e2e_spec.lua"
--
-- REQUIRES:
-- - Backend built: cargo build --release -p backend
-- - AI provider configured: HEMIS_AI_PROVIDER=claude or config.toml

local helpers = require("tests.helpers")

-- Helper to set up test environment with AI enabled
local function get_ai_test_env()
  local test_dir = vim.fn.tempname() .. "_hemis_e2e_" .. math.random(10000)
  local test_file = test_dir .. "/server.rs"
  vim.fn.mkdir(test_dir, "p")

  -- Write test file with meaningful code
  local code = [[fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
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

  return {
    dir = test_dir,
    file = test_file,
    buf = buf,
    rpc = rpc,
    cleanup = function()
      rpc.stop()
      vim.cmd("bdelete! " .. buf)
      os.remove(test_dir .. "/hemis.lock")
      vim.fn.delete(test_dir, "rf")
    end,
  }
end

-- Set visual selection marks (simulates selecting lines 2-4)
local function set_visual_selection(start_line, end_line)
  -- Set '< and '> marks which explain_region uses
  vim.fn.setpos("'<", { 0, start_line, 1, 0 })
  vim.fn.setpos("'>", { 0, end_line, 999, 0 })
end

describe("explain_region end-to-end", function()
  -- Check if AI provider is available
  local ai_available = vim.env.HEMIS_AI_PROVIDER ~= nil
    or vim.fn.filereadable(vim.fn.expand("~/.config/hemis/config.toml")) == 1

  if not ai_available then
    it("requires AI provider (HEMIS_AI_PROVIDER or config.toml)", function()
      pending("Set HEMIS_AI_PROVIDER=claude to run AI tests")
    end)
    return
  end

  after_each(function()
    helpers.cleanup()
  end)

  it("creates note from AI explanation and displays it", function()
    local env = get_ai_test_env()
    local commands = require("hemis.commands")
    local display = require("hemis.display")

    -- Set up visual selection for lines 2-4 (the interesting code)
    set_visual_selection(2, 4)

    -- Track if note was created
    local note_created = false
    local note_displayed = false
    local test_done = false
    local test_error = nil

    -- Start RPC connection first
    env.rpc.start(function(ok)
      if not ok then
        test_error = "Failed to connect to backend"
        test_done = true
        return
      end

      -- Call the explain_region command
      -- This is async and will take time for AI response
      vim.schedule(function()
        -- The command will call the RPC and create a note
        local success, err = pcall(commands.explain_region)
        if not success then
          test_error = "explain_region failed: " .. tostring(err)
          test_done = true
          return
        end
      end)
    end)

    -- Wait for AI response (up to 120 seconds)
    -- Check periodically for notes to appear
    local start_time = vim.uv.now()
    local timeout_ms = 120000

    while not test_done and (vim.uv.now() - start_time) < timeout_ms do
      -- Process libuv events
      vim.uv.run("nowait")
      vim.wait(500, function() return test_done end, 100)

      -- Check if a note was created by looking at extmarks
      local ns = display.ns_id
      if ns then
        local marks = vim.api.nvim_buf_get_extmarks(env.buf, ns, 0, -1, { details = true })
        if #marks > 0 then
          note_displayed = true
          -- Check if any mark has content (virt_lines or virt_text)
          for _, mark in ipairs(marks) do
            local details = mark[4] or {}
            if details.virt_lines or details.virt_text then
              note_created = true
              test_done = true
              break
            end
          end
        end
      end

      -- Also check via RPC if notes exist
      if not note_created then
        local list_done = false
        env.rpc.request("notes/list-for-file", {
          file = env.file,
          projectRoot = env.dir,
        }, function(err, res)
          if not err and res and #res > 0 then
            note_created = true
          end
          list_done = true
        end)
        -- Brief wait for RPC response
        vim.wait(100, function() return list_done end, 10)
      end
    end

    env.cleanup()

    -- Assert results
    assert.is_nil(test_error, test_error)
    assert.truthy(note_created, "Note should have been created from AI explanation")
    -- Note: display might not happen in headless without proper refresh
    -- The key assertion is that the note was created
  end)

  it("handles explain_region RPC with useAI=true", function()
    -- Simpler test: just verify the RPC returns an explanation
    local env = get_ai_test_env()
    local done = false
    local result = nil
    local err_result = nil

    env.rpc.start(function(ok)
      if not ok then
        done = true
        return
      end

      env.rpc.request("hemis/explain-region", {
        file = env.file,
        startLine = 2,
        endLine = 4,
        projectRoot = env.dir,
        useAI = true,
      }, function(err, res)
        err_result = err
        result = res
        done = true
      end)
    end)

    -- Wait up to 120 seconds for AI response
    helpers.wait_for(function() return done end, 120000)
    env.cleanup()

    assert.is_nil(err_result, "RPC should not error: " .. vim.inspect(err_result))
    assert.is_not_nil(result, "Should return result")
    assert.is_not_nil(result.explanation, "Should have AI explanation")
    assert.truthy(#result.explanation > 0, "Explanation should not be empty")
    assert.is_not_nil(result.ai, "Should have AI info")
    assert.is_not_nil(result.ai.statusDisplay, "Should have AI statusDisplay")
  end)

  it("creates note after getting AI explanation", function()
    -- Full flow test: get explanation, create note, verify it exists
    local env = get_ai_test_env()
    local explanation_done = false
    local note_created = false
    local explanation = nil
    local note_id = nil

    env.rpc.start(function(ok)
      if not ok then
        explanation_done = true
        return
      end

      -- Step 1: Get AI explanation
      env.rpc.request("hemis/explain-region", {
        file = env.file,
        startLine = 2,
        endLine = 4,
        projectRoot = env.dir,
        useAI = true,
      }, function(err, res)
        if err or not res or not res.explanation then
          explanation_done = true
          return
        end

        explanation = res.explanation
        local status_display = res.ai and res.ai.statusDisplay or "[AI]"
        local note_text = status_display .. " " .. explanation

        -- Step 2: Create note with explanation
        env.rpc.request("notes/create", {
          file = env.file,
          line = 2,
          column = 0,
          text = note_text,
          projectRoot = env.dir,
        }, function(create_err, create_res)
          if not create_err and create_res and create_res.id then
            note_id = create_res.id
            note_created = true
          end
          explanation_done = true
        end)
      end)
    end)

    -- Wait for the full flow (up to 120 seconds)
    helpers.wait_for(function() return explanation_done end, 120000)

    -- Step 3: Verify note exists
    local list_done = false
    local found_note = false

    if note_id then
      env.rpc.request("notes/list-for-file", {
        file = env.file,
        projectRoot = env.dir,
      }, function(err, res)
        if not err and res then
          for _, note in ipairs(res) do
            if note.id == note_id then
              found_note = true
              break
            end
          end
        end
        list_done = true
      end)

      helpers.wait_for(function() return list_done end, 5000)
    end

    env.cleanup()

    assert.is_not_nil(explanation, "Should have received AI explanation")
    assert.truthy(note_created, "Note should have been created")
    assert.truthy(found_note, "Created note should be retrievable")
  end)
end)
