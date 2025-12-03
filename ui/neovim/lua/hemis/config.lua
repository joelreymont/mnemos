-- Configuration module for Hemis
local M = {}

M.defaults = {
  -- Path to hemis backend binary (auto-detected if nil)
  backend = nil,

  -- Path to hemis config file (defaults to ~/.config/hemis/config.toml)
  -- The config file sets db-path and ai-provider
  config_path = nil,

  -- Hemis directory for socket/lock files (defaults to ~/.hemis)
  hemis_dir = nil,

  -- Auto-refresh notes on BufEnter
  auto_refresh = true,

  -- Log level: "debug", "info", "warn", "error"
  log_level = "warn",

  -- Keymap prefix
  keymap_prefix = "<leader>h",

  -- Enable keymaps
  keymaps = true,

  -- Note display style: "full" (comment block) or "minimal" (single line indicator)
  display_style = "full",

  -- Real-time position tracking: update note positions as you type (debounced)
  -- Set to false to disable (reduces RPC traffic during editing)
  realtime_tracking = true,
}

M.options = {}

function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", {}, M.defaults, opts or {})

  -- Auto-detect backend path if not specified
  if not M.options.backend then
    local plugin_dir = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
    local candidates = {
      plugin_dir .. "../../../../target/release/hemis",
      plugin_dir .. "../../../../target/debug/hemis",
    }
    for _, path in ipairs(candidates) do
      local expanded = vim.fn.expand(path)
      if vim.fn.executable(expanded) == 1 then
        M.options.backend = expanded
        break
      end
    end
  end
end

function M.get(key)
  return M.options[key]
end

return M
