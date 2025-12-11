-- Hemis: A Second Brain for Your Code
-- Development configuration for LazyVim
-- Symlink: ln -sf /Users/joel/Work/hemis/ui/neovim/lazy-config.lua ~/.config/nvim/lua/plugins/hemis.lua
--
-- Keymaps are registered by the plugin (keymaps = true), no need to duplicate here.
-- which-key picks up the descriptions from vim.keymap.set's desc field.

return {
  {
    dir = "/Users/joel/Work/hemis/ui/neovim",
    opts = {
      backend = "/Users/joel/Work/hemis/target/debug/hemis",
      backend_env = {
        HEMIS_DB_PATH = "/Users/joel/Work/hemis/hemis-notes.db",
      },
      auto_refresh = true,
      keymaps = true,
      keymap_prefix = "<leader>h",
      display_style = "full",
      log_level = "info",
    },
  },
}
