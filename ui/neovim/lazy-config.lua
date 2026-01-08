-- Mnemos: A Second Brain for Your Code
-- Development configuration for LazyVim
-- Symlink: ln -sf /Users/joel/Work/mnemos/ui/neovim/lazy-config.lua ~/.config/nvim/lua/plugins/mnemos.lua
--
-- Keymaps are registered by the plugin (keymaps = true), no need to duplicate here.
-- which-key picks up the descriptions from vim.keymap.set's desc field.

return {
  {
    dir = "/Users/joel/Work/mnemos/ui/neovim",
    opts = {
      backend = "/Users/joel/Work/mnemos/zig-out/bin/mnemos",
      backend_env = {
        MNEMOS_DB_PATH = "/Users/joel/Work/mnemos/mnemos-notes.db",
      },
      auto_refresh = true,
      keymaps = true,
      keymap_prefix = "<leader>m",
      display_style = "full",
      log_level = "info",
    },
  },
}
