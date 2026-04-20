-- ~/.config/nvim/init.lua  — v2.3.12 entry point
-- Load order: bootstrap → options → autocmds → keymaps → plugins → theme
--
-- FIX (v2.3.12):
--   • vim.g.nvim_ide_version moved to core/bootstrap.lua (step 1).
--     It was set here at step 10, AFTER require("plugins") at step 6.
--     ui.lua reads the version during plugin config() which runs at step 6,
--     so it always saw nil and fell back to the hardcoded "2.3.5" string.
--     bootstrap.lua runs before any plugin; the version is now always correct.

-- 1. Bootstrap lazy.nvim + set leader + version stamp (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands — loaded synchronously so BufReadPre/BufRead fire for the
--    first file opened via CLI (e.g. `nvim myfile.lua`).
require("core.autocmds")

-- 4. Global keymaps
require("core.keymaps")

-- 5. User commands
require("core.commands")

-- 6. Plugin manager + all plugin specs
local ok, err = pcall(require, "plugins")
if not ok then
  vim.notify(
    "[init.lua] plugins/init.lua failed to load:\n" .. tostring(err)
      .. "\n\nTroubleshooting:\n"
      .. "  1. Delete ~/.local/share/nvim and ~/.cache/nvim\n"
      .. "  2. Restart Neovim — lazy.nvim will reinstall\n"
      .. "  3. Run :MasonInstallAll\n",
    vim.log.levels.ERROR
  )
  return
end

-- 7. Theme (after plugins so colorscheme plugins are available)
require("core.theme").setup()

-- 8. HUD accent overrides (applied after colorscheme)
pcall(function() require("core.hud").apply() end)

-- 9. Startup stats — hooked to LazyDone so it fires after all plugins are
--    fully initialised, not after a hardcoded timer.
vim.api.nvim_create_autocmd("User", {
  pattern  = "LazyDone",
  once     = true,
  callback = function()
    local ok_lazy, lazy = pcall(require, "lazy")
    if ok_lazy then
      local s = lazy.stats()
      vim.notify(
        string.format("⚡ %d plugins loaded in %.2fms", s.count, s.startuptime),
        vim.log.levels.INFO
      )
    end
  end,
})

-- NOTE: vim.g.nvim_ide_version is now set in core/bootstrap.lua (step 1).
-- It must be set before plugins load so ui.lua's dashboard version string
-- displays correctly. Do not set it here.
