-- ~/.config/nvim/init.lua — v2.4.1 entry point
--
-- Load order: bootstrap → options → autocmds → keymaps → commands → plugins → theme → highlights
--

-- 1. Bootstrap lazy.nvim + leader keys + version stamp (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands — loaded synchronously so BufReadPre/BufRead fire for
--    the first file opened via CLI (e.g. `nvim myfile.lua`).
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

-- 8. Highlight overrides — applied after colorscheme; also auto-reapplied
--    on every ColorScheme event via the autocmd in core/highlights.lua.
pcall(function() require("core.highlights").apply() end)

-- 9. Startup stats — hooked to LazyDone so it fires after all plugins are
--    fully initialised.
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

-- NOTE: vim.g.nvim_ide_version is set in core/bootstrap.lua (step 1).
-- Do not set it here — it must be available before any plugin config() runs.
