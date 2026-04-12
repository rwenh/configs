-- ~/.config/nvim/init.lua  — v2.3.6 entry point
-- Load order: bootstrap → options → autocmds → keymaps → plugins → theme

-- 1. Bootstrap lazy.nvim + set leader (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands — loaded synchronously so BufReadPre/BufRead fire for the
--    first file opened via CLI (e.g. `nvim myfile.lua`).
--    FIX: was deferred with vim.defer_fn(0ms) which caused missed events.
require("core.autocmds")

-- 4. Global keymaps
require("core.keymaps")

-- 5. User commands
require("core.commands")

-- 6. Plugin manager + all plugin specs
--    Hard-fail guard: if plugins/init.lua errors, abort early with a clear
--    message rather than silently producing a broken state.
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

-- 9. Startup stats — hooked to LazyDone event so it fires after all plugins
--    are fully initialised, not after a hardcoded timer.
--    FIX: was vim.defer_fn(150ms) which was fragile on slow machines.
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

-- 10. Version stamp (for :Health and debug output)
-- v2.3.6: version stamp corrected (was stale at 2.3.4 while codebase was
--         already at 2.3.5); harpoon/todo-comments pcall guards added;
--         blink.cmp C-p/C-n "show" fallback leak fixed; dap.lua
--         mason-nvim-dap "python" → "debugpy" package name corrected.
vim.g.nvim_ide_version = "2.3.6"
