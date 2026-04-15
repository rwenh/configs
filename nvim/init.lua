-- ~/.config/nvim/init.lua  — v2.3.9 entry point
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
-- v2.3.9:  runner.lua JS/TS run_tests() cd prefix; MasonInstallAll fortls+gopls;
--          elixir DAP resolver fixed; fortls in mason-lspconfig ensure_installed.
-- v2.3.9b: version stamp corrected 2.3.8→2.3.9; options.lua matchparen restored
--          (vim-matchup owns it); advanced.lua vim-matchup spec added; dap.lua
--          large-file bp-restore guard; test.lua neotest-rust deferred on
--          LspAttach instead of FileType; completion.lua cmdline comment;
--          treesitter.lua latex parser removed; KEYMAP_REFERENCE.md xX/xL/xQ
--          documented; runner.lua c/cpp CTest entries; hud specs <leader>,r
--          global duplicate removed.
vim.g.nvim_ide_version = "2.3.9"
