-- ~/.config/nvim/init.lua — v2.4.1 entry point
--
-- Load order: bootstrap → options → autocmds → keymaps → commands → plugins → theme → highlights

-- 1. Bootstrap lazy.nvim + leader keys + version stamp (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands
require("core.autocmds")

-- 4. Global keymaps
require("core.keymaps")

-- 5. User commands
require("core.commands")

-- 6. Plugin manager + all plugin specs
--
local _plugins_ok = true
local _plugins_err = nil

local ok, err = pcall(require, "plugins")
if not ok then
  _plugins_ok  = false
  _plugins_err = err
  vim.notify(
    "[init.lua] plugins/init.lua failed to load:\n" .. tostring(err)
      .. "\n\nTroubleshooting:\n"
      .. "  1. Delete ~/.local/share/nvim and ~/.cache/nvim\n"
      .. "  2. Restart Neovim — lazy.nvim will reinstall\n"
      .. "  3. Run :MasonInstallAll\n",
    vim.log.levels.ERROR
  )
end

-- 7. Theme (after plugins so colorscheme plugins are available;
local ok_theme, theme_err = pcall(function()
  require("core.theme").setup()
end)
if not ok_theme then
  vim.notify(
    "[init.lua] theme.setup() failed: " .. tostring(theme_err)
    .. "\nFalling back to built-in 'default' colorscheme.",
    vim.log.levels.WARN
  )
  pcall(vim.cmd.colorscheme, "default")
end

-- 8. Highlight overrides
local ok_hl, hl_err = pcall(function()
  require("core.highlights").apply()
end)
if not ok_hl then
  vim.notify(
    "[init.lua] highlights.apply() failed: " .. tostring(hl_err),
    vim.log.levels.WARN
  )
end

-- 9. Startup stats (only when plugins loaded successfully)
if _plugins_ok then
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
end

-- NOTE: vim.g.nvim_ide_version is set in core/bootstrap.lua (step 1).
