-- ~/.config/nvim/init.lua
-- Modular Neovim IDE - Entry Point

if vim.loader then vim.loader.enable() end

_G.nvim_ide = {
  version = "3.0-delicate",
  root = vim.fn.stdpath("config"),
  data = vim.fn.stdpath("data"),
  cache = vim.fn.stdpath("cache"),
  debug = vim.env.NVIM_DEBUG == "1",
  failed = {},
  startup_time = vim.uv.hrtime(),
  os_type = vim.uv.os_uname().sysname,
}

local function safe_require(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    _G.nvim_ide.failed[mod] = result
    return nil
  end
  return result
end

safe_require("core.options")
safe_require("core.lazy")
safe_require("core.theme")
safe_require("plugins")
safe_require("config.lsp")
safe_require("config.completion")
safe_require("config.treesitter")
safe_require("config.telescope")
safe_require("config.git")
safe_require("config.dap")
safe_require("config.ui")
safe_require("config.testing")
safe_require("config.null-ls")
safe_require("core.keymaps")
safe_require("core.autocmds")
safe_require("core.commands")
safe_require("utils.runner")
safe_require("utils.platform")

vim.defer_fn(function()
  local lazy_ok, lazy = pcall(require, "lazy")
  local startup_time = (vim.uv.hrtime() - _G.nvim_ide.startup_time) / 1e6
  local plugin_count = lazy_ok and lazy.stats().loaded or 0
  
  vim.notify(
    string.format("⚡ %.0fms • %d plugins", startup_time, plugin_count),
    vim.log.levels.INFO,
    { title = "Ready" }
  )
end, 100)