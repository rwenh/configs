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
  dap_adapters = {},
  project_root = nil,
  memory_limit_mb = 1024,
}

local function safe_require(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    _G.nvim_ide.failed[mod] = { error = tostring(result), timestamp = os.time() }
    if _G.nvim_ide.debug then
      vim.schedule(function()
        vim.notify("Failed to load: " .. mod .. "\n" .. tostring(result), vim.log.levels.ERROR)
      end)
    end
    return nil
  end
  return result
end

-- Core configuration (must load first)
safe_require("core.options")
safe_require("core.lazy")

-- Theme (load early for visual consistency)
safe_require("core.theme")

-- Plugins (managed by lazy.nvim)
safe_require("plugins")

-- Configuration modules (order matters for dependencies)
safe_require("config.lsp")
safe_require("config.completion")
safe_require("config.treesitter")
safe_require("config.telescope")
safe_require("config.git")
safe_require("config.dap")
safe_require("config.ui")
safe_require("config.testing")
safe_require("config.null-ls")

-- Core behavior
safe_require("core.keymaps")
safe_require("core.autocmds")
safe_require("core.commands")

-- Utilities
safe_require("utils.runner")
safe_require("utils.platform")

-- Startup notification with profiling
vim.defer_fn(function()
  local lazy_ok, lazy = pcall(require, "lazy")
  local startup_time = (vim.uv.hrtime() - _G.nvim_ide.startup_time) / 1e6
  local plugin_count = lazy_ok and lazy.stats().loaded or 0
  
  local message = string.format("⚡ %.0fms • %d plugins", startup_time, plugin_count)
  
  if next(_G.nvim_ide.failed) then
    local failed_count = vim.tbl_count(_G.nvim_ide.failed)
    message = message .. string.format(" • %d failed", failed_count)
  end
  
  vim.notify(message, vim.log.levels.INFO, { title = "Ready" })
  
  -- Memory check on startup
  local memory_mb = collectgarbage("count") / 1024
  if memory_mb > _G.nvim_ide.memory_limit_mb * 0.8 then
    vim.notify(
      string.format("High memory usage: %.1fMB", memory_mb),
      vim.log.levels.WARN,
      { title = "Memory Warning" }
    )
  end
end, 100)

-- Periodic health monitoring (every 5 minutes)
local health_timer = vim.uv.new_timer()
if health_timer then
  health_timer:start(300000, 300000, vim.schedule_wrap(function()
    local helpers = require("utils.helpers")
    helpers.check_memory()
  end))
end
