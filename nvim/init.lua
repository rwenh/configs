-- ~/.config/nvim/init.lua
-- Neovim Entry Point

if vim.loader then vim.loader.enable() end

_G.nvim_ide = {
  version = "2.0",
  root    = vim.fn.stdpath("config"),
  data    = vim.fn.stdpath("data"),
  cache   = vim.fn.stdpath("cache"),
  debug   = vim.env.NVIM_DEBUG == "1",
}

local function safe_load(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    vim.schedule(function()
      vim.notify("Failed to load: " .. mod .. "\n" .. tostring(result), vim.log.levels.ERROR)
    end)
    return nil
  end
  return result
end

-- Load in correct order
safe_load("core.options")
safe_load("core.bootstrap")

safe_load("plugins")

-- Theme setup AFTER plugins so lazy.nvim has loaded the colorscheme plugin
local theme = safe_load("core.theme")
if theme then theme.setup() end

-- Deferred: keymaps, autocmds, commands don't need to block startup
vim.defer_fn(function()
  safe_load("core.keymaps")
  safe_load("core.autocmds")
  safe_load("core.commands")
end, 0)

-- Startup stats
vim.defer_fn(function()
  local ok, lazy = pcall(require, "lazy")
  if ok then
    local s = lazy.stats()
    vim.notify(
      string.format("⚡ %d/%d plugins · %.0fms", s.loaded, s.count, s.startuptime),
      vim.log.levels.INFO
    )
  end
end, 150)
