-- ~/.config/nvim/init.lua
-- Neovim Entry Point

if vim.loader then vim.loader.enable() end

_G.claude_ide = {
  version = "claude-1.1",
  root = vim.fn.stdpath("config"),
  data = vim.fn.stdpath("data"),
  cache = vim.fn.stdpath("cache"),
  debug = vim.env.NVIM_DEBUG == "1",
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
safe_load("core.theme")
safe_load("plugins")

-- Deferred loading
vim.defer_fn(function()
  safe_load("core.keymaps")
  safe_load("core.autocmds")
  safe_load("core.commands")
end, 0)

-- Show stats
vim.defer_fn(function()
  local lazy_ok, lazy = pcall(require, "lazy")
  if lazy_ok then
    local stats = lazy.stats()
    vim.notify(string.format("⚡ %d/%d plugins loaded in %.2fms", stats.loaded, stats.count, stats.startuptime), vim.log.levels.INFO)
  end
end, 100)