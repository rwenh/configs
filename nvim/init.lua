-- ~/.config/nvim/init.lua
-- Neovim Entry Point

if vim.loader then vim.loader.enable() end

-- FIX #1: Version bumped to match actual release (was "2.0", config is v2.1)
_G.nvim_ide = {
  version = "2.1",
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

-- FIX #4: Hard-fail guard — if plugins don't load, nothing downstream works anyway
local plugins_ok = safe_load("plugins")
if not plugins_ok then
  vim.notify(
    "FATAL: plugins/init.lua failed to load. Aborting further init.\nRun :messages for details.",
    vim.log.levels.ERROR
  )
  return
end

-- Theme setup AFTER plugins so lazy.nvim has loaded the colorscheme plugin
local theme = safe_load("core.theme")
if theme then theme.setup() end

-- FIX #2: autocmds loaded SYNCHRONOUSLY so BufReadPre/BufRead fire for the
-- first file opened via CLI (e.g. `nvim myfile.lua`). Deferring autocmds
-- causes them to be registered after the initial buffer events have already fired.
safe_load("core.autocmds")

-- Keymaps and commands are UI-only and safe to defer
vim.defer_fn(function()
  safe_load("core.keymaps")
  safe_load("core.commands")
end, 0)

-- FIX #3: Use the LazyDone event instead of a hardcoded 150ms timer.
-- The timer was fragile — on slow machines or with many plugins, lazy.nvim
-- may not have finished accumulating startuptime within 150ms.
-- RECALIBRATION: Added defensive checks for stats existence and type validation
vim.api.nvim_create_autocmd("User", {
  pattern  = "LazyDone",
  once     = true,
  callback = function()
    vim.schedule(function()
      local ok, lazy = pcall(require, "lazy")
      if not ok then return end

      -- Type-safe stats access with nil checks
      local stats = lazy.stats and lazy.stats()
      if not stats or not stats.count or not stats.loaded then return end

      -- Only notify if we have meaningful numbers
      if stats.count > 0 then
        vim.notify(
          string.format("⚡ %d/%d plugins · %.0fms", stats.loaded, stats.count, stats.startuptime or 0),
          vim.log.levels.INFO
        )
      end
    end)
  end,
})
