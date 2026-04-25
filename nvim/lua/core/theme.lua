-- lua/core/theme.lua - Theme management with dynamic switching
--
-- FIX (v2.2.2):
--   • toggle() logic: invalidate_bg_cache() then re-reading the hour snapped
--     back to auto-mode within the same clock-hour. The cache now stores a
--     _manual_override that takes precedence over the time-based resolution.
--     Calling toggle() sets the override; setup() on next startup clears it
--     (intentional — manual overrides don't persist across sessions).

local M = {}

-- ╔══════════════════════════════════════════════════════╗
-- ║           THEME CONFIGURATION - EDIT HERE            ║
-- ╚══════════════════════════════════════════════════════╝
M.config = {
  -- "catppuccin" | "tokyonight" | "rose-pine" | "kanagawa"
  -- "gruvbox-material" | "solarized" | "solarized-osaka"
  theme     = "tokyonight",
  day_start = 7,
  day_end   = 19,
  fallback  = "default",
}

M.available = {
  "catppuccin", "tokyonight", "rose-pine",
  "kanagawa", "gruvbox-material", "solarized", "solarized-osaka",
}

-- Cache: hour-based auto resolution + explicit manual override.
-- _manual_override = nil  → use time-based logic
-- _manual_override = "dark" | "light"  → use this until setup() is called again
local _cache = { hour = nil, value = nil, _manual_override = nil }

local function resolve_background()
  -- Manual override wins (set by toggle())
  if _cache._manual_override then
    return _cache._manual_override
  end

  local ok, h = pcall(function() return tonumber(os.date("%H")) end)
  if not ok or not h then return "dark" end

  if _cache.hour == h and _cache.value then
    return _cache.value
  end

  local bg = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  _cache.hour  = h
  _cache.value = bg
  return bg
end

local function apply(bg)
  vim.o.background = bg

  local ok, err = pcall(function()
    vim.cmd.colorscheme(M.config.theme)
  end)

  if not ok then
    vim.notify(
      string.format("[theme] '%s' not found, falling back to '%s'\n%s",
        M.config.theme, M.config.fallback, tostring(err)),
      vim.log.levels.WARN
    )
    pcall(function() vim.cmd.colorscheme(M.config.fallback) end)
  end
end

function M.setup()
  -- Clear any manual override from a previous session toggle
  _cache._manual_override = nil
  _cache.hour  = nil
  _cache.value = nil
  pcall(function() apply(resolve_background()) end)
end

function M.toggle()
  -- FIX: compute next_bg from current state, then pin it as a manual override.
  -- Previously we invalidated the hour-cache then immediately re-resolved,
  -- which returned the same time-based value within the same clock-hour.
  local current = vim.o.background
  local next_bg = current == "dark" and "light" or "dark"

  -- Pin the override so resolve_background() always returns next_bg until
  -- the user calls setup() again (i.e. until next Neovim launch).
  _cache._manual_override = next_bg
  _cache.hour  = nil
  _cache.value = nil

  pcall(function()
    apply(next_bg)
    vim.notify(
      string.format("[theme] %s › %s", M.config.theme, next_bg),
      vim.log.levels.INFO
    )
  end)
end

function M.switch(theme_name)
  if not theme_name or theme_name == "" then
    vim.notify("[theme] Theme name cannot be empty", vim.log.levels.ERROR)
    return
  end

  local valid = vim.tbl_contains(M.available, theme_name)
  if not valid then
    vim.notify(
      string.format(
        "[theme] '%s' is not in the available list:\n%s",
        theme_name, table.concat(M.available, ", ")
      ),
      vim.log.levels.ERROR
    )
    return
  end

  M.config.theme = theme_name
  -- Preserve the current manual override (or re-resolve if none)
  pcall(function()
    apply(resolve_background())
    vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
  end)
end

function M.get_active_theme()
  return M.config.theme or "tokyonight"
end

-- FIX: if the user runs :colorscheme <name> directly (bypassing theme.lua),
-- _manual_override stays set to the previous toggle state and
-- resolve_background() will keep reporting the old value on the next
-- toggle() call. Listen for ColorScheme and clear the override when the
-- newly active scheme no longer matches what theme.lua set.
vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("ThemeCacheSync", { clear = true }),
  callback = function(e)
    if e.match ~= M.config.theme and not e.match:match("^" .. M.config.theme) then
      _cache._manual_override = nil
      _cache.hour  = nil
      _cache.value = nil
    end
  end,
  desc = "Clear theme.lua manual-override cache on external :colorscheme",
})

return M
