-- lua/core/theme.lua — theme management with automatic dark/light switching
--

local M = {}

-- ── Configuration ─────────────────────────────────────────────────────────────
-- Edit these values to change the active theme and auto-dark schedule.
-- Use M.switch() at runtime rather than mutating M.config directly.
M.config = {
  theme     = "tokyonight",   -- active theme name
  day_start = 7,              -- hour (0-23) when light mode starts
  day_end   = 19,             -- hour (0-23) when dark mode starts
  fallback  = "default",      -- colorscheme used when the primary is unavailable
}

M.available = {
  "catppuccin", "tokyonight", "rose-pine",
  "kanagawa", "gruvbox-material", "solarized", "solarized-osaka",
}

-- ── Internal cache ────────────────────────────────────────────────────────────
-- Fields:
--   hour            integer|nil  last hour used for time-based resolution
--   value           string|nil   last resolved background ("dark"/"light")
--   manual_override string|nil   pinned value from toggle(); nil = use time
local _cache = { hour = nil, value = nil, manual_override = nil }

local _setup_called = false

-- ── Private helpers ───────────────────────────────────────────────────────────

local function resolve_background()
  if _cache.manual_override then return _cache.manual_override end

  local ok, h = pcall(function() return tonumber(os.date("%H")) end)
  if not ok or not h then return "dark" end   -- safe fallback for headless

  if _cache.hour == h and _cache.value then return _cache.value end

  local bg = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  _cache.hour  = h
  _cache.value = bg
  return bg
end

local function apply(bg)
  vim.o.background = bg
  local ok, err = pcall(vim.cmd.colorscheme, M.config.theme)
  if not ok then
    vim.notify(
      string.format("[theme] '%s' unavailable, falling back to '%s'\n%s",
        M.config.theme, M.config.fallback, tostring(err)),
      vim.log.levels.WARN
    )
    pcall(vim.cmd.colorscheme, M.config.fallback)
  end
end

-- ── Public API ────────────────────────────────────────────────────────────────

function M.setup()
  if not vim.tbl_contains(M.available, M.config.theme) then
    vim.notify(
      string.format("[theme] config.theme '%s' is not in M.available — "
        .. "check lua/core/theme.lua", M.config.theme),
      vim.log.levels.WARN
    )
  end

  -- Clear any manual override from a previous session.
  _cache.manual_override = nil
  _cache.hour  = nil
  _cache.value = nil

  _setup_called = true
  apply(resolve_background())
end

function M.toggle()
  if not _setup_called then
    vim.notify("[theme] toggle() called before setup() — call require('core.theme').setup() first",
      vim.log.levels.WARN)
  end

  local next_bg = vim.o.background == "dark" and "light" or "dark"
  _cache.manual_override = next_bg
  _cache.hour  = nil
  _cache.value = nil

  apply(next_bg)
  vim.notify(
    string.format("[theme] %s › %s", M.config.theme, next_bg),
    vim.log.levels.INFO
  )
end

--- Switch to *theme_name*, inheriting the current background state.
---@param theme_name string
function M.switch(theme_name)
  if not theme_name or theme_name == "" then
    vim.notify("[theme] theme name cannot be empty", vim.log.levels.ERROR)
    return
  end
  if not vim.tbl_contains(M.available, theme_name) then
    vim.notify(
      string.format("[theme] '%s' not in available list: %s",
        theme_name, table.concat(M.available, ", ")),
      vim.log.levels.ERROR
    )
    return
  end
  M.config.theme = theme_name
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
end

---@return string  name of the currently active theme
function M.get_active_theme()
  return M.config.theme or "tokyonight"
end

-- ── ColorScheme autocmd ───────────────────────────────────────────────────────

vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("ThemeCacheSync", { clear = true }),
  callback = function(e)
    -- If the new colorscheme was NOT set by theme.lua, clear our cache so
    -- the next toggle() works from clean state.
    if not vim.startswith(e.match, M.config.theme) then
      _cache.manual_override = nil
      _cache.hour  = nil
      _cache.value = nil
    end
  end,
  desc = "Clear theme.lua cache when an external :colorscheme command fires",
})

return M
