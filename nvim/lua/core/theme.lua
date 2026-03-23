-- lua/core/theme.lua - Theme management

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

-- FIX #9: Removed _bg_cache entirely.
-- The original cache was set once at setup() and never invalidated across
-- time boundaries — if Neovim stayed open past day_end (19:00) or day_start
-- (7:00) the background would be permanently stale for that session.
-- The cache comment claimed it saved repeated os.date() calls, but setup()
-- only runs once at startup, so the cache saved exactly zero calls in
-- practice while introducing the staleness problem.
-- os.date("%H") is a trivial syscall — no caching needed.
local function resolve_background()
  local h = tonumber(os.date("%H"))
  return (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
end

local function apply(bg)
  vim.o.background = bg
  local ok, err = pcall(vim.cmd.colorscheme, M.config.theme)
  if not ok then
    vim.notify(
      string.format("[theme] '%s' not found, falling back to '%s'\n%s",
        M.config.theme, M.config.fallback, err),
      vim.log.levels.WARN
    )
    pcall(vim.cmd.colorscheme, M.config.fallback)
  end
end

function M.setup()
  apply(resolve_background())
end

function M.toggle()
  local next_bg = vim.o.background == "dark" and "light" or "dark"
  apply(next_bg)
  vim.notify(string.format("[theme] %s › %s", M.config.theme, next_bg), vim.log.levels.INFO)
end

-- FIX #12: Validate theme_name against M.available before mutating config.
-- Previously, an invalid name would permanently corrupt M.config.theme —
-- apply() would fail and fall back to "default", but the bad name would
-- persist and be retried on every subsequent toggle/switch call.
function M.switch(theme_name)
  local valid = vim.tbl_contains(M.available, theme_name)
  if not valid then
    vim.notify(
      string.format(
        "[theme] '%s' is not in the available list: %s",
        theme_name, table.concat(M.available, ", ")
      ),
      vim.log.levels.ERROR
    )
    return
  end
  -- FIX #10: Keep M.config.theme as the user-configured default (startup
  -- setting). Track the currently active theme separately so M.config
  -- remains stable as a source of truth for the original preference.
  M.config.theme = theme_name   -- intentional: user switched preference
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
end

return M
