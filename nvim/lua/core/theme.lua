-- lua/core/theme.lua - Theme management (config-driven, no side effects on require)

local M = {}

-- ╔══════════════════════════════════════════════════════╗
-- ║           THEME CONFIGURATION - EDIT HERE            ║
-- ╚══════════════════════════════════════════════════════╝
M.config = {
  -- Pick ONE theme name from the list below:
  --
  --   "catppuccin"       → soft pastel, modern favourite
  --   "tokyonight"       → dark neon-ish, clean
  --   "rose-pine"        → earthy, elegant
  --   "kanagawa"         → Japanese ink aesthetic
  --   "gruvbox-material" → warm retro
  --   "solarized"        → classic precision
  --   "solarized-osaka"  → solarized meets tokyonight
  --
  theme = "tokyonight",

  -- Auto light/dark by hour (24h). Light between day_start and day_end.
  day_start = 7,
  day_end   = 19,

  -- Fallback if chosen theme plugin not installed
  fallback = "default",
}

-- Internal: resolve background based on time
local function resolve_background()
  local h = tonumber(os.date("%H"))
  return (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
end

-- Internal: apply the chosen colorscheme
local function apply(bg)
  vim.o.background = bg
  local ok, err = pcall(vim.cmd.colorscheme, M.config.theme)
  if not ok then
    vim.notify(
      string.format("[theme] '%s' not found, falling back to '%s'\n%s", M.config.theme, M.config.fallback, err),
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

function M.switch(theme_name)
  M.config.theme = theme_name
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
end

-- List available themes (for reference / command completion)
M.available = {
  "catppuccin",
  "tokyonight",
  "rose-pine",
  "kanagawa",
  "gruvbox-material",
  "solarized",
  "solarized-osaka",
}

return M
