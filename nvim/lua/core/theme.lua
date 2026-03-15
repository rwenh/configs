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

-- Cache background so os.date() isn't called on every setup()/switch()
local _bg_cache = nil

local function resolve_background()
  if _bg_cache then return _bg_cache end
  local h = tonumber(os.date("%H"))
  _bg_cache = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  return _bg_cache
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
  -- Toggling means overriding the cached value
  local next_bg = vim.o.background == "dark" and "light" or "dark"
  _bg_cache = next_bg
  apply(next_bg)
  vim.notify(string.format("[theme] %s › %s", M.config.theme, next_bg), vim.log.levels.INFO)
end

function M.switch(theme_name)
  M.config.theme = theme_name
  _bg_cache = nil  -- invalidate cache so time is re-evaluated for new theme
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
end

return M
