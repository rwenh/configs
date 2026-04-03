-- lua/core/theme.lua - Theme management with dynamic switching

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

-- RECALIBRATION: Safe background resolution with time-based switching
local function resolve_background()
  local ok, h = pcall(function()
    return tonumber(os.date("%H"))
  end)

  if not ok or not h then
    return "dark"  -- Fallback on error
  end

  return (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
end

-- RECALIBRATION: Safe colorscheme application with comprehensive error handling
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
    pcall(function()
      vim.cmd.colorscheme(M.config.fallback)
    end)
  end
end

function M.setup()
  pcall(function()
    apply(resolve_background())
  end)
end

function M.toggle()
  local next_bg = vim.o.background == "dark" and "light" or "dark"
  pcall(function()
    apply(next_bg)
    vim.notify(string.format("[theme] %s › %s", M.config.theme, next_bg), vim.log.levels.INFO)
  end)
end

-- RECALIBRATION: Safe theme switching with validation
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

  -- Update config and apply
  M.config.theme = theme_name
  pcall(function()
    apply(resolve_background())
    vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
  end)
end

-- RECALIBRATION: Safe get_active_theme for lazy loading
function M.get_active_theme()
  return M.config.theme or "tokyonight"
end

return M
