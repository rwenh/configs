-- lua/core/theme.lua — theme management with auto dark/light switching
--
-- Fixes vs v2.5.0:
--   * the day/night switch now actually happens at the boundary (a 60 s timer);
--     before, the background was only evaluated at startup
--   * manual override + ready flag live in vim.g, so `:Reload core.theme` keeps state
--   * :ThemeAuto returns to time-based switching after a manual <leader>ut

local M = {}

M.config = {
  theme     = "tokyonight",
  day_start = 7,
  day_end   = 19,
  fallback  = "default",
}

M.available = {
  "catppuccin", "tokyonight", "rose-pine",
  "kanagawa", "gruvbox-material", "solarized", "solarized-osaka",
}

-- ── Per-theme option patches ──────────────────────────────────────────────────
--   vim.g.theme_option_patches = { tokyonight = { background = "dark" } }
local _builtin_patches = {
  tokyonight           = {},
  catppuccin           = {},
  kanagawa             = {},
  ["rose-pine"]        = {},
  ["gruvbox-material"] = {},
  solarized            = {},
  ["solarized-osaka"]  = {},
}

local function get_patches(theme_name)
  local user = type(vim.g.theme_option_patches) == "table"
    and (vim.g.theme_option_patches[theme_name] or {}) or {}
  return vim.tbl_extend("force", _builtin_patches[theme_name] or {}, user)
end

-- ── Change hooks ──────────────────────────────────────────────────────────────
local _on_change_hooks = {}

---@param fn fun(theme_name: string, background: string)
function M.on_change(fn)
  if type(fn) == "function" then table.insert(_on_change_hooks, fn) end
end

local function fire_hooks(theme_name, bg)
  for _, fn in ipairs(_on_change_hooks) do pcall(fn, theme_name, bg) end
end

-- ── State (vim.g-backed so it survives :Reload) ───────────────────────────────
local OVERRIDE_KEY = "_nvim_theme_override"
local READY_KEY    = "_nvim_theme_ready"

local function get_override()
  local v = vim.g[OVERRIDE_KEY]
  if v == "light" or v == "dark" then return v end
  return nil
end
local function set_override(v) vim.g[OVERRIDE_KEY] = v end   -- nil clears

local _cache = { hour = nil, value = nil }

local function resolve_background()
  local o = get_override()
  if o then return o end
  local ok, h = pcall(function() return tonumber(os.date("%H")) end)
  if not ok or not h then return "dark" end
  if _cache.hour == h and _cache.value then return _cache.value end
  local bg = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  _cache.hour, _cache.value = h, bg
  return bg
end

local function apply_patches(theme_name)
  for key, val in pairs(get_patches(theme_name)) do
    pcall(function() vim.o[key] = val end)
  end
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
    return
  end
  apply_patches(M.config.theme)
  fire_hooks(M.config.theme, bg)
end

-- ── Day/night timer ───────────────────────────────────────────────────────────
-- A previous copy of this module (after :Reload) may have left a timer behind.
local function stop_timer()
  local t = _G._nvim_theme_timer
  if t then
    pcall(function() t:stop(); t:close() end)
    _G._nvim_theme_timer = nil
  end
end

local function start_timer()
  stop_timer()
  if vim.g.disable_theme_autoswitch == true then return end
  local t = vim.uv.new_timer()
  if not t then return end
  _G._nvim_theme_timer = t
  t:start(60000, 60000, vim.schedule_wrap(function()
    if get_override() then return end
    local want = resolve_background()
    if want ~= vim.o.background then apply(want) end
  end))
end

function M.setup()
  if not vim.tbl_contains(M.available, M.config.theme) then
    vim.notify(
      string.format("[theme] config.theme '%s' is not in M.available", M.config.theme),
      vim.log.levels.WARN
    )
  end
  set_override(nil)
  _cache.hour, _cache.value = nil, nil
  vim.g[READY_KEY] = true
  apply(resolve_background())
  start_timer()
end

function M.toggle()
  if not vim.g[READY_KEY] then
    vim.notify("[theme] toggle() called before setup()", vim.log.levels.WARN)
  end
  local next_bg = vim.o.background == "dark" and "light" or "dark"
  set_override(next_bg)
  apply(next_bg)
  vim.notify(string.format("[theme] %s › %s (manual — :ThemeAuto to resume time-based)", M.config.theme, next_bg), vim.log.levels.INFO)
end

--- Drop the manual override and follow the clock again.
function M.auto()
  set_override(nil)
  _cache.hour, _cache.value = nil, nil
  local bg = resolve_background()
  apply(bg)
  vim.notify(string.format("[theme] %s › %s (auto)", M.config.theme, bg), vim.log.levels.INFO)
end

---@param theme_name string
function M.switch(theme_name)
  if not theme_name or theme_name == "" then
    vim.notify("[theme] theme name cannot be empty", vim.log.levels.ERROR); return
  end
  if not vim.tbl_contains(M.available, theme_name) then
    vim.notify(
      string.format("[theme] '%s' not in available list: %s", theme_name, table.concat(M.available, ", ")),
      vim.log.levels.ERROR
    )
    return
  end
  M.config.theme           = theme_name
  vim.g._nvim_active_theme = theme_name
  _cache.hour, _cache.value = nil, nil
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", theme_name), vim.log.levels.INFO)
end

---@return string
function M.get_active_theme()
  return M.config.theme or "tokyonight"
end

vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("ThemeCacheSync", { clear = true }),
  callback = function(e)
    if not vim.startswith(e.match, M.config.theme) then
      set_override(nil)
      _cache.hour, _cache.value = nil, nil
    end
  end,
  desc = "Clear theme override when an external :colorscheme fires",
})

vim.api.nvim_create_autocmd("VimLeavePre", {
  group    = vim.api.nvim_create_augroup("ThemeTimerCleanup", { clear = true }),
  callback = stop_timer,
  desc     = "Close the day/night timer handle",
})

vim.api.nvim_create_user_command("ThemeAuto", function() M.auto() end,
  { desc = "Resume time-based dark/light switching" })

return M
