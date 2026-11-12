-- lua/core/theme.lua — theme management with auto dark/light switching
--
-- M.themes maps each theme to the colorscheme it uses for each background. A switch loads the
-- theme's plugin first (lazy themes are off the runtime path until loaded), sets vim.o.background,
-- then applies the colorscheme for that background.

local M = {}

M.config = {
  theme = "tokyonight",
  day_start = 7,
  day_end = 19,
  fallback = "default",
}

-- plugin: the lazy.nvim name (the `name` field in plugins/specs/ui.lua).
-- dark / light: the colorscheme applied for each background.
M.themes = {
  tokyonight = { plugin = "tokyonight", dark = "tokyonight-moon", light = "tokyonight-day" },
  catppuccin = { plugin = "catppuccin", dark = "catppuccin-mocha", light = "catppuccin-latte" },
  ["rose-pine"] = { plugin = "rose-pine", dark = "rose-pine", light = "rose-pine-dawn" },
  kanagawa = { plugin = "kanagawa", dark = "kanagawa-dragon", light = "kanagawa-lotus" },
  ["gruvbox-material"] = { plugin = "gruvbox-material", dark = "gruvbox-material", light = "gruvbox-material" },
  solarized = { plugin = "solarized", dark = "solarized", light = "solarized" },
  ["solarized-osaka"] = { plugin = "solarized-osaka", dark = "solarized-osaka", light = "solarized-osaka" },
}

M.available = vim.tbl_keys(M.themes)
table.sort(M.available)

-- ── Per-theme option patches ──────────────────────────────────────────────────
--   vim.g.theme_option_patches = { tokyonight = { ... } }
local function apply_patches(theme_name)
  local patches = type(vim.g.theme_option_patches) == "table" and vim.g.theme_option_patches[theme_name]
  if type(patches) ~= "table" then
    return
  end
  for key, val in pairs(patches) do
    pcall(function()
      vim.o[key] = val
    end)
  end
end

-- ── Change hooks ──────────────────────────────────────────────────────────────
local _on_change_hooks = {}

---@param fn fun(theme_name: string, background: string)
function M.on_change(fn)
  if type(fn) == "function" then
    table.insert(_on_change_hooks, fn)
  end
end

local function fire_hooks(theme_name, bg)
  for _, fn in ipairs(_on_change_hooks) do
    pcall(fn, theme_name, bg)
  end
end

-- ── State (vim.g-backed so it survives :Reload) ───────────────────────────────
local OVERRIDE_KEY = "_nvim_theme_override"
local READY_KEY = "_nvim_theme_ready"

local function get_override()
  local v = vim.g[OVERRIDE_KEY]
  if v == "light" or v == "dark" then
    return v
  end
  return nil
end

local function set_override(v)
  vim.g[OVERRIDE_KEY] = v
end

local _cache = { hour = nil, value = nil }

local function resolve_background()
  local o = get_override()
  if o then
    return o
  end
  local ok, h = pcall(function()
    return tonumber(os.date("%H"))
  end)
  if not ok or not h then
    return "dark"
  end
  if _cache.hour == h and _cache.value then
    return _cache.value
  end
  local bg = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  _cache.hour, _cache.value = h, bg
  return bg
end

-- ── Apply ─────────────────────────────────────────────────────────────────────
-- A lazy theme is not on the runtime path until lazy.nvim loads it, so load it first.
local function ensure_loaded(plugin)
  local ok, lazy = pcall(require, "lazy")
  if ok then
    pcall(lazy.load, { plugins = { plugin } })
  end
end

local function apply(bg)
  local spec = M.themes[M.config.theme]
  if not spec then
    vim.notify(string.format("[theme] '%s' is not in M.themes", tostring(M.config.theme)), vim.log.levels.WARN)
    pcall(vim.cmd.colorscheme, M.config.fallback)
    return
  end
  ensure_loaded(spec.plugin)
  vim.o.background = bg
  local scheme = spec[bg] or spec.dark
  local ok, err = pcall(vim.cmd.colorscheme, scheme)
  if not ok then
    vim.notify(
      string.format(
        "[theme] '%s' (%s) failed; falling back to '%s'\n%s",
        scheme,
        M.config.theme,
        M.config.fallback,
        tostring(err)
      ),
      vim.log.levels.WARN
    )
    pcall(vim.cmd.colorscheme, M.config.fallback)
    return
  end
  apply_patches(M.config.theme)
  fire_hooks(M.config.theme, bg)
end

-- ── Day/night timer ───────────────────────────────────────────────────────────
local function stop_timer()
  local t = _G._nvim_theme_timer
  if t then
    pcall(function()
      t:stop()
      t:close()
    end)
    _G._nvim_theme_timer = nil
  end
end

local function start_timer()
  stop_timer()
  if vim.g.disable_theme_autoswitch == true then
    return
  end
  local t = vim.uv.new_timer()
  if not t then
    return
  end
  _G._nvim_theme_timer = t
  t:start(
    60000,
    60000,
    vim.schedule_wrap(function()
      if get_override() then
        return
      end
      local want = resolve_background()
      if want ~= vim.o.background then
        apply(want)
      end
    end)
  )
end

function M.setup()
  if not M.themes[M.config.theme] then
    vim.notify(string.format("[theme] config.theme '%s' is not in M.themes", M.config.theme), vim.log.levels.WARN)
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
  vim.notify(
    string.format("[theme] %s › %s (manual; :ThemeAuto resumes time-based)", M.config.theme, next_bg),
    vim.log.levels.INFO
  )
end

--- Drop the manual override and follow the clock again.
function M.auto()
  set_override(nil)
  _cache.hour, _cache.value = nil, nil
  local bg = resolve_background()
  apply(bg)
  vim.notify(string.format("[theme] %s › %s (auto)", M.config.theme, bg), vim.log.levels.INFO)
end

---@param name string
function M.switch(name)
  if not name or name == "" then
    vim.notify("[theme] theme name cannot be empty", vim.log.levels.ERROR)
    return
  end
  if not M.themes[name] then
    vim.notify(
      string.format("[theme] '%s' not in: %s", name, table.concat(M.available, ", ")),
      vim.log.levels.ERROR
    )
    return
  end
  M.config.theme = name
  vim.g._nvim_active_theme = name
  _cache.hour, _cache.value = nil, nil
  apply(resolve_background())
  vim.notify(string.format("[theme] switched to %s", name), vim.log.levels.INFO)
end

---@return string
function M.get_active_theme()
  return M.config.theme or "tokyonight"
end

vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("ThemeCacheSync", { clear = true }),
  callback = function(e)
    -- Our own applies fire this too; only an outside :colorscheme drops the manual override.
    local spec = M.themes[M.config.theme]
    if spec and (e.match == spec.dark or e.match == spec.light) then
      return
    end
    set_override(nil)
    _cache.hour, _cache.value = nil, nil
  end,
  desc = "Clear theme override when an external :colorscheme fires",
})

vim.api.nvim_create_autocmd("VimLeavePre", {
  group = vim.api.nvim_create_augroup("ThemeTimerCleanup", { clear = true }),
  callback = stop_timer,
  desc = "Close the day/night timer handle",
})

vim.api.nvim_create_user_command("ThemeAuto", function()
  M.auto()
end, { desc = "Resume time-based dark/light switching" })

vim.api.nvim_create_user_command("Theme", function(o)
  M.switch(vim.trim(o.args))
end, {
  nargs = 1,
  complete = function(lead)
    return vim.tbl_filter(function(n)
      return vim.startswith(n, lead)
    end, M.available)
  end,
  desc = "Switch colour theme: :Theme catppuccin",
})

return M
