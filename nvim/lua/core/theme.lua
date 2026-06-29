-- lua/core/theme.lua — theme management with auto dark/light switching
--

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
--
-- Example:
--   vim.g.theme_option_patches = {
--     tokyonight      = { background = "dark"  },
--     ["rose-pine"]   = { background = "light" },
--   }

local _builtin_patches = {
  tokyonight       = { background = "dark"  },
  catppuccin       = { background = "dark"  },
  kanagawa         = { background = "dark"  },
  ["rose-pine"]    = { background = "dark"  },
  ["gruvbox-material"] = { background = "dark" },
  solarized           = { background = "light" },
  ["solarized-osaka"] = { background = "dark"  },
}

local function get_patches(theme_name)
  local user = type(vim.g.theme_option_patches) == "table"
    and (vim.g.theme_option_patches[theme_name] or {}) or {}
  local builtin = _builtin_patches[theme_name] or {}
  return vim.tbl_extend("force", builtin, user)
end

-- ── Change hooks ──────────────────────────────────────────────────────────────
-- Registered via M.on_change(fn); called after every successful theme switch.
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

-- ── Internal cache ────────────────────────────────────────────────────────────
local _cache = { hour = nil, value = nil, manual_override = nil }
local _setup_called = false

local function resolve_background()
  if _cache.manual_override then return _cache.manual_override end
  local ok, h = pcall(function() return tonumber(os.date("%H")) end)
  if not ok or not h then return "dark" end
  if _cache.hour == h and _cache.value then return _cache.value end
  local bg = (h >= M.config.day_start and h < M.config.day_end) and "light" or "dark"
  _cache.hour  = h
  _cache.value = bg
  return bg
end

local function apply_patches(theme_name)
  local patches = get_patches(theme_name)
  for key, val in pairs(patches) do
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

function M.setup()
  if not vim.tbl_contains(M.available, M.config.theme) then
    vim.notify(
      string.format("[theme] config.theme '%s' is not in M.available", M.config.theme),
      vim.log.levels.WARN
    )
  end
  _cache.manual_override = nil
  _cache.hour  = nil
  _cache.value = nil
  _setup_called = true
  apply(resolve_background())
end

function M.toggle()
  if not _setup_called then
    vim.notify("[theme] toggle() called before setup()", vim.log.levels.WARN)
  end
  local next_bg = vim.o.background == "dark" and "light" or "dark"
  _cache.manual_override = next_bg
  _cache.hour  = nil
  _cache.value = nil
  apply(next_bg)
  vim.notify(string.format("[theme] %s › %s", M.config.theme, next_bg), vim.log.levels.INFO)
end

---@param theme_name string
function M.switch(theme_name)
  if not theme_name or theme_name == "" then
    vim.notify("[theme] theme name cannot be empty", vim.log.levels.ERROR); return
  end
  if not vim.tbl_contains(M.available, theme_name) then
    vim.notify(
      string.format("[theme] '%s' not in available list: %s",
        theme_name, table.concat(M.available, ", ")),
      vim.log.levels.ERROR
    )
    return
  end
  M.config.theme            = theme_name
  vim.g._nvim_active_theme  = theme_name
  _cache.hour  = nil
  _cache.value = nil
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
      _cache.manual_override = nil
      _cache.hour  = nil
      _cache.value = nil
    end
  end,
  desc = "Clear theme.lua cache when external :colorscheme fires",
})

return M
