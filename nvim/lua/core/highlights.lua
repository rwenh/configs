-- lua/core/highlights.lua — theme- and background-aware highlight overrides
--
-- Fixes vs v2.5.0: the tokyonight overrides hard-coded DARK hex values and were
-- applied even when background=light (day mode is the default 07:00–19:00), giving
-- dark floats on a light theme. Palettes are now chosen per vim.o.background.
--
-- An override entry may be a table OR a function(palette, bg) → table.

local M = {}

local PALETTE = {
  dark = {
    BG = "#0d1117", BLUE = "#7aa2f7", PURPLE = "#bb9af7", ORANGE = "#ff9e64",
    CYAN = "#7dcfff", GREY = "#3d5a6e", DIM = "#1e2030", SCOPE = "#3d59a1",
    GREEN = "#9ece6a", RED = "#f7768e", PROMPT = "#16161e", FG = "#c0caf5",
    STOPPED_LINE = "#1a2b1a",
  },
  light = {   -- tokyonight "day"
    BG = "#d0d5e3", BLUE = "#2e7de9", PURPLE = "#9854f1", ORANGE = "#b15c00",
    CYAN = "#007197", GREY = "#8990b3", DIM = "#c4c8da", SCOPE = "#92a6d5",
    GREEN = "#587539", RED = "#f52a65", PROMPT = "#c4c8da", FG = "#3760bf",
    STOPPED_LINE = "#d5e5d0",
  },
}

---@param bg string?  defaults to vim.o.background
---@return table
function M.palette(bg)
  return PALETTE[bg or vim.o.background] or PALETTE.dark
end

local _builtin = {
  __default = function(P)
    return {
      DapBreakpoint  = { fg = P.RED },
      DapLogPoint    = { fg = P.CYAN },
      DapStopped     = { fg = P.GREEN, bold = true },
      DapStoppedLine = { bg = P.STOPPED_LINE },
    }
  end,

  tokyonight = function(P)
    return {
      LineNr                  = { fg = P.GREY },
      CursorLineNr            = { fg = P.CYAN,   bold = true },
      NormalFloat             = { bg = P.BG },
      FloatBorder             = { fg = P.BLUE,   bg = P.BG },
      FloatTitle              = { fg = P.PURPLE, bold = true },
      TelescopeNormal         = { bg = P.BG },
      TelescopeBorder         = { fg = P.BLUE,   bg = P.BG },
      TelescopePromptBorder   = { fg = P.PURPLE, bg = P.PROMPT },
      TelescopePromptNormal   = { bg = P.PROMPT },
      TelescopePromptPrefix   = { fg = P.ORANGE },
      TelescopeResultsTitle   = { fg = P.BG,     bg = P.BLUE   },
      TelescopePreviewTitle   = { fg = P.BG,     bg = P.PURPLE },
      TelescopeSelectionCaret = { fg = P.ORANGE },
      TreesitterContextBottom = { underline = true, sp = P.BLUE },
      IblIndent               = { fg = P.DIM   },
      IblScope                = { fg = P.SCOPE },
      WhichKeyBorder          = { fg = P.BLUE   },
      WhichKeyGroup           = { fg = P.PURPLE },
      WhichKeyDesc            = { fg = P.FG },
      WhichKeySeparator       = { fg = P.SCOPE  },
    }
  end,

  catppuccin = {
    FloatBorder             = { fg = "#89b4fa" },
    FloatTitle              = { fg = "#cba6f7", bold = true },
    TelescopeBorder         = { fg = "#89b4fa" },
    TelescopePromptBorder   = { fg = "#cba6f7" },
    TelescopePromptPrefix   = { fg = "#fab387" },
    TelescopeResultsTitle   = { fg = "#1e1e2e", bg = "#89b4fa" },
    TelescopePreviewTitle   = { fg = "#1e1e2e", bg = "#cba6f7" },
    IblIndent               = { fg = "#313244" },
    IblScope                = { fg = "#585b70" },
    WhichKeyBorder          = { fg = "#89b4fa" },
    WhichKeyGroup           = { fg = "#cba6f7" },
  },

  ["rose-pine"] = {
    FloatBorder             = { fg = "#31748f" },
    FloatTitle              = { fg = "#c4a7e7", bold = true },
    TelescopeBorder         = { fg = "#31748f" },
    TelescopePromptBorder   = { fg = "#c4a7e7" },
    TelescopePromptPrefix   = { fg = "#ebbcba" },
    IblIndent               = { fg = "#21202e" },
    IblScope                = { fg = "#403d52" },
    WhichKeyGroup           = { fg = "#c4a7e7" },
  },

  kanagawa = {
    FloatBorder             = { fg = "#7e9cd8" },
    FloatTitle              = { fg = "#957fb8", bold = true },
    TelescopeBorder         = { fg = "#7e9cd8" },
    TelescopePromptBorder   = { fg = "#957fb8" },
    TelescopePromptPrefix   = { fg = "#ffa066" },
    IblIndent               = { fg = "#1f1f28" },
    IblScope                = { fg = "#363646" },
    WhichKeyGroup           = { fg = "#957fb8" },
  },

  ["gruvbox-material"] = {
    FloatBorder             = { fg = "#7daea3" },
    FloatTitle              = { fg = "#d3869b", bold = true },
    TelescopeBorder         = { fg = "#7daea3" },
    TelescopePromptBorder   = { fg = "#d3869b" },
    TelescopePromptPrefix   = { fg = "#e78a4e" },
    IblIndent               = { fg = "#282828" },
    IblScope                = { fg = "#3c3836" },
    WhichKeyGroup           = { fg = "#d3869b" },
  },

  solarized = {
    FloatBorder           = { fg = "#268bd2" },
    FloatTitle            = { fg = "#6c71c4", bold = true },
    TelescopeBorder       = { fg = "#268bd2" },
    TelescopePromptBorder = { fg = "#6c71c4" },
    TelescopePromptPrefix = { fg = "#cb4b16" },
    IblIndent             = { fg = "#073642" },
    IblScope              = { fg = "#0d4a57" },
    WhichKeyGroup         = { fg = "#6c71c4" },
  },

  ["solarized-osaka"] = {
    FloatBorder           = { fg = "#268bd2" },
    FloatTitle            = { fg = "#6c71c4", bold = true },
    TelescopeBorder       = { fg = "#268bd2" },
    TelescopePromptBorder = { fg = "#6c71c4" },
    TelescopePromptPrefix = { fg = "#cb4b16" },
    IblIndent             = { fg = "#073642" },
    IblScope              = { fg = "#0d4a57" },
    WhichKeyGroup         = { fg = "#6c71c4" },
  },
}

local _canonical_keys_by_length = (function()
  local keys = {}
  for key in pairs(_builtin) do
    if key ~= "__default" then table.insert(keys, key) end
  end
  table.sort(keys, function(a, b) return #a > #b end)
  return keys
end)()

local function resolve_canonical(theme)
  if _builtin[theme] then return theme end
  for _, key in ipairs(_canonical_keys_by_length) do
    if theme:find(key, 1, true) then return key end
  end
  return theme
end

local _user_overrides = {}

---@param theme  string
---@param groups table|function  groups, or function(palette, bg) → groups
function M.register(theme, groups)
  if type(theme) ~= "string" or (type(groups) ~= "table" and type(groups) ~= "function") then
    vim.notify(
      "[highlights] register(): expected (string, table|function), got ("
      .. type(theme) .. ", " .. type(groups) .. ")",
      vim.log.levels.WARN
    )
    return
  end
  -- Stack entries (not deep-merge) so function and table overrides can coexist.
  _user_overrides[theme] = _user_overrides[theme] or {}
  table.insert(_user_overrides[theme], groups)
end

function M.apply()
  if vim.g.disable_highlight_overrides then return end

  local bg        = vim.o.background == "light" and "light" or "dark"
  local P         = M.palette(bg)
  local theme     = tostring(vim.g._nvim_active_theme or "")
  local canonical = resolve_canonical(theme)

  local merged = {}
  local function merge_into(entry)
    if type(entry) == "function" then
      local ok, res = pcall(entry, P, bg)
      entry = ok and res or nil
    end
    if type(entry) == "table" then
      for group, attrs in pairs(entry) do
        merged[group] = vim.tbl_extend("force", merged[group] or {}, attrs)
      end
    end
  end

  merge_into(_builtin.__default)
  merge_into(_builtin[canonical])
  for _, key in ipairs({ "__default", canonical }) do
    for _, entry in ipairs(_user_overrides[key] or {}) do merge_into(entry) end
  end

  local failed = {}
  for group, attrs in pairs(merged) do
    local ok, err = pcall(vim.api.nvim_set_hl, 0, group, attrs)
    if not ok then table.insert(failed, string.format("  %s: %s", group, tostring(err))) end
  end
  if #failed > 0 then
    vim.notify(
      string.format("[highlights] %d group(s) failed to apply:\n%s", #failed, table.concat(failed, "\n")),
      vim.log.levels.WARN
    )
  end
end

vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("HighlightOverrides", { clear = true }),
  callback = function()
    local ok, t = pcall(require, "core.theme")
    if ok and t.get_active_theme then
      vim.g._nvim_active_theme = t.get_active_theme()
    end
    M.apply()
  end,
  desc = "Re-apply highlight overrides after theme change",
})

return M
