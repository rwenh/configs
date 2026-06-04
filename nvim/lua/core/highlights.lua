-- lua/core/highlights.lua — theme-aware highlight overrides
--

local M = {}

-- ── Color constants (TokyoNight palette) ──────────────────────────────────────
local BG     = "#0d1117"
local BLUE   = "#7aa2f7"
local PURPLE = "#bb9af7"
local ORANGE = "#ff9e64"
local CYAN   = "#7dcfff"
local GREY   = "#3d5a6e"
local DIM    = "#1e2030"
local SCOPE  = "#3d59a1"
local GREEN  = "#9ece6a"
local RED    = "#f7768e"

-- ── Built-in per-theme override tables ────────────────────────────────────────
--
-- Each key is a theme name as stored in vim.g._nvim_active_theme.

local _builtin = {
  -- DAP signs: applied for every theme.
  __default = {
    DapBreakpoint  = { fg = RED },
    DapStopped     = { fg = GREEN, bold = true },
    DapStoppedLine = { bg = "#1a2b1a" },
  },

  -- TokyoNight: full accent palette.
  tokyonight = {
    LineNr                  = { fg = GREY },
    CursorLineNr            = { fg = CYAN,   bold = true },
    NormalFloat             = { bg = BG },
    FloatBorder             = { fg = BLUE,   bg = BG },
    FloatTitle              = { fg = PURPLE, bold = true },
    TelescopeNormal         = { bg = BG },
    TelescopeBorder         = { fg = BLUE,   bg = BG },
    TelescopePromptBorder   = { fg = PURPLE, bg = "#16161e" },
    TelescopePromptNormal   = { bg = "#16161e" },
    TelescopePromptPrefix   = { fg = ORANGE },
    TelescopeResultsTitle   = { fg = BG,     bg = BLUE   },
    TelescopePreviewTitle   = { fg = BG,     bg = PURPLE },
    TelescopeSelectionCaret = { fg = ORANGE },
    TreesitterContextBottom = { underline = true, sp = BLUE },
    IblIndent               = { fg = DIM   },
    IblScope                = { fg = SCOPE },
    WhichKeyBorder          = { fg = BLUE   },
    WhichKeyGroup           = { fg = PURPLE },
    WhichKeyDesc            = { fg = "#c0caf5" },
    WhichKeySeparator       = { fg = SCOPE  },
  },

  -- Catppuccin: subtle border accents using mocha palette.
  catppuccin = {
    FloatBorder             = { fg = "#89b4fa" },   -- blue
    FloatTitle              = { fg = "#cba6f7", bold = true },   -- mauve
    TelescopeBorder         = { fg = "#89b4fa" },
    TelescopePromptBorder   = { fg = "#cba6f7" },
    TelescopePromptPrefix   = { fg = "#fab387" },   -- peach
    TelescopeResultsTitle   = { fg = "#1e1e2e", bg = "#89b4fa" },
    TelescopePreviewTitle   = { fg = "#1e1e2e", bg = "#cba6f7" },
    IblIndent               = { fg = "#313244" },
    IblScope                = { fg = "#585b70" },
    WhichKeyBorder          = { fg = "#89b4fa" },
    WhichKeyGroup           = { fg = "#cba6f7" },
  },

  -- Rose-pine: earthy tones.
  ["rose-pine"] = {
    FloatBorder             = { fg = "#31748f" },   -- pine
    FloatTitle              = { fg = "#c4a7e7", bold = true },   -- iris
    TelescopeBorder         = { fg = "#31748f" },
    TelescopePromptBorder   = { fg = "#c4a7e7" },
    TelescopePromptPrefix   = { fg = "#ebbcba" },   -- rose
    IblIndent               = { fg = "#21202e" },
    IblScope                = { fg = "#403d52" },
    WhichKeyGroup           = { fg = "#c4a7e7" },
  },

  -- Kanagawa: ink-wash accents.
  kanagawa = {
    FloatBorder             = { fg = "#7e9cd8" },   -- crystalBlue
    FloatTitle              = { fg = "#957fb8", bold = true },   -- oniViolet
    TelescopeBorder         = { fg = "#7e9cd8" },
    TelescopePromptBorder   = { fg = "#957fb8" },
    TelescopePromptPrefix   = { fg = "#ffa066" },   -- surimiOrange
    IblIndent               = { fg = "#1f1f28" },
    IblScope                = { fg = "#363646" },
    WhichKeyGroup           = { fg = "#957fb8" },
  },

  -- Gruvbox-material: warm amber.
  ["gruvbox-material"] = {
    FloatBorder             = { fg = "#7daea3" },   -- aqua
    FloatTitle              = { fg = "#d3869b", bold = true },   -- purple
    TelescopeBorder         = { fg = "#7daea3" },
    TelescopePromptBorder   = { fg = "#d3869b" },
    TelescopePromptPrefix   = { fg = "#e78a4e" },   -- orange
    IblIndent               = { fg = "#282828" },
    IblScope                = { fg = "#3c3836" },
    WhichKeyGroup           = { fg = "#d3869b" },
  },
}

-- ── User-registered overrides ─────────────────────────────────────────────────
-- Shape: { [theme_name] = { GroupName = {attrs}, ... } }

local _user_overrides = {}

-- ── Public: M.register ────────────────────────────────────────────────────────
--
-- Usage (from any lang spec or plugin config):
--   require("core.highlights").register("tokyonight", {
--     VhdlSignal = { fg = "#7aa2f7" },
--   })
--   require("core.highlights").register("__default", {
--     MyCustomSign = { fg = "#ff0000" },
--   })
--
---@param theme    string  theme name or "__default" for all themes
---@param groups   table   { GroupName = { attrs } }
function M.register(theme, groups)
  if type(theme) ~= "string" or type(groups) ~= "table" then
    vim.notify(
      "[highlights] register(): expected (string, table), got ("
      .. type(theme) .. ", " .. type(groups) .. ")",
      vim.log.levels.WARN
    )
    return
  end
  _user_overrides[theme] = vim.tbl_deep_extend(
    "force", _user_overrides[theme] or {}, groups
  )
end

-- ── Public: M.apply ───────────────────────────────────────────────────────────

function M.apply()
  if vim.g.disable_highlight_overrides then return end

  local theme = tostring(vim.g._nvim_active_theme or "")

  -- Resolve the canonical theme name: "tokyonight-moon" → "tokyonight".
  local canonical = theme
  for key in pairs(_builtin) do
    if key ~= "__default" and theme:find(key, 1, true) then
      canonical = key
      break
    end
  end

  -- Build the merged set: __default < named builtin < user __default < user named.
  local merged = {}

  local function merge_into(tbl)
    if type(tbl) == "table" then
      for group, attrs in pairs(tbl) do
        merged[group] = vim.tbl_extend("force", merged[group] or {}, attrs)
      end
    end
  end

  merge_into(_builtin.__default)
  merge_into(_builtin[canonical])
  merge_into(_user_overrides.__default)
  merge_into(_user_overrides[canonical])

  local ok, err = pcall(function()
    for group, attrs in pairs(merged) do
      vim.api.nvim_set_hl(0, group, attrs)
    end
  end)

  if not ok then
    vim.notify("[highlights] override failed: " .. tostring(err), vim.log.levels.WARN)
  end
end

-- ── Auto-reapply on ColorScheme ───────────────────────────────────────────────
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
