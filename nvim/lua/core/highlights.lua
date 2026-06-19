-- lua/core/highlights.lua — theme-aware highlight overrides
--

local M = {}

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

local _builtin = {
  __default = {
    DapBreakpoint  = { fg = RED },
    DapStopped     = { fg = GREEN, bold = true },
    DapStoppedLine = { bg = "#1a2b1a" },
  },

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
}

local _user_overrides = {}

---@param theme  string
---@param groups table
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

function M.apply()
  if vim.g.disable_highlight_overrides then return end

  local theme = tostring(vim.g._nvim_active_theme or "")

  local canonical = theme
  for key in pairs(_builtin) do
    if key ~= "__default" and theme:find(key, 1, true) then
      canonical = key
      break
    end
  end

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

  local failed = {}
  for group, attrs in pairs(merged) do
    local ok, err = pcall(vim.api.nvim_set_hl, 0, group, attrs)
    if not ok then
      table.insert(failed, { group = group, err = tostring(err) })
    end
  end

  if #failed > 0 then
    local msgs = {}
    for _, f in ipairs(failed) do
      table.insert(msgs, string.format("  %s: %s", f.group, f.err))
    end
    vim.notify(
      string.format(
        "[highlights] %d group(s) failed to apply:\n%s",
        #failed, table.concat(msgs, "\n")
      ),
      vim.log.levels.DEBUG
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
