-- lua/core/highlights.lua — synthwave accent highlight overrides
--

local M = {}

-- ── Color constants (TokyoNight palette) ──────────────────────────────────────
-- To adapt for a different dark theme change ONLY these values; apply() will
-- use them for any theme whose name starts with that theme's identifier string.
local BG     = "#0d1117"   -- float background  (deep dark)
local BLUE   = "#7aa2f7"   -- border / accent    (tokyonight blue)
local PURPLE = "#bb9af7"   -- title / group      (tokyonight purple)
local ORANGE = "#ff9e64"   -- prompt prefix
local CYAN   = "#7dcfff"   -- cursor line number
local GREY   = "#3d5a6e"   -- inactive line numbers
local DIM    = "#1e2030"   -- indent guides (barely visible)
local SCOPE  = "#3d59a1"   -- indent scope line
local GREEN  = "#9ece6a"   -- dap stopped
local RED    = "#f7768e"   -- dap breakpoint

-- ── Full TokyoNight override table ────────────────────────────────────────────
local tn_overrides = {
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
  DapBreakpoint           = { fg = RED },
  DapStopped              = { fg = GREEN, bold = true },
  DapStoppedLine          = { bg = "#1a2b1a" },
  IblIndent               = { fg = DIM   },
  IblScope                = { fg = SCOPE },
  WhichKeyBorder          = { fg = BLUE   },
  WhichKeyGroup           = { fg = PURPLE },
  WhichKeyDesc            = { fg = "#c0caf5" },
  WhichKeySeparator       = { fg = SCOPE  },
}

-- ── Minimal theme-agnostic overrides ─────────────────────────────────────────
-- These only set DAP sign
-- colours and the DapStoppedLine background — values that no standard theme
-- defines out of the box, so they are safe to apply universally.
local minimal_overrides = {
  DapBreakpoint  = { fg = RED },
  DapStopped     = { fg = GREEN, bold = true },
  DapStoppedLine = { bg = "#1a2b1a" },
}

-- ── apply() ───────────────────────────────────────────────────────────────────

function M.apply()
  if vim.g.disable_highlight_overrides then return end

  local theme    = tostring(vim.g._nvim_active_theme or "")
  local is_tokyonight = theme:find("tokyonight", 1, true) ~= nil

  local target = is_tokyonight and tn_overrides or minimal_overrides

  local ok, err = pcall(function()
    for group, attrs in pairs(target) do
      vim.api.nvim_set_hl(0, group, attrs)
    end
  end)
  if not ok then
    vim.notify("[highlights] override failed: " .. tostring(err),
      vim.log.levels.WARN)
  end
end

-- ── Auto-reapply on ColorScheme ───────────────────────────────────────────────
-- After a theme toggle (:<leader>ut) the ColorScheme event fires and resets all
-- highlights; re-running apply() here restores them for every theme change.
vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("HighlightOverrides", { clear = true }),
  callback = function()
    -- Sync the active-theme stamp so apply() uses the correct override set.
    local ok, t = pcall(require, "core.theme")
    if ok and t.get_active_theme then
      vim.g._nvim_active_theme = t.get_active_theme()
    end
    M.apply()
  end,
  desc = "Re-apply highlight overrides after theme change",
})

return M
