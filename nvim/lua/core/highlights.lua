-- lua/core/highlights.lua — synthwave accent highlight overrides
--
-- Applied after every ColorScheme event so overrides survive theme toggles.
-- All color values are TokyoNight-tuned but declared as named constants so
-- adapting to other dark themes is a single-edit operation.
--

local M = {}

-- ── Color constants ───────────────────────────────────────────────────────────
-- FIX D1: named constants replace repeated hex literals.
-- To adapt for a different dark theme, change the values here only.
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

-- ── Override table ────────────────────────────────────────────────────────────
local overrides = {
  -- Line numbers
  LineNr       = { fg = GREY },
  CursorLineNr = { fg = CYAN, bold = true },

  -- Floating windows — glass panel
  NormalFloat  = { bg = BG },
  FloatBorder  = { fg = BLUE,   bg = BG },
  FloatTitle   = { fg = PURPLE, bold = true },

  -- Telescope
  TelescopeNormal        = { bg = BG },
  TelescopeBorder        = { fg = BLUE,   bg = BG },
  TelescopePromptBorder  = { fg = PURPLE, bg = "#16161e" },
  TelescopePromptNormal  = { bg = "#16161e" },
  TelescopePromptPrefix  = { fg = ORANGE },
  TelescopeResultsTitle  = { fg = BG,     bg = BLUE   },
  TelescopePreviewTitle  = { fg = BG,     bg = PURPLE },
  TelescopeSelectionCaret = { fg = ORANGE },

  -- Treesitter context
  TreesitterContextBottom = { underline = true, sp = BLUE },

  -- DAP
  DapBreakpoint  = { fg = RED  },
  DapStopped     = { fg = GREEN, bold = true },
  DapStoppedLine = { bg = "#1a2b1a" },

  -- Indent guides
  IblIndent = { fg = DIM   },
  IblScope  = { fg = SCOPE },

  -- Which-key
  WhichKeyBorder    = { fg = BLUE   },
  WhichKeyGroup     = { fg = PURPLE },
  WhichKeyDesc      = { fg = "#c0caf5" },
  WhichKeySeparator = { fg = SCOPE  },
}

-- ── apply() ───────────────────────────────────────────────────────────────────

function M.apply()
  local ok, err = pcall(function()
    for group, attrs in pairs(overrides) do
      vim.api.nvim_set_hl(0, group, attrs)
    end
  end)
  if not ok then
    vim.notify("[hud] highlight override failed: " .. tostring(err),
      vim.log.levels.WARN)
  end
end

-- ── Auto-reapply on ColorScheme ───────────────────────────────────────────────
-- After a theme toggle (:<leader>ut) the ColorScheme event fires and a new
-- colorscheme resets all highlights — the overrides were lost until restart.
-- Registering apply() on ColorScheme ensures they survive every theme change.
vim.api.nvim_create_autocmd("ColorScheme", {
  group    = vim.api.nvim_create_augroup("HighlightOverrides", { clear = true }),
  callback = M.apply,
  desc     = "Re-apply highlight overrides after theme change",
})

return M
