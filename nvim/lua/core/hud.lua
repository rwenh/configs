-- lua/core/hud.lua
-- Synthwave accent layer — overlaid on any theme
local M = {}

local overrides = {
  -- Neon line numbers
  LineNr         = { fg = "#3d5a6e" },
  CursorLineNr   = { fg = "#7dcfff", bold = true },

  -- Floating windows — glass panel feel
  NormalFloat    = { bg = "#0d1117" },
  FloatBorder    = { fg = "#7aa2f7", bg = "#0d1117" },
  FloatTitle     = { fg = "#bb9af7", bold = true },

  -- Telescope — cyberpunk search
  TelescopeNormal        = { bg = "#0d1117" },
  TelescopeBorder        = { fg = "#7aa2f7", bg = "#0d1117" },
  TelescopePromptBorder  = { fg = "#bb9af7", bg = "#16161e" },
  TelescopePromptNormal  = { bg = "#16161e" },
  TelescopePromptPrefix  = { fg = "#ff9e64" },
  TelescopeResultsTitle  = { fg = "#0d1117", bg = "#7aa2f7" },
  TelescopePreviewTitle  = { fg = "#0d1117", bg = "#bb9af7" },
  TelescopeSelectionCaret = { fg = "#ff9e64" },

  -- Treesitter context — subtle neon underline
  TreesitterContextBottom = { underline = true, sp = "#7aa2f7" },

  -- DAP — hot breakpoints
  DapBreakpoint   = { fg = "#f7768e" },
  DapStopped      = { fg = "#9ece6a", bold = true },
  DapStoppedLine  = { bg = "#1a2b1a" },

  -- Dashboard
  DashboardHeader = { fg = "#7aa2f7" },
  DashboardFooter = { fg = "#565f89" },

  -- Indent lines — barely visible grid
  IblIndent      = { fg = "#1e2030" },
  IblScope       = { fg = "#3d59a1" },

  -- Which-key panel
  WhichKeyBorder  = { fg = "#7aa2f7" },
  WhichKeyGroup   = { fg = "#bb9af7" },
  WhichKeyDesc    = { fg = "#c0caf5" },
  WhichKeySeparator = { fg = "#3d59a1" },
}

function M.apply()
  for group, attrs in pairs(overrides) do
    pcall(vim.api.nvim_set_hl, 0, group, attrs)
  end
end

return M
