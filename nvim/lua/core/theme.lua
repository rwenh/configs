-- ~/.config/nvim/lua/core/theme.lua

local M = {}

M.colors = {
  base03  = "#002b36", base02  = "#073642", base01  = "#586e75", base00  = "#657b83",
  base0   = "#839496", base1   = "#93a1a1", base2   = "#eee8d5", base3   = "#fdf6e3",
  yellow  = "#b58900", orange  = "#cb4b16", red     = "#dc322f", magenta = "#d33682",
  violet  = "#6c71c4", blue    = "#268bd2", cyan    = "#2aa198", green   = "#859900",
}

local function rgb(hex)
  if not hex or type(hex) ~= "string" then
    return 0
  end
  local r, g, b = hex:match("#(..)(..)(..)")
  if not r or not g or not b then
    return 0
  end
  return tonumber(r, 16) * 65536 + tonumber(g, 16) * 256 + tonumber(b, 16)
end

function M.apply_highlights()
  local c = M.colors
  local hl = vim.api.nvim_set_hl

  -- Syntax
  hl(0, "Function", { fg = rgb(c.blue), bold = true })
  hl(0, "Type", { fg = rgb(c.yellow), bold = true })
  hl(0, "Keyword", { fg = rgb(c.green), bold = true })
  hl(0, "String", { fg = rgb(c.cyan) })
  hl(0, "Number", { fg = rgb(c.violet) })
  hl(0, "Comment", { fg = rgb(c.base01), italic = true })
  hl(0, "Operator", { fg = rgb(c.green) })
  hl(0, "Constant", { fg = rgb(c.cyan) })
  hl(0, "Special", { fg = rgb(c.red) })

  -- UI
  hl(0, "NormalFloat", { bg = rgb(c.base02) })
  hl(0, "FloatBorder", { fg = rgb(c.base01), bg = rgb(c.base02) })
  hl(0, "Pmenu", { bg = rgb(c.base02) })
  hl(0, "PmenuSel", { bg = rgb(c.base01), bold = true })
  hl(0, "PmenuThumb", { bg = rgb(c.base00) })
  hl(0, "StatusLine", { fg = rgb(c.base0), bg = rgb(c.base02) })
  hl(0, "WinSeparator", { fg = rgb(c.base01) })

  -- Diagnostics
  hl(0, "DiagnosticError", { fg = rgb(c.red) })
  hl(0, "DiagnosticWarn", { fg = rgb(c.yellow) })
  hl(0, "DiagnosticInfo", { fg = rgb(c.blue) })
  hl(0, "DiagnosticHint", { fg = rgb(c.cyan) })
  hl(0, "DiagnosticUnderlineError", { sp = rgb(c.red), underline = true })
  hl(0, "DiagnosticUnderlineWarn", { sp = rgb(c.yellow), underline = true })
  hl(0, "DiagnosticUnderlineInfo", { sp = rgb(c.blue), underline = true })
  hl(0, "DiagnosticUnderlineHint", { sp = rgb(c.cyan), underline = true })

  -- Git
  hl(0, "GitSignsAdd", { fg = rgb(c.green) })
  hl(0, "GitSignsChange", { fg = rgb(c.yellow) })
  hl(0, "GitSignsDelete", { fg = rgb(c.red) })
  hl(0, "DiffAdd", { bg = rgb("#073642"), fg = rgb(c.green) })
  hl(0, "DiffChange", { bg = rgb("#073642"), fg = rgb(c.yellow) })
  hl(0, "DiffDelete", { bg = rgb("#073642"), fg = rgb(c.red) })

  -- Telescope
  hl(0, "TelescopeNormal", { bg = rgb(c.base03) })
  hl(0, "TelescopeBorder", { fg = rgb(c.base01), bg = rgb(c.base03) })
  hl(0, "TelescopeSelection", { bg = rgb(c.base02), bold = true })
  hl(0, "TelescopeMatching", { fg = rgb(c.cyan), bold = true })

  -- NvimTree
  hl(0, "NvimTreeNormal", { bg = rgb(c.base03) })
  hl(0, "NvimTreeFolderIcon", { fg = rgb(c.blue) })
  hl(0, "NvimTreeGitNew", { fg = rgb(c.green) })
  hl(0, "NvimTreeGitDirty", { fg = rgb(c.yellow) })
  hl(0, "NvimTreeGitDeleted", { fg = rgb(c.red) })

  -- Indent
  hl(0, "IndentBlanklineChar", { fg = rgb(c.base02) })
  hl(0, "IndentBlanklineContextChar", { fg = rgb(c.base01) })

  -- Cursor
  hl(0, "CursorLine", { bg = rgb(c.base02) })
  hl(0, "CursorLineNr", { fg = rgb(c.yellow), bold = true })
  hl(0, "LineNr", { fg = rgb(c.base01) })

  -- Search
  hl(0, "Search", { bg = rgb(c.yellow), fg = rgb(c.base03), bold = true })
  hl(0, "IncSearch", { bg = rgb(c.orange), fg = rgb(c.base03), bold = true })

  -- Visual
  hl(0, "Visual", { bg = rgb(c.base02) })
  hl(0, "VisualNOS", { bg = rgb(c.base02) })
end

-- FIXED: Optional auto-theme switching based on time
function M.init()
  -- Only auto-switch if explicitly enabled
  if vim.g.nvim_ide_auto_theme then
    local hour = tonumber(os.date("%H"))
    vim.o.background = (hour >= 6 and hour < 18) and "light" or "dark"
  else
    -- Default to dark theme
    vim.o.background = vim.o.background or "dark"
  end

  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("ThemeOverride", { clear = true }),
    pattern = "solarized",
    callback = function()
      vim.defer_fn(M.apply_highlights, 10)
    end,
  })
end

-- Helper to switch theme
function M.toggle()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
  M.apply_highlights()
end

-- Set light theme
function M.set_light()
  vim.o.background = "light"
  pcall(vim.cmd.colorscheme, "solarized")
  M.apply_highlights()
end

-- Set dark theme
function M.set_dark()
  vim.o.background = "dark"
  pcall(vim.cmd.colorscheme, "solarized")
  M.apply_highlights()
end

return M
