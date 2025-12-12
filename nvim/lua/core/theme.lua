-- ~/.config/nvim/lua/core/theme.lua
-- Solarized colorscheme with auto-switching

local M = {}

-- Solarized color palette
M.colors = {
  base03  = "#002b36",
  base02  = "#073642",
  base01  = "#586e75",
  base00  = "#657b83",
  base0   = "#839496",
  base1   = "#93a1a1",
  base2   = "#eee8d5",
  base3   = "#fdf6e3",
  yellow  = "#b58900",
  orange  = "#cb4b16",
  red     = "#dc322f",
  magenta = "#d33682",
  violet  = "#6c71c4",
  blue    = "#268bd2",
  cyan    = "#2aa198",
  green   = "#859900",
}

-- Convert hex to RGB number
local function hex_to_rgb(hex)
  if type(hex) ~= "string" or not hex:match("^#%x%x%x%x%x%x$") then
    return 0
  end
  
  local r = tonumber(hex:sub(2, 3), 16)
  local g = tonumber(hex:sub(4, 5), 16)
  local b = tonumber(hex:sub(6, 7), 16)
  
  if not (r and g and b) then return 0 end
  return (r * 65536) + (g * 256) + b
end

-- Apply custom Solarized highlights
function M.apply_highlights()
  local c = M.colors
  
  -- Syntax highlights
  local syntax_groups = {
    { groups = { "Function", "@function", "@function.call", "@method", "Type", "@type", "Keyword", "@keyword" },
      opts = { fg = hex_to_rgb(c.green), bold = false } },
    { groups = { "Constant", "@constant", "Number", "@number", "String", "@string" },
      opts = { fg = hex_to_rgb(c.cyan) } },
    { groups = { "Comment", "@comment" },
      opts = { fg = hex_to_rgb(c.base01), italic = true } },
    { groups = { "@variable", "Identifier" },
      opts = { fg = hex_to_rgb(c.base0) } },
    { groups = { "@variable.builtin", "Special" },
      opts = { fg = hex_to_rgb(c.red) } },
    { groups = { "Operator", "@operator", "Delimiter" },
      opts = { fg = hex_to_rgb(c.base01) } },
    { groups = { "PreProc", "@preproc", "Include", "Macro" },
      opts = { fg = hex_to_rgb(c.orange) } },
  }
  
  -- UI highlights
  local ui_highlights = {
    NormalFloat = { bg = hex_to_rgb(c.base02), fg = hex_to_rgb(c.base0) },
    FloatBorder = { bg = hex_to_rgb(c.base02), fg = hex_to_rgb(c.base01) },
    Pmenu = { bg = hex_to_rgb(c.base02), fg = hex_to_rgb(c.base0) },
    PmenuSel = { bg = hex_to_rgb(c.base01), fg = hex_to_rgb(c.base2), bold = true },
    DiagnosticError = { fg = hex_to_rgb(c.red) },
    DiagnosticWarn = { fg = hex_to_rgb(c.yellow) },
    DiagnosticInfo = { fg = hex_to_rgb(c.blue) },
    DiagnosticHint = { fg = hex_to_rgb(c.cyan) },
    DapBreakpoint = { fg = hex_to_rgb(c.red), bold = true },
    DapStopped = { fg = hex_to_rgb(c.green), bold = true },
    TelescopeNormal = { bg = hex_to_rgb(c.base03) },
    TelescopeSelection = { fg = hex_to_rgb(c.base1), bg = hex_to_rgb(c.base02) },
    NvimTreeNormal = { bg = hex_to_rgb(c.base03) },
    NvimTreeDirectoryIcon = { fg = hex_to_rgb(c.blue) },
  }
  
  -- Apply syntax groups
  for _, group_def in ipairs(syntax_groups) do
    for _, group in ipairs(group_def.groups) do
      vim.api.nvim_set_hl(0, group, group_def.opts)
    end
  end
  
  -- Apply UI highlights
  for group, opts in pairs(ui_highlights) do
    vim.api.nvim_set_hl(0, group, opts)
  end
end

-- Auto-switching state
M.auto_switching_enabled = true
M.theme_timer = nil

-- Get theme based on time of day
function M.get_theme_for_time()
  local hour = tonumber(os.date("%H"))
  return (hour >= 6 and hour < 18) and "light" or "dark"
end

-- Auto-switch theme
function M.auto_switch()
  if not M.auto_switching_enabled then return end
  
  local auto_bg = M.get_theme_for_time()
  local current_bg = vim.o.background
  
  if auto_bg ~= current_bg then
    vim.o.background = auto_bg
    pcall(vim.cmd.colorscheme, "solarized")
    vim.schedule(function()
      M.apply_highlights()
      vim.cmd("doautocmd ColorScheme")
    end)
    vim.notify("Auto-switched to Solarized " .. auto_bg .. " mode", vim.log.levels.INFO)
  end
end

-- Setup auto theme switching
function M.setup_auto_switching()
  if M.theme_timer then
    pcall(function()
      M.theme_timer:stop()
      M.theme_timer:close()
    end)
    M.theme_timer = nil
  end
  
  vim.o.background = M.get_theme_for_time()
  
  M.theme_timer = vim.uv.new_timer()
  if M.theme_timer then
    M.theme_timer:start(0, 1800000, function()  -- Check every 30 minutes
      vim.schedule(M.auto_switch)
    end)
  end
end

-- Initialize theme
function M.init()
  M.setup_auto_switching()
  
  -- Create autocmd for colorscheme changes
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("SolarizedEnforcement", { clear = true }),
    pattern = "solarized",
    callback = function()
      vim.defer_fn(M.apply_highlights, 50)
    end,
  })
  
  -- Cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
      if M.theme_timer then
        pcall(function()
          M.theme_timer:stop()
          M.theme_timer:close()
        end)
      end
    end,
  })
end

return M
