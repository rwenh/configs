-- lua/core/theme.lua - Theme management

local M = {}

function M.setup()
  local hour = tonumber(os.date("%H"))
  vim.o.background = (hour >= 7 and hour < 19) and "light" or "dark"
  
  -- Try solarized, fallback to default
  local ok = pcall(vim.cmd, "colorscheme solarized")
  if not ok then
    vim.cmd. colorscheme("default")
  end
end

function M.toggle()
  vim.o.background = vim.o.background == "dark" and "light" or "dark"
  local ok = pcall(vim.cmd, "colorscheme solarized")
  if not ok then
    vim. cmd.colorscheme("default")
  end
end

M. setup()
return M