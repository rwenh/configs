-- lua/core/focus.lua
-- Focus mode: collapse all chrome, keep only the text
-- Toggle with <leader>uF — goes deeper than ZenMode alone

local M = {}

local _state = {
  active       = false,
  laststatus   = nil,
  showtabline  = nil,
  signcolumn   = nil,
  ruler        = nil,
  number       = nil,
  relativenumber = nil,
  cursorline   = nil,
}

function M.enter()
  if _state.active then return end
  _state.active = true

  -- Snapshot
  _state.laststatus    = vim.o.laststatus
  _state.showtabline   = vim.o.showtabline
  _state.ruler         = vim.o.ruler
  _state.signcolumn    = vim.wo.signcolumn
  _state.number        = vim.wo.number
  _state.relativenumber = vim.wo.relativenumber
  _state.cursorline    = vim.wo.cursorline

  -- Strip everything
  vim.o.laststatus    = 0
  vim.o.showtabline   = 0
  vim.o.ruler         = false
  vim.wo.signcolumn   = "no"
  vim.wo.number       = false
  vim.wo.relativenumber = false
  vim.wo.cursorline   = false

  -- Enable Twilight + Zen
  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")

  vim.notify("Focus mode", vim.log.levels.INFO)
end

function M.exit()
  if not _state.active then return end
  _state.active = false

  vim.o.laststatus    = _state.laststatus    or 3
  vim.o.showtabline   = _state.showtabline   or 1
  vim.o.ruler         = _state.ruler         ~= nil and _state.ruler or false
  vim.wo.signcolumn   = _state.signcolumn    or "yes:1"
  vim.wo.number       = _state.number        ~= nil and _state.number or true
  vim.wo.relativenumber = _state.relativenumber ~= nil and _state.relativenumber or true
  vim.wo.cursorline   = _state.cursorline    ~= nil and _state.cursorline or true

  pcall(vim.cmd, "Twilight")
  pcall(vim.cmd, "ZenMode")

  vim.notify("Focus off", vim.log.levels.INFO)
end

function M.toggle()
  if _state.active then M.exit() else M.enter() end
end

return M
