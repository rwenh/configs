-- ~/.config/nvim/lua/core/lazy.lua
-- Bootstrap and configure lazy.nvim plugin manager

local function safe_path_join(...)
  local parts = {...}
  if vim.fs and vim.fs.joinpath then
    return vim.fs.joinpath(unpack(parts))
  else
    return table.concat(parts, vim.fn.has('win32') == 1 and '\\' or '/')
  end
end

local lazypath = safe_path_join(vim.fn.stdpath("data"), "lazy", "lazy.nvim")

-- Bootstrap lazy.nvim if not installed
if not vim.uv.fs_stat(lazypath) then
  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath
  })

  if vim.v.shell_error ~= 0 then
    vim.schedule(function()
      vim.notify("Failed to install lazy.nvim: " .. tostring(out), vim.log.levels.ERROR)
    end)
    return
  end
end

vim.opt.rtp:prepend(lazypath)

-- Note: Plugin specifications are loaded from plugins/init.lua
-- This file only handles the lazy.nvim bootstrap
