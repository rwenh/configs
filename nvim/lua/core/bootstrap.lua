-- lua/core/bootstrap.lua - Bootstrap configuration (loads FIRST)
-- Responsibility: set leader keys + clone lazy.nvim if missing.
-- rtp prepend happens in plugins/init.lua to avoid double-prepend.

-- Leader keys MUST be set before any plugin loading
vim.g.mapleader      = " "
vim.g.maplocalleader = " "

-- Clone lazy.nvim if not present
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  vim.notify("Bootstrapping lazy.nvim — cloning from GitHub…", vim.log.levels.INFO)

  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })

  if vim.v.shell_error ~= 0 then
    vim.notify(
      "Failed to clone lazy.nvim:\n" .. tostring(out)
        .. "\n\nTroubleshooting:\n"
        .. "1. Check your internet connection\n"
        .. "2. Verify git is installed: git --version\n"
        .. "3. Check git config: git config --list\n",
      vim.log.levels.ERROR
    )
    return
  end

  vim.notify("lazy.nvim bootstrap successful!", vim.log.levels.INFO)
end

-- FIX: rtp prepend moved to plugins/init.lua.
-- Previously both bootstrap.lua and plugins/init.lua called rtp:prepend(lazypath),
-- inserting lazy.nvim twice into &runtimepath. Double entries confuse module
-- resolution on some systems. plugins/init.lua is the sole owner of prepend.
