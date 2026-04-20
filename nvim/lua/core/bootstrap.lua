-- lua/core/bootstrap.lua - Bootstrap configuration (loads FIRST)
-- Responsibility: set leader keys + version stamp + clone lazy.nvim if missing.
-- rtp prepend happens in plugins/init.lua to avoid double-prepend.
--
-- FIX (v2.3.12):
--   • vim.g.nvim_ide_version moved here from init.lua step 10.
--     init.lua sets it AFTER require("plugins") (step 6), which means
--     ui.lua's config() reads it at plugin-load time and always saw nil
--     → fell back to "2.3.5" hardcoded in the or-fallback. Version string
--     was never shown correctly on the dashboard. Moving it to bootstrap.lua
--     (the very first require) guarantees it is set before any plugin config
--     runs.

-- ── Version ───────────────────────────────────────────────────────────────
vim.g.nvim_ide_version = "2.3.12"

-- ── Leader keys (must precede any plugin loading) ─────────────────────────
vim.g.mapleader      = " "
vim.g.maplocalleader = " "

-- ── Clone lazy.nvim if not present ────────────────────────────────────────
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
