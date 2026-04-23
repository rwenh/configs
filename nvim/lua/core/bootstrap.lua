-- lua/core/bootstrap.lua - Bootstrap configuration (loads FIRST)
-- Responsibility: set leader keys + version stamp + clone lazy.nvim if missing.
-- rtp prepend happens in plugins/init.lua to avoid double-prepend.
--
-- OPT (v2.3.14):
--   • This is now the SOLE lazy.nvim clone site. The duplicate clone block
--     in plugins/init.lua has been removed. Having two clone paths produced
--     two distinct error messages and two slightly different fallback paths
--     that could diverge silently. bootstrap.lua runs first (step 1 of
--     init.lua), so by the time plugins/init.lua runs, lazy.nvim is already
--     guaranteed to be present or the user has been notified of failure.

-- ── Version ───────────────────────────────────────────────────────────────
-- Set before any plugin loads so ui.lua's dashboard version string is correct.
vim.g.nvim_ide_version = "2.3.14"

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
