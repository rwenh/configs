-- lua/core/bootstrap.lua — bootstrap configuration (loads FIRST)
--
-- Responsibilities:
--   1. Set leader keys (must precede any plugin loading).
--   2. Stamp vim.g.nvim_ide_version (must precede ui.lua dashboard read).
--   3. Stamp vim.g._nvim_active_theme so ui.lua theme_spec() can read
--      config.theme during lazy.nvim plugin-spec evaluation — before
--      require("core.theme").setup() is called in step 7 of init.lua.
--   4. Clone lazy.nvim if missing; clean up partial clones on failure.
--

local VERSION  = "2.4.1"
local LAZY_URL = "https://github.com/folke/lazy.nvim.git"

-- ── Version stamp (must precede plugin config() calls) ───────────────────────
vim.g.nvim_ide_version = VERSION

-- ── Leader keys (must precede any plugin loading) ────────────────────────────
vim.g.mapleader      = " "
vim.g.maplocalleader = " "

-- ── Theme stamp ───────────────────────────────────────────────────────────────
vim.g._nvim_active_theme = (function()
  local ok, t = pcall(require, "core.theme")
  return (ok and t.config and t.config.theme) or "tokyonight"
end)()

-- ── Clone lazy.nvim if not present ───────────────────────────────────────────
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  vim.notify("Bootstrapping lazy.nvim — cloning from GitHub…", vim.log.levels.INFO)

  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none",
    LAZY_URL, "--branch=stable", lazypath,
  })

  if vim.v.shell_error ~= 0 then
    vim.fn.delete(lazypath, "rf")

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
