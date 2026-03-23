-- lua/core/bootstrap.lua - Bootstrap configuration (loads FIRST)
--
-- LOAD ORDER NOTE (issue #1): mapleader MUST be set before lazy.nvim is
-- required (done correctly below), and bootstrap.lua MUST load before
-- options.lua in init.lua. Current init.lua loads options first — safe as
-- long as options.lua contains no mappings. If options.lua ever gains a
-- mapping, swap the load order: bootstrap → options.

-- Set leader keys FIRST - before anything else!
vim.g.mapleader      = " "
vim.g.maplocalleader = " "

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  vim.notify("Bootstrapping lazy.nvim — cloning from GitHub…", vim.log.levels.INFO)

  -- FIX #2: Capture system() output so we can detect clone failures and
  -- surface a clear error instead of a cryptic rtp/module-not-found crash.
  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })

  if vim.v.shell_error ~= 0 then
    vim.notify(
      "Failed to clone lazy.nvim:\n" .. out
        .. "\nCheck your network connection and that git is installed.",
      vim.log.levels.ERROR
    )
    -- Return early — rtp:prepend on a missing path would cause confusing
    -- downstream errors.
    return
  end
end

vim.opt.rtp:prepend(lazypath)
