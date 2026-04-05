-- lua/plugins/init.lua - Lazy plugin manager setup

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  vim.notify("Installing lazy.nvim...", vim.log.levels.INFO)

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
    return
  end
end

vim.opt.rtp:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
  vim.notify("Failed to load lazy.nvim", vim.log.levels.ERROR)
  return
end

local setup_ok = pcall(function()
  lazy.setup("plugins.specs", {
    defaults = {
      lazy    = true,
      version = false,
    },
    performance = {
      reset_packpath = true,
      rtp = {
        reset = true,
        paths = {},
      },
    },
    checker = {
      enabled   = true,
      notify    = true,
      frequency = 3600,
    },
    change_detection = {
      enabled = true,
      notify  = true,
    },
    ui = {
      size   = { width = 0.8, height = 0.8 },
      wrap   = true,
      border = "rounded",
      icons  = {
        cmd     = "⌘",
        config  = "🔧",
        event   = "📅",
        ft      = "📂",
        init    = "⚙",
        keys    = "🔑",
        plugin  = "🔌",
        runtime = "💻",
        require = "🌙",
        source  = "📄",
        start   = "🚀",
        task    = "📋",
        lazy    = "💤",
      },
    },
    debug = false,
  })
end)

if not setup_ok then
  vim.notify("lazy.nvim setup failed", vim.log.levels.ERROR)
end
