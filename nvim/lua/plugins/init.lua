-- lua/plugins/init.lua - Lazy plugin manager setup

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

-- Bootstrap lazy.nvim
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

-- Setup lazy with specs
require("lazy").setup({
  spec = "plugins.specs",
  defaults = { lazy = true, version = false },
  install = { colorscheme = { "solarized" } },
  checker = { enabled = true, notify = false },
  change_detection = { enabled = true, notify = false },
  performance = {
    cache = { enabled = true },
    rtp = {
      disabled_plugins = {
        "gzip", "matchit", "matchparen", "netrwPlugin",
        "tarPlugin", "tohtml", "tutor", "zipPlugin",
      },
    },
  },
  ui = {
    border = "rounded",
    title = "Claude's IDE",
  },
})