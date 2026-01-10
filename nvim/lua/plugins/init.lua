-- lua/plugins/init.lua - Lazy plugin manager setup

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