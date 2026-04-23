-- lua/plugins/init.lua - Lazy plugin manager setup
--
-- OPT (v2.3.14):
--   • Duplicate lazy.nvim clone block removed. core/bootstrap.lua (step 1
--     of init.lua) is the sole authoritative clone site. By the time this
--     file is required (step 6), lazy.nvim is guaranteed to be present or
--     the user has already been notified of the failure in bootstrap.lua.
--     Keeping a second clone here produced divergent error messages and made
--     the failure path harder to reason about.

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

-- rtp prepend is still here (not in bootstrap.lua) to avoid double-prepend
-- on subsequent startups where the path is already in rtp.
vim.opt.rtp:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
  vim.notify(
    "[plugins/init.lua] Failed to load lazy.nvim.\n"
      .. "Delete ~/.local/share/nvim and restart Neovim to reinstall.",
    vim.log.levels.ERROR
  )
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
