-- lua/plugins/init.lua — lazy.nvim setup
--

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

-- rtp prepend lives here (not in bootstrap.lua) to avoid double-prepend on
-- subsequent launches where the path is already present.
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

local setup_ok, setup_err = pcall(function()
  lazy.setup("plugins.specs", {
    defaults = {
      lazy = true,

      -- A handful of specs override this with explicit version pins:
      --   blink.cmp      → "1.*"   (API changed significantly on 1.x)
      --   rustaceanvim   → "^5"    (v5 renamed commands)
      --   bufferline.nvim → "*"    (semver-stable)
      -- When adding a new plugin, prefer version = false unless the plugin
      -- has documented breaking-change releases.
      version = false,
    },

    performance = {
      reset_packpath = true,
      rtp = {
        reset = true,
      },
    },

    checker = {
      enabled   = true,
      notify    = false,
      frequency = 3600,
    },

    change_detection = {
      enabled = true,
      notify  = false,
    },

    ui = {
      size   = { width = 0.8, height = 0.8 },
      wrap   = true,
      border = "rounded",
      icons  = {
        cmd     = " ",
        config  = " ",
        event   = " ",
        ft      = " ",
        init    = " ",
        keys    = " ",
        plugin  = " ",
        runtime = " ",
        require = "󰢱 ",
        source  = " ",
        start   = " ",
        task    = " ",
        lazy    = "󰒲 ",
      },
    },

    debug = false,
  })
end)

if not setup_ok then
  vim.notify(
    "lazy.nvim setup failed:\n" .. tostring(setup_err),
    vim.log.levels.ERROR
  )
end
