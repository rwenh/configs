-- lua/plugins/init.lua - Lazy plugin manager setup

require("lazy").setup({
  spec     = "plugins.specs",
  defaults = {
    lazy = true,
    -- FIX #3: Removed version = false from defaults.
    -- version = false globally overrides per-spec version constraints —
    -- meaning rest.nvim's explicit `version = "^2"` pin in rest.lua was
    -- silently ignored and HEAD was installed instead. Removing this default
    -- restores per-spec pinning. Plugins without a version spec still track
    -- HEAD (Lazy's own default when version is unset).
  },
  install  = { colorscheme = { "tokyonight" } },
  checker  = {
    enabled = true,
    notify  = false,  -- checks for updates silently; run :Lazy update to apply
  },
  change_detection = { enabled = true, notify = false },
  performance = {
    cache = { enabled = true },
    rtp   = {
      -- NOTE: Several entries here overlap with g.loaded_* globals set in
      -- options.lua (gzip, matchit, matchparen, netrwPlugin, tarPlugin,
      -- zipPlugin). The g.loaded_* approach is evaluated earlier (before rtp
      -- is walked) and is the primary disable mechanism. This list is a
      -- complementary second layer via Lazy's rtp injection.
      --
      -- FIX #2: Added "getscript", "vimball", "tohtml" to align with the full
      -- set disabled by g.loaded_* in options.lua. "tutor" kept — no
      -- g.loaded_tutor counterpart, Lazy is the sole disabler for it.
      disabled_plugins = {
        "gzip",
        "getscript",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "vimball",
        "zipPlugin",
      },
    },
  },
  ui = {
    border = "rounded",
    -- NOTE: Leading icon requires a Nerd Font — renders as a box in
    -- standard fonts. Change to title = "Lazy" if not using Nerd Fonts.
    title  = "  Lazy",
  },
})
