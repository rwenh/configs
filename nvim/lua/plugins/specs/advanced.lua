-- lua/plugins/specs/advanced.lua - Advanced features

return {
  -- Escape without timeout delay (solves the timeoutlen lag on 'j' when using jk/kj mappings)
  -- Native inoremap jk <Esc> still causes Neovim to wait `timeoutlen` ms after every 'j'.
  -- better-escape.nvim intercepts at the Lua level and fires immediately — zero perceived lag.
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      timeout = 200,
      default_mappings = false,
      mappings = {
        i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
        v = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
      },
    },
  },

  -- Symbol navigation (breadcrumb context in winbar)
  {
    "SmiteshP/nvim-navic",
    dependencies = "neovim/nvim-lspconfig",
    opts = {
      separator   = " > ",
      highlight   = true,
      depth_limit = 5,
    },
  },

  -- Colorizer
  {
    "NvChad/nvim-colorizer.lua",
    event = "BufReadPre",
    opts = {
      filetypes = { "*", "!lazy" },
      user_default_options = {
        RGB      = true,
        RRGGBB   = true,
        RRGGBBAA = true,
        css      = true,
        tailwind = true,
        mode     = "background",
      },
    },
  },

  -- Rainbow brackets
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = "VeryLazy",
  },

  -- Motion repeat (enables '.' for plugin maps)
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },

  -- File type icons
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true,
  },
}
