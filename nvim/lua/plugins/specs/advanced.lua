-- lua/plugins/specs/advanced.lua - Advanced features
-- NOTE: code_runner.nvim removed — core.util.runner handles file execution natively.

return {
  -- Better escape (v2.0+ API)
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      timeout = 200,
      default_mappings = false,
      mappings = {
        i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
      },
    },
  },

  -- Symbol navigation (breadcrumb context in lualine/winbar)
  {
    "SmiteshP/nvim-navic",
    dependencies = "neovim/nvim-lspconfig",
    opts = {
      separator  = " > ",
      highlight  = true,
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
