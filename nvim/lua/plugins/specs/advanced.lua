-- lua/plugins/specs/advanced.lua - Advanced features

return {
  -- Code runner
  {
    "CRAG666/code_runner.nvim",
    cmd = "RunCode",
    opts = {
      mode = "float",
      filetype = {
        python = "python3 -u",
        rust = "cd $dir && rustc $fileName && $dir/$fileNameWithoutExt",
        go = "go run",
        javascript = "node",
        typescript = "tsx",
        lua = "lua",
        sh = "bash",
      },
    },
  },

  -- Better escape
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      mapping = { "jk", "kj" },
      timeout = 200,
      clear_empty_lines = false,
      keys = "<Esc>",
    },
  },

  -- Symbol navigation
  {
    "SmiteshP/nvim-navic",
    dependencies = "neovim/nvim-lspconfig",
    opts = {
      separator = " > ",
      highlight = true,
      depth_limit = 5,
    },
  },

  -- Colorizer
  {
    "NvChad/nvim-colorizer.lua",
    event = "BufReadPre",
    opts = {
      filetypes = { "*", "! lazy" },
      user_default_options = {
        RGB = true,
        RRGGBB = true,
        RRGGBBAA = true,
        css = true,
        tailwind = true,
        mode = "background",
      },
    },
  },

  -- Rainbow brackets
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = "VeryLazy",
  },

  -- Motion repeat
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },

  -- File type icon colors
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true,
  },
}
