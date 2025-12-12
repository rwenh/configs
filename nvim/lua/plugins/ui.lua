-- ~/.config/nvim/lua/plugins/ui.lua

return {
  {
    "maxmx03/solarized.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.o.termguicolors = true
      
      require("solarized").setup({
        transparent = { enabled = false },
        palette = "solarized",
        styles = { comments = { italic = true } },
        plugins = { all = true },
      })
      
      vim.o.background = "dark"
      vim.cmd.colorscheme("solarized")
      
      local theme = require("core.theme")
      vim.defer_fn(theme.apply_highlights, 10)
      theme.init()
    end,
  },

  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  {
    "akinsho/bufferline.nvim",
    version = "*",
    event = "VeryLazy",
    dependencies = "nvim-tree/nvim-web-devicons",
  },

  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    config = function()
      local notify = require("notify")
      notify.setup({
        background_colour = "#002b36",
        fps = 60,
        render = "compact",
        timeout = 2000,
        top_down = false,
        stages = "fade",
      })
      vim.notify = notify
    end,
  },

  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    config = function()
      require("noice").setup({
        lsp = {
          override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
          },
          signature = { enabled = false },
        },
        presets = {
          bottom_search = true,
          command_palette = true,
          long_message_to_split = true,
          lsp_doc_border = true,
        },
        views = {
          cmdline_popup = {
            position = { row = "50%", col = "50%" },
            size = { width = 60, height = "auto" },
          },
        },
      })
    end,
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    config = function()
      require("which-key").setup({
        preset = "modern",
        delay = 300,
        icons = { mappings = false },
        spec = {
          { "<leader>f", group = "Find" },
          { "<leader>g", group = "Git" },
          { "<leader>r", group = "Run" },
          { "<leader>d", group = "Debug" },
          { "<leader>t", group = "Test/Terminal" },
          { "<leader>b", group = "Buffer" },
          { "<leader>c", group = "Code" },
          { "<leader>s", group = "Split" },
          { "<leader>x", group = "Trouble" },
        },
      })
    end,
  },

  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics" },
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics" },
      { "<leader>xs", "<cmd>Trouble symbols toggle<cr>", desc = "Symbols" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<cr>", desc = "Quickfix" },
    },
    config = function()
      require("trouble").setup({
        use_diagnostic_signs = true,
      })
    end,
  },

  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  {
    "akinsho/toggleterm.nvim",
    version = "*",
    keys = {
      { "<C-\\>", "<cmd>ToggleTerm<cr>", desc = "Terminal" },
      { "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Float Terminal" },
      { "<leader>th", "<cmd>ToggleTerm direction=horizontal<cr>", desc = "Horizontal Terminal" },
    },
    config = function()
      require("toggleterm").setup({
        direction = "float",
        float_opts = { border = "curved" },
        shade_terminals = true,
      })
    end,
  },

  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    config = function()
      require("persistence").setup()
    end,
    keys = {
      { "<leader>qs", function() require("persistence").load() end, desc = "Restore Session" },
      { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Last Session" },
    },
  },

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main = "ibl",
  },

  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    config = function()
      require("dressing").setup({
        input = { relative = "editor" },
        select = { backend = { "telescope", "builtin" } },
      })
    end,
  },

  {
    "lewis6991/satellite.nvim",
    event = "VeryLazy",
    config = function()
      require("satellite").setup({
        current_only = false,
        winblend = 50,
      })
    end,
  },
}