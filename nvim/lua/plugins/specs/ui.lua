-- lua/plugins/specs/ui.lua - UI plugins

return {
  -- Colorscheme
  {
    "maxmx03/solarized.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("solarized").setup()
      vim.cmd.colorscheme("solarized")
    end,
  },

  -- Statusline
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        theme = "auto",
        globalstatus = true,
        component_separators = { left = "|", right = "|" },
        section_separators = { left = "", right = "" },
      },
      sections = {
        lualine_c = { { "filename", path = 1 }, "diagnostics" },
        lualine_x = { "encoding", "fileformat", "filetype" },
      },
    },
  },

  -- Bufferline
  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        mode = "buffers",
        diagnostics = "nvim_lsp",
        offsets = { { filetype = "NvimTree", text = "File Explorer" } },
      },
    },
  },

  -- Indent guides
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main = "ibl",
    opts = {
      indent = { char = "│" },
      scope = { enabled = true },
      exclude = { filetypes = { "help", "lazy", "mason", "notify" } },
    },
  },

  -- Notifications
  {
    "rcarriga/nvim-notify",
    lazy = false,
    opts = {
      timeout = 3000,
      stages = "fade",
      render = "compact",
      top_down = false,
    },
    init = function()
      vim.notify = require("notify")
    end,
  },

  -- Better UI elements
  {
    "stevearc/dressing.nvim",
    lazy = true,
    opts = {},
  },

  -- Trouble (diagnostics)
  {
    "folke/trouble.nvim",
    cmd = "Trouble",
    opts = {},
  },

  -- Which-key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "modern",
      spec = {
        { "<leader>b", group = "buffer" },
        { "<leader>c", group = "code" },
        { "<leader>d", group = "debug" },
        { "<leader>f", group = "find" },
        { "<leader>g", group = "git" },
        { "<leader>r", group = "run" },
        { "<leader>s", group = "search" },
        { "<leader>t", group = "test" },
        { "<leader>u", group = "ui" },
        { "<leader>x", group = "trouble" },
      },
    },
  },

  -- Terminal
  {
    "akinsho/toggleterm.nvim",
    cmd = "ToggleTerm",
    opts = {
      size = 15,
      open_mapping = [[<C-\>]],
      hide_numbers = true,
      direction = "float",
      float_opts = { border = "curved" },
    },
  },

  -- Dashboard
  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    opts = {
      theme = "doom",
      config = {
        header = {
          "",
          " ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗",
          " ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║",
          " ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║",
          " ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║",
          " ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║",
          " ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝",
          "",
        },
        center = {
          { icon = " ", desc = "Find File", action = "Telescope find_files" },
          { icon = " ", desc = "Recent Files", action = "Telescope oldfiles" },
          { icon = " ", desc = "Find Text", action = "Telescope live_grep" },
          { icon = " ", desc = "Config", action = "edit $MYVIMRC" },
          { icon = " ", desc = "Lazy", action = "Lazy" },
          { icon = " ", desc = "Quit", action = "qa" },
        },
      },
    },
  },
}