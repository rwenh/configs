-- lua/plugins/specs/ui.lua - UI plugins (all themes pre-configured, pick in core/theme.lua)

return {

  -- ┌─────────────────────────────────────────────────────┐
  -- │                    THEMES                            │
  -- └─────────────────────────────────────────────────────┘

  { "catppuccin/nvim",            name = "catppuccin",       lazy = false, priority = 1000 },
  { "folke/tokyonight.nvim",      lazy = false, priority = 1000,
    opts = { style = "moon", transparent = false } },
  { "rose-pine/neovim",           name = "rose-pine",        lazy = false, priority = 1000 },
  { "rebelot/kanagawa.nvim",      lazy = false, priority = 1000 },
  { "sainnhe/gruvbox-material",   lazy = false, priority = 1000,
    init = function() vim.g.gruvbox_material_better_performance = 1 end },
  { "maxmx03/solarized.nvim",     lazy = false, priority = 1000 },
  { "craftzdog/solarized-osaka.nvim", lazy = false, priority = 1000,
    opts = { transparent = false } },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                   STATUS / BUFFER                    │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        theme = "auto",
        globalstatus = true,
        component_separators = { left = "|", right = "|" },
        section_separators   = { left = "", right = "" },
        disabled_filetypes   = { statusline = { "dashboard", "alpha" } },
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = { "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
    },
  },

  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        mode        = "buffers",
        diagnostics = "nvim_lsp",
        offsets     = { { filetype = "NvimTree", text = "Explorer", highlight = "Directory" } },
        show_buffer_close_icons = true,
        show_close_icon         = false,
        separator_style         = "slant",
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                      UI ELEMENTS                     │
  -- └─────────────────────────────────────────────────────┘

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main  = "ibl",
    opts  = {
      indent  = { char = "│" },
      scope   = { enabled = true },
      exclude = { filetypes = { "help", "lazy", "mason", "notify", "dashboard" } },
    },
  },

  {
    "rcarriga/nvim-notify",
    lazy = false,
    opts = {
      timeout  = 3000,
      stages   = "fade_in_slide_out",
      render   = "compact",
      top_down = false,
      max_width = 60,
    },
    init = function()
      vim.notify = require("notify")
    end,
  },

  {
    "stevearc/dressing.nvim",
    lazy = true,
    opts = {},
  },

  {
    "folke/trouble.nvim",
    cmd  = "Trouble",
    opts = { use_diagnostic_signs = true },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts  = {
      preset = "modern",
      spec   = {
        { "<leader>b",  group = "buffer" },
        { "<leader>c",  group = "code" },
        { "<leader>d",  group = "debug" },
        { "<leader>f",  group = "find" },
        { "<leader>g",  group = "git" },
        { "<leader>r",  group = "run/rust" },
        { "<leader>s",  group = "search" },
        { "<leader>t",  group = "test/theme" },
        { "<leader>u",  group = "ui" },
        { "<leader>x",  group = "trouble" },
      },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    cmd  = "ToggleTerm",
    opts = {
      size         = 15,
      open_mapping = [[<C-\>]],
      hide_numbers = true,
      direction    = "float",
      float_opts   = { border = "curved" },
      on_open = function()
        vim.opt_local.number         = false
        vim.opt_local.relativenumber = false
      end,
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                     DASHBOARD                        │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvimdev/dashboard-nvim",
    lazy     = false,
    priority = 90,
    opts = {
      theme = "doom",
      config = {
        header = {
          "",
          "  ██████╗ ███████╗ █████╗ ██╗     ██╗████████╗██╗   ██╗",
          "  ██╔══██╗██╔════╝██╔══██╗██║     ██║╚══██╔══╝╚██╗ ██╔╝",
          "  ██████╔╝█████╗  ███████║██║     ██║   ██║    ╚████╔╝ ",
          "  ██╔══██╗██╔══╝  ██╔══██║██║     ██║   ██║     ╚██╔╝  ",
          "  ██║  ██║███████╗██║  ██║███████╗██║   ██║      ██║   ",
          "  ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝   ╚═╝      ╚═╝   ",
          "",
          "  « Reality is merely code we haven't debugged yet »",
          "",
        },
        center = {
          { icon = "  ", desc = "Find File       ", action = "Telescope find_files",   key = "f" },
          { icon = "  ", desc = "Recent Files    ", action = "Telescope oldfiles",      key = "r" },
          { icon = "  ", desc = "Find Text       ", action = "Telescope live_grep",     key = "g" },
          { icon = "  ", desc = "Sessions        ", action = "SessionRestore",          key = "s" },
          { icon = "  ", desc = "Config          ", action = "edit $MYVIMRC",           key = "c" },
          { icon = "  ", desc = "Theme           ", action = "lua require('core.theme').toggle()", key = "t" },
          { icon = "󰒲  ", desc = "Lazy            ", action = "Lazy",                   key = "l" },
          { icon = "  ", desc = "Quit            ", action = "qa",                      key = "q" },
        },
        footer = function()
          local ok, lazy = pcall(require, "lazy")
          if ok then
            local s = lazy.stats()
            return { string.format("⚡ %d plugins ready", s.count) }
          end
          return {}
        end,
      },
    },
  },
}
