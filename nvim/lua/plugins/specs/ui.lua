-- nvim/lua/plugins/specs/ui.lua
-- UI plugins with smart theme switching and comprehensive UI enhancements

-- Defer theme lookup into a function to capture current theme at plugin load time.
local function get_active()
  local ok, theme_config = pcall(function()
    return require("core.theme").config.theme
  end)
  return ok and theme_config or "tokyonight-moon"
end

local function theme_spec(plugin_name, name, extra)
  local derived = name or (plugin_name:match("/([^/]+)%.nvim$") or plugin_name:match("/([^/]+)$"))
  return vim.tbl_extend("force", {
    plugin_name,
    name     = name or nil,
    lazy     = get_active() ~= derived,
    priority = 1000,
  }, extra or {})
end

return {

  -- ┌─────────────────────────────────────────────────────┐
  -- │                    THEMES                            │
  -- └─────────────────────────────────────────────────────┘

  theme_spec("folke/tokyonight.nvim", "tokyonight-moon", {
    opts = {
      style           = "moon",
      transparent     = false,
      terminal_colors = true,
      styles = {
        comments  = { italic = true },
        keywords  = { italic = false },
        functions = {},
        variables = {},
        sidebars  = "dark",
        floats    = "dark",
      },
      sidebars                 = { "qf", "help", "lazy", "neo-tree", "oil" },
      day_brightness           = 0.3,
      hide_inactive_statusline = false,
      dim_inactive             = false,
      lualine_bold             = false,
    },
    config = function(_, opts)
      require("tokyonight").setup(opts)
      vim.cmd.colorscheme("tokyonight")
    end,
  }),

  theme_spec("catppuccin/nvim", "catppuccin", {
    opts = {
      flavour    = "mocha",
      background = { light = "latte", dark = "mocha" },
      transparent_background = false,
      show_end_of_buffer     = false,
      integrations = {
        aerial            = true,
        alpha             = true,
        cmp               = true,
        dap               = true,
        dap_ui            = true,
        dashboard         = true,
        fidget            = true,
        gitsigns          = true,
        illuminate        = true,
        indent_blankline  = true,
        lsp_trouble       = true,
        mason             = true,
        native_lsp        = true,
        navic             = true,
        neotree           = true,
        notify            = true,
        nvimtree          = true,
        overseer          = true,
        rainbow_delimiters = true,
        telescope         = true,
        treesitter        = true,
        ufo               = true,
        which_key         = true,
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin-mocha")
    end,
  }),

  theme_spec("rose-pine/neovim", "rose-pine", {
    opts = {
      dark_variant              = "main",
      disable_background        = false,
      disable_float_background  = false,
    },
    config = function(_, opts)
      require("rose-pine").setup(opts)
      vim.cmd.colorscheme("rose-pine")
    end,
  }),

  theme_spec("rebelot/kanagawa.nvim", "kanagawa", {
    opts = {
      theme      = "dragon",
      background = { dark = "dragon", light = "lotus" },
      transparent = false,
      overrides  = function(_colors) return {} end,
    },
    config = function(_, opts)
      require("kanagawa").setup(opts)
      vim.cmd.colorscheme("kanagawa-dragon")
    end,
  }),

  theme_spec("sainnhe/gruvbox-material", "gruvbox-material", {
    init = function()
      vim.g.gruvbox_material_better_performance = 1
      vim.g.gruvbox_material_background         = "hard"
      vim.g.gruvbox_material_foreground         = "material"
      vim.o.background                          = "dark"
    end,
    config = function()
      vim.cmd.colorscheme("gruvbox-material")
    end,
  }),

  theme_spec("maxmx03/solarized.nvim", "solarized", {
    opts = {
      theme                = "dark",
      enable_end_of_buffer = false,
    },
    config = function(_, opts)
      require("solarized").setup(opts)
      vim.cmd.colorscheme("solarized")
    end,
  }),

  theme_spec("craftzdog/solarized-osaka.nvim", "solarized-osaka", {
    opts = {
      transparent     = false,
      terminal_colors = true,
    },
    config = function(_, opts)
      require("solarized-osaka").setup(opts)
      vim.cmd.colorscheme("solarized-osaka")
    end,
  }),

  -- ┌─────────────────────────────────────────────────────┐
  -- │              STATUS LINE & BUFFER LINE               │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvim-lualine/lualine.nvim",
    event        = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme                = "auto",
        globalstatus         = true,
        component_separators = { left = "|", right = "|" },
        section_separators   = { left = "", right = "" },
        disabled_filetypes   = { statusline = { "dashboard", "alpha", "neo-tree", "oil" } },
        refresh              = { statusline = 1000, tabline = 1000, winbar = 1000 },
      },
      sections = {
        lualine_a = {
          { "mode", fmt = function(str) return str:sub(1, 1) end },
        },
        lualine_b = {
          "branch",
          {
            "diff",
            symbols  = { added = "  ", modified = "  ", removed = "  " },
            colored  = true,
            diff_color = {
              added    = { fg = "#90EE90" },
              modified = { fg = "#FFD700" },
              removed  = { fg = "#FF6B6B" },
            },
          },
          {
            "diagnostics",
            symbols = { error = "  ", warn = "  ", info = "  ", hint = "  " },
            colored = true,
          },
        },
        lualine_c = {
          {
            "filename",
            path    = 1,
            symbols = { modified = "  ", readonly = "  ", unnamed = " [No Name] " },
          },
        },
        lualine_x = {
          { "filetype", icons_enabled = true, icon = nil },
          "encoding",
          {
            "fileformat",
            icons_enabled = true,
            symbols       = { unix = "LF", dos = "CRLF", mac = "CR" },
          },
        },
        lualine_y = {
          { "progress", fmt = function(str) return str .. "  " end },
        },
        lualine_z = {
          { "location", fmt = function(str) return "Ln " .. str end },
        },
      },
      inactive_sections = {
        lualine_a = {}, lualine_b = {},
        lualine_c = { "filename" },
        lualine_x = { "location" },
        lualine_y = {}, lualine_z = {},
      },
    },
  },

  {
    "akinsho/bufferline.nvim",
    event        = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        mode             = "buffers",
        themable         = true,
        numbers          = "none",
        close_command    = "bdelete! %d",
        diagnostics      = "nvim_lsp",
        offsets = {
          { filetype = "NvimTree", text = "Explorer", highlight = "Directory", separator = true },
        },
        show_buffer_close_icons = true,
        show_close_icon         = false,
        separator_style         = "thin",
        always_show_bufferline  = true,
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                   NOTIFICATIONS                      │
  -- └─────────────────────────────────────────────────────┘

  {
    "rcarriga/nvim-notify",
    lazy = true,
    opts = {
      timeout      = 3000,
      max_height   = function() return math.floor(vim.o.lines * 0.75) end,
      max_width    = function() return math.floor(vim.o.columns * 0.75) end,
      on_open      = function(win)
        vim.api.nvim_win_set_config(win, { zindex = 100 })
      end,
      render       = "default",
      stages       = "fade_in_slide_out",
      top_down     = true,
    },
    config = function(_, opts)
      local notify = require("notify")
      notify.setup(opts)
      vim.notify = notify
    end,
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                   TROUBLE                            │
  -- └─────────────────────────────────────────────────────┘

  {
    "folke/trouble.nvim",
    cmd  = { "Trouble", "TroubleToggle" },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>",                        desc = "Diagnostics (Trouble)" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",           desc = "Buffer Diagnostics" },
      { "<leader>xL", "<cmd>Trouble loclist toggle<cr>",                            desc = "Location List" },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<cr>",                             desc = "Quickfix List" },
    },
    -- v3 flat opts — removed legacy v2 fields (focus, multiline, indent_guides)
    opts = {
      auto_close     = false,
      auto_preview   = true,
      auto_refresh   = true,
      focus          = false,
      restore        = true,
      icons          = {
        indent = { fold_open = " ", fold_closed = " " },
        folder_closed = " ", folder_open = " ",
      },
      modes = {
        diagnostics = { auto_open = false },
        lsp         = { auto_open = false },
      },
      win  = { type = "split", position = "bottom", size = 0.3 },
      keys = {
        ["?"]     = "help",
        ["r"]     = "refresh",
        ["R"]     = "toggle_refresh",
        ["q"]     = "close",
        ["o"]     = "jump_close",
        ["<esc>"] = "cancel",
        ["<cr>"]  = "jump",
        ["<tab>"] = "jump",
        ["<c-x>"] = "jump_split",
        ["<c-v>"] = "jump_vsplit",
        ["<c-t>"] = "jump_tab",
        ["za"]    = "fold_toggle",
        ["zo"]    = "fold_open",
        ["zc"]    = "fold_close",
        ["zM"]    = "fold_close_all",
        ["zR"]    = "fold_open_all",
        ["k"]     = "prev",
        ["j"]     = "next",
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                   WHICH-KEY                          │
  -- └─────────────────────────────────────────────────────┘

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    -- FIX: Removed init block that set vim.o.timeoutlen = 300.
    -- options.lua owns timeoutlen = 500 and which-key was silently overriding
    -- it at VeryLazy time. which-key reads timeoutlen from vim.o automatically.
    opts = {
      preset = "modern",
      delay  = function(ctx) return ctx.plugin and 0 or 200 end,
      notify = true,
      plugins = {
        marks     = true,
        registers = true,
        spelling  = { enabled = true, suggestions = 9 },
        presets   = {
          operators    = true,
          motions      = true,
          text_objects = true,
          windows      = true,
          nav          = true,
          z            = true,
          g            = true,
        },
      },
      win = {
        border  = "rounded",
        padding = { 1, 2 },
        winblend = 0,
        zindex  = 1000,
      },
      layout = {
        align   = "center",
        height  = { min = 4, max = 25 },
        spacing = 3,
        width   = { min = 20, max = 50 },
      },
      keys = { scroll_down = "<c-d>", scroll_up = "<c-u>" },
      spec = {
        { "<leader>b",  group = "buffer" },
        { "<leader>c",  group = "code" },
        { "<leader>e",  group = "explorer" },
        { "<leader>f",  group = "find" },
        { "<leader>h",  group = "harpoon" },
        { "<leader>r",  group = "run/rust" },
        { "<leader>s",  group = "split" },
        { "<leader>t",  group = "test/theme" },
        { "<leader>u",  group = "ui" },
        { "<leader>w",  group = "window" },
        { "<leader>x",  group = "utils" },
        { "<leader>,",  group = "lsp" },
        { "<leader>.",  group = "git-hunks" },
        { "<leader>;",  group = "debug" },
        { "<leader>'",  group = "run/test" },
        { "<leader>/",  group = "search/replace" },
        { "<leader>\\", group = "terminal" },
        -- Language groups
        { "<leader>py", group = "python" },
        { "<leader>rb", group = "ruby" },
        { "<leader>rs", group = "rust" },
        { "<leader>go", group = "go" },
        { "<leader>jv", group = "java" },
        { "<leader>ex", group = "elixir" },
        { "<leader>kt", group = "kotlin" },
        { "<leader>cc", group = "cpp/cmake" },
        { "<leader>vh", group = "vhdl" },
        { "<leader>ft", group = "fortran" },
        { "<leader>z",  group = "zig" },
        { "<leader>co", group = "cobol" },
        { "<leader>md", group = "markdown" },
        { "<leader>ts", group = "typescript" },
        { "<leader>jp", group = "js-packages" },
        { "<leader>db", group = "database" },
        { "<leader>re", group = "rest" },
        { "<leader>tc", group = "test-coverage" },
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                    TERMINAL                          │
  -- └─────────────────────────────────────────────────────┘

  {
    "akinsho/toggleterm.nvim",
    version = "*",
    cmd     = { "ToggleTerm", "TermExec", "TermSend" },
    opts    = {
      size = function(term)
        if term.direction == "horizontal" then return 15
        elseif term.direction == "vertical" then return vim.o.columns * 0.4
        end
      end,
      open_mapping = [[<C-\>]],
      on_open = function(term)
        vim.opt_local.number         = false
        vim.opt_local.relativenumber = false
        if term.direction == "float" then
          vim.keymap.set("t", "<ESC>", [[<C-\><C-n>]], { buffer = term.bufnr })
        end
      end,
      on_close              = function(_term) end,
      hide_numbers          = true,
      shade_filetypes       = {},
      shade_terminals       = true,
      shading_factor        = 2,
      start_in_insert       = true,
      insert_mappings       = true,
      terminal_mappings     = true,
      persist_size          = true,
      persist_mode          = true,
      close_on_exit         = true,
      shell                 = vim.o.shell,
      auto_scroll           = true,
      float_opts            = {
        border     = "curved",
        winblend   = 3,
        highlights = { border = "FloatBorder", background = "NormalFloat" },
      },
      winbar = {
        enabled        = false,
        name_formatter = function(term) return term.name end,
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                    DASHBOARD/SPLASH                  │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvimdev/dashboard-nvim",
    lazy         = false,
    priority     = 90,
    dependencies = { "nvim-tree/nvim-web-devicons" },
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
          "  « Configuration is poetry; execution is art »",
          "",
        },
        center = {
          { icon = "  ", desc = "Find File",    action = "Telescope find_files",  key = "f" },
          { icon = "  ", desc = "Recent Files", action = "Telescope oldfiles",    key = "r" },
          { icon = "  ", desc = "Find Text",    action = "Telescope live_grep",   key = "g" },
          {
            icon   = "  ",
            desc   = "Sessions",
            action = function()
              if pcall(require, "auto-session") then
                require("auto-session.session-lens").search_session()
              else
                vim.notify("auto-session not loaded", vim.log.levels.WARN)
              end
            end,
            key = "s",
          },
          {
            icon   = "  ",
            desc   = "Config",
            action = "edit " .. vim.fn.stdpath("config") .. "/init.lua",
            key    = "c",
          },
          {
            icon   = "  ",
            desc   = "Theme",
            action = function()
              if pcall(require, "core.theme") then
                require("core.theme").toggle()
              else
                vim.notify("Theme module not found", vim.log.levels.WARN)
              end
            end,
            key = "t",
          },
          { icon = "󰒲  ", desc = "Lazy",   action = "Lazy",        key = "l" },
          { icon = "  ", desc = "Health", action = "checkhealth", key = "h" },
          { icon = "  ", desc = "Quit",   action = "qa",          key = "q" },
        },
        footer = function()
          local stats = { os_time = os.date("  %A, %B %d, %Y | %H:%M:%S") }
          local ok, lazy = pcall(require, "lazy")
          if ok then
            local s = lazy.stats()
            stats.plugins = string.format("⚡ %d plugins loaded in %.2fms", s.count, s.startuptime)
          end
          return { stats.plugins or "", stats.os_time }
        end,
      },
    },
  },
}
