-- lua/plugins/specs/ui.lua
--
local _active_theme = vim.g._nvim_active_theme or "tokyonight"

local function theme_spec(plugin_name, name, extra)
  local is_active = (name == _active_theme)
    or (_active_theme == "tokyonight" and name and vim.startswith(name, "tokyonight"))
  return vim.tbl_extend("force", {
    plugin_name,
    name     = name or nil,
    lazy     = not is_active,
    priority = 1000,
  }, extra or {})
end

local LUALINE_MODE_ICONS = require("core.util.icons").modes

-- ── Dashboard header ─────────────────────────────────────────────────────────
local function build_header()
  local ver = tostring(vim.g.nvim_ide_version or "?")
  local logo = {
    "╔══════════════════════════════════════════════════════════════╗",
    "║  ███╗   ██╗██╗   ██╗██╗███╗   ███╗ ██╗██████╗ ███████╗     ║",
    "║  ████╗  ██║██║   ██║██║████╗ ████║ ██║██╔══██╗██╔════╝     ║",
    "║  ██╔██╗ ██║██║   ██║██║██╔████╔██║ ██║██║  ██║█████╗       ║",
    "║  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║ ██║██║  ██║██╔══╝       ║",
    "║  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║ ██║██████╔╝███████╗     ║",
    "║  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝ ╚═╝╚═════╝ ╚══════╝    ║",
    "║                                                              ║",
    string.format("║  [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]  v%-5s║", ver),
    "╚══════════════════════════════════════════════════════════════╝",
  }
  local header = table.concat(logo, "\n")
  local ok, Q = pcall(require, "core.util.quotes")
  if ok then
    local q   = Q.session()
    local fmt = Q.formatted(q)
    local indented = vim.tbl_map(
      function(l) return "  " .. l end,
      vim.split(fmt, "\n", { plain = true })
    )
    header = header .. "\n\n" .. table.concat(indented, "\n")
  end
  return header
end

return {

  -- ═══════════════════════════════════════════════════════════════════════════
  -- THEMES
  -- ═══════════════════════════════════════════════════════════════════════════

  theme_spec("folke/tokyonight.nvim", "tokyonight", {
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
      sidebars                 = { "qf", "help", "lazy", "mason", "notify" },
      day_brightness           = 0.3,
      hide_inactive_statusline = false,
      dim_inactive             = false,
      lualine_bold             = false,
    },
    config = function(_, opts)
      require("tokyonight").setup(opts)
      vim.cmd.colorscheme("tokyonight-moon")
    end,
  }),

  theme_spec("catppuccin/nvim", "catppuccin", {
    opts = {
      flavour    = "mocha",
      background = { light = "latte", dark = "mocha" },
      transparent_background = false,
      integrations = {
        cmp = true, dap = true, dap_ui = true,
        dashboard = true, fidget = true, gitsigns = true,
        indent_blankline = { enabled = true }, lsp_trouble = true,
        mason = true, native_lsp = { enabled = true },
        navic = { enabled = true }, neotree = true, notify = true,
        rainbow_delimiters = true, telescope = { enabled = true },
        treesitter = true, ufo = true, which_key = true,
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin-mocha")
    end,
  }),

  theme_spec("rose-pine/neovim", "rose-pine", {
    opts = {
      dark_variant             = "main",
      disable_background       = false,
      disable_float_background = false,
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
    config = function() vim.cmd.colorscheme("gruvbox-material") end,
  }),

  theme_spec("maxmx03/solarized.nvim", "solarized", {
    opts = { theme = "dark", enable_end_of_buffer = false },
    config = function(_, opts)
      require("solarized").setup(opts)
      vim.cmd.colorscheme("solarized")
    end,
  }),

  theme_spec("craftzdog/solarized-osaka.nvim", "solarized-osaka", {
    opts = { transparent = false, terminal_colors = true },
    config = function(_, opts)
      require("solarized-osaka").setup(opts)
      vim.cmd.colorscheme("solarized-osaka")
    end,
  }),

  -- ═══════════════════════════════════════════════════════════════════════════
  -- STATUSLINE
  -- ═══════════════════════════════════════════════════════════════════════════

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
        disabled_filetypes   = {
          statusline = {
            "dashboard", "alpha", "neo-tree", "oil", "snacks_dashboard",
          },
        },
        refresh = { statusline = 1000, tabline = 1000, winbar = 1000 },
      },
      sections = {
        lualine_a = {
          {
            "mode",
            fmt = function(str)
              return LUALINE_MODE_ICONS[str] or str:sub(1, 1)
            end,
          },
        },
        lualine_b = {
          "branch",
          {
            "diff",
            symbols    = require("core.util.icons").git,
            colored    = true,
            diff_color = {
              added    = { fg = "#90EE90" },
              modified = { fg = "#FFD700" },
              removed  = { fg = "#FF6B6B" },
            },
          },
          {
            "diagnostics",
            symbols = require("core.util.icons").diagnostics,
            colored = true,
          },
        },
        lualine_c = {
          { "filename", path = 1,
            symbols = { modified = "  ", readonly = "  ", unnamed = " [No Name] " } },
        },
        lualine_x = {
          { "filetype",   icons_enabled = true, icon = nil },
          "encoding",
          { "fileformat", icons_enabled = true,
            symbols = { unix = "LF", dos = "CRLF", mac = "CR" } },
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

  -- ═══════════════════════════════════════════════════════════════════════════
  -- BUFFERLINE
  -- ═══════════════════════════════════════════════════════════════════════════

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
        diagnostics      = false,
        offsets          = {
          { filetype = "NvimTree", text = "Explorer",
            highlight = "Directory", separator = true },
        },
        show_buffer_close_icons = true,
        show_close_icon         = false,
        separator_style         = "thin",
        always_show_bufferline  = true,
      },
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- NOTIFICATIONS
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    opts  = {
      timeout    = 3000,
      max_height = function() return math.floor(vim.o.lines   * 0.75) end,
      max_width  = function() return math.floor(vim.o.columns * 0.75) end,
      on_open    = function(win)
        vim.api.nvim_win_set_config(win, { zindex = 100 })
      end,
      render   = "default",
      stages   = "fade_in_slide_out",
      top_down = true,
    },
    config = function(_, opts)
      local notify = require("notify")
      notify.setup(opts)
      vim.notify = notify
    end,
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- DIAGNOSTICS PANEL (Trouble)
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "folke/trouble.nvim",
    cmd  = { "Trouble" },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>",              desc = "Diagnostics (Trouble)" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics"   },
      { "<leader>xL", "<cmd>Trouble loclist toggle<cr>",                  desc = "Location List"         },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<cr>",                   desc = "Quickfix List"         },
    },
    opts = {
      auto_close   = false,
      auto_preview = true,
      auto_refresh = true,
      focus        = false,
      restore      = true,
      icons        = {
        indent        = { fold_open = " ", fold_closed = " " },
        folder_closed = " ",
        folder_open   = " ",
      },
      modes = {
        diagnostics = { auto_open = false },
        lsp         = { auto_open = false },
      },
      win  = { type = "split", position = "bottom", size = 0.3 },
      keys = {
        ["?"]     = "help",           ["r"]     = "refresh",
        ["R"]     = "toggle_refresh", ["q"]     = "close",
        ["o"]     = "jump_close",     ["<esc>"] = "cancel",
        ["<cr>"]  = "jump",           ["<tab>"] = "jump",
        ["<c-x>"] = "jump_split",     ["<c-v>"] = "jump_vsplit",
        ["<c-t>"] = "jump_tab",       ["za"]    = "fold_toggle",
        ["zo"]    = "fold_open",      ["zc"]    = "fold_close",
        ["zM"]    = "fold_close_all", ["zR"]    = "fold_open_all",
        ["k"]     = "prev",           ["j"]     = "next",
      },
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- WHICH-KEY
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts  = {
      preset = "modern",
      delay  = function(ctx) return ctx.plugin and 0 or 200 end,
      notify = true,
      plugins = {
        marks     = true,
        registers = true,
        spelling  = { enabled = true, suggestions = 9 },
        presets   = {
          operators    = true, motions      = true,
          text_objects = true, windows      = true,
          nav          = true, z            = true, g = true,
        },
      },
      win    = { border = "rounded", padding = { 1, 2 }, zindex = 1000 },
      layout = { align = "center", height = { min = 4, max = 25 },
                 spacing = 3, width = { min = 20, max = 50 } },
      keys   = { scroll_down = "<c-d>", scroll_up = "<c-u>" },
      spec   = {
        { "<leader>b",  group = "buffer"       },
        { "<leader>e",  group = "explorer"     },
        { "<leader>f",  group = "find"         },
        { "<leader>h",  group = "harpoon"      },
        { "<leader>o",  group = "overseer"     },
        { "<leader>q",  group = "session"      },
        { "<leader>s",  group = "split"        },
        { "<leader>t",  group = "test/theme"   },
        { "<leader>u",  group = "ui"           },
        { "<leader>w",  group = "window"       },
        { "<leader>x",  group = "utils"        },
        { "<leader>,",  group = "lsp"          },
        { "<leader>.",  group = "git"          },
        { "<leader>;",  group = "debug"        },
        { "<leader>'",  group = "run/test"     },
        { "<leader>/",  group = "search"       },
        { "<leader>\\", group = "terminal"     },
        { "<leader>uF", desc  = "Deep focus mode" },
        { "<leader>py", group = "python"       },
        { "<leader>rb", group = "ruby"         },
        { "<leader>go", group = "go"           },
        { "<leader>jv", group = "java"         },
        { "<leader>ex", group = "elixir"       },
        { "<leader>kt", group = "kotlin"       },
        { "<leader>cc", group = "cpp/cmake"    },
        { "<leader>vh", group = "vhdl"         },
        { "<leader>ft", group = "fortran"      },
        { "<leader>z",  group = "zig"          },
        { "<leader>co", group = "cobol"        },
        { "<leader>ts", group = "typescript"   },
        { "<leader>jp", group = "js-packages"  },
        { "<leader>db", group = "database"     },
        { "<leader>re", group = "rest"         },
        { "<leader>tc", group = "test-coverage"},
        { "<leader>r",  group = "rust"         },
        { "<leader>c",  group = "c/code"       },
        { "<leader>gc", group = "git-conflict" },
        { "<leader>m",  group = "markdown"     },
      },
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- TERMINAL
  -- ═══════════════════════════════════════════════════════════════════════════

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
      open_mapping  = [[<C-\>]],
      on_open = function(term)
        vim.opt_local.number         = false
        vim.opt_local.relativenumber = false
        if term.direction == "float" then
          vim.keymap.set("t", "<ESC>", [[<C-\><C-n>]], { buffer = term.bufnr })
        end
      end,
      hide_numbers      = true,
      shade_terminals   = true,
      shading_factor    = 2,
      start_in_insert   = true,
      insert_mappings   = true,
      terminal_mappings = true,
      persist_size      = true,
      persist_mode      = true,
      close_on_exit     = true,
      shell             = vim.o.shell,
      auto_scroll       = true,
      float_opts        = {
        border     = "curved",
        winblend   = 3,
        highlights = { border = "FloatBorder", background = "NormalFloat" },
      },
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- DASHBOARD — snacks.nvim
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "folke/snacks.nvim",
    lazy     = false,
    priority = 90,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local ok_snacks, snacks = pcall(require, "snacks")
      if not ok_snacks then
        vim.notify("snacks.nvim not available", vim.log.levels.WARN)
        return
      end

      snacks.setup({
        dashboard = {
          enabled  = true,
          width    = 66,
          row      = nil,
          col      = nil,
          pane_gap = 4,
          autokeys = "1234567890abcdefghijklmnopqrstuvwxyz",
          preset = {
            header = build_header(),
            keys   = {
              { icon = " ", key = "n", desc = "New Buffer",      action = ":enew"                               },
              { icon = " ", key = "f", desc = "Find Files",      action = ":Telescope find_files"               },
              { icon = " ", key = "r", desc = "Recent Files",    action = ":Telescope oldfiles"                 },
              { icon = " ", key = "g", desc = "Live Search",     action = ":Telescope live_grep"                },
              { icon = " ", key = "s", desc = "Restore Session", action = ":lua require('persistence').load()"  },
              { icon = " ", key = "G", desc = "Git Status",      action = ":LazyGit"                            },
              { icon = " ", key = "c", desc = "Config",          action = ":edit ~/.config/nvim/init.lua"       },
              { icon = " ", key = "m", desc = "Mason",           action = ":Mason"                              },
              { icon = " ", key = "l", desc = "Lazy",            action = ":Lazy"                               },
              { icon = " ", key = "h", desc = "Health",          action = ":checkhealth"                        },
              { icon = " ", key = "q", desc = "Quit",            action = ":qa"                                 },
            },
          },
          sections = {
            { section = "header" },
            { section = "keys",  gap = 0, padding = 1 },
            { section = "startup" },
          },
        },
        bigfile      = { enabled = false }, notifier     = { enabled = false },
        quickfile    = { enabled = false }, statuscolumn = { enabled = false },
        words        = { enabled = false }, scroll       = { enabled = false },
        lazygit      = { enabled = false }, terminal     = { enabled = false },
        picker       = { enabled = false }, indent       = { enabled = false },
        input        = { enabled = false }, scope        = { enabled = false },
        zen          = { enabled = false }, image        = { enabled = false },
      })
    end,
  },
}
