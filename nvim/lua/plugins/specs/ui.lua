-- lua/plugins/specs/ui.lua
--
-- FIX (v2.2.3):
--   • Dashboard footer: doom theme requires a plain table of strings, not a
--     callable. Wrapping in a function caused the footer to never render.
--     Fixed: footer is now a static table built once at load time. Plugin/mem
--     stats that need runtime values are placed in center items or omitted.
--   • which-key: <leader>md group removed — no keymaps use that prefix.
--     Markdown bindings are <leader>mp and <leader>tm (both covered by other
--     groups or standalone).
--   • theme_spec lazy flag: the closure was fine (captured at load time which
--     is correct for lazy=false on the active theme). No change needed —
--     mid-session theme switches use M.switch() which calls colorscheme
--     directly, not lazy's loading mechanism.

local function get_active()
  local ok, t = pcall(function() return require("core.theme").config.theme end)
  return ok and t or "tokyonight"
end

local function theme_spec(plugin_name, name, extra)
  local active    = get_active()
  local is_active = (name == active)
    or (active == "tokyonight" and name and name:match("^tokyonight"))
  return vim.tbl_extend("force", {
    plugin_name,
    name     = name or nil,
    lazy     = not is_active,
    priority = 1000,
  }, extra or {})
end

return {

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
      show_end_of_buffer     = false,
      integrations = {
        aerial = true, alpha = true, cmp = true, dap = true, dap_ui = true,
        dashboard = true, fidget = true, gitsigns = true, illuminate = true,
        indent_blankline = { enabled = true }, lsp_trouble = true, mason = true,
        native_lsp = { enabled = true }, navic = { enabled = true },
        neotree = true, notify = true, nvimtree = true, overseer = true,
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
      overrides  = function(_) return {} end,
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
          {
            "mode",
            fmt = function(str)
              local icons = {
                NORMAL     = "󰰓 N", INSERT    = "󰰄 I",
                VISUAL     = "󰰫 V", ["V-LINE"] = "󰰫 VL",
                ["V-BLOCK"] = "󰰫 VB", COMMAND  = " C",
                TERMINAL   = " T", REPLACE   = "󰰟 R",
              }
              return icons[str] or str:sub(1, 1)
            end,
          },
        },
        lualine_b = {
          "branch",
          { "diff",
            symbols    = { added = "  ", modified = "  ", removed = "  " },
            colored    = true,
            diff_color = {
              added    = { fg = "#90EE90" },
              modified = { fg = "#FFD700" },
              removed  = { fg = "#FF6B6B" },
            },
          },
          { "diagnostics",
            symbols = { error = "  ", warn = "  ", info = "  ", hint = "  " },
            colored = true,
          },
        },
        lualine_c = {
          { "filename", path = 1,
            symbols = { modified = "  ", readonly = "  ", unnamed = " [No Name] " } },
        },
        lualine_x = {
          { "filetype", icons_enabled = true, icon = nil },
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

  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    opts = {
      timeout    = 3000,
      max_height = function() return math.floor(vim.o.lines * 0.75) end,
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

  {
    "folke/trouble.nvim",
    cmd  = { "Trouble" },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics" },
      { "<leader>xL", "<cmd>Trouble loclist toggle<cr>", desc = "Location List" },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<cr>",  desc = "Quickfix List" },
    },
    opts = {
      auto_close   = false,
      auto_preview = true,
      auto_refresh = true,
      focus        = false,
      restore      = true,
      icons = {
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
        ["?"]     = "help",      ["r"]     = "refresh",
        ["R"]     = "toggle_refresh", ["q"] = "close",
        ["o"]     = "jump_close", ["<esc>"] = "cancel",
        ["<cr>"]  = "jump",      ["<tab>"] = "jump",
        ["<c-x>"] = "jump_split", ["<c-v>"] = "jump_vsplit",
        ["<c-t>"] = "jump_tab",  ["za"]    = "fold_toggle",
        ["zo"]    = "fold_open", ["zc"]    = "fold_close",
        ["zM"]    = "fold_close_all", ["zR"] = "fold_open_all",
        ["k"]     = "prev",      ["j"]     = "next",
      },
    },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
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
      win = { border = "rounded", padding = { 1, 2 }, zindex = 1000 },
      layout = {
        align   = "center",
        height  = { min = 4, max = 25 },
        spacing = 3,
        width   = { min = 20, max = 50 },
      },
      keys = { scroll_down = "<c-d>", scroll_up = "<c-u>" },
      spec = {
        { "<leader>o",  group = "overseer/tasks" },
        { "<leader>uF", group = "focus-mode" },
        { "<leader>b",  group = "buffer" },
        { "<leader>c",  group = "code" },
        { "<leader>e",  group = "explorer" },
        { "<leader>f",  group = "find" },
        { "<leader>h",  group = "harpoon" },
        { "<leader>q",  group = "session" },
        { "<leader>r",  group = "rust" },
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
        { "<leader>py", group = "python" },
        { "<leader>rb", group = "ruby" },
        { "<leader>go", group = "go" },
        { "<leader>jv", group = "java" },
        { "<leader>ex", group = "elixir" },
        { "<leader>kt", group = "kotlin" },
        { "<leader>cc", group = "cpp/cmake" },
        { "<leader>vh", group = "vhdl" },
        { "<leader>ft", group = "fortran" },
        { "<leader>z",  group = "zig" },
        { "<leader>co", group = "cobol" },
        -- FIX: <leader>md group removed — no keymaps use that prefix.
        -- Markdown bindings are <leader>mp (MarkdownPreviewToggle) and
        -- <leader>tm (TableModeToggle), both outside the md* namespace.
        { "<leader>ts", group = "typescript" },
        { "<leader>jp", group = "js-packages" },
        { "<leader>db", group = "database" },
        { "<leader>re", group = "rest" },
        { "<leader>tc", group = "test-coverage" },
      },
    },
  },

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
      on_close              = function(_) end,
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

  {
    "nvimdev/dashboard-nvim",
    lazy         = false,
    priority     = 90,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      -- ── helpers ────────────────────────────────────────────────────────
      local ver = tostring(vim.g.nvim_ide_version or "2.2.5")

      -- rain charset: kanji + hex + symbols
      local RC = {
        "ア","イ","ウ","エ","オ","カ","キ","ク","ケ","コ",
        "サ","シ","ス","セ","ソ","タ","チ","ツ","テ","ト",
        "0","1","2","3","4","5","6","7","8","9",
        "A","B","C","D","E","F","λ","Ψ","Ω","∑",
        "{","}","[","]","<",">","/","\\","=","#","░","▒","▓",
      }
      local function rc() return RC[math.random(#RC)] end

      -- sparse rain row: ~13% density, dot fill for the rest
      local W = 64 -- inner content width (no border chars)
      local function rain_row(density)
        density = density or 0.13
        local t = {}
        for _ = 1, W do
          t[#t+1] = math.random() < density and rc() or "·"
        end
        return " " .. table.concat(t) .. " "
      end

      -- centre a string inside W, no border chars (dashboard centres itself)
      local function c(s)
        local pad = W - vim.fn.strdisplaywidth(s)
        local lp  = math.floor(pad / 2)
        return string.rep(" ", lp) .. s
      end

      -- separator line: ── style with corner glyphs
      local function sep(l, r)
        l = l or "├"
        r = r or "┤"
        return " " .. l .. string.rep("─", W - 2) .. r .. " "
      end

      -- ── rotating hacker quotes ─────────────────────────────────────────
      local quotes = {
        { '"Fix one thing and two more break.',
          " That's growth.",
          "                    — The Humble Hacker" },
        { '"Debugging is twice as hard as writing',
          ' the code in the first place."',
          "                    — Brian Kernighan" },
        { '"Any fool can write code a computer',
          ' can understand. Good code humans can."',
          "                    — Martin Fowler" },
        { '"First, solve the problem.',
          ' Then, write the code."',
          "                    — John Johnson" },
        { '"The best error message is the one',
          ' that never shows up."',
          "                    — Thomas Fuchs" },
        { '"Programs must be written for people',
          ' to read, and only incidentally for machines."',
          "                    — Harold Abelson" },
      }
      math.randomseed(os.time())
      local q = quotes[math.random(#quotes)]

      -- ── system status line ─────────────────────────────────────────────
      local function status_line()
        local ok, lazy = pcall(require, "lazy")
        local plugin_count = ok and lazy.stats().count or 0
        local time = os.date("%H:%M")
        local host = vim.fn.hostname()
        return string.format(
          " ● ONLINE  ·  %d plugins  ·  %s  ·  %s ",
          plugin_count, host, time
        )
      end

      -- ── header assembly ────────────────────────────────────────────────
      local header = {
        "",
        -- top rain band (denser)
        rain_row(0.20),
        rain_row(0.15),
        -- top bar: version + mode indicator
        c("┌─[ NVIM-IDE v" .. ver .. " ]" .. string.rep("─", 30) .. "[ SYS::READY ]─┐"),
        c("│" .. string.rep(" ", W - 2) .. "│"),
        -- logo: alternating bright/dim lines for scanline effect
        c("│  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  │"),
        c("│  ███╗   ██╗██╗   ██╗██╗███╗   ███╗  ██╗██████╗ ███████╗ │"),
        c("│  ████╗  ██║██║   ██║██║████╗ ████║  ██║██╔══██╗██╔════╝ │"),
        c("│  ██╔██╗ ██║██║   ██║██║██╔████╔██║  ██║██║  ██║█████╗   │"),
        c("│  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║  ██║██║  ██║██╔══╝   │"),
        c("│  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║  ██║██████╔╝███████╗ │"),
        c("│  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚═════╝ ╚══════╝ │"),
        c("│  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  │"),
        -- meta pills
        c("│" .. string.rep(" ", W - 2) .. "│"),
        c("│     [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]     │"),
        c("│" .. string.rep(" ", W - 2) .. "│"),
        -- scanline separator
        c("├" .. string.rep("·", W - 2) .. "┤"),
        -- quote block
        c("│" .. string.rep(" ", W - 2) .. "│"),
        c("│  ▎ " .. q[1]),
        c("│  ▎ " .. q[2]),
        c("│  ▎ " .. q[3]),
        c("│" .. string.rep(" ", W - 2) .. "│"),
        -- scanline separator
        c("├" .. string.rep("·", W - 2) .. "┤"),
        c("│" .. string.rep(" ", W - 2) .. "│"),
        -- refactor tagline
        c("│    refactor · debug · test · analyze · optimize · conquer   │"),
        c("│" .. string.rep(" ", W - 2) .. "│"),
        -- status bar
        c("│  " .. status_line()),
        c("└" .. string.rep("─", W - 2) .. "┘"),
        -- bottom rain band
        rain_row(0.15),
        rain_row(0.20),
        "",
      }

      -- ── dashboard setup ────────────────────────────────────────────────
      require("dashboard").setup({
        theme = "doom",
        config = {
          header = header,
          center = {
            { icon = "  ", desc = "New Buffer              ", action = "enew",                             key = "n" },
            { icon = "  ", desc = "Find Files              ", action = "Telescope find_files",              key = "f" },
            { icon = "  ", desc = "Recent Files            ", action = "Telescope oldfiles",                key = "r" },
            { icon = "  ", desc = "Live Search             ", action = "Telescope live_grep",               key = "g" },
            { icon = "  ", desc = "Restore Session         ", action = "lua require('persistence').load()", key = "s" },
            { icon = "  ", desc = "Git Status              ", action = "LazyGit",                           key = "G" },
            { icon = "  ", desc = "Config Editor           ", action = "edit ~/.config/nvim/init.lua",      key = "c" },
            { icon = "  ", desc = "Language Servers        ", action = "Mason",                             key = "m" },
            { icon = "  ", desc = "Plugins (Lazy)          ", action = "Lazy",                              key = "l" },
            { icon = "  ", desc = "System Health           ", action = "checkhealth",                       key = "h" },
            { icon = "  ", desc = "Exit                    ", action = "qa",                                key = "q" },
          },
          -- footer: plain table of strings — doom theme does table.concat directly
          footer = {
            "",
            "  " .. rc() .. " " .. rc() .. " " .. rc()
              .. "  ·  the machine is ready  ·  "
              .. rc() .. " " .. rc() .. " " .. rc(),
            "",
          },
        },
      })
    end,
  },
}
