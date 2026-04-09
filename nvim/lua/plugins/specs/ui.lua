-- lua/plugins/specs/ui.lua
--
-- v2.3.0 — dashboard-nvim replaced with folke/snacks.nvim
--   • snacks.nvim dashboard supports per-pane custom render functions,
--     enabling a real animated matrix rain header via vim.uv timer +
--     nvim_buf_set_lines on the live dashboard buffer.
--   • All other plugins (themes, lualine, bufferline, notify, trouble,
--     which-key, toggleterm) are unchanged.
--   • dashboard-nvim dependency removed from plugins/init.lua import list.
--
-- RAIN ENGINE:
--   Each column has an independent drop-head that falls at a randomised
--   speed. Characters are drawn from a mixed katakana + hex + symbol set.
--   The timer fires every 80 ms and updates only the header lines so the
--   center menu stays stable. The timer is stopped on DashboardClosed /
--   BufLeave from the dashboard buffer.

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

  -- ── Themes ──────────────────────────────────────────────────────────────

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

  -- ── Status & buffer line ─────────────────────────────────────────────────

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
        disabled_filetypes   = { statusline = { "dashboard", "alpha", "neo-tree", "oil", "snacks_dashboard" } },
        refresh              = { statusline = 1000, tabline = 1000, winbar = 1000 },
      },
      sections = {
        lualine_a = {
          {
            "mode",
            fmt = function(str)
              local icons = {
                NORMAL      = "󰰓 N", INSERT     = "󰰄 I",
                VISUAL      = "󰰫 V", ["V-LINE"]  = "󰰫 VL",
                ["V-BLOCK"] = "󰰫 VB", COMMAND   = " C",
                TERMINAL    = " T", REPLACE    = "󰰟 R",
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

  -- ── Notifications ────────────────────────────────────────────────────────

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

  -- ── Diagnostics ──────────────────────────────────────────────────────────

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

  -- ── Which-key ────────────────────────────────────────────────────────────

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
      win    = { border = "rounded", padding = { 1, 2 }, zindex = 1000 },
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
        { "<leader>ts", group = "typescript" },
        { "<leader>jp", group = "js-packages" },
        { "<leader>db", group = "database" },
        { "<leader>re", group = "rest" },
        { "<leader>tc", group = "test-coverage" },
      },
    },
  },

  -- ── Terminal ─────────────────────────────────────────────────────────────

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

  -- ── Dashboard — snacks.nvim with animated matrix rain ───────────────────
  --
  -- ARCHITECTURE:
  --   snacks.dashboard renders sections. The "header" section uses a custom
  --   text() function that returns the current rain frame. A vim.uv timer
  --   fires every 80 ms and calls snacks.dashboard.update() to redraw.
  --   Each column has an independent drop-state (head position + speed).
  --   Characters are drawn from a wide katakana + hex + symbol pool.
  --   Highlight groups MATRIX_HEAD (bright green) / MATRIX_TRAIL (dim green)
  --   / MATRIX_DIM (near-black green) are defined on ColorScheme autocmd so
  --   they survive theme toggling.
  --
  -- IMPORTANT: snacks.nvim replaces dashboard-nvim entirely.
  --   Remove `{ import = "plugins.specs.ui" }` duplication is fine; this
  --   spec carries lazy=false + priority=90 to match old dashboard behaviour.
  {
    "folke/snacks.nvim",
    lazy     = false,
    priority = 90,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      -- ── Highlight groups ─────────────────────────────────────────────
      local function set_matrix_hl()
        vim.api.nvim_set_hl(0, "MatrixHead",  { fg = "#00ff41", bold  = true })
        vim.api.nvim_set_hl(0, "MatrixTrail", { fg = "#00cc33" })
        vim.api.nvim_set_hl(0, "MatrixMid",   { fg = "#008822" })
        vim.api.nvim_set_hl(0, "MatrixDim",   { fg = "#003311" })
        vim.api.nvim_set_hl(0, "MatrixBorder",{ fg = "#00ff41", bold  = true })
        vim.api.nvim_set_hl(0, "MatrixTitle", { fg = "#00ff41", bold  = true })
        vim.api.nvim_set_hl(0, "MatrixSub",   { fg = "#00cc33" })
        vim.api.nvim_set_hl(0, "MatrixFaint", { fg = "#1a6632" })
        vim.api.nvim_set_hl(0, "MatrixQuote", { fg = "#00aa2a", italic = true })
      end
      set_matrix_hl()
      vim.api.nvim_create_autocmd("ColorScheme", {
        group    = vim.api.nvim_create_augroup("MatrixHl", { clear = true }),
        callback = set_matrix_hl,
      })

      -- ── Rain engine ──────────────────────────────────────────────────
      local COLS  = 64   -- character columns in the rain field
      local ROWS  = 12   -- visible rain rows
      local POOL  = {
        "ア","イ","ウ","エ","オ","カ","キ","ク","ケ","コ",
        "サ","シ","ス","セ","ソ","タ","チ","ツ","テ","ト",
        "ナ","ニ","ヌ","ネ","ノ","ハ","ヒ","フ","ヘ","ホ",
        "0","1","2","3","4","5","6","7","8","9",
        "A","B","C","D","E","F","λ","Ψ","Ω","∑","∂","∇",
        "{","}","[","]","<",">","/","\\","=","#",
        "░","▒","▓","│","┼","╋","╬","╔","╗","╚","╝",
      }
      local function rc() return POOL[math.random(#POOL)] end

      -- State: each column has a head row (float), speed, and trail length
      math.randomseed(os.time())
      local cols = {}
      for c = 1, COLS do
        cols[c] = {
          head   = math.random() * ROWS,       -- current head position (float)
          speed  = 0.15 + math.random() * 0.4, -- rows per tick
          trail  = 3 + math.random(5),          -- trail length
          chars  = {},                           -- per-row character (randomised)
          active = math.random() > 0.3,          -- some columns start inactive
          pause  = math.random(20),              -- ticks before (re)activation
        }
        for r = 1, ROWS do cols[c].chars[r] = rc() end
      end

      -- Render one frame → returns table of {text, hl} segment lists per row
      -- Returns a flat list of strings (one per line) for snacks text section.
      -- We build a single string per row; snacks will apply per-line highlights
      -- via the `hl` field in the text section items.
      local function render_rain_lines()
        -- Advance state
        for c = 1, COLS do
          local col = cols[c]
          if col.active then
            col.head = col.head + col.speed
            -- randomise chars along the trail occasionally
            if math.random() < 0.3 then
              local r = math.random(ROWS)
              col.chars[r] = rc()
            end
            if col.head > ROWS + col.trail then
              col.active = false
              col.pause  = math.random(15)
              col.head   = -col.trail
              col.trail  = 3 + math.random(5)
              col.speed  = 0.15 + math.random() * 0.4
            end
          else
            col.pause = col.pause - 1
            if col.pause <= 0 then col.active = true end
          end
        end

        -- Build lines: for each row, for each col, pick char + classify
        local lines = {}
        for r = 1, ROWS do
          local segments = {}  -- {char, class}  0=empty 1=dim 2=mid 3=trail 4=head
          for c = 1, COLS do
            local col = cols[c]
            local head_row = math.floor(col.head)
            local dist     = head_row - r   -- positive = head is below this row
            local ch = col.chars[r]
            if dist == 0 then
              segments[c] = { ch, 4 }   -- head
            elseif dist > 0 and dist <= col.trail then
              -- brightness decays with distance from head
              if dist == 1 then segments[c] = { ch, 3 }
              elseif dist <= 3 then segments[c] = { ch, 2 }
              else segments[c] = { ch, 1 }
              end
            else
              segments[c] = { " ", 0 }
            end
          end
          lines[r] = segments
        end
        return lines
      end

      -- Convert segment matrix to snacks-compatible text items
      -- snacks text section accepts: { { text, hl } ... } per line
      -- We flatten to one item-list per row.
      local CLASS_HL = { [0]="", [1]="MatrixDim", [2]="MatrixMid", [3]="MatrixTrail", [4]="MatrixHead" }

      local function rain_to_items(lines)
        local items = {}
        for _, row in ipairs(lines) do
          local row_items = {}
          local buf_text  = ""
          local buf_class = 0
          -- merge consecutive same-class segments
          for _, seg in ipairs(row) do
            local ch, cls = seg[1], seg[2]
            if cls == buf_class then
              buf_text = buf_text .. ch
            else
              if buf_text ~= "" then
                table.insert(row_items, { buf_text, hl = CLASS_HL[buf_class] })
              end
              buf_text  = ch
              buf_class = cls
            end
          end
          if buf_text ~= "" then
            table.insert(row_items, { buf_text, hl = CLASS_HL[buf_class] })
          end
          -- newline between rows
          table.insert(row_items, { "\n" })
          for _, item in ipairs(row_items) do
            table.insert(items, item)
          end
        end
        return items
      end

      -- ── Logo lines (static, rendered once) ───────────────────────────
      local ver = tostring(vim.g.nvim_ide_version or "2.3.0")

      local LOGO = {
        { "╔══════════════════════════════════════════════════════════════╗", "MatrixBorder" },
        { "║  ███╗   ██╗██╗   ██╗██╗███╗   ███╗ ██╗██████╗ ███████╗     ║", "MatrixTitle"  },
        { "║  ████╗  ██║██║   ██║██║████╗ ████║ ██║██╔══██╗██╔════╝     ║", "MatrixTitle"  },
        { "║  ██╔██╗ ██║██║   ██║██║██╔████╔██║ ██║██║  ██║█████╗       ║", "MatrixTitle"  },
        { "║  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║ ██║██║  ██║██╔══╝       ║", "MatrixSub"    },
        { "║  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║ ██║██████╔╝███████╗     ║", "MatrixSub"    },
        { "║  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝ ╚═╝╚═════╝ ╚══════╝    ║", "MatrixFaint"  },
        { "║                                                               ║", "MatrixBorder" },
        { "║   [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]  v"..ver.."  ║", "MatrixQuote"  },
        { "╚══════════════════════════════════════════════════════════════╝", "MatrixBorder" },
      }

      -- rotating quotes
      local QUOTES = {
        "\"Fix one thing and two more break. That's growth.\"",
        "\"Debugging is twice as hard as writing the code.\"  — Kernighan",
        "\"First, solve the problem. Then, write the code.\"  — Johnson",
        "\"The best error message is the one that never shows.\"  — Fuchs",
        "\"Programs must be written for people to read.\"  — Abelson",
        "\"Any fool can write code a computer understands.\"  — Fowler",
      }
      local _quote = QUOTES[math.random(#QUOTES)]

      -- ── snacks.nvim setup ────────────────────────────────────────────
      local ok_snacks, snacks = pcall(require, "snacks")
      if not ok_snacks then
        vim.notify("snacks.nvim not available", vim.log.levels.WARN)
        return
      end

      -- Current rain frame (shared state updated by timer)
      local _rain_lines = render_rain_lines()
      local _rain_items = rain_to_items(_rain_lines)
      local _timer      = nil
      local _dash_buf   = nil

      local function build_header_text()
        -- Returns a flat list of {text, hl} items: rain + logo + quote
        local items = {}
        -- rain (top)
        for _, item in ipairs(_rain_items) do
          table.insert(items, item)
        end
        -- logo
        for _, line in ipairs(LOGO) do
          table.insert(items, { line[1], hl = line[2] })
          table.insert(items, { "\n" })
        end
        -- quote
        table.insert(items, { "\n" })
        table.insert(items, { "  " .. _quote .. "\n", hl = "MatrixQuote" })
        table.insert(items, { "\n" })
        return items
      end

      snacks.setup({
        -- ── dashboard ──────────────────────────────────────────────────
        dashboard = {
          enabled = true,
          width   = 66,
          row     = nil,
          col     = nil,
          pane_gap = 4,
          autokeys = "1234567890abcdefghijklmnopqrstuvwxyz",

          preset = {
            -- override default header with our animated rain header
            header = "",  -- cleared — we use a custom section below
            keys = {
              { icon = " ", key = "n", desc = "New Buffer",        action = ":enew" },
              { icon = " ", key = "f", desc = "Find Files",        action = ":Telescope find_files" },
              { icon = " ", key = "r", desc = "Recent Files",      action = ":Telescope oldfiles" },
              { icon = " ", key = "g", desc = "Live Search",       action = ":Telescope live_grep" },
              { icon = " ", key = "s", desc = "Restore Session",   action = ":lua require('persistence').load()" },
              { icon = " ", key = "G", desc = "Git Status",        action = ":LazyGit" },
              { icon = " ", key = "c", desc = "Config",            action = ":edit ~/.config/nvim/init.lua" },
              { icon = " ", key = "m", desc = "Mason",             action = ":Mason" },
              { icon = " ", key = "l", desc = "Lazy",              action = ":Lazy" },
              { icon = " ", key = "h", desc = "Health",            action = ":checkhealth" },
              { icon = " ", key = "q", desc = "Quit",              action = ":qa" },
            },
          },

          sections = {
            -- Animated rain + logo header
            {
              pane   = 1,
              text   = function() return build_header_text() end,
              align  = "left",
              padding = 0,
            },
            -- Menu
            { section = "keys", gap = 0, padding = 1 },
            -- Footer: startup stats
            {
              pane  = 1,
              text  = function()
                local ok_l, lazy = pcall(require, "lazy")
                local count = ok_l and lazy.stats().count or 0
                local time  = os.date("%H:%M")
                return {
                  { "\n  ● ONLINE · " .. count .. " plugins · " .. time .. "\n", hl = "MatrixFaint" },
                }
              end,
            },
          },
        },

        -- Disable all other snacks modules we don't need
        -- (they can be enabled individually later)
        bigfile      = { enabled = false },
        notifier     = { enabled = false },
        quickfile    = { enabled = false },
        statuscolumn = { enabled = false },
        words        = { enabled = false },
        scroll       = { enabled = false },
        lazygit      = { enabled = false },
        terminal     = { enabled = false },
        picker       = { enabled = false },
        indent       = { enabled = false },
        input        = { enabled = false },
        scope        = { enabled = false },
        zen          = { enabled = false },
        image        = { enabled = false },
      })

      -- ── Timer: animate rain every 80 ms ──────────────────────────────
      -- Start timer when dashboard opens, stop on leave.
      vim.api.nvim_create_autocmd("User", {
        pattern  = "SnacksDashboardOpened",
        group    = vim.api.nvim_create_augroup("MatrixRain", { clear = true }),
        callback = function(e)
          _dash_buf = e.buf

          if _timer then
            pcall(function() _timer:stop(); _timer:close() end)
            _timer = nil
          end

          _timer = vim.uv.new_timer()
          _timer:start(0, 80, vim.schedule_wrap(function()
            -- Check the dashboard buffer is still visible
            if not _dash_buf or not vim.api.nvim_buf_is_valid(_dash_buf) then
              pcall(function() _timer:stop(); _timer:close() end)
              _timer = nil
              return
            end

            -- Advance rain state and rebuild items
            _rain_lines = render_rain_lines()
            _rain_items = rain_to_items(_rain_lines)

            -- Ask snacks to redraw the dashboard
            -- snacks exposes Snacks.dashboard.update() on the global
            pcall(function()
              if Snacks and Snacks.dashboard then
                Snacks.dashboard.update()
              end
            end)
          end))
        end,
      })

      vim.api.nvim_create_autocmd("User", {
        pattern  = "SnacksDashboardClosed",
        group    = vim.api.nvim_create_augroup("MatrixRainStop", { clear = true }),
        callback = function()
          if _timer then
            pcall(function() _timer:stop(); _timer:close() end)
            _timer = nil
          end
          _dash_buf = nil
        end,
      })
    end,
  },
}
