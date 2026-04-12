-- lua/plugins/specs/ui.lua
--
-- v3.0.0 — snacks dashboard: cinematic Matrix rain overlay
--
-- ARCHITECTURE:
--   Same float-over-dashboard approach as v2, but the rain engine is now
--   a full simulation with:
--     • Speed tiers  — each column gets a speed bucket (slow/mid/fast)
--     • Density tiers — sparse columns skip many rows; dense ones fire thick
--     • Character mutation — active trail cells randomly flip each tick
--     • Phosphor glow — head character is painted at +2 columns with a faint
--       highlight, simulating CRT column bleed
--     • Background tint — the rain float window background is set to #0a0f0a
--       (very dark green); achieved via a dedicated hl group on the float
--     • Flicker — random columns spike to full brightness for 1 frame
--     • Reflection layer — a second, very faint upward-drifting inverted rain
--       float sits below the logo at 15% opacity using winblend
--     • Wake effect — logo character cells briefly spike to MatrixWake when
--       the rain head crosses within 2 rows, then decay over 6 frames
--
--   PHASES (managed by _phase):
--     "decode"  — 0..DECODE_MS  : logo characters reveal one-by-one left→right
--     "rain"    — DECODE_MS..   : steady-state rain over full screen
--     "drain"   — triggered by keypress: columns accelerate off-screen, fade
--                 over DRAIN_MS, then fire "enter" transition
--     "enter"   — dashboard menu items fade in green→white over ENTER_MS
--
--   Logo stays bright MatrixHead green the entire time after decode phase.
--   Plain snacks header string is used (no function, avoids crash).

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
        cmp = true, dap = true, dap_ui = true,
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

  -- ── Dashboard — snacks.nvim + cinematic Matrix rain ──────────────────────
  --
  -- PHASES:
  --   "decode"  — LOGO characters reveal column-by-column from rain noise
  --   "rain"    — full steady-state rain with depth/glow/flicker/wake
  --   "drain"   — columns accelerate off-screen on keypress, ~400ms fade
  --   "enter"   — menu items fade in green→white after drain completes
  --
  -- LAYERS:
  --   zindex=200  main rain float   (winblend=100 = transparent bg)
  --   zindex=190  reflection float  (inverted, winblend=92 ≈ 15% opacity)
  --   zindex=150  dashboard (snacks default)
  --   Background tint: MatrixRainBg hl group applied to the main float
  {
    "folke/snacks.nvim",
    lazy     = false,
    priority = 90,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()

      -- ── Highlight groups ───────────────────────────────────────────────
      local function set_matrix_hl()
        -- Rain layers
        vim.api.nvim_set_hl(0, "MatrixHead",   { fg = "#00ff41", bold  = true })
        vim.api.nvim_set_hl(0, "MatrixGlow",   { fg = "#ccffe0", bold  = true })  -- phosphor bleed
        vim.api.nvim_set_hl(0, "MatrixTrail",  { fg = "#00cc33" })
        vim.api.nvim_set_hl(0, "MatrixMid",    { fg = "#008822" })
        vim.api.nvim_set_hl(0, "MatrixDim",    { fg = "#003311" })
        vim.api.nvim_set_hl(0, "MatrixFlick",  { fg = "#88ffaa", bold  = true })  -- flicker spike
        -- Logo
        vim.api.nvim_set_hl(0, "MatrixLogo",   { fg = "#00ff41", bold  = true })
        vim.api.nvim_set_hl(0, "MatrixBorder", { fg = "#00aa2a" })
        vim.api.nvim_set_hl(0, "MatrixWake",   { fg = "#ffffff", bold  = true })  -- wake flash
        vim.api.nvim_set_hl(0, "MatrixQuote",  { fg = "#00aa2a", italic = true })
        -- Reflection (faint inverted)
        vim.api.nvim_set_hl(0, "MatrixRefl",   { fg = "#005518" })
        -- Dark-green background tint for the rain float
        vim.api.nvim_set_hl(0, "MatrixRainBg", { bg = "#0a0f0a" })
      end
      set_matrix_hl()
      vim.api.nvim_create_autocmd("ColorScheme", {
        group    = vim.api.nvim_create_augroup("MatrixHl", { clear = true }),
        callback = set_matrix_hl,
      })

      -- ── Constants ──────────────────────────────────────────────────────
      local DECODE_MS = 1200   -- ms: logo decode phase duration
      local DRAIN_MS  = 420    -- ms: drain-out animation duration
      local TICK_MS   = 80     -- ms: animation frame interval
      local TICK_DRAIN_MS = 22 -- ms: accelerated tick during drain

      -- ── Character pool ─────────────────────────────────────────────────
      local POOL = {
        "ア","イ","ウ","エ","オ","カ","キ","ク","ケ","コ",
        "サ","シ","ス","セ","ソ","タ","チ","ツ","テ","ト",
        "ナ","ニ","ヌ","ネ","ノ","ハ","ヒ","フ","ヘ","ホ",
        "0","1","2","3","4","5","6","7","8","9",
        "A","B","C","D","E","F","λ","Ψ","Ω","∑","∂","∇",
        "{","}","[","]","<",">","/","\\","=","#",
        "░","▒","▓","│","┼","╋","╬","╔","╗","╚","╝",
      }
      local function rc() return POOL[math.random(#POOL)] end

      math.randomseed(os.time())

      -- ── Speed tier buckets ─────────────────────────────────────────────
      -- slow  = 0.12..0.22   mid   = 0.30..0.50   fast  = 0.65..1.10
      local function rand_speed()
        local r = math.random()
        if r < 0.30 then return 0.12 + math.random() * 0.10
        elseif r < 0.65 then return 0.30 + math.random() * 0.20
        else return 0.65 + math.random() * 0.45
        end
      end

      -- ── Density tier — fraction of columns that are "thick" ────────────
      -- thick:  trail 7..14   normal: trail 3..6   sparse: trail 1..2
      local function rand_trail()
        local r = math.random()
        if r < 0.20 then return 7 + math.random(7)
        elseif r < 0.70 then return 3 + math.random(3)
        else return 1 + math.random(1)
        end
      end

      -- ── Column state ───────────────────────────────────────────────────
      local R_ROWS, R_COLS = 0, 0
      local rain_cols = {}

      local function init_cols(rows, cols)
        rain_cols = {}
        for c = 1, cols do
          rain_cols[c] = {
            head   = math.random() * rows,
            speed  = rand_speed(),
            trail  = rand_trail(),
            chars  = {},
            active = math.random() > 0.25,
            pause  = math.random(20),
            flick  = 0,      -- flicker countdown (frames)
          }
          for r = 1, rows do rain_cols[c].chars[r] = rc() end
        end
      end

      -- ── Tick — returns str_lines, class_matrix ─────────────────────────
      -- class: 0=space  1=dim  2=mid  3=trail  4=head  5=glow  6=flicker
      local function tick(rows, cols, drain_factor)
        drain_factor = drain_factor or 1.0

        for c = 1, cols do
          local col = rain_cols[c]
          if col.active then
            col.head = col.head + col.speed * drain_factor
            -- mutation: random trail characters flip
            if math.random() < 0.30 then
              col.chars[math.random(rows)] = rc()
            end
            -- reset when fallen off
            if col.head > rows + col.trail then
              col.active = false
              col.pause  = math.random(15)
              col.head   = -(col.trail)
              col.trail  = rand_trail()
              col.speed  = rand_speed()
            end
          else
            col.pause = col.pause - 1
            if col.pause <= 0 then col.active = true end
          end
          -- flicker: random spike probability
          if col.flick > 0 then
            col.flick = col.flick - 1
          elseif math.random() < 0.004 then
            col.flick = 1 + math.random(2)
          end
        end

        local str_lines    = {}
        local class_matrix = {}
        for r = 1, rows do
          local row_chars, row_cls = {}, {}
          for c = 1, cols do
            local col   = rain_cols[c]
            local hdist = math.floor(col.head) - r
            local ch, cls
            if hdist == 0 then
              ch  = col.chars[r]
              cls = (col.flick > 0) and 6 or 4   -- flicker on head
            elseif hdist > 0 and hdist <= col.trail then
              ch = col.chars[r]
              if hdist == 1 then
                cls = (col.flick > 0) and 6 or 3
              elseif hdist <= 3 then
                cls = 2
              else
                cls = 1
              end
            else
              ch = " "; cls = 0
            end
            row_chars[c] = ch
            row_cls[c]   = cls
          end
          str_lines[r]    = table.concat(row_chars)
          class_matrix[r] = row_cls
        end
        return str_lines, class_matrix
      end

      -- ── Paint rain buffer ──────────────────────────────────────────────
      local CLASS_HL = {
        [1] = "MatrixDim",
        [2] = "MatrixMid",
        [3] = "MatrixTrail",
        [4] = "MatrixHead",
        [5] = "MatrixGlow",
        [6] = "MatrixFlick",
      }

      local _ns   = vim.api.nvim_create_namespace("MatrixRainNs")
      local _rbuf = nil   -- main rain buffer
      local _rwin = nil   -- main rain window
      local _xbuf = nil   -- reflection buffer
      local _xwin = nil   -- reflection window
      local _timer = nil
      local _phase = "idle"   -- idle / decode / rain / drain / enter
      local _drain_start = 0
      local _decode_start = 0

      -- Wake grid: _wake[r][c] = remaining bright frames (counts down each tick)
      local _wake = {}

      local function init_wake(rows, cols)
        _wake = {}
        for r = 1, rows do
          _wake[r] = {}
          for c = 1, cols do _wake[r][c] = 0 end
        end
      end

      local function close_rain()
        if _timer then
          pcall(function() _timer:stop(); _timer:close() end)
          _timer = nil
        end
        if _rwin and vim.api.nvim_win_is_valid(_rwin) then
          pcall(vim.api.nvim_win_close, _rwin, true)
        end
        if _rbuf and vim.api.nvim_buf_is_valid(_rbuf) then
          pcall(vim.api.nvim_buf_delete, _rbuf, { force = true })
        end
        if _xwin and vim.api.nvim_win_is_valid(_xwin) then
          pcall(vim.api.nvim_win_close, _xwin, true)
        end
        if _xbuf and vim.api.nvim_buf_is_valid(_xbuf) then
          pcall(vim.api.nvim_buf_delete, _xbuf, { force = true })
        end
        _rwin = nil; _rbuf = nil
        _xwin = nil; _xbuf = nil
        _phase = "idle"
      end

      local function paint_rain(str_lines, class_matrix, rows, cols, wake_grid)
        if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end
        pcall(function()
          vim.api.nvim_buf_set_lines(_rbuf, 0, rows, false, str_lines)
        end)
        pcall(function()
          vim.api.nvim_buf_clear_namespace(_rbuf, _ns, 0, rows)
          for r = 1, rows do
            local byte_col = 0
            for c = 1, cols do
              local cls = class_matrix[r][c]
              local ch  = (cls == 0) and " " or (rain_cols[c].chars[r] or " ")
              local blen = #ch

              -- Wake override: logo-area character hit by rain head
              local effective_cls = cls
              if wake_grid and wake_grid[r] and (wake_grid[r][c] or 0) > 0 then
                effective_cls = 4  -- MatrixHead brightness during wake
                if cls == 0 then ch = rc() end
              end

              if effective_cls > 0 then
                vim.api.nvim_buf_set_extmark(_rbuf, _ns, r - 1, byte_col, {
                  end_col  = byte_col + blen,
                  hl_group = CLASS_HL[effective_cls],
                  priority = 500,
                })
              end

              -- Phosphor glow: bleed the head character 1-2 cells right
              if cls == 4 and c < cols - 1 then
                vim.api.nvim_buf_set_extmark(_rbuf, _ns, r - 1, byte_col + blen, {
                  end_col  = byte_col + blen * 2,
                  hl_group = "MatrixGlow",
                  priority = 400,
                })
              end

              byte_col = byte_col + blen
            end
          end
        end)
      end

      -- ── Reflection layer paint ──────────────────────────────────────────
      -- Reflection is a separate float with winblend=92 (≈15% opacity)
      -- showing upward-drifting inverted rain symbols.
      local refl_cols = {}

      local function init_refl(rows, cols)
        refl_cols = {}
        for c = 1, cols do
          refl_cols[c] = {
            head   = math.random() * rows,
            speed  = -(0.08 + math.random() * 0.12),   -- negative = upward
            trail  = 2 + math.random(3),
            chars  = {},
            active = math.random() > 0.50,
            pause  = math.random(30),
          }
          for r = 1, rows do refl_cols[c].chars[r] = rc() end
        end
      end

      local _rns = vim.api.nvim_create_namespace("MatrixReflNs")

      local function paint_refl(rows, cols)
        if not _xbuf or not vim.api.nvim_buf_is_valid(_xbuf) then return end

        -- Advance reflection columns
        for c = 1, cols do
          local col = refl_cols[c]
          if col.active then
            col.head = col.head + col.speed
            if col.head < -(col.trail) then
              col.active = false
              col.pause  = math.random(40)
              col.head   = rows + col.trail
              col.speed  = -(0.08 + math.random() * 0.12)
            end
          else
            col.pause = col.pause - 1
            if col.pause <= 0 then col.active = true end
          end
        end

        local str_lines = {}
        for r = 1, rows do
          local row_chars = {}
          for c = 1, cols do
            local col = refl_cols[c]
            local hdist = math.floor(col.head) - r
            if hdist >= 0 and hdist < col.trail then
              row_chars[c] = col.chars[r]
            else
              row_chars[c] = " "
            end
          end
          str_lines[r] = table.concat(row_chars)
        end

        pcall(function()
          vim.api.nvim_buf_set_lines(_xbuf, 0, rows, false, str_lines)
        end)
        pcall(function()
          vim.api.nvim_buf_clear_namespace(_xbuf, _rns, 0, rows)
          for r = 1, rows do
            local byte_col = 0
            for c = 1, cols do
              local col    = refl_cols[c]
              local hdist  = math.floor(col.head) - r
              local active = hdist >= 0 and hdist < col.trail
              local ch     = active and col.chars[r] or " "
              local blen   = #ch
              if active then
                vim.api.nvim_buf_set_extmark(_xbuf, _rns, r - 1, byte_col, {
                  end_col  = byte_col + blen,
                  hl_group = "MatrixRefl",
                  priority = 300,
                })
              end
              byte_col = byte_col + blen
            end
          end
        end)
      end

      -- ── Decode phase: logo reveal ───────────────────────────────────────
      -- Logo lines are stored with their column offsets in the dashboard buffer.
      -- We track how many characters have been "revealed"; unrevealed chars are
      -- replaced with a random pool char highlighted MatrixHead (looks like they
      -- are being decoded from rain). This is drawn into the RAIN float, so it
      -- overlays the snacks header area at the correct rows/cols.
      --
      -- NOTE: decode is cosmetic overlay only; the real snacks header is static
      -- underneath. We draw noise chars over the logo area and progressively
      -- reveal them by switching to MatrixLogo highlight.

      local LOGO_LINES = {
        "╔══════════════════════════════════════════════════════════════╗",
        "║  ███╗   ██╗██╗   ██╗██╗███╗   ███╗ ██╗██████╗ ███████╗     ║",
        "║  ████╗  ██║██║   ██║██║████╗ ████║ ██║██╔══██╗██╔════╝     ║",
        "║  ██╔██╗ ██║██║   ██║██║██╔████╔██║ ██║██║  ██║█████╗       ║",
        "║  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║ ██║██║  ██║██╔══╝       ║",
        "║  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║ ██║██████╔╝███████╗     ║",
        "║  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝ ╚═╝╚═════╝ ╚══════╝    ║",
        "║                                                              ║",
        "║  [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]           ║",
        "╚══════════════════════════════════════════════════════════════╝",
      }
      local LOGO_ROWS   = #LOGO_LINES
      -- FIX (v2.3.5): LOGO_WIDTH removed — it used Lua's # operator which
      -- returns byte length, not display columns. Box-drawing chars are 3 bytes
      -- each in UTF-8, so #LOGO_LINES[1] ≈ 130-160, not 64. The variable was
      -- never read (all decode logic uses LOGO_COLS_DISPLAY below), so it was
      -- dead misleading code. Removed entirely.
      --
      -- We track decoded progress in terms of *display columns* (character
      -- count per line). A single reveal cursor advances left→right, top→bottom.

      -- Total "cells" = LOGO_ROWS * max_char_count_per_line
      -- We approximate each line as 64 display cells (the logo is 64 wide).
      local LOGO_COLS_DISPLAY = 64
      local LOGO_TOTAL_CELLS  = LOGO_ROWS * LOGO_COLS_DISPLAY

      -- _decode_cells: how many cells are revealed so far
      local _decode_cells = 0

      -- Logo screen row offset (from the top of the terminal).
      -- We compute this when the rain float opens, based on terminal height.
      local _logo_row_offset = 0   -- 0-indexed top row of the logo inside the float

      local _logo_ns = vim.api.nvim_create_namespace("MatrixLogoNs")

      -- Paint the logo overlay on the rain buffer.
      -- revealed_cells: how many cells from the top-left are "revealed" (green logo).
      -- Unrevealed cells are drawn as random rain chars in MatrixHead.
      local function paint_logo_decode(revealed_cells)
        if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end

        pcall(function()
          vim.api.nvim_buf_clear_namespace(_rbuf, _logo_ns, 0, -1)
        end)

        for li = 1, LOGO_ROWS do
          local r = _logo_row_offset + li  -- 1-indexed buffer row
          if r < 1 or r > R_ROWS then goto continue_li end

          local line_start_cell = (li - 1) * LOGO_COLS_DISPLAY

          -- Build extmarks for each display column
          -- We need actual byte offsets in the buffer line.
          -- The logo uses box-drawing chars (3 bytes each) and ASCII (1 byte).
          -- Rather than parsing byte-by-byte, we re-use the rain buffer line
          -- and overlay highlights at approximate positions.
          -- For a simpler implementation: we overwrite the logo area cells
          -- directly in the buffer line.

          local logo_str  = LOGO_LINES[li]
          local chars     = vim.fn.split(logo_str, "\\zs")  -- split into UTF-8 chars
          local n_chars   = #chars

          -- Determine reveal count for this line
          local cells_before_line = line_start_cell
          local cells_in_line     = n_chars

          local byte_col = 0
          for ci = 1, cells_in_line do
            local cell_idx = cells_before_line + ci
            local ch       = chars[ci]
            local blen     = #ch
            local hl

            if cell_idx <= revealed_cells then
              -- Revealed: use logo colour (border chars dimmer)
              local is_border = (ch == "║" or ch == "╔" or ch == "╗"
                               or ch == "╚" or ch == "╝" or ch == "═")
              hl = is_border and "MatrixBorder" or "MatrixLogo"
            else
              -- Unrevealed: rain noise char, MatrixHead flicker
              ch = rc()
              hl = "MatrixHead"
            end

            if ch ~= " " then
              pcall(function()
                vim.api.nvim_buf_set_extmark(_rbuf, _logo_ns, r - 1, byte_col, {
                  end_col  = byte_col + blen,
                  hl_group = hl,
                  priority = 600,
                })
              end)
            end

            byte_col = byte_col + blen
          end

          ::continue_li::
        end
      end

      -- Paint the final fully-revealed logo (called during rain + drain phases)
      local function paint_logo_final()
        if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf) then return end

        pcall(function()
          vim.api.nvim_buf_clear_namespace(_rbuf, _logo_ns, 0, -1)
        end)

        for li = 1, LOGO_ROWS do
          local r = _logo_row_offset + li
          if r < 1 or r > R_ROWS then goto continue_fl end

          local logo_str = LOGO_LINES[li]
          local chars    = vim.fn.split(logo_str, "\\zs")
          local byte_col = 0
          for _, ch in ipairs(chars) do
            local blen = #ch
            if ch ~= " " then
              local is_border = (ch == "║" or ch == "╔" or ch == "╗"
                               or ch == "╚" or ch == "╝" or ch == "═")
              pcall(function()
                vim.api.nvim_buf_set_extmark(_rbuf, _logo_ns, r - 1, byte_col, {
                  end_col  = byte_col + blen,
                  hl_group = is_border and "MatrixBorder" or "MatrixLogo",
                  priority = 600,
                })
              end)
            end
            byte_col = byte_col + blen
          end

          ::continue_fl::
        end
      end

      -- ── Update wake grid ───────────────────────────────────────────────
      -- For each active rain head, if it is within 2 rows of a logo row,
      -- mark those logo cells as waking.
      local WAKE_FRAMES = 6

      local function update_wake(rows, cols)
        if not _wake or #_wake == 0 then return end
        -- Decay all wake cells
        for r = 1, rows do
          if _wake[r] then
            for c = 1, cols do
              if (_wake[r][c] or 0) > 0 then
                _wake[r][c] = _wake[r][c] - 1
              end
            end
          end
        end
        -- Trigger new wake cells when rain head crosses logo zone
        for c = 1, cols do
          local col = rain_cols[c]
          if col.active then
            local hr = math.floor(col.head)
            for li = 1, LOGO_ROWS do
              local lr = _logo_row_offset + li
              if math.abs(hr - lr) <= 2 then
                -- Mark logo columns in this rain column row
                if _wake[lr] then
                  _wake[lr][c] = WAKE_FRAMES
                  -- Also mark adjacent cells for width
                  if c > 1 then _wake[lr][c-1] = math.max(_wake[lr][c-1] or 0, WAKE_FRAMES - 1) end
                  if c < cols then _wake[lr][c+1] = math.max(_wake[lr][c+1] or 0, WAKE_FRAMES - 1) end
                end
              end
            end
          end
        end
      end

      -- ── Main animation frame ───────────────────────────────────────────
      local function frame()
        if not _rbuf or not vim.api.nvim_buf_is_valid(_rbuf)
        or not _rwin or not vim.api.nvim_win_is_valid(_rwin) then
          close_rain(); return
        end

        local now = vim.uv.hrtime() / 1e6   -- ms

        if _phase == "decode" then
          -- Advance reveal cursor proportionally through DECODE_MS
          local elapsed  = now - _decode_start
          local progress = math.min(elapsed / DECODE_MS, 1.0)
          -- Ease-in-out
          local t = progress < 0.5 and (2 * progress * progress)
                                    or (1 - 2*(1-progress)*(1-progress))
          _decode_cells = math.floor(t * LOGO_TOTAL_CELLS)

          local sl, cm = tick(R_ROWS, R_COLS)
          paint_rain(sl, cm, R_ROWS, R_COLS, nil)
          paint_logo_decode(_decode_cells)
          paint_refl(R_ROWS, R_COLS)

          if progress >= 1.0 then
            _phase = "rain"
          end

        elseif _phase == "rain" then
          update_wake(R_ROWS, R_COLS)
          local sl, cm = tick(R_ROWS, R_COLS)
          paint_rain(sl, cm, R_ROWS, R_COLS, _wake)
          paint_logo_final()
          paint_refl(R_ROWS, R_COLS)

        elseif _phase == "drain" then
          local elapsed = now - _drain_start
          local factor  = 1.0 + (elapsed / DRAIN_MS) * 8.0   -- accelerate
          local alpha   = math.max(0, 1.0 - elapsed / DRAIN_MS)

          local sl, cm = tick(R_ROWS, R_COLS, factor)
          paint_rain(sl, cm, R_ROWS, R_COLS, nil)
          paint_refl(R_ROWS, R_COLS)

          -- Fade out: blend 0→100 as alpha 1→0 (chars go from fully visible to gone).
          -- winblend=0 was set in trigger_drain() so the first frame has no flash.
          local blend = math.floor((1.0 - alpha) * 100)
          if _rwin and vim.api.nvim_win_is_valid(_rwin) then
            pcall(function()
              vim.wo[_rwin].winblend = blend
            end)
          end
          if _xwin and vim.api.nvim_win_is_valid(_xwin) then
            pcall(function()
              vim.wo[_xwin].winblend = math.min(100, 92 + math.floor((1 - alpha) * 8))
            end)
          end

          if elapsed >= DRAIN_MS then
            close_rain()
            -- Trigger enter phase: menu items fade in green→white
            -- (We re-highlight the snacks dashboard buffer.)
            _phase = "enter"
            vim.defer_fn(function()
              -- Flash all visible text in the dashboard green for 200ms,
              -- then clear the override. This gives the "fade in from green" feel.
              local dash_bufs = {}
              for _, buf in ipairs(vim.api.nvim_list_bufs()) do
                if pcall(function()
                  return vim.bo[buf].filetype == "snacks_dashboard"
                end) then
                  table.insert(dash_bufs, buf)
                end
              end
              local ens = vim.api.nvim_create_namespace("MatrixEnterNs")
              for _, buf in ipairs(dash_bufs) do
                local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
                for li, line in ipairs(lines) do
                  if line ~= "" then
                    pcall(function()
                      vim.api.nvim_buf_set_extmark(buf, ens, li - 1, 0, {
                        end_col  = #line,
                        hl_group = "MatrixTrail",
                        priority = 50,
                      })
                    end)
                  end
                end
              end
              -- Fade out the green tint over 300ms
              local steps = 6
              local step  = 0
              local fade_timer = vim.uv.new_timer()
              fade_timer:start(50, 50, vim.schedule_wrap(function()
                step = step + 1
                if step >= steps then
                  fade_timer:stop(); fade_timer:close()
                  for _, buf in ipairs(dash_bufs) do
                    pcall(vim.api.nvim_buf_clear_namespace, buf, ens, 0, -1)
                  end
                  _phase = "idle"
                end
              end))
            end, 50)
          end
        end
      end

      -- ── Drain trigger ──────────────────────────────────────────────────
      -- Called when the user presses any key on the dashboard. Switches
      -- _phase to "drain"; the animation loop handles the rest.
      local function trigger_drain()
        if _phase ~= "rain" and _phase ~= "decode" then return end
        _phase = "drain"
        _drain_start = vim.uv.hrtime() / 1e6

        -- FIX (v2.3.5): reset winblend to 0 at drain start.
        -- During rain the main float is created with winblend=100 (transparent
        -- background, fg characters visible). The drain frame() loop ramps
        -- blend from 0→100 (chars fade from fully visible to invisible).
        -- Without this reset, the first drain frame snaps winblend from 100→0
        -- causing a one-frame dark-green background flash before the fade
        -- actually begins. Resetting to 0 here makes the transition seamless.
        if _rwin and vim.api.nvim_win_is_valid(_rwin) then
          pcall(function() vim.wo[_rwin].winblend = 0 end)
        end
        if _xwin and vim.api.nvim_win_is_valid(_xwin) then
          pcall(function() vim.wo[_xwin].winblend = 0 end)
        end

        -- Switch to fast drain timer
        if _timer then
          pcall(function() _timer:stop(); _timer:close() end)
          _timer = vim.uv.new_timer()
          _timer:start(0, TICK_DRAIN_MS, vim.schedule_wrap(function()
            if _phase == "idle" then
              pcall(function() _timer:stop(); _timer:close() end)
              _timer = nil
              return
            end
            frame()
          end))
        end
      end

      -- ── Open rain ──────────────────────────────────────────────────────
      local function open_rain(dash_win)
        close_rain()

        local total_rows = vim.o.lines   - 1
        local total_cols = vim.o.columns

        R_ROWS = total_rows
        R_COLS = total_cols

        init_cols(R_ROWS, R_COLS)
        init_wake(R_ROWS, R_COLS)
        init_refl(R_ROWS, R_COLS)

        -- Logo row offset: centre vertically in the terminal
        -- (snacks centres its content, so we mirror that)
        _logo_row_offset = math.floor((total_rows - LOGO_ROWS) / 2) - 2
        if _logo_row_offset < 0 then _logo_row_offset = 0 end

        -- ── Main rain buffer / float ──────────────────────────────────
        _rbuf = vim.api.nvim_create_buf(false, true)
        vim.bo[_rbuf].buftype    = "nofile"
        vim.bo[_rbuf].bufhidden  = "wipe"
        vim.bo[_rbuf].swapfile   = false
        vim.bo[_rbuf].modifiable = true

        local blank = {}
        for r = 1, R_ROWS do blank[r] = string.rep(" ", R_COLS) end
        vim.api.nvim_buf_set_lines(_rbuf, 0, -1, false, blank)

        _rwin = vim.api.nvim_open_win(_rbuf, false, {
          relative  = "editor",
          row       = 0,
          col       = 0,
          width     = total_cols,
          height    = total_rows,
          style     = "minimal",
          focusable = false,
          zindex    = 200,
        })
        vim.wo[_rwin].winblend   = 100   -- transparent bg; only fg chars visible
        vim.wo[_rwin].wrap       = false
        vim.wo[_rwin].cursorline = false
        vim.wo[_rwin].number     = false
        vim.wo[_rwin].signcolumn = "no"

        -- Apply the dark-green background tint to the float's Normal hl
        -- (winblend=100 makes the background transparent, so this has no
        --  direct visual effect on its own, but setting winhighlight allows
        --  future winblend tweaks to blend against the tint rather than black)
        vim.wo[_rwin].winhighlight = "Normal:MatrixRainBg"

        -- ── Reflection buffer / float ─────────────────────────────────
        _xbuf = vim.api.nvim_create_buf(false, true)
        vim.bo[_xbuf].buftype    = "nofile"
        vim.bo[_xbuf].bufhidden  = "wipe"
        vim.bo[_xbuf].swapfile   = false
        vim.bo[_xbuf].modifiable = true

        local rblank = {}
        for r = 1, R_ROWS do rblank[r] = string.rep(" ", R_COLS) end
        vim.api.nvim_buf_set_lines(_xbuf, 0, -1, false, rblank)

        _xwin = vim.api.nvim_open_win(_xbuf, false, {
          relative  = "editor",
          row       = 0,
          col       = 0,
          width     = total_cols,
          height    = total_rows,
          style     = "minimal",
          focusable = false,
          zindex    = 190,
        })
        vim.wo[_xwin].winblend   = 92    -- ≈15% opacity for faint reflection
        vim.wo[_xwin].wrap       = false
        vim.wo[_xwin].cursorline = false
        vim.wo[_xwin].number     = false
        vim.wo[_xwin].signcolumn = "no"

        -- ── Start decode phase ────────────────────────────────────────
        _phase        = "decode"
        _decode_start = vim.uv.hrtime() / 1e6
        _decode_cells = 0

        -- First frame
        frame()

        -- Normal-speed timer for decode + rain phases
        _timer = vim.uv.new_timer()
        _timer:start(TICK_MS, TICK_MS, vim.schedule_wrap(function()
          if _phase == "drain" or _phase == "idle" then
            -- drain phase uses its own timer (already switched in trigger_drain)
            return
          end
          frame()
        end))
      end

      -- ── Logo for snacks header ─────────────────────────────────────────
      -- Plain static string — no function, avoids the dashboard.lua:382 crash.
      -- The rain float (zindex 190-200) overlays this from above with the
      -- animated decode + logo hl. After drain the snacks buffer is visible.
      local ver = tostring(vim.g.nvim_ide_version or "2.3.5")
      local LOGO_HEADER = table.concat({
        "╔══════════════════════════════════════════════════════════════╗",
        "║  ███╗   ██╗██╗   ██╗██╗███╗   ███╗ ██╗██████╗ ███████╗     ║",
        "║  ████╗  ██║██║   ██║██║████╗ ████║ ██║██╔══██╗██╔════╝     ║",
        "║  ██╔██╗ ██║██║   ██║██║██╔████╔██║ ██║██║  ██║█████╗       ║",
        "║  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║ ██║██║  ██║██╔══╝       ║",
        "║  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║ ██║██████╔╝███████╗     ║",
        "║  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝ ╚═╝╚═════╝ ╚══════╝    ║",
        "║                                                              ║",
        "║  [ LSP ]  [ DAP ]  [ TREESITTER ]  [ 20+ LANGS ]  v"..ver.." ║",
        "╚══════════════════════════════════════════════════════════════╝",
      }, "\n")
      local QUOTES = {
        '"Fix one thing, two more break.  That\'s growth."',
        '"Debugging is twice as hard as writing the code."  — Kernighan',
        '"First, solve the problem. Then, write the code."  — Johnson',
        '"The best error message is the one that never shows."  — Fuchs',
        '"Programs must be written for people to read."  — Abelson',
        '"Any fool can write code a computer understands."  — Fowler',
      }
      local QUOTE = QUOTES[math.random(#QUOTES)]

      -- ── snacks setup ──────────────────────────────────────────────────
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
            header = LOGO_HEADER .. "\n\n  " .. QUOTE,
            keys = {
              { icon = " ", key = "n", desc = "New Buffer",      action = ":enew" },
              { icon = " ", key = "f", desc = "Find Files",      action = ":Telescope find_files" },
              { icon = " ", key = "r", desc = "Recent Files",    action = ":Telescope oldfiles" },
              { icon = " ", key = "g", desc = "Live Search",     action = ":Telescope live_grep" },
              { icon = " ", key = "s", desc = "Restore Session", action = ":lua require('persistence').load()" },
              { icon = " ", key = "G", desc = "Git Status",      action = ":LazyGit" },
              { icon = " ", key = "c", desc = "Config",          action = ":edit ~/.config/nvim/init.lua" },
              { icon = " ", key = "m", desc = "Mason",           action = ":Mason" },
              { icon = " ", key = "l", desc = "Lazy",            action = ":Lazy" },
              { icon = " ", key = "h", desc = "Health",          action = ":checkhealth" },
              { icon = " ", key = "q", desc = "Quit",            action = ":qa" },
            },
          },

          sections = {
            { section = "header" },
            { section = "keys",  gap = 0, padding = 1 },
            { section = "startup" },
          },
        },

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

      -- ── Lifecycle hooks ────────────────────────────────────────────────
      vim.api.nvim_create_autocmd("User", {
        pattern  = "SnacksDashboardOpened",
        group    = vim.api.nvim_create_augroup("MatrixRainOpen", { clear = true }),
        callback = function(e)
          local dash_win = nil
          for _, w in ipairs(vim.api.nvim_list_wins()) do
            if pcall(function() return vim.api.nvim_win_get_buf(w) == e.buf end) then
              dash_win = w; break
            end
          end
          vim.defer_fn(function() open_rain(dash_win) end, 30)
        end,
      })

      -- Any keypress on the dashboard triggers drain-out instead of hard-close
      vim.api.nvim_create_autocmd("User", {
        pattern  = "SnacksDashboardOpened",
        group    = vim.api.nvim_create_augroup("MatrixRainDrainKey", { clear = true }),
        callback = function(e)
          -- FIX: removed vim.keymap.set("n", "<expr><buffer>", ...) — that
          -- string is not a valid key sequence and was never triggered.
          -- CursorMoved and BufLeave are sufficient to catch all real
          -- user interactions on the dashboard reliably.

          vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
            buffer   = e.buf,
            once     = true,
            callback = function() trigger_drain() end,
          })

          -- BufLeave: reliable "user navigated away" signal
          vim.api.nvim_create_autocmd("BufLeave", {
            buffer   = e.buf,
            once     = true,
            callback = function() trigger_drain() end,
          })
        end,
      })

      -- Hard close on dashboard closed (safety net if drain already finished)
      vim.api.nvim_create_autocmd("User", {
        pattern  = "SnacksDashboardClosed",
        group    = vim.api.nvim_create_augroup("MatrixRainClose", { clear = true }),
        callback = close_rain,
      })

      -- Also hard-close if cursor moves into any normal buffer
      vim.api.nvim_create_autocmd("BufEnter", {
        group    = vim.api.nvim_create_augroup("MatrixRainBufEnter", { clear = true }),
        callback = function()
          if _rwin and vim.api.nvim_win_is_valid(_rwin) then
            local ft = vim.bo.filetype
            if ft ~= "snacks_dashboard" and ft ~= "" then
              if _phase == "rain" or _phase == "decode" then
                trigger_drain()
              else
                close_rain()
              end
            end
          end
        end,
      })
    end,
  },
}
