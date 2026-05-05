-- lua/plugins/specs/hud.lua — visual enhancement plugins
--
-- 1. Editor chrome       indent guides, scrollview, tint, smear cursor
-- 2. Overlay / popups    noice, barbecue, symbol-usage, inc-rename
-- 3. Navigation helpers  oil, neoscroll, mini.animate
-- 4. Focus tools         zen-mode, twilight
-- 5. Misc               (blame.nvim moved to git.lua; TodoTelescope key in editor.lua)
--
-- NOTE: nvim-notify is owned by ui.lua (single authoritative spec).
--       This file only lists it as a dependency where required by noice.
--

return {

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 1. EDITOR CHROME
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main  = "ibl",
    opts  = {
      indent  = { char = "│", tab_char = "│" },
      scope   = {
        enabled    = true,
        show_start = true,
        show_end   = false,
        highlight  = { "Function", "Label" },
      },
      exclude = {
        filetypes = {
          "help", "dashboard", "lazy", "mason", "notify",
          "toggleterm", "lazyterm", "neo-tree", "snacks_dashboard",
        },
      },
    },
  },

  {
    "dstein64/nvim-scrollview",
    event = "BufReadPost",
    opts  = {
      current_only       = true,
      base               = "right",
      column             = 1,
      signs_on_startup   = { "all" },
      diagnostics_severities = {
        vim.diagnostic.severity.ERROR,
        vim.diagnostic.severity.WARN,
      },
    },
  },

  {
    "levouh/tint.nvim",
    event = "VeryLazy",
    opts  = {
      tint                      = -30,
      saturation                = 0.7,
      tint_background_colors    = true,
      highlight_ignore_patterns = {
        "WinSeparator", "Status.*", "IndentBlankline.*", "NvimTree.*",
      },
    },
  },

  {
    "sphamba/smear-cursor.nvim",
    event = "VeryLazy",
    opts  = {
      stiffness                        = 0.8,
      trailing_exponent                = 3,
      hide_target_hack                 = true,
      legacy_computing_symbols_support = false,
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 2. OVERLAY / POPUPS
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "folke/noice.nvim",
    event        = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"]                 = true,
          ["cmp.entry.get_documentation"]                   = true,
        },
        hover     = { enabled = true },
        signature = { enabled = true },
        progress  = { enabled = false },
      },
      presets = {
        bottom_search         = true,
        command_palette       = true,
        long_message_to_split = true,
        inc_rename            = true,
        lsp_doc_border        = true,
      },
      routes = {
        { filter = { event = "msg_show", kind = "", find = "written"           }, opts = { skip = true } },
        { filter = { event = "msg_show", kind = "", find = "%d+ lines"         }, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Query error"      }, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Impossible pattern"}, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Invalid node type"}, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Invalid syntax"   }, opts = { skip = true } },
      },
      views = {
        cmdline_popup = {
          position = { row = "40%", col = "50%" },
          size     = { width = 60, height = "auto" },
          border   = { style = "rounded", padding = { 0, 1 } },
        },
        popupmenu = {
          relative = "editor",
          position = { row = "43%", col = "50%" },
          size     = { width = 60, height = 10 },
          border   = { style = "rounded", padding = { 0, 1 } },
        },
      },
    },
  },

  {
    "utilyre/barbecue.nvim",
    event        = "LspAttach",
    dependencies = { "SmiteshP/nvim-navic", "nvim-tree/nvim-web-devicons" },
    opts = {
      attach_navic  = false,   -- navic attached in lsp.lua LspAttach
      show_modified = true,
    },
  },

  {
    "Wansmer/symbol-usage.nvim",
    event = "LspAttach",
    config = function()
      local function fmt(symbol)
        local parts = {}
        if symbol.references then
          local n = symbol.references
          table.insert(parts, ("󰌹 %s"):format(n <= 1 and "1 use" or (n .. " uses")))
        end
        if symbol.definition and symbol.definition > 0 then
          table.insert(parts, ("󰳽 %s"):format(symbol.definition))
        end
        return table.concat(parts, " │ ")
      end
      pcall(function()
        require("symbol-usage").setup({
          hl          = { link = "Comment" },
          vt_position = "end_of_line",
          request_pending_text = false,
          text_format = fmt,
        })
      end)
    end,
  },

  {
    "smjonas/inc-rename.nvim",
    cmd  = "IncRename",
    opts = {},
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 3. NAVIGATION HELPERS
  -- ═══════════════════════════════════════════════════════════════════════════

  {
    "karb94/neoscroll.nvim",
    event = "VeryLazy",
    opts  = {
      mappings             = { "<C-u>", "<C-d>", "zt", "zz", "zb" },
      hide_cursor          = true,
      stop_eof             = true,
      respect_scrolloff    = true,
      easing_function      = "sine",
      cursor_scrolls_alone = true,
    },
  },

  {
    "stevearc/oil.nvim",
    cmd  = "Oil",
    keys = {
      { "<leader>eo", "<cmd>Oil<cr>", desc = "Oil file editor" },
    },
    opts = {
      default_file_explorer = false,
      view_options          = { show_hidden = true },
      float = {
        padding     = 2,
        border      = "rounded",
        win_options = { winblend = 5 },
      },
      keymaps = {
        ["<CR>"]  = "actions.select",
        ["<C-s>"] = "actions.select_vsplit",
        ["<C-p>"] = "actions.preview",
        ["-"]     = "actions.parent",
        ["_"]     = "actions.open_cwd",
        ["`"]     = "actions.cd",
        ["~"]     = "actions.tcd",
        ["gs"]    = "actions.change_sort",
        ["gx"]    = "actions.open_external",
        ["g."]    = "actions.toggle_hidden",
        ["g\\"]   = "actions.toggle_trash",
      },
    },
  },

  {
    "echasnovski/mini.animate",
    event = "VeryLazy",
    config = function()
      local ok, animate = pcall(require, "mini.animate")
      if not ok then
        vim.notify("mini.animate failed to load", vim.log.levels.WARN)
        return
      end

      local mouse_scrolled = false
      for _, dir in ipairs({ "Up", "Down" }) do
        local key = "<ScrollWheel" .. dir .. ">"
        vim.keymap.set({ "", "i" }, key, function()
          mouse_scrolled = true
          return key
        end, { expr = true })
      end

      animate.setup({
        resize = { timing = animate.gen_timing.linear({ duration = 50,  unit = "total" }) },
        open   = { timing = animate.gen_timing.linear({ duration = 40,  unit = "total" }) },
        close  = { timing = animate.gen_timing.linear({ duration = 40,  unit = "total" }) },
        scroll = {
          timing    = animate.gen_timing.linear({ duration = 80, unit = "total" }),
          subscroll = animate.gen_subscroll.equal({
            predicate = function(total_scroll)
              if vim.b[0] and vim.b[0].large_file then return false end
              if mouse_scrolled then mouse_scrolled = false; return false end
              return math.abs(total_scroll) > 5
            end,
          }),
        },
      })
    end,
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 4. FOCUS TOOLS
  -- ═══════════════════════════════════════════════════════════════════════════

  -- focus.lua drives twilight manually when using <leader>uF.
  -- Using <leader>uz directly activates zen without twilight — intentional.
  {
    "folke/zen-mode.nvim",
    cmd  = "ZenMode",
    keys = { { "<leader>uz", "<cmd>ZenMode<cr>", desc = "Zen mode" } },
    opts = {
      window = {
        backdrop  = 0.95,
        width     = 0.75,
        height    = 1,
        options   = {
          signcolumn     = "no",
          number         = false,
          relativenumber = false,
          cursorline     = false,
          cursorcolumn   = false,
          foldcolumn     = "0",
          list           = false,
        },
      },
      plugins = {
        options  = { enabled = true, ruler = false, showcmd = false },
        twilight = { enabled = false },   -- focus.lua drives twilight manually
        gitsigns = { enabled = false },
        tmux     = { enabled = false },
      },
    },
  },

  {
    "folke/twilight.nvim",
    cmd  = "Twilight",
    keys = { { "<leader>uT", "<cmd>Twilight<cr>", desc = "Twilight focus" } },
    opts = {
      dimming    = { alpha = 0.25, color = { "Normal", "#ffffff" }, inactive = false },
      context    = 15,
      treesitter = true,
      expand     = { "function", "method", "table", "if_statement" },
    },
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 5. MISC
  -- ═══════════════════════════════════════════════════════════════════════════

  -- TodoTelescope key owned by editor.lua primary todo-comments spec.
  -- blame.nvim owned by git.lua.
}
