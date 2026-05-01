-- lua/plugins/specs/hud.lua — visual enhancement plugins
--
-- Organised into clearly labelled sections:
--   1. Editor chrome       (indent guides, scrollview, tint, smear cursor)
--   2. Overlay / popups    (noice, barbecue, symbol-usage, inc-rename)
--   3. Navigation helpers  (oil, neoscroll, mini.animate)
--   4. Focus tools         (zen-mode, twilight)
--   5. Misc               (blame — belongs in git.lua architecturally)
--

return {

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 1. EDITOR CHROME
  -- ═══════════════════════════════════════════════════════════════════════════

  -- ── Indent guides ──────────────────────────────────────────────────────────

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
          "toggleterm", "lazyterm",
          "neo-tree",
          "snacks_dashboard",
        },
      },
    },
  },

  -- ── Scroll minimap ─────────────────────────────────────────────────────────

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

  -- ── Window dimmer ──────────────────────────────────────────────────────────
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

  -- ── Cursor smear ───────────────────────────────────────────────────────────
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

  -- ── Noice ──────────────────────────────────────────────────────────────────

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
        { filter = { event = "msg_show", kind = "",    find = "written"          }, opts = { skip = true } },
        { filter = { event = "msg_show", kind = "",    find = "%d+ lines"        }, opts = { skip = true } },
        { filter = { event = "msg_show",               find = "Query error"      }, opts = { skip = true } },
        { filter = { event = "msg_show",               find = "Impossible pattern"}, opts = { skip = true } },
        { filter = { event = "msg_show",               find = "Invalid node type"}, opts = { skip = true } },
        { filter = { event = "msg_show",               find = "Invalid syntax"   }, opts = { skip = true } },
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

  -- ── Breadcrumb bar ─────────────────────────────────────────────────────────
  {
    "utilyre/barbecue.nvim",
    event        = "LspAttach",
    dependencies = { "SmiteshP/nvim-navic", "nvim-tree/nvim-web-devicons" },
    opts = {
      -- attach_navic=false: navic is attached in lsp.lua's LspAttach callback.
      attach_navic = false,
      show_modified = true,
    },
  },

  -- ── Symbol usage (reference counts) ───────────────────────────────────────

  {
    "Wansmer/symbol-usage.nvim",
    event = "LspAttach",
    config = function()
      local function format_symbol(symbol)
        local parts = {}
        if symbol.references then
          local n     = symbol.references
          local label = n <= 1 and "1 use" or (n .. " uses")
          table.insert(parts, ("󰌹 %s"):format(label))
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
          text_format = format_symbol,
        })
      end)
    end,
  },

  -- ── inc-rename ─────────────────────────────────────────────────────────────

  {
    "smjonas/inc-rename.nvim",
    cmd  = "IncRename",
    opts = {},
  },

  -- ═══════════════════════════════════════════════════════════════════════════
  -- 3. NAVIGATION HELPERS
  -- ═══════════════════════════════════════════════════════════════════════════

  -- ── Smooth scroll ──────────────────────────────────────────────────────────

  {
    "karb94/neoscroll.nvim",
    event = "VeryLazy",
    opts  = {
      mappings            = { "<C-u>", "<C-d>", "zt", "zz", "zb" },
      hide_cursor         = true,
      stop_eof            = true,
      respect_scrolloff   = true,
      easing_function     = "sine",
      cursor_scrolls_alone = true,
    },
  },

  -- ── File manager inline (oil) ──────────────────────────────────────────────

  {
    "stevearc/oil.nvim",
    cmd  = "Oil",
    keys = {
      { "<leader>eo", "<cmd>Oil<cr>", desc = "Oil file editor" },
    },
    opts = {
      default_file_explorer = false,   -- netrw already disabled in options.lua
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

  -- ── mini.animate ───────────────────────────────────────────────────────────

  -- The flag is set before the scroll event processes and reset in predicate.
  -- Rapid trackpad scrolls may cause the second event to animate unexpectedly;
  -- this is a cosmetic limitation of the flag approach, not a data hazard.
  {
    "echasnovski/mini.animate",
    event = "VeryLazy",
    config = function()
      local ok, animate = pcall(require, "mini.animate")
      if not ok then
        vim.notify("mini.animate failed to load", vim.log.levels.WARN)
        return
      end

      -- Mouse scroll flag: prevents animating mouse-wheel scrolls.
      -- Note: rapid trackpad scrolls may occasionally animate due to the
      -- single-flag approach.
      local mouse_scrolled = false
      for _, direction in ipairs({ "Up", "Down" }) do
        local key = "<ScrollWheel" .. direction .. ">"
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

  -- ── Zen mode ───────────────────────────────────────────────────────────────
  -- When using <leader>uz directly (without focus.lua), twilight does NOT
  -- activate — this is intentional asymmetry documented here.
  -- To always activate twilight with zen, enable it here AND remove the
  -- focus.lua manual calls.
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
          signcolumn    = "no",
          number        = false,
          relativenumber = false,
          cursorline    = false,
          cursorcolumn  = false,
          foldcolumn    = "0",
          list          = false,
        },
      },
      plugins = {
        options  = { enabled = true, ruler = false, showcmd = false },
        -- FIX B7: disabled here; focus.lua drives twilight manually.
        twilight = { enabled = false },
        gitsigns = { enabled = false },
        tmux     = { enabled = false },
      },
    },
  },

  -- ── Twilight ───────────────────────────────────────────────────────────────
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

  -- ── Git blame virtual text ─────────────────────────────────────────────────
  -- The <leader>.B keymap is documented in
  -- KEYMAP_REFERENCE.md under the Git section.
  -- TODO: move to git.lua in a future consolidation pass.
  {
    "FabijanZulj/blame.nvim",
    cmd  = "BlameToggle",
    keys = { { "<leader>.B", "<cmd>BlameToggle<cr>", desc = "Toggle git blame" } },
    opts = {
      date_format       = "%Y-%m-%d",
      virtual_style     = "right_align",
      focus_blame       = true,
      merge_consecutive = false,
      max_summary_width = 30,
    },
  },

  -- ── TodoTelescope key extension ────────────────────────────────────────────
  -- This optional=true spec adds only the <leader>xT key; the key could live
  -- in editor.lua's primary spec keys= table but is kept here to avoid making
  -- editor.lua depend on the xT prefix being free.
  {
    "folke/todo-comments.nvim",
    optional = true,
    keys = {
      { "<leader>xT", "<cmd>TodoTelescope<cr>", desc = "Find TODOs" },
    },
  },

  -- ── Notifications backend ──────────────────────────────────────────────────
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
}
