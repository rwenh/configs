-- lua/plugins/specs/hud.lua
--
-- FIX (v2.2.4):
--   • mini.animate scroll subscroll predicate now checks vim.b.large_file.
--   • folke/zen-mode.nvim spec present (focus.lua dependency).
--   • render-markdown.nvim absent here (markdown.lua owns it).
--
-- FIX (v2.3.1b):
--   • <leader>ft TodoTelescope key moved to <leader>xT.
--
-- FIX (v2.3.7):
--   • mini.animate: require("mini.animate") moved from opts=function() into
--     config() so it runs only after the plugin is confirmed loaded.
--
-- FIX (v2.3.9b):
--   • inc-rename.nvim keys= entry removed. The spec previously registered
--     <leader>,r as a GLOBAL normal-mode map (expr=true) pointing to IncRename.
--     lsp.lua's LspAttach callback already registers <leader>,r as a
--     BUFFER-LOCAL map with a pcall/fallback pattern. The global version here
--     leaked into every non-LSP buffer, was architecturally wrong (LSP tooling
--     living in the HUD spec), and caused which-key to show the entry twice.
--     lsp.lua is the sole owner of <leader>,r. inc-rename is kept here with
--     cmd="IncRename" only — lsp.lua triggers cmd-based lazy-loading via
--     pcall(vim.cmd, "IncRename " .. word) in its LspAttach handler.

return {

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main  = "ibl",
    opts  = {
      indent  = { char = "│", tab_char = "│" },
      scope   = { enabled = true, show_start = true, show_end = false,
                  highlight = { "Function", "Label" } },
      exclude = {
        filetypes = {
          "help", "dashboard", "lazy", "mason", "notify",
          "toggleterm", "lazyterm", "NvimTree",
        },
      },
    },
  },

  {
    "karb94/neoscroll.nvim",
    event = "VeryLazy",
    opts  = {
      mappings            = { "<C-u>", "<C-d>", "<C-b>", "<C-f>", "zt", "zz", "zb" },
      hide_cursor         = true,
      stop_eof            = true,
      respect_scrolloff   = true,
      easing_function     = "sine",
      cursor_scrolls_alone = true,
    },
  },

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
        progress  = { enabled = true },
      },
      presets = {
        bottom_search         = true,
        command_palette       = true,
        long_message_to_split = true,
        inc_rename            = true,
        lsp_doc_border        = true,
      },
      routes = {
        { filter = { event = "msg_show", find = "written" },            opts = { skip = true } },
        { filter = { event = "msg_show", find = "%d+ lines" },          opts = { skip = true } },
        { filter = { event = "msg_show", find = "Query error" },        opts = { skip = true } },
        { filter = { event = "msg_show", find = "Impossible pattern" }, opts = { skip = true } },
        { filter = { event = "msg_show", find = "Invalid node type" },  opts = { skip = true } },
        { filter = { event = "msg_show", find = "Invalid syntax" },     opts = { skip = true } },
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
      attach_navic   = false,
      create_autocmd = true,
      show_dirname   = false,
      show_basename  = false,
      show_modified  = true,
      show_navic     = true,
      theme          = "auto",
      symbols = {
        modified  = "●",
        ellipsis  = "…",
        separator = "›",
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
    "FabijanZulj/blame.nvim",
    cmd  = "BlameToggle",
    keys = { { "<leader>.B", "<cmd>BlameToggle<cr>", desc = "Toggle blame" } },
    opts = {
      date_format       = "%Y-%m-%d",
      virtual_style     = "right_align",
      focus_blame       = true,
      merge_consecutive = false,
      max_summary_width = 30,
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
    "stevearc/oil.nvim",
    cmd  = "Oil",
    keys = { { "<leader>eo", "<cmd>Oil<cr>", desc = "Oil file editor" } },
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

  -- FIX (v2.3.9b): keys= removed entirely.
  -- The previous spec registered <leader>,r globally as an expr map for
  -- IncRename. lsp.lua's LspAttach handler already owns <leader>,r as a
  -- buffer-local map with cmd-based lazy-load + pcall fallback. The global
  -- map here was architecturally wrong, leaked into non-LSP buffers, and
  -- duplicated the which-key entry. cmd="IncRename" is kept so lsp.lua's
  -- pcall(vim.cmd, "IncRename ...") can trigger lazy-loading correctly.
  {
    "smjonas/inc-rename.nvim",
    cmd  = "IncRename",
    opts = {},
  },

  {
    "Wansmer/symbol-usage.nvim",
    event = "LspAttach",
    opts  = {
      hl          = { link = "Comment" },
      vt_position = "end_of_line",
      request_pending_text = false,
      text_format = function(symbol)
        local res = {}
        if symbol.references then
          local usage = symbol.references <= 1 and "1 use"
                        or symbol.references .. " uses"
          table.insert(res, ("󰌹 %s"):format(usage))
        end
        if symbol.definition and symbol.definition > 0 then
          table.insert(res, ("󰳽 %s"):format(symbol.definition))
        end
        return table.concat(res, " │ ")
      end,
    },
  },

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
        options    = { enabled = true, ruler = false, showcmd = false },
        twilight   = { enabled = false },
        gitsigns   = { enabled = false },
        tmux       = { enabled = false },
      },
    },
  },

  {
    "folke/twilight.nvim",
    cmd  = "Twilight",
    keys = { { "<leader>uT", "<cmd>Twilight<cr>", desc = "Twilight focus" } },
    opts = {
      dimming   = { alpha = 0.25, color = { "Normal", "#ffffff" }, inactive = false },
      context   = 15,
      treesitter = true,
      expand    = { "function", "method", "table", "if_statement" },
    },
  },

  -- FIX (v2.3.7): mini.animate moved to config() from opts=function().
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
      for _, scroll in ipairs({ "Up", "Down" }) do
        local key = "<ScrollWheel" .. scroll .. ">"
        vim.keymap.set({ "", "i" }, key, function()
          mouse_scrolled = true
          return key
        end, { expr = true })
      end

      animate.setup({
        resize = { timing = animate.gen_timing.linear({ duration = 50, unit = "total" }) },
        open   = { timing = animate.gen_timing.linear({ duration = 40, unit = "total" }) },
        close  = { timing = animate.gen_timing.linear({ duration = 40, unit = "total" }) },
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

  -- FIX (v2.3.1b): key moved from <leader>ft to <leader>xT.
  {
    "folke/todo-comments.nvim",
    optional = true,
    keys = {
      { "<leader>xT", "<cmd>TodoTelescope<cr>", desc = "Find TODOs" },
    },
  },
}
