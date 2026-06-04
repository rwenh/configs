-- lua/plugins/specs/hud.lua — visual enhancement plugins
--

return {

  -- ── Indent guides ─────────────────────────────────────────────────────────
  { "lukas-reineke/indent-blankline.nvim", event = { "BufReadPost","BufNewFile" }, main = "ibl",
    opts = {
      indent  = { char = "│", tab_char = "│" },
      scope   = { enabled = true, show_start = true, show_end = false, highlight = { "Function","Label" } },
      exclude = { filetypes = { "help","dashboard","lazy","mason","notify","toggleterm","lazyterm","neo-tree","snacks_dashboard" } },
    } },

  -- ── Scrollview with scope-aware signs ────────────────────────────────────
  {
    "dstein64/nvim-scrollview",
    event = "BufReadPost",
    opts  = {
      current_only     = true,
      base             = "right",
      column           = 1,
      signs_on_startup = { "all" },
      diagnostics_severities = {
        vim.diagnostic.severity.ERROR,
        vim.diagnostic.severity.WARN,
      },
      -- Show search match count in the scrollbar.
      search_count = true,
    },
  },

  { "levouh/tint.nvim", event = "VeryLazy",
    cond = function() return vim.g.disable_tint ~= true end,
    opts = { tint = -30, saturation = 0.7, tint_background_colors = true,
             highlight_ignore_patterns = { "WinSeparator","Status.*","IndentBlankline.*","NvimTree.*" } } },

  { "sphamba/smear-cursor.nvim", event = "VeryLazy",
    cond = function() return vim.g.disable_smear_cursor ~= true end,
    opts = { stiffness = 0.8, trailing_exponent = 3, hide_target_hack = true, legacy_computing_symbols_support = false } },

  -- ── Noice ─────────────────────────────────────────────────────────────────
  { "folke/noice.nvim", event = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim","rcarriga/nvim-notify" },
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"]                 = true,
          ["cmp.entry.get_documentation"]                   = true,
        },
        hover = { enabled = true }, signature = { enabled = true }, progress = { enabled = false },
      },
      presets = { bottom_search = true, command_palette = true, long_message_to_split = true, inc_rename = true, lsp_doc_border = true },
      routes = {
        { filter = { event = "msg_show", kind = "", find = "written"            }, opts = { skip = true } },
        { filter = { event = "msg_show", kind = "", find = "%d+ lines"          }, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Query error"       }, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Impossible pattern"}, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Invalid node type" }, opts = { skip = true } },
        { filter = { event = "msg_show",             find = "Invalid syntax"    }, opts = { skip = true } },
      },
      views = {
        cmdline_popup = { position = { row = "40%", col = "50%" }, size = { width = 60, height = "auto" }, border = { style = "rounded", padding = { 0,1 } } },
        popupmenu     = { relative = "editor", position = { row = "43%", col = "50%" }, size = { width = 60, height = 10 }, border = { style = "rounded", padding = { 0,1 } } },
      },
    } },

  -- ── Barbecue + winbar fallback ────────────────────────────────────────────
  --
  {
    "utilyre/barbecue.nvim",
    event        = "LspAttach",
    dependencies = { "SmiteshP/nvim-navic", "nvim-tree/nvim-web-devicons" },
    opts = { attach_navic = false, show_modified = true },
    config = function(_, opts)
      local ok, barbecue = pcall(require, "barbecue")
      if not ok then return end
      pcall(function() barbecue.setup(opts) end)

      -- Fallback winbar for non-LSP buffers or when barbecue has nothing to show.
      vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
        group    = vim.api.nvim_create_augroup("WinbarFallback", { clear = true }),
        callback = function(e)
          if vim.bo[e.buf].buftype ~= "" then
            vim.wo.winbar = nil
            return
          end
          -- Only set the fallback when barbecue hasn't set its own winbar.
          vim.defer_fn(function()
            if vim.wo.winbar and vim.wo.winbar ~= "" then return end
            local rel = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(e.buf), ":~:.")
            if rel ~= "" then
              vim.wo.winbar = " " .. rel
            end
          end, 50)
        end,
        desc = "Set minimal winbar fallback when barbecue has none",
      })
    end,
  },

  { "Wansmer/symbol-usage.nvim", event = "LspAttach",
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
        require("symbol-usage").setup({ hl = { link = "Comment" }, vt_position = "end_of_line",
          request_pending_text = false, text_format = fmt })
      end)
    end },

  { "smjonas/inc-rename.nvim", cmd = "IncRename", opts = {} },

  -- ── Navigation helpers ─────────────────────────────────────────────────────
  { "karb94/neoscroll.nvim", event = "VeryLazy",
    opts = { mappings = { "<C-u>","<C-d>","zt","zz","zb" }, hide_cursor = true,
             stop_eof = true, respect_scrolloff = true, easing_function = "sine",
             cursor_scrolls_alone = true } },

  { "stevearc/oil.nvim", cmd = "Oil",
    keys = { { "<leader>eo", "<cmd>Oil<cr>", desc = "Oil file editor" } },
    opts = {
      default_file_explorer = false,
      view_options = { show_hidden = true },
      float = { padding = 2, border = "rounded", win_options = { winblend = 5 } },
      keymaps = {
        ["<CR>"]  = "actions.select",  ["<C-s>"] = "actions.select_vsplit",
        ["<C-p>"] = "actions.preview", ["-"]     = "actions.parent",
        ["_"]     = "actions.open_cwd",["g."]    = "actions.toggle_hidden",
      },
    } },

  { "echasnovski/mini.animate", event = "VeryLazy",
    config = function()
      local ok, animate = pcall(require, "mini.animate")
      if not ok then return end
      local mouse_scrolled = false
      for _, dir in ipairs({ "Up","Down" }) do
        local key = "<ScrollWheel" .. dir .. ">"
        vim.keymap.set({ "","i" }, key, function() mouse_scrolled = true; return key end, { expr = true })
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
    end },

  -- ── Focus tools ────────────────────────────────────────────────────────────
  { "folke/zen-mode.nvim", cmd = "ZenMode",
    keys = { { "<leader>uz", "<cmd>ZenMode<cr>", desc = "Zen mode" } },
    opts = {
      window = { backdrop = 0.95, width = 0.75, height = 1,
                 options = { signcolumn = "no", number = false, relativenumber = false,
                             cursorline = false, cursorcolumn = false, foldcolumn = "0", list = false } },
      plugins = { options = { enabled = true, ruler = false, showcmd = false },
                  twilight = { enabled = false }, gitsigns = { enabled = false }, tmux = { enabled = false } },
    } },

  { "folke/twilight.nvim", cmd = "Twilight",
    keys = { { "<leader>uT", "<cmd>Twilight<cr>", desc = "Twilight focus" } },
    opts = { dimming = { alpha = 0.25, color = { "Normal","#ffffff" }, inactive = false },
             context = 15, treesitter = true,
             expand  = { "function","method","table","if_statement" } } },
}
