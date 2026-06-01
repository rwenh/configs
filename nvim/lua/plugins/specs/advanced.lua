-- lua/plugins/specs/advanced.lua
--

return {

  -- ── Icons (loaded first — many plugins depend on this) ────────────────────

  {
    "nvim-tree/nvim-web-devicons",
    lazy     = false,
    priority = 950,
    opts = {
      default     = true,
      color_icons = true,
      variant     = "default",
      strict      = true,
    },
    config = function(_, opts)
      require("nvim-web-devicons").setup(opts)
    end,
  },

  -- ── Escape handling ────────────────────────────────────────────────────────
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts  = {
      timeout          = 200,
      default_mappings = false,
      mappings = {
        i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
      },
    },
  },

  -- ── Bracket matching (replaces matchparen) ────────────────────────────────

  {
    "andymass/vim-matchup",
    event = { "BufReadPost", "BufNewFile" },
    init  = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred  = 1
      vim.g.matchup_matchparen_hi_surround_always = 0
      vim.g.matchup_matchpref = { html = { tagnameonly = 1 } }
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      opts.matchup        = opts.matchup or {}
      opts.matchup.enable = true
    end,
  },

  -- ── Symbol navigation / breadcrumbs ───────────────────────────────────────
  {
    "SmiteshP/nvim-navic",
    event = "LspAttach",
    opts  = {
      icons = require("core.util.icons").kinds,
      lsp   = { auto_attach = false, preference = nil },
      highlight             = true,
      separator             = " > ",
      depth_limit           = 5,
      depth_limit_indicator = "..",
      safe_output           = true,
      lazy_update_context   = false,
      click                 = false,
      format_text           = function(text) return text end,
    },
  },

  -- ── Color highlighting ─────────────────────────────────────────────────────

  {
    "brenoprata10/nvim-highlight-colors",
    event = "BufReadPost",
    opts  = {
      render              = "background",  -- "background" | "foreground" | "virtual"
      enable_named_colors = true,
      enable_tailwind     = true,
      exclude_filetypes   = { "lazy", "mason", "lspinfo", "snacks_dashboard" },
      exclude_buftypes    = { "nofile", "prompt" },
    },
    config = function(_, opts)
      pcall(function() require("nvim-highlight-colors").setup(opts) end)
    end,
  },

  -- ── Rainbow delimiters ─────────────────────────────────────────────────────
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local rd = require("rainbow-delimiters")
      pcall(function()
        require("rainbow-delimiters.setup").setup({
          strategy = {
            [""]  = rd.strategy["global"],
            vim   = rd.strategy["local"],
            latex = rd.strategy["local"],
          },
          query = {
            [""]  = "rainbow-delimiters",
            lua   = "rainbow-blocks",
            latex = "rainbow-delimiters-latex",
          },
          priority  = { [""] = 110, lua = 210, latex = 210 },
          highlight = {
            "RainbowDelimiterRed", "RainbowDelimiterYellow", "RainbowDelimiterBlue",
            "RainbowDelimiterOrange", "RainbowDelimiterGreen", "RainbowDelimiterViolet",
            "RainbowDelimiterCyan",
          },
          blacklist = { "html", "markdown", "" },
        })
      end)
    end,
  },

  -- ── Motion helpers ─────────────────────────────────────────────────────────
  { "tpope/vim-repeat",  event = "VeryLazy" },
  { "tpope/vim-abolish", event = "VeryLazy" },

  {
    "gbprod/stay-in-place.nvim",
    event = "VeryLazy",
    opts  = {},
  },

  -- ── mini.* editing enhancements (unified spec) ────────────────────────────

  {
    "echasnovski/mini.nvim",
    event = "VeryLazy",
    keys  = {
      { "gc",          mode = { "n", "v" } },
      { "gcc" },
      { "ga",          mode = { "n", "v" } },
      { "gA",          mode = { "n", "v" } },
      { "gS" },
      { "gsa",         mode = "v"          },
      { "gsd" }, { "gsf" }, { "gsF" },
      { "gsh" }, { "gsr" },
      { "<A-h>", mode = { "n", "v", "i" } },
      { "<A-j>", mode = { "n", "v", "i" } },
      { "<A-k>", mode = { "n", "v", "i" } },
      { "<A-l>", mode = { "n", "v", "i" } },
    },
    config = function()
      for _, mod in ipairs({
        "align", "comment", "move", "splitjoin", "surround"
      }) do
        local ok, err = pcall(function() require("mini." .. mod).setup() end)
        if not ok then
          vim.notify(
            string.format("[mini.%s] setup failed: %s", mod, tostring(err)),
            vim.log.levels.WARN
          )
        end
      end
    end,
  },

  -- ── Undo tree ──────────────────────────────────────────────────────────────
  {
    "mbbill/undotree",
    cmd  = "UndotreeToggle",
    keys = { { "<leader>xu", "<cmd>UndotreeToggle<CR>", desc = "Undo Tree" } },
    init = function()
      vim.g.undotree_SetFocusWhenToggle = 1
      vim.g.undotree_ShortIndicators    = 1
      vim.g.undotree_WindowLayout       = 2
      vim.g.undotree_DiffpanelHeight    = 8
    end,
  },

  -- ── Folding enhancement (nvim-ufo) ────────────────────────────────────────

  {
    "kevinhwang91/nvim-ufo",
    event        = "VeryLazy",
    dependencies = { "kevinhwang91/promise-async" },
    opts = function()
      local function fold_text(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local suffix      = ("  %d "):format(endLnum - lnum)
        local sufWidth    = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth    = 0
        for _, chunk in ipairs(virtText) do
          local chunkText  = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            table.insert(newVirtText, { chunkText, chunk[2] })
            break
          end
          curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end

      return {
        fold_virt_text_handler  = fold_text,
        open_fold_hl_timeout    = 400,
        close_fold_kinds_for_ft = { _ = { "imports", "comment" } },
        preview = {
          win_config = {
            border      = { "", "─", "", "", "", "─", "", "" },
            winhighlight = "Normal:Folded",
            winblend    = 12,
          },
          mappings = {
            scrollU = "<C-u>", scrollD = "<C-d>",
            jumpTop = "[{",    jumpBot = "]}",
          },
        },
        provider_selector = function(bufnr, _ft, _bt)
          if vim.b[bufnr] and vim.b[bufnr].large_file then return "" end
          return { "treesitter", "indent" }
        end,
      }
    end,

    config = function(_, opts)
      local ok, ufo = pcall(require, "ufo")
      if not ok then
        vim.notify("nvim-ufo failed to load", vim.log.levels.WARN)
        return
      end
      local setup_ok = pcall(function() ufo.setup(opts) end)
      if not setup_ok then
        vim.notify("nvim-ufo setup failed", vim.log.levels.WARN)
        return
      end
      vim.keymap.set("n", "zR", ufo.openAllFolds)
      vim.keymap.set("n", "zM", ufo.closeAllFolds)
      vim.keymap.set("n", "zr", ufo.openFoldsExceptKinds)
      vim.keymap.set("n", "zm", ufo.closeFoldsWith)
    end,
  },

  -- ── Annotations (neogen) ──────────────────────────────────────────────────

  {
    "danymat/neogen",
    lazy = true,
    cmd  = "Neogen",
    keys = {
      { "<leader>xg", "<cmd>Neogen<CR>", desc = "Generate docstring" },
    },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "L3MON4D3/LuaSnip",
    },
    opts = {
      snippet_engine = "luasnip",
      languages = {
        lua        = { template = { annotation_convention = "emmylua"           } },
        typescript = { template = { annotation_convention = "tsdoc"             } },
        javascript = { template = { annotation_convention = "jsdoc"             } },
        rust       = { template = { annotation_convention = "nightly"           } },
        go         = { template = { annotation_convention = "go"                } },
        cpp        = { template = { annotation_convention = "doxygen"           } },
        c          = { template = { annotation_convention = "doxygen"           } },
        python     = { template = { annotation_convention = "google_docstrings" } },
        java       = { template = { annotation_convention = "javadoc"           } },
        kotlin     = { template = { annotation_convention = "kdoc"              } },
      },
    },
  },
}
