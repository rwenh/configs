-- lua/plugins/specs/advanced.lua — editing enhancements
--

return {

  { "nvim-tree/nvim-web-devicons", lazy = false, priority = 950,
    opts = { default = true, color_icons = true, variant = "default", strict = true },
    config = function(_, opts) require("nvim-web-devicons").setup(opts) end },

  { "max397574/better-escape.nvim", event = "InsertEnter",
    opts = { timeout = 200, default_mappings = false,
             mappings = { i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } } } } },

  { "andymass/vim-matchup", event = { "BufReadPost","BufNewFile" },
    init = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred  = 1
    end },

  { "nvim-treesitter/nvim-treesitter", optional = true,
    opts = function(_, opts) opts.matchup = opts.matchup or {}; opts.matchup.enable = true end },

  { "SmiteshP/nvim-navic", event = "LspAttach",
    opts = { icons = require("core.util.icons").kinds, lsp = { auto_attach = false },
             highlight = true, separator = " > ", depth_limit = 5, safe_output = true } },

  { "brenoprata10/nvim-highlight-colors", event = "BufReadPost",
    opts = { render = "background", enable_named_colors = true, enable_tailwind = true,
             exclude_filetypes = { "lazy","mason","lspinfo","snacks_dashboard" } },
    config = function(_, opts) pcall(function() require("nvim-highlight-colors").setup(opts) end) end },

  { "HiPhish/rainbow-delimiters.nvim", event = { "BufReadPre","BufNewFile" },
    config = function()
      local rd = require("rainbow-delimiters")
      pcall(function()
        require("rainbow-delimiters.setup").setup({
          strategy  = { [""] = rd.strategy["global"], vim = rd.strategy["local"] },
          query     = { [""] = "rainbow-delimiters", lua = "rainbow-blocks" },
          highlight = { "RainbowDelimiterRed","RainbowDelimiterYellow","RainbowDelimiterBlue",
                        "RainbowDelimiterOrange","RainbowDelimiterGreen","RainbowDelimiterViolet","RainbowDelimiterCyan" },
          blacklist = { "html","markdown","" },
        })
      end)
    end },

  { "tpope/vim-repeat",  event = "VeryLazy" },
  { "tpope/vim-abolish", event = "VeryLazy" },
  { "gbprod/stay-in-place.nvim", event = "VeryLazy", opts = {} },

  -- ── mini.nvim — unified spec ───────────────────────────────────────────────
  {
    "echasnovski/mini.nvim",
    event = "VeryLazy",
    keys  = {
      { "gc", mode = { "n","v" } }, { "gcc" },
      { "ga", mode = { "n","v" } }, { "gA", mode = { "n","v" } },
      { "gS" },
      { "gsa", mode = "v" }, { "gsd" }, { "gsf" }, { "gsF" }, { "gsh" }, { "gsr" },
      { "<A-h>", mode = { "n","v","i" } }, { "<A-j>", mode = { "n","v","i" } },
      { "<A-k>", mode = { "n","v","i" } }, { "<A-l>", mode = { "n","v","i" } },
    },
    config = function()
      -- Core editing modules
      for _, mod in ipairs({ "align","comment","move","splitjoin","surround" }) do
        local ok, err = pcall(function() require("mini." .. mod).setup() end)
        if not ok then vim.notify(string.format("[mini.%s] setup failed: %s", mod, tostring(err)), vim.log.levels.WARN) end
      end

      -- ── mini.pairs — auto-close brackets, quotes, etc. ───────────────────
      if vim.g.disable_mini_pairs ~= true then
        pcall(function()
          require("mini.pairs").setup({
            modes   = { insert = true, command = false, terminal = false },
            mappings = {
              ["("]  = { action = "open",  pair = "()",  neigh_pattern = "[^\\]." },
              ["["]  = { action = "open",  pair = "[]",  neigh_pattern = "[^\\]." },
              ["{"]  = { action = "open",  pair = "{}",  neigh_pattern = "[^\\]." },
              [")"]  = { action = "close", pair = "()",  neigh_pattern = "[^\\]." },
              ["]"]  = { action = "close", pair = "[]",  neigh_pattern = "[^\\]." },
              ["}"]  = { action = "close", pair = "{}",  neigh_pattern = "[^\\]." },
              ['"']  = { action = "closeopen", pair = '""', neigh_pattern = "[^\\].", register = { cr = false } },
              ["'"]  = { action = "closeopen", pair = "''", neigh_pattern = "[^%a\\].", register = { cr = false } },
              ["`"]  = { action = "closeopen", pair = "``", neigh_pattern = "[^\\].", register = { cr = false } },
            },
          })
        end)
      end

      -- ── mini.visits — frecency tracking (config also in editor.lua) ──────
      -- Setup is idempotent;
      pcall(function()
        require("mini.visits").setup({
          store = { path = vim.fn.stdpath("data") .. "/mini-visits.json" },
        })
      end)
    end,
  },

  -- ── mini.files — file explorer alternative ────────────────────────────────
  --
  -- Lightweight file manager: directory tree in a floating buffer.
  -- <leader>em  open mini.files at the current file's directory.
  -- <leader>eM  open mini.files at the project root.
  -- Coexists with neo-tree; use whichever fits the task.
  {
    "echasnovski/mini.nvim",
    keys = {
      {
        "<leader>em",
        function()
          local ok, mf = pcall(require, "mini.files")
          if not ok then vim.notify("[mini.files] not available", vim.log.levels.WARN); return end
          mf.open(vim.fn.expand("%:p:h"), true)
        end,
        desc = "mini.files (current dir)",
      },
      {
        "<leader>eM",
        function()
          local ok, mf = pcall(require, "mini.files")
          if not ok then vim.notify("[mini.files] not available", vim.log.levels.WARN); return end
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          mf.open(root, true)
        end,
        desc = "mini.files (project root)",
      },
    },
    config = function()
      pcall(function()
        require("mini.files").setup({
          windows = { preview = true, width_focus = 50, width_preview = 70 },
          options = { use_as_default_explorer = false },
        })
      end)
    end,
  },

  -- ── undotree ──────────────────────────────────────────────────────────────
  { "mbbill/undotree", cmd = "UndotreeToggle",
    keys = { { "<leader>xu", "<cmd>UndotreeToggle<CR>", desc = "Undo Tree" } },
    init = function()
      vim.g.undotree_SetFocusWhenToggle = 1
      vim.g.undotree_ShortIndicators    = 1
      vim.g.undotree_WindowLayout       = 2
    end },

  -- ── nvim-ufo ──────────────────────────────────────────────────────────────
  {
    "kevinhwang91/nvim-ufo",
    event = "VeryLazy", dependencies = { "kevinhwang91/promise-async" },
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
            table.insert(newVirtText, { chunkText, chunk[2] }); break
          end
          curWidth = curWidth + chunkWidth
        end
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end
      return {
        fold_virt_text_handler  = fold_text,
        open_fold_hl_timeout    = 400,
        close_fold_kinds_for_ft = { _ = { "imports","comment" } },
        provider_selector = function(bufnr)
          if vim.b[bufnr] and vim.b[bufnr].large_file then return "" end
          return { "treesitter","indent" }
        end,
      }
    end,
    config = function(_, opts)
      local ok, ufo = pcall(require, "ufo")
      if not ok then return end
      pcall(function() ufo.setup(opts) end)
      vim.keymap.set("n", "zR", ufo.openAllFolds)
      vim.keymap.set("n", "zM", ufo.closeAllFolds)
      vim.keymap.set("n", "zr", ufo.openFoldsExceptKinds)
      vim.keymap.set("n", "zm", ufo.closeFoldsWith)
    end,
  },

  -- ── neogen ────────────────────────────────────────────────────────────────
  { "danymat/neogen", lazy = true, cmd = "Neogen",
    keys = { { "<leader>xg", "<cmd>Neogen<CR>", desc = "Generate docstring" } },
    dependencies = { "nvim-treesitter/nvim-treesitter", "L3MON4D3/LuaSnip" },
    opts = {
      snippet_engine = "luasnip",
      languages = {
        lua = { template = { annotation_convention = "emmylua" } },
        typescript = { template = { annotation_convention = "tsdoc" } },
        javascript = { template = { annotation_convention = "jsdoc" } },
        rust  = { template = { annotation_convention = "nightly" } },
        go    = { template = { annotation_convention = "go" } },
        cpp   = { template = { annotation_convention = "doxygen" } },
        c     = { template = { annotation_convention = "doxygen" } },
        python = { template = { annotation_convention = "google_docstrings" } },
        java  = { template = { annotation_convention = "javadoc" } },
        kotlin = { template = { annotation_convention = "kdoc" } },
      },
    } },
}
