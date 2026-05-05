-- lua/plugins/specs/lang/database.lua — database UI and SQL completion
--

local shared = require("plugins.specs.lang.shared")
return {
  -- ── vim-dadbod-ui ──────────────────────────────────────────────────────────
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      "tpope/vim-dadbod",
      "kristijanhusak/vim-dadbod-completion",
    },
    cmd  = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    keys = {
      { "<leader>dbu", "<cmd>DBUIToggle<cr>",        desc = "DB UI Toggle"        },
      { "<leader>dba", "<cmd>DBUIAddConnection<cr>", desc = "DB Add Connection"   },
      { "<leader>dbf", "<cmd>DBUIFindBuffer<cr>",    desc = "DB Find Buffer"      },
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts           = 1
      vim.g.db_ui_save_location            = vim.fn.stdpath("data") .. "/dadbod-ui"
      vim.g.db_ui_show_database_icon       = 1
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_win_position             = "left"
      vim.g.db_ui_winwidth                 = 40
    end,
  },

  -- ── vim-dadbod-completion ──────────────────────────────────────────────────

  {
    "kristijanhusak/vim-dadbod-completion",
    ft     = { "sql", "mysql", "plsql" },
    config = function()
      -- Omnifunc fallback for manual <C-x><C-o> completion.
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("DadbodCompletion", { clear = true }),
        pattern  = { "sql", "mysql", "plsql" },
        callback = function()
          pcall(function()
            vim.opt_local.omnifunc = "vim_dadbod_completion#omni"
          end)
        end,
      })

      pcall(function()
        local ok, blink = pcall(require, "blink.cmp")
        if ok and blink.add_source then
        end
      end)
    end,
  },

  shared.treesitter({ "sql" }),

  -- ── Conform: sqlfmt (merged from sql.lua) ──────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      if vim.fn.executable("sqlfmt") ~= 1 then
        return
      end
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      for _, ft in ipairs({ "sql", "mysql" }) do
        opts.formatters_by_ft[ft] = { "sqlfmt" }
      end
    end,
  },
}
