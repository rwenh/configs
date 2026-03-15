-- lua/plugins/specs/lang/database.lua - Database tools
-- Single source of truth for dadbod + UI. sql.lua is now empty (or removed).

return {
  {
    "tpope/vim-dadbod",
    lazy = true,
  },

  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      "tpope/vim-dadbod",
      "kristijanhusak/vim-dadbod-completion",
    },
    cmd  = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    keys = {
      { "<leader>dbu", "<cmd>DBUIToggle<cr>",        desc = "DB UI Toggle" },
      { "<leader>dba", "<cmd>DBUIAddConnection<cr>", desc = "DB Add Connection" },
      { "<leader>dbf", "<cmd>DBUIFindBuffer<cr>",    desc = "DB Find Buffer" },
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts  = 1
      vim.g.db_ui_save_location   = vim.fn.stdpath("data") .. "/dadbod-ui"
      vim.g.db_ui_show_database_icon = 1
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_win_position    = "left"
      vim.g.db_ui_winwidth        = 40
    end,
  },

  -- blink.cmp source for DB completion
  {
    "kristijanhusak/vim-dadbod-completion",
    lazy = true,
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = { "sql", "mysql", "plsql", "psql" },
        callback = function()
          -- blink.cmp picks up vim-dadbod-completion automatically via LSP fallback
          vim.opt_local.omnifunc = "vim_dadbod_completion#omni"
        end,
      })
    end,
  },

  -- Treesitter parsers for SQL dialects
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "sql" })
      end
    end,
  },
}
