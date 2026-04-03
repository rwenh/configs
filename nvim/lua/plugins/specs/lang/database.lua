-- lua/plugins/specs/lang/database.lua - Database tools

return {
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
      vim.g.db_ui_use_nerd_fonts           = 1
      vim.g.db_ui_save_location            = vim.fn.stdpath("data") .. "/dadbod-ui"
      vim.g.db_ui_show_database_icon       = 1
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_win_position             = "left"
      vim.g.db_ui_winwidth                 = 40
    end,
  },

  -- DB completion source
  {
    "kristijanhusak/vim-dadbod-completion",
    ft     = { "sql", "mysql", "plsql", "psql" },
    config = function()
      -- RECALIBRATION: Safe autocmd with augroup
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("DadbodCompletion", { clear = true }),
        pattern  = { "sql", "mysql", "plsql", "psql" },
        callback = function()
          pcall(function()
            vim.opt_local.omnifunc = "vim_dadbod_completion#omni"
          end)
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
