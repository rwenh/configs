-- lua/plugins/specs/lang/database.lua - Database tools
--
-- FIX (v2.2.3):
--   • vim-dadbod-completion ft trigger listed "psql" — Neovim's filetype for
--     PostgreSQL is "sql" (set via autocmd or ftdetect), never "psql".
--     The completion source never loaded for .sql files opened as PostgreSQL.
--     Fixed: "psql" removed; canonical filetypes are sql/mysql/plsql.

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

  {
    "kristijanhusak/vim-dadbod-completion",
    -- FIX: "psql" is not a Neovim filetype — removed.
    -- Neovim detects PostgreSQL files as "sql". "plsql" covers PL/SQL (Oracle).
    ft     = { "sql", "mysql", "plsql" },
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("DadbodCompletion", { clear = true }),
        pattern  = { "sql", "mysql", "plsql" },
        callback = function()
          pcall(function()
            vim.opt_local.omnifunc = "vim_dadbod_completion#omni"
          end)
        end,
      })
    end,
  },

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
