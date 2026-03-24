-- lua/plugins/specs/lang/database.lua - Database tools
-- Single source of truth for dadbod + UI. sql.lua defers to this file.

return {
  -- FIX #4: Removed standalone { "tpope/vim-dadbod", lazy = true } spec.
  -- vim-dadbod-ui already declares it as a dependency — Lazy loads it
  -- automatically. The redundant spec adds nothing.

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
    -- FIX #1: Added ft trigger — previously lazy=true with no trigger meant
    -- config never ran and omnifunc was never set for SQL buffers.
    ft     = { "sql", "mysql", "plsql", "psql" },
    config = function()
      -- FIX #2: Added augroup to prevent autocmd accumulation on reload.
      -- FIX #3: Corrected comment — blink.cmp does NOT pick this up
      -- automatically. The omnifunc must be set explicitly, which is what
      -- this autocmd does. Remove this block only if you add
      -- vim-dadbod-completion as an explicit blink source instead.
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("DadbodCompletion", { clear = true }),
        pattern  = { "sql", "mysql", "plsql", "psql" },
        callback = function()
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
