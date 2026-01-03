-- lua/plugins/specs/lang/sql.lua - SQL development

return {
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = "tpope/vim-dadbod",
    ft = "sql",
    cmd = { "DBUI", "DBUIToggle" },
  },

  {
    "nanotee/sqls.nvim",
    ft = "sql",
    dependencies = "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")
      lspconfig.sqls.setup({})
    end,
  },
}