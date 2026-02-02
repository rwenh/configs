-- lua/plugins/specs/lang/sql.lua - SQL development
-- Note: LSP (sqls) is configured in lsp.lua, no need to duplicate here

return {
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = "tpope/vim-dadbod",
    ft = "sql",
    cmd = { "DBUI", "DBUIToggle" },
  },
}
