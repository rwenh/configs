-- lua/plugins/specs/lang/database.lua - Database tools
-- vim-dadbod-ui is the canonical definition in sql.lua.
-- This file adds the DBUI/DBUIToggle commands for non-SQL filetypes (e.g., opening from dashboard).

return {
  {
    "kristijanhusak/vim-dadbod-ui",
    optional = true,   -- sql.lua owns the actual spec; this just ensures the cmds are accessible
    cmd = { "DBUI", "DBUIToggle" },
  },
}
