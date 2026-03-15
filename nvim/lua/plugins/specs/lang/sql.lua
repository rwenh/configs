-- lua/plugins/specs/lang/sql.lua
-- dadbod + UI are fully managed in database.lua.
-- This file adds SQL-specific conform formatter and lint only.

return {
  -- Conform: sqlfmt formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        sql   = { "sqlfmt" },
        mysql = { "sqlfmt" },
      },
    },
  },
}
