-- lua/plugins/specs/lang/sql.lua - SQL development

return {
  -- Conform: sqlfmt formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.sql   = { "sqlfmt" }
      opts.formatters_by_ft.mysql = { "sqlfmt" }
    end,
  },
}
