-- lua/plugins/specs/lang/c.lua - C development
-- clangd_extensions covers both C and C++; cpp.lua delegates here via optional.

return {
  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = {
      inlay_hints = { inline = true },
      ast = {
        role_icons = {
          type            = "",
          declaration     = "",
          expression      = "",
          specifier       = "",
          statement       = "",
          ["template argument"] = "",
        },
      },
    },
  },
}
