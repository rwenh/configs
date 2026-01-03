-- lua/plugins/specs/lang/fortran. lua - Fortran development

return {
  {
    "neovim/nvim-lspconfig",
    ft = "fortran",
    config = function()
      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp"). default_capabilities()
      lspconfig.fortls.setup({
        capabilities = capabilities,
      })
    end,
  },
}