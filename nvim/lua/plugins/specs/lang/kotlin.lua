-- lua/plugins/specs/lang/kotlin.lua - Kotlin development

return {
  -- Kotlin LSP
  {
    "neovim/nvim-lspconfig",
    ft = "kotlin",
    config = function()
      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      
      lspconfig.kotlin_language_server.setup({
        capabilities = capabilities,
        root_dir = lspconfig.util.root_pattern("settings.gradle", "settings.gradle.kts", "build.gradle", "build.gradle.kts"),
      })
    end,
  },

  -- Kotlin formatting
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        kotlin = { "ktlint" },
      },
    },
  },

  -- Gradle integration
  {
    "mfussenegger/nvim-jdtls",
    optional = true,
    ft = "kotlin",
  },
}