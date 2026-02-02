-- lua/plugins/specs/lang/kotlin.lua - Kotlin development
-- Note: LSP is configured in lsp.lua, no need to duplicate here

return {
  -- Kotlin formatting (already handled in conform.nvim in lsp.lua)
  -- LSP (kotlin_language_server) is handled by the main lsp.lua config

  -- Gradle integration
  {
    "mfussenegger/nvim-jdtls",
    optional = true,
    ft = "kotlin",
  },
}
