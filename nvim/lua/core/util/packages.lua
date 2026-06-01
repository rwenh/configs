-- lua/core/util/packages.lua — Mason and LSP package lists (single source of truth)
--

local M = {}

-- ── mason-lspconfig server names ─────────────────────────────────────────────

M.lspconfig = {
  "lua_ls", "basedpyright",
  "html", "cssls", "jsonls", "yamlls",
  "clangd", "gopls", "solargraph",
  "kotlin_language_server",
  "zls", "tailwindcss", "elixirls",
  "fortls", "sqls",
}

-- ── Mason registry package names ─────────────────────────────────────────────
-- Used by: core/commands.lua  :MasonInstallAll
M.mason = {
  lsp = {
    "lua-language-server", "basedpyright",
    "html-lsp", "css-lsp", "json-lsp", "yaml-language-server",
    "clangd", "gopls", "solargraph", "elixir-ls",
    "kotlin-language-server",
    "tailwindcss-language-server",
    "zls", "fortls", "sqls", "jdtls",
  },
  dap = {
    "debugpy", "codelldb", "delve",
    "js-debug-adapter", "java-debug-adapter", "java-test",
    "elixir-ls",
  },
  formatters = {
    "stylua", "prettier", "shfmt",
    "black", "isort",
    "goimports", "gofumpt",
    "ktlint", "rubocop",
    "clang-format", "fprettify",
  },
  linters = {
    "ruff", "eslint_d", "shellcheck", "htmlhint", "stylelint",
  },
}

return M
