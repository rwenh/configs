-- lua/core/util/packages.lua — Mason and LSP package lists (single source of truth)
--
-- Two naming schemes exist in the Mason ecosystem:
--   M.lspconfig    lspconfig server identifiers, used by mason-lspconfig
--                  ensure_installed (e.g. "lua_ls", "html", "cssls")
--   M.mason        Mason registry package names, used by :MasonInstallAll
--                  (e.g. "lua-language-server", "html-lsp", "css-lsp")
--
-- Both tables must stay in sync when adding a new LSP server.
-- All other consumers (commands.lua, lsp.lua) require this module.
--

local M = {}

-- ── mason-lspconfig server names ─────────────────────────────────────────────
-- Used by: plugins/specs/lsp.lua  mason-lspconfig ensure_installed
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
