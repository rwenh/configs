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
  -- Optional extras: installed on demand, not part of MasonInstallAll.
  extras = {
    "vale",                   -- prose linter for markdown/rst
    "marksman",               -- markdown LSP
    "taplo",                  -- TOML LSP / formatter
    "buf",                    -- protobuf formatter / linter
    "hadolint",               -- Dockerfile linter
    "actionlint",             -- GitHub Actions linter
    "terraform-ls",           -- Terraform LSP
    "ansible-language-server",
  },
}

-- ── Version pins ──────────────────────────────────────────────────────────────
--

M.versions = {
  ["lua-language-server"]       = nil,    -- latest
  ["basedpyright"]              = nil,    -- latest
  ["prettier"]                  = nil,    -- latest
  ["stylua"]                    = nil,    -- latest
  ["black"]                     = nil,    -- latest
  ["ruff"]                      = nil,    -- latest
  ["eslint_d"]                  = nil,    -- latest
  ["debugpy"]                   = nil,    -- latest
  ["codelldb"]                  = nil,    -- latest
  ["delve"]                     = nil,    -- latest
  ["js-debug-adapter"]          = nil,    -- latest
  -- Example of a pinned package:
  -- ["kotlin-language-server"] = "1.3.4",
}

-- ── Public: M.get ─────────────────────────────────────────────────────────────
--
-- Usage:
--   M.get()              → all core packages (lsp + dap + formatters + linters)
--   M.get("lsp")         → LSP servers only
--   M.get("formatters")  → formatters only
--   M.get("extras")      → optional extras list
--   M.get("all")         → core + extras
--
---@param  category string?
---@return string[]
function M.get(category)
  local core_sections = { "lsp", "dap", "formatters", "linters" }

  if category == "extras" then
    local result = vim.deepcopy(M.mason.extras or {})
    -- Merge user-defined extras from vim.g.
    local user = type(vim.g.mason_extras) == "table" and vim.g.mason_extras or {}
    for _, pkg in ipairs(user) do
      if not vim.tbl_contains(result, pkg) then
        table.insert(result, pkg)
      end
    end
    return result
  end

  if category == "all" then
    local result = M.get()
    for _, pkg in ipairs(M.get("extras")) do
      if not vim.tbl_contains(result, pkg) then
        table.insert(result, pkg)
      end
    end
    return result
  end

  if category then
    return vim.deepcopy(M.mason[category] or {})
  end

  -- Default: all core sections flat.
  local result = {}
  for _, section in ipairs(core_sections) do
    for _, pkg in ipairs(M.mason[section] or {}) do
      if not vim.tbl_contains(result, pkg) then
        table.insert(result, pkg)
      end
    end
  end
  return result
end

-- ── Public: M.version_pin ─────────────────────────────────────────────────────
--
---@param  name string
---@return string|nil
function M.version_pin(name)
  return M.versions[name]
end

return M
