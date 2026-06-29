-- lua/core/util/packages.lua — Mason and LSP package lists (single source of truth)
--

local M = {}

M.lspconfig = {
  "lua_ls", "basedpyright",
  "html", "cssls", "jsonls", "yamlls",
  "clangd", "gopls", "solargraph",
  "kotlin_language_server",
  "zls", "tailwindcss", "elixirls",
  "fortls", "sqls",
}

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
  extras = {
    "vale",
    "marksman",
    "taplo",
    "buf",
    "hadolint",
    "actionlint",
    "terraform-ls",
    "ansible-language-server",
  },
}

M.versions = {
  ["lua-language-server"] = nil,
  ["basedpyright"]        = nil,
  ["prettier"]            = nil,
  ["stylua"]              = nil,
  ["black"]               = nil,
  ["ruff"]                = nil,
  ["eslint_d"]            = nil,
  ["debugpy"]             = nil,
  ["codelldb"]            = nil,
  ["delve"]               = nil,
  ["js-debug-adapter"]    = nil,
}

-- ── M.get ─────────────────────────────────────────────────────────────────────
---@param  category string?
---@return string[]
function M.get(category)
  local core_sections = { "lsp", "dap", "formatters", "linters" }

  if category == "extras" then
    local result = vim.deepcopy(M.mason.extras or {})
    local user = type(vim.g.mason_extras) == "table" and vim.g.mason_extras or {}
    for _, pkg in ipairs(user) do
      if not vim.tbl_contains(result, pkg) then table.insert(result, pkg) end
    end
    return result
  end

  if category == "all" then
    local result = M.get()
    for _, pkg in ipairs(M.get("extras")) do
      if not vim.tbl_contains(result, pkg) then table.insert(result, pkg) end
    end
    return result
  end

  if category then return vim.deepcopy(M.mason[category] or {}) end

  local result = {}
  for _, section in ipairs(core_sections) do
    for _, pkg in ipairs(M.mason[section] or {}) do
      if not vim.tbl_contains(result, pkg) then table.insert(result, pkg) end
    end
  end
  return result
end

-- ── M.version_pin ─────────────────────────────────────────────────────────────
---@param  name string
---@return string|nil
function M.version_pin(name)
  return M.versions[name]
end

-- ── M.validate ────────────────────────────────────────────────────────────────
--
-- Servers managed externally (rustaceanvim, nvim-jdtls) are excluded.
--
-- Call once at startup (e.g. from init.lua after core modules load):
--   require("core.util.packages").validate()
--
local EXTERNALLY_MANAGED = {
  -- rust-analyzer managed by rustaceanvim
  ["rust-analyzer"] = true,
  -- jdtls managed by nvim-jdtls
  ["jdtls"]         = true,
}

function M.validate()
  -- Normalise: strip hyphens, underscores, "language", "server", "ls", "lsp".
  local function normalise(s)
    s = s:lower()
    s = s:gsub("%-", ""):gsub("_", "")
    s = s:gsub("languageserver", ""):gsub("language", "")
    s = s:gsub("server", ""):gsub("lsp$", ""):gsub("ls$", "")
    return s
  end

  local mason_norm = {}
  for _, pkg in ipairs(M.mason.lsp) do
    mason_norm[normalise(pkg)] = pkg
  end

  local issues = {}
  for _, server in ipairs(M.lspconfig) do
    if not EXTERNALLY_MANAGED[server] then
      local norm = normalise(server)
      if not mason_norm[norm] then
        table.insert(issues, string.format(
          "  lspconfig '%s' has no matching Mason package (norm: '%s')", server, norm
        ))
      end
    end
  end

  if #issues > 0 then
    vim.notify(
      "[packages] M.validate(): possible lspconfig↔Mason naming drift:\n"
      .. table.concat(issues, "\n")
      .. "\n\nUpdate M.lspconfig or M.mason.lsp in packages.lua.",
      vim.log.levels.DEBUG
    )
  else
    vim.notify("[packages] M.validate(): all lspconfig entries have Mason counterparts.", vim.log.levels.DEBUG)
  end
end

return M
