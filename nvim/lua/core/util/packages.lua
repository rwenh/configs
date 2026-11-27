-- lua/core/util/packages.lua — the tool registry: language servers, Mason packages and their binaries
--
-- One row per language server (M.servers). Everything else is derived from it:
--   M.lspconfig   the names mason-lspconfig and nvim-lspconfig use (rows not managed elsewhere)
--   M.mason.lsp   the Mason package for each row
--   M.bin(server) the executable that must be on PATH before the server is started
-- The old design kept three lists in step and matched them with a name normaliser, which is
-- how drift went unnoticed. A row cannot disagree with itself, so validate() checks structure.
--
-- Servers started by another plugin carry `managed_by`: jdtls is started by nvim-jdtls, so it
-- has no lspconfig entry here and no binary check.

local M = {}

M.servers = {
  { server = "lua_ls", mason = "lua-language-server", bin = "lua-language-server" },
  { server = "basedpyright", mason = "basedpyright", bin = "basedpyright-langserver" },
  { server = "html", mason = "html-lsp", bin = "vscode-html-language-server" },
  { server = "cssls", mason = "css-lsp", bin = "vscode-css-language-server" },
  { server = "jsonls", mason = "json-lsp", bin = "vscode-json-language-server" },
  { server = "yamlls", mason = "yaml-language-server", bin = "yaml-language-server" },
  { server = "clangd", mason = "clangd", bin = "clangd" },
  { server = "gopls", mason = "gopls", bin = "gopls" },
  { server = "solargraph", mason = "solargraph", bin = "solargraph" },
  { server = "elixirls", mason = "elixir-ls", bin = "elixir-ls" },
  { server = "kotlin_language_server", mason = "kotlin-language-server", bin = "kotlin-language-server" },
  { server = "tailwindcss", mason = "tailwindcss-language-server", bin = "tailwindcss-language-server" },
  { server = "zls", mason = "zls", bin = "zls" },
  { server = "fortls", mason = "fortls", bin = "fortls" },
  { server = "sqls", mason = "sqls", bin = "sqls" },
  { server = "jdtls", mason = "jdtls", managed_by = "nvim-jdtls" },
}

local function derive()
  local lspconfig, mason_lsp = {}, {}
  for _, row in ipairs(M.servers) do
    if not row.managed_by then
      table.insert(lspconfig, row.server)
    end
    table.insert(mason_lsp, row.mason)
  end
  return lspconfig, mason_lsp
end

local _lspconfig, _mason_lsp = derive()

M.lspconfig = _lspconfig

M.mason = {
  lsp = _mason_lsp,
  dap = {
    "debugpy",
    "codelldb",
    "delve",
    "js-debug-adapter",
    "java-debug-adapter",
    "java-test",
    "kotlin-debug-adapter",
    "elixir-ls",
  },
  formatters = {
    "stylua",
    "prettier",
    "shfmt",
    "black",
    "isort",
    "goimports",
    "gofumpt",
    "ktlint",
    "rubocop",
    "clang-format",
    "fprettify",
  },
  linters = {
    "ruff",
    "eslint_d",
    "shellcheck",
    "htmlhint",
    "stylelint",
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

--- Executable that must be on PATH for `server` (an lspconfig name), or nil when unknown or managed elsewhere.
---@param server string
---@return string|nil
function M.bin(server)
  for _, row in ipairs(M.servers) do
    if row.server == server then
      return row.bin
    end
  end
  return nil
end

-- ── M.get ─────────────────────────────────────────────────────────────────────
---@param  category string?
---@return string[]
function M.get(category)
  local core_sections = { "lsp", "dap", "formatters", "linters" }

  if category == "extras" then
    local result = vim.deepcopy(M.mason.extras or {})
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

-- ── M.validate ────────────────────────────────────────────────────────────────

---@return { ok: boolean, issues: string[] }
function M.validate()
  local issues = {}
  local seen_server, seen_mason = {}, {}
  for i, row in ipairs(M.servers) do
    if type(row.server) ~= "string" or row.server == "" then
      issues[#issues + 1] = ("  servers[%d]: no lspconfig name"):format(i)
    elseif seen_server[row.server] then
      issues[#issues + 1] = ("  duplicate lspconfig name '%s'"):format(row.server)
    end
    seen_server[row.server or ""] = true

    if type(row.mason) ~= "string" or row.mason == "" then
      issues[#issues + 1] = ("  %s: no Mason package"):format(tostring(row.server))
    elseif seen_mason[row.mason] then
      issues[#issues + 1] = ("  duplicate Mason package '%s'"):format(row.mason)
    end
    seen_mason[row.mason or ""] = true

    if not row.managed_by and (type(row.bin) ~= "string" or row.bin == "" or row.bin:find("%s")) then
      issues[#issues + 1] = ("  %s: no usable binary name (set bin, or managed_by)"):format(tostring(row.server))
    end
  end

  if #issues > 0 then
    vim.notify("[packages] M.validate(): tool registry problems:\n" .. table.concat(issues, "\n"), vim.log.levels.DEBUG)
  end
  return { ok = (#issues == 0), issues = issues }
end

-- ── M.validate_dap ────────────────────────────────────────────────────────────
-- Call once at startup alongside M.validate().
---@return { ok: boolean, issues: string[] }
function M.validate_dap()
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    vim.notify("[packages] validate_dap(): mason-registry not available", vim.log.levels.DEBUG)
    return { ok = false, issues = { "  mason-registry not available" } }
  end

  local issues = {}
  for _, pkg_name in ipairs(M.mason.dap) do
    local ok_pkg, _ = pcall(function()
      return registry.get_package(pkg_name)
    end)
    if not ok_pkg then
      table.insert(issues, string.format("  mason.dap entry '%s' not found in registry", pkg_name))
    end
  end

  if #issues > 0 then
    vim.notify(
      "[packages] validate_dap(): unknown Mason DAP package names:\n"
        .. table.concat(issues, "\n")
        .. "\n\nCheck M.mason.dap in packages.lua for typos.",
      vim.log.levels.DEBUG
    )
  end
  return { ok = (#issues == 0), issues = issues }
end

-- ── M.validate_lsp ────────────────────────────────────────────────────────────
-- Checks each server row against what is really present: an lspconfig entry in the runtime, and a Mason
-- package in the registry. Used by :checkhealth core. If Mason's registry has never been downloaded, every
-- package will be listed; run :MasonUpdate and check again.
---@return { ok: boolean, issues: string[] }
function M.validate_lsp()
  local issues = {}
  local ok_reg, registry = pcall(require, "mason-registry")
  -- nvim-lspconfig is lazy-loaded, so it is usually off the runtime path when health runs. Its directory
  -- comes from lazy's own plugin table, which is available once lazy is set up.
  local lspconfig_dir
  local ok_cfg, lazy_cfg = pcall(require, "lazy.core.config")
  if ok_cfg and lazy_cfg.plugins and lazy_cfg.plugins["nvim-lspconfig"] then
    lspconfig_dir = lazy_cfg.plugins["nvim-lspconfig"].dir
  end
  local function has_config(server)
    if lspconfig_dir and vim.uv.fs_stat(lspconfig_dir .. "/lsp/" .. server .. ".lua") then
      return true
    end
    return #vim.api.nvim_get_runtime_file("lsp/" .. server .. ".lua", false) > 0
  end
  for _, row in ipairs(M.servers) do
    if not row.managed_by then
      if not has_config(row.server) then
        issues[#issues + 1] = ("  %s: no lsp/%s.lua in nvim-lspconfig"):format(row.server, row.server)
      end
      if ok_reg and not pcall(registry.get_package, row.mason) then
        issues[#issues + 1] = ("  %s: Mason package '%s' is not in the registry"):format(row.server, row.mason)
      end
    end
  end
  if not ok_reg then
    issues[#issues + 1] = "  mason-registry is not loadable, so Mason package names were not checked"
  end
  return { ok = (#issues == 0), issues = issues }
end

return M
