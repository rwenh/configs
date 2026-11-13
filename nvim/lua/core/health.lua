-- lua/core/health.lua — :checkhealth core
--
-- Install hints come from core.util.platform (which reads core.util.system_pkgs), so this file names
-- no distribution's packages. A tool with an alternative binary name lists both.

local platform = require("core.util.platform")
local packages = require("core.util.packages")

local M = {}

local ROOT = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h:h:h")
local MASON_BIN = vim.fn.stdpath("data") .. "/mason/bin/"
local MASON_PKG = vim.fn.stdpath("data") .. "/mason/packages/"

-- ── Helpers ───────────────────────────────────────────────────────────────────

--- First of `bins` found on PATH or in Mason's bin directory, or nil.
local function find(bins)
  for _, b in ipairs(bins) do
    if vim.fn.executable(b) == 1 then
      return b
    end
    if vim.fn.executable(MASON_BIN .. b) == 1 then
      return MASON_BIN .. b
    end
  end
  return nil
end

local function mason_installed(pkg_name)
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    return false
  end
  local ok_pkg, pkg = pcall(registry.get_package, pkg_name)
  return ok_pkg and pkg ~= nil and pkg:is_installed()
end

--- One tool row. The hint is `opts.advice`, else the registry entry for `opts.key` (default bins[1]).
---@param label string
---@param bins  string[]
---@param opts  { key: string?, optional: boolean?, advice: string? }?
local function tool(label, bins, opts)
  opts = opts or {}
  if find(bins) then
    vim.health.ok(label)
  elseif opts.optional then
    vim.health.info(label .. " not found (optional)")
  else
    vim.health.warn(label .. " not found", { opts.advice or platform.install_hint(opts.key or bins[1]) })
  end
end

--- A Mason-installed package row.
local function mason_row(label, pkg)
  if mason_installed(pkg) then
    vim.health.ok(label)
  else
    vim.health.warn(label .. " not found", { ":MasonInstall " .. pkg })
  end
end

--- A debug adapter, found as a binary on PATH or in Mason's bin directory.
local function adapter(label, bin, mason_pkg)
  if find({ bin }) then
    vim.health.ok(label)
  else
    vim.health.warn(label .. " not found", { ":MasonInstall " .. mason_pkg })
  end
end

--- An LSP row from core.util.packages: found as a binary, or installed through Mason.
local function lsp_row(row)
  if find({ row.bin }) or (row.mason and mason_installed(row.mason)) then
    vim.health.ok(row.server)
  else
    local advice = row.mason and (":MasonInstall " .. row.mason) or platform.install_hint(row.bin)
    vim.health.warn(row.server .. " not found", { advice })
  end
end

local function ts_tools_check()
  local ok_lazy, lazy_cfg = pcall(require, "lazy.core.config")
  local declared = ok_lazy and lazy_cfg.plugins and lazy_cfg.plugins["typescript-tools.nvim"] ~= nil
  local loaded = package.loaded["typescript-tools"] ~= nil
    or declared
    or #vim.api.nvim_get_runtime_file("lua/typescript-tools/init.lua", false) > 0

  if loaded then
    vim.health.ok("TypeScript   (typescript-tools.nvim)")
  else
    vim.health.warn("TypeScript   (typescript-tools.nvim) not loaded", { ":Lazy install" })
  end

  if vim.fn.executable("tsc") == 1 then
    vim.health.ok("TypeScript   (tsc compiler on PATH)")
  else
    vim.health.info(
      "TypeScript   (tsc compiler) not found on PATH — fine if 'typescript' is a local "
        .. "devDependency; for a global install, see the typescript package in the npm docs"
    )
  end
end

-- ── Checks ────────────────────────────────────────────────────────────────────

function M.check()
  -- ── Core ────────────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide core")

  local ver = vim.version()
  if ver.major > 0 or ver.minor >= 11 then
    vim.health.ok(string.format("Neovim v%d.%d.%d", ver.major, ver.minor, ver.patch))
  else
    vim.health.error(string.format("Neovim v%d.%d.%d — v0.11+ required", ver.major, ver.minor, ver.patch))
  end
  vim.health.info("nvim-ide version: " .. tostring(vim.g.nvim_ide_version or "unknown"))
  vim.health.info(
    string.format(
      "platform: %s%s, package manager: %s",
      platform.os(),
      platform.distro() and (" (" .. platform.distro() .. ")") or "",
      platform.pm() or "none detected"
    )
  )

  if vim.fn.executable("node") == 1 then
    local node_ver = vim.trim(vim.fn.system("node --version 2>/dev/null"))
    local major = tonumber(node_ver:match("v(%d+)"))
    if major and major >= 18 then
      vim.health.ok("node " .. node_ver)
    elseif major then
      vim.health.warn("node " .. node_ver .. " — v18+ recommended for LSP servers")
    end
  else
    tool("node", { "node" }, { key = "node" })
  end

  tool("git", { "git" })
  tool("ripgrep", { "rg" }, { key = "rg" })
  tool("fd", { "fd", "fdfind" }, { key = "fd" })
  tool("lazygit", { "lazygit" })
  tool("python3", { "python3" })
  tool("cargo", { "cargo" })
  tool("gcc/clang", { "gcc", "clang" }, { key = "gcc" })
  tool("tree-sitter CLI", { "tree-sitter" }, { key = "tree-sitter", optional = true })

  local clip = platform.os() == "macos" and { "pbcopy" } or { "wl-copy", "xclip", "xsel" }
  tool("clipboard provider (for unnamedplus)", clip, { key = "xclip" })

  if vim.fn.filereadable(ROOT .. "/lazy-lock.json") == 1 then
    vim.health.ok("lazy-lock.json present")
  else
    vim.health.warn("lazy-lock.json not found", { "Run :Lazy lock and commit the file" })
  end

  do
    local ok_icons, icons = pcall(require, "core.util.icons")
    if ok_icons and type(icons.check_nerd_font) == "function" then
      local font_ok, font = icons.check_nerd_font()
      if font_ok == nil then
        vim.health.info("Nerd Font: cannot verify from terminal Neovim — check visually")
      elseif font_ok then
        vim.health.ok("Nerd Font detected (guifont: " .. tostring(font) .. ")")
      else
        vim.health.warn(
          "guifont does not mention a Nerd Font: " .. tostring(font),
          { "Icons throughout the UI will render as boxes. Install a Nerd Font: https://www.nerdfonts.com/" }
        )
      end
    end
  end

  -- ── Config consistency ──────────────────────────────────────────────────────
  vim.health.start("nvim-ide config consistency")

  do
    local ok_v1, result1 = pcall(packages.validate)
    if not ok_v1 then
      vim.health.warn("packages.validate() raised an error", { tostring(result1) })
    elseif result1.ok then
      vim.health.ok("lspconfig ↔ Mason naming: no drift detected")
    else
      vim.health.warn("lspconfig ↔ Mason naming drift detected", result1.issues)
    end

    local ok_v2, result2 = pcall(packages.validate_dap)
    if not ok_v2 then
      vim.health.warn("packages.validate_dap() raised an error", { tostring(result2) })
    elseif result2.ok then
      vim.health.ok("Mason DAP package names: all found in registry")
    else
      vim.health.warn("Mason DAP package name(s) not found in registry", result2.issues)
    end
  end

  -- ── LSP servers ─────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide LSP servers")

  for _, row in ipairs(packages.servers) do
    if not row.managed_by then
      lsp_row(row)
    end
  end
  ts_tools_check()
  vim.health.info("cobol: no language server; cobc compile and check work without one")
  tool("vhdl_ls", { "vhdl_ls" }, { key = "vhdl_ls" })

  -- ── Formatters ──────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide formatters")

  for _, f in ipairs({
    { "stylua (Lua)", "stylua" },
    { "black (Python)", "black" },
    { "isort (Python)", "isort" },
    { "ruff (Python)", "ruff" },
    { "goimports (Go)", "goimports" },
    { "gofumpt (Go)", "gofumpt" },
    { "prettier (Web/JSON/YAML/MD)", "prettier" },
    { "eslint_d (JS/TS)", "eslint_d" },
    { "rubocop (Ruby)", "rubocop" },
    { "ktlint (Kotlin)", "ktlint" },
    { "clang-format (C/C++)", "clang-format" },
    { "fprettify (Fortran)", "fprettify" },
    { "shfmt (Shell)", "shfmt" },
    { "vsg (VHDL)", "vsg" },
    { "sqlfmt (SQL)", "sqlfmt", true },
  }) do
    tool(f[1], { f[2] }, { key = f[2], optional = f[3] })
  end

  -- ── DAP adapters ────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide DAP adapters")

  adapter("Python (debugpy-adapter)", "debugpy-adapter", "debugpy")
  adapter("C/C++/Rust/Zig (codelldb)", "codelldb", "codelldb")
  adapter("Go (delve)", "dlv", "delve")

  do
    local ok_mason, mason = pcall(require, "core.util.mason")
    if ok_mason and mason.js_debug_script() ~= nil then
      vim.health.ok("JS/TS (js-debug-adapter)")
    else
      vim.health.warn("JS/TS (js-debug-adapter) not found", { ":MasonInstall js-debug-adapter" })
    end
  end

  mason_row("Java (java-debug-adapter)", "java-debug-adapter")
  mason_row("Java (java-test)", "java-test")
  tool("Ruby (rdbg)", { "rdbg" }, { key = "rdbg" })

  if vim.fn.filereadable(MASON_PKG .. "elixir-ls/debugger.sh") == 1 or find({ "elixir-ls-debugger" }) then
    vim.health.ok("Elixir (elixir-ls debugger)")
  else
    vim.health.warn("Elixir (elixir-ls debugger) not found", { ":MasonInstall elixir-ls" })
  end

  -- ── Language tooling ────────────────────────────────────────────────────────
  vim.health.start("nvim-ide language tooling")

  for _, r in ipairs({
    { "go runtime", "go" },
    { "java runtime (17+)", "java" },
    { "ruby runtime", "ruby" },
    { "elixir runtime", "elixir" },
    { "mix (Elixir)", "mix" },
    { "gfortran (Fortran)", "gfortran" },
    { "cobc (GnuCOBOL)", "cobc" },
    { "ghdl (VHDL)", "ghdl" },
    { "zig compiler", "zig" },
    { "curl (REST client)", "curl" },
    { "shellcheck", "shellcheck" },
    { "gtkwave (VHDL waves)", "gtkwave", true },
    { "deno (Markdown preview)", "deno", true },
    { "jq (REST formatting)", "jq", true },
    { "tmux (pane navigation)", "tmux", true },
    { "htmlhint (HTML lint)", "htmlhint", true },
    { "stylelint (CSS lint)", "stylelint", true },
  }) do
    tool(r[1], { r[2] }, { key = r[2], optional = r[3] })
  end

  -- ── Plugin infrastructure ───────────────────────────────────────────────────
  vim.health.start("nvim-ide plugin infrastructure")

  local ok_lazy, lazy = pcall(require, "lazy")
  if ok_lazy then
    local s = lazy.stats()
    vim.health.ok(string.format("lazy.nvim: %d plugins, %.2fms startup", s.count, s.startuptime))
  else
    vim.health.error("lazy.nvim not loaded")
  end

  for _, mod in ipairs({
    { "mason.nvim", "mason" },
    { "nvim-treesitter", "nvim-treesitter" },
    { "blink.cmp", "blink.cmp" },
    { "conform.nvim", "conform" },
    { "nvim-lint", "lint" },
    { "nvim-dap", "dap" },
    { "neotest", "neotest" },
  }) do
    if pcall(require, mod[2]) then
      vim.health.ok(mod[1] .. " loaded")
    else
      vim.health.error(mod[1] .. " not loaded")
    end
  end

  local fzf_dir = vim.fn.stdpath("data") .. "/lazy/telescope-fzf-native.nvim"
  local fzf_built = false
  for _, name in ipairs({ "build/libfzf.so", "build/libfzf.dylib", "build/libfzf.dll", "libfzf.so", "libfzf.dylib" }) do
    if vim.fn.filereadable(fzf_dir .. "/" .. name) == 1 then
      fzf_built = true
      break
    end
  end
  if fzf_built then
    vim.health.ok("telescope-fzf-native compiled")
  else
    vim.health.warn("telescope-fzf-native not compiled", { "cd " .. fzf_dir .. " && make" })
  end
end

return M
