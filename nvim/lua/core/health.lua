-- lua/core/health.lua — :checkhealth core
--

local M = {}

local function bin_ok(bin)
  return vim.fn.executable(bin) == 1
end

local function mason_installed(pkg_name)
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    return false
  end
  local ok_pkg, pkg = pcall(registry.get_package, pkg_name)
  return ok_pkg and pkg and pkg:is_installed()
end

local function lsp_check(label, binary, mason_pkg, advice)
  local installed = (binary and bin_ok(binary)) or (mason_pkg and mason_installed(mason_pkg))
  if installed then
    vim.health.ok(label)
  else
    vim.health.warn(
      label .. " not found",
      { advice or (mason_pkg and (":MasonInstall " .. mason_pkg) or ("Install: " .. (binary or ""))) }
    )
  end
end

-- typescript-tools.nvim talks to the TypeScript compiler's language service
-- directly — it does not use (and this config does not install) a standalone
-- typescript-language-server binary. Checking for that binary here produced a
-- permanent false "not found" warning for the default setup. Check the two
-- things that actually matter instead: the plugin is loaded, and a
-- TypeScript compiler is discoverable.
local function ts_tools_check()
  local ok_lazy, lazy_cfg = pcall(require, "lazy.core.config")
  local declared = ok_lazy and lazy_cfg.plugins and lazy_cfg.plugins["typescript-tools.nvim"] ~= nil
  local loaded = package.loaded["typescript-tools"] ~= nil
    or declared -- lazy (ft=...) plugins are not on the rtp until first use
    or #vim.api.nvim_get_runtime_file("lua/typescript-tools/init.lua", false) > 0

  if loaded then
    vim.health.ok("TypeScript   (typescript-tools.nvim)")
  else
    vim.health.warn("TypeScript   (typescript-tools.nvim) not loaded", { ":Lazy install" })
  end

  if bin_ok("tsc") then
    vim.health.ok("TypeScript   (tsc compiler on PATH)")
  else
    vim.health.info(
      "TypeScript   (tsc compiler) not found on PATH — fine if 'typescript' is a local "
        .. "devDependency instead of a global install; install globally with: npm install -g typescript"
    )
  end
end

local function fmt_check(label, binary, optional)
  if bin_ok(binary) then
    vim.health.ok(label)
  elseif optional then
    vim.health.info(label .. " not found (optional)")
  else
    vim.health.warn(label .. " not found", { ":MasonInstall " .. binary })
  end
end

local function lang_check(label, binary, optional, advice)
  if bin_ok(binary) then
    vim.health.ok(label)
  elseif optional then
    vim.health.info(label .. " not found (optional)")
  else
    vim.health.warn(label .. " not found", { advice or ("Install: " .. binary) })
  end
end

function M.check()
  -- ── Core ──────────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide core")

  local ver = vim.version()
  if ver.major > 0 or ver.minor >= 11 then
    vim.health.ok(string.format("Neovim v%d.%d.%d", ver.major, ver.minor, ver.patch))
  else
    vim.health.error(string.format("Neovim v%d.%d.%d — v0.11+ required", ver.major, ver.minor, ver.patch))
  end

  vim.health.info("nvim-ide version: " .. tostring(vim.g.nvim_ide_version or "unknown"))

  if bin_ok("node") then
    local node_ver = vim.trim(vim.fn.system("node --version 2>/dev/null"))
    local major = tonumber(node_ver:match("v(%d+)"))
    if major and major >= 18 then
      vim.health.ok("node " .. node_ver)
    elseif major then
      vim.health.warn("node " .. node_ver .. " — v18+ recommended for LSP servers")
    end
  else
    vim.health.warn("node not found", { "Install: nodejs" })
  end

  for _, entry in ipairs({
    { "git", "git", nil },
    { "ripgrep", "rg", "Install: ripgrep" },
    { "fd", "fd", "Install: fd" },
    { "lazygit", "lazygit", "Install: lazygit" },
    { "python3", "python3", "Install: python3" },
    { "cargo", "cargo", "Install: cargo" },
    { "gcc/clang", "gcc", "Install: gcc" },
  }) do
    local label, binary, advice = entry[1], entry[2], entry[3]
    if bin_ok(binary) then
      vim.health.ok(label)
    else
      vim.health.warn(label .. " not found", { advice or ("Install: " .. binary) })
    end
  end

  local lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json"
  if vim.fn.filereadable(lockfile) == 1 then
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
          { "Icons throughout the UI will render as boxes/tofu. Install a Nerd Font: https://www.nerdfonts.com/" }
        )
      end
    end
  end

  -- ── Config internal consistency ───────────────────────────────────────────
  --
  vim.health.start("nvim-ide config consistency")

  do
    local ok_pkgs, packages = pcall(require, "core.util.packages")
    if not ok_pkgs then
      vim.health.error("core.util.packages not loadable")
    else
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
  end

  -- ── LSP servers ───────────────────────────────────────────────────────────
  vim.health.start("nvim-ide LSP servers")

  -- One row per server in core.util.packages, so this list cannot drift from the registry.
  local ok_pk, pk = pcall(require, "core.util.packages")
  if ok_pk then
    for _, row in ipairs(pk.servers) do
      if not row.managed_by then
        lsp_check(row.server, row.bin, row.mason)
      end
    end
  else
    vim.health.error("core.util.packages not loadable")
  end
  ts_tools_check()
  vim.health.info("cobol: no language server is provided; cobc compile and check work without one")
  lsp_check("vhdl_ls", "vhdl_ls", nil, "cargo install vhdl_ls")

  -- ── Formatters ────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide formatters")

  fmt_check("stylua     (Lua)", "stylua")
  fmt_check("black      (Python)", "black")
  fmt_check("isort      (Python)", "isort")
  fmt_check("ruff       (Python)", "ruff")
  fmt_check("goimports  (Go)", "goimports")
  fmt_check("gofumpt    (Go)", "gofumpt")
  fmt_check("prettier   (Web/JSON/YAML/MD)", "prettier")
  fmt_check("eslint_d   (JS/TS)", "eslint_d")
  fmt_check("rubocop    (Ruby)", "rubocop")
  fmt_check("ktlint     (Kotlin)", "ktlint")
  fmt_check("clang-format (C/C++)", "clang-format")
  fmt_check("fprettify  (Fortran)", "fprettify")
  fmt_check("shfmt      (Shell)", "shfmt")
  fmt_check("vsg        (VHDL)", "vsg")
  fmt_check("sqlfmt     (SQL)", "sqlfmt", true)

  -- ── DAP adapters ──────────────────────────────────────────────────────────
  vim.health.start("nvim-ide DAP adapters")

  local mason_bin = vim.fn.stdpath("data") .. "/mason/bin/"
  local mason_pkg = vim.fn.stdpath("data") .. "/mason/packages/"

  local function dap_check(label, binary, mason_bin_name, fallback_path, advice)
    local found = (binary and bin_ok(binary))
      or (mason_bin_name and vim.fn.executable(mason_bin .. mason_bin_name) == 1)
      or (fallback_path and vim.fn.filereadable(fallback_path) == 1)
    if found then
      vim.health.ok(label)
    else
      vim.health.warn(label .. " not found", { advice or ":MasonInstall " .. (mason_bin_name or binary or "") })
    end
  end

  dap_check("Python       (debugpy)", "debugpy", "debugpy", nil, "pip install debugpy")
  dap_check("C/C++/Rust/Zig (codelldb)", nil, "codelldb", nil)
  dap_check("Go           (delve/dlv)", "dlv", "dlv", nil)

  do
    local ok_mason, mason = pcall(require, "core.util.mason")
    local found = ok_mason and mason.js_debug_script() ~= nil
    if found then
      vim.health.ok("JS/TS        (js-debug-adapter)")
    else
      vim.health.warn("JS/TS        (js-debug-adapter) not found", { ":MasonInstall js-debug-adapter" })
    end
  end

  dap_check("Java         (java-debug-adapter)", nil, nil, nil, ":MasonInstall java-debug-adapter")
  dap_check("Ruby         (rdbg)", "rdbg", nil, nil, "gem install debug OR bundle add debug")
  dap_check("Elixir       (elixir-ls debugger)", "elixir-ls-debugger", nil, mason_pkg .. "elixir-ls/debugger.sh")

  if mason_installed("java-test") then
    vim.health.ok("Java         (java-test)")
  else
    vim.health.warn("Java         (java-test) not found", { ":MasonInstall java-test" })
  end

  -- ── Language tooling ──────────────────────────────────────────────────────
  vim.health.start("nvim-ide language tooling")

  lang_check("go runtime", "go", false, "Install: go")
  lang_check("java runtime (17+)", "java", false, "Install: openjdk-17")
  lang_check("ruby runtime", "ruby", false, "Install: ruby")
  lang_check("elixir runtime", "elixir", false, "Install: elixir")
  lang_check("mix (Elixir)", "mix", false, "Install: elixir")
  lang_check("gfortran (Fortran)", "gfortran", false, "Install: gcc-fortran")
  lang_check("cobc (GnuCOBOL)", "cobc", false, "Install: gnucobol")
  lang_check("ghdl (VHDL)", "ghdl", false, "Install: ghdl")
  lang_check("zig compiler", "zig", false, "https://ziglang.org/download/")
  lang_check("curl (REST client)", "curl", false, "Install: curl")
  lang_check("shellcheck", "shellcheck", false, "Install: shellcheck")
  lang_check("gtkwave (VHDL waves)", "gtkwave", true, "Install: gtkwave")
  lang_check("deno (MD preview)", "deno", true, "cargo install deno OR install deno")
  lang_check("jq (REST formatting)", "jq", true, "Install: jq")
  lang_check("tmux (pane nav)", "tmux", true, "Install: tmux")
  lang_check("htmlhint (HTML lint)", "htmlhint", true, "npm i -g htmlhint")
  lang_check("stylelint (CSS lint)", "stylelint", true, "npm i -g stylelint")

  -- ── Plugin infrastructure ─────────────────────────────────────────────────
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
    local label, mod_name = mod[1], mod[2]
    if pcall(require, mod_name) then
      vim.health.ok(label .. " loaded")
    else
      vim.health.error(label .. " not loaded")
    end
  end

  local plugin_dir = vim.fn.stdpath("data") .. "/lazy/telescope-fzf-native.nvim"
  local fzf_candidates = {
    plugin_dir .. "/build/libfzf.so",
    plugin_dir .. "/build/libfzf.dylib",
    plugin_dir .. "/build/libfzf.dll",
    plugin_dir .. "/libfzf.so",
    plugin_dir .. "/libfzf.dylib",
  }
  local fzf_built = false
  for _, candidate in ipairs(fzf_candidates) do
    if vim.fn.filereadable(candidate) == 1 then
      fzf_built = true
      break
    end
  end
  if fzf_built then
    vim.health.ok("telescope-fzf-native compiled")
  else
    vim.health.warn("telescope-fzf-native not compiled", {
      "cd " .. plugin_dir .. " && make",
    })
  end
end

return M
