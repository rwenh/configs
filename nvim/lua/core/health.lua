-- lua/core/health.lua — :checkhealth core
--

local M = {}

local function bin_ok(bin)
  return vim.fn.executable(bin) == 1
end

local function mason_installed(pkg_name)
  local ok, registry = pcall(require, "mason-registry")
  if not ok then return false end
  local ok_pkg, pkg = pcall(registry.get_package, pkg_name)
  return ok_pkg and pkg and pkg:is_installed()
end

local function lsp_check(label, binary, mason_pkg, advice)
  local installed = (binary and bin_ok(binary)) or (mason_pkg and mason_installed(mason_pkg))
  if installed then
    vim.health.ok(label)
  else
    vim.health.warn(label .. " not found", { advice or (mason_pkg and (":MasonInstall " .. mason_pkg) or ("Install: " .. (binary or ""))) })
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
    vim.health.error(
      string.format("Neovim v%d.%d.%d — v0.11+ required", ver.major, ver.minor, ver.patch)
    )
  end

  vim.health.info("nvim-ide version: " .. tostring(vim.g.nvim_ide_version or "unknown"))

  if bin_ok("node") then
    local node_ver = vim.trim(vim.fn.system("node --version 2>/dev/null"))
    local major    = tonumber(node_ver:match("v(%d+)"))
    if major and major >= 18 then
      vim.health.ok("node " .. node_ver)
    elseif major then
      vim.health.warn("node " .. node_ver .. " — v18+ recommended for LSP servers")
    end
  else
    vim.health.warn("node not found", { "Install: sudo zypper in nodejs" })
  end

  for _, entry in ipairs({
    { "git",       "git",       nil },
    { "ripgrep",   "rg",        "sudo zypper in ripgrep" },
    { "fd",        "fd",        "sudo zypper in fd"      },
    { "lazygit",   "lazygit",   "sudo zypper in lazygit" },
    { "python3",   "python3",   "sudo zypper in python3" },
    { "cargo",     "cargo",     "sudo zypper in cargo"   },
    { "gcc/clang", "gcc",       "sudo zypper in gcc"     },
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

  -- ── LSP servers ───────────────────────────────────────────────────────────
  vim.health.start("nvim-ide LSP servers")

  lsp_check("Lua          (lua_ls)",                  "lua-language-server",         "lua-language-server")
  lsp_check("Python       (basedpyright)",             "basedpyright",                "basedpyright")
  lsp_check("Rust         (rust-analyzer)",            "rust-analyzer",               nil, "rustup component add rust-analyzer")
  lsp_check("Go           (gopls)",                    "gopls",                       "gopls")
  lsp_check("C/C++        (clangd)",                   "clangd",                      "clangd")
  lsp_check("Ruby         (solargraph)",               "solargraph",                  "solargraph", "gem install solargraph")
  lsp_check("Elixir       (elixir-ls)",                nil,                           "elixir-ls")
  lsp_check("Java         (jdtls)",                    nil,                           "jdtls")
  lsp_check("Kotlin       (kotlin-language-server)",   "kotlin-language-server",      "kotlin-language-server")
  lsp_check("TypeScript   (typescript-tools)",         "typescript-language-server",  "typescript-language-server")
  lsp_check("HTML         (html-lsp)",                 "vscode-html-language-server", "html-lsp")
  lsp_check("CSS          (cssls)",                    "vscode-css-language-server",  "css-lsp")
  lsp_check("JSON         (jsonls)",                   "vscode-json-language-server", "json-lsp")
  lsp_check("YAML         (yamlls)",                   "yaml-language-server",        "yaml-language-server")
  lsp_check("Tailwind     (tailwindcss-ls)",           "tailwindcss-language-server", "tailwindcss-language-server")
  lsp_check("Zig          (zls)",                      "zls",                         "zls")
  lsp_check("Fortran      (fortls)",                   "fortls",                      "fortls")
  lsp_check("SQL          (sqls)",                     "sqls",                        "sqls")
  lsp_check("COBOL        (cobol-language-server)",    "cobol-language-server",       nil, "npm i -g @broadcommfd/cobol-language-support")
  lsp_check("VHDL         (vhdl_ls)",                  "vhdl_ls",                     nil, "cargo install vhdl_ls")

  -- ── Formatters ────────────────────────────────────────────────────────────
  vim.health.start("nvim-ide formatters")

  fmt_check("stylua     (Lua)",                "stylua")
  fmt_check("black      (Python)",             "black")
  fmt_check("isort      (Python)",             "isort")
  fmt_check("ruff       (Python)",             "ruff")
  fmt_check("goimports  (Go)",                 "goimports")
  fmt_check("gofumpt    (Go)",                 "gofumpt")
  fmt_check("prettier   (Web/JSON/YAML/MD)",   "prettier")
  fmt_check("eslint_d   (JS/TS)",              "eslint_d")
  fmt_check("rubocop    (Ruby)",               "rubocop")
  fmt_check("ktlint     (Kotlin)",             "ktlint")
  fmt_check("clang-format (C/C++)",            "clang-format")
  fmt_check("fprettify  (Fortran)",            "fprettify")
  fmt_check("shfmt      (Shell)",              "shfmt")
  fmt_check("vsg        (VHDL)",               "vsg")
  fmt_check("sqlfmt     (SQL)",                "sqlfmt",     true)

  -- ── DAP adapters ──────────────────────────────────────────────────────────
  vim.health.start("nvim-ide DAP adapters")

  local mason_bin = vim.fn.stdpath("data") .. "/mason/bin/"
  local mason_pkg = vim.fn.stdpath("data") .. "/mason/packages/"

  local function dap_check(label, binary, mason_bin_name, fallback_path, advice)
    local found = (binary and bin_ok(binary))
      or (mason_bin_name and vim.fn.executable(mason_bin .. mason_bin_name) == 1)
      or (fallback_path   and vim.fn.filereadable(fallback_path) == 1)
    if found then
      vim.health.ok(label)
    else
      vim.health.warn(label .. " not found", { advice or ":MasonInstall " .. (mason_bin_name or binary or "") })
    end
  end

  dap_check("Python       (debugpy)",
    "debugpy", "debugpy", nil, "pip install debugpy")
  dap_check("C/C++/Rust/Zig (codelldb)",
    nil, "codelldb", nil)
  dap_check("Go           (delve/dlv)",
    "dlv", "dlv", nil)

  do
    local js_debug_root = mason_pkg .. "js-debug-adapter"
    local js_debug_candidates = {
      js_debug_root .. "/js-debug/src/dapDebugServer.js",
      js_debug_root .. "/extension/src/dapDebugServer.js",
      js_debug_root .. "/out/src/dapDebugServer.js",
      js_debug_root .. "/dist/src/dapDebugServer.js",
    }
    local found = false
    for _, candidate in ipairs(js_debug_candidates) do
      if vim.fn.filereadable(candidate) == 1 then found = true; break end
    end
    if found then
      vim.health.ok("JS/TS        (js-debug-adapter)")
    else
      vim.health.warn("JS/TS        (js-debug-adapter) not found", { ":MasonInstall js-debug-adapter" })
    end
  end

  dap_check("Java         (java-debug-adapter)",
    nil, nil, nil, ":MasonInstall java-debug-adapter")
  dap_check("Ruby         (rdbg)",
    "rdbg", nil, nil, "gem install debug OR bundle add debug")
  dap_check("Elixir       (elixir-ls debugger)",
    "elixir-ls-debugger", nil, mason_pkg .. "elixir-ls/debugger.sh")

  if mason_installed("java-test") then
    vim.health.ok("Java         (java-test)")
  else
    vim.health.warn("Java         (java-test) not found", { ":MasonInstall java-test" })
  end

  -- ── Language tooling ──────────────────────────────────────────────────────
  vim.health.start("nvim-ide language tooling")

  lang_check("go runtime",            "go",         false, "sudo zypper in go")
  lang_check("java runtime (17+)",    "java",       false, "sudo zypper in java-17-openjdk")
  lang_check("ruby runtime",          "ruby",       false, "sudo zypper in ruby")
  lang_check("elixir runtime",        "elixir",     false, "sudo zypper in elixir")
  lang_check("mix (Elixir)",          "mix",        false, "sudo zypper in elixir")
  lang_check("gfortran (Fortran)",    "gfortran",   false, "sudo zypper in gcc-fortran")
  lang_check("cobc (GnuCOBOL)",       "cobc",       false, "sudo zypper in gnucobol")
  lang_check("ghdl (VHDL)",           "ghdl",       false, "sudo zypper in ghdl")
  lang_check("zig compiler",          "zig",        false, "https://ziglang.org/download/")
  lang_check("curl (REST client)",    "curl",       false, "sudo zypper in curl")
  lang_check("shellcheck",            "shellcheck", false, "sudo zypper in ShellCheck")
  lang_check("gtkwave (VHDL waves)",  "gtkwave",    true,  "sudo zypper in gtkwave")
  lang_check("deno (MD preview)",     "deno",       true,  "cargo install deno OR sudo zypper in deno")
  lang_check("jq (REST formatting)",  "jq",         true,  "sudo zypper in jq")
  lang_check("tmux (pane nav)",       "tmux",       true,  "sudo zypper in tmux")
  lang_check("htmlhint (HTML lint)",  "htmlhint",   true,  "npm i -g htmlhint")
  lang_check("stylelint (CSS lint)",  "stylelint",  true,  "npm i -g stylelint")

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
    { "mason.nvim",        "mason"           },
    { "nvim-treesitter",   "nvim-treesitter" },
    { "blink.cmp",         "blink.cmp"       },
    { "conform.nvim",      "conform"         },
    { "nvim-lint",         "lint"            },
    { "nvim-dap",          "dap"             },
    { "neotest",           "neotest"         },
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
    if vim.fn.filereadable(candidate) == 1 then fzf_built = true; break end
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
