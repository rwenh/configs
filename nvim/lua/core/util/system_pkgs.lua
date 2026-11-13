-- lua/core/util/system_pkgs.lua — tool name to package name, per package manager
--
-- Keys are binary names, so a caller passes the name it already checks. Columns: apt, dnf, zypper,
-- pacman, apk, brew (package name). `fallback` is a language package manager, a URL or a manual
-- step, used when no column matches this system.
--
-- These names come from common repositories. They were NOT checked against live package indexes:
-- confirm each one on your system before you rely on its hint.

local M = {}

--- The same package name on every native manager.
local function std(name)
  return { apt = name, dnf = name, zypper = name, pacman = name, apk = name, brew = name }
end

local function with(base, overrides)
  return vim.tbl_extend("force", base, overrides)
end

local function pip(name)
  return { fallback = "python3 -m pip install " .. name }
end
local function npm(name)
  return { fallback = "npm install -g " .. name }
end
local function gem(name)
  return { fallback = "gem install " .. name }
end
local function cargo(name)
  return { fallback = "cargo install " .. name }
end

-- ── Languages and runtimes ────────────────────────────────────────────────────

local java = {
  apt = "openjdk-17-jdk",
  dnf = "java-17-openjdk-devel",
  zypper = "java-17-openjdk-devel",
  pacman = "jdk17-openjdk",
  apk = "openjdk17-jdk",
  brew = "openjdk@17",
  fallback = "install a JDK 17 or newer",
}

local elixir = std("elixir")
local ruby = std("ruby")
local cargo_row = { pacman = "rust", brew = "rust", fallback = "https://rustup.rs" }

M.java = java
M.javac = java
M.go = { apt = "golang-go", dnf = "golang", zypper = "go", pacman = "go", apk = "go", brew = "go" }
M.ruby = ruby
M.gem = ruby
M.rspec = gem("rspec")
M.rdbg = gem("debug")
M.rubocop = gem("rubocop")
M.bundle = gem("bundler")
M.elixir = elixir
M.mix = elixir
M.node = { apt = "nodejs", dnf = "nodejs", zypper = "nodejs", pacman = "nodejs", apk = "nodejs", brew = "node" }
M.npm = { apt = "npm", dnf = "npm", zypper = "npm", pacman = "npm", apk = "npm", brew = "node" }
M.tsx = npm("tsx")
M["ts-node"] = npm("ts-node")
M.python3 = { apt = "python3", dnf = "python3", zypper = "python3", pacman = "python", apk = "python3", brew = "python" }
M.python = M.python3
M.cargo = cargo_row
M.rustc = cargo_row
M.vhdl_ls = cargo("vhdl_ls")
M.zig = with({}, { dnf = "zig", pacman = "zig", brew = "zig", fallback = "https://ziglang.org/download/" })
M.julia = with({}, { pacman = "julia", brew = "julia", fallback = "https://julialang.org/downloads/" })
M.nim = with({}, { pacman = "nim", brew = "nim", fallback = "https://nim-lang.org/install.html" })
M.kotlinc = with({}, { pacman = "kotlin", brew = "kotlin", fallback = "https://kotlinlang.org/docs/command-line.html" })

-- ── Compilers, build and VHDL / COBOL / Fortran ───────────────────────────────

M.gcc = with(std("gcc"), { fallback = "xcode-select --install (macOS)" })
M["g++"] = with(std("gcc"), { apt = "g++", dnf = "gcc-c++", zypper = "gcc-c++" })
M.gfortran = with(std("gcc"), { apt = "gfortran", dnf = "gcc-gfortran", zypper = "gcc-fortran", pacman = "gcc-fortran" })
M.make = std("make")
M.cmake = std("cmake")
M.ctest = M.cmake
M.cobc = std("gnucobol")
M.ghdl = std("ghdl")
M.gtkwave = std("gtkwave")

-- ── Shell, search, network and utilities ──────────────────────────────────────

M.git = std("git")
M.rg = std("ripgrep")
M.fd = { apt = "fd-find", dnf = "fd-find", zypper = "fd", pacman = "fd", apk = "fd", brew = "fd" }
M.lazygit = {
  dnf = "lazygit",
  pacman = "lazygit",
  brew = "lazygit",
  fallback = "https://github.com/jesseduffield/lazygit#installation",
}
M.curl = std("curl")
M.jq = std("jq")
M.tmux = std("tmux")
M.shellcheck = std("shellcheck")
M["tree-sitter"] = {
  zypper = "tree-sitter",
  pacman = "tree-sitter-cli",
  brew = "tree-sitter",
  fallback = "npm install -g tree-sitter-cli",
}
M.deno = { pacman = "deno", brew = "deno", fallback = "https://docs.deno.com/runtime/getting_started/installation/" }
M.xclip = {
  apt = "xclip",
  dnf = "xclip",
  zypper = "xclip",
  pacman = "xclip",
  apk = "xclip",
  fallback = "install xclip, xsel or wl-clipboard",
}

-- ── Formatters and linters (language package managers as fallback) ───────────

M.stylua = with(cargo("stylua"), { pacman = "stylua", brew = "stylua" })
M.black = pip("black")
M.isort = pip("isort")
M.ruff = with(pip("ruff"), { pacman = "ruff", brew = "ruff" })
M.fprettify = pip("fprettify")
M.vsg = pip("vsg")
M.sqlfmt = pip("shandy-sqlfmt")
M.goimports = { fallback = "go install golang.org/x/tools/cmd/goimports@latest" }
M.gofumpt = { fallback = "go install mvdan.cc/gofumpt@latest" }
M.shfmt = { apt = "shfmt", pacman = "shfmt", brew = "shfmt", fallback = "go install mvdan.cc/sh/v3/cmd/shfmt@latest" }
M.ktlint = { pacman = "ktlint", brew = "ktlint", fallback = "https://pinterest.github.io/ktlint/" }
M["clang-format"] = {
  apt = "clang-format",
  dnf = "clang-tools-extra",
  zypper = "clang-tools",
  pacman = "clang",
  brew = "clang-format",
}
M.prettier = npm("prettier")
M.eslint_d = npm("eslint_d")
M.stylelint = npm("stylelint")
M.htmlhint = npm("htmlhint")
M["markdown-link-check"] = npm("markdown-link-check")

return M
