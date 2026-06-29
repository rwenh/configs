# INSTALL — v2.5.0

Complete installation guide. Quick-start: [README.md](README.md).

---

## Contents

- [Prerequisites](#prerequisites)
- [System Packages](#system-packages)
- [Language Tooling](#language-tooling)
- [Fonts](#fonts)
- [Configuration](#configuration)
- [First Launch](#first-launch)
- [Lockfile Workflow](#lockfile-workflow)
- [Escape Hatches](#escape-hatches)
- [Hot-swap Reference](#hot-swap-reference)
- [Troubleshooting](#troubleshooting)

---

## Prerequisites

| Tool | Min version | Check |
|------|-------------|-------|
| Neovim | **0.11** | `nvim --version` |
| git | any | `git --version` |
| gcc or clang | any | `gcc --version` |
| node | **18** | `node --version` |
| npm | **8** | `npm --version` |
| python3 | **3.9** | `python3 --version` |
| pip | any | `pip3 --version` |
| rust + cargo | stable | `rustc --version` |
| ripgrep | any | `rg --version` |
| fd | any | `fd --version` |
| lazygit | any | `lazygit --version` |
| Nerd Font | **v3+** | visual check in terminal |

---

## System Packages

### openSUSE Leap 16.0

```bash
sudo zypper in \
  gcc gcc-c++ make cmake ninja git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck \
  ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard
```

### Ubuntu / Debian

```bash
sudo apt update && sudo apt install -y \
  gcc g++ make cmake ninja-build git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-dev \
  rustc cargo golang ripgrep fd-find shellcheck \
  ruby ruby-dev erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip wl-clipboard

ln -s $(which fdfind) ~/.local/bin/fd
```

### Arch Linux

```bash
sudo pacman -S \
  gcc make cmake ninja git curl wget unzip \
  neovim nodejs npm python python-pip \
  rust cargo go ripgrep fd shellcheck \
  ruby erlang elixir gnucobol ghdl gtkwave \
  sqlite xclip wl-clipboard lazygit
```

### macOS (Homebrew)

```bash
brew install neovim git node python3 rust go ripgrep fd lazygit \
  shellcheck ruby erlang elixir sqlite3 gnu-cobol ghdl
```

> Ensure `$(go env GOPATH)/bin` is in `PATH`.

---

## Language Tooling

### Python

```bash
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv vsg
```

### Node / JavaScript / TypeScript

```bash
npm install -g typescript ts-node tsx prettier eslint_d neovim
```

### Ruby

```bash
gem install solargraph rubocop debug
```

### Rust tools

```bash
cargo install stylua vhdl_ls
```

### Go tools

```bash
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest
```

> Both tools are required — the config warns individually if either is missing.

### COBOL LSP — manual install required

```bash
npm install -g @broadcommfd/cobol-language-support
```

Verify: `cobol-language-server --version`

### VHDL LSP

```bash
cargo install vhdl_ls
```

Verify: `vhdl_ls --version`

---

## Fonts

```bash
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv
```

Set `FiraCode Nerd Font Mono` in your terminal.

---

## Configuration

```bash
git clone https://github.com/rwenh/configs.git ~/dotfiles

[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)

ln -sf ~/dotfiles/nvim ~/.config/nvim
```

---

## First Launch

```vim
" Step 1 — open Neovim; lazy.nvim auto-bootstraps. Wait for 100% complete, then:

" Step 2 — install LSP servers, DAP adapters, formatters, linters
:MasonInstallAll

" Step 3 — install Treesitter parsers
:TSUpdate

" Step 4 — verify
:checkhealth

" Step 5 — pin all plugin commits for reproducible deployments
:Lazy lock
```

| Check | Expected |
|-------|----------|
| `:checkhealth` | No ERROR items |
| `:Mason` | All packages show ✓ |
| `:TSUpdate` | All parsers up to date |
| Statusline | Branch, diagnostics, filetype icons visible |
| Dashboard | Logo + quote on startup |
| `gd` on any symbol | LSP jumps to definition |
| `lazy-lock.json` | Present in `~/.config/nvim/` after `:Lazy lock` |

> Slow network: set `vim.g.mason_install_timeout_ms = 300000` in `init.lua` before first launch.

---

## Lockfile Workflow

`lazy-lock.json` pins every plugin to an exact commit SHA, making deployments fully reproducible.

```vim
" After :Lazy update — re-pin to the new commits
:Lazy lock

" On a new machine after git pull — restore exact pinned commits
:Lazy restore

" Preview what would change before updating
:Lazy check
```

```bash
# Commit the lockfile with your dotfiles
cd ~/dotfiles
git add nvim/lazy-lock.json
git commit -m "chore: update plugin lockfile"
```

> Without a committed `lazy-lock.json`, `:Lazy update` can introduce breaking changes.
> The lockfile is the recommended mitigation when using `version = false` specs.

---

## Escape Hatches

Set at the **top of `init.lua`** before `require("core.bootstrap")`.

### Core display & behaviour

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_highlight_overrides` | `false` | Skip all highlight overrides |
| `vim.g.disable_tint` | `false` | Disable tint.nvim |
| `vim.g.disable_smear_cursor` | `false` | Disable smear-cursor |
| `vim.g.disable_mini_pairs` | `false` | Disable mini.pairs auto-close |
| `vim.g.disable_autoformat` | `false` | Disable format-on-save globally |
| `vim.g.disable_treesitter_folds` | `false` | Use indent folding instead of treesitter |
| `vim.g.auto_cd_root` | `false` | Auto-cd to project root on BufEnter |
| `vim.g.runner_autosave` | `true` | Auto-save before running a file |

### LSP & formatting

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.format_timeout_ms` | `3000` | Global format-on-save timeout (ms) |
| `vim.g.format_timeout_by_ft` | `{}` | Per-filetype timeout table |
| `vim.g.enable_pylint` | `false` | Opt-in pylint alongside ruff |
| `vim.g.debugpy_python` | `nil` | Pin Python interpreter for debugpy |
| `vim.g.completion_sources_by_ft` | `nil` | Per-filetype blink.cmp source list |

### DAP & testing

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.dap_bp_autosave_ms` | `60000` | DAP breakpoint autosave interval (ms) |
| `vim.g.neotest_concurrency` | `4` | Parallel test suite concurrency |

### Mason & packages

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.mason_install_timeout_ms` | `120000` | Per-install timeout (ms) |
| `vim.g.mason_extras` | `nil` | Extra Mason packages for `:MasonInstallAll` |

### Project root

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.path_max_walk_depth` | `20` | Upward-walk limit for root detection |
| `vim.g.path_cache_ttl` | `30` | Root-cache lifetime in seconds |
| `vim.g.path_debug` | `false` | Log root-detection fallbacks at DEBUG |
| `vim.g.path_ignore_dirs` | `nil` | Extra directory names to skip |
| `vim.g.cmake_build_dir` | `"build"` | CMake build directory name |

### Language-specific

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_vsg_format` | `false` | Disable VHDL vsg formatter |
| `vim.g.elixir_use_nextls` | `false` | Use NextLS for Elixir instead of elixir-ls |
| `vim.g.kotlin_spring_cache_ttl` | `300` | Spring Boot detection cache TTL (seconds) |
| `vim.g.python_docstring_style` | `"google_docstrings"` | Neogen Python docstring convention |
| `vim.g.c_build_flags` | `"-Wall -Wextra -g"` | GCC build flags |
| `vim.g.cpp_build_flags` | `"-Wall -std=c++17 -g"` | G++ build flags |
| `vim.g.fortran_build_flags` | `"-Wall"` | gfortran build flags |
| `vim.g.ts_disable` | `nil` | List of Treesitter parsers to disable |
| `vim.g.filetype_options` | `nil` | Per-filetype vim option overrides |
| `vim.g.spell_wordlist` | `nil` | Path to personal spell file |

### UI extras

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.symbol_usage_max_lines` | `2000` | Disable symbol-usage.nvim above this line count |
| `vim.g.force_mini_animate_scroll` | `false` | Force mini.animate scroll with neoscroll present |
| `vim.g.octo_timeout_ms` | `10000` | Octo.nvim request timeout (ms) |
| `vim.g.workflow_template_debounce_ms` | `300` | Overseer template load debounce (ms) |

---

## Hot-swap Reference

### Safe to `:luafile`

These modules contain no startup-only state and can be reloaded mid-session:

```vim
:luafile ~/.config/nvim/lua/core/keymaps.lua
:luafile ~/.config/nvim/lua/core/commands.lua
:luafile ~/.config/nvim/lua/core/autocmds.lua
:luafile ~/.config/nvim/lua/core/highlights.lua
:luafile ~/.config/nvim/lua/core/util/quotes.lua
:luafile ~/.config/nvim/lua/core/util/runner.lua
:luafile ~/.config/nvim/lua/core/util/path.lua
:luafile ~/.config/nvim/lua/core/util/exec.lua
:luafile ~/.config/nvim/lua/core/util/term.lua
:luafile ~/.config/nvim/lua/core/util/mason.lua
```

> `focus.lua` supports hot-reload when focus mode is **inactive**. If focus mode
> is currently active, exit it first (`<leader>uF`) before reloading.

### Plugin reloads (use Lazy)

```vim
:Lazy reload nvim-lspconfig
:Lazy reload neotest
:Lazy reload nvim-dap
:Lazy reload conform.nvim
:Lazy reload nvim-lint
:Lazy reload overseer.nvim
```

### Requires full restart

| Module | Reason |
|--------|--------|
| `ui.lua` | Dashboard registers autocmds at startup |
| `bootstrap.lua` | Leader keys must precede all plugin loading |
| `options.lua` | Some options only take effect before plugins load |
| `treesitter.lua` | Parser state is process-scoped |
| Any theme spec | Priority flags set at startup |
| `lsp.lua` | Capabilities memoised at startup |
| `completion.lua` | blink.cmp capability injection precedes LSP init |
| `dap.lua` | Adapter FileType autocmds registered once |
| `test.lua` | Adapter list assembled at config time |
| Any `lang/*.lua` | LSP on_attach keymaps set at startup |
| Any `vim.g` flag | Flags read once at plugin-load time |

---

## Troubleshooting

```vim
:Health                              " version, LSP count, memory
:checkhealth
:checkhealth lsp
:LspInfo
:Mason
:lua print(vim.g.nvim_ide_version)
```

| Symptom | Fix |
|---------|-----|
| LSP not attaching | `:MasonInstallAll` · `:checkhealth lsp` |
| Completion not working | `:Lazy update` · verify `version = "1.*"` in `completion.lua` |
| blink.cmp major-version warning | Update version pin in `completion.lua` |
| COBOL LSP not attaching | `npm i -g @broadcommfd/cobol-language-support` + restart |
| VHDL LSP not attaching | `cargo install vhdl_ls` + restart |
| TypeScript LSP double-attaching | Ensure typescript-tools.nvim installed |
| tint / smear-cursor artefacts | `vim.g.disable_tint = true` / `vim.g.disable_smear_cursor = true` |
| `:MasonInstallAll` times out | `vim.g.mason_install_timeout_ms = 240000` |
| debugpy not found | `vim.g.debugpy_python = "/path/to/python"` |
| Wrong project root | `vim.g.path_debug = true` · check DEBUG notifications |
| Java workspace stale | Delete `~/.local/share/nvim/jdtls-workspace/` + restart |
| `:LspRestart` not re-attaching jdtls | Ensure java.lua v2.5.0 — `jdtls_started` now clears on `LspDetach` |
| Treesitter highlighting broken | `:TSUpdate` |
| DAP adapter not found | `:MasonInstallAll` |
| `gd` does nothing | `:LspInfo` · `:checkhealth lsp` |
| Kotlin tests not running | `chmod +x gradlew` in project root |
| Large file slow | Expected — auto-protection at 500 KB |
| Plugins not loading | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |
| Format silently does nothing | Check for WARN notification; increase `vim.g.format_timeout_ms` |
| htmlhint not linting HTML | Add `.htmlhintrc` or `"htmlhint"` key in `package.json` |
| stylelint not linting CSS | Add `.stylelintrc.json` or `"stylelint"` key in `package.json` |
| telescope-fzf-native fails to load | `cd ~/.local/share/nvim/lazy/telescope-fzf-native.nvim && make` |
| symbol-usage slow on large codebase | `vim.g.symbol_usage_max_lines = 1000` |
| `Ctrl+h/j/k/l` stays inside Neovim | `is_vim` check missing from `.tmux.conf` |
| Elixir env not re-checked on project switch | Ensure elixir.lua v2.5.0 |
| Ruby bundler not re-checked on project switch | Ensure ruby.lua v2.5.0 |
| Plugins drifting unexpectedly | Run `:Lazy lock` and commit `lazy-lock.json` |
| `<Esc>` exits lazygit/TUI unexpectedly | See keymaps.lua comment for 3 workaround options |

### Nuclear reset

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim
nvim
# :MasonInstallAll  :TSUpdate  :Lazy lock  :checkhealth
```

### Neovim too old (openSUSE)

```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```
