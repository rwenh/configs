# INSTALL — v2.4.1

Complete installation guide. Quick-start: [README.md](README.md).

---

## Contents

- [Prerequisites](#prerequisites)
- [System Packages](#system-packages)
- [Language Tooling](#language-tooling)
- [Fonts](#fonts)
- [Configuration](#configuration)
- [First Launch](#first-launch)
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
```

| Check | Expected |
|-------|----------|
| `:checkhealth` | No ERROR items |
| `:Mason` | All packages show ✓ |
| `:TSUpdate` | All parsers up to date |
| Statusline | Branch, diagnostics, filetype icons visible |
| Dashboard | Logo + quote on startup |
| `gd` on any symbol | LSP jumps to definition |

> Slow network: set `vim.g.mason_install_timeout_ms = 300000` in `init.lua` before first launch.

---

## Escape Hatches

Set at the **top of `init.lua`** before `require("core.bootstrap")`.

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_highlight_overrides` | `false` | Skip all highlight overrides |
| `vim.g.disable_tint` | `false` | Disable tint.nvim |
| `vim.g.disable_smear_cursor` | `false` | Disable smear-cursor |
| `vim.g.disable_vsg_format` | `false` | Disable VHDL vsg formatter |
| `vim.g.mason_install_timeout_ms` | `120000` | Per-install timeout (ms) |
| `vim.g.debugpy_python` | `nil` | Pin Python interpreter for debugpy |
| `vim.g.path_max_walk_depth` | `20` | Upward-walk limit for root detection |
| `vim.g.path_cache_ttl` | `30` | Root-cache lifetime in seconds |
| `vim.g.path_debug` | `false` | Log root-detection fallbacks at DEBUG |

---

## Hot-swap Reference

### Safe to `:luafile`

```vim
:luafile ~/.config/nvim/lua/core/keymaps.lua
:luafile ~/.config/nvim/lua/core/commands.lua
:luafile ~/.config/nvim/lua/core/focus.lua
:luafile ~/.config/nvim/lua/core/autocmds.lua
:luafile ~/.config/nvim/lua/core/highlights.lua
:luafile ~/.config/nvim/lua/core/util/quotes.lua
:luafile ~/.config/nvim/lua/core/util/runner.lua
:luafile ~/.config/nvim/lua/core/util/path.lua
:luafile ~/.config/nvim/lua/core/util/exec.lua
:luafile ~/.config/nvim/lua/core/util/term.lua
```

### Plugin reloads (use Lazy)

```vim
:Lazy reload nvim-lspconfig
:Lazy reload neotest
:Lazy reload nvim-dap
:Lazy reload conform.nvim
:Lazy reload nvim-lint
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
| COBOL LSP not attaching | `npm i -g @broadcommfd/cobol-language-support` + restart |
| VHDL LSP not attaching | `cargo install vhdl_ls` + restart |
| TypeScript LSP double-attaching | Ensure typescript-tools.nvim installed |
| tint / smear-cursor artefacts | `vim.g.disable_tint = true` / `vim.g.disable_smear_cursor = true` |
| `:MasonInstallAll` times out | `vim.g.mason_install_timeout_ms = 240000` |
| debugpy not found | `vim.g.debugpy_python = "/path/to/python"` |
| Wrong project root | `vim.g.path_debug = true` · check DEBUG notifications |
| Java workspace stale | Delete `~/.local/share/nvim/jdtls-workspace/` + restart |
| Treesitter highlighting broken | `:TSUpdate` |
| DAP adapter not found | `:MasonInstallAll` |
| `gd` does nothing | `:LspInfo` · `:checkhealth lsp` |
| Kotlin tests not running | `chmod +x gradlew` in project root |
| Large file slow | Expected — auto-protection at 500 KB |
| Plugins not loading | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |

### Nuclear reset

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim
nvim
# :MasonInstallAll  :TSUpdate  :checkhealth
```

### Neovim too old (openSUSE)

```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```
