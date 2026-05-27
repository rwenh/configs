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
- [Load-order Constraints](#load-order-constraints)
- [Hot-swap Reference](#hot-swap-reference)
- [Troubleshooting](#troubleshooting)

---

## Prerequisites

All must be present before first launch.

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

# fd is fd-find on Debian/Ubuntu:
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
# pbcopy/pbpaste are built-in; no clipboard package needed
```

> **Go tools:** ensure `$(go env GOPATH)/bin` is in `PATH`.

---

## Language Tooling

Install after system packages. Skip sections for languages you won't use.

### Python

```bash
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv
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

Not in the Mason registry:

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

FiraCode Nerd Font recommended:

```bash
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv
```

Set `FiraCode Nerd Font Mono` in your terminal. The "Mono" variant prevents icon width misalignment in Neovim.

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

Run these in order. Each step depends on the previous.

```vim
" Step 1 — open Neovim; lazy.nvim auto-bootstraps and installs all plugins.
"           Wait for the lazy.nvim UI to show 100% complete, then:

" Step 2 — install LSP servers, DAP adapters, formatters, linters
:MasonInstallAll

" Step 3 — install Treesitter parsers
:TSUpdate

" Step 4 — verify
:checkhealth
```

Expected state after install:

| Check | Expected |
|-------|----------|
| `:checkhealth` | No ERROR items |
| `:Mason` | All packages show ✓ |
| `:TSUpdate` | All parsers installed / up to date |
| Statusline | Branch, diagnostics, filetype icons visible |
| Dashboard | Logo + quote on startup |
| `gd` on any symbol | LSP jumps to definition |

---

## Load-order Constraints

Enforced in `plugins/specs/init.lua`. Do not reorder.

| Earlier | Later | Why |
|---------|-------|-----|
| `completion` | `lsp` | blink.cmp must register capabilities before LSP servers start |
| `lsp` | `lang/*` | lang specs extend `formatters_by_ft` and `linters_by_ft` |
| `web` | `html`, `css` | nvim-ts-autotag and emmet must load before HTML/CSS configs run |
| `database` | — | owns all SQL config; `sql.lua` was deleted |

---

## Hot-swap Reference

### Pure Lua modules (safe to `:luafile`)

```vim
:luafile ~/.config/nvim/lua/core/keymaps.lua
:luafile ~/.config/nvim/lua/core/commands.lua
:luafile ~/.config/nvim/lua/core/focus.lua
:luafile ~/.config/nvim/lua/core/autocmds.lua
:luafile ~/.config/nvim/lua/core/highlights.lua
:luafile ~/.config/nvim/lua/core/util/quotes.lua
:luafile ~/.config/nvim/lua/core/util/runner.lua
:luafile ~/.config/nvim/lua/core/util/path.lua
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
| `ui.lua` | snacks.nvim dashboard registers autocmds at startup |
| `bootstrap.lua` | leader keys must precede all plugin loading |
| `options.lua` | some options only take effect before plugins load |
| `treesitter.lua` | parser state is process-scoped |
| Any theme spec | lazy.nvim priority flags are set at startup |

---

## Troubleshooting

### Diagnostic commands

```vim
:Health                              " version, LSP count, memory
:checkhealth                         " full Neovim check
:checkhealth lsp
:LspInfo                             " active clients for current buffer
:Mason
:lua print(vim.g.nvim_ide_version)   " confirm version stamp
```

### Issue reference

| Symptom | Fix |
|---------|-----|
| LSP not attaching | `:MasonInstallAll` · `:checkhealth lsp` |
| Completion not working | `:Lazy update` · verify `version = "1.*"` in `completion.lua` |
| COBOL LSP not attaching | `npm i -g @broadcommfd/cobol-language-support` + restart |
| VHDL LSP not attaching | `cargo install vhdl_ls` + restart |
| Dashboard quote not visible | Full restart required after `ui.lua` changes |
| Treesitter highlighting broken | `:TSUpdate` |
| DAP adapter not found | `:MasonInstallAll` |
| `gd` does nothing | `:LspInfo` in buffer · `:checkhealth lsp` |
| Kotlin tests not running | `chmod +x gradlew` in project root |
| Large file slow | Expected — auto-protection at 500 KB |
| Plugins not loading at all | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |

### Neovim too old (openSUSE)

```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```

### Nuclear reset

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim
nvim
# :MasonInstallAll  :TSUpdate  :checkhealth
```
