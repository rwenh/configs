# nvim-ide

A modular Neovim IDE config — lazy-loaded, LSP-first, 20+ languages.

> Tested on **openSUSE Leap 16.0** · Neovim **0.11+** required

---

## Requirements

| Tool | Purpose |
|------|---------|
| `neovim >= 0.11` | Core |
| `git` | Plugin manager bootstrap |
| `gcc` / `clang` | Treesitter parser compilation |
| `node >= 18` + `npm` | LSP servers, formatters |
| `python3` + `pip` | Python LSP, debugpy, tools |
| `rust` + `cargo` | Rust LSP, stylua |
| `ripgrep` | Telescope live grep |
| `fd` | Telescope file finder |
| `lazygit` | LazyGit integration |
| A [Nerd Font](https://www.nerdfonts.com/) | Icons |

---

## Installation

### 1 — System packages

```bash
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck \
  ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard
```

### 2 — Language tooling

```bash
# Python
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv

# Node
npm install -g typescript ts-node tsx prettier eslint_d neovim

# Ruby
gem install solargraph rubocop debug

# Rust
cargo install stylua vhdl_ls

# Go
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest
```

### 3 — Nerd Font

```bash
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv
```

Then set **FiraCode Nerd Font** (or any Nerd Font) in your terminal emulator.

### 4 — Clone & link

```bash
git clone https://github.com/rwenh/configs.git ~/dotfiles

# Backup existing config
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)

ln -sf ~/dotfiles/nvim ~/.config/nvim
```

### 5 — First launch

```bash
nvim                  # lazy.nvim auto-installs all plugins
```

Then inside Neovim:

```vim
:MasonInstallAll      " install all LSP servers, DAP adapters, formatters
:checkhealth          " verify everything is healthy
```

Restart Neovim once Mason finishes.

---

## One-shot script

```bash
bash <(curl -fsSL https://raw.githubusercontent.com/rwenh/configs/main/install.sh)
```

<details>
<summary>Or paste manually</summary>

```bash
#!/bin/bash
set -e

echo "→ System packages"
sudo zypper in -y gcc gcc-c++ make cmake ninja git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck \
  ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard

echo "→ Python"
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv

echo "→ Node"
npm install -g typescript ts-node tsx prettier eslint_d neovim

echo "→ Ruby"
gem install solargraph rubocop debug

echo "→ Rust"
cargo install stylua vhdl_ls

echo "→ Nerd Font (FiraCode)"
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv && cd ~

echo "→ Dotfiles"
[ -d ~/dotfiles ] || git clone https://github.com/rwenh/configs.git ~/dotfiles
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)
ln -sf ~/dotfiles/nvim ~/.config/nvim

echo "✓ Done — run: nvim"
```
</details>

---

## Structure

```
~/.config/nvim/
├── init.lua                    # entry point
└── lua/
    ├── core/
    │   ├── bootstrap.lua       # lazy.nvim bootstrap, leader key
    │   ├── options.lua         # vim options
    │   ├── keymaps.lua         # global keymaps
    │   ├── autocmds.lua        # autocommands
    │   ├── commands.lua        # user commands (:MasonInstallAll etc.)
    │   ├── theme.lua           # theme management + dark/light toggle
    │   └── util/
    │       ├── path.lua        # project root detection
    │       └── runner.lua      # file/selection/test runner engine
    └── plugins/
        ├── init.lua            # lazy.nvim setup
        └── specs/
            ├── ui.lua          # themes, statusline, bufferline, dashboard
            ├── editor.lua      # telescope, tree, flash, harpoon, sessions
            ├── lsp.lua         # LSP, Mason, conform, nvim-lint
            ├── completion.lua  # blink.cmp
            ├── treesitter.lua  # treesitter + context
            ├── git.lua         # gitsigns, lazygit, diffview
            ├── dap.lua         # nvim-dap + UI + adapters
            ├── test.lua        # neotest + coverage
            ├── advanced.lua    # colorizer, navic, rainbow, better-escape
            └── lang/           # per-language plugin specs (20+ languages)
```

---

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | ts_ls + typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | ts_ls | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | — | — | JUnit |
| Kotlin | kotlin_language_server | — | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls | elixir-ls | mix | — | ExUnit |
| C | clangd | codelldb | clang-format | — | — |
| C++ | clangd + clangd_extensions | codelldb | clang-format | — | — |
| HTML | html-lsp | — | prettier | htmlhint | — |
| CSS / SCSS | cssls | — | prettier | stylelint | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb / lldb | zig fmt | — | zig test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-server | — | — | — | — |
| VHDL | vhdl_ls | — | vsg | — | — |

---

## Key Bindings

Leader key: `Space`

### Core

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fw` | Live grep |
| `<leader>fb` | Find buffers |
| `<leader>ee` | Toggle explorer |
| `<leader>.g` | LazyGit |
| `<leader>ww` | Save |
| `<leader>wq` | Save & quit |
| `<leader>ut` | Toggle dark/light theme |
| `<leader>uz` | Zen mode |
| `s` | Flash jump |

### LSP (on attach)

| Key | Action |
|-----|--------|
| `gd` | Go to definition |
| `gr` | References |
| `K` | Hover docs |
| `<leader>,a` | Code action |
| `<leader>,r` | Rename |
| `<leader>,f` | Format |
| `]d` / `[d` | Next / prev diagnostic |

### Debug

| Key | Action |
|-----|--------|
| `F5` | Continue / start |
| `F6` | Toggle breakpoint |
| `F7` | Step into |
| `F8` | Step over |
| `F9` | Step out |
| `<leader>;t` | Toggle DAP UI |

### Run & Test

| Key | Action |
|-----|--------|
| `<leader>'r` | Run file |
| `<leader>'t` | Run tests |
| `<leader>'n` | Neotest nearest |
| `<leader>'f` | Neotest file |

> Full reference: [`KEYMAP_REFERENCE.md`](KEYMAP_REFERENCE.md)

---

## Themes

Edit `lua/core/theme.lua` to change the active theme:

```lua
M.config = {
  theme     = "tokyonight",   -- active theme
  day_start = 7,              -- auto light mode start hour
  day_end   = 19,             -- auto dark mode start hour
}
```

Available: `tokyonight` · `catppuccin` · `rose-pine` · `kanagawa` · `gruvbox-material` · `solarized` · `solarized-osaka`

Toggle at runtime: `<leader>ut`

---

## Troubleshooting

**Plugins not loading**
```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim
nvim  # re-installs everything
```

**LSP not attaching**
```vim
:checkhealth lsp
:LspInfo
```

**Mason package failed**
```vim
:MasonInstallAll
:checkhealth mason
```

**Neovim version too old** (openSUSE default repos)
```bash
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_15.5/ editors
sudo zypper ref && sudo zypper in neovim
```

---

## Updating

```vim
:Lazy update          " update plugins
:MasonUpdate          " update Mason registry
:TSUpdate             " update treesitter parsers
```
