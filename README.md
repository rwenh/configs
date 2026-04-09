# nvim-ide

A modular Neovim IDE config — lazy-loaded, LSP-first, 20+ languages.

> Tested on **openSUSE Leap 16.0** · Neovim **0.11+** required · v2.3.0

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

echo "→ Go"
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest

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
    │   ├── focus.lua           # deep focus mode (strips chrome + Twilight + Zen)
    │   ├── hud.lua             # synthwave accent highlight overrides
    │   └── util/
    │       ├── path.lua        # project root detection + caching
    │       └── runner.lua      # file/selection/test runner engine
    └── plugins/
        ├── init.lua            # lazy.nvim setup
        └── specs/
            ├── ui.lua          # themes, statusline, bufferline, snacks dashboard
            ├── editor.lua      # telescope, tree, flash, harpoon, sessions
            ├── lsp.lua         # LSP, Mason, conform, nvim-lint
            ├── completion.lua  # blink.cmp
            ├── treesitter.lua  # treesitter + context + textobjects
            ├── git.lua         # gitsigns, lazygit, diffview, octo
            ├── dap.lua         # nvim-dap + UI + adapters (all languages)
            ├── test.lua        # neotest + coverage (all adapters)
            ├── advanced.lua    # colorizer, navic, rainbow, better-escape, ufo
            ├── hud.lua         # indent-blankline, neoscroll, noice, barbecue…
            ├── workflow.lua    # overseer task runner
            └── lang/           # per-language plugin specs (20+ languages)
```

---

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff | pytest |
| Rust | rust-analyzer *(rustaceanvim)* | codelldb | rustfmt | clippy | cargo |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | — | — | JUnit |
| Kotlin | kotlin_language_server | — | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls | elixir-ls | mix | — | ExUnit |
| C | clangd | codelldb | clang-format | — | — |
| C++ | clangd + clangd_extensions | codelldb | clang-format | — | — |
| HTML | html-lsp | — | prettier | htmlhint | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint | — |
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
| `<leader>fo` | Recent files |
| `<leader>ee` | Toggle explorer |
| `<leader>.g` | LazyGit |
| `<leader>ww` | Save |
| `<leader>wq` | Save & quit |
| `<leader>ut` | Toggle dark/light theme |
| `<leader>uz` | Zen mode |
| `<leader>xu` | Undo tree |
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
| `<leader>'d` | Neotest debug nearest |

### Language shortcuts (examples)

| Key | Action |
|-----|--------|
| `<leader>rh` | Rust hover actions |
| `<leader>got` | Go test |
| `<leader>pyv` | Python select venv |
| `<leader>tso` | TS organize imports |
| `<leader>ccb` | CMake build |
| `<leader>dbu` | Toggle DB UI |
| `<leader>rer` | REST run request |
| `<leader>mp` | Markdown preview |

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

## Known Issues / Pending Fixes (v2.3.0)

The following modules have known bugs being tracked for v2.3.1:

| Module | Issue |
|--------|-------|
| `completion.lua` | `version=false` tracks HEAD — pin to latest stable blink.cmp tag |
| `lsp.lua` | nvim-0.11 `vim.lsp.enable()` + mason-lspconfig double-attach on some servers |
| `dap.lua` | `load_breakpoints()` restores on large files before treesitter parses — marks land on wrong lines |
| `python.lua` | iron.nvim REPL keymaps (`send_motion`, `visual_send`) set globally at load time, not per-buffer |
| `test.lua` | neotest-rust race condition if rustaceanvim not yet attached on first Rust file open |
| `workflow.lua` | `overseer.run_template({ name="build" })` unhandled error when no build template matches |
| `autocmds.lua` | `TrimWhitespace` calls `nvim_buf_set_lines` per line in a loop — should be batched in one call |
| `treesitter.lua` | `foldexpr` uses deprecated v3 API — should be `v:lua.vim.treesitter.foldexpr()` |
| `advanced.lua` | `close_fold_kinds` field renamed to `close_fold_kinds_for_ft` in recent nvim-ufo |
| `keymaps.lua` | `<leader>un` collision — mapped to both "toggle line numbers" and "Noice dismiss" |

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
