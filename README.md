# nvim-ide

Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages.

> Neovim **0.11+** · openSUSE Leap 16.0 · v**2.3.16**

---

## Requirements

| Tool | Purpose |
|------|---------|
| `neovim >= 0.11` | Core |
| `git` | Plugin bootstrap |
| `gcc` / `clang` | Treesitter parsers |
| `node >= 18` + `npm` | LSP servers, formatters |
| `python3` + `pip` | Python LSP, debugpy |
| `rust` + `cargo` | Rust LSP, stylua |
| `ripgrep` | Telescope live grep |
| `fd` | Telescope file finder |
| `lazygit` | LazyGit TUI |
| [Nerd Font](https://www.nerdfonts.com/) | Icons |

---

## Install

```bash
# 1. System packages
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck \
  ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard

# 2. Language tooling
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv
npm install -g typescript ts-node tsx prettier eslint_d neovim
gem install solargraph rubocop debug
cargo install stylua vhdl_ls
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest

# 3. Nerd Font (FiraCode)
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv

# 4. Clone and link
git clone https://github.com/rwenh/configs.git ~/dotfiles
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
ln -sf ~/dotfiles/nvim ~/.config/nvim

# 5. First launch
nvim                   # lazy.nvim auto-installs all plugins
:MasonInstallAll       # install LSP servers, DAP adapters, formatters
:checkhealth           # verify
```

One-shot: `bash <(curl -fsSL https://raw.githubusercontent.com/rwenh/configs/main/install.sh)`

---

## Structure

```
~/.config/nvim/
├── init.lua
└── lua/
    ├── core/
    │   ├── bootstrap.lua       # lazy.nvim bootstrap, leader key, version stamp
    │   ├── options.lua         # vim options
    │   ├── keymaps.lua         # global keymaps
    │   ├── autocmds.lua        # autocommands
    │   ├── commands.lua        # user commands (:MasonInstallAll, :Format, …)
    │   ├── theme.lua           # theme management + dark/light toggle
    │   ├── focus.lua           # deep focus mode
    │   ├── highlights.lua      # highlight overrides (survives theme toggles)
    │   ├── rain.lua            # matrix rain dashboard engine
    │   └── util/
    │       ├── path.lua        # project root detection with TTL cache
    │       ├── runner.lua      # file / selection / test runner
    │       ├── term.lua        # toggleterm launch helper
    │       ├── buf_keymap.lua  # buffer-local keymap registration helper
    │       ├── mason.lua       # Mason binary / package path helpers
    │       ├── packages.lua    # Mason + lspconfig package lists (single source)
    │       └── icons.lua       # shared Nerd Font icon constants
    └── plugins/
        ├── init.lua            # lazy.nvim setup
        └── specs/
            ├── ui.lua          # themes, statusline, bufferline, dashboard
            ├── editor.lua      # telescope, neo-tree, flash, harpoon, sessions
            ├── lsp.lua         # LSP, Mason, conform, nvim-lint
            ├── completion.lua  # blink.cmp
            ├── treesitter.lua  # treesitter + context + textobjects
            ├── git.lua         # gitsigns, lazygit, diffview, neogit, octo
            ├── dap.lua         # nvim-dap + UI + adapters
            ├── test.lua        # neotest + coverage
            ├── advanced.lua    # ufo, neogen, rainbow, matchup, mini.*
            ├── hud.lua         # noice, barbecue, oil, animate, zen, twilight, blame
            ├── workflow.lua    # overseer task runner
            └── lang/           # per-language specs (20+ languages)
                └── shared.lua  # filetype constants + shared.treesitter() helper
```

---

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | — | — | JUnit |
| Kotlin | kotlin_language_server | java-debug | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls | elixir-ls | mix | — | ExUnit |
| C | clangd | codelldb | clang-format | clang-tidy | ctest |
| C++ | clangd | codelldb | clang-format | — | ctest |
| HTML | html-lsp | — | prettier | htmlhint | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb | zig fmt | — | zig test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-server | — | — | — | — |
| VHDL | vhdl_ls | — | vsg | — | — |

---

## Key Bindings

Leader: `Space` · Full reference: [`KEYMAP_REFERENCE.md`](KEYMAP_REFERENCE.md)

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fw` | Live grep |
| `<leader>ee` | Toggle explorer |
| `<leader>.g` | LazyGit |
| `gd` / `gr` / `K` | LSP: definition / references / hover |
| `<leader>,a` | Code action |
| `<leader>,f` | Format |
| `F5`–`F11` | DAP: continue / breakpoint / step |
| `<leader>'r` | Run file |
| `<leader>'t` | Run tests |
| `<leader>ut` | Toggle dark/light |
| `<leader>uF` | Deep focus |
| `s` | Flash jump |

---

## Themes

Set in `lua/core/theme.lua`:

```lua
M.config = {
  theme     = "tokyonight",
  day_start = 7,
  day_end   = 19,
}
```

Available: `tokyonight` · `catppuccin` · `rose-pine` · `kanagawa` · `gruvbox-material` · `solarized` · `solarized-osaka`
Toggle at runtime: `<leader>ut`

---

## Updating

```vim
:Lazy update
:MasonUpdate
:TSUpdate
```

## Troubleshooting

```bash
# Corrupted plugin state
rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim

# Neovim too old (openSUSE)
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```

```vim
:checkhealth lsp    " LSP not attaching
:LspInfo
:MasonInstallAll    " Mason package failed
:checkhealth mason
```
