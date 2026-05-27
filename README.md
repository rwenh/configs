# nvim-ide

> Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages.

**Neovim 0.11+** · openSUSE Leap 16.0 · **v2.4.1**

---

## Contents

- [Requirements](#requirements)
- [Quick Install](#quick-install)
- [Structure](#structure)
- [Architecture](#architecture)
- [Languages](#languages)
- [Plugins](#plugins)
- [Key Bindings](#key-bindings)
- [Themes](#themes)
- [Deep Focus Mode](#deep-focus-mode)
- [Updating](#updating)
- [Troubleshooting](#troubleshooting)
- [Changelog](#changelog)

---

## Requirements

### Hard requirements

| Tool | Min version | Purpose |
|------|-------------|---------|
| `neovim` | **0.11** | `vim.lsp.config()` / `vim.lsp.enable()` API |
| `git` | any | Plugin bootstrap via lazy.nvim |
| `gcc` or `clang` | any | Treesitter parser compilation |
| `node` + `npm` | **18** | LSP servers, formatters, JS debug adapter |
| `python3` + `pip` | **3.9** | basedpyright, debugpy |
| `rust` + `cargo` | stable | rust-analyzer, stylua, vhdl_ls |
| `ripgrep` | any | Telescope live grep |
| `fd` | any | Telescope file finder |
| `lazygit` | any | LazyGit TUI (`<leader>.g`) |
| [Nerd Font](https://www.nerdfonts.com/) | v3+ | Icons throughout UI |

### Soft requirements (per language)

| Language | Additional tooling |
|----------|--------------------|
| Go | `go >= 1.21`, `goimports`, `gofumpt` |
| Java | `java >= 17` (for jdtls) |
| Ruby | `ruby >= 3.0`, `gem install solargraph rubocop debug` |
| Elixir | `elixir`, `mix` |
| Fortran | `gfortran`, `fprettify` |
| COBOL | `gnucobol`; LSP manual install — see [INSTALL.md](INSTALL.md) |
| VHDL | `ghdl`, `gtkwave`; `cargo install vhdl_ls` |
| REST | `curl`, optionally `jq` |
| SQL | `sqlfmt` (formatter, optional) |

---

## Quick Install

```bash
# 1. System packages (openSUSE Leap 16.0)
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

# 3. COBOL LSP (not in Mason registry)
npm install -g @broadcommfd/cobol-language-support

# 4. Nerd Font (FiraCode recommended)
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv

# 5. Clone and link
git clone https://github.com/rwenh/configs.git ~/dotfiles
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
ln -sf ~/dotfiles/nvim ~/.config/nvim

# 6. First launch
nvim                   # lazy.nvim bootstraps and installs all plugins
:MasonInstallAll       # LSP servers, DAP adapters, formatters, linters
:TSUpdate              # Treesitter parsers
:checkhealth           # verify
```

One-shot installer:
```bash
bash <(curl -fsSL https://raw.githubusercontent.com/rwenh/configs/main/install.sh)
```

---

## Structure

```
~/.config/nvim/
├── init.lua
└── lua/
    ├── core/
    │   ├── bootstrap.lua         ← leader keys, version stamp, lazy.nvim bootstrap
    │   ├── options.lua
    │   ├── keymaps.lua           ← global keymaps (lazy-require pattern)
    │   ├── autocmds.lua
    │   ├── commands.lua          ← :MasonInstallAll, :Format, …
    │   ├── theme.lua             ← auto dark/light + toggle
    │   ├── focus.lua             ← deep focus mode
    │   ├── highlights.lua        ← overrides that survive theme toggles
    │   └── util/
    │       ├── path.lua          ← project root detection (TTL cache)
    │       ├── runner.lua        ← file / selection / test runner (20+ filetypes)
    │       ├── term.lua          ← toggleterm launch helper
    │       ├── buf_keymap.lua    ← buffer-local keymap registration + dedup
    │       ├── mason.lua         ← Mason binary/path helpers
    │       ├── packages.lua      ← Mason + lspconfig lists (single source of truth)
    │       ├── icons.lua         ← shared Nerd Font icon constants
    │       ├── exec.lua          ← executable guard helper
    │       ├── snippets.lua      ← LuaSnip snippet factory helpers
    │       └── quotes.lua        ← dashboard quotes (36 curated, 6 categories)
    └── plugins/specs/
        ├── ui.lua                ← themes, statusline, bufferline, dashboard
        ├── editor.lua            ← telescope, neo-tree, flash, harpoon, sessions
        ├── lsp.lua               ← LSP, Mason, conform, nvim-lint
        ├── completion.lua        ← blink.cmp
        ├── treesitter.lua
        ├── git.lua
        ├── dap.lua               ← nvim-dap + UI + all language adapters
        ├── test.lua              ← neotest + adapters + coverage
        ├── advanced.lua          ← ufo, neogen, rainbow, matchup, mini.*
        ├── hud.lua               ← noice, barbecue, oil, animate, zen, twilight
        ├── workflow.lua          ← overseer task runner
        └── lang/                 ← one file per language (c, cpp, rust, go, …)
```

### Load-order constraints

| Earlier | Later | Reason |
|---------|-------|--------|
| `completion` | `lsp` | blink.cmp capabilities injected before LSP servers start |
| `lsp` | `lang/*` | lang specs extend formatter and linter tables |
| `web` | `html`, `css` | autotag + emmet must be present when html/css configs run |
| `database` | — | owns all SQL config; `sql.lua` was removed |

---

## Architecture

**Single source of truth** — `packages.lua` owns all Mason/lspconfig package names. Both `:MasonInstallAll` and `mason-lspconfig.ensure_installed` read from it. Never add a server anywhere else.

**Capability memoisation** — `get_capabilities()` in `lsp.lua` is called once and cached, preventing a deep-copy per server on startup.

**Project root detection** — `path.find_root()` walks upward from the current file through standard root markers (`.git`, `Cargo.toml`, `package.json`, etc.) with a 30-second TTL, invalidated on `DirChanged`.

**Buffer-local keymap deduplication** — `bkm.batch(buf, maps, flag)` stores a boolean in `vim.b[buf][flag]`, making repeated calls on the same buffer no-ops. Prevents double-registration on `:luafile` reloads.

**Deferred DAP** — adapters register inside `FileType once=true` autocmds. Zero DAP initialisation cost unless a relevant file is opened.

**Executable guards** — `exec.require_bin(name, hint)` centralises all `vim.fn.executable()` checks. Replaced ~65 inline guard blocks across the codebase.

**Large-file protection** — files over 500 KB have treesitter, folding, syntax highlighting, cursorline, and spellcheck automatically disabled.

---

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo test |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | jdtls built-in | — | JUnit |
| Kotlin | kotlin_language_server | java-debug | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls | elixir-ls | mix format | — | ExUnit |
| C | clangd | codelldb | clang-format | clang-tidy | ctest |
| C++ | clangd | codelldb | clang-format | — | ctest |
| HTML | html-lsp | — | prettier | htmlhint | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb | zig fmt | — | zig build test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-server¹ | — | — | — | — |
| VHDL | vhdl_ls² | — | vsg | — | ghdl |

> ¹ Not in Mason registry: `npm i -g @broadcommfd/cobol-language-support`
> ² Not in Mason registry: `cargo install vhdl_ls`

---

## Plugins

### Core infrastructure

| Plugin | Role |
|--------|------|
| lazy.nvim | Plugin manager |
| mason.nvim | LSP/DAP/formatter installer |
| nvim-lspconfig | LSP client configuration |
| blink.cmp | Completion engine |
| nvim-treesitter | Syntax, folds, textobjects |
| conform.nvim | Format-on-save runner |
| nvim-lint | Async linter |
| nvim-dap + dapui | Debug adapter protocol |
| neotest | Unified test runner |

### Editor & Git

| Plugin | Role |
|--------|------|
| telescope.nvim | Fuzzy finder |
| neo-tree.nvim | File explorer |
| flash.nvim | Motion jump (`s`) |
| harpoon2 | File bookmarks |
| persistence.nvim | Session management |
| nvim-spectre | Project search/replace |
| gitsigns.nvim | Inline hunk signs + staging |
| lazygit.nvim | Full-screen Git TUI |
| diffview.nvim | Side-by-side diff + history |
| neogit | Magit-style commit/rebase UI |
| git-conflict.nvim | 3-way conflict resolution |
| octo.nvim | GitHub PR/issue workflow |

### Visual

| Plugin | Role |
|--------|------|
| snacks.nvim | Dashboard |
| lualine.nvim | Statusline |
| bufferline.nvim | Buffer tabs |
| noice.nvim | UI overhaul (cmdline, messages) |
| barbecue.nvim | LSP breadcrumb bar |
| nvim-highlight-colors | Inline colour preview |
| indent-blankline.nvim | Indent guides |
| nvim-ufo | Fold enhancement with previews |
| zen-mode.nvim | Distraction-free writing |
| twilight.nvim | Dim inactive code |
| smear-cursor.nvim | Cursor motion trail |

---

## Key Bindings

Leader: `Space` · Full reference: [KEYMAP_REFERENCE.md](KEYMAP_REFERENCE.md)

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files (Telescope) |
| `<leader>fw` | Live grep |
| `<leader>ee` | Toggle file explorer |
| `<leader>.g` | LazyGit |
| `gd` / `gD` / `gr` | LSP: definition / declaration / references |
| `K` | LSP: hover docs |
| `<leader>,a` | Code action |
| `<leader>,f` | Format buffer or range |
| `<leader>,r` | Rename symbol |
| `F5` / `F6` / `F8` | DAP: continue / breakpoint / step over |
| `<leader>'r` | Run file |
| `<leader>'n` | Neotest nearest test |
| `<leader>'t` | Run project test suite |
| `<leader>uF` | Deep focus mode |
| `<leader>ut` | Toggle dark / light theme |
| `s` | Flash jump |
| `<C-\>` | Toggle terminal |

---

## Themes

Configure in `lua/core/theme.lua`:

```lua
M.config = {
  theme     = "tokyonight",
  day_start = 7,    -- light mode from 07:00
  day_end   = 19,   -- dark mode from 19:00
  fallback  = "default",
}
```

| Name | Style |
|------|-------|
| `tokyonight` | Deep blue-purple (default) |
| `catppuccin` | Warm pastel |
| `rose-pine` | Earthy rose/grey |
| `kanagawa` | Japanese ink-wash |
| `gruvbox-material` | Warm amber |
| `solarized` | Precision contrast |
| `solarized-osaka` | Solarized + Tokyo influence |

Toggle: `<leader>ut` · Switch: `require("core.theme").switch("catppuccin")`

---

## Deep Focus Mode

`<leader>uF` hides the statusline, bufferline, signcolumn, ruler, and line numbers, then enables Twilight and ZenMode. All options are snapshotted before activation and restored exactly on exit — including when Neovim exits mid-session via `VimLeavePre`.

- `<leader>uz` — ZenMode only, chrome intact
- `<leader>uT` — Twilight only

---

## Updating

```vim
:Lazy update       " plugins
:MasonUpdate       " LSP/DAP/formatter packages
:TSUpdate          " Treesitter parsers
```

After a major Neovim upgrade:
```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim
# then: :MasonInstallAll  :TSUpdate  :checkhealth
```

---

## Troubleshooting

```vim
:Health            " custom summary (version, LSP count, memory)
:checkhealth lsp
:LspInfo           " active clients for current buffer
:Mason
```

| Symptom | Fix |
|---------|-----|
| LSP not attaching | `:MasonInstallAll` · `:checkhealth lsp` |
| Completion broken | `:Lazy update` · verify `version = "1.*"` in `completion.lua` |
| COBOL LSP missing | `npm i -g @broadcommfd/cobol-language-support` + restart |
| VHDL LSP missing | `cargo install vhdl_ls` + restart |
| Large file very slow | Expected — treesitter/folding auto-disabled above 500 KB |
| Kotlin gradlew fails | `chmod +x gradlew` in project root |
| Dashboard blank after config change | Full restart required; `:luafile` is insufficient for `ui.lua` |
| Plugins not loading at all | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |

**Neovim too old (openSUSE):**
```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```

---

## Changelog

### v2.4.1

- `bootstrap.lua` — version stamp corrected to 2.4.1
- `runner.lua` — `javascriptreact`/`typescriptreact` handled in `run_tests()`
- `focus.lua` — ZenMode desync resolved; `zm.open()`/`zm.close()` used directly
- `dap.lua` — dead `cmd` variable in `setup_ruby()` removed
- `python.lua` — `operatorfunc` saved and restored after iron send-motion keymap
- `ui.lua` — `build_header()` inlines logo + quote; `scanner.lua` no longer loaded

### v2.4.0

- `packages.lua` introduced as single source of truth for Mason/lspconfig lists
- `core/hud.lua` renamed to `core/highlights.lua`
- `get_capabilities()` memoised in `lsp.lua`
- `shared.treesitter()` helper — eliminates 15 duplicate treesitter extension blocks
- `blame.nvim` moved to `git.lua`; `<leader>xT` moved to `editor.lua`
- `sql.lua` deleted — `database.lua` owns all SQL config
- `jdtls` removed from `M.lspconfig`; managed by `nvim-jdtls` in `java.lua`
- `find_ancestor_with` removed; `path.find_root` used throughout `runner.lua`
