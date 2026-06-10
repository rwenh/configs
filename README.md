# nvim-ide

> Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages.

**Neovim 0.11+** · openSUSE Leap 16.0 · **v2.4.1**

---

## Contents

- [Requirements](#requirements)
- [Quick Install](#quick-install)
- [Structure](#structure)
- [Architecture](#architecture)
- [Escape Hatches](#escape-hatches)
- [Languages](#languages)
- [Plugins](#plugins)
- [Key Bindings](#key-bindings)
- [Themes](#themes)
- [Deep Focus Mode](#deep-focus-mode)
- [Updating](#updating)
- [Troubleshooting](#troubleshooting)

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
| `lazygit` | any | LazyGit TUI |
| [Nerd Font](https://www.nerdfonts.com/) | v3+ | Icons throughout UI |

### Soft requirements (per language)

| Language | Additional tooling |
|----------|--------------------|
| Go | `go >= 1.21`, `goimports`, `gofumpt` |
| Java | `java >= 17` (jdtls) |
| Ruby | `ruby >= 3.0`, `gem install solargraph rubocop debug` |
| Elixir | `elixir`, `mix` |
| Fortran | `gfortran`, `fprettify` |
| COBOL | `gnucobol`; LSP manual install — see [INSTALL.md](INSTALL.md) |
| VHDL | `ghdl`, `gtkwave`; `cargo install vhdl_ls`; `pip install vsg` |
| REST | `curl`, optionally `jq` |
| SQL | `sqlfmt` (formatter, optional) |
| tmux integration | **optional** — `tmux >= 3.2`; enables seamless `Ctrl+h/j/k/l` pane navigation via `vim-tmux-navigator` |

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
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv vsg
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
    │   ├── keymaps.lua
    │   ├── autocmds.lua
    │   ├── commands.lua          ← :MasonInstallAll, :Format, …
    │   ├── theme.lua
    │   ├── focus.lua
    │   ├── highlights.lua
    │   └── util/
    │       ├── path.lua          ← project root detection (TTL cache)
    │       ├── runner.lua        ← file / selection / test runner (20+ filetypes)
    │       ├── term.lua          ← toggleterm launch helper
    │       ├── buf_keymap.lua    ← buffer-local keymap registration + dedup
    │       ├── mason.lua         ← Mason binary/path helpers
    │       ├── packages.lua      ← Mason + lspconfig lists (single source of truth)
    │       ├── icons.lua
    │       ├── exec.lua          ← executable guard helper
    │       ├── snippets.lua      ← LuaSnip snippet factory helpers
    │       └── quotes.lua        ← dashboard quotes
    └── plugins/specs/
        ├── ui.lua
        ├── editor.lua
        ├── lsp.lua
        ├── completion.lua
        ├── treesitter.lua
        ├── git.lua
        ├── dap.lua
        ├── test.lua
        ├── advanced.lua
        ├── hud.lua
        ├── workflow.lua
        └── lang/                 ← one file per language
```

### Load-order constraints

| Earlier | Later | Reason |
|---------|-------|--------|
| `completion` | `lsp` | blink.cmp capabilities injected before LSP servers start |
| `lsp` | `lang/*` | lang specs extend formatter and linter tables |
| `web` | `html`, `css` | autotag + emmet must be present when html/css configs run |

---

## Architecture

**Single source of truth** — `packages.lua` owns all Mason/lspconfig package names. Both `:MasonInstallAll` and `mason-lspconfig.ensure_installed` read from it.

**Capability memoisation** — `get_capabilities()` in `lsp.lua` is called once and cached, preventing a deep-copy per server on startup.

**Project root detection** — `path.find_root()` walks upward through standard root markers with a configurable TTL cache, invalidated on `DirChanged`.

**Buffer-local keymap deduplication** — `bkm.batch(buf, maps, flag)` stores a boolean in `vim.b[buf][flag]`, making repeated calls no-ops.

**Deferred DAP** — adapters register inside `FileType once=true` autocmds. Zero DAP cost unless a relevant file is opened.

**Executable guards** — `exec.require_bin(name, hint)` centralises all `vim.fn.executable()` checks across the codebase.

**Large-file protection** — files over 500 KB have treesitter, folding, syntax highlighting, cursorline, and spellcheck automatically disabled.

**Theme-aware highlights** — `highlights.lua` applies full TokyoNight overrides when active and a minimal universal set for all other themes.

**Atomic Mason installs** — `:MasonInstallAll` uses an atomic pending counter; timeout configurable via `vim.g.mason_install_timeout_ms`.

**Per-project config files** — `.lspconfig.lua`, `.nvim-dap.lua`, `.overseer.lua` at the project root are loaded automatically and override built-in defaults.

---

## Escape Hatches

Set in `init.lua` **before** `require("core.bootstrap")`:

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_highlight_overrides` | `false` | Skip all highlight overrides |
| `vim.g.disable_tint` | `false` | Disable tint.nvim inactive-window dimming |
| `vim.g.disable_smear_cursor` | `false` | Disable smear-cursor motion trail |
| `vim.g.disable_vsg_format` | `false` | Disable vsg VHDL formatter |
| `vim.g.mason_install_timeout_ms` | `120000` | Per-install timeout for `:MasonInstallAll` |
| `vim.g.debugpy_python` | `nil` | Pin Python interpreter for debugpy |
| `vim.g.path_max_walk_depth` | `20` | Upward-walk limit for root detection |
| `vim.g.path_cache_ttl` | `30` | Root-cache lifetime in seconds |
| `vim.g.path_debug` | `false` | Log silent cwd fallbacks at DEBUG level |

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
| VHDL | vhdl_ls² | — | vsg³ | — | ghdl |

> ¹ Not in Mason registry: `npm i -g @broadcommfd/cobol-language-support`  
> ² Not in Mason registry: `cargo install vhdl_ls`  
> ³ Formats via `--stdin` so buffer content (not on-disk file) is formatted

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
| vim-tmux-navigator | Seamless `Ctrl+h/j/k/l` across Neovim splits and tmux panes (transparent outside tmux) |
| harpoon2 | File bookmarks |
| mini.files | Lightweight file manager |
| mini.visits | Frecency-based file switching |
| persistence.nvim | Session management |
| nvim-spectre | Project search/replace |
| gitsigns.nvim | Inline hunk signs + staging |
| lazygit.nvim | Full-screen Git TUI |
| diffview.nvim | Side-by-side diff + history |
| neogit | Magit-style commit/rebase UI |
| git-conflict.nvim | 3-way conflict resolution |
| git-worktree.nvim | Worktree management |
| octo.nvim | GitHub PR/issue workflow |
| overseer.nvim | Task runner |

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
| nvim-scrollview | Scrollbar with diagnostic signs |
| nvim-ufo | Fold enhancement with previews |
| zen-mode.nvim | Distraction-free writing |
| twilight.nvim | Dim inactive code |
| smear-cursor.nvim | Cursor motion trail (opt-out: `vim.g.disable_smear_cursor`) |
| tint.nvim | Inactive window dimming (opt-out: `vim.g.disable_tint`) |

---

## Key Bindings

Leader: `Space` · Full reference: [KEYMAP_REFERENCE.md](KEYMAP_REFERENCE.md)

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
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

Highlight overrides apply the full TokyoNight accent palette when TokyoNight is active; other themes receive a minimal set (DAP signs only). Suppress with `vim.g.disable_highlight_overrides = true`.

---

## Deep Focus Mode

`<leader>uF` hides the statusline, bufferline, signcolumn, ruler, and line numbers, then enables Twilight and ZenMode. All options are snapshotted and restored exactly on exit, including on `VimLeavePre`.

- `<leader>uz` — ZenMode only
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
| TypeScript LSP double-attaching | Ensure typescript-tools.nvim installed; `ts_ls` fallback only fires when absent |
| `<leader>ts*` missing in JS/JSX | `:Lazy reload typescript-tools.nvim` |
| VHDL format applies stale on-disk content | Confirm vsg formatter uses `--stdin` flag |
| Iron send-motion wrong range | ModeChanged operatorfunc guard handles this — check `python.lua` |
| `<Esc>` exits terminal before nested TUI | By design; see keymaps.lua terminal note |
| `Ctrl+h/j/k/l` doesn't cross into tmux pane | `is_vim` check missing from `.tmux.conf`; ensure the `bind -n C-h if-shell "$is_vim"` block is present |
| tint / smear-cursor artefacts | `vim.g.disable_tint = true` / `vim.g.disable_smear_cursor = true` |
| Highlight colours wrong on non-TN theme | `vim.g.disable_highlight_overrides = true` |
| `:MasonInstallAll` times out | `vim.g.mason_install_timeout_ms = 240000` |
| Wrong project root detected | `vim.g.path_debug = true` · check DEBUG notifications |
| debugpy not found in venv | `vim.g.debugpy_python = "/path/to/python"` |
| Large file very slow | Expected — treesitter/folding auto-disabled above 500 KB |
| Kotlin gradlew fails | `chmod +x gradlew` in project root |
| Dashboard blank after config change | Full restart required |
| Java workspace stale | Delete `~/.local/share/nvim/jdtls-workspace/` and restart |
| Plugins not loading at all | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |

**Neovim too old (openSUSE):**
```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```
