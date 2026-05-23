# INSTALL — v2.4.0

Complete installation guide. For a quick-start see [README.md](README.md).

---

## Table of Contents

- [Prerequisites](#prerequisites)
- [System Packages](#system-packages)
- [Language Tooling](#language-tooling)
- [Fonts](#fonts)
- [Configuration](#configuration)
- [First Launch Checklist](#first-launch-checklist)
- [File Structure](#file-structure)
- [Load-order Constraints](#load-order-constraints)
- [Hot-Swap Reference](#hot-swap-reference)
- [Troubleshooting](#troubleshooting)
- [Changelog](#changelog)

---

## Prerequisites

All of these must be present before the first launch. The version column is
the minimum — newer is always fine.

| Tool | Min version | Check command | Notes |
|------|-------------|---------------|-------|
| Neovim | **0.11** | `nvim --version` | Uses `vim.lsp.config()` / `vim.lsp.enable()` |
| git | any | `git --version` | lazy.nvim bootstrap |
| gcc or clang | any | `gcc --version` | Treesitter parser compilation |
| node | **18** | `node --version` | LSP servers, JS debug adapter |
| npm | **8** | `npm --version` | Mason package installs |
| python3 | **3.9** | `python3 --version` | basedpyright, debugpy |
| pip | any | `pip3 --version` | Python tool installs |
| rust | stable | `rustc --version` | rust-analyzer, stylua, vhdl_ls |
| cargo | stable | `cargo --version` | Rust tool installs |
| ripgrep | any | `rg --version` | Telescope live grep |
| fd | any | `fd --version` | Telescope file finder |
| lazygit | any | `lazygit --version` | Git TUI (`<leader>.g`) |
| Nerd Font | **v3+** | visual check | Icons — see [Fonts](#fonts) |

---

## System Packages

### openSUSE Leap 16.0 (primary platform)

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

# fd is installed as fd-find on Debian/Ubuntu — create an alias:
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

# lazygit also available via AUR: yay -S lazygit
```

### macOS (Homebrew)

```bash
brew install neovim git node python3 rust go ripgrep fd lazygit \
  shellcheck ruby erlang elixir sqlite3

# For clipboard support:
# pbcopy/pbpaste are built-in on macOS — no additional package needed

# gnucobol:
brew install gnu-cobol

# ghdl:
brew install ghdl
```

> **Note on Go:** Ensure `$(go env GOPATH)/bin` is in your `PATH` or the
> Go tools installed below will not be found by Neovim.

---

## Language Tooling

Install these after system packages. Each section is independent — skip
sections for languages you will not use.

### Python

```bash
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv
```

| Package | Purpose |
|---------|---------|
| pynvim | Neovim Python provider |
| debugpy | Python DAP adapter |
| black | Formatter (primary) |
| isort | Import sorter (runs after black) |
| ruff | Linter |
| pytest | Test runner (neotest-python) |
| ipython | Enhanced REPL (iron.nvim prefers it) |
| virtualenv | Virtual environment management |

### Node / JavaScript / TypeScript

```bash
npm install -g typescript ts-node tsx prettier eslint_d neovim
```

| Package | Purpose |
|---------|---------|
| typescript | TypeScript compiler + tsserver |
| ts-node | TypeScript executor (fallback runner) |
| tsx | Modern TS/JSX executor (primary runner) |
| prettier | Formatter for JS/TS/HTML/CSS/JSON/YAML/MD |
| eslint_d | Fast ESLint daemon (linter) |
| neovim | Neovim Node.js provider |

### Ruby

```bash
gem install solargraph rubocop debug
```

| Gem | Purpose |
|-----|---------|
| solargraph | LSP server |
| rubocop | Formatter + linter |
| debug | DAP adapter (rdbg) |

### Rust tools

```bash
cargo install stylua vhdl_ls
```

| Crate | Purpose |
|-------|---------|
| stylua | Lua formatter |
| vhdl_ls | VHDL LSP (not in Mason registry) |

### Go tools

```bash
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest
```

| Tool | Purpose |
|------|---------|
| goimports | Formatter (manages imports) |
| gofumpt | Stricter gofmt (runs after goimports) |

### COBOL LSP — manual install required

The COBOL language server is **not in the Mason registry** and cannot be
installed via `:MasonInstallAll`. Install it manually:

```bash
npm install -g @broadcommfd/cobol-language-support
```

Verify: `cobol-language-server --version`

The server attaches automatically to `.cob`, `.cbl`, `.cpy`, `.COB`, `.CBL`
files if the binary is on `PATH`.

### VHDL LSP — cargo install

```bash
cargo install vhdl_ls
```

Verify: `vhdl_ls --version`

---

## Fonts

Any Nerd Font v3+ works. FiraCode Nerd Font is the recommended choice:

```bash
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip
rm FiraCode.zip
fc-cache -fv
```

Set `FiraCode Nerd Font Mono` (or `FiraCode Nerd Font`) in your terminal
emulator's font settings. The "Mono" variant is recommended for Neovim to
prevent icon width misalignment.

**Verify font is active:** Open Neovim and check that the statusline icons,
explorer folder icons, and diagnostic signs render as glyphs rather than boxes.
Running `:lua print(vim.g.nvim_ide_version)` should show the version with no
icon corruption in the notification popup.

---

## Configuration

```bash
# Clone the dotfiles repository
git clone https://github.com/rwenh/configs.git ~/dotfiles

# Back up any existing Neovim config
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)

# Back up any existing Neovim data (optional but recommended on first install)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)

# Symlink the config
ln -sf ~/dotfiles/nvim ~/.config/nvim
```

---

## First Launch Checklist

Run these steps in order after cloning. Each depends on the previous.

```vim
" Step 1 — Open Neovim (lazy.nvim auto-bootstraps and installs all plugins)
" Wait for the lazy.nvim UI to show 100% complete, then:

" Step 2 — Install LSP servers, DAP adapters, formatters, linters
:MasonInstallAll

" Step 3 — Install Treesitter parsers
:TSUpdate

" Step 4 — Verify the setup
:checkhealth

" Step 5 — Verify LSP for a specific language (optional)
" Open a Python file, then:
:checkhealth lsp
:LspInfo
```

Expected post-install state:

| Check | Expected |
|-------|----------|
| `:checkhealth` | No ERROR items; WARN items for unused languages are normal |
| `:Mason` | All listed packages show ✓ |
| `:TSUpdate` | All parsers show "installed" or "up to date" |
| Statusline | Shows branch, diagnostics, filetype icons |
| Dashboard | Logo + session quote shown in header on startup |
| `gd` on any symbol | LSP jumps to definition |

---

## File Structure

Version stamps show the last version each file was meaningfully changed.
Files marked `← NEW` were added in the current version.

```
~/.config/nvim/
├── init.lua                          ← v2.3.8
└── lua/
    ├── core/
    │   ├── autocmds.lua              ← v2.4.0
    │   ├── bootstrap.lua             ← v2.4.0
    │   ├── commands.lua              ← v2.4.0
    │   ├── focus.lua                 ← v2.4.0
    │   ├── highlights.lua            ← v2.4.0  ✦ renamed from hud.lua
    │   ├── keymaps.lua               ← v2.4.0  ✦ _family/_dap_maps indirection removed
    │   ├── options.lua               ← v2.3.9b
    │   ├── scanner.lua               ← DEPRECATED v2.4.1 (safe to delete)
    │   ├── theme.lua                 ← v2.4.0
    │   └── util/
    │       ├── buf_keymap.lua        ← v2.4.0
    │       ├── exec.lua              ← NEW      ✦ executable guard utility
    │       ├── icons.lua             ← v2.4.0
    │       ├── mason.lua             ← v2.4.0  ✦ bin_ok executable-only fix
    │       ├── packages.lua          ← v2.4.0  ✦ single source for Mason/lspconfig lists
    │       ├── path.lua              ← v2.4.0
    │       ├── quotes.lua            ← NEW      ✦ 36 curated quotes, 6 categories
    │       ├── runner.lua            ← v2.4.0  ✦ path module used; JSX/TSX fix pending
    │       ├── snippets.lua          ← NEW      ✦ LuaSnip snippet factory helpers
    │       └── term.lua              ← v2.4.0
    └── plugins/
        ├── init.lua                  ← v2.3.14
        └── specs/
            ├── advanced.lua          ← v2.4.0  ✦ nvim-ufo IIFE unfolded
            ├── completion.lua        ← v2.3.6
            ├── dap.lua               ← v2.4.0
            ├── editor.lua            ← v2.4.0  ✦ <leader>xT TodoTelescope key moved here
            ├── git.lua               ← v2.4.0  ✦ blame.nvim moved here
            ├── hud.lua               ← v2.4.0  ✦ restored; blame + TodoTelescope removed
            ├── lsp.lua               ← v2.4.0  ✦ get_capabilities memoized; packages.lua sourced
            ├── test.lua              ← v2.4.0  ✦ neotest internal API removed
            ├── treesitter.lua        ← v2.4.0
            ├── ui.lua                ← v2.4.1  ✦ build_header() inlines logo + quote; scanner removed
            ├── workflow.lua          ← v2.3.1
            └── lang/
                ├── shared.lua        ← v2.4.0  ✦ treesitter() helper; JS_TS_FT literal table
                ├── c.lua             ← v2.4.0  ✦ shared.treesitter()
                ├── cobol.lua         ← v2.4.0
                ├── cpp.lua           ← v2.4.0  ✦ shared.treesitter()
                ├── css.lua           ← v2.4.0  ✦ shared.treesitter()
                ├── database.lua      ← v2.4.0  ✦ shared.treesitter()
                ├── elixir.lua        ← v2.4.0  ✦ shared.treesitter()
                ├── fortran.lua       ← v2.4.0  ✦ shared.treesitter()
                ├── go.lua            ← v2.4.0  ✦ dead conform spec removed
                ├── html.lua          ← v2.4.0  ✦ shared.treesitter()
                ├── java.lua          ← v2.4.0  ✦ OS-aware config_dir
                ├── javascript.lua    ← v2.4.0  ✦ shared.treesitter()
                ├── kotlin.lua        ← v2.4.0  ✦ shared.treesitter()
                ├── markdown.lua      ← v2.2.3
                ├── python.lua        ← v2.4.0
                ├── rest.lua          ← v2.4.0  ✦ shared.treesitter()
                ├── ruby.lua          ← v2.4.0  ✦ shared.treesitter()
                ├── rust.lua          ← v2.4.0  ✦ detect_edition cached per Cargo.toml
                ├── typescript.lua    ← v2.4.0  ✦ shared.treesitter(); JS_TS_FT; deepcopy removed
                ├── vhdl.lua          ← v2.4.0  ✦ shared.treesitter()
                ├── web.lua           ← v2.3.1
                └── zig.lua           ← v2.4.0  ✦ shared.treesitter()
```

---

## Load-order Constraints

Enforced in `plugins/specs/init.lua`. **Do not reorder these.**

| Earlier | Later | Why |
|---------|-------|-----|
| `completion` | `lsp` | blink.cmp must register capabilities before LSP servers are set up |
| `lsp` | `lang/*` | lang specs extend `formatters_by_ft` and `linters_by_ft` tables |
| `web` | `html`, `css` | nvim-ts-autotag and emmet-vim must load before HTML/CSS configs run |
| `database` | — | owns all SQL configuration; the former `sql.lua` was deleted |

---

## Hot-Swap Reference

These modules can be reloaded without restarting Neovim. Plugin-backed
modules require a full restart.

### Pure Lua modules (safe to hot-reload)

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
:Lazy reload nvim-dap-python
:Lazy reload rustaceanvim
:Lazy reload neogen
:Lazy reload conform.nvim
:Lazy reload nvim-lint
```

### Requires full restart

| Module | Reason |
|--------|--------|
| `ui.lua` | snacks.nvim dashboard registers autocmds at startup |
| `bootstrap.lua` | leader keys must precede all plugin loading |
| `options.lua` | some options only take effect before plugins load |
| `treesitter.lua` | parser installation state is process-scoped |
| Any `theme_spec` | lazy.nvim theme priority/lazy flags are set at startup |

---

## Troubleshooting

### Diagnostic commands

```vim
:Health                " custom summary: version, LSP count, memory
:checkhealth           " full Neovim health check
:checkhealth lsp       " LSP client health
:LspInfo               " active clients for current buffer
:Mason                 " package status
:Lazy                  " plugin load status and timing
:lua print(vim.g.nvim_ide_version)   " confirm version stamp
```

### Issue reference

| Symptom | Root cause | Fix |
|---------|------------|-----|
| Highlight overrides not applied | `init.lua` calls `core.hud` (old name) | Change to `require("core.highlights").apply()` in root `init.lua` |
| `<leader>'r` fails on `.jsx`/`.tsx` | `javascriptreact`/`typescriptreact` not in `runners{}` | Pending fix — add entries to `runner.lua` |
| LSP `ensure_installed` drift from `packages.lua` | `lsp.lua` hardcodes list | Change to `require("core.util.packages").lspconfig` |
| Theme toggle broken after `M.switch()` | `manual_override` not cleared | Add `_cache.manual_override = nil` to `M.switch()` |
| Focus mode opens ZenMode unexpectedly on exit | `set_zen()` always toggles | Pending fix — track ZenMode state independently |
| Plugins not loading | Corrupt lazy state | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |
| COBOL LSP not attaching | Not in Mason registry | `npm i -g @broadcommfd/cobol-language-support` |
| VHDL LSP not attaching | Not in Mason registry | `cargo install vhdl_ls` |
| Dashboard quote not visible | `ui.lua` not updated | Ensure `build_header()` is present in `ui.lua` config; run `:Lazy reload snacks.nvim` |
| Treesitter highlighting broken | Parser version mismatch | `:TSUpdate` |
| DAP adapter not found | Mason package missing | `:MasonInstallAll` |
| blink.cmp not completing | Version pin issue | `:Lazy update` · verify `version = "1.*"` in `completion.lua` |
| `gd` does nothing | LSP not attached | `:LspInfo` in buffer · `:checkhealth lsp` |
| Kotlin tests not running | Missing gradlew executable bit | `chmod +x gradlew` |
| Large file is slow | Expected — auto-protection kicks in at 500KB | Disable with `vim.b[bufnr].large_file = false` if needed |

### Neovim too old

```bash
# openSUSE Leap 16.0 — add editors repository
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ \
  editors
sudo zypper ref
sudo zypper in neovim
```

### Nuclear reset (clears all state)

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim
nvim
# :MasonInstallAll
# :TSUpdate
# :checkhealth
```

---

## Changelog

### v2.4.0

**Bug fixes**
- `java.lua` — OS-aware jdtls config directory (Linux / macOS / Windows)
- `runner.lua` — JSX/TSX tmpfile now uses correct extension (`.js` / `.ts`)
- `lsp.lua` — eslint_d double-registration via new `merge_linters()` helper
- `specs/hud.lua` — restored (was silently missing from specs/init.lua import list)

**Architecture**
- `core/hud.lua` → `core/highlights.lua` — clearer naming, same content
- `packages.lua` created as single source of truth for all Mason/lspconfig names
- `commands.lua` and `lsp.lua` both source from `packages.lua`
- `blame.nvim` moved from `hud.lua` to `git.lua` — semantic correctness
- `<leader>xT` (TodoTelescope) moved to `editor.lua`
- `sql.lua` deleted — `database.lua` is the SQL config owner

**Boilerplate reduction**
- `shared.treesitter()` helper eliminates 15 identical treesitter extension blocks
- 7 lang files: removed unnecessary `toggleterm optional=true`
- `typescript.lua` — pointless deepcopy removed
- `shared.lua` — `JS_TS_FT` literal table, replaces inline repetition

**Performance**
- `lsp.lua` — `get_capabilities()` memoised; called once per session
- `ui.lua` — active theme cached at module load (was recalculated on every render)
- `rust.lua` — `detect_edition()` result cached per `Cargo.toml` path
- `autocmds.lua` — dead pcall on `tabpagenr` removed
- `mason.lua` — `bin_ok` uses `executable()` only (not `filereadable()`)

**Dead code removal**
- `go.lua` — commented-out dead conform spec removed
- `advanced.lua` — nvim-ufo IIFE unfolded to readable setup
- `keymaps.lua` — `_family`/`_dap_maps`/`_fkey_maps` indirection removed
- `runner.lua` — `find_ancestor_with` removed; `path.find_root` used throughout

**Hardening**
- `test.lua` — internal neotest API call removed; opts snapshot as stable fallback

---

### v2.3.16

`path.lua` headless pcall · `runner.lua` selection mark clamp + executable
guards + non-blocking VHDL run · `term.lua` nil-root guard · `java.lua`
sha256 workspace hash · `test.lua` neotest-rust live adapter deferred state ·
`dap.lua` codelldb existence check · `autocmds.lua` RestoreCursor targets
correct window · `kotlin.lua` gradlew executable bit · `bootstrap.lua` partial
clone cleanup · `lsp.lua` double-pcall blink caps + format_on_save buffer guard ·
`focus.lua` VimLeavePre restore · `theme.lua` ColorScheme cache sync ·
`commands.lua` MasonInstallAll mutex · `python.lua` DAP keymap guard ·
`keymaps.lua` ww/wq pcall guards · `ui.lua` open_win pcall + timer idle bail ·
`treesitter.lua` fs_stat per-buffer cache

### v2.3.15

`focus.lua` boolean restore bug · `dap.lua` dead Python section ·
`rust.lua` rustfmt conform wiring · `python.lua` subprocess-free debugpy probe ·
`lsp.lua` shellcheck guard · `advanced.lua` kotlin neogen annotation ·
`zig.lua` once=true removed · `keymaps.lua` duplicate xx/xu removed ·
`commands.lua` ToggleAutoformat + gofumpt support ·
`autocmds.lua` TrimWhitespace buftype guard

### v2.3.0 — v2.3.14

snacks.nvim dashboard · matrix rain engine · blink.cmp navigation keys ·
nvim-0.11 double-attach prevention · `diagnostic.jump` API ·
iron REPL scoped keymaps · neotest-go constructor · TrimWhitespace batch ·
native foldexpr · various runner and test adapter fixes

### v2.2 — v2.1

octo.nvim integration · cssmodules LSP guard · blink.cmp version pin ·
rest.nvim v3 API · java workspace sha256 · elixir-tools disabled (replaced by elixir-ls)

### v2.0

Initial release
