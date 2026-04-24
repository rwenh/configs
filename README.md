# nvim-ide

A modular Neovim IDE config — lazy-loaded, LSP-first, 20+ languages.

> Tested on **openSUSE Leap 16.0** · Neovim **0.11+** required · v2.3.15

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
    │   ├── bootstrap.lua       # lazy.nvim bootstrap (sole clone site), leader key, version stamp
    │   ├── options.lua         # vim options
    │   ├── keymaps.lua         # global keymaps — all lazy-requires via unified lazy() factory
    │   ├── autocmds.lua        # autocommands
    │   ├── commands.lua        # user commands (:MasonInstallAll etc.)
    │   ├── theme.lua           # theme management + dark/light toggle
    │   ├── focus.lua           # deep focus mode (strips chrome + Twilight + Zen)
    │   ├── hud.lua             # synthwave accent highlight overrides
    │   └── util/
    │       ├── path.lua        # project root detection + caching
    │       ├── runner.lua      # file/selection/test runner engine
    │       └── term.lua        # shared toggleterm launch helper (float / float_at_root)
    └── plugins/
        ├── init.lua            # lazy.nvim setup (rtp prepend + lazy.setup)
        └── specs/
            ├── ui.lua          # themes, statusline, bufferline, snacks dashboard
            ├── editor.lua      # telescope, tree, flash, harpoon, sessions
            ├── lsp.lua         # LSP, Mason, conform, nvim-lint
            ├── completion.lua  # blink.cmp
            ├── treesitter.lua  # treesitter + context + textobjects
            ├── git.lua         # gitsigns, lazygit, diffview, octo
            ├── dap.lua         # nvim-dap + UI + adapters (all languages except Python)
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
| Python | basedpyright | debugpy *(nvim-dap-python)* | black + isort | ruff | pytest |
| Rust | rust-analyzer *(rustaceanvim)* | codelldb | rustfmt | clippy | cargo |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | — | — | JUnit |
| Kotlin | kotlin_language_server | — | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls | elixir-ls | mix | — | ExUnit |
| C | clangd | codelldb | clang-format | clang-tidy | ctest |
| C++ | clangd + clangd_extensions | codelldb | clang-format | — | ctest |
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

## Known Issues / Pending Fixes (v2.3.15)

All previously listed known issues have been resolved. No open issues remain.

| Resolved in | Issue | Fix |
|-------------|-------|-----|
| v2.3.15 | `focus.lua` `apply_spec()` used a broken Lua `a and b or c` ternary for boolean restore; `number`, `relativenumber`, `cursorline` were forcibly re-enabled on focus exit if the user had them off | Replaced with explicit `if saved ~= nil then saved else default end` |
| v2.3.15 | `dap.lua` manually registered `dap.adapters.python` and `dap.configurations.python`; `python.lua`'s `nvim-dap-python.setup()` silently overwrote both on ft=python — dap.lua's Python section was dead code | Python DAP section removed from `dap.lua`; `python.lua` is the sole owner |
| v2.3.15 | `rust.lua` had no `conform` spec; `rustfmt` was entirely absent from the formatter pipeline despite being documented in the README | Added `optional=true` conform spec to `rust.lua` with `rust = { "rustfmt" }` |
| v2.3.15 | `python.lua` OPT comment claimed "no subprocess" but `vim.fn.system()` still spawned a process for site-packages discovery | Replaced with pure stat/glob checks; zero subprocess calls |
| v2.3.15 | `lsp.lua` registered `shellcheck` for `sh` unconditionally (outside the executable guard), then registered it again inside the guard — binary-absent systems got lint errors; the guarded `sh` entry was dead code | Moved `sh` inside the executable guard; deduplicated |
| v2.3.15 | `advanced.lua` neogen languages table claimed "ALL languages" but omitted `kotlin`; `<leader>xg` was a no-op on Kotlin files | Added `kotlin = { "kdoc" }` to the languages table and an `optional=true` ft trigger spec |
| v2.3.15 | `zig.lua` DAP FileType autocmd had `once=true` — the same pattern fixed for neotest-rust in v2.3.10; any non-zig FileType firing first discarded the registration | Removed `once=true`; augroup `clear=true` provides idempotency |
| v2.3.15 | `keymaps.lua` registered `<leader>xx` (Trouble) and `<leader>xu` (Undotree) despite both being owned by plugin `keys=` specs in `ui.lua` and `advanced.lua` | Both removed from `keymaps.lua`; plugin specs are sole owners |
| v2.3.15 | `commands.lua` `ToggleAutoformat` used a double-negated `disable_*` variable for its notification, printing "false" when disabling; `disable_autoformat` was also never initialised | Rewrote notification to print "enabled" / "disabled"; added initialisation guard |
| v2.3.15 | `commands.lua` `MasonInstallAll` missing `gofumpt`; `lsp.lua` conform wires `go = { "goimports", "gofumpt" }` but only `goimports` was listed | Added `"gofumpt"` to the Formatters section |
| v2.3.15 | `autocmds.lua` `TrimWhitespace` BufWritePre callback had no `buftype` guard, inconsistent with every other callback in the same file | Added `if vim.bo[e.buf].buftype ~= "" then return end` |
| v2.3.15 | `README.md` and `INSTALL.md` troubleshooting repo URL referenced `openSUSE_Leap_15.5` while the header states the config is tested on Leap 16.0 | Updated URL to `openSUSE_Leap_16.0` in both files |
| v2.3.14 | `runner.lua` launched terminals via three independent inline toggleterm blocks, bypassing `core.util.term` which was introduced specifically to centralise this pattern | Unified all terminal launches through `term.float()` / `term.float_at_root()` |
| v2.3.14 | `keymaps.lua` defined a hand-rolled `harpoon_call()` factory immediately after establishing the `lazy()` factory — two identical patterns in the same file | Removed `harpoon_call()`; Harpoon, Flash, and focus maps all use `lazy()` |
| v2.3.14 | `focus.lua` `enter()` and `exit()` each iterated the SPEC table independently with subtly different boolean-guard logic | Unified into a single `apply_spec(active)` function |
| v2.3.14 | `test.lua` `jest_cmd()` contained an inline lockfile-detection fallback that duplicated `runner.detect_js_test_cmd()` verbatim — dead code since v2.3.11 | Fallback removed; function delegates cleanly to `runner.detect_js_test_cmd()` |
| v2.3.14 | `lsp.lua` nvim-lint linter merge used a verbose hand-written nested deduplication loop | Replaced with `merge_linters(ft, linters)` helper |
| v2.3.14 | `lsp.lua` optional server table used a structurally different call shape from the primary `servers` table, making the two paths harder to compare | Call shapes unified |
| v2.3.14 | `python.lua` debugpy probe used recursive async `vim.system()` callbacks — unnecessary for path resolution, which requires no subprocess import | Replaced with synchronous checks |
| v2.3.14 | `bootstrap.lua` and `plugins/init.lua` both contained a lazy.nvim clone block, producing two divergent error messages and two code paths for the same failure | Duplicate removed from `plugins/init.lua`; `bootstrap.lua` is the sole clone site |
| v2.3.14 | `ui.lua` version fallback `or "2.3.5"` was dead code since v2.3.12 guaranteed the version is set before any plugin config runs | Replaced with `or "unknown"` to surface bootstrap failures visibly |

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
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```

---

## Updating

```vim
:Lazy update          " update plugins
:MasonUpdate          " update Mason registry
:TSUpdate             " update treesitter parsers
```
