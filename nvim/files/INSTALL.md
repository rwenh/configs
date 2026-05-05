# INSTALL — v2.3.16

## Prerequisites

| Tool | Min version | Check |
|------|------------|-------|
| Neovim | 0.11 | `nvim --version` |
| git | any | `git --version` |
| node + npm | 18 | `node --version` |
| python3 + pip | 3.9 | `python3 --version` |
| rust + cargo | stable | `rustc --version` |
| ripgrep | any | `rg --version` |
| fd | any | `fd --version` |
| lazygit | any | `lazygit --version` |

---

## Installation

```bash
# System packages (openSUSE Leap 16.0)
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip \
  neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck \
  ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard

# Python
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv

# Node
npm install -g typescript ts-node tsx prettier eslint_d neovim

# Ruby
gem install solargraph rubocop debug

# Rust tools
cargo install stylua vhdl_ls

# Go tools
go install golang.org/x/tools/cmd/goimports@latest
go install mvdan.cc/gofumpt@latest

# Nerd Font
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv

# Config
git clone https://github.com/rwenh/configs.git ~/dotfiles
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)
ln -sf ~/dotfiles/nvim ~/.config/nvim

# Launch
nvim
:MasonInstallAll
:checkhealth
```

---

## File Structure

```
~/.config/nvim/
├── init.lua                              ← v2.3.8
└── lua/
    ├── core/
    │   ├── autocmds.lua                  ← v2.3.16
    │   ├── bootstrap.lua                 ← v2.3.16
    │   ├── commands.lua                  ← v2.3.16
    │   ├── focus.lua                     ← v2.3.16
    │   ├── highlights.lua                ← v2.4.0   ✦ renamed from hud.lua
    │   ├── keymaps.lua                   ← v2.4.0   ✦ _family/_dap_maps indirection removed
    │   ├── options.lua                   ← v2.3.9b
    │   ├── rain.lua                      ← v2.3.16
    │   ├── theme.lua                     ← v2.3.16
    │   └── util/
    │       ├── buf_keymap.lua            ← v2.3.16
    │       ├── icons.lua                 ← v2.3.16
    │       ├── mason.lua                 ← v2.4.0   ✦ bin_ok executable-only fix
    │       ├── packages.lua              ← v2.4.0   ✦ new — single source for Mason/lspconfig lists
    │       ├── path.lua                  ← v2.3.16
    │       ├── runner.lua                ← v2.4.0   ✦ find_ancestor_with removed; path module used
    │       └── term.lua                  ← v2.3.16
    └── plugins/
        ├── init.lua                      ← v2.3.14
        └── specs/
            ├── init.lua                  ← v2.1  (load-order sensitive)
            ├── advanced.lua              ← v2.4.0   ✦ nvim-ufo IIFE unfolded
            ├── completion.lua            ← v2.3.6
            ├── dap.lua                   ← v2.3.16
            ├── editor.lua                ← v2.4.0   ✦ <leader>xT TodoTelescope key moved here
            ├── git.lua                   ← v2.4.0   ✦ blame.nvim moved here; architecture comment
            ├── hud.lua                   ← v2.4.0   ✦ restored; blame + TodoTelescope removed
            ├── lsp.lua                   ← v2.4.0   ✦ get_capabilities memoized; eslint_d fix; packages.lua source
            ├── test.lua                  ← v2.4.0   ✦ neotest internal API removed; stable fallback
            ├── treesitter.lua            ← v2.3.16
            ├── ui.lua                    ← v2.4.0   ✦ get_active cached at module load
            ├── workflow.lua              ← v2.3.1
            └── lang/
                ├── shared.lua            ← v2.4.0   ✦ treesitter() helper; JS_TS_FT literal table
                ├── c.lua                 ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
                ├── cobol.lua             ← v2.4.0   ✦ toggleterm optional removed
                ├── cpp.lua               ← v2.4.0   ✦ shared.treesitter()
                ├── css.lua               ← v2.4.0   ✦ shared.treesitter()
                ├── database.lua          ← v2.4.0   ✦ shared.treesitter()
                ├── elixir.lua            ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
                ├── fortran.lua           ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
                ├── go.lua                ← v2.4.0   ✦ dead conform spec removed; shared.treesitter()
                ├── html.lua              ← v2.4.0   ✦ shared.treesitter()
                ├── java.lua              ← v2.4.0   ✦ config_linux → OS-aware config_dir
                ├── javascript.lua        ← v2.4.0   ✦ shared.treesitter()
                ├── kotlin.lua            ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
                ├── markdown.lua          ← v2.2.3
                ├── python.lua            ← v2.3.16
                ├── rest.lua              ← v2.4.0   ✦ shared.treesitter()
                ├── ruby.lua              ← v2.4.0   ✦ shared.treesitter()
                ├── rust.lua              ← v2.4.0   ✦ detect_edition cached per Cargo.toml path
                ├── typescript.lua        ← v2.4.0   ✦ shared.treesitter(); JS_TS_FT; deepcopy removed
                ├── vhdl.lua              ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
                ├── web.lua               ← v2.3.1
                └── zig.lua               ← v2.4.0   ✦ shared.treesitter(); toggleterm optional removed
```

> **Load-order constraint in `specs/init.lua`:**
> `completion` before `lsp` · `web` before `html`/`css` · `database` owns SQL config (sql.lua deleted)

---

## Hot-Swap (no restart required)

```vim
" Pure Lua modules — no plugin state
:luafile ~/.config/nvim/lua/core/keymaps.lua
:luafile ~/.config/nvim/lua/core/commands.lua
:luafile ~/.config/nvim/lua/core/focus.lua
:luafile ~/.config/nvim/lua/core/autocmds.lua

" Plugin reloads
:Lazy reload nvim-lspconfig
:Lazy reload neotest
:Lazy reload nvim-dap
:Lazy reload rustaceanvim
:Lazy reload nvim-dap-python
:Lazy reload nvim-dap
:Lazy reload neogen
```

> `ui.lua` (snacks dashboard) requires a full restart.

---

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| Plugins not loading | `rm -rf ~/.local/share/nvim ~/.cache/nvim` then `nvim` |
| LSP not attaching | `:checkhealth lsp` · `:LspInfo` |
| Mason package failed | `:MasonInstallAll` · `:checkhealth mason` |
| Dashboard rain not animating | `:lua Snacks.dashboard.open()` |
| Neovim too old | Add editors repo (see below) |

```bash
# Neovim too old — openSUSE Leap 16.0
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```

---

## Changelog

| Version | Notable changes |
|---------|----------------|
| v2.4.0 | **Bugs:** `java.lua` OS-aware jdtls config dir · `runner.lua` JSX/TSX tmpfile extension fix · `lsp.lua` eslint_d double-registration · `specs/hud.lua` restored (was missing). **Architecture:** `core/hud.lua` → `highlights.lua` · `packages.lua` new single source of truth for Mason/lspconfig lists · `commands.lua` + `lsp.lua` source from it · `blame.nvim` → `git.lua` · `<leader>xT` → `editor.lua` · `sql.lua` deleted. **Boilerplate:** `shared.treesitter()` helper eliminates 15× identical treesitter extension blocks · 7× lang toggleterm `optional=true` removed · `typescript.lua` deepcopy removed · `shared.lua` `JS_TS_FT` literal table. **Perf:** `lsp.lua` `get_capabilities()` memoized · `ui.lua` `get_active()` cached at module load · `rust.lua` `detect_edition()` cached per Cargo.toml · `autocmds.lua` dead pcall on tabpagenr removed · `mason.lua` `bin_ok` executable-only. **Dead weight:** `go.lua` dead commented spec removed · `advanced.lua` nvim-ufo IIFE unfolded · `keymaps.lua` `_family`/`_dap_maps`/`_fkey_maps` indirection removed · `runner.lua` `find_ancestor_with` consolidated into `path.find_root`. **Hardening:** `test.lua` internal neotest API call removed; stable `opts` snapshot fallback |
| v2.3.16 | `path.lua` headless pcall · `runner.lua` selection clamp + executable guards + non-blocking VHDL · `term.lua` nil-root guard · `java.lua` sha256 workspace hash · `test.lua` neotest-rust live adapter state · `dap.lua` codelldb existence check · `autocmds.lua` RestoreCursor correct window · `kotlin.lua` gradlew executable bit · `bootstrap.lua` partial clone cleanup · `lsp.lua` double-pcall blink caps + format_on_save buffer guard · `focus.lua` VimLeavePre restore · `theme.lua` ColorScheme cache sync · `commands.lua` MasonInstallAll mutex · `python.lua` DAP keymap guard · `keymaps.lua` ww/wq pcall · `ui.lua` open_win pcall + timer idle bail · `treesitter.lua` fs_stat per-buffer cache |
| v2.3.15 | `focus.lua` boolean restore · `dap.lua` dead Python section removed · `rust.lua` rustfmt conform · `python.lua` subprocess-free probe · `lsp.lua` shellcheck guard · `advanced.lua` kotlin neogen · `zig.lua` once=true removed · `keymaps.lua` duplicate xx/xu removed · `commands.lua` ToggleAutoformat + gofumpt · `autocmds.lua` TrimWhitespace buftype guard |
| v2.3.14 | `runner.lua` unified through term · `keymaps.lua` harpoon_call removed · `focus.lua` apply_spec unified · `test.lua` jest_cmd cleaned · `lsp.lua` merge_linters helper · `python.lua` sync probe · `bootstrap.lua` deduped · `ui.lua` version fallback |
| v2.3.10 | neotest-rust once=true · vim-matchup ts.setup · elixir-ls DAP ensure_installed · fortran/vhdl/cobol informational messages |
| v2.3.9 | runner.lua JS/TS cd prefix · commands.lua fortls + gopls · dap.lua Elixir resolver · lsp.lua fortls ensure_installed |
| v2.3.8 | neotest-vitest constructor · runner.lua cd prefix for python/rust/go/zig |
| v2.3.7 | lsp.lua elixir-ls wired · specs/hud.lua mini.animate opts→config |
| v2.3.6 | harpoon/todo-comments pcall · blink.cmp "show" removed · debugpy package name |
| v2.3.5 | spectre/DAP pcall guards · sm native toggle · conform v6 API · neotest-elixir constructor · drain flash fix |
| v2.3.4–v2.3.1 | blink.cmp nav keys · nvim-0.11 double-attach · diagnostic.jump · iron REPL scoped · neotest-go constructor · TrimWhitespace batch · foldexpr native |
| v2.3.0 | snacks.nvim dashboard · matrix rain engine |
| v2.2–v2.1 | octo.nvim · cssmodules guard · blink.cmp version pin · rest.nvim v3 · java workspace sha · elixir-tools disabled |
| v2.0 | Initial release |
