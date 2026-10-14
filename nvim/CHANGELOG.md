# nvim-ide — setup, plugins & changelog

Neovim 0.11+ · v2.6.1 · See [README.md](README.md) for the overview and [KEYMAPS.md](KEYMAPS.md) for every key.

## Contents

- [Setup](#setup) · [Escape hatches](#escape-hatches) · [Plugins by phase](#plugins-by-phase) · [Hot-swap](#hot-swap) · [Troubleshooting](#troubleshooting) · [Changelog](#changelog)

---

## Setup

### 1. Requirements

| Tool | Min | Purpose |
|------|-----|---------|
| `neovim` | **0.11** | `vim.lsp.config()` / `vim.lsp.enable()` |
| `git`, `gcc` or `clang`, `make` | any | plugin bootstrap, treesitter parsers, telescope-fzf |
| `node` + `npm` | **18** / 8 | LSP servers, formatters, JS debug adapter |
| `python3` + `pip` | **3.9** | basedpyright, debugpy |
| `rust` + `cargo` | stable | rust-analyzer, stylua, vhdl_ls |
| `ripgrep`, `fd`, `lazygit` | any | Telescope, LazyGit |
| `tree-sitter` CLI | any | auto-installs missing parsers; kulala.nvim builds its parser with it |
| Nerd Font | **v3+** | icons (FiraCode Nerd Font Mono recommended) |

Per language (only if you use it): Go ≥ 1.21 + `goimports` + `gofumpt` · Java ≥ 17 · Ruby ≥ 3.0 (`solargraph rubocop debug`) ·
Elixir + `mix` · `gfortran` + `fprettify` · `gnucobol` · `ghdl` + `gtkwave` + `vhdl_ls` + `vsg` · `zig` · `curl` (REST) ·
`deno` (Markdown browser preview) · `tmux ≥ 3.2` (optional pane navigation).

### 2. System packages

```bash
# openSUSE Leap 16.0
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip neovim nodejs npm python3 python3-pip python3-devel \
  rust cargo go ripgrep fd lazygit tree-sitter ShellCheck ruby ruby-devel erlang elixir gnucobol ghdl gtkwave \
  sqlite3 xclip xsel wl-clipboard

# Ubuntu / Debian (Neovim >= 0.11 needed: use the unstable PPA or a release tarball)
sudo apt update && sudo apt install -y gcc g++ make cmake ninja-build git curl wget unzip neovim nodejs npm python3 \
  python3-pip python3-dev rustc cargo golang ripgrep fd-find shellcheck ruby ruby-dev erlang elixir gnucobol ghdl \
  gtkwave sqlite3 xclip wl-clipboard && ln -s "$(which fdfind)" ~/.local/bin/fd

# Arch
sudo pacman -S gcc make cmake ninja git curl wget unzip neovim nodejs npm python python-pip rust cargo go ripgrep fd \
  shellcheck ruby erlang elixir gnucobol ghdl gtkwave sqlite xclip wl-clipboard lazygit

# macOS
brew install neovim git node python3 rust go ripgrep fd lazygit shellcheck ruby erlang elixir sqlite3 gnu-cobol ghdl
```

### 3. Language tooling

```bash
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv vsg
npm install -g typescript ts-node tsx prettier eslint_d neovim tree-sitter-cli
gem install solargraph rubocop debug
cargo install stylua vhdl_ls
go install golang.org/x/tools/cmd/goimports@latest && go install mvdan.cc/gofumpt@latest   # both needed
```

COBOL has no standalone language server available (the `@broadcommfd/cobol-language-support` package does not exist);
compile/run with `<leader>cob` works through `cobc`. If a `cobol-language-server` binary is on `PATH` it is used.

### 4. Font

```bash
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip && rm FiraCode.zip && fc-cache -fv        # then pick "FiraCode Nerd Font Mono" in your terminal
```

### 5. Install the config

```bash
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.bak.$(date +%Y%m%d)
[ -d ~/.local/share/nvim ] && mv ~/.local/share/nvim ~/.local/share/nvim.bak.$(date +%Y%m%d)
git clone https://github.com/rwenh/configs.git ~/dotfiles && ln -sf ~/dotfiles/nvim ~/.config/nvim
```

Upgrading from an older version: delete `lua/plugins/init.lua` (replaced by `lua/core/lazy.lua`).

### 6. First launch

```vim
nvim                " lazy.nvim bootstraps and installs every plugin; wait for 100%
:MasonInstallAll    " LSP servers, DAP adapters, formatters, linters  (':MasonInstallAll dry-run' previews)
:TSUpdate           " treesitter parsers
:checkhealth core   " expect no ERROR items
:Lazy lock          " pin plugin commits -> commit lazy-lock.json
```

Slow network: `vim.g.mason_install_timeout_ms = 300000` in `init.lua` before the first launch.

### 7. Verify

```bash
bash tests/run.sh     # isolated sandbox: spec lint, module load, units, behaviour, smoke, keymaps, docs-not-stale
```

### Lockfile

`lazy-lock.json` pins every plugin commit. After `:Lazy update` run `:Lazy lock` and commit it; on a new machine run
`:Lazy restore`; `:Lazy check` previews updates. Without a committed lockfile `:Lazy update` can introduce breaking changes.

---

## Escape hatches

<!-- BEGIN GENERATED:flags -->
54 flags — set at the top of `init.lua`, before `require("core.bootstrap")`.

**Core display & behaviour**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.disable_highlight_overrides` | boolean | `false` | Skip all highlight overrides |
| `vim.g.disable_tint` | boolean | `false` | Disable tint.nvim inactive-window dimming |
| `vim.g.disable_smear_cursor` | boolean | `false` | Disable smear-cursor (re-enables mini.animate's cursor animation) |
| `vim.g.disable_mini_pairs` | boolean | `false` | Disable mini.pairs auto-close brackets |
| `vim.g.disable_autoformat` | boolean | `false` | Disable format-on-save globally (`:ToggleAutoformat` flips it live) |
| `vim.g.disable_treesitter_folds` | boolean | `false` | Use indent folding instead of treesitter |
| `vim.g.disable_trim_whitespace` | boolean | `false` | Don't trim trailing whitespace on save (per buffer: `vim.b.disable_trim_whitespace`) |
| `vim.g.disable_theme_autoswitch` | boolean | `false` | Don't flip dark/light at `day_start`/`day_end` (startup choice only) |
| `vim.g.theme_option_patches` | table | `nil` | Per-theme option patches, e.g. `{ tokyonight = { background = "dark" } }` |
| `vim.g.icon_overrides` | table | `nil` | Override icon groups, e.g. `{ kinds = { Function = "ƒ" } }` |
| `vim.g.auto_cd_root` | boolean | `false` | Auto-cd to project root on `BufEnter` |
| `vim.g.runner_autosave` | boolean | `true` | Auto-save before running a file with the runner |
| `vim.g.disable_project_configs` | boolean | `false` | Never load per-project Lua files (`.lspconfig.lua`, `.runner.lua`, `.nvim-dap.lua`, `.overseer.lua`); otherwise they load only after you `:trust` them |
| `vim.g.spell_wordlist` | string | `nil` | Path to a personal spell file |
| `vim.g.filetype_options` | table | `nil` | Per-filetype option overrides, e.g. `{ rust = { colorcolumn = "100" } }` |

**LSP & formatting**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.format_timeout_ms` | number | `3000` | Global format timeout (ms) |
| `vim.g.format_timeout_by_ft` | table | `{}` | Per-filetype timeout, e.g. `{ python = 5000, cpp = 4000 }` |
| `vim.g.enable_pylint` | boolean | `false` | Opt-in pylint alongside ruff |
| `vim.g.lsp_on_attach_overrides` | table | `nil` | Per-server on_attach overrides, e.g. `{ lua_ls = function(c, b) end }` |
| `vim.g.completion_sources_by_ft` | table | `nil` | Per-filetype blink.cmp sources, e.g. `{ sql = { "lsp", "dadbod", "buffer" } }` (SQL defaults to dadbod) |
| `vim.g.debugpy_python` | string | `nil` | Pin the Python interpreter used by debugpy |
| `vim.g.ts_import_preference` | string | `"non-relative"` | TypeScript import module-specifier preference |

**DAP & testing**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.dap_bp_autosave_ms` | number | `60000` | DAP breakpoint autosave interval (ms); `0` disables |
| `vim.g.neotest_concurrency` | number | `4` | Parallel test-suite concurrency (`<leader>'P`) |

**Task runner & workflow**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.mason_install_timeout_ms` | number | `120000` | Per-install timeout for `:MasonInstallAll` (ms) |
| `vim.g.mason_extras` | table | `nil` | Extra Mason packages appended to the extras list |
| `vim.g.workflow_template_debounce_ms` | number | `300` | Overseer `.overseer.lua` load debounce on `BufEnter` (ms) |
| `vim.g.overseer_auto_scroll` | boolean | `true` | Auto-scroll Overseer task output |
| `vim.g.term_max_runs` | number | `3` | Finished one-shot run terminals kept before older ones are shut down |
| `vim.g.terminal_tui_programs` | table | `nil` | Extra full-screen programs that keep their own `<Esc>` (lazygit, htop, yazi… are built in) |

**Project root & paths**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.path_max_walk_depth` | number | `20` | Upward-walk limit for root detection |
| `vim.g.path_cache_ttl` | number | `30` | Root-cache lifetime (seconds) |
| `vim.g.path_debug` | boolean | `false` | Log cwd fallbacks at DEBUG |
| `vim.g.path_prefer_vcs` | boolean | `false` | Prefer the `.git`/`.hg`/`.svn` root over a nearer `Makefile`/`package.json` |
| `vim.g.path_ignore_dirs` | table | `nil` | Extra directory names (exact match) to skip during the root walk |
| `vim.g.cmake_build_dir` | string | `"build"` | CMake build directory name (C/C++/CTest) |
| `vim.g.lazy_cache_path` | string | `nil` | Custom path for the offline lazy.nvim cache |

**UI extras**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.symbol_usage_max_lines` | number | `2000` | Disable symbol-usage.nvim above this line count |
| `vim.g.force_mini_animate_scroll` | boolean | `false` | Force mini.animate scroll even when neoscroll is active |
| `vim.g.octo_timeout_ms` | number | `10000` | Octo.nvim request timeout (ms) |

**Language-specific**

| Flag | Type | Default | Effect |
|------|------|---------|--------|
| `vim.g.disable_vsg_format` | boolean | `false` | Disable the vsg VHDL formatter |
| `vim.g.elixir_use_nextls` | boolean | `false` | Use NextLS instead of elixir-ls (needs the `nextls` binary) |
| `vim.g.nextls_bin` | string | `"nextls"` | Path to the NextLS binary |
| `vim.g.kotlin_spring_cache_ttl` | number | `300` | Spring Boot detection cache TTL (seconds; `0` = no cache) |
| `vim.g.python_venv_auto_refresh` | boolean | `false` | Auto-refresh venv-selector on startup |
| `vim.g.python_docstring_style` | string | `"google_docstrings"` | Neogen Python docstring convention |
| `vim.g.c_build_flags` | string | `"-Wall -Wextra -g"` | GCC flags for `<leader>cb` (sanitised to `[A-Za-z0-9 -._=/+:,@]`) |
| `vim.g.cpp_build_flags` | string | `"-Wall -std=c++17 -g"` | G++ flags for `<leader>ccx` and the runner |
| `vim.g.fortran_build_flags` | string | `"-Wall"` | gfortran flags for `<leader>ftb` |
| `vim.g.cobol_format` | string | `"fixed"` | `"free"` adds `-free` to cobc |
| `vim.g.rustaceanvim_features` | table | `nil` | Active Cargo features (toggle with `<leader>rf`; applied live) |
| `vim.g.rest_auth_tokens` | table | `nil` | Seed auth tokens for the REST client, `{ name = "Bearer …" }` |
| `vim.g.ts_disable` | table | `nil` | Treesitter parsers to disable, e.g. `{ "vhdl", "fortran" }` |
| `vim.g.ts_auto_install` | boolean | `true` | Auto-install missing parsers when the tree-sitter CLI is present |

<!-- END GENERATED:flags -->

---

## Plugins by phase

Phases follow the delivery waves: core specs (Waves 1–2), language specs (Wave 3). Tests, flags registry and generated docs (Wave 4)
are tooling, not plugins.

<!-- BEGIN GENERATED:plugins -->
117 plugins. `Loads on` = lazy.nvim trigger (startup / event / cmd / ft / keys / on-require).

**Core (Waves 1–2): UI, editor, completion, LSP, treesitter, git, DAP, test, workflow** — 76

| Plugin | Repo | Spec | Loads on |
|--------|------|------|----------|
| LuaSnip | `L3MON4D3/LuaSnip` | completion | on-require |
| actions-preview.nvim | `aznhe21/actions-preview.nvim` | lsp | on-require |
| barbecue.nvim | `utilyre/barbecue.nvim` | hud | event |
| better-escape.nvim | `max397574/better-escape.nvim` | advanced | event |
| blame.nvim | `FabijanZulj/blame.nvim` | git | cmd+keys |
| blink.cmp | `saghen/blink.cmp` | completion | event |
| bufferline.nvim | `akinsho/bufferline.nvim` | ui | event |
| catppuccin | `catppuccin/nvim` | ui | on-require |
| conform.nvim | `stevearc/conform.nvim` | lsp | event |
| diffview.nvim | `sindrets/diffview.nvim` | git | cmd |
| fidget.nvim | `j-hui/fidget.nvim` | lsp | event |
| flash.nvim | `folke/flash.nvim` | editor | event |
| git-conflict.nvim | `akinsho/git-conflict.nvim` | git | event+keys |
| git-worktree.nvim | `ThePrimeagen/git-worktree.nvim` | git | event+keys |
| gitsigns.nvim | `lewis6991/gitsigns.nvim` | git | event |
| gruvbox-material | `sainnhe/gruvbox-material` | ui | on-require |
| gv.vim | `junegunn/gv.vim` | git | cmd+keys |
| harpoon | `ThePrimeagen/harpoon` | editor | event |
| inc-rename.nvim | `smjonas/inc-rename.nvim` | hud | cmd |
| indent-blankline.nvim | `lukas-reineke/indent-blankline.nvim` | hud | event |
| kanagawa | `rebelot/kanagawa.nvim` | ui | on-require |
| lazygit.nvim | `kdheepak/lazygit.nvim` | git | cmd |
| lualine.nvim | `nvim-lualine/lualine.nvim` | ui | event |
| mason.nvim | `williamboman/mason.nvim` | lsp | cmd |
| mini.animate | `echasnovski/mini.animate` | advanced | event |
| mini.nvim | `echasnovski/mini.nvim` | advanced | event+keys |
| neo-tree.nvim | `nvim-neo-tree/neo-tree.nvim` | editor | cmd+keys |
| neogen | `danymat/neogen` | advanced | cmd+ft+keys |
| neogit | `NeogitOrg/neogit` | git | cmd+keys |
| neoscroll.nvim | `karb94/neoscroll.nvim` | hud | event |
| neotest | `nvim-neotest/neotest` | test | keys |
| noice.nvim | `folke/noice.nvim` | hud | event |
| nvim-coverage | `andythigpen/nvim-coverage` | test | cmd+keys |
| nvim-dap | `mfussenegger/nvim-dap` | dap | ft+keys |
| nvim-dap-virtual-text | `rcarriga/nvim-dap-virtual-text` | dap | event |
| nvim-highlight-colors | `brenoprata10/nvim-highlight-colors` | advanced | event |
| nvim-lint | `mfussenegger/nvim-lint` | lsp | event |
| nvim-lspconfig | `neovim/nvim-lspconfig` | lsp | event |
| nvim-navic | `SmiteshP/nvim-navic` | advanced | event |
| nvim-notify | `rcarriga/nvim-notify` | ui | event |
| nvim-scrollview | `dstein64/nvim-scrollview` | hud | event |
| nvim-spectre | `nvim-pack/nvim-spectre` | editor | cmd |
| nvim-treesitter | `nvim-treesitter/nvim-treesitter` | advanced | startup |
| nvim-treesitter-context | `nvim-treesitter/nvim-treesitter-context` | treesitter | event |
| nvim-treesitter-textobjects | `nvim-treesitter/nvim-treesitter-textobjects` | treesitter | startup |
| nvim-ufo | `kevinhwang91/nvim-ufo` | advanced | event |
| nvim-web-devicons | `nvim-tree/nvim-web-devicons` | advanced | startup |
| octo.nvim | `pwntester/octo.nvim` | git | cmd+keys |
| oil.nvim | `stevearc/oil.nvim` | hud | cmd+keys |
| overseer.nvim | `stevearc/overseer.nvim` | workflow | cmd+keys |
| persistence.nvim | `folke/persistence.nvim` | editor | event+keys |
| rainbow-delimiters.nvim | `HiPhish/rainbow-delimiters.nvim` | advanced | event |
| rose-pine | `rose-pine/neovim` | ui | on-require |
| smear-cursor.nvim | `sphamba/smear-cursor.nvim` | hud | event |
| snacks.nvim | `folke/snacks.nvim` | ui | startup |
| solarized | `maxmx03/solarized.nvim` | ui | on-require |
| solarized-osaka | `craftzdog/solarized-osaka.nvim` | ui | on-require |
| stay-in-place.nvim | `gbprod/stay-in-place.nvim` | advanced | event |
| symbol-usage.nvim | `Wansmer/symbol-usage.nvim` | hud | event |
| telescope-fzf-native.nvim | `nvim-telescope/telescope-fzf-native.nvim` | editor | on-require |
| telescope.nvim | `nvim-telescope/telescope.nvim` | editor | cmd+keys |
| template.nvim | `nvimdev/template.nvim` | editor | cmd |
| tint.nvim | `levouh/tint.nvim` | hud | event |
| todo-comments.nvim | `folke/todo-comments.nvim` | editor | event+cmd+keys |
| toggleterm.nvim | `akinsho/toggleterm.nvim` | ui | cmd+ft+keys |
| tokyonight | `folke/tokyonight.nvim` | ui | startup |
| trouble.nvim | `folke/trouble.nvim` | ui | cmd+keys |
| twilight.nvim | `folke/twilight.nvim` | hud | cmd+keys |
| undotree | `mbbill/undotree` | advanced | cmd+keys |
| vim-abolish | `tpope/vim-abolish` | advanced | event |
| vim-fugitive | `tpope/vim-fugitive` | git | cmd+keys |
| vim-matchup | `andymass/vim-matchup` | advanced | event |
| vim-repeat | `tpope/vim-repeat` | advanced | event |
| vim-tmux-navigator | `christoomey/vim-tmux-navigator` | editor | cmd+keys |
| which-key.nvim | `folke/which-key.nvim` | ui | event |
| zen-mode.nvim | `folke/zen-mode.nvim` | hud | cmd+keys |

**Languages (Wave 3): per-language specs** — 22

| Plugin | Repo | Spec | Loads on |
|--------|------|------|----------|
| clangd_extensions.nvim | `p00f/clangd_extensions.nvim` | lang/c | ft |
| cmake-tools.nvim | `Civitasv/cmake-tools.nvim` | lang/cpp | ft+keys |
| emmet-vim | `mattn/emmet-vim` | lang/web | ft |
| go.nvim | `ray-x/go.nvim` | lang/go | ft+keys |
| iron.nvim | `Vigemus/iron.nvim` | lang/python | ft+keys |
| kulala.nvim | `mistweaverco/kulala.nvim` | lang/rest | event+ft+keys |
| markview.nvim | `OXY2DEV/markview.nvim` | lang/markdown | event+ft+keys |
| nvim-dap-python | `mfussenegger/nvim-dap-python` | lang/python | ft |
| nvim-jdtls | `mfussenegger/nvim-jdtls` | lang/java | ft |
| nvim-treesitter-endwise | `RRethy/nvim-treesitter-endwise` | lang/ruby | event |
| nvim-ts-autotag | `windwp/nvim-ts-autotag` | lang/web | ft |
| package-info.nvim | `vuki656/package-info.nvim` | lang/javascript | event+keys |
| peek.nvim | `toppair/peek.nvim` | lang/markdown | ft+keys |
| rustaceanvim | `mrcjkb/rustaceanvim` | lang/rust | startup |
| typescript-tools.nvim | `pmizio/typescript-tools.nvim` | lang/typescript | ft+keys |
| venv-selector.nvim | `linux-cultist/venv-selector.nvim` | lang/python | cmd+ft+keys |
| vim-dadbod-completion | `kristijanhusak/vim-dadbod-completion` | lang/database | ft |
| vim-dadbod-ui | `kristijanhusak/vim-dadbod-ui` | lang/database | cmd+keys |
| vim-markdown | `preservim/vim-markdown` | lang/markdown | ft |
| vim-python-pep8-indent | `Vimjas/vim-python-pep8-indent` | lang/python | ft |
| vim-table-mode | `dhruvasagar/vim-table-mode` | lang/markdown | cmd+ft+keys |
| vim-test | `vim-test/vim-test` | lang/ruby | ft+keys |

**Dependencies (pulled in by the plugins above)** — 19

| Plugin | Repo | Spec | Loads on |
|--------|------|------|----------|
| friendly-snippets | `rafamadriz/friendly-snippets` | — | on-require |
| guihua.lua | `ray-x/guihua.lua` | — | on-require |
| lazy.nvim | `folke/lazy.nvim` | — | on-require |
| mason-lspconfig.nvim | `mason-lspconfig.nvim` | — | on-require |
| neotest-elixir | `jfpedroza/neotest-elixir` | — | on-require |
| neotest-go | `nvim-neotest/neotest-go` | — | on-require |
| neotest-java | `rcasia/neotest-java` | — | on-require |
| neotest-jest | `haydenmeade/neotest-jest` | — | on-require |
| neotest-kotlin | `codymikol/neotest-kotlin` | — | on-require |
| neotest-python | `nvim-neotest/neotest-python` | — | on-require |
| neotest-rspec | `olimorris/neotest-rspec` | — | on-require |
| neotest-rust | `rouge8/neotest-rust` | — | on-require |
| neotest-vitest | `marilari88/neotest-vitest` | — | on-require |
| nui.nvim | `MunifTanjim/nui.nvim` | — | on-require |
| nvim-dap-ui | `rcarriga/nvim-dap-ui` | — | on-require |
| nvim-nio | `nvim-neotest/nvim-nio` | — | on-require |
| plenary.nvim | `nvim-lua/plenary.nvim` | — | on-require |
| promise-async | `kevinhwang91/promise-async` | — | on-require |
| vim-dadbod | `tpope/vim-dadbod` | — | on-require |

<!-- END GENERATED:plugins -->

---

## Hot-swap

`:Reload core.util.runner` / `:Reload core.util.*` clears `package.loaded`, re-requires, and rolls back atomically on error
(`:luafile` does not hot-swap a module). `:Reload!` bypasses the guards.

| Safe with `:Reload` | Needs `:Lazy reload <plugin>` | Needs a full restart |
|---------------------|-------------------------------|----------------------|
| `core.keymaps` `core.commands` `core.autocmds` `core.highlights` `core.theme` `core.focus` `core.util.*` | `nvim-lspconfig` `neotest` `nvim-dap` `conform.nvim` `nvim-lint` `overseer.nvim` | `core.bootstrap` `core.options` `ui` `treesitter` `lsp` `completion` `dap` `test` any `lang/*` any `vim.g` flag (read once at plugin load) |

---

## Troubleshooting

`:Health` · `:checkhealth core` · `:checkhealth lsp` · `:LspInfo` · `:Mason` · `:lua print(vim.g.nvim_ide_version)`

| Symptom | Fix |
|---------|-----|
| "loop or previous error loading module" | Startup now prints `FIRST REAL ERROR` + traceback; fix that error. Delete a stale `lua/plugins/init.lua` |
| LSP not attaching / `gd` does nothing | `:MasonInstallAll` · `:checkhealth lsp` · `:LspInfo` |
| DAP adapter not found | `:MasonInstallAll` |
| Completion broken | `:Lazy update` · keep `version = "1.*"` in `completion.lua` |
| VHDL LSP missing | `cargo install vhdl_ls` + restart |
| TypeScript LSP double-attaching | ensure `typescript-tools.nvim` installed (`ts_ls` is only a fallback) |
| Treesitter highlighting broken | `:TSUpdate` (config pins `nvim-treesitter` to `branch = "master"`) |
| `:MasonInstallAll` times out | `vim.g.mason_install_timeout_ms = 240000` |
| debugpy not found | `vim.g.debugpy_python = "/path/to/python"` |
| Wrong project root | `vim.g.path_debug = true` and read DEBUG notifications |
| Java workspace stale | delete `~/.local/share/nvim/jdtls-workspace/` + restart |
| Kotlin tests not running | `chmod +x gradlew` |
| Large file slow | expected: protections apply above 500 KB |
| Format silently does nothing | check the WARN notification; raise `vim.g.format_timeout_ms` / `format_timeout_by_ft` |
| htmlhint / stylelint not linting | add `.htmlhintrc` / `.stylelintrc.json` (or the key in `package.json`) |
| telescope-fzf-native fails | `cd ~/.local/share/nvim/lazy/telescope-fzf-native.nvim && make` |
| tint / smear-cursor artefacts | `vim.g.disable_tint = true` / `vim.g.disable_smear_cursor = true` |
| symbol-usage slow | `vim.g.symbol_usage_max_lines = 1000` |
| `Ctrl+h/j/k/l` stays inside Neovim | add the `is_vim` check to `.tmux.conf` |
| `<Esc>` leaves a TUI (lazygit) | TUIs listed in `vim.g.terminal_tui_programs` keep their own `<Esc>` |
| Plugins drift after `:Lazy update` | `:Lazy lock` and commit `lazy-lock.json` |
| Plugins not loading at all | `rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim && nvim`, then `:MasonInstallAll` `:TSUpdate` `:Lazy lock` |
| Neovim too old (openSUSE) | `sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors && sudo zypper ref && sudo zypper in neovim` |

---

## Changelog

### v2.6.1 — calibration (current)

Base: the Wave 4 modules. Verified headless on Neovim 0.11.4 from a fresh plugin install (`tests/run.sh`, 0 failures).

| Module | Change |
|--------|--------|
| `core/lazy.lua` (new) | lazy.nvim loader, replaces `lua/plugins/init.lua` (the module name `plugins` was a poisoned-require magnet) |
| `init.lua` | `safe_require`: clears LuaJIT "loop or previous error" poison, retries once, prints traceback + FIRST REAL ERROR |
| `core/commands.lua` | `:MasonInstallAll dry-run` ignored its argument and started a real install (`find("dry%-run", 1, true)` is a literal search) |
| `plugins/specs/lang/javascript.lua` | package-info `colors` (deprecated) → `highlights` |
| `plugins/specs/treesitter.lua` | treesitter-context skipped on Fortran (its query is invalid for the installed parser) |
| `plugins/specs/advanced.lua` | rainbow-delimiters `submodules = false` |
| `core/util/vhdl.lua` | restored verbatim (missing from the flat project upload; required by `<leader>vht`) |
| `core/bootstrap.lua` | version stamp |

Tooling added: `tests/{smoke,modules,units,behaviour,keymaps,specs_lint}.lua`, `tests/run.sh`, `scripts/gen_docs.lua`, `stylua.toml`.
`tests/keymaps.lua` checks both directions (every documented key exists; every leader map is documented).
`tests/specs_lint.lua` fails when a plugin sets `init`/`config`/`build`/`cond`/`version`/`branch`/`main`/`priority` in more than one spec
(lazy.nvim keeps only the last, so earlier ones silently never run).
Docs consolidated: README + CHANGELOG (setup, flags, plugins) + KEYMAPS. Flag and plugin tables are generated by `scripts/gen_docs.lua`.

Not exercised headless: LSP/DAP/jdtls attach-time behaviour, real Mason installs, tmux, GUI.
`:DBUIToggle` leaves `E716 … "collapsed"` in `v:errmsg`: vim-dadbod-ui's own swallowed `silent! call remove(...)`; benign.

### v2.6.0 — Wave 4 (proof)

Flags registry (`core/flags.lua`, single source of truth), generated docs, drift and keymap tests, luacheck, CI.

### Wave 3 — languages

Language modules (C/C++, Rust, Go, Python, Java, Kotlin, JS/TS, web, Ruby, Elixir, Fortran, Zig, COBOL, VHDL, SQL, REST, Markdown),
`tests/unit_lang.lua`, extended smoke test.

### Wave 2 — core

Core modules (options, autocmds, keymaps, commands, theme, focus, highlights, util/*) with headless tests (`tests/run.sh`).

### Wave 1 — dead-on-arrival fixes

Fixes for modules that could not work as shipped.

### v2.5.0 — baseline

Modular lazy-loaded Neovim IDE: 20+ languages, LSP/DAP/test/git/task-runner stack.
