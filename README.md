# nvim-ide

> Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages.

**Neovim 0.11+** · openSUSE Leap 16.0 · **v2.5.0**

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
| tmux integration | **optional** — `tmux >= 3.2`; enables seamless `Ctrl+h/j/k/l` pane navigation |

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

# 7. Pin all plugin commits for reproducible deployments
:Lazy lock             # writes lazy-lock.json — commit this to your dotfiles
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

**Single source of truth** — `packages.lua` owns all Mason/lspconfig package names. Both `:MasonInstallAll` and `mason-lspconfig.ensure_installed` read from it. `M.validate()` cross-checks naming drift between the two lists at startup (DEBUG level).

**Capability memoisation** — `get_capabilities()` in `lsp.lua` is called once and cached, preventing a deep-copy per server on startup.

**Project root detection** — `path.find_root()` walks upward through standard root markers with a configurable TTL cache, invalidated on `DirChanged`.

**Per-project env validation** — Ruby, Elixir, and Go each validate their tool environment (bundler, mix/Hex, goimports/gofumpt) per project root on first buffer open and on `DirChanged`, so switching between projects re-validates each environment independently.

**Buffer-local keymap deduplication** — `bkm.batch(buf, maps, flag)` stores a verified boolean in `vim.b[buf][flag]`, with post-write verification. Repeated calls and invalid buffers are silent no-ops.

**Atomic focus mode** — `focus.lua` snapshots all options before applying; any mid-loop failure triggers a full rollback so the UI is never left in a partial state.

**Deferred DAP** — adapters register inside `FileType once=true` autocmds. Zero DAP cost unless a relevant file is opened.

**Executable guards** — `exec.require_bin(name, hint)` centralises all `vim.fn.executable()` checks. `exec.warn_bin()` for optional tools, `exec.require_all()` for multi-binary requirements.

**Large-file protection** — files over 500 KB have treesitter, folding, syntax highlighting, cursorline, and spellcheck automatically disabled. The size flag is re-evaluated on `BufWritePost` so a file that shrinks below the threshold re-enables features on next open.

**Theme-aware highlights** — `highlights.lua` applies full TokyoNight overrides when active and a minimal universal set for all other themes. Each group is applied individually so one failing group does not abort the rest.

**Atomic Mason installs** — `:MasonInstallAll` uses an atomic pending counter; timeout configurable via `vim.g.mason_install_timeout_ms`. Version cache auto-clears after `:MasonUpdate`.

**Per-project config files** — `.lspconfig.lua`, `.nvim-dap.lua`, `.overseer.lua` at the project root are loaded automatically and override built-in defaults.

**Lockfile workflow** — `lazy-lock.json` pins every plugin commit. Run `:Lazy lock` after `:Lazy update` and commit the lockfile for fully reproducible deployments. Use `:Lazy restore` on a new machine.

**Debounced event handlers** — `BufEnter`-triggered operations (Overseer template loading, symbol-usage) are debounced to avoid accumulating calls during rapid buffer switching at startup.

---

## Escape Hatches

Set in `init.lua` **before** `require("core.bootstrap")`:

### Core display & behaviour

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_highlight_overrides` | `false` | Skip all highlight overrides |
| `vim.g.disable_tint` | `false` | Disable tint.nvim inactive-window dimming |
| `vim.g.disable_smear_cursor` | `false` | Disable smear-cursor motion trail |
| `vim.g.disable_mini_pairs` | `false` | Disable mini.pairs auto-close brackets |
| `vim.g.disable_autoformat` | `false` | Disable format-on-save globally |
| `vim.g.disable_treesitter_folds` | `false` | Use indent-based folding instead of treesitter |
| `vim.g.auto_cd_root` | `false` | Auto-cd to project root on `BufEnter` |
| `vim.g.runner_autosave` | `true` | Auto-save before running a file with runner.lua |

### LSP & formatting

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.format_timeout_ms` | `3000` | Global format-on-save timeout (ms) |
| `vim.g.format_timeout_by_ft` | `{}` | Per-filetype timeout, e.g. `{ python = 5000, cpp = 4000 }` |
| `vim.g.enable_pylint` | `false` | Opt-in pylint alongside ruff for Python |
| `vim.g.lsp_on_attach_overrides` | `nil` | Per-server on_attach overrides, e.g. `{ lua_ls = function(c,b) end }` |
| `vim.g.completion_sources_by_ft` | `nil` | Per-filetype blink.cmp source list, e.g. `{ sql = { "lsp","dadbod","buffer" } }` |
| `vim.g.debugpy_python` | `nil` | Pin Python interpreter for debugpy |
| `vim.g.ts_import_preference` | `"non-relative"` | TypeScript import module specifier preference |

### DAP & testing

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.dap_bp_autosave_ms` | `60000` | DAP breakpoint autosave interval (ms); `0` to disable |
| `vim.g.neotest_concurrency` | `4` | Parallel test suite concurrency (`<leader>'P`) |

### Task runner & workflow

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.mason_install_timeout_ms` | `120000` | Per-install timeout for `:MasonInstallAll` |
| `vim.g.mason_extras` | `nil` | Extra Mason packages appended to the extras list |
| `vim.g.workflow_template_debounce_ms` | `300` | Overseer `.overseer.lua` load debounce on `BufEnter` (ms) |
| `vim.g.overseer_auto_scroll` | `true` | Auto-scroll Overseer task output |

### Project root & paths

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.path_max_walk_depth` | `20` | Upward-walk limit for root detection |
| `vim.g.path_cache_ttl` | `30` | Root-cache lifetime in seconds |
| `vim.g.path_debug` | `false` | Log silent cwd fallbacks at DEBUG level |
| `vim.g.path_ignore_dirs` | `nil` | Extra directory names to skip during root walk |
| `vim.g.cmake_build_dir` | `"build"` | CMake build directory name (used by C/C++/CTest) |
| `vim.g.lazy_cache_path` | `nil` | Custom path for offline lazy.nvim cache |

### UI extras

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.symbol_usage_max_lines` | `2000` | Disable symbol-usage.nvim above this line count |
| `vim.g.force_mini_animate_scroll` | `false` | Force mini.animate scroll even when neoscroll.nvim active |
| `vim.g.octo_timeout_ms` | `10000` | Octo.nvim request timeout (ms) |

### Language-specific

| Flag | Default | Effect |
|------|---------|--------|
| `vim.g.disable_vsg_format` | `false` | Disable vsg VHDL formatter |
| `vim.g.elixir_use_nextls` | `false` | Use NextLS instead of elixir-ls (requires `nextls` binary) |
| `vim.g.kotlin_spring_cache_ttl` | `300` | Spring Boot detection cache TTL in seconds |
| `vim.g.python_venv_auto_refresh` | `false` | Auto-refresh venv-selector on startup |
| `vim.g.python_docstring_style` | `"google_docstrings"` | Neogen Python docstring convention |
| `vim.g.c_build_flags` | `"-Wall -Wextra -g"` | GCC build flags for `<leader>cb` |
| `vim.g.cpp_build_flags` | `"-Wall -std=c++17 -g"` | G++ build flags for `<leader>ccr` |
| `vim.g.fortran_build_flags` | `"-Wall"` | gfortran build flags for `<leader>ftb` |
| `vim.g.rustaceanvim_features` | `nil` | Active Cargo feature flags (toggled via `<leader>rf`) |
| `vim.g.filetype_options` | `nil` | Per-filetype vim option overrides, e.g. `{ rust = { colorcolumn = "100" } }` |
| `vim.g.spell_wordlist` | `nil` | Path to personal spell file |
| `vim.g.ts_disable` | `nil` | List of Treesitter parsers to disable, e.g. `{ "vhdl", "fortran" }` |
| `vim.g.ts_auto_install` | `true` | Auto-install missing parsers when tree-sitter CLI present |
| `vim.g.nextls_bin` | `"nextls"` | Path to NextLS binary (Elixir) |

---

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff (+ pylint opt-in) | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo test |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | jdtls built-in | — | JUnit |
| Kotlin | kotlin_language_server | java-debug | ktlint | ktlint | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls (or NextLS) | elixir-ls | mix format | — | ExUnit |
| C | clangd | codelldb | clang-format | clang-tidy | ctest |
| C++ | clangd | codelldb | clang-format | — | ctest |
| HTML | html-lsp | — | prettier | htmlhint¹ | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint¹ | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb | zig fmt | — | zig build test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-server² | — | — | — | — |
| VHDL | vhdl_ls³ | — | vsg⁴ | — | ghdl |

> ¹ Only enabled when a config file is detected in the project root  
> ² Not in Mason registry: `npm i -g @broadcommfd/cobol-language-support`  
> ³ Not in Mason registry: `cargo install vhdl_ls`  
> ⁴ Formats via `--stdin` so buffer content (not on-disk file) is formatted

---

## Plugins

### Core infrastructure

| Plugin | Role |
|--------|------|
| lazy.nvim | Plugin manager |
| mason.nvim | LSP/DAP/formatter installer |
| nvim-lspconfig | LSP client configuration |
| blink.cmp | Completion engine (v1.x) |
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
| vim-tmux-navigator | Seamless `Ctrl+h/j/k/l` across Neovim splits and tmux panes |
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

---

## Deep Focus Mode

`<leader>uF` hides the statusline, bufferline, signcolumn, ruler, and line numbers, then enables Twilight and ZenMode. All options are snapshotted atomically and restored exactly on exit — including rollback if any option fails to apply — and on `VimLeavePre`.

- `<leader>uz` — ZenMode only
- `<leader>uT` — Twilight only

---

## Updating

```vim
:Lazy update       " plugins
:MasonUpdate       " LSP/DAP/formatter packages  (version cache auto-clears)
:TSUpdate          " Treesitter parsers
:Lazy lock         " re-pin commits after update — commit lazy-lock.json
```

After a major Neovim upgrade:
```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim
# then: :MasonInstallAll  :TSUpdate  :checkhealth  :Lazy lock
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
| blink.cmp major-version warning at startup | Update `completion.lua` version pin to match installed major |
| COBOL LSP missing | `npm i -g @broadcommfd/cobol-language-support` + restart |
| VHDL LSP missing | `cargo install vhdl_ls` + restart |
| TypeScript LSP double-attaching | Ensure typescript-tools.nvim installed; `ts_ls` fallback only fires when absent |
| `<leader>ts*` missing in JS/JSX | `:Lazy reload typescript-tools.nvim` |
| VHDL format applies stale on-disk content | Confirm vsg formatter uses `--stdin` flag |
| Iron send-motion wrong range | ModeChanged operatorfunc guard handles this — check `python.lua` |
| `<Esc>` exits terminal before nested TUI | By design; see keymaps.lua for 3 workaround options (delete binding / double-Escape / per-terminal) |
| `Ctrl+h/j/k/l` doesn't cross into tmux pane | `is_vim` check missing from `.tmux.conf`; ensure the `bind -n C-h if-shell "$is_vim"` block is present |
| tint / smear-cursor artefacts | `vim.g.disable_tint = true` / `vim.g.disable_smear_cursor = true` |
| Highlight colours wrong on non-TN theme | `vim.g.disable_highlight_overrides = true` |
| `:MasonInstallAll` times out | `vim.g.mason_install_timeout_ms = 240000` |
| Wrong project root detected | `vim.g.path_debug = true` · check DEBUG notifications |
| debugpy not found in venv | `vim.g.debugpy_python = "/path/to/python"` |
| Large file very slow | Expected — treesitter/folding auto-disabled above 500 KB |
| Kotlin gradlew fails | `chmod +x gradlew` in project root |
| Kotlin Spring keymaps missing after project switch | Spring detection cache TTL may be stale; set `vim.g.kotlin_spring_cache_ttl = 0` to disable cache |
| Dashboard blank after config change | Full restart required |
| Java workspace stale | Delete `~/.local/share/nvim/jdtls-workspace/` and restart |
| `:LspRestart` not re-attaching jdtls | Fixed in v2.5.0; ensure java.lua is updated |
| Plugins not loading at all | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |
| Format silently does nothing | Check WARN notification for timeout details; increase `vim.g.format_timeout_ms` or `vim.g.format_timeout_by_ft` |
| htmlhint not linting | Create `.htmlhintrc` or add `"htmlhint"` key in `package.json` |
| stylelint not linting | Create `.stylelintrc.json` or add `"stylelint"` key in `package.json` |
| telescope-fzf-native not loading | Build failed; run `cd ~/.local/share/nvim/lazy/telescope-fzf-native.nvim && make` |
| symbol-usage slow on large project | `vim.g.symbol_usage_max_lines = 1000` to lower the gate |
| mini.animate double-scroll animation | `vim.g.force_mini_animate_scroll = false` (default); neoscroll.nvim takes priority |
| Elixir env check not re-running after project switch | Fixed in v2.5.0; ensure elixir.lua is updated |
| Plugins drifting after :Lazy update | Run `:Lazy lock` and commit `lazy-lock.json` to your dotfiles |

**Neovim too old (openSUSE):**
```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors
sudo zypper ref && sudo zypper in neovim
```
