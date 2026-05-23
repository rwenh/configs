# nvim-ide

> Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages, battle-tested.

**Neovim 0.11+** · openSUSE Leap 16.0 · **v2.4.0**

---

## Table of Contents

- [Overview](#overview)
- [Requirements](#requirements)
- [Quick Install](#quick-install)
- [Structure](#structure)
- [Architecture Decisions](#architecture-decisions)
- [Languages](#languages)
- [Plugins at a Glance](#plugins-at-a-glance)
- [Key Bindings](#key-bindings)
- [Themes](#themes)
- [Dashboard & Splash Screen](#dashboard--splash-screen)
- [Deep Focus Mode](#deep-focus-mode)
- [Updating](#updating)
- [Troubleshooting](#troubleshooting)
- [Changelog](#changelog)

---

## Overview

A single-maintainer Neovim configuration built to function as a genuine IDE
across 20+ languages without sacrificing startup speed or editor stability.
Every design decision optimises for three properties in this order:

1. **Correctness** — silent failures are treated as bugs; every code path is
   guarded and notifies the user when it cannot proceed.
2. **Modularity** — each language has an isolated spec file; core utilities
   live in `lua/core/util/`; nothing cross-requires without an explicit
   dependency declaration.
3. **Performance** — plugins load on the narrowest possible trigger (filetype,
   command, or key); capabilities are memoised; per-buffer caches use TTL
   invalidation; large files disable expensive features automatically.

---

## Requirements

### Hard requirements

| Tool | Min version | Purpose |
|------|-------------|---------|
| `neovim` | **0.11** | Core — uses `vim.lsp.config()` / `vim.lsp.enable()` API |
| `git` | any | Plugin bootstrap via lazy.nvim |
| `gcc` or `clang` | any | Treesitter parser compilation |
| `node` + `npm` | **18** | LSP servers, formatters, JS debug adapter |
| `python3` + `pip` | **3.9** | Python LSP (basedpyright), debugpy |
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
| COBOL | `gnucobol` (`cobc`); LSP installed manually — see [INSTALL.md](INSTALL.md) |
| VHDL | `ghdl`, `gtkwave`; `cargo install vhdl_ls` |
| REST | `curl`, optionally `jq` for JSON formatting |
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

# 3. COBOL LSP — not in Mason registry, install manually
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
:MasonInstallAll       # installs LSP servers, DAP adapters, formatters, linters
:TSUpdate              # installs Treesitter parsers
:checkhealth           # verify the setup
```

One-shot installer:

```bash
bash <(curl -fsSL https://raw.githubusercontent.com/rwenh/configs/main/install.sh)
```

---

## Structure

```
~/.config/nvim/
├── init.lua                          ← entry point; load order documented inline
└── lua/
    ├── core/
    │   ├── bootstrap.lua             ← lazy.nvim bootstrap, leader keys, version stamp
    │   ├── options.lua               ← vim.opt settings
    │   ├── keymaps.lua               ← global keymaps (lazy-require pattern)
    │   ├── autocmds.lua              ← autocommands (yank flash, cursor restore, …)
    │   ├── commands.lua              ← user commands (:MasonInstallAll, :Format, …)
    │   ├── theme.lua                 ← theme management + auto dark/light + toggle
    │   ├── focus.lua                 ← deep focus mode (strips chrome, enables zen)
    │   ├── highlights.lua            ← synthwave accent overrides (survives theme toggles)
    │   ├── scanner.lua               ← DEPRECATED v2.4.1 — safe to delete
    │   └── util/
    │       ├── path.lua              ← project root detection with TTL cache
    │       ├── runner.lua            ← file / selection / test runner (20+ filetypes)
    │       ├── term.lua              ← toggleterm launch helper (float / root-relative)
    │       ├── buf_keymap.lua        ← buffer-local keymap registration + deduplication
    │       ├── mason.lua             ← Mason binary / package path helpers
    │       ├── packages.lua          ← Mason + lspconfig package lists (single source of truth)
    │       ├── icons.lua             ← shared Nerd Font icon constants
    │       ├── exec.lua              ← executable guard helper  ← NEW v2.5.0
    │       ├── snippets.lua          ← LuaSnip snippet factory helpers  ← NEW v2.5.0
    │       └── quotes.lua            ← 36 curated quotes (6 categories, weighted)  ← NEW v2.5.0
    └── plugins/
        ├── init.lua                  ← lazy.nvim setup
        └── specs/
            ├── ui.lua                ← themes, statusline, bufferline, dashboard
            ├── editor.lua            ← telescope, neo-tree, flash, harpoon, sessions
            ├── lsp.lua               ← LSP, Mason, conform, nvim-lint
            ├── completion.lua        ← blink.cmp
            ├── treesitter.lua        ← treesitter + context + textobjects
            ├── git.lua               ← gitsigns, lazygit, diffview, neogit, octo, blame
            ├── dap.lua               ← nvim-dap + UI + all language adapters
            ├── test.lua              ← neotest + adapters + coverage
            ├── advanced.lua          ← ufo, neogen, rainbow, matchup, mini.*
            ├── hud.lua               ← noice, barbecue, oil, animate, zen, twilight
            ├── workflow.lua          ← overseer task runner
            └── lang/                 ← per-language specs (one file per language)
                ├── shared.lua        ← filetype constants + shared.treesitter() helper
                ├── c.lua
                ├── cpp.lua
                ├── rust.lua
                ├── go.lua
                ├── python.lua
                ├── java.lua
                ├── kotlin.lua
                ├── javascript.lua
                ├── typescript.lua
                ├── web.lua           ← autotag + emmet (load BEFORE html/css)
                ├── html.lua
                ├── css.lua
                ├── database.lua      ← dadbod UI + SQL config owner
                ├── markdown.lua
                ├── rest.lua
                ├── ruby.lua
                ├── elixir.lua
                ├── fortran.lua
                ├── zig.lua
                ├── cobol.lua
                └── vhdl.lua
```

### Load-order constraints

These constraints are enforced in `plugins/specs/init.lua` and must not be
reordered:

| Constraint | Reason |
|------------|--------|
| `completion` before `lsp` | blink.cmp capabilities are injected into lsp.lua |
| `lsp` before `lang/*` | lang specs extend LSP server lists and formatter tables |
| `web` before `html` / `css` | autotag and emmet must be available when html/css init |
| `database` is SQL config owner | `sql.lua` was deleted; database.lua owns all SQL config |

---

## Architecture Decisions

### Single source of truth — `packages.lua`

All Mason package names and lspconfig server identifiers live in
`lua/core/util/packages.lua`. Both `:MasonInstallAll` (in `commands.lua`) and
`mason-lspconfig.ensure_installed` (in `lsp.lua`) read from this file.
**Do not add a new LSP server anywhere else** — add it to `packages.lua` only.

```lua
-- lua/core/util/packages.lua
M.lspconfig = { "lua_ls", "basedpyright", ... }  -- for mason-lspconfig
M.mason.lsp  = { "lua-language-server", ... }     -- for :MasonInstallAll
M.mason.dap  = { "debugpy", "codelldb", ... }
M.mason.formatters = { "stylua", "prettier", ... }
M.mason.linters    = { "ruff", "eslint_d", ... }
```

### Capability memoisation — `lsp.lua`

`get_capabilities()` is called once and cached in a module-level `_caps`
variable. This prevents blink.cmp's capability table from being deep-copied
on every `lsp_setup()` call, which was previously O(n×servers) per startup.

### Project root detection — `path.lua`

`path.find_root()` walks upward from the current file looking for root markers
(`.git`, `Cargo.toml`, `package.json`, etc.) with a 30-second TTL cache.
Cache is invalidated on `DirChangedPre` / `DirChanged`. The cache key is the
normalised directory path; the value is `{ root, time }`.

### Buffer-local keymap deduplication — `buf_keymap.lua`

`bkm.batch(buf, maps, flag_name)` stores a boolean in `vim.b[buf][flag_name]`
on first call, making subsequent calls no-ops. This prevents double-registration
on `:luafile` reloads. `bkm.on_ft(ft, maps, flag_name)` also retroactively
applies to already-open buffers of the target filetype.

### Deferred DAP registration — `dap.lua`

All DAP adapters and configurations are registered inside `FileType` autocmds
with `once = true`. This means zero DAP initialisation cost at startup — the
adapter for Go is never loaded unless a Go file is opened in that session.

### Optional servers via binary gate — `lsp.lua`

Servers not in the Mason registry (`vhdl_ls`, `cobol_ls`) are set up only if
their binary is already executable. This prevents error noise on systems where
the language is not installed.

```lua
-- Pattern used for VHDL, COBOL, Fortran:
if vim.fn.executable(bin) == 1 then
  lsp_setup(server, config)
end
```

### Executable guard utility — `exec.lua` (v2.5.0)

All `if vim.fn.executable("X") ~= 1 then ... return end` guards were
consolidated into `require("core.util.exec").require_bin(name, hint)` which
returns `true`/`false` and handles the notification internally. This removed
~65 identical guard blocks across runner.lua, dap.lua, and all lang files.

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
| Java | jdtls | java-debug | jdtls built-in | — | JUnit (neotest-java) |
| Kotlin | kotlin_language_server | java-debug | ktlint | ktlint | JUnit (neotest-java) |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec (vim-test + neotest) |
| Elixir | elixir-ls | elixir-ls | mix format | — | ExUnit (neotest-elixir) |
| C | clangd | codelldb | clang-format | clang-tidy | ctest |
| C++ | clangd | codelldb | clang-format | — | ctest |
| HTML | html-lsp | — | prettier | htmlhint | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb | zig fmt | — | zig build test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-server¹ | — | — | — | — |
| VHDL | vhdl_ls² | — | vsg | — | ghdl simulation |

> ¹ COBOL LSP: not in Mason registry. Install: `npm i -g @broadcommfd/cobol-language-support`
> ² vhdl_ls: not in Mason registry. Install: `cargo install vhdl_ls`

### Test mechanism guide

Each language with test support offers multiple mechanisms — choose based on
the workflow:

| Mechanism | Trigger | Best for |
|-----------|---------|----------|
| `<leader>'n` | neotest nearest | Interactive TDD, rich output panel |
| `<leader>'f` | neotest file | All tests in current file |
| `<leader>'t` | runner.lua | Full project test suite (CI-style) |
| `<leader>got` | go.nvim GoTest | Go with go.nvim env integration |
| `<leader>rbn/f/s` | vim-test | Ruby quick file/suite runs |
| `<leader>ext` | mix test | Elixir full suite |
| `<leader>ktt` | gradle/maven | Kotlin/Java build-tool test |

---

## Plugins at a Glance

### Core infrastructure

| Plugin | Role |
|--------|------|
| lazy.nvim | Plugin manager |
| mason.nvim | LSP/DAP/formatter installer |
| nvim-lspconfig | LSP client configuration |
| blink.cmp | Completion engine |
| nvim-treesitter | Syntax, folds, textobjects |
| conform.nvim | Formatter runner (format-on-save) |
| nvim-lint | Async linter |
| nvim-dap + dapui | Debug adapter protocol |
| neotest | Unified test runner |

### Editor

| Plugin | Role |
|--------|------|
| telescope.nvim | Fuzzy finder |
| neo-tree.nvim | File explorer |
| flash.nvim | Motion jump (`s`) |
| harpoon2 | File bookmarks |
| persistence.nvim | Session management |
| nvim-spectre | Project-wide search/replace |
| todo-comments.nvim | TODO/FIXME tracking |

### Git

| Plugin | Role |
|--------|------|
| gitsigns.nvim | Inline hunk signs + staging |
| lazygit.nvim | Full-screen Git TUI |
| diffview.nvim | Side-by-side diff + history |
| neogit | Magit-style commit/rebase UI |
| git-conflict.nvim | 3-way conflict resolution |
| gv.vim | Commit graph browser |
| octo.nvim | GitHub PR/issue workflow |
| blame.nvim | Per-line git blame |

### Visual

| Plugin | Role |
|--------|------|
| snacks.nvim | Dashboard with logo + session quote |
| lualine.nvim | Statusline |
| bufferline.nvim | Buffer tabs |
| nvim-notify | Notification popups |
| noice.nvim | UI overhaul (cmdline, messages) |
| barbecue.nvim | LSP breadcrumb bar |
| symbol-usage.nvim | Inline reference counts |
| indent-blankline.nvim | Indent guides |
| nvim-ufo | Fold enhancement with previews |
| zen-mode.nvim | Distraction-free writing |
| twilight.nvim | Dim inactive code regions |
| smear-cursor.nvim | Cursor motion trail |
| tint.nvim | Dim inactive splits |

---

## Key Bindings

Leader: `Space` · Full reference: [KEYMAP_REFERENCE.md](KEYMAP_REFERENCE.md)

### Most-used bindings

| Key | Action |
|-----|--------|
| `<leader>ff` | Find files |
| `<leader>fw` | Live grep |
| `<leader>fb` | Find buffer |
| `<leader>fo` | Recent files |
| `<leader>ee` | Toggle file explorer |
| `<leader>.g` | LazyGit |
| `gd` / `gD` | LSP: definition / declaration |
| `gr` | LSP: references |
| `K` | LSP: hover docs |
| `<leader>,a` | Code action |
| `<leader>,f` | Format |
| `<leader>,r` | Rename symbol |
| `F5`–`F11` | DAP: continue / breakpoint / step / terminate |
| `<leader>'r` | Run file |
| `<leader>'t` | Run tests |
| `<leader>'n` | Neotest nearest |
| `<leader>ut` | Toggle dark / light theme |
| `<leader>uF` | Deep focus mode |
| `s` | Flash jump |
| `<C-\>` | Toggle terminal |

### Navigation shortcuts

| Key | Action |
|-----|--------|
| `]d` / `[d` | Next / prev diagnostic |
| `]h` / `[h` | Next / prev git hunk |
| `]b` / `[b` | Next / prev buffer |
| `]t` / `[t` | Next / prev TODO |
| `]f` / `[f` | Next / prev function (treesitter) |
| `]c` / `[c` | Next / prev class (treesitter) |

---

## Themes

Configure in `lua/core/theme.lua`:

```lua
M.config = {
  theme     = "tokyonight",   -- active theme
  day_start = 7,              -- hour when light mode activates (0-23)
  day_end   = 19,             -- hour when dark mode activates (0-23)
  fallback  = "default",      -- used when primary theme is unavailable
}
```

Available themes:

| Name | Style |
|------|-------|
| `tokyonight` | Deep blue-purple, muted contrast (default) |
| `catppuccin` | Warm pastel, excellent plugin integration |
| `rose-pine` | Earthy rose/grey, minimal |
| `kanagawa` | Japanese ink-wash inspired, Dragon variant |
| `gruvbox-material` | Warm amber, retro comfort |
| `solarized` | Precision-tuned contrast, dark and light |
| `solarized-osaka` | Solarized with Tokyo influence |

Toggle at runtime: `<leader>ut`

Switch programmatically:

```lua
require("core.theme").switch("catppuccin")
```

### Auto dark/light

The theme automatically selects dark or light mode based on the hour.
`day_start = 7` and `day_end = 19` means light mode from 07:00–19:00,
dark outside those hours. `toggle()` pins the current mode for the
session, overriding the time-based selection until restart.

---

## Dashboard & Splash Screen

The snacks.nvim dashboard opens on startup with a logo header, session quote,
quick-action keys, and lazy.nvim startup stats.

### Layout

```
╔══════════════════════════════════════════════════════════════╗
║  NVIM-IDE logo + version stamp                               ║
╚══════════════════════════════════════════════════════════════╝

  "Session quote — one per session, time-of-day weighted."
  — Author

  [n] New Buffer   [f] Find Files   [r] Recent Files  ...
  Loaded N plugins in X ms
```

The header (logo + quote) is built once by `build_header()` in `ui.lua` when
the dashboard first renders. It calls `require("core.util.quotes").session()`
which selects a quote on first call, caches it in `vim.g`, and returns the
same quote for the rest of the session — so re-opening the dashboard with
`<leader>l` → snacks shows the same quote.

### Quote system — `lua/core/util/quotes.lua`

36 quotes across 6 categories: `craft`, `debug`, `simplicity`, `growth`,
`humor`, `systems`. Selection uses weighted random (craft and debug appear
twice as often). Time-of-day weighting applies: growth quotes in the morning,
humor late at night, balanced during work hours.

```lua
local Q = require("core.util.quotes")

Q.random()           -- random from all categories
Q.random("craft")    -- random from one category
Q.weighted()         -- time-of-day weighted selection (recommended)
Q.session()          -- one quote per session (cached in vim.g)
Q.formatted(q)       -- returns "text\n— author" display string
```

> **Note:** `lua/core/scanner.lua` is no longer loaded and can be deleted:
> ```bash
> rm ~/.config/nvim/lua/core/scanner.lua
> ```

---

## Deep Focus Mode

`<leader>uF` activates deep focus, which:

- Sets `laststatus = 0` (hides statusline)
- Sets `showtabline = 0` (hides bufferline)
- Disables `ruler`, `signcolumn`, `number`, `relativenumber`, `cursorline`
- Enables `TwilightEnable` (dims inactive code)
- Toggles `ZenMode` (centred buffer with padding)

All options are snapshot before activation and restored exactly on exit —
including when Neovim exits mid-session (`VimLeavePre` handler). The state
is tracked in `focus.is_active()`.

`<leader>uz` activates ZenMode alone, without stripping the full chrome.
`<leader>uT` activates Twilight alone.

---

## Updating

```vim
:Lazy update          " update all plugins
:MasonUpdate          " update installed Mason packages
:TSUpdate             " update Treesitter parsers
```

After a major Neovim upgrade:

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim
nvim
# Then inside Neovim:
# :MasonInstallAll
# :TSUpdate
# :checkhealth
```

---

## Troubleshooting

### Quick diagnosis

```vim
:Health           " custom health summary (version, LSP, memory)
:checkhealth      " full Neovim health check
:checkhealth lsp  " LSP-specific health
:LspInfo          " active LSP clients for current buffer
:Mason            " Mason package status
```

### Common issues

| Symptom | Diagnosis | Fix |
|---------|-----------|-----|
| Highlight overrides not applying | stale module name after v2.4.0 rename | Fixed in v2.4.0 — `init.lua` calls `require("core.highlights").apply()` |
| LSP not attaching | Binary missing or Mason install failed | `:MasonInstallAll` · `:checkhealth mason` |
| `<leader>'r` gives "no runner" on JSX/TSX | `javascriptreact`/`typescriptreact` not in `run_tests()` dispatch | Fixed in v2.4.1 — update `runner.lua` |
| Plugins not loading at all | Corrupted lazy state | `rm -rf ~/.local/share/nvim ~/.cache/nvim && nvim` |
| Dashboard not opening | snacks.nvim needs restart after config change | Full restart required — `:luafile` is insufficient for `ui.lua` |
| Focus mode leaves ZenMode open after exit | ZenMode toggle desync in `focus.lua` | Fixed in v2.4.1 — update `focus.lua` |
| Theme toggle broken after `M.switch()` | `manual_override` was not cleared | Fixed in v2.4.0 — `theme.lua` |
| Completion not working | blink.cmp version mismatch | `:Lazy update` · check `completion.lua` pins `version = "1.*"` |
| COBOL LSP not attaching | Not in Mason registry | `npm i -g @broadcommfd/cobol-language-support` then restart |
| Kotlin gradlew not executable | Missing executable bit | `chmod +x gradlew` in project root |
| Large file extremely slow | Treesitter and folding enabled | Expected — `autocmds.lua` disables both above 500KB automatically |

### Neovim version too old (openSUSE)

```bash
sudo zypper ar -f \
  https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ \
  editors
sudo zypper ref && sudo zypper in neovim
```

### Nuclear reset

```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim ~/.local/state/nvim
nvim  # lazy.nvim re-bootstraps from scratch
```

---

## Changelog

### v2.4.1

**Bug fixes**
- `bootstrap.lua` — version stamp corrected to 2.4.0 (was frozen at 2.3.16; affected dashboard header, `:Health`, startup notification)
- `runner.lua` — `run_tests()` now handles `javascriptreact` and `typescriptreact`; `<leader>'t` in `.jsx`/`.tsx` no longer shows "no test runner"
- `focus.lua` — ZenMode desync resolved; replaced `:ZenMode` toggle with `zm.open()`/`zm.close()` API calls so focus mode is immune to independent `<leader>uz` usage
- `dap.lua` — dead `cmd` variable in `setup_ruby()` removed; single `args` local used in callback, eliminating silent drift
- `python.lua` — `vim.o.operatorfunc` saved and restored after iron send-motion keymap; prevents iron handler leaking into subsequent `g@` invocations

**Boilerplate / consistency**
- `cobol.lua` — snippets converted to `require("core.util.snippets").load()` pattern; consistent with c.lua, fortran.lua, vhdl.lua; gains LuaSnip-unavailable fallback
- `runner.lua` — duplicate `c`/`cpp` CTest blocks replaced with shared `ctest_thunk` closure; JS/TS variants share a single `js_test` closure
- `quotes.lua` — `math.randomseed()` called once at module load; `seed_rng()` function and two call sites removed
- `packages.lua` — `jdtls` removed from `M.lspconfig` (managed by nvim-jdtls in java.lua, not lspconfig); remains in `M.mason.lsp` for installation
- `shared.lua` — `treesitter()` deduplicates parser names via seen-set; prevents duplicate entries when multiple lang specs overlap
- `test.lua` — `vim.schedule()` removed from `load_adapter` notify; opts() runs in main loop, the defer was unnecessary
- `treesitter.lua` — `vim.tbl_flatten` replaced with `vim.iter():flatten():totable()`; eliminates deprecation warning on Neovim 0.11+

**Dashboard**
- `scanner.lua` — animation overlay removed; quote now rendered statically in the snacks dashboard header via `build_header()` in `ui.lua`; appears instantly on startup with no timers, no float window, no race conditions; `lua/core/scanner.lua` is now unreferenced and can be deleted

**Documentation**
- `README.md` — "Known Issues & Pending" section removed; all 4 listed bugs were already fixed in v2.4.0; Troubleshooting table updated with correct fix references

### v2.4.0

**Bug fixes**
- `java.lua` — OS-aware jdtls config directory (Linux/macOS/Windows)
- `runner.lua` — JSX/TSX tmpfile extension fix for selection runner
- `lsp.lua` — eslint_d double-registration via `merge_linters()` helper
- `specs/hud.lua` — restored (was silently missing; noice, oil, zen, twilight were not loading)

**Architecture**
- `core/hud.lua` renamed to `core/highlights.lua` — clearer purpose
- `packages.lua` introduced as single source of truth for Mason/lspconfig lists
- `commands.lua` and `lsp.lua` now source from `packages.lua`
- `blame.nvim` moved from hud.lua to `git.lua` — correct ownership
- `<leader>xT` (TodoTelescope) moved to `editor.lua` — correct ownership
- `sql.lua` deleted — `database.lua` owns all SQL configuration

**Boilerplate reduction**
- `shared.treesitter()` helper eliminates 15 identical treesitter extension blocks
- 7 lang files: `toggleterm optional=true` removed
- `typescript.lua` — `deepcopy` removed (was unnecessary)
- `shared.lua` — `JS_TS_FT` literal table replaces repeated inline lists

**Performance**
- `lsp.lua` — `get_capabilities()` memoised (called once, cached)
- `ui.lua` — `get_active()` cached at module load
- `rust.lua` — `detect_edition()` cached per `Cargo.toml` path
- `autocmds.lua` — dead `pcall` on `tabpagenr` removed
- `mason.lua` — `bin_ok` uses `executable()` only (not `filereadable`)

**Dead code removal**
- `go.lua` — dead commented conform spec removed
- `advanced.lua` — nvim-ufo IIFE unfolded to readable config
- `keymaps.lua` — `_family`/`_dap_maps`/`_fkey_maps` indirection removed
- `runner.lua` — `find_ancestor_with` consolidated into `path.find_root`

**Hardening**
- `test.lua` — internal neotest API call removed; stable `opts` snapshot fallback

### v2.3.16

`path.lua` headless pcall · `runner.lua` selection clamp + executable guards +
non-blocking VHDL · `term.lua` nil-root guard · `java.lua` sha256 workspace
hash · `test.lua` neotest-rust live adapter state · `dap.lua` codelldb
existence check · `autocmds.lua` RestoreCursor correct window ·
`kotlin.lua` gradlew executable bit · `bootstrap.lua` partial clone cleanup ·
`lsp.lua` double-pcall blink caps + format_on_save buffer guard ·
`focus.lua` VimLeavePre restore · `theme.lua` ColorScheme cache sync ·
`commands.lua` MasonInstallAll mutex · `python.lua` DAP keymap guard ·
`keymaps.lua` ww/wq pcall · `ui.lua` open_win pcall + timer idle bail ·
`treesitter.lua` fs_stat per-buffer cache

### v2.3.15

`focus.lua` boolean restore · `dap.lua` dead Python section removed ·
`rust.lua` rustfmt conform · `python.lua` subprocess-free probe ·
`lsp.lua` shellcheck guard · `advanced.lua` kotlin neogen ·
`zig.lua` once=true removed · `keymaps.lua` duplicate xx/xu removed ·
`commands.lua` ToggleAutoformat + gofumpt ·
`autocmds.lua` TrimWhitespace buftype guard

### v2.3.14

`runner.lua` unified through term · `keymaps.lua` harpoon_call removed ·
`focus.lua` apply_spec unified · `test.lua` jest_cmd cleaned ·
`lsp.lua` merge_linters helper · `python.lua` sync probe ·
`bootstrap.lua` deduped · `ui.lua` version fallback

### v2.3.0

snacks.nvim dashboard · matrix rain animation engine

### v2.0

Initial public release

---

*Documentation last updated: v2.4.0*
