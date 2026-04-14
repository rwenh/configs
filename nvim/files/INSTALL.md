# INSTALLATION INSTRUCTIONS — v2.3.8

## What Was Changed (v2.0 → v2.1)

1. **better-escape.nvim restored** — handles `jk`/`kj` at Lua level with zero lag
2. **blink.cmp capabilities** — moved from completion.lua into lsp.lua
3. **LSP optional server detection** — replaced fragile gsub heuristic with explicit binary name table
4. **VHDL keymaps** — `<leader>vs*` → `<leader>vh*`
5. **COBOL keymaps** — `<leader>cb` → `<leader>cob`
6. **database.lua** — promoted to full dadbod owner; sql.lua defers to it
7. **css.lua / html.lua / cpp.lua / fortran.lua / kotlin.lua** — fully fledged
8. **javascript.lua / typescript.lua** — ESLint, neotest adapters, typescript-tools
9. **ruby.lua** — replaced unmaintained nvim-dap-ruby with direct rdbg DAP config
10. **python.lua** — unified all keymaps under `<leader>py*`
11. **zig.lua** — DAP config via codelldb/lldb
12. **markdown.lua** — table mode, paste-image support
13. **test.lua** — neotest adapters for Ruby, Elixir, JS/TS, Kotlin/Java
14. **commands.lua (MasonInstallAll)** — rubocop, ktlint, java-debug-adapter, java-test, vhdl-ls, ruff, eslint_d, htmlhint, stylelint, fprettify, clang-format
15. **ui.lua (which-key)** — language prefix groups
16. **theme.lua** — background resolution result cached
17. **runner.lua** — run_selection covers all 9 languages
18. **specs/init.lua** — import order fixed: web before html/css, database before sql

---

## What Was Changed (v2.1 → v2.1.1)

19. **init.lua** — version field corrected; `core.autocmds` loads synchronously
20. **init.lua** — startup stats hooked to `LazyDone` event (was 150ms timer)
21. **init.lua** — hard-fail guard after plugins load
22. **Fortran keymaps** — `<leader>fo*` → `<leader>ft*` (freed Telescope `<leader>fo`)
23. **REST keymaps** — `<leader>h*` → `<leader>re*` (freed Harpoon `<leader>h`)

---

## What Was Changed (v2.1.1 → v2.2)

24. **Double-LSP clients eliminated** — rustaceanvim, typescript-tools, go.nvim, elixir-tools all set their respective servers to disabled so lsp.lua owns them exclusively
25. **`optional=true` config→init migrations** — css, html, javascript, typescript, kotlin
26. **Duplicate neotest registrations removed** — test.lua is sole owner of all adapters
27. **DAP config→init migrations** — ruby, zig
28. **Shell escaping fixes** — cobol, fortran, vhdl
29. **Various fixes** — c.lua inlay hints, cpp clang-format naming, database omnifunc, java workspace hash, kotlin build tool detection, markdown preview build, python DAP scoping, rust crates keymaps scoped to Cargo.toml, sql formatter docs, typescript TSTools filetype, vhdl vsg conform, web autotag opts, zig fmt

---

## What Was Changed (v2.2 → v2.2.1)

30. **KEYMAP_REFERENCE.md** — Rust keymaps corrected; crates.nvim removed from docs; Markdown keymaps corrected; stale `<leader>uu` undo tree entry removed from UI Toggles
31. **web.lua colorizer conflict** — `norcalli/nvim-colorizer.lua` removed; `NvChad/nvim-colorizer.lua` in advanced.lua is canonical
32. **tailwind-tools duplicate** — css.lua is sole owner; web.lua entry removed

---

## What Was Changed (v2.2.1 → v2.2.2 – v2.2.5)

33. **git.lua** — octo.nvim added for GitHub PR/issue/review workflow
34. **autocmds.lua** — TrimWhitespace replaced with Lua line iterator (no undo entry when nothing changed)
35. **css.lua / html.lua** — `vim.lsp.config()` guarded behind `nvim-0.11` check; falls back to lspconfig on 0.10
36. **completion.lua** — version pinned to `"1.*"`; all four default blink.cmp sources explicitly declared
37. **advanced.lua** — nvim-ufo `provider_selector` guards `large_file` buffers
38. **dap.lua** — `load_breakpoints()` BufReadPost rewrite; `mason-nvim-dap handlers=nil`
39. **lsp.lua** — `nvim-0.11` guard on all `vim.lsp.config()+vim.lsp.enable()` calls; `format_on_save` always returns nil or complete table
40. **hud.lua** — mini.animate scroll guard for `large_file` buffers
41. **python.lua** — iron `repl_open_cmd` wrapped in function (deferred evaluation)
42. **treesitter.lua** — tag pin removed; dead `fold` key removed
43. **rest.lua** — `version="^2"` removed; commands updated for v3 API
44. **workflow.lua** — OverseerBuild removed; overseer-community-tasks removed; builtin templates only
45. **test.lua** — duplicate keymaps removed; neotest-rust require path corrected
46. **editor.lua** — telescope-fzf-native cmake/make fallback; flash.nvim specced
47. **markdown.lua** — `render_modes` "c" removed
48. **database.lua** — "psql" filetype corrected to "sql"
49. **java.lua** — re-attach guard uses `vim.b[e.buf]`; workspace uses full path hash
50. **kotlin.lua** — DRY shared helper; shellescape fixes
51. **commands.lua** — MasonInstallAll package names corrected (elixir-ls-debugger removed, cobol-language-server removed, vhdl-ls → rust_hdl, kotlin-debug-adapter removed)

---

## What Was Changed (v2.2.5 → v2.3.0)

52. **ui.lua — dashboard-nvim replaced with snacks.nvim**
    - `nvimdev/dashboard-nvim` removed; `folke/snacks.nvim` added with `dashboard` module enabled
    - Animated matrix rain header: real-time column-drop animation via `vim.uv` timer (80 ms ticks)
    - Each of 64 rain columns has independent head position, speed, and trail length
    - Characters drawn from katakana + hex + symbols pool; trail brightness decays per distance
    - `MatrixHead` / `MatrixTrail` / `MatrixMid` / `MatrixDim` highlight groups defined on ColorScheme autocmd (survive theme toggle)
    - Timer starts on `SnacksDashboardOpened`, stops on `SnacksDashboardClosed`
    - Logo + rotating quote rendered below rain, above menu; all other snacks modules disabled
    - lualine `disabled_filetypes` updated to include `snacks_dashboard`
53. **README.md** — Known Issues table added; structure updated for snacks; v2.3.0
54. **KEYMAP_REFERENCE.md** — `<leader>un` collision resolved; Elixir keymaps documented; session, treesitter text-object, `<leader>'P/w/W`, and `<leader>,i` keymaps added

---

## What Was Changed (v2.3.0 → v2.3.1 – v2.3.4)

55. **completion.lua** — version pinned to `"1.*"`; `<Tab>`/`<S-Tab>` stripped to snippet-only; `<C-n>`/`<C-j>` and `<C-p>`/`<C-k>` as exclusive menu-nav aliases; `cmdline` source removed from `sources.default`
56. **advanced.lua** — nvim-ufo: `close_fold_kinds` renamed to `close_fold_kinds_for_ft`; neogen marked as PRIMARY owner; cpp.lua and python.lua extend via `optional=true`; `<leader>xg` keymap lives here only
57. **python.lua** — iron.nvim REPL keymaps moved from `keymaps={}` in setup to per-buffer FileType autocmd; neogen spec `optional=true`; annotation convention corrected to `google_docstrings`
58. **cpp.lua** — neogen spec `optional=true`; duplicate `<leader>ccd` key entry removed; `config()` removed from optional extension spec
59. **dap.lua** — `load_breakpoints()` hooked to `User LazyDone` (was fragile `vim.defer_fn(100ms)`); JS/TS `pick_process` wrapped in function (lazy eval); `mason-nvim-dap handlers` key removed entirely to restore default adapter setup
60. **lsp.lua** — mason-lspconfig `handlers = { function() end }` added to suppress double-attach on Nvim 0.11; `vim.diagnostic.goto_next/prev` replaced with `vim.diagnostic.jump()` + 0.10 fallback; `<leader>,r` uses `:IncRename` shim for cmd-based lazy loading; `<leader>,o` changed from AerialToggle to `Trouble lsp_document_symbols`
61. **hud.lua** — TodoTelescope key moved from `<leader>ft` to `<leader>xT` (resolved conflict with Fortran `<leader>ft*` group)
62. **keymaps.lua** — removed duplicates owned by plugin `keys=` specs (`<leader>ee/ef/ec/er`, `<leader>uz`, `<leader>uT`, `<leader>eo`, `<leader>.p/.r/.B`); `<leader>ob` overseer fallback simplified; `<leader>xg` removed (owned by advanced.lua); `<leader>xx` fixed to `Trouble diagnostics toggle`
63. **autocmds.lua** — TrimWhitespace batched into single `nvim_buf_set_lines` call; `checktime` uses `vim.bo.buftype` not `vim.o.buftype`; LargeFile: `vim.treesitter.stop()` added; BufWinEnter re-stamps `foldmethod=manual` for large files
64. **treesitter.lua** — `foldexpr` updated to `v:lua.vim.treesitter.foldexpr()`
65. **test.lua** — neotest-rust deferred via one-shot FileType autocmd + `vim.schedule()`; neotest-go changed from raw module to `require("neotest-go")({})` constructor call
66. **runner.lua** — rust runner walks up 5 levels for Cargo.toml; elixir/ruby `run_tests()` prefixed with `cd <root> &&`; `bun.lock` text lockfile added alongside `bun.lockb`; path cache normalised with `fnamemodify(":p")`
67. **workflow.lua** — `overseer.run_template()` wrapped in pcall with OverseerRun fallback for both build and shell shortcuts
68. **options.lua** — `sessionoptions`: "terminal" and "help" removed; `auto_cd_root` initialised to `false` (opt-in)

---

## What Was Changed (v2.3.4 → v2.3.5)

69. **keymaps.lua — Spectre pcall guards** — `require("spectre")` was called bare in the `<leader>/s`, `<leader>/w`, and `<leader>/f` handlers. `nvim-spectre` only loads on `:Spectre` (cmd=); pressing these keys before that fired an unhandled `module not found` error. All three handlers now use `pcall` with a clear warning message.

70. **keymaps.lua — DAP keymaps pcall guards** — All inline `require("dap")`, `require("dapui")`, and `require("dap.ui.widgets")` calls in the `<leader>;*` and `F5`–`F11` maps were bare. If DAP failed to load (Mason not done, missing adapter) any of these keys threw an unhandled stack trace. All DAP keymaps now use a consistent pcall pattern with a `[dap] not loaded` notification on failure.

71. **keymaps.lua — `<leader>sm` MaximizerToggle removed** — mapped to `:MaximizerToggle` but no maximizer plugin was ever specced in the project. Pressing `<leader>sm` always silently failed. Replaced with a native Lua toggle using `vim.w._maximized` + `wincmd | wincmd _` / `wincmd =` — zero plugin dependency, correct per-window state.

72. **lsp.lua — conform v6 API fix** — The `fmt()` function inside `LspAttach` used `lsp_fallback = true` (conform v5 key, silently ignored on v6). Updated to `lsp_format = "fallback"`. The `format_on_save` closure was already correct; only the inline LSP-attach format handler was missed.

73. **commands.lua — conform v6 API fix** — Both call sites in the `:Format` command (ranged and whole-buffer) updated from `lsp_fallback = true` to `lsp_format = "fallback"` to match the lsp.lua fix and conform v6.

74. **test.lua — neotest-elixir constructor** — `neotest-elixir` was returned as a raw module table with a comment claiming "no constructor". This was true of `<v0.2` but neotest-elixir v0.2+ exports a callable. Changed to `require("neotest-elixir")({})` — identical fix to neotest-go in v2.3.2. Elixir tests were silently not running through neotest.

75. **ui.lua — `LOGO_WIDTH` dead variable removed** — declared as `#LOGO_LINES[1]` (byte length, not display columns) with a misleading comment calling box-drawing chars "ASCII". The variable was never read; all decode logic correctly uses `LOGO_COLS_DISPLAY = 64`. Removed entirely.

76. **ui.lua — drain winblend flash fixed** — `trigger_drain()` now resets both rain floats to `winblend=0` before starting the drain timer. Previously the floats held `winblend=100` from creation; the first drain frame computed `blend=0` and snapped the window from transparent to opaque for one frame, causing a visible dark-green background flash at the moment the user pressed a key. The `0→100` fade ramp in `frame()` now begins from the correct baseline.

---

## What Was Changed (v2.3.5 → v2.3.6)

77. **keymaps.lua — Harpoon pcall guards** — All six `require("harpoon")` calls in `<leader>ha/hm/h1–h4` and `<M-1–4>` were bare. If harpoon fails to load (lazy-load not yet triggered) pressing any of these threw an unhandled stack trace. All wrapped in pcall, consistent with the spectre/dap pattern from v2.3.5.

78. **keymaps.lua — todo-comments pcall guards** — `]t` / `[t` called `require("todo-comments")` bare. Same fix applied.

79. **completion.lua — blink.cmp nav keys "show" removed** — `<C-p>/<C-n>/<C-k>/<C-j>` were mapped to `{ "select_prev/next", "show" }`. The `"show"` action calls `"fallback"` internally when the menu is open, re-invoking native `i-^P/i-^N` and opening a competing popup. Mapped to `{ "select_prev" }` / `{ "select_next" }` only.

80. **dap.lua — `"python"` → `"debugpy"` in ensure_installed** — `mason-nvim-dap ensure_installed` listed `"python"` which is not a Mason registry package name. The correct name is `"debugpy"`. Caused a startup warning on every launch and the Python DAP adapter was never auto-installed. Known since v2.2.4; single-token fix.

---

## What Was Changed (v2.3.6 → v2.3.7)

81. **lsp.lua — elixir-ls wired** — `elixir.lua` disables `elixirls` inside `elixir-tools` with the comment "lsp.lua owns elixirls", but `elixir-ls` was never in `mason-lspconfig ensure_installed` and never had a `servers` table entry. Elixir had no LSP unless the user ran `:MasonInstall` manually. Added `"elixir-ls"` to `ensure_installed` and an `elixirls` entry in the `servers` table.

82. **hud.lua (plugins/specs) — mini.animate opts→config migration** — `require("mini.animate")` was called inside `opts=function()`, which lazy evaluates at spec-parse time before mini.animate is installed. On a fresh install this caused a startup error. All `animate.gen_timing` / `gen_subscroll` calls and the `mouse_scrolled` closure moved into `config()` which only runs after the plugin is confirmed loaded.

---

## What Was Changed (v2.3.8 → v2.3.9) — Current

87. **runner.lua — `run_tests()` JS/TS cd prefix** — `detect_js_test_cmd()` returned a bare package-manager command with no `cd <root> &&` prefix. When the shell's cwd differed from the project root, the package manager couldn't locate `package.json` and failed silently. All JS/TS test commands now prefixed with `cd <root> &&`, consistent with every other language.

88. **commands.lua — MasonInstallAll missing `fortls` and `gopls`** — `lsp.lua` wires both as servers but neither was in `MasonInstallAll`. A fresh install had no way to auto-install them via `:MasonInstallAll`. Both added to the LSP section.

89. **dap.lua — Elixir DAP debugger path resolver** — The fallback chain ended with `exepath("elixir-ls")` which resolves to the LSP binary, not the DAP debugger. Using the LSP binary as a DAP adapter silently failed every Elixir debug session. Resolver now only returns a known DAP-capable path (`debugger.sh` or `elixir-ls-debugger`); falls back to nil with a warning rather than wiring the wrong binary.

90. **lsp.lua — `fortls` added to `mason-lspconfig ensure_installed`** — `fortls` was in the optional servers table (binary-checked at runtime) but absent from `ensure_installed`. A fresh install had no way to auto-install it. Consistent with the elixir-ls fix in v2.3.7.

---

## What Was Changed (v2.3.7 → v2.3.8)

83. **test.lua — neotest-vitest constructor** — `neotest-vitest` was returned as a raw module table. neotest-vitest exports a callable constructor; not invoking it silently gave neotest an invalid adapter object and Vitest tests never ran. Fixed: `require("neotest-vitest")({})` — identical fix to neotest-go (v2.3.2) and neotest-elixir (v2.3.5).

84. **runner.lua — `run_tests()` cd prefix for python/rust/go/zig** — These four commands lacked the `cd <root> &&` prefix that ruby/elixir/kotlin/java already had. `pytest` needs `pyproject.toml`/`setup.cfg`; `cargo test` needs `Cargo.toml`; `go test` needs `go.mod`; `zig build test` needs `build.zig`. All four now cd to project root before running.

85. **keymaps.lua — Overseer duplicate maps removed** — `<leader>ot`, `<leader>or`, and `<leader>ob` were registered both here and in `workflow.lua`'s `keys=` table. `workflow.lua` is the sole owner (handles lazy-loading and the smart `run_template` fallback for `<leader>ob`). The duplicates here caused which-key to list each entry twice and silently overwrote `workflow.lua`'s smart build logic.

86. **treesitter.lua — `"comment"` removed from `ignore_install`** — The comment treesitter parser is required by `todo-comments.nvim` (multiline TODO detection) and `noice.nvim` (`long_message_to_split` preset). Ignoring it silently disabled multiline todo highlighting. Only `"vim"` remains in `ignore_install`.

---

## Known Issues (v2.3.9)

| Module | Issue | Since |
|--------|-------|-------|
| `dap.lua` | Breakpoint restore on large files lands on wrong lines — treesitter not yet parsed at restore time | v2.2.4 |
| `test.lua` | neotest-rust race condition if rustaceanvim not yet fully attached on first Rust file open | v2.3.1 |
| `runner.lua` | `run_tests()` has no entry for `c`, `cpp`, `fortran`, `vhdl`, `cobol` — `<leader>'t` in those filetypes notifies "No test runner" | v2.0 |
| `options.lua` | `matchparen` disabled with no replacement — cursor-position bracket matching is fully off | v2.0 |

### Issues resolved this release (v2.3.9)

| Issue | Fix |
|-------|-----|
| JS/TS `run_tests()` missing `cd <root> &&` prefix | #87 |
| MasonInstallAll missing `fortls` and `gopls` | #88 |
| Elixir DAP resolver fell back to LSP binary as DAP adapter | #89 |
| `fortls` absent from `mason-lspconfig ensure_installed` | #90 |

### Issues resolved in v2.3.8

| Issue | Fix |
|-------|-----|
| neotest-vitest returned as raw module, not adapter | #83 |
| `run_tests()` missing cd prefix for python/rust/go/zig | #84 |
| Overseer keymaps duplicated in keymaps.lua and workflow.lua | #85 |
| `"comment"` parser in `ignore_install` broke multiline TODOs | #86 |

### Issues resolved in earlier releases

| Issue | Resolved |
|-------|----------|
| Spectre keymaps crash without pcall | v2.3.5 |
| DAP keymaps crash without pcall | v2.3.5 |
| `<leader>sm` maps to missing MaximizerToggle plugin | v2.3.5 |
| `lsp_fallback` silently ignored by conform v6 (lsp.lua) | v2.3.5 |
| `lsp_fallback` silently ignored by conform v6 (commands.lua) | v2.3.5 |
| neotest-elixir returned as raw module, not adapter | v2.3.5 |
| `LOGO_WIDTH` dead misleading variable | v2.3.5 |
| Drain phase dark-green flash on keypress | v2.3.5 |
| Harpoon / todo-comments keymaps crash without pcall | v2.3.6 |
| blink.cmp `"show"` on nav keys re-invoked native completion popup | v2.3.6 |
| `mason-nvim-dap ensure_installed` listed `"python"` not `"debugpy"` | v2.3.6 |
| elixir-ls never auto-installed or wired in lsp.lua | v2.3.7 |
| mini.animate `require()` in `opts=function()` failed on fresh install | v2.3.7 |
| nvim-0.11 double-attach via mason-lspconfig default handler | v2.3.4 |
| `vim.diagnostic.goto_next/prev` deprecated on Nvim 0.11 | v2.3.3 |
| iron.nvim REPL keymaps leaked globally to all buffers | v2.3.1 |
| neotest-go returned raw module (no constructor call) | v2.3.2 |
| neotest-rust race condition deferred | v2.3.1 |
| overseer `run_template` unhandled error on missing template | v2.3.1 |
| TrimWhitespace N+1 buf_set_lines calls | v2.3.1 |
| `foldexpr` used deprecated vimscript shim | v2.3.1 |
| nvim-ufo `close_fold_kinds` renamed key | v2.3.1 |
| `<leader>ft` conflict between Fortran group and TodoTelescope | v2.3.1b |
| blink.cmp Tab fighting snippet + menu simultaneously | v2.3.4 |
| dap.lua `load_breakpoints()` fragile 100ms timer | v2.3.3 |
| `completion.lua version=false` tracking HEAD | v2.3.1 |

---

## File Structure (v2.3.8)

```
~/.config/nvim/
├── init.lua                          ← v2.3.8
└── lua/
    ├── core/
    │   ├── autocmds.lua              ← v2.3.3
    │   ├── bootstrap.lua             ← v2.1.1
    │   ├── commands.lua              ← v2.3.9  ✦ fortls + gopls in MasonInstallAll
    │   ├── focus.lua                 ← v2.2.2
    │   ├── hud.lua                   ← v2.2.4
    │   ├── keymaps.lua               ← v2.3.8  ✦ overseer duplicates removed
    │   ├── options.lua               ← v2.3.3
    │   ├── theme.lua                 ← v2.2.2
    │   └── util/
    │       ├── path.lua              ← v2.3.2
    │       └── runner.lua            ← v2.3.9  ✦ JS/TS run_tests() cd prefix
    └── plugins/
        ├── init.lua                  ← v2.1.1
        └── specs/
            ├── init.lua              ← v2.1 (import order load-sensitive)
            ├── advanced.lua          ← v2.3.1b
            ├── completion.lua        ← v2.3.6  ✦ blink nav keys "show" removed
            ├── dap.lua               ← v2.3.9  ✦ elixir DAP resolver fixed
            ├── editor.lua            ← v2.2.4
            ├── git.lua               ← v2.2.2
            ├── hud.lua               ← v2.3.7  ✦ mini.animate opts→config
            ├── lsp.lua               ← v2.3.9  ✦ fortls in ensure_installed
            ├── test.lua              ← v2.3.8  ✦ neotest-vitest constructor
            ├── treesitter.lua        ← v2.3.8  ✦ "comment" removed from ignore_install
            ├── ui.lua                ← v2.3.5  ✦ LOGO_WIDTH removed; drain flash
            ├── workflow.lua          ← v2.3.1
            └── lang/
                ├── c.lua             ← v2.2
                ├── cobol.lua         ← v2.2
                ├── cpp.lua           ← v2.3.1
                ├── css.lua           ← v2.2.5
                ├── database.lua      ← v2.2.3
                ├── elixir.lua        ← v2.2.3
                ├── fortran.lua       ← v2.2
                ├── go.lua            ← v2.0
                ├── html.lua          ← v2.2.5
                ├── java.lua          ← v2.2.3
                ├── javascript.lua    ← v2.2
                ├── kotlin.lua        ← v2.2.3
                ├── markdown.lua      ← v2.2.3
                ├── python.lua        ← v2.3.1b
                ├── rest.lua          ← v2.2.5
                ├── ruby.lua          ← v2.0
                ├── rust.lua          ← v2.2.3
                ├── sql.lua           ← v2.0
                ├── typescript.lua    ← v2.2
                ├── vhdl.lua          ← v2.2.3
                ├── web.lua           ← v2.3.1
                └── zig.lua           ← v2.2
```

> **Import order in specs/init.lua is load-order sensitive:**
> `web` must precede `html`/`css`; `database` must precede `sql`.
> Do not reorder these imports.

---

## Installation Steps

1. **Backup your current config:**
   ```bash
   mv ~/.config/nvim ~/.config/nvim.backup.$(date +%Y%m%d)
   mv ~/.local/share/nvim ~/.local/share/nvim.backup.$(date +%Y%m%d)
   mv ~/.cache/nvim ~/.cache/nvim.backup.$(date +%Y%m%d)
   ```

2. **Place files into correct locations** using the structure above.

3. **Start Neovim:**
   ```bash
   nvim
   ```
   lazy.nvim bootstraps and installs all plugins on first launch.

4. **Install LSP servers, DAP adapters, formatters, and linters:**
   ```vim
   :MasonInstallAll
   ```

5. **Verify everything is healthy:**
   ```vim
   :checkhealth
   :Lazy health
   ```

Restart Neovim once Mason finishes to ensure all servers attach cleanly.

---

## Hot-Swap (upgrading without full restart)

For `keymaps.lua` and `commands.lua` — pure Lua, no plugin state:

```vim
:luafile ~/.config/nvim/lua/core/keymaps.lua
:luafile ~/.config/nvim/lua/core/commands.lua
```

For `lsp.lua` — reload the plugin spec:

```vim
:Lazy reload nvim-lspconfig
```

For `test.lua` — reload neotest:

```vim
:Lazy reload neotest
```

For `ui.lua` — the snacks dashboard is `lazy=false`; a full Neovim restart is required for the changes to take effect cleanly.

---

## Troubleshooting

**Plugins not loading / corrupted state**
```bash
rm -rf ~/.local/share/nvim ~/.cache/nvim
nvim   # lazy.nvim reinstalls everything from scratch
```

**LSP not attaching**
```vim
:checkhealth lsp
:LspInfo
```

**Mason package failed to install**
```vim
:MasonInstallAll
:checkhealth mason
```

**Dashboard rain not animating** — snacks.nvim may have loaded before the `SnacksDashboardOpened` autocmd registered. Force-reopen:
```vim
:lua Snacks.dashboard.open()
```

**snacks.nvim dashboard blank** — ensure `folke/snacks.nvim` is specced with `lazy=false, priority=90`. Run `:Lazy install` if newly added.

**Neovim version too old** (openSUSE default repos ship an outdated build)
```bash
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_15.5/ editors
sudo zypper ref && sudo zypper in neovim
```

**Updating plugins, Mason registry, and parsers**
```vim
:Lazy update
:MasonUpdate
:TSUpdate
```
