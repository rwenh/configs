# INSTALLATION INSTRUCTIONS — v2.3.0

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
32. **tailwind-tools duplicate** — css.lua is sole owner; web.lua entry to be removed in next pass

---

## What Was Changed (v2.2.1 → v2.2.2 – v2.2.5)

33. **git.lua** — octo.nvim added for GitHub PR/issue/review workflow
34. **autocmds.lua** — TrimWhitespace replaced with Lua line iterator (no undo entry when nothing changed)
35. **css.lua / html.lua** — `vim.lsp.config()` guarded behind `nvim-0.11` check; falls back to lspconfig on 0.10
36. **completion.lua** — `version=false`; all four default blink.cmp sources explicitly declared
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

## What Was Changed (v2.2.5 → v2.3.0) — Current

52. **ui.lua — dashboard-nvim replaced with snacks.nvim**
    - `nvimdev/dashboard-nvim` removed
    - `folke/snacks.nvim` added with `dashboard` module enabled
    - Animated matrix rain header: real-time column-drop animation via `vim.uv` timer (80 ms ticks)
    - Each of 64 rain columns has independent head position, speed, and trail length
    - Characters drawn from katakana + hex + symbols pool; trail brightness decays per distance
    - `MatrixHead` / `MatrixTrail` / `MatrixMid` / `MatrixDim` highlight groups defined on ColorScheme autocmd (survive theme toggle)
    - Timer starts on `SnacksDashboardOpened`, stops on `SnacksDashboardClosed`
    - Logo + rotating quote rendered below rain, above menu
    - All other snacks modules disabled — only dashboard enabled
    - lualine `disabled_filetypes` updated to include `snacks_dashboard`

53. **README.md** — Known Issues table added; structure updated for snacks; v2.3.0
54. **KEYMAP_REFERENCE.md** — `<leader>un` collision resolved (sole mapping: Noice dismiss); Elixir keymaps documented; session keymaps added; treesitter text object keymaps added; `<leader>'P/w/W` added; `<leader>,i` inlay hints added
55. **INSTALL.md** — this document updated

---

## Known Issues Being Tracked for v2.3.1

| Module | Issue |
|--------|-------|
| `completion.lua` | `version=false` tracks HEAD — pin to latest stable blink.cmp tag |
| `lsp.lua` | nvim-0.11 double-attach possible on servers also managed by mason-lspconfig default handler |
| `dap.lua` | Breakpoint restore on large files lands on wrong lines (treesitter not yet parsed) |
| `python.lua` | iron.nvim REPL keymaps set globally at load, not per-buffer |
| `test.lua` | neotest-rust race condition if rustaceanvim not attached on first Rust file open |
| `workflow.lua` | `overseer.run_template({ name="build" })` throws unhandled error when no template matches |
| `autocmds.lua` | TrimWhitespace calls `nvim_buf_set_lines` per line — should batch in one call |
| `treesitter.lua` | `foldexpr` uses deprecated v3 string — should be `v:lua.vim.treesitter.foldexpr()` |
| `advanced.lua` | `close_fold_kinds` renamed to `close_fold_kinds_for_ft` in recent nvim-ufo |

---

## File Structure

```
~/.config/nvim/
├── init.lua                          ← v2.3.0 (unchanged from 2.2.5)
└── lua/
    ├── core/
    │   ├── autocmds.lua              ← unchanged
    │   ├── bootstrap.lua             ← unchanged
    │   ├── commands.lua              ← unchanged (v2.2.4)
    │   ├── focus.lua                 ← unchanged
    │   ├── hud.lua                   ← unchanged
    │   ├── keymaps.lua               ← unchanged (v2.2.4)
    │   ├── options.lua               ← unchanged (v2.2.4)
    │   ├── theme.lua                 ← unchanged (v2.2.2)
    │   └── util/
    │       ├── path.lua              ← unchanged (v2.2.3)
    │       └── runner.lua            ← unchanged (v2.2.3)
    └── plugins/
        ├── init.lua                  ← unchanged
        └── specs/
            ├── init.lua              ← unchanged (import order preserved)
            ├── advanced.lua          ← unchanged (v2.2.4)
            ├── completion.lua        ← unchanged (v2.2.5)
            ├── dap.lua               ← unchanged (v2.2.4)
            ├── editor.lua            ← unchanged (v2.2.4)
            ├── git.lua               ← unchanged (v2.2.2)
            ├── hud.lua               ← unchanged (v2.2.4)
            ├── lsp.lua               ← unchanged (v2.2.4)
            ├── test.lua              ← unchanged (v2.2.5)
            ├── treesitter.lua        ← unchanged (v2.2.4)
            ├── ui.lua                ← UPDATED (v2.3.0) — snacks.nvim dashboard
            ├── workflow.lua          ← unchanged (v2.2.5)
            └── lang/
                ├── c.lua             ← unchanged
                ├── cobol.lua         ← unchanged (v2.2)
                ├── cpp.lua           ← unchanged (v2.2)
                ├── css.lua           ← unchanged (v2.2.5)
                ├── database.lua      ← unchanged (v2.2.3)
                ├── elixir.lua        ← unchanged (v2.2.3)
                ├── fortran.lua       ← unchanged (v2.2)
                ├── go.lua            ← unchanged
                ├── html.lua          ← unchanged (v2.2.5)
                ├── java.lua          ← unchanged (v2.2.3)
                ├── javascript.lua    ← unchanged (v2.2)
                ├── kotlin.lua        ← unchanged (v2.2.3)
                ├── markdown.lua      ← unchanged (v2.2.3)
                ├── python.lua        ← unchanged (v2.2.4)
                ├── rest.lua          ← unchanged (v2.2.5)
                ├── ruby.lua          ← unchanged
                ├── rust.lua          ← unchanged (v2.2.3)
                ├── sql.lua           ← unchanged
                ├── typescript.lua    ← unchanged (v2.2)
                ├── vhdl.lua          ← unchanged (v2.2.3)
                ├── web.lua           ← unchanged (v2.2.1)
                └── zig.lua           ← unchanged (v2.2)
```

> **Import order in specs/init.lua is load-order sensitive:**
> `web` must precede `html`/`css`; `database` must precede `sql`.
> Do not reorder these imports.

---

## Installation Steps

1. **Backup your current config:**
   ```bash
   mv ~/.config/nvim ~/.config/nvim.backup
   mv ~/.local/share/nvim ~/.local/share/nvim.backup
   mv ~/.cache/nvim ~/.cache/nvim.backup
   ```

2. **Place files into correct locations** using the structure above.

3. **Start Neovim:**
   ```bash
   nvim
   ```
   Lazy.nvim installs all plugins including snacks.nvim automatically.

4. **Install LSP servers and tools:**
   ```vim
   :MasonInstallAll
   ```

5. **Verify:**
   ```vim
   :checkhealth
   :Lazy health
   ```

## Troubleshooting

**Dashboard rain not animating** — snacks.nvim may not have fired `SnacksDashboardOpened` if it loaded before the autocmd registered. Run `:lua Snacks.dashboard.open()` to force-reopen.

**snacks.nvim dashboard blank** — ensure `folke/snacks.nvim` is in your lazy spec with `lazy=false, priority=90`. Run `:Lazy install` if newly added.

**Other errors on startup:**
1. Delete `~/.local/share/nvim` and `~/.cache/nvim`
2. Restart Neovim — lazy.nvim reinstalls everything
3. Run `:MasonInstallAll`
4. Run `:checkhealth` and address warnings
