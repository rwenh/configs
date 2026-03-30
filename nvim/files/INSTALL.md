# INSTALLATION INSTRUCTIONS

## What Was Changed (v2.0 → v2.1)

1. **better-escape.nvim restored** — handles `jk`/`kj` at Lua level with zero timeout lag; native mappings removed from keymaps.lua
2. **blink.cmp capabilities** — moved from completion.lua into lsp.lua where it belongs
3. **LSP optional server detection** — replaced fragile gsub heuristic with explicit binary name table
4. **VHDL keymaps** — `<leader>vs*` → `<leader>vh*` (was conflicting with Python venv selector)
5. **COBOL keymaps** — `<leader>cb` → `<leader>cob` (freed `<leader>cb` for C namespace)
6. **database.lua** — promoted to full dadbod owner (was just a thin cmd alias); sql.lua now defers to it
7. **css.lua / html.lua / cpp.lua / fortran.lua / kotlin.lua** — were empty shells, now fully fledged
8. **javascript.lua / typescript.lua** — added ESLint, neotest adapters, import organizer (typescript-tools)
9. **ruby.lua** — replaced unmaintained nvim-dap-ruby with direct rdbg DAP config
10. **python.lua** — unified all keymaps under `<leader>py*` prefix
11. **zig.lua** — added DAP config via codelldb/lldb
12. **markdown.lua** — added table mode, paste-image support
13. **test.lua** — added neotest adapters for Ruby, Elixir, JS/TS, Kotlin/Java
14. **commands.lua (MasonInstallAll)** — added rubocop, ktlint, elixir-ls-debugger, java-debug-adapter, java-test, vhdl-ls, ruff, eslint_d, htmlhint, stylelint, fprettify, clang-format
15. **ui.lua (which-key)** — added groups for all language prefixes
16. **theme.lua** — background resolution result is now cached
17. **runner.lua** — run_selection now covers all 9 languages that have runners, not just 5
18. **specs/init.lua** — import order fixed: web before html/css, database before sql
    > ⚠️ NOTE: This order is load-order sensitive. web.lua must load before html.lua/css.lua,
    > and database.lua must load before sql.lua. This dependency is intentional — do not
    > reorder these imports.

## What Was Changed (v2.1 → v2.1.1 — Post-Audit Fixes)

19. **init.lua** — version field corrected to "2.1" (was stuck at "2.0")
20. **init.lua** — `core.autocmds` now loads synchronously (before defer_fn).
    Previously deferred with 0ms which caused BufReadPre/BufRead to be missed
    for the first file opened via CLI (e.g. `nvim myfile.lua`).
21. **init.lua** — startup stats notification now hooks into the `LazyDone` User
    event instead of a hardcoded 150ms timer. The timer was fragile on slower
    machines or with many plugins.
22. **init.lua** — added hard-fail guard after `plugins` load. If plugins/init.lua
    fails, init now aborts early with a clear error rather than silently producing
    a broken state (no LSP, no colorscheme, etc.).
23. **Fortran keymaps** — `<leader>fo*` → `<leader>ft*`. The `<leader>fo` prefix
    was shadowing Telescope's "recent files" binding (`<leader>fo`), causing
    which-key to block and Telescope to require an extra keypress.
    Update fortran.lua keymaps accordingly.
24. **REST keymaps** — `<leader>h*` → `<leader>re*`. REST bindings were mixed into
    Harpoon's `<leader>h` namespace with no visual separation, risking future
    suffix collisions. Harpoon retains sole ownership of `<leader>h*`.
    Update rest.lua keymaps accordingly.

## What Was Changed (v2.1.1 → v2.2 — Lang Module Audit)

**Double-LSP clients eliminated** (each was causing two LSP instances per buffer):
- **rust.lua** — `rustaceanvim` now exclusively manages `rust-analyzer`; `rust_analyzer` removed from `lsp.lua`
- **typescript.lua** — `typescript-tools.nvim` now exclusively manages `tsserver`; `ts_ls` removed from `lsp.lua`
- **go.lua** — `go.nvim` set `lsp_cfg = false`; `gopls` solely owned by `lsp.lua`
- **elixir.lua** — `elixir-tools.nvim` set `elixirls.enable = false`; `elixirls` solely owned by `lsp.lua`

**`optional=true` config→init migrations** (config on optional specs never ran):
- `css.lua` — cssmodules_ls registration, stylelint linter
- `html.lua` — htmlhint linter, html LSP settings
- `javascript.lua` — eslint_d linter
- `typescript.lua` — eslint_d linter
- `kotlin.lua` — ktlint linter

**Duplicate neotest adapter registrations removed** (test.lua is the central owner):
- `javascript.lua`, `typescript.lua` — vitest + jest removed
- `kotlin.lua` — neotest-java removed
- `ruby.lua` — neotest-rspec removed
- `elixir.lua` — neotest-elixir spec removed entirely
- `python.lua` — empty no-op neotest spec removed

**DAP config→init migrations** (optional=true config never ran):
- `ruby.lua` — removed entirely (dap.lua already owns Ruby adapter)
- `zig.lua` — moved to init with FileType autocmd

**Shell escaping fixes** (raw paths broke on spaces in filenames):
- `cobol.lua` — cobc compile/check commands
- `fortran.lua` — gfortran build/check commands
- `vhdl.lua` — ghdl -e and -r entity name from user input

**LSP ownership fixes** (opts.servers is LazyVim API, not compatible here):
- `html.lua` — html server settings moved to `vim.lsp.config()` call in init

**rustaceanvim** — `vim.g.rustaceanvim` moved from `config` to `init` (read at load time, not after)

**Other fixes:**
- `c.lua` — stale `inlay_hints` API replaced with native `vim.lsp.inlay_hint.enable()`
- `cpp.lua` — `clang_format` (underscore) corrected to `clang-format` (hyphen); CMake keys now available in cmake filetype too
- `database.lua` — dadbod-completion had no lazy trigger; omnifunc never registered
- `go.lua` — added `build` step for go.nvim; removed duplicate gopls settings
- `java.lua` — `start_or_attach` moved into FileType autocmd; nil root_dir guard; glob bundle fix; `add_commands()` removed (deleted upstream)
- `kotlin.lua` — build tool detection now uses `find_root()` instead of cwd-relative `filereadable()`
- `markdown.lua` — `markdown-preview.nvim` build fn cleaned; render-markdown got treesitter dependency
- `python.lua` — DAP keymaps scoped to Python buffers; docstring key `pyd→pyg`; invalid iron `cr` keymap removed
- `ruby.lua` — duplicate rubocop conform spec removed (lsp.lua owns it)
- `rust.lua` — crates.nvim keymaps scoped to Cargo.toml buffer
- `sql.lua` — sqlfmt documented as pip-installed (not Mason-managed)
- `treesitter.lua` — explicit `config` fn to call `nvim-treesitter.configs.setup()`
- `typescript.lua` — TSTools keys now include typescriptreact filetype
- `vhdl.lua` — vsg conform opts use function form; unused `d` node removed
- `web.lua` — autotag double-nested opts fixed; emmet filetypes corrected to actual Neovim names; augroup added
- `zig.lua` — `zig_fmt_autosave=0`; conform zigfmt spec added; lldb empty-string guard fixed
- `runner.lua` — kotlin/java `run_tests()` now uses `find_root()` for build tool detection

## File Structure

```
~/.config/nvim/
├── init.lua                          ← UPDATED (v2.1.1 fixes)
└── lua/
    ├── core/
    │   ├── autocmds.lua              ← unchanged
    │   ├── bootstrap.lua             ← unchanged
    │   ├── commands.lua              ← UPDATED
    │   ├── keymaps.lua               ← UPDATED (jk/kj native mappings removed)
    │   ├── options.lua               ← unchanged
    │   ├── theme.lua                 ← UPDATED
    │   └── util/
    │       ├── path.lua              ← unchanged
    │       └── runner.lua            ← UPDATED
    └── plugins/
        ├── init.lua                  ← unchanged
        └── specs/
            ├── init.lua              ← UPDATED (import order — see note in #18)
            ├── advanced.lua          ← UPDATED (better-escape restored)
            ├── completion.lua        ← UPDATED (capabilities removed, now in lsp.lua)
            ├── dap.lua               ← unchanged
            ├── editor.lua            ← unchanged
            ├── git.lua               ← unchanged
            ├── lsp.lua               ← UPDATED (capabilities here, optional server detection)
            ├── test.lua              ← UPDATED (more neotest adapters)
            ├── treesitter.lua        ← unchanged
            ├── ui.lua                ← UPDATED (which-key lang groups)
            └── lang/
                ├── c.lua             ← unchanged
                ├── cobol.lua         ← UPDATED (keymaps, snippets)
                ├── cpp.lua           ← UPDATED (cmake, docgen, clang-format)
                ├── css.lua           ← UPDATED (tailwind, stylelint, treesitter)
                ├── database.lua      ← UPDATED (full dadbod owner)
                ├── elixir.lua        ← unchanged
                ├── fortran.lua       ← UPDATED (keymaps fo* → ft*, build integration, snippets)
                ├── go.lua            ← unchanged
                ├── html.lua          ← UPDATED (htmlhint, prettier)
                ├── java.lua          ← unchanged
                ├── javascript.lua    ← UPDATED (eslint, neotest, package-info)
                ├── kotlin.lua        ← UPDATED (neotest, build integration)
                ├── markdown.lua      ← UPDATED (table mode, paste-image)
                ├── python.lua        ← UPDATED (unified py* prefix)
                ├── rest.lua          ← UPDATED (keymaps h* → re*, env support)
                ├── ruby.lua          ← UPDATED (rdbg DAP, neotest-rspec)
                ├── rust.lua          ← unchanged
                ├── sql.lua           ← UPDATED (defers dadbod to database.lua)
                ├── typescript.lua    ← UPDATED (typescript-tools, eslint, neotest)
                ├── vhdl.lua          ← UPDATED (keymaps vh*, proper snippets)
                ├── web.lua           ← unchanged
                └── zig.lua           ← UPDATED (DAP via codelldb/lldb)
```

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
   Lazy.nvim will auto-install all plugins.

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

If you see errors on startup:
1. Delete `~/.local/share/nvim` and `~/.cache/nvim`
2. Restart Neovim — lazy.nvim will reinstall everything
3. Run `:MasonInstallAll`
4. Run `:checkhealth` and address any warnings
