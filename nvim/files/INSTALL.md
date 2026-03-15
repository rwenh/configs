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

## File Structure

```
~/.config/nvim/
├── init.lua                          ← unchanged
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
            ├── init.lua              ← UPDATED (import order)
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
                ├── fortran.lua       ← UPDATED (build integration, snippets)
                ├── go.lua            ← unchanged
                ├── html.lua          ← UPDATED (htmlhint, prettier)
                ├── java.lua          ← unchanged
                ├── javascript.lua    ← UPDATED (eslint, neotest, package-info)
                ├── kotlin.lua        ← UPDATED (neotest, build integration)
                ├── markdown.lua      ← UPDATED (table mode, paste-image)
                ├── python.lua        ← UPDATED (unified py* prefix)
                ├── rest.lua          ← UPDATED (keymaps, env support)
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
