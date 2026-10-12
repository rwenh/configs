# v2.6.1 — Wave 4 base + calibration (Wave 5 tooling)

Base = your updated Wave 4 project files (59 Lua modules + 3 docs). Verified headless on Neovim 0.11.4,
fresh plugin install: `bash tests/run.sh` → specs_lint, modules, units, smoke, keymaps all pass; luacheck 0 warnings;
stylua --check clean; generated docs up to date.

## Calibrated modules (changes on top of Wave 4)
| Module | Change |
|--------|--------|
| `lua/plugins/specs/lang/javascript.lua` | package-info `colors` (deprecated) → `highlights` |
| `lua/plugins/specs/treesitter.lua` | treesitter-context skipped on fortran (shipped context query is invalid for the installed parser: "Invalid node type do_loop") |
| `lua/plugins/specs/advanced.lua` | rainbow-delimiters `submodules = false` (avoids pulling a GitLab test submodule) |
| `lua/core/bootstrap.lua` | version stamp 2.6.1 |
| `lua/core/lazy.lua` (NEW) | loader (was `lua/plugins/init.lua`): the module name `plugins` is a poisoned-require magnet |
| `init.lua` (root, merged) | your v2.6.0 root + `safe_require` (clears LuaJIT "loop or previous error" poison, retries, prints traceback + FIRST REAL ERROR) |
| `lua/core/util/vhdl.lua` (restored) | verbatim v2.5.0 module; absent from the flat project folder (name collides with lang/vhdl.lua) but required by `<leader>vht` |

Delete `lua/plugins/init.lua` if it still exists. Everything else is byte-identical to your Wave 4 files.

## Added tooling
`tests/{smoke,modules,units,keymaps,specs_lint}.lua`, `tests/lib/keymaps.lua`, `tests/run.sh`,
`scripts/gen_docs.lua` → `docs/{KEYMAPS,PLUGINS}.generated.md`, `stylua.toml`, `lazy-lock.json` (commit it).
Dotfiles (`.luacheckrc`, `.styluaignore`, `.github/workflows/ci.yml`) were intentionally left out of this package.

## Notes
- Your Wave 4 files already contained the fixes from my first calibration pass (treesitter `master` pin, cobol parser,
  rust/elixir/go fixes, init/config override fixes, mini/LuaSnip/surround, dap key stubs); `tests/specs_lint.lua` now keeps
  the override class from coming back.
- Not exercised headless: LSP/DAP/jdtls attach-time behaviour, real Mason installs, tmux/GUI.
- `:DBUIToggle` E716 reproduces with `-u NONE` on 4 vim-dadbod-ui commits (upstream).
- If you keep a `tests/` or `tools/` directory from Wave 3/4 (e.g. `unit_docs.lua`, `gen_docs.lua`), merge rather than
  overwrite; my files are `smoke/modules/units/keymaps/specs_lint` and `scripts/gen_docs.lua`.
