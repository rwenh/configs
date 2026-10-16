# Docs curation

Each file has one job. Other files link to it instead of repeating it.

| File | Job | Checked by |
|------|-----|-----------|
| MANIFEST.md | identity, external tools, plugin dependencies and pinning, configuration flags, project files | the generated tables (scripts/gen_docs.lua) and manifest_lint |
| README.md | what it is, install, first use, layout, build and test, design, troubleshooting | version line matched to bootstrap.lua by changelog_lint |
| CHANGELOG.md | releases, newest first; Unreleased at the top; changed APIs and behaviour called out | changelog_lint |
| KEYMAPS.md | every key by context | keymap_lint (documented keys bound, bound keys documented, no conflicts) |

## What moved
- Setup steps, requirements and the external-tools table: CHANGELOG → README (steps) and MANIFEST (tools).
- Flag and plugin reference tables: CHANGELOG → MANIFEST. The generator now writes there.
- Hot-swap and per-release tables: removed from the changelog; the behaviour notes stay in the entries.

## What changed in the keymap reference
- Removed `zp`: documented but never bound.
- Added the keys that were bound but undocumented: `]A` / `[A`, `<C-u>`, `<C-d>`, `zt`, `zz`, `zb`, `q`.
- Each key now appears once, in its owning context. Navigation no longer repeats the buffer, hunk, diagnostic, function, class, and parameter motions.
- The terminal `<Esc>` note is one sentence and matches the code.

## Open
- The manifest lint fails on three plugins without a lock entry (`tpope/vim-rails`, `elixir-tools/elixir-tools.nvim`, `laytan/tailwind-tools.nvim`). The MANIFEST states this. Lock them on a machine that loads them, or remove the specs.
- The generated tables were regenerated from the current config only by copying them forward. Run `nvim --headless "+luafile scripts/gen_docs.lua"` once on a full checkout to confirm they match.
