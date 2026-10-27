# Docs curation

Each file has one job. Other files link to it instead of repeating it. README, KEYMAPS and CHANGELOG are maintained by hand; the MANIFEST tables are generated from the code. The code is the source for every fact the docs state.

| File | Job | Source of truth |
|------|-----|-----------------|
| MANIFEST.md | identity, external tools, pinning, project files; the flags and plugins tables are generated | `lazy-lock.json` (versions), `lua/core/flags.lua` (flags), `lua/plugins/specs/` (plugins) |
| README.md | what it is, install, first use, layout, verification, design, troubleshooting | the code |
| CHANGELOG.md | releases, newest first; Unreleased at the top; behaviour changes called out | the code and `lazy-lock.json` |
| KEYMAPS.md | every key, by context | `lua/core/keymaps.lua` and `lua/plugins/specs/` |

## Generated tables

- `scripts/gen_docs.lua` rewrites the two regions of MANIFEST.md between `BEGIN GENERATED` and `END GENERATED` markers. It reads `lua/core/flags.lua` and the plugin spec files, so it runs on a fresh checkout with no install.
- Regenerate after changing a spec or a flag: `nvim --headless -u NONE "+luafile scripts/gen_docs.lua"`.
- Check without writing: `GEN_DOCS=check nvim --headless -u NONE "+luafile scripts/gen_docs.lua"` (exit 1 if stale).
- README, KEYMAPS and CHANGELOG are still maintained by hand. The generator never writes them.

## Open

- A full docs rewrite is a batch of its own. This batch corrected only the entries it changed.
