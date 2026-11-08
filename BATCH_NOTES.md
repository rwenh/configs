# nvim-ide — batch 4 (Unreleased)

The tarball holds `nvim/`, the complete repository: `init.lua`, `lua/` (67 Lua files), `scripts/gen_docs.lua`,
and the docs. There is no `tests/` directory in this tree. Formatting follows `stylua.toml` and is a fixed point.

## What changed since batch 3

- `scripts/gen_docs.lua` is kept. It regenerates the flags and plugins tables in MANIFEST.md straight from
  `lua/core/flags.lua` and the plugin spec files. It needs no running lazy.nvim, writes only MANIFEST.md, and
  `GEN_DOCS=check` exits 1 when a table is stale. The earlier version wrote CHANGELOG.md.
- The lazy spec loader (`core/lazy.lua`) finds the repository from its own path. It used to look for the language
  specs under `stdpath("config")`, so the repo only worked when linked into `~/.config/nvim`. Setup is an explicit
  call from `init.lua`.
- kulala's setup is skipped when the `tree-sitter` CLI is missing. Its setup raised an uncaught error without it.
- Stylua pass over the whole tree.

## Verification (Neovim 0.11.4 in the sandbox; the checks are not shipped)

- Full startup of the repo's `init.lua`, linked as the config directory, with all plugins at their locked commits:
  118 plugins registered, the tokyonight colorscheme loads, no error messages. The only message is a tree-sitter
  parser download that the sandbox cannot reach.
- Every plugin force-loaded one at a time: 118 load, none raise.
- `:checkhealth core`: 0 errors, 54 warnings. All 54 are tools this sandbox does not have (formatters, language
  servers, runtimes). The Mason DAP package-name check depends on Mason's downloaded registry, which the sandbox
  does not have, so those names are not verified here.
- Lock: all 120 checkouts are at their locked commit.
- Behavioural suites from batch 3 still pass on the final formatted tree: cache and projconf, root rule, registry,
  36 module behaviours, 33 spec modules, C/C++ builds with real gcc and g++, no leaked globals.

## Not verified

- Tools this sandbox lacks: Mason-installed servers and DAP adapters, `fd`, `go`, `cargo`, `debugpy`, `ruff`.
  The config's own notices for them fire as designed.
- Feature behaviour inside plugins (completion, picking, debugging sessions). This verifies configuration loading,
  not interactive use.

## Decisions still open (from batch 3)

1. `.env` is trust-gated: a project with a `.env` prompts once to trust it on open, and startup resolves the database.
2. Project roots: a primary marker wins over a nearer `package.json`. `path_prefer_vcs` keeps the VCS-first rule.
3. `.nvim-dap.lua` is trusted when a project buffer is entered, not on the first continue.

## Still open

- Duplicated Python interpreter resolvers (`runner.python_bin` and the candidates in `python.lua`).
- Structure: split `lsp.lua`, extract `ui.lua`, per-language DAP ownership.
- Tool registry for formatters, DAP adapters and runtimes (language servers only so far).
- `shared.lua` still has its own cache for linter gating.
- README and KEYMAPS prose, which the generator does not cover.
