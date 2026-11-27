# Changelog

Versions follow `lua/core/bootstrap.lua`. Newest first. Unreleased changes are listed at the top until the next release.

### Unreleased

#### Verification
- `sh scripts/check.sh` is the single gate: pinned StyLua, luacheck, parse, unit tests, structure and docs checks, and the generated tables. It exits 0 on this tree.
- `tests/run.lua`: 27 unit tests for the pure modules. No plugins are needed.
- `scripts/check_graph.lua`: every local require resolves; every spec file is imported by the registry; every `vim.g` read is registered; every registered flag is read.
- `scripts/check_docs.lua`: every KEYMAPS.md key is bound; every lazy-lock.json entry is used; every declared repository is pinned.
- `.stylua-version` pins StyLua 2.6.0. `.luacheckrc` configures luacheck.

#### Fixed
- Java `<leader>'r` failed on packaged files (javac, then `java` by bare class name). It now runs the JDK 11+ source launcher.
- C, Kotlin and VHDL build products go to the cache, not beside the source or into the project root.
- Rust `<leader>'r` runs the file's own bin or example. The feature toggle restores the cargo default when emptied.
- `:MasonInstallAll` times out on a stall (no package finished within the limit), not on total duration. The flag documentation now says so.
- `:checkhealth core` checks lspconfig entries and Mason packages. Its previous "no drift detected" message checked only row shape.
- The lazy.nvim pin is read from `lazy-lock.json`. An offline cache must be the pinned commit. Startup runs no git subprocess.
- mini.animate has one scroll owner (neoscroll). Its scrolling is opt-in, so there is no load-order race.
- `<leader>fv` no longer matches sibling directories (`/a/proj` against `/a/proj-old`).
- A missing formatter is reported on manual `:Format`. It was silent.
- The workflow "run current file" template condition no longer resolves commands or creates directories when listed.
- The Java `close_on_exit` option was ignored; removed. The selection interpreter path is shell-quoted.
- README: copy-paste debris from the source bundle removed.

#### Changed
- Nine hand-rolled TTL caches (ruby, javascript, kotlin, java, elixir, shared, python, test, theme) use `core.util.cache`.
- Root detection stops at the first VCS root. A stray primary marker above a repository no longer captures it.
- Declined project files (`.lspconfig.lua`, `.nvim-dap.lua`, `.overseer.lua`, and `.env` URLs) stay declined until the file changes or the new `:ProjConfRecheck` runs.
- Core keymaps are checked for replacement once plugins load. `:KeymapHealth` reports replaced maps as well as registration conflicts. The four deliberate rebinds of Neovim defaults (`]b`, `[b`, `]t`, `[t`) are not reported.
- Neovim 0.11 API only: `vim.hl` and `vim.uv` without fallbacks; `unpack` without the table shim.
- EDITS.md sections 1–9 applied. A failed registry load in `lazy.lua` is now fatal, not an empty plugin list.
- `:checkhealth core` shows the Mason LSP check's real result (see Fixed).

#### Added
- oil.nvim (`-`, `<leader>eo`), mini.files (`<leader>em`, `<leader>eM`), neogit (`<leader>.N`, `<leader>.C`), gv.vim (`<leader>.v`), vim-fugitive, tailwind-tools.nvim (loads on markup and style files; sets up in Tailwind projects).
- `<leader>got`, `<leader>gof`, `<leader>goc` (Go); `<leader>ktt` (Kotlin test, with the `test` goal for Gradle and Maven).
- `:ProjConfRecheck`.

#### Removed
- blame.nvim and vim-test. No keys were in use (gitsigns owns `.B`; neotest owns test keys). Their lock entries are gone.
- KEYMAPS rows with no binding: `<leader>'t`, `<leader>'F`, and the `<leader>tcv/tcs/tct` aliases.
- `vim.g.term_max_runs` from MANIFEST. It was documented but never registered or read.

#### Deferred
- Language manifest: one registration per language, instead of about twelve touchpoints.
- Layering: move `cobol`, `vhdl` and `jvm` from `core/util` into `lang/`.
- Route every `vim.g` read through `flags.get`. `check_graph.lua` enforces registration for now.
- Generate KEYMAPS.md from a keymap registry. `check_docs.lua` enforces agreement for now.

#### Added
- `core/util/platform.lua` and `core/util/system_pkgs.lua`. Install hints for apt, dnf, zypper, pacman, apk and brew come from one registry, and `:checkhealth core` uses it.
- `:Theme <name>` switches theme, with completion.
- `vim.g.large_file_bytes` overrides the 500 KB large-file threshold. Notices state the actual threshold.
- Single source for size limits (`core/util/limits.lua`).
- `core/util/cache.lua`: one TTL and file-signature cache for the config. An entry keeps an explicit nil or false, is checked against its file's signature, and clears on `DirChanged`.
- `core/util/projconf.lua`: one trust-gated loader for `.runner.lua`, `.lspconfig.lua`, `.nvim-dap.lua` and `.overseer.lua`. It replaces four copies, each with its own cache.
- `require("core.flags").get(name, default)`: typed read of an escape hatch. A name that is not registered is reported once.
- `vim.g.db_url_env_vars` and `vim.g.db_url_dotenv_files` are registered in `core/flags.lua`. They were read but undocumented.
- The first install of lazy.nvim checks out the commit that `lazy-lock.json` pins.

#### Changed
- Distribution-specific install commands are removed from the Java, COBOL, REST, Markdown and runner modules. `:checkhealth core` accepts `fdfind` and looks for `debugpy-adapter`. The `compile_commands.json` link replaces a dangling link instead of calling `ln`. Overseer shell commands are passed directly, and the Go PATH notice is no longer shell syntax. The dashboard config action uses `stdpath("config")`.
- Themes: `core/theme.lua` maps each theme to its colorscheme per background. A switch loads a lazy plugin before applying it.
- REST auth tokens stay in the process environment. Requests use `{{$processEnv NVIM_REST_<NAME>}}`. Nothing is written to a buffer.
- Fortran, COBOL and VHDL run and check through `core.util.runner`, and COBOL flags come from `core.util.cobol`. `<leader>'r`, `<leader>ftb`, `<leader>cob` and `<leader>vhr` now build the same way.
- `python_has` no longer blocks the editor.
- `autocmds.lua` owns the large-file flag. Treesitter reads it.
- The health LSP list is generated from `core/util/packages.lua`.
- `core/util/packages.lua` is one registry: a row per language server, holding its lspconfig name, Mason package and binary. The three parallel lists and the fuzzy name matching are gone, and `validate()` checks the rows.
- Project roots: a primary marker (`mix.exs`, `Cargo.toml`, `pyproject.toml`, `go.mod`, `config/application.rb`, …) wins over a nearer `package.json`, `Makefile` or `Gemfile`. Before, the nearest marker of any kind won, so a Phoenix `assets/package.json` became the root. `vim.g.path_prefer_vcs` keeps the VCS-first rule.
- `.env` (database URL) is read through `vim.secure`: it asks once per content hash, like other project files. It is off with `disable_project_configs`.
- `.nvim-dap.lua` is applied when a project buffer is entered, and once when nvim-dap loads. The wrapper around `dap.continue` is gone.
- `lazy-lock.json` locks `tailwind-tools.nvim`, `vim-rails` and `elixir-tools.nvim`. They had been installing at latest.
- ruff and rubocop are registered with nvim-lint only when installed. nvim-lint errors are shown once instead of being swallowed.
- Servers whose binary is missing are skipped and reported once, instead of failing on every attach.
- Language servers that run from a project's `node_modules/.bin` (HTML, CSS, JSON, YAML, Tailwind) are accepted when installed locally.
- COBOL has no language server in this config: `cobol-language-support` is not a standalone binary. `cobc` compile and run are unchanged.
- The `tree-sitter` CLI is checked when a `.http` or `.rest` buffer opens. The curl and tree-sitter checks no longer run at every startup.
- Version detection uses one cache (`core/util/exec.lua`). `core/util/mason.lua` delegates to it; its public functions are unchanged.
- Terminal commands launch through one helper. The fallback opens a new buffer and uses `termopen`.
- Rust formatting reads the edition from `Cargo.toml` on each format.
- `<leader>cb` and `<leader>ccx` run through `core.util.runner`, the same path as `<leader>'r`. The C and C++ compile logic was in three places and is in one. These keys now also honour `.runner.lua` and autosave a modified buffer, as `<leader>'r` does.
- `<Esc>` in a float terminal follows the same rules as other terminals: it leaves terminal mode unless the program is listed in `vim.g.terminal_tui_programs`.

#### Fixed
- Java and Kotlin Spring Boot keymaps passed goal names that Gradle and Maven do not know. They now run `bootRun` and `bootJar`.
- `:KotlinDoctor` called `jvm.build_tool`, which does not exist, and failed. It now uses `jvm.detect_tool`.
- The build-flag sanitiser let a newline reach the shell string. Only a literal space is allowed.
- The `.env` file was read with no trust check.
- The C linter was registered as `clang-tidy`, a name nvim-lint never runs, so C linting never ran. It is `clangtidy`.
- `lsp.lua` registered `htmlhint` for every session, and attempted `cobol_ls` with a binary that is never spawned. Both are removed. htmlhint is owned by `lang/html.lua`.
- `peek.nvim` ran `deno` on install even when deno was missing. The build now reports the problem and skips.
- The debugger's project configuration ran inside `dap.continue`, so a broken `.nvim-dap.lua` looked like a debugger failure.
- Rust edition stayed at the first value after `Cargo.toml` changed.
- The basedpyright health check looked for the CLI, not `basedpyright-langserver`.
- The COBOL health check and language server looked for a binary that is never spawned.
- A float terminal's `<Esc>` mapping overrode the full-screen-program exemption.

#### Removed
- The `NvimIdeReady` bridge (no listener), the which-key group queue in `keymaps.lua` (no caller), and `flags.not_flags` (unused).
- `tests/`. The repository ships no test suite. `:checkhealth core` and `:MasonInstallAll dry-run` are the checks.

#### Kept
- `scripts/gen_docs.lua` regenerates the flags and plugins tables in MANIFEST.md. It reads the sources directly instead of a running lazy.nvim, and it writes MANIFEST.md (it previously targeted CHANGELOG.md). `GEN_DOCS=check` exits 1 when a table is stale.

### 2.6.1 (current) — calibration

#### Changed
- Plugin loader moved from `lua/plugins/init.lua` to `lua/core/lazy.lua`. Upgrading from v2.5: delete `lua/plugins/init.lua`.
- `:MasonInstallAll dry-run` reports instead of installing.
- `safe_require` clears LuaJIT load poisoning and shows the first real error.
- Failed module reloads roll back atomically (`:Reload`).
- Deep focus mode is transactional and restores the window it started in.

#### Fixed
- Theme day/night switching and the manual toggle were both inert: a hard-coded background overrode the clock.
- Nerd Font glyphs lost during an earlier pass are restored.
- Build flags are checked against an allow-list before they reach a shell.
- typescript-tools no longer produces a false "typescript-language-server not found" warning.

### 2.6.0 — proof

- Escape-hatch registry (`core/flags.lua`) with type checks, and generated reference tables.
- Core modules (options, autocmds, keymaps, commands, theme, focus, highlights, util) with headless tests, and fixes for modules that could not work as shipped.

### 2.5.0 — baseline

- Modular, lazy-loaded Neovim IDE with 20+ languages and an LSP, DAP, test, git, and task-runner stack.