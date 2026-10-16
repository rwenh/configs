# Changelog

Versions follow `lua/core/bootstrap.lua`. Newest first. Unreleased changes are listed at the top until the next release.

### Unreleased

#### Added
- `vim.g.large_file_bytes` overrides the 500 KB large-file threshold. Notices state the actual threshold.
- Single source for size limits (`core/util/limits.lua`).
- Regression tests under `tests/` (static lints, runtime suite, one regression test per fix below).

#### Changed
- Servers whose binary is missing are skipped and reported once, instead of failing on every attach.
- Language servers that run from a project's `node_modules/.bin` (HTML, CSS, JSON, YAML, Tailwind) are accepted when installed locally.
- COBOL uses the `cobol-language-support` binary, the name `nvim-lspconfig` starts.
- The `tree-sitter` CLI is checked when a `.http` or `.rest` buffer opens. The curl and tree-sitter checks no longer run at every startup.
- Version detection uses one cache (`core/util/exec.lua`). `core/util/mason.lua` delegates to it; its public functions are unchanged.
- Terminal commands launch through one helper. The fallback opens a new buffer and uses `termopen`.
- Rust formatting reads the edition from `Cargo.toml` on each format.
- `<Esc>` in a float terminal follows the same rules as other terminals: it leaves terminal mode unless the program is listed in `vim.g.terminal_tui_programs`.

#### Fixed
- Rust edition stayed at the first value after `Cargo.toml` changed.
- The basedpyright health check looked for the CLI, not `basedpyright-langserver`.
- The COBOL health check and language server looked for a binary that is never spawned.
- A float terminal's `<Esc>` mapping overrode the full-screen-program exemption.

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

- Escape-hatch registry (`core/flags.lua`) with type checks, generated reference tables, drift and keymap tests, and CI.
- Language modules for C/C++, Rust, Go, Python, Java, Kotlin, JS/TS, web, Ruby, Elixir, Fortran, Zig, COBOL, VHDL, SQL, REST, and Markdown.
- Core modules (options, autocmds, keymaps, commands, theme, focus, highlights, util) with headless tests, and fixes for modules that could not work as shipped.

### 2.5.0 — baseline

- Modular, lazy-loaded Neovim IDE with 20+ languages and an LSP, DAP, test, git, and task-runner stack.
