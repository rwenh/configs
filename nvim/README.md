# nvim-ide[cite: 17]

A modular Neovim IDE: lazy-loaded, LSP-first, 20+ languages.[cite: 17]

**Neovim 0.11+** · Linux and macOS · **v2.6.1**

- [MANIFEST.md](MANIFEST.md): identity, requirements, plugin dependencies, configuration flags[cite: 17]
- [KEYMAPS.md](KEYMAPS.md): every key, by context[cite: 17]
- [CHANGELOG.md](CHANGELOG.md): what changed in each release[cite: 17]

## What you get[cite: 17]

- Per-language LSP, DAP, formatting, linting, tests, and a task runner.[cite: 17]
- Lazy loading: a plugin loads on the event, command, filetype, or key that needs it.[cite: 17]
- Guarded: missing tools are reported once, project configs need `:trust`, and multi-step changes roll back on failure.[cite: 17]

## Install[cite: 17]

Requirements are listed in [MANIFEST.md](MANIFEST.md#external-tools). There is no build step.[cite: 17]

```bash
git clone [https://github.com/rwenh/configs.git](https://github.com/rwenh/configs.git) ~/dotfiles
ln -sfn ~/dotfiles/nvim ~/.config/nvim     # move any existing ~/.config/nvim aside first
nvim                                        # lazy.nvim installs the plugins; wait for 100%
```[cite: 17]

Then, inside Neovim:[cite: 17]

```vim
:MasonInstallAll     " LSP servers, DAP adapters, formatters, linters
:TSUpdate            " treesitter parsers
:checkhealth core    " expect no ERROR
:Lazy lock           " pin the plugin commits, then commit lazy-lock.json
```[cite: 17]

On a slow network, raise `vim.g.mason_install_timeout_ms` in `init.lua` before the first launch.[cite: 17]

## Use[cite: 17]

Leader is `Space`. Start with these five:[cite: 17]

| Key | Action |
|-----|--------|
| `<leader>ff` | find files |
| `<leader>fw` | live grep |
| `<leader>ee` | file explorer |
| `<leader>.g` | LazyGit |
| `F5` | debug, in a supported filetype |[cite: 17]

- Commands: `:Health`, `:Format`, `:Reload core.util.runner`, `:ThemeAuto`, `:MasonInstallAll dry-run`, `:KeymapHealth`.[cite: 17]
- Theme: `lua/core/theme.lua`; `<leader>ut` toggles dark and light.[cite: 17]
- Per-project overrides: `.lspconfig.lua`, `.runner.lua`, `.nvim-dap.lua`, `.overseer.lua`. They load after `:trust`.[cite: 17]

## Languages[cite: 17]

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff (+ pylint opt-in) | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo test |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript / JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | jdtls built-in | — | JUnit |
| Kotlin | kotlin_language_server | kotlin-debug-adapter | ktlint | — | JUnit |
| Ruby | solargraph | rdbg | rubocop | rubocop | rspec |
| Elixir | elixir-ls (or NextLS) | elixir-ls | mix format | — | ExUnit |
| C / C++ | clangd | codelldb | clang-format | clang-tidy (C) | ctest |
| HTML | html-lsp | — | prettier | htmlhint¹ | — |
| CSS / SCSS | cssls + cssmodules_ls | — | prettier | stylelint¹ | — |
| SQL | sqls | — | sqlfmt | — | — |
| Markdown | — | — | prettier | — | — |
| Zig | zls | codelldb | zig fmt | — | zig build test |
| Fortran | fortls | — | fprettify | — | — |
| COBOL | cobol-language-support² | — | — | — | — |
| VHDL | vhdl_ls³ | — | vsg⁴ | — | ghdl |[cite: 17]

¹ Only when a config file is found in the project root.[cite: 17]
² Not provided by a package manager. Compile and run with `cobc` (`<leader>cob`); a `cobol-language-support` binary on `PATH` is used if present.[cite: 17]
³ Not in Mason: `cargo install vhdl_ls`.[cite: 17]
⁴ Formats a temporary copy with `vsg --fix -f`.[cite: 17]

## Layout[cite: 17]


```

init.lua                  entry point and load order
lua/core/                 bootstrap, options, keymaps, autocmds, commands, theme, focus, highlights, flags, lazy, health
lua/core/util/            path, runner, term, format, secure, projconf, cache, reload, exec, mason, packages, limits, …
lua/plugins/specs/        ui, editor, lsp, completion, treesitter, git, dap, test, advanced, hud, workflow
lua/plugins/specs/lang/   one file per language, plus shared helpers
lazy-lock.json            pinned plugin commits
scripts/gen_docs.lua      regenerates the flags and plugins tables in MANIFEST.md from the sources

```[cite: 17]

## Verify[cite: 17]

There is no test suite in the repository. Check a change with:[cite: 17]

- `:checkhealth core`, then `:Health` for a one-screen summary.[cite: 17]
- `:MasonInstallAll dry-run` to see which tools are missing.[cite: 17]
- Open a file in each language you touched and run its build, run and test keys from KEYMAPS.md.[cite: 17]

## Design[cite: 17]

- One source per concern: `core/flags.lua` for escape hatches (read them with `flags.get`), `core/util/packages.lua` for the language-server registry (one row per server: lspconfig name, Mason package, binary), `core/util/cache.lua` for every TTL and file-signature cache, `core/util/projconf.lua` for the trust-gated per-project Lua files, `core/util/limits.lua` for size limits, `core/util/exec.lua` for version detection.[cite: 17]
- Project roots: a primary marker (`mix.exs`, `Cargo.toml`, `pyproject.toml`, `config/application.rb`, …) wins over a nearer `package.json` or `Makefile`. See the header of `core/util/path.lua`.[cite: 17]
- Large files (over 500 KB by default) drop treesitter, folds, syntax, and extra features.[cite: 17]
- DAP adapters register on the first matching filetype.[cite: 17]

## Troubleshooting[cite: 17]

| Symptom | Fix |
|---------|-----|
| LSP does not attach; `gd` does nothing | `:MasonInstallAll`, then `:checkhealth lsp` and `:LspInfo`. A server missing from `PATH` is named once in a notice. |
| `:MasonInstallAll` times out | raise `vim.g.mason_install_timeout_ms` |
| Treesitter highlighting broken | `:TSUpdate` |
| Plugins drift after `:Lazy update` | `:Lazy lock`, then commit `lazy-lock.json` |
| Plugins do not load | remove `~/.local/share/nvim`, `~/.cache/nvim`, and `~/.local/state/nvim`; restart; then `:MasonInstallAll`, `:TSUpdate`, `:Lazy lock` |
| `<Esc>` leaves a full-screen program | add the program to `vim.g.terminal_tui_programs` |
| Wrong project root | set `vim.g.path_debug = true` and read the DEBUG notices |
| Java workspace is stale | delete `~/.local/share/nvim/jdtls-workspace/` and restart |
| `.http` files fail to load | install the tree-sitter CLI (`:checkhealth core` prints the command for your system) |
| Format silently does nothing | read the WARN notice; raise `vim.g.format_timeout_ms` or `vim.g.format_timeout_by_ft` |
| Neovim older than 0.11 | install 0.11+ from your package manager, the release tarball or an AppImage; check with `nvim --version` |[cite: 17]
