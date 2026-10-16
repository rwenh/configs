# nvim-ide

A modular Neovim IDE: lazy-loaded, LSP-first, 20+ languages.

**Neovim 0.11+** · openSUSE Leap 16.0 (also Ubuntu, Arch, macOS) · **v2.6.1**

- [MANIFEST.md](MANIFEST.md): identity, requirements, plugin dependencies, configuration flags
- [KEYMAPS.md](KEYMAPS.md): every key, by context
- [CHANGELOG.md](CHANGELOG.md): what changed in each release

## What you get

- Per-language LSP, DAP, formatting, linting, tests, and a task runner.
- Lazy loading: a plugin loads on the event, command, filetype, or key that needs it.
- Guarded: missing tools are reported once, project configs need `:trust`, and multi-step changes roll back on failure.

## Install

Requirements are listed in [MANIFEST.md](MANIFEST.md#external-tools). There is no build step.

```bash
git clone https://github.com/rwenh/configs.git ~/dotfiles
ln -sfn ~/dotfiles/nvim ~/.config/nvim     # move any existing ~/.config/nvim aside first
nvim                                        # lazy.nvim installs the plugins; wait for 100%
```

Then, inside Neovim:

```vim
:MasonInstallAll     " LSP servers, DAP adapters, formatters, linters
:TSUpdate            " treesitter parsers
:checkhealth core    " expect no ERROR
:Lazy lock           " pin the plugin commits, then commit lazy-lock.json
```

On a slow network, raise `vim.g.mason_install_timeout_ms` in `init.lua` before the first launch.

## Use

Leader is `Space`. Start with these five:

| Key | Action |
|-----|--------|
| `<leader>ff` | find files |
| `<leader>fw` | live grep |
| `<leader>ee` | file explorer |
| `<leader>.g` | LazyGit |
| `F5` | debug, in a supported filetype |

- Commands: `:Health`, `:Format`, `:Reload core.util.runner`, `:ThemeAuto`, `:MasonInstallAll dry-run`, `:KeymapHealth`.
- Theme: `lua/core/theme.lua`; `<leader>ut` toggles dark and light.
- Per-project overrides: `.lspconfig.lua`, `.runner.lua`, `.nvim-dap.lua`, `.overseer.lua`. They load after `:trust`.

## Languages

| Language | LSP | DAP | Format | Lint | Test |
|----------|-----|-----|--------|------|------|
| Lua | lua_ls | — | stylua | — | — |
| Python | basedpyright | debugpy | black + isort | ruff (+ pylint opt-in) | pytest |
| Rust | rust-analyzer | codelldb | rustfmt | clippy | cargo test |
| Go | gopls | delve | goimports + gofumpt | staticcheck | go test |
| TypeScript / JavaScript | typescript-tools | pwa-node | prettier | eslint_d | vitest / jest |
| Java | jdtls | java-debug | jdtls built-in | — | JUnit |
| Kotlin | kotlin_language_server | java-debug | ktlint | ktlint | JUnit |
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
| VHDL | vhdl_ls³ | — | vsg⁴ | — | ghdl |

¹ Only when a config file is found in the project root.
² Not provided by a package manager. Compile and run with `cobc` (`<leader>cob`); a `cobol-language-support` binary on `PATH` is used if present.
³ Not in Mason: `cargo install vhdl_ls`.
⁴ Formats a temporary copy with `vsg --fix -f`.

## Layout

```
init.lua                  entry point and load order
lua/core/                 bootstrap, options, keymaps, autocmds, commands, theme, focus, highlights, flags, lazy, health
lua/core/util/            path, runner, term, format, secure, reload, exec, mason, packages, limits, …
lua/plugins/specs/        ui, editor, lsp, completion, treesitter, git, dap, test, advanced, hud, workflow
lua/plugins/specs/lang/   one file per language, plus shared helpers
lazy-lock.json            pinned plugin commits
tests/                    static lints, runtime suite, regression tests
scripts/gen_docs.lua      regenerates the tables in MANIFEST.md
```

## Build and test

```bash
bash tests/run.sh                                  # all checks: static, runtime (sandboxed), docs
nvim -l tests/static/keymap_lint.lua               # one static lint; needs no plugins
nvim --headless "+luafile scripts/gen_docs.lua"    # regenerate the tables in MANIFEST.md
```

The static lints need only Neovim 0.11. The runtime suite installs the plugins in an isolated sandbox.

## Design

- One source per concern: `core/flags.lua` for escape hatches, `core/util/packages.lua` for LSP and Mason names, `core/util/limits.lua` for size limits, `core/util/exec.lua` for version detection.
- Large files (over 500 KB by default) drop treesitter, folds, syntax, and extra features.
- DAP adapters register on the first matching filetype.
- A plugin shared by several spec files sets `init` and `config` in one place only; the spec lint enforces this.

## Troubleshooting

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
| `.http` files fail to load | install the tree-sitter CLI (`sudo zypper in tree-sitter`) |
| Format silently does nothing | read the WARN notice; raise `vim.g.format_timeout_ms` or `vim.g.format_timeout_by_ft` |
| Neovim too old (openSUSE) | `sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_16.0/ editors && sudo zypper ref && sudo zypper in neovim` |
