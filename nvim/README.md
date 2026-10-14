# nvim-ide

> Modular Neovim IDE — lazy-loaded, LSP-first, 20+ languages.

**Neovim 0.11+** · openSUSE Leap 16.0 (also Ubuntu, Arch, macOS) · **v2.6.1**

| Doc | What is in it |
|-----|---------------|
| **[CHANGELOG.md](CHANGELOG.md)** | setup guide, escape-hatch flags, every plugin by phase, hot-swap, troubleshooting, version history |
| **[KEYMAPS.md](KEYMAPS.md)** | every keymap, by area and language |

## Quick start

```bash
git clone https://github.com/rwenh/configs.git ~/dotfiles && ln -sf ~/dotfiles/nvim ~/.config/nvim   # back up the old one first
nvim                  # lazy.nvim installs the plugins
:MasonInstallAll      # LSP / DAP / formatters / linters
:TSUpdate             # treesitter parsers
:checkhealth core     # verify, then :Lazy lock and commit lazy-lock.json
```

System packages, language tooling, fonts and tmux notes: [CHANGELOG.md → Setup](CHANGELOG.md#setup).

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
| COBOL | none available² | — | — | — | — |
| VHDL | vhdl_ls³ | — | vsg⁴ | — | ghdl |

> ¹ only when a config file is found in the project root · ² compile/run via `cobc` (`<leader>cob`); a `cobol-language-server`
> binary on `PATH` is used if present · ³ not in Mason: `cargo install vhdl_ls` · ⁴ formats a temp copy with `vsg --fix -f`

## Structure

```
~/.config/nvim/
├── init.lua                  entry point (poison-proof require, load order)
├── lazy-lock.json            pinned plugin commits — commit it
├── lua/core/                 bootstrap, options, keymaps, autocmds, commands, theme, focus, highlights, flags, lazy, health
├── lua/core/util/            path, runner, term, format, secure, reload, jsonc, jvm, vhdl, mason, packages, icons, exec, snippets, quotes, buf_keymap
├── lua/plugins/specs/        ui, editor, lsp, completion, treesitter, git, dap, test, advanced, hud, workflow
├── lua/plugins/specs/lang/   one file per language (+ shared helpers)
├── tests/                    headless tests (run: bash tests/run.sh)
└── scripts/gen_docs.lua      regenerates the flag and plugin tables in CHANGELOG.md
```

Load order: bootstrap → options → autocmds → keymaps → commands → lazy/plugins → theme → highlights.
Specs: `completion` before `lsp` (blink capabilities), `lsp` before `lang/*`, `web` before `html`/`css`.

## Design notes

- **Single sources of truth** — `core/flags.lua` (every `vim.g` escape hatch), `core/util/packages.lua` (Mason + lspconfig names).
- **Per-project root detection** — `core/util/path.lua` walks up through root markers with a TTL cache, cleared on `DirChanged`.
- **Trust-gated project files** — `.lspconfig.lua`, `.runner.lua`, `.nvim-dap.lua`, `.overseer.lua` load only after `:trust`
  (`vim.g.disable_project_configs = true` disables them).
- **Atomic focus mode** — snapshot, apply, roll back on any failure (`<leader>uF`).
- **Large files** (> 500 KB) automatically drop treesitter, folds, syntax, cursorline, spell.
- **Deferred DAP** — adapters register on first matching `FileType`; zero cost otherwise.
- **Lazy-spec rule** — a plugin shared by several spec files must not set `init`/`config` in more than one
  (lazy.nvim keeps only the last); `tests/specs_lint.lua` enforces it.

## Themes

`lua/core/theme.lua` → `M.config = { theme = "tokyonight", day_start = 7, day_end = 19, fallback = "default" }`.
Available: `tokyonight` (default), `catppuccin`, `rose-pine`, `kanagawa`, `gruvbox-material`, `solarized`, `solarized-osaka`.
`<leader>ut` toggles dark/light (`:ThemeAuto` returns to time-based); `require("core.theme").switch("catppuccin")`.

## Development

```bash
bash tests/run.sh                                   # spec lint, module load, units, behaviour, smoke, keymaps, docs-not-stale
nvim --headless "+luafile scripts/gen_docs.lua"     # regenerate CHANGELOG.md flag + plugin tables
luacheck --std luajit --globals vim --no-max-line-length --no-unused-args .
stylua --check init.lua lua/core/lazy.lua tests scripts
```
