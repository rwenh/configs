# Emacs IDE — v3.5.0

`Emacs 29.1+` · `straight.el` · `55 modules` · `50 languages` · `<2s cold start`

A production-grade, fully offline, maximally ergonomic Emacs IDE.
All packages managed via `straight.el`. No network access required after first
bootstrap. Config-driven via a single `config.yml`. Module hierarchy:

```
core/          10 infrastructure modules
modules/       29 feature modules
modules/langs/ 17 language IDE layers  (lazy, zero boot cost)
lib/            6 ide-* shared-utility library
var/              runtime state (logs, caches, custom.el, backups)
```

---

## Load Order

```
config → health → diagnose → package → profiler → security → telemetry → recovery
→ ui-core → ui-theme → ui-modeline → ui-dashboard → ui-workspace
→ completion-core → completion-snippets → editing-core → core-dev
→ tools-lsp → tools-project → tools-git → tools-terminal
→ tools-format → apheleia-langs-patch
→ tools-org → tools-spelling → tools-notes → tools-rest
→ tools-test-runner-registry → tools-test → debug-core
→ tools-repl → tools-project-detect → tools-hydra
→ tools-database → tools-devops → emacs-ide-analytics
→ keybindings                     ← always last
langs/: lazy on file-open or project-switch
lib/:   required lazily by individual modules as needed
```

---

## Quick Start

```bash
git clone <this-repo> ~/.emacs.d
emacs
# straight.el bootstraps on first launch (~1 min, one-time)
```

Edit `config.yml`, then `M-x emacs-ide-config-reload` (or `C-c R`).

---

## Core Modules

### `emacs-ide-config.el`
Simple, robust YAML parser (no external deps) → 30+ `defvar`s.  
`config.yml` → `emacs-ide-config-apply` → live `emacs-ide-*` vars.  
All feature modules read settings via `(emacs-ide-config-get 'section 'key default)`.
```
M-x emacs-ide-config-reload   C-c R    reload config.yml
M-x emacs-ide-config-show              view all active values
M-x emacs-ide-config-edit              open config.yml
```

### `emacs-ide-health.el`
Extensible check registry. Built-in checks: system tools, LSP servers, config,
Emacs version. Periodic idle recheck every 60 s. Modeline segment (via `ui-modeline.el`).
```
M-x emacs-ide-health-check-all    run full report
M-x emacs-ide-health-auto-fix     cancel void timers, re-run checks
M-x emacs-ide-health-status       alias for check-all
```

### `emacs-ide-recovery.el`
Crash detection (increment-on-start, reset-on-clean-exit), config backup/restore,
per-session package disable, recovery log, 5-min flush timer.
```
C-c r r   recovery report
C-c r v   view log
C-c r b   backup config.yml
C-c r d   disable a package for this session
C-c r C-r reset crash counter
```

### `emacs-ide-diagnose.el`
Comprehensive diagnostics: core modules, feature modules, lib layer (✓ loaded /
○ on disk / ✗ missing), key functions, mode status, performance, config validation.
```
M-x emacs-ide-diagnose           full diagnostics
M-x emacs-ide-diagnose-lsp       LSP server PATH check
M-x emacs-ide-diagnose-languages language module status
```

### `emacs-ide-spot-check.el`
Integrity check: 94+ commands, 47 direct-symbol keybindings, 16 lambda bindings,
40 module features. Run after any structural change.
```
M-x emacs-ide-spot-check
```

### `emacs-ide-test.el`
ERT test suite: Emacs version, startup time, directory structure, load-path,
core modules, health system, recovery, packages, completion, editing modes,
git, essential files, and smoke tests for tools-database/devops/analytics.
```
M-x emacs-ide-run-tests
```

### `emacs-ide-security.el`
TLS verification (gnutls), prime bits, package signature policy, auth-sources,
network-security-level. All values from config.yml `security:` section.
```
M-x emacs-ide-security-check    audit: TLS, GPG, auth-source, custom-file
M-x emacs-ide-security-harden   re-apply settings
```

### `emacs-ide-profiler.el`
Wraps Emacs's built-in profiler. CPU+mem profiling with start/stop/report/reset.
```
C-c D s   start profiler
C-c D r   stop + report
C-c D q   stop only
M-x emacs-ide-profile-startup   (requires esup)
```

### `emacs-ide-telemetry.el`
Local-only usage analytics. Pending count ring with 0.5s idle flush. Session-start
log entry (elapsed, GC cycles, package count). Config-driven enable/disable.
```
M-x emacs-ide-telemetry-report   top 20 most-used commands
M-x emacs-ide-telemetry-clear
M-x emacs-ide-telemetry-enable/disable
```

### `emacs-ide-package.el`
Wraps `require` with timing advice. Tracks all package load times in a hash
table. Configurable slow-package threshold (default 0.1s).
```
M-x emacs-ide-package-report     slowest packages
M-x emacs-ide-package-clear-times
```

---

## Lib Layer — `lib/ide-*.el`

Six dependency-free, zero-side-effect utility libraries. Required lazily by
feature modules as needed. The `ide-*` prefix is distinct from the IDE's own
`emacs-ide-*` namespace.

| File | Contents |
|---|---|
| `ide-common.el` | Project root/name, region-or-symbol, large-buffer predicate, shell-lines, executable-or-message, format-bytes, mode-label, alist-get-in |
| `ide-simple.el` | DWIM escape, smart-home, kill-line-backward, open-line-above/below, duplicate-line-or-region, insert-date, rename/delete-file-and-buffer, copy-path, scratch-for-mode, monocle |
| `ide-window.el` | Split-sensibly, toggle-split-direction, swap-buffers, kill-side-windows, display-buffer-bottom action, act-and-restore macro |
| `ide-search.el` | Occur: URLs, long-lines, TODO keywords; project-grep-symbol (consult→projectile→rgrep); isearch-other-end |
| `ide-pair.el` | wrap-or-insert for `*`, `_`, backtick, `=`, `~`; setup-markdown-bindings, setup-org-bindings |
| `ide-register.el` | Quick-file registers from config.yml, jump-or-bookmark, list-populated, store-quick-files |

Lib-wired bindings (registered in `keybindings.el`):
```
C-c z     monocle toggle
C-c M-d   duplicate line/region
C-c M-o   open line below    C-c M-O   open line above
C-c M-.   insert date
C-c M-r   rename file + buffer
C-c M-c   copy file path (prefix = project-relative)
C-c M-s   scratch buffer for current mode
C-c s t   occur: TODO keywords   C-c s u   occur: URLs
C-c s l   occur: long lines      C-c s s   project-grep symbol
C-c q j   quick-jump register    C-c q l   list registers
C-c q s   save window config to register
```

---

## Feature Modules

### `ui-core.el`
ef-themes, nerd-icons, font setup, pixel-smooth-scroll, line numbers, show-paren,
hl-line, doom-modeline, rainbow-delimiters, highlight-numbers, hl-todo, beacon,
dimmer, pulsar, highlight-indent-guides, which-key, ace-window, winner-mode,
neotree, visual-fill-column, tab-bar.

### `ui-theme.el`
ef-themes toggle/select, environment-specific theme override, auto dark/light by
time (configurable dark-hour / light-hour), modus-themes migration helper.
```
F12       toggle dark/light
M-x emacs-ide-theme-enable-auto / disable-auto
M-x emacs-ide-select-theme
```

### `ui-modeline.el`
doom-modeline with an `emacs-ide-health` segment showing ✓/⚠N/✗N. Click to
run health check. Powerline fallback if doom-modeline is absent.

### `ui-dashboard.el`
dashboard.el with three custom sections: IDE actions, workspaces (persp.el),
and the live health check results. Kills itself on first file-open.

### `ui-workspace.el`
perspective.el workspaces reflected in tab-bar. Default workspaces from
config.yml. Auto-creates a workspace per Projectile project. Consult buffer
source scoped to current workspace. Save on exit.
```
C-c W s   switch   C-c W n   new   C-c W k   kill   C-c W r   rename
C-c W 1–9  by index
M-x emacs-ide-workspace-status
```

### `completion-core.el`
vertico + vertico-multiform, orderless, marginalia, consult (full binding set),
consult-lsp, consult-projectile, embark + embark-consult, corfu + corfu-popupinfo
+ corfu-history, Cape, hippie-expand, savehist, saveplace, abbreviations.

### `completion-snippets.el`
yasnippet + yasnippet-snippets + yasnippet-capf (integrates with corfu).

### `editing-core.el`
delete-selection-mode, auto-revert, trailing-whitespace strip, repeat-mode,
editorconfig, smartparens (with Markdown/Org pairs), **ide-pair wrap-on-select**,
undo-tree, multiple-cursors, expand-region, avy, move-text, surround, olivetti,
wgrep, whitespace, electric-pair-mode (global fallback). Optional Meow modal.
```
M-x emacs-ide-toggle-meow / install-meow
```

### `core-dev.el`
Language registry (`emacs-ide-dev-register`), enable-check (config.yml
`languages:` section), treesit-auto, compile/repl/formatter/DAP/test-runner
helper macros used by all lang modules.

### `tools-lsp.el`
lsp-mode with config-scoped `lsp-client-packages` (only enabled langs loaded),
inlay hints, semantic tokens, sideline, breadcrumb, flycheck, lsp-ui, lsp-treemacs
(deferred), dumb-jump xref fallback.
```
C-c L     lsp-status
M-x emacs-ide-lsp-check-servers
```

### `tools-project.el`
Projectile with rg/fd/find indexing, config-driven search paths and ignored dirs,
ibuffer-project, treemacs + treemacs-projectile + treemacs-magit, project
compile/run/test auto-detect, project create helpers (Python/Rust/Go).
```
C-c p *   projectile-command-map
F9        treemacs
```

### `tools-git.el`
magit + diff-hl (gutter) + git-timemachine + magit-todos + git-link + forge +
git-commit. Full set of convenience wrappers.
```
C-x g     magit-status
C-x v t   git-timemachine
```

### `tools-terminal.el`
vterm + multi-vterm, eshell, dired (with diredfl), dockerfile-mode,
docker-compose-mode, docker integration. ANSI color in compile buffers.
```
C-c t   vterm-here    C-c e   eshell-here
```

### `tools-format.el`
apheleia (on-save formatting). Formatter status command.
```
M-x emacs-ide-check-formatters
```

### `apheleia-langs-patch.el`
Complete formatter map for all 29 supported languages. Canonical formatter
assignments loaded after apheleia so lang modules' own assignments don't override.

### `tools-org.el`
org-mode + org-bullets, full capture templates (task/note/meeting/bug), custom
agenda views, babel languages, environment-specific org-directory from config.yml.
```
C-c a/c/l   agenda/capture/store-link
```

### `tools-spelling.el`
flyspell (aspell → hunspell → ispell), flyspell-correct + avy-menu, smart
check-word predicate (skips URLs, long identifiers).
```
M-x emacs-ide-spell-toggle / spell-buffer
```

### `tools-notes.el`
org-roam + org-roam-ui + consult-org-roam. Notes directory from config.yml
`general.notes-directory`. Lazy DB autosync after 3s idle.
```
C-c n f/i/b/d/D/g/c/s   roam commands
C-c n n   neotree
C-c n /   full-text search notes directory
```

### `tools-rest.el`
verb (org-mode HTTP client) + restclient + restclient-jq.
```
C-c V s   REST scratch buffer
C-c V i   insert request template (org-mode)
```

### `tools-test-runner-registry.el`
Per-major-mode test runner registry. Language modules register via
`emacs-ide-test-register-runner`. Fallback marker-file detection.
```
C-c X f   file    C-c X p   project
C-c X .   point   C-c X w   watch
C-c X l   last    C-c X s   runner status
C-c C-t   smart dispatch   C-c C-T   force project suite
```

### `tools-test.el`
Auto-detect and dispatch, last-command repeat, history ring (50 entries), report.

### `debug-core.el`
dap-mode (when `debug.enable: true`), convenience wrappers for breakpoints,
conditional breakpoints, DAP REPL.
```
M-x emacs-ide-debug-toggle-breakpoint
M-x emacs-ide-debug-repl
```

### `tools-repl.el`
Unified REPL hub. Mode registry with `:launch`, `:buffer-name`, `:send-region-fn`.
Built-in registrations for Python/Rust/JS/Go/Lua/Clojure/Haskell/R/Julia.
Side-window display rules, config-driven height/side/auto-focus.
```
C-c x r   launch/switch   C-c x s   send region
C-c x b   send buffer     C-c x d   send defun
C-c x l   send line       C-c x t   toggle window
```

### `tools-project-detect.el`
29 project marker files, 32-extension map. On project switch and file-open,
pre-warms the appropriate lang module via 0.5s idle timer.
```
C-c D d   show status
M-x emacs-ide-detect-current-project
M-x emacs-ide-detect-reset-cache
```

### `tools-hydra.el`
10 hydras: window, buffer, git, lsp, project, test, debug, toggle, repl, search.
```
C-c h w/b/g/l/p/t/d/u/r/s   open hydra
C-c h D/o/A                   database/devops/analytics hydra
C-c h h                       list all
```

### `tools-database.el`
Named connection registry from config.yml `database.connections`. Schema browser
(information_schema per DB type). 100-entry query history ring, named bookmarks,
interactive SELECT builder. PostgreSQL / MySQL / SQLite / MSSQL.
```
C-c d c   connect          C-c d d   schema browser
C-c d s   status           C-c d t   describe table
C-c d h   query history    C-c d q   SELECT builder
C-c d b   run bookmark     C-c d a   add connection
C-c h D   database hydra
```

### `tools-devops.el`
Docker: live ps/logs/exec/stop/build/run/pull/prune. Compose: auto-detects
`docker-compose.yml`; handles v1 and v2 CLI. Kubernetes: behind
`kubernetes-enable: false`. Terraform: plan→save `.tfplan`→apply. Env: pure-Elisp
`.env` loader with snapshot/restore.
```
C-c o s   devops status
C-c h o   devops hydra
M-x emacs-ide-devops-docker-ps / logs / exec / build
M-x emacs-ide-devops-compose-up / down / logs
M-x emacs-ide-devops-terraform-plan / apply / init
M-x emacs-ide-devops-env-load / env-unload
```

### `emacs-ide-analytics.el`
LOC: 29-language extension map, tokei→cloc→Elisp fallback. Git: commits, top
contributors, 12-week ASCII bar chart, most-changed files. Coverage: reads
`coverage.xml`, `coverage-summary.json`, `cover.out`, `tarpaulin-report.json`
— no re-runs. Complexity: radon/gocyclo + long-function heuristic. Deps:
npm/cargo/pip/bundler/go outdated. Dashboard: all metrics + project checklist.
```
C-c A l   LOC             C-c A g   git stats
C-c A c   coverage        C-c A x   complexity
C-c A d   deps audit      C-c A D   dashboard
C-c A s   status
C-c h A   analytics hydra
```

### `keybindings.el`
```
C-x C-b   ibuffer              C-x b    consult-buffer
C-x g     magit-status         C-x v t  git-timemachine
M-#       dictionary            M-/      hippie-expand
M-o       ace-window            C-M-y    dumb-jump-hydra

C-h f/v/k/F/C/d   helpful-*

C-c a/c/l         org
C-c B/b           compile/recompile
C-c H             keybindings-help
C-c L             lsp-status
C-c P             presentation-mode
C-c R             config-reload
C-c ?             which-key-top-level
F12               toggle-theme

C-c h *           hydra menus (w/b/g/l/p/t/d/u/r/s + D/o/A)
C-c x *           repl hub
C-c X *           test runner
C-c r *           recovery
C-c V *           rest client
C-c W *           workspaces
C-c D *           project-detect + profiler
C-c d *           database client
C-c o *           devops (docker/compose/k8s/tf/env)
C-c A *           analytics
C-c s *           ide-search (occur: t=todo u=url l=long s=symbol)
C-c q *           ide-register (j=jump l=list s=save-win-config)
C-c z             monocle
C-c M-d           duplicate line/region
C-c M-r/c/o/O/.   file rename/path/open-line/date
C-c p *           projectile
C-c n *           notes + org-roam
```

---

## Lang Modules (lazy, zero boot cost)

| Module | Languages | LSP | Formatter |
|---|---|---|---|
| `lang-python.el` | Python | pyright | black + isort |
| `lang-web.el` | JS/TS/HTML/CSS | typescript-language-server | prettier |
| `lang-rust.el` | Rust | rust-analyzer | rustfmt |
| `lang-go.el` | Go | gopls | gofmt/goimports |
| `lang-c.el` | C/C++/CUDA/CMake | clangd | clang-format |
| `lang-jvm.el` | Java/Kotlin/Scala/Groovy | jdtls/kotlin-ls/metals | google-java-format/ktlint |
| `lang-functional.el` | Haskell/Clojure/Elixir/OCaml/Erlang | hls/clojure-lsp/elixir-ls | ormolu/cljfmt/mix-format |
| `lang-lua.el` | Lua | lua-language-server | stylua |
| `lang-shell.el` | Bash/Zsh/Fish | bash-language-server | shfmt |
| `lang-sql.el` | SQL | sqls | pg_format |
| `lang-data.el` | R/Julia | r-languageserver | — |
| `lang-systems.el` | Zig/Nix/D/V | zls/nil | zig-fmt/nixpkgs-fmt |
| `lang-ruby.el` | Ruby/Rails | solargraph/ruby-lsp | rubocop/standardrb |
| `lang-php.el` | PHP | intelephense | php-cs-fixer |
| `lang-csharp.el` | C#/.NET/Unity | omnisharp/csharp-ls | csharpier |
| `lang-dart.el` | Dart/Flutter | dart | dart-format |
| `lang-prose.el` | Markdown/YAML/JSON/TOML/Terraform | yaml-ls/vscode-json-ls | prettier/taplo |

All lang modules: REPL hub registration, test runner registration, DAP debug
template (where applicable), apheleia formatter mapping.

---

## `config.yml` — Key Toggles

```yaml
general:
  theme: ef-dark          # ef-dark ef-light ef-cherie ef-spring …
  font: JetBrains Mono
  font-size: 11

lsp:
  enable: true
  inlay-hints: true
  diagnostics-provider: flycheck  # or flymake

completion:
  backend: corfu
  delay: 0.15
  fuzzy-matching: true

languages:              # false → lang module never loads
  python: true
  haskell: false        # off by default (tier 3)

database:
  default-connection: ~              # name of default connection
  connections: {}                    # named connection alist

devops:
  docker-enable: true
  kubernetes-enable: false           # requires kubectl on PATH
  terraform-enable: true

analytics:
  loc-tool: auto                     # auto → tokei → cloc → elisp
  exclude-dirs: [".git" "node_modules"]

registers:              # populated into Emacs registers at startup
  init: ~/.emacs.d/init.el
  config: ~/.emacs.d/config.yml
  todo: ~/org/todo.org

theme:
  auto-switch: false    # true → dark/light by dark-hour/light-hour
  dark-hour: 19
  light-hour: 7

performance:
  gc-threshold: 16777216   # 16 MB
  startup-time-target: 3.0
```

---

## Tool Installation

```bash
# LSP servers
pip install pyright                  # Python
npm install -g typescript-language-server  # JS/TS
rustup component add rust-analyzer   # Rust
go install golang.org/x/tools/gopls@latest  # Go
# (see config.yml QUICK REFERENCE for full list)

# Formatters
pip install black isort              # Python
npm install -g prettier              # JS/TS/JSON/YAML/Markdown
cargo install rustfmt                # Rust (usually comes with rustup)
go install golang.org/x/tools/cmd/gofmt@latest  # Go

# Analytics (optional — Elisp fallback always available)
cargo install tokei                  # LOC counter (fastest)
pip install radon                    # Python complexity

# Diagnostics runtime
M-x emacs-ide-health-check-all
M-x emacs-ide-lsp-check-servers
M-x emacs-ide-check-formatters
M-x emacs-ide-diagnose
```

---

## Diagnostics Quick Reference

```
M-x emacs-ide-spot-check         command + keybinding + feature integrity
M-x emacs-ide-run-tests          full ERT suite
M-x emacs-ide-diagnose           per-module file + feature + lib layer
M-x emacs-ide-health-check-all   system tools + LSP + formatters
M-x emacs-ide-security-check     TLS + GPG + auth-source
M-x emacs-ide-startup-report     phase timings (init.el)
M-x emacs-ide-early-init-report  phase timings (early-init.el)
M-x emacs-ide-package-report     slowest packages
M-x emacs-ide-telemetry-report   most-used commands
M-x emacs-ide-analytics-dashboard project health overview
```

---

## Changelog

| Version | Changes |
|---|---|
| **v3.5.0** | `tools-database` — named connections, schema browser, query history, SELECT builder · `tools-devops` — Docker/Compose v1+v2/Kubernetes/Terraform/.env loader · `emacs-ide-analytics` — LOC (29 langs), git bar chart, coverage reader, complexity, deps audit, unified dashboard · Lib layer: 6 `ide-*.el` shared utilities (`ide-common`, `ide-simple`, `ide-window`, `ide-search`, `ide-pair`, `ide-register`) · Phase 4 wires: `editing-core`→`ide-pair`, `keybindings`→`ide-simple`+`ide-register`+`ide-search`, analytics/database/devops→`ide-common`, `emacs-ide-diagnose` lib layer section · config.yml `database:`/`devops:`/`analytics:`/`registers:` sections · keybinding prefixes `C-c d`, `C-c o`, `C-c A`, `C-c s`, `C-c q`, `C-c z` |
| v3.4.1 | Added `emacs-ide-config-get` · `emacs-ide-config-show` · `defvar` for 30+ config variables · Fixed `(when emacs-ide-debug-enable)` → `bound-and-true-p` · `emacs-ide-startup-time-target` wired to config.yml |
| v3.4.0 | `lang-ruby` · `lang-php` · `lang-csharp` · `lang-dart` · apheleia C#/Dart/Clojure/Terraform/TOML |
| v3.3.1 | exec-path-from-shell · global-so-long-mode · bidi defaults · dumb-jump · ibuffer-project · editorconfig · repeat-mode · treesit-auto |
| v3.0.0 | 50-lang lazy loading · ef-themes · nerd-icons · hydra menus · workspaces · REPL hub · Meow |
