# Emacs IDE — v3.4.1

`Emacs 29.1+` · `straight.el` · `52 modules` · `50 languages` · `<2s cold start`

```bash
git clone <repo> ~/.emacs.d && emacs --init-directory ~/.emacs.d
# First run: straight.el bootstraps + installs all packages (~3 min, once)
# Safe mode: emacs --emacs-ide-safe  |  EMACS_SAFE_MODE=1 emacs
# After Emacs upgrade: M-x emacs-ide-purge-bytecode-cache
```

---

## Architecture

```
early-init.el        18-phase startup: GC · frame · native-comp · JIT · TLS
init.el              Bootstrap · exec-path · so-long · bidi · buf-rules
config.yml           Single source of truth — M-x emacs-ide-config-reload (C-c R)

core/       (10)     Always loaded at boot
modules/    (26)     Eager feature modules, loaded in strict order
modules/langs/ (17)  Lazy lang modules — zero boot cost, load on file open
var/                 Runtime state (never commit)
snippets/            YASnippet custom snippets
```

**Load order:**
```
config → health → diagnose → package → profiler → security → telemetry → recovery
→ ui-core → ui-theme → ui-modeline → ui-dashboard → ui-workspace
→ completion-core → completion-snippets → editing-core → core-dev
→ tools-lsp → tools-project → tools-git → tools-terminal
→ tools-format → apheleia-langs-patch
→ tools-org → tools-spelling → tools-notes → tools-rest
→ tools-test-runner-registry → tools-test → debug-core
→ tools-repl → tools-project-detect → tools-hydra
→ keybindings                     ← always last
langs/: lazy on file-open or project-switch
```

---

## Runtime Dependencies

**Required:** `git` `ripgrep` `emacs 29.1+`  
**Recommended:** `fd` `node/npm` `python3` `go` `rust/cargo` `java 21`  
**Fonts:** `M-x nerd-icons-install-fonts` (once)

---

## Module Reference

### `early-init.el`
18 benchmark phases. GC deferred during init, restored to 16MB after. `file-name-handler-alist` disabled at phase 2. Native comp speed 2, async silent. TLS: gnutls verify + 3072-bit primes. Theme flash prevention guarded by `display-graphic-p`.
```
M-x emacs-ide-early-init-report
```

---

### `init.el`
Bootstraps straight.el, loads all modules in order. `exec-path-from-shell` imports 13 env vars. `global-so-long-mode`, bidi, bookmarks, buffer display rules.
```
M-x emacs-ide-show-version
M-x emacs-ide-startup-report
M-x emacs-ide-config-reload        C-c R
M-x emacs-ide-purge-bytecode-cache
M-x emacs-ide-update
M-x emacs-ide-freeze-versions
```

---

### `core/emacs-ide-config.el`
Hand-rolled YAML parser. Reads `config.yml` → nested alist → applies to all IDE variables. All config variables have `defvar` fallbacks so modules are safe before config loads.
```
M-x emacs-ide-config-reload        C-c R
M-x emacs-ide-config-edit
M-x emacs-ide-config-show
```
**Public API:** `(emacs-ide-config-get 'section 'key default)`

---

### `core/emacs-ide-health.el`
Registry of health checks: `system-tools` `lsp` `config` `emacs-version`. Runs on 60s idle. Results shown in modeline.
```
M-x emacs-ide-health-check-all     (alias: emacs-ide-health-status)
M-x emacs-ide-health-auto-fix
```

---

### `core/emacs-ide-diagnose.el`
```
M-x emacs-ide-diagnose
M-x emacs-ide-diagnose-lsp
M-x emacs-ide-diagnose-languages
```

---

### `core/emacs-ide-recovery.el`
Crash counter, config backup, package disable, 5-min periodic flush.
```
M-x emacs-ide-recovery-report
M-x emacs-ide-recovery-view-log
M-x emacs-ide-recovery-backup-config
M-x emacs-ide-recovery-restore-config
M-x emacs-ide-recovery-disable-package
M-x emacs-ide-recovery-reset-crash-count   C-c r C-r
```

---

### `core/emacs-ide-security.el`
TLS verify, 3072-bit primes, gnutls-cli, auth-sources, `network-security-level`. Re-applies on config reload.
```
M-x emacs-ide-security-check
M-x emacs-ide-security-harden
```

---

### `core/emacs-ide-package.el`
Wraps `require` to track per-package load times.
```
M-x emacs-ide-package-report
M-x emacs-ide-package-clear-times
```

---

### `core/emacs-ide-profiler.el`
```
M-x emacs-ide-profile-start        C-c D s
M-x emacs-ide-profile-report       C-c D r
M-x emacs-ide-profile-stop         C-c D q
M-x emacs-ide-profile-reset
M-x emacs-ide-profile-startup
```

---

### `core/emacs-ide-telemetry.el`
Local-only command tracking. Logs startup metrics to `var/telemetry.log`.
```
M-x emacs-ide-telemetry-report
M-x emacs-ide-telemetry-clear
M-x emacs-ide-telemetry-enable
M-x emacs-ide-telemetry-disable
```

---

### `core/emacs-ide-test.el`
ERT suite: Emacs version, startup phases, directory structure, core modules, health/recovery system, packages, completion, editing modes, theme, git.
```
M-x emacs-ide-run-tests
```

---

### `core/emacs-ide-spot-check.el`
Integrity check: 94+ commands, 42 direct-symbol keybindings, 13 lambda bindings, 37 module features.
```
M-x emacs-ide-spot-check
```

---

### `ui-core.el`
ef-themes, nerd-icons, font stack, ligatures, pixel-scroll, line numbers, show-paren, hl-line, doom-modeline, rainbow-delimiters, hl-todo, beacon, dimmer, pulsar, which-key, ace-window, winner-mode, neotree, tab-bar, presentation mode.
```
M-x emacs-ide-presentation-mode    C-c P
F12   toggle dark/light theme
```

---

### `ui-theme.el`
ef-themes toggle and auto dark/light switching by hour. Per-environment theme override.
```
M-x emacs-ide-toggle-theme         F12
M-x emacs-ide-select-theme
M-x emacs-ide-theme-enable-auto
M-x emacs-ide-theme-disable-auto
```

---

### `ui-workspace.el`
perspective.el named workspaces. Tab-bar shows workspace names. Auto-creates workspace per project. Saves on exit.
```
C-c W s/n/k/r   switch/new/kill/rename
C-c W b/i/l/w   buf/ibuffer/load/save
M-1..9          switch by index
M-x emacs-ide-workspace-status
```

---

### `completion-core.el`
vertico + orderless + marginalia + consult + embark + corfu + cape. All delays/prefixes/heights config-driven via `emacs-ide-config-get`.
```
C-x b     consult-buffer          M-y       consult-yank-pop
M-s l     consult-line            M-s r     consult-ripgrep
C-.       embark-act              C-;       embark-dwim
```

---

### `editing-core.el`
`delete-selection-mode`, `global-auto-revert-mode`, smartparens, undo-tree, multiple-cursors, expand-region, avy, move-text, editorconfig, repeat-mode.
```
C->       mc/mark-next            C-=       er/expand-region
C-:       avy-goto-char           M-↑/↓    move-text
C-/       undo-tree-undo          C-x u     undo-tree-visualize
```

---

### `core-dev.el`
Shared API for lang modules: language registry, `emacs-ide-dev-lang-enabled-p`, compile/repl/formatter/dap/test-runner helpers. `treesit-auto` for grammar management.

---

### `tools-lsp.el`
lsp-mode for C/C++/Python/Rust/Go/Java/JS/TS (classic + ts-mode). lsp-ui, flycheck, lsp-treemacs. `dumb-jump` as xref fallback (covers any language).
```
C-c l r/f/a/d/R/i/h   rename/format/action/definition/refs/impl/hover
C-M-y                  dumb-jump-hydra
M-x emacs-ide-lsp-status           C-c L
M-x emacs-ide-lsp-check-servers
```

---

### `tools-project.el`
Projectile (alien indexing, rg/fd), consult-projectile, treemacs, ibuffer-project.
```
C-c p f/p/s r/c/t/r/b/k   find/switch/ripgrep/compile/test/run/buf/kill
F9                          treemacs
M-x emacs-ide-project-info
M-x emacs-ide-project-compile / run / test
```

---

### `tools-git.el`
Magit, diff-hl, git-timemachine, magit-todos, git-link, forge. Gated on `git.enable`.
```
C-x g     magit-status    C-x M-g   magit-dispatch    C-x v t   timemachine
M-x emacs-ide-git-status / stage-file / blame-toggle / copy-link / stash
```

---

### `tools-terminal.el`
compile (ANSI color), comint, vterm, multi-vterm, eshell, dired, docker.
```
C-c t     vterm-here      C-c e     eshell-here
C-c M-t   multi-vterm
M-x emacs-ide-vterm-toggle / project / run-file
```

---

### `tools-format.el` + `apheleia-langs-patch.el`
apheleia async format on save. 50-language formatter map covering all lang modules.
```
M-x emacs-ide-check-formatters
```

---

### `tools-repl.el`
Unified REPL hub. Side-window display (side + height from config). Built-in registrations for Python, Rust, Node, Go, Lua, Clojure, Haskell, R, Julia.
```
C-c x r/s/b/d/l/t   launch/send-region/send-buf/send-defun/send-line/toggle
M-x emacs-ide-repl-status
```

---

### `tools-test.el` + `tools-test-runner-registry.el`
Auto-detects framework from mode + markers. Per-lang runner registry. Test history with pass/fail/duration.
```
C-c C-t   smart-dispatch    C-c C-T   force project suite
C-c X f/p/./w/s/l           file/project/at-point/watch/status/last
M-x emacs-ide-test-report
```

---

### `debug-core.el`
dap-mode (sessions, locals, controls, REPL). Gated on `debug.enable`.
```
M-x emacs-ide-debug-toggle-breakpoint
M-x emacs-ide-debug-repl
```

---

### `tools-hydra.el`
10 hydra menus under `C-c h`: window · buffer · git · lsp · project · test · debug · toggles · repl · search.
```
C-c h w/b/g/l/p/t/d/u/r/s   open hydra
C-c h h                       list all
```

---

### `tools-project-detect.el`
Detects language from project markers on project-switch and find-file. Pre-warms lang module on 0.5s idle.
```
C-c D d   detect-show-status
M-x emacs-ide-detect-current-project
M-x emacs-ide-detect-reset-cache
```

---

### `tools-org.el`
Full org-mode: agenda, capture (task/note/meeting/bug), babel, export. Paths from config per environment.
```
C-c a/c/l   agenda/capture/store-link
```

---

### `tools-notes.el`
org-roam, org-roam-ui graph, consult-org-roam. 5 capture templates.
```
C-c n f/i/b/d/g/s/p/c   find/insert/buffer/today/graph/search/project/capture
```

---

### `tools-spelling.el`
flyspell (aspell → hunspell → ispell). flyspell-correct with avy-menu.
```
C-c S s/b/n/t/c   word/buffer/next/toggle/correct
```

---

### `tools-rest.el`
verb (org-mode HTTP) + restclient (`.http` files).
```
C-c V s/i   rest-scratch/insert-template
```

---

### `keybindings.el`

```
C-x C-b   ibuffer                 C-x b     consult-buffer
C-x g     magit-status            C-x v t   git-timemachine
M-#       dictionary              M-/       hippie-expand
M-o       ace-window              C-M-y     dumb-jump-hydra

C-h f/v/k/F/C/d   helpful-*

C-c a/c/l         org
C-c B/b           compile/recompile
C-c H             keybindings-help
C-c L             lsp-status
C-c P             presentation-mode
C-c R             config-reload
C-c ?             which-key-top-level
F12               toggle-theme

C-c h *           hydra menus
C-c x *           repl hub
C-c X *           test runner
C-c r *           recovery
C-c V *           rest client
C-c W *           workspaces
C-c D *           project-detect + profiler
C-c p *           projectile
C-c n *           notes + org-roam
```

---

## Lang Modules (lazy, zero boot cost)

| Module | Languages | LSP | Formatter | Test | REPL |
|---|---|---|---|---|---|
| `lang-python` | Python | pyright | black+isort | pytest | ipython |
| `lang-rust` | Rust | rust-analyzer | rustfmt | cargo test | evcxr |
| `lang-web` | JS/TS/HTML/CSS | tsserver / vscode-ls | prettier | jest/vitest | node |
| `lang-go` | Go | gopls | gofmt/goimports | go test | gore |
| `lang-c` | C/C++/CUDA/CMake | clangd | clang-format | ctest | — |
| `lang-jvm` | Java/Kotlin/Scala/Groovy | jdtls/kotlin-ls/metals | gjf/ktlint/scalafmt | mvn/gradle/sbt | — |
| `lang-lua` | Lua | lua-language-server | stylua | busted | lua |
| `lang-shell` | Bash/Zsh/Fish | bash-ls | shfmt | bats | bash |
| `lang-sql` | SQL | sqls | pg_format | — | sql |
| `lang-data` | R/Julia | r-ls/julia-ls | styler | Rscript | R/julia |
| `lang-functional` | Haskell/Clojure/Elixir/OCaml/Erlang | hls/clojure-lsp/elixir-ls/ocamllsp | ormolu/cljfmt/mix-fmt | cabal/clj/mix | ghci/cider/iex |
| `lang-systems` | Zig/Nix/D/V | zls/nil | zigfmt/nixpkgs-fmt | zig test | nix repl |
| `lang-prose` | MD/YAML/TOML/JSON/Docker/Terraform | yaml-ls/json-ls | prettier/taplo/terraform-fmt | — | — |
| `lang-ruby` | Ruby/Rails | solargraph/ruby-lsp | rubocop/standardrb | rspec | irb/pry |
| `lang-php` | PHP | intelephense/phpactor | php-cs-fixer/phpcbf | phpunit/pest | psysh |
| `lang-csharp` | C#/.NET | omnisharp/csharp-ls | csharpier | dotnet test | dotnet-script |
| `lang-dart` | Dart/Flutter | dart analysis server | dart format | flutter test | — |

---

## `config.yml` — Key Toggles

```yaml
lsp:
  enable: true          # false → tools-lsp.el skipped entirely
  inlay-hints: true

git:
  enable: true          # false → magit + diff-hl + forge not loaded

debug:
  enable: true          # false → dap-mode not loaded; helper cmds still available

editing:
  meow: false           # true → Meow modal editing

theme:
  auto-switch: false    # true → dark/light by dark-hour/light-hour

languages:              # false → lang module never loads
  python: true
  haskell: false        # off by default (tier 3)
```

---

## Diagnostics

```
M-x emacs-ide-spot-check               commands + keybindings + features
M-x emacs-ide-run-tests                full ERT suite
M-x emacs-ide-diagnose                 module load + config validation
M-x emacs-ide-diagnose-lsp             LSP server PATH check
M-x emacs-ide-diagnose-languages       per-lang enable/load/LSP/formatter matrix
M-x emacs-ide-health-status            system tools + LSP + config health
M-x emacs-ide-health-auto-fix          attempt auto remediation
M-x emacs-ide-config-show              active variable values
M-x emacs-ide-lsp-check-servers        which servers are on PATH
M-x emacs-ide-startup-report           phase timings
M-x emacs-ide-package-report           top-20 slowest package loads
M-x emacs-ide-repl-status              live REPL registry
M-x emacs-ide-test-runner-status       registered per-lang test runners
M-x emacs-ide-workspace-status         perspective workspaces + buffer counts
M-x emacs-ide-security-check           TLS + auth-sources + GPG
M-x emacs-ide-recovery-report          crash count + log + disabled pkgs
M-x emacs-ide-telemetry-report         session command usage
M-x emacs-ide-check-formatters         formatter PATH status
M-x emacs-ide-detect-show-status       project detection + pre-warmed langs
```

---

## Recovery

```bash
EMACS_SAFE_MODE=1 emacs
```
→ `M-x emacs-ide-recovery-view-log` → `M-x emacs-ide-recovery-disable-package` → restart → `M-x emacs-ide-recovery-reset-crash-count`

---

## Changelog

| Version | Changes |
|---|---|
| **v3.4.1** | Added `emacs-ide-config-get` (was referenced everywhere, never defined) · Added `emacs-ide-config-show` · Added `defvar` for all 30+ config variables · Fixed bare `(when emacs-ide-debug-enable)` and `(when emacs-ide-git-enable)` to `bound-and-true-p` · Added `defvar emacs-ide-startup-time-target` · Defined `emacs-ide-startup-time-target` wired to `config.yml performance.startup-time-target` |
| v3.4.0 | lang-ruby · lang-php · lang-csharp · lang-dart · apheleia-langs-patch C#/Dart/Clojure/Terraform/TOML entries |
| v3.3.1 | exec-path-from-shell · global-so-long-mode · bidi defaults · dumb-jump · ibuffer-project · editorconfig · repeat-mode · treesit-auto · display-buffer-alist polish |
| v3.2.x | recovery timer · lsp-check-servers · diagnose-languages · repl-send-line · config reload hook · modeline keymap · 15 bug fixes |
| v3.0.0 | 50-lang lazy loading · ef-themes · nerd-icons · hydra menus · workspaces · REPL hub · Meow |
