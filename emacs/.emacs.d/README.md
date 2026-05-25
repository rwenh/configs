# Emacs IDE — v3.3.1

`Emacs 29.1+` · `straight.el` · `52 modules` · `50 languages` · `<2s cold start`

```bash
git clone <repo> ~/.emacs.d && emacs --init-directory ~/.emacs.d
# First launch: straight.el bootstraps + installs all packages (~3 min, once only)
# Safe mode:    emacs --emacs-ide-safe  |  EMACS_SAFE_MODE=1 emacs
# After Emacs upgrade: M-x emacs-ide-purge-bytecode-cache
```

---

## Architecture

```
early-init.el          18-phase startup: GC · frame · native-comp · JIT · bidi · TLS
init.el                Bootstrap · exec-path · so-long · bidi · bookmarks · buf-rules
config.yml             Single source of truth — M-x emacs-ide-config-reload to apply

core/        (10)      Always loaded at boot, before any feature module
modules/     (26)      Eager feature modules, loaded in strict order
modules/langs/ (13)    Lazy lang modules — zero boot cost, load on file open
var/                   Runtime state (never commit)
snippets/              YASnippet custom snippets
```

**Load order (strict):**
```
config → health → diagnose → package → profiler → security → telemetry → recovery
→ ui-core → ui-theme → ui-modeline → ui-dashboard → ui-workspace
→ completion-core → completion-snippets → editing-core → core-dev
→ tools-lsp → tools-project → tools-git → tools-terminal
→ tools-format → apheleia-langs-patch
→ tools-org → tools-spelling → tools-notes → tools-rest
→ tools-test-runner-registry → tools-test → debug-core
→ tools-repl → tools-project-detect → tools-hydra
→ keybindings                          ← always last
→ [test + spot-check]                  ← eagerly at end of init
langs/: lazy on file-open or project-switch
```

---

## Runtime Dependencies

**Required:** `git` `ripgrep` `emacs 29.1+`  
**Recommended:** `fd` `node/npm` `python3` `go` `rust/cargo` `java 21`  
**Fonts:** `M-x nerd-icons-install-fonts` (once)

**Packages auto-installed by straight.el** (selected):

| Layer | Packages |
|---|---|
| Completion | `vertico` `orderless` `marginalia` `consult` `embark` `corfu` `cape` `yasnippet` |
| UI | `ef-themes` `nerd-icons` `doom-modeline` `dashboard` `perspective` `rainbow-delimiters` `hl-todo` `beacon` `dimmer` `pulsar` `which-key` `avy` `ace-window` `neotree` `treemacs` |
| Editing | `smartparens` `undo-tree` `multiple-cursors` `expand-region` `move-text` `wgrep` `olivetti` `editorconfig` |
| LSP/DAP | `lsp-mode` `lsp-ui` `flycheck` `dap-mode` `dumb-jump` |
| Project | `projectile` `consult-projectile` `treemacs-projectile` `ibuffer-project` `treesit-auto` |
| Git | `magit` `diff-hl` `git-timemachine` `forge` `git-link` `magit-todos` |
| Format | `apheleia` |
| Terminal | `vterm` `multi-vterm` |
| Notes | `org-roam` `org-roam-ui` `consult-org-roam` |
| REST | `verb` `restclient` |
| Hydra | `hydra` |
| Env | `exec-path-from-shell` |
| Tree-sitter | `treesit-auto` |

---

## Module Reference

### `early-init.el`
18 benchmark phases. GC deferred to `most-positive-fixnum` during init, restored to 16MB after. `file-name-handler-alist` disabled at phase 2 (covers all subsequent requires). Native comp: speed 2, async silent. bidi: `left-to-right` + `inhibit-bpa`. JIT lock, redisplay, I/O, Wayland/pgtk tuned. TLS: gnutls verify + 3072-bit primes. Theme flash prevention guards on `display-graphic-p`. Safe mode via `--emacs-ide-safe` flag.

```
M-x emacs-ide-early-init-report    per-phase benchmark timings
```

---

### `init.el`
Bootstraps straight.el, loads all modules in order, tracks startup phases. **Calibrated additions:** `exec-path-from-shell` imports `PATH SSH_AUTH_SOCK SSH_AGENT_PID GPG_AGENT_INFO GOPATH GOROOT PYTHONPATH JAVA_HOME CARGO_HOME RUSTUP_HOME LANG LC_CTYPE NVM_DIR PYENV_ROOT RBENV_ROOT` from login shell. `global-so-long-mode` enabled globally. `bidi-paragraph-direction left-to-right` + `bidi-inhibit-bpa t` as defaults. `bookmark-save-flag 1` (crash-safe). `switch-to-buffer-obey-display-actions t` + `switch-to-buffer-in-dedicated-window pop`.

```
M-x emacs-ide-show-version
M-x emacs-ide-startup-report
M-x emacs-ide-reload-init          destructive full reload
M-x emacs-ide-config-reload        safe config-only reload  (C-c R)
M-x emacs-ide-purge-bytecode-cache delete .elc/.eln, prompt restart
M-x emacs-ide-update               straight-pull-all + rebuild
M-x emacs-ide-freeze-versions      write straight/versions/default.el
```

---

### `core/emacs-ide-config.el`
Hand-rolled YAML parser (4 indent levels). Reads `config.yml` → nested alist → applies to all IDE variables. Environment detection via `$EMACS_ENVIRONMENT` or hostname. `emacs-ide-config-reload-hook` fires only on explicit reload, never on startup load.

```
M-x emacs-ide-config-reload        C-c R
M-x emacs-ide-config-edit
M-x emacs-ide-config-show
```

**Key config sections:** `general` `lsp` `completion` `performance` `features` `editing` `theme` `workspace` `project` `git` `terminal` `debug` `languages` `lang-settings` `repl` `security` `telemetry` `environments`

---

### `core/emacs-ide-health.el`
Registry of health checks. Runs on 60s idle timer after init. Checks: `system-tools` `lsp` `config` `emacs-version`. Results cached in `emacs-ide-health-results`. Summary string fed to modeline segment.

```
M-x emacs-ide-health-check-all     (alias: emacs-ide-health-status)
M-x emacs-ide-health-auto-fix      cancel void timers, re-run checks
```

---

### `core/emacs-ide-diagnose.el`
Comprehensive diagnostics across all modules, modes, config, LSP servers, and language formatters.

```
M-x emacs-ide-diagnose
M-x emacs-ide-diagnose-lsp
M-x emacs-ide-diagnose-languages
```

---

### `core/emacs-ide-recovery.el`
Crash counter increments on every startup, resets on clean `kill-emacs-hook`. Periodic 5-min idle timer flushes state. Config backup to `var/backups/`. Session timer declared as `emacs-ide-recovery--session-timer`.

```
M-x emacs-ide-recovery-report
M-x emacs-ide-recovery-view-log
M-x emacs-ide-recovery-backup-config
M-x emacs-ide-recovery-restore-config
M-x emacs-ide-recovery-disable-package
M-x emacs-ide-recovery-reset-crash-count   C-c r C-r
```
```
C-c r r   report    C-c r v   log    C-c r b   backup    C-c r d   disable pkg
```

---

### `core/emacs-ide-security.el`
TLS verify, 3072-bit primes, gnutls-cli program, `package-check-signature`, auth-sources, `network-security-level`. Re-applies on config reload.

```
M-x emacs-ide-security-check
M-x emacs-ide-security-harden
```

---

### `core/emacs-ide-package.el`
Wraps `require` with advice to track per-package load times. Slow threshold configurable. Report shows top-20 slowest.

```
M-x emacs-ide-package-report
M-x emacs-ide-package-clear-times
```

---

### `core/emacs-ide-profiler.el`
Wraps Emacs built-in `profiler`. State synced to prevent double-start.

```
M-x emacs-ide-profile-start        C-c D s
M-x emacs-ide-profile-report       C-c D r
M-x emacs-ide-profile-stop         C-c D q
M-x emacs-ide-profile-reset
M-x emacs-ide-profile-startup      uses esup if available
M-x emacs-ide-early-init-report
```

---

### `core/emacs-ide-telemetry.el`
Local-only. Tracks command counts via `post-command-hook` with 0.5s idle flush. Logs startup metrics (elapsed, GC count, pkg count) to `var/telemetry.log` with rotation at 10MB.

```
M-x emacs-ide-telemetry-report
M-x emacs-ide-telemetry-clear
M-x emacs-ide-telemetry-enable
M-x emacs-ide-telemetry-disable
```

---

### `core/emacs-ide-test.el`
ERT suite covering: Emacs version, startup phases, directory structure, core module features, health system, recovery system, packages, completion, editing modes, theme, git binary, required files.

```
M-x emacs-ide-run-tests
```

---

### `core/emacs-ide-spot-check.el`
Integrity check for all 94+ commands, 42 direct-symbol keybindings, 13 lambda-bound keybindings, and 37 module features. Direct bindings verified with strict `eq`; lambda bindings verified as non-nil.

```
M-x emacs-ide-spot-check
```

---

### `ui-core.el`
ef-themes, nerd-icons (+ dired/completion/corfu icon integrations), font stack (JetBrains Mono → Cascadia → Fira → Iosevka → Source Code Pro), ligatures, pixel-scroll precision, global line numbers (relative configurable), `show-paren` mixed style, `global-hl-line-mode`, doom-modeline, rainbow-delimiters, rainbow-mode, highlight-numbers, hl-todo, beacon, dimmer, pulsar, highlight-indent-guides, which-key, ace-window, winner-mode, neotree, diredfl, visual-fill-column, tab-bar, presentation mode.

**Calibrated additions:**
- `display-buffer-alist`: `*Help*` own window · `*Completions*` own window 10-line · `*Dictionary*` left side-window 70-char
- `Man-notify-method 'aggressive` — man pages focus immediately
- `ediff-window-setup-function 'ediff-setup-windows-plain` — no popup frame

```
M-x emacs-ide-presentation-mode    C-c P
M-x emacs-ide-set-transparency
F12   toggle dark/light theme
```

---

### `ui-theme.el`
ef-themes toggle (dark ↔ light). Auto dark/light switching by hour (`theme.dark-hour` / `theme.light-hour`). Per-environment theme override from `config.yml`. `emacs-ide-after-theme-hook` fires after every `load-theme`. Modus-themes migration helper.

```
M-x emacs-ide-toggle-theme         F12
M-x emacs-ide-select-theme
M-x emacs-ide-theme-enable-auto
M-x emacs-ide-theme-disable-auto
```

---

### `ui-modeline.el`
doom-modeline with custom `emacs-ide-health` segment (click → run health check). Keymap created lazily (no TTY errors). Falls back to powerline → default modeline.

---

### `ui-dashboard.el`
Startup dashboard with custom sections: `ide-actions` `ide-workspace` `ide-health` `recents` `projects` `bookmarks`. Refreshes on 6s idle. Kills itself when first file opens.

---

### `ui-workspace.el`
perspective.el named workspaces. Tab-bar shows workspace names. Auto-creates workspace per Projectile project. Saves on exit. Burly window-layout bookmarks.

```
C-c W s   switch    C-c W n   new       C-c W k   kill      C-c W r   rename
C-c W b   buf       C-c W i   ibuffer   C-c W l   load      C-c W w   save
C-c W S   burly-save                    C-c W R   burly-restore
M-1..9    switch by index
```

```
M-x emacs-ide-workspace-status
M-x emacs-ide-workspace-switch-by-index
```

---

### `completion-core.el`
vertico (multiform: grid for files, buffer for grep/imenu, flat for buffers, alpha-sort for symbols/commands). orderless (literal + regexp + flex). marginalia right-aligned. consult (full binding set). consult-lsp, consult-projectile. embark (which-key indicator). corfu (auto, popupinfo, history, config-driven delay/prefix/height). cape (keyword, elisp-block, file, dabbrev). hippie-expand. savehist, saveplace, abbrevs. Minibuffer: recursive, depth-indicate, electric-default.

**consult bindings:**
```
C-x b     consult-buffer          C-x C-r  consult-recent-file
M-y       consult-yank-pop        M-g g    consult-goto-line
M-s l     consult-line            M-s r    consult-ripgrep
M-s G     consult-git-grep        M-g i    consult-imenu
M-g s     consult-lsp-symbols     M-g e    consult-lsp-diagnostics
C-.       embark-act              C-;      embark-dwim
```

---

### `completion-snippets.el`
yasnippet + yasnippet-snippets + yasnippet-capf (integrates with corfu).

```
C-c y e   expand    C-c y n   new snippet    C-c y v   visit file    C-c y i   insert
```

---

### `editing-core.el`
`delete-selection-mode` `global-auto-revert-mode`. Trailing whitespace on save. Final newline enforced. Smartparens (disables `electric-pair-mode` locally when active). undo-tree (visualizer, timestamps). multiple-cursors. expand-region. avy. move-text. surround. olivetti. wgrep. whitespace. `electric-pair-mode` (global fallback).

**Calibrated additions:**
- `editorconfig-mode` — reads `.editorconfig` per project (built-in on Emacs 30, package on 29)
- `repeat-mode` — Emacs 28+, repeat key sequences by pressing only the last key
- `auto-window-vscroll nil` — eliminates scroll stutter
- `scroll-preserve-screen-position t` — C-v/M-v keeps relative cursor position
- `kill-do-not-save-duplicates t` — clean kill-ring

```
C->       mc/mark-next            C-<      mc/mark-prev
C-=       er/expand-region        C--      er/contract-region
C-:       avy-goto-char           C-'      avy-goto-char-2
M-g w     avy-goto-word-1         M-↑/↓   move-text
C-/       undo-tree-undo          C-?      undo-tree-redo
C-x u     undo-tree-visualize
C-M-f/b   sp-forward/backward-sexp
C-)       sp-forward-slurp        C-}      sp-forward-barf
```

---

### `core-dev.el`
Shared API for all lang modules. Language registry (`emacs-ide-dev-register`). Config language enable check (`emacs-ide-dev-lang-enabled-p`). Helpers: `emacs-ide-dev-bind-compile` `emacs-ide-dev-attach-repl` `emacs-ide-dev-attach-formatter` `emacs-ide-dev-attach-dap` `emacs-ide-dev-register-test-runner`.

**Calibrated addition:** `treesit-auto` replaces manual `emacs-ide-dev-ensure-treesit` idle-timer approach.
- `global-treesit-auto-mode` — auto-switches `foo-mode → foo-ts-mode` when grammar present
- `treesit-auto-install 'prompt` — installs missing grammars on demand
- `treesit-auto-add-to-auto-mode-alist 'all` — wires file extensions to ts-modes
- `emacs-ide-dev-ensure-treesit` kept as no-op shim for backward compat

---

### `tools-lsp.el`
lsp-mode with hooks for C/C++/Python/Rust/Go/Java/JS/TS (classic + ts-mode variants). lsp-ui (doc at-point, sideline, peek, imenu). flycheck on lsp-mode hook. All settings config-driven: `lsp.idle-delay` `lsp.inlay-hints` `lsp.semantic-tokens` `lsp.lens` `lsp.breadcrumb` `lsp.sideline`. Inlay hints propagated to lsp-rust, lsp-lua, lsp-typescript individually. Re-applies on config reload.

**Calibrated addition:** `dumb-jump` as xref fallback.
- Registers via `xref-backend-functions` — LSP wins when active, dumb-jump covers the gap
- Covers any language, any file, even before lang module pre-warms
- Prefers `rg → ag → grep`
- `C-M-y` → `dumb-jump-hydra` (go / other-window / external / prompt / quick-look / back)

```
C-c l r   lsp-rename              C-c l f   lsp-format-buffer
C-c l a   lsp-execute-code-action C-c l d   lsp-find-definition
C-c l R   lsp-find-references     C-c l i   lsp-find-implementation
C-c l h   lsp-describe-thing-at-point
C-M-y     dumb-jump-hydra
```

```
M-x emacs-ide-lsp-status           C-c L
M-x emacs-ide-lsp-check-servers
```

---

### `tools-project.el`
Projectile (alien indexing, rg/fd generic command, recentf sort, ignored dirs from config). consult-projectile. Treemacs + treemacs-projectile + treemacs-magit.

**Calibrated addition:** `ibuffer-project` — groups ibuffer by project root, sorts by project-relative path. Remaps `C-x C-b` to `ibuffer-list-buffers`. `ibuffer-movement-cycle nil`, `ibuffer-old-time 24`.

```
C-c p f   find-file      C-c p p   switch-project  C-c p s r  ripgrep
C-c p c   compile        C-c p t   test            C-c p r    run
C-c p b   switch-buffer  C-c p k   kill-buffers    C-c p i    invalidate-cache
C-c p h   consult-projectile                        F9         treemacs
```

```
M-x emacs-ide-project-info
M-x emacs-ide-project-compile / run / test
M-x emacs-ide-project-create-python / rust / go
```

---

### `tools-git.el`
Magit (same-window diff, all-hunk refine, gravatars, no-confirm stage/unstage). diff-hl (flydiff, margin on TTY). git-timemachine. magit-todos (TODO/FIXME/HACK/NOTE/BUG/PERF/REVIEW). git-link (permalink to GitHub/GitLab). forge (topic limit 60). git-commit (fill-column + summary-max-length from config). All gated on `git.enable`.

```
C-x g     magit-status    C-x M-g   magit-dispatch    C-x v t   git-timemachine
```

```
M-x emacs-ide-git-status / stage-file / unstage-file / commit-amend
M-x emacs-ide-git-push / pull / create-branch / diff-buffer / log-buffer
M-x emacs-ide-git-blame-toggle / copy-link / stash / stash-pop
M-x emacs-ide-git-flow-feature-start / finish
```

---

### `tools-terminal.el`
compile (ANSI color, scroll to first-error, always-kill). comint (read-only prompt, scroll to bottom). vterm (max-scrollback, kill-buffer-on-exit, timer-delay, update-pwd). multi-vterm. eshell (50k history, visual commands). dired (group-dirs-first, dwim-target, wdired). dockerfile-mode, docker-compose-mode, docker.

```
C-c t     vterm-here      C-c T     vterm-other-window    C-c e     eshell-here
C-c M-t   multi-vterm     C-c M-n/p multi-vterm-next/prev
```

```
M-x emacs-ide-vterm-here / toggle / project / send-string / run-command / run-file
M-x emacs-ide-eshell-here
M-x emacs-ide-docker-build / docker-compose-up
```

---

### `tools-format.el` + `apheleia-langs-patch.el`
apheleia (async format on save, no cursor jump). Full 50-language formatter map:

`black/isort/ruff` `prettier` `rustfmt` `gofmt/goimports` `clang-format` `google-java-format` `ktlint` `scalafmt` `stylua` `shfmt` `pgformatter/sqlfluff` `ormolu/fourmolu` `mix-format` `ocamlformat` `zigfmt` `nixpkgs-fmt` `rubocop/standardrb` `phpcbf` `cljfmt` `terraform-fmt` `taplo`

```
M-x emacs-ide-check-formatters
```

---

### `tools-repl.el`
Unified REPL hub with registry per major-mode. Side-window display (side + height from config). Built-in registrations for Python, Rust (evcxr), Node, Go (gore), Lua, Clojure (CIDER), Haskell, R (ESS), Julia.

```
C-c x r   launch/switch   C-c x s   send-region    C-c x b   send-buffer
C-c x d   send-defun      C-c x l   send-line+adv  C-c x t   toggle-window
C-c x R   test-report
```

```
M-x emacs-ide-repl-status
```

---

### `tools-test.el` + `tools-test-runner-registry.el`
Auto-detects test framework from major-mode and project markers. History with pass/fail/duration. Per-lang runner registry (`:file-fn` `:project-fn` `:point-fn` `:watch-fn`).

Supported: pytest · unittest · cargo test · go test · jest · vitest · npm test · rspec · mvn test · gradle test · mix test · cabal test · stack test · bats · ERT · ctest · make test

```
C-c C-t   smart-dispatch (file→project→auto)    C-c C-T   force project suite
C-c X f   run-file    C-c X p   run-project    C-c X .   run-at-point
C-c X w   watch       C-c X s   runner-status  C-c X l   run-last
```

```
M-x emacs-ide-test-report
M-x emacs-ide-test-runner-status
```

---

### `debug-core.el`
dap-mode (sessions, locals, controls, tooltip, REPL). Gated on `debug.enable`. Helper commands always available regardless of gate.

```
M-x emacs-ide-debug-toggle-breakpoint
M-x emacs-ide-debug-delete-all-breakpoints
M-x emacs-ide-debug-set-conditional-breakpoint
M-x emacs-ide-debug-repl
```

---

### `tools-org.el`
Full org-mode: agenda (week view, custom commands), capture (task/note/meeting/bug templates), babel (elisp/python/shell/js/C), export (md/html). Paths from config per environment. `org-confirm-babel-evaluate` auto-approves files under home. Diary entries on dashboard.

```
C-c a   org-agenda    C-c c   org-capture    C-c l   org-store-link
```

---

### `tools-notes.el`
org-roam with 5 capture templates (default/project/bug/reference/literature). Daily notes. consult-org-roam search/backlinks/forward-links. org-roam-ui graph. DB autosync on 3s idle. Directory from `config.yml general.notes-directory`.

```
C-c n f   find-node      C-c n i   insert-link    C-c n b   buffer-toggle
C-c n d   today          C-c n D   date           C-c n g   graph
C-c n s   search         C-c n B   backlinks      C-c n F   forward-links
C-c n p   project-note   C-c n c   capture        C-c n /   full-text-search
```

---

### `tools-spelling.el`
flyspell (aspell → hunspell → ispell). flyspell-prog-mode on prog-mode. Custom word filter (skips symbols, URLs, long tokens). flyspell-correct with avy-menu.

```
C-c S s   ispell-word    C-c S b   spell-buffer    C-c S n   next-error
C-c S t   toggle         C-c S c   correct-wrapper
```

---

### `tools-rest.el`
verb (org-mode HTTP client). restclient + restclient-jq (`.http` files).

```
C-c V s   rest-scratch    C-c V i   insert-request-template
```

---

### `tools-hydra.el`
10 hydra menus, all gated on `(with-eval-after-load 'hydra)`.

```
C-c h w   window    C-c h b   buffer    C-c h g   git       C-c h l   lsp
C-c h p   project   C-c h t   test      C-c h d   debug     C-c h u   toggles
C-c h r   repl      C-c h s   search    C-c h h   list-all
```

**window:** split-v/h · undo · delete · balance · resize · windmove · ace · swap · maximize · tab-bar  
**buffer:** consult-buffer · ibuffer · recent · kill · revert · scratch · new · save-all · imenu  
**git:** status · log · dispatch · diff · blame · timemachine · hunk-next/prev/revert · stash · forge · push · pull  
**lsp:** definition · references · implementation · type-def · outline · symbols · rename · action · format · hover · ui-toggle · sig-help · diagnostics · lens · workspace  
**project:** find-file · find-dir · ripgrep · grep · occur · switch-project · switch-buffer · kill-bufs · compile · recompile · run · treemacs · neotree · ibuffer  
**test:** file · project · at-point · last · watch · report · runner-status  
**debug:** step-in/next/out · continue · restart · quit · breakpoint-toggle/condition/log/delete-all · locals · eval · expressions · up/down-frame · repl · launch  
**toggles:** theme · line-nums · relative-nums · indent-guides · whitespace · visual-line · fill-column · flycheck · flyspell · dimmer · beacon · pulsar · treemacs · neotree · presentation · olivetti  
**repl:** launch · toggle · send-region/buffer/defun/line · status  
**search:** line · multi · ripgrep · grep · git-grep · find · imenu · imenu-multi · outline · symbols · mark · global-mark · keep-lines · focus-lines

---

### `tools-project-detect.el`
Reads project root markers → detects language tier → pre-warms lang module on 0.5s idle timer. Fires on `projectile-after-switch-project-hook` and `find-file-hook`. Extension fallback for non-project files. Cache cleared on config reload.

**Markers (sample):** `pyproject.toml/setup.py/Pipfile` → python · `Cargo.toml` → rust · `go.mod` → go · `package.json/tsconfig.json` → js/ts · `CMakeLists.txt/Makefile` → c · `pom.xml/build.gradle` → java · `flake.nix` → nix · `build.zig` → zig · …22 more

```
C-c D d   detect-show-status
M-x emacs-ide-detect-current-project
M-x emacs-ide-detect-reset-cache
```

---

### `keybindings.el` — Global Keymap Reference

Loads last. All global bindings canonical here.

**Upgraded built-ins:**
```
C-x C-b   ibuffer                 C-x b     consult-buffer
C-x C-r   consult-recent-file     M-y       consult-yank-pop
C-x g     magit-status            C-x M-g   magit-dispatch
C-x v t   git-timemachine
M-#       dictionary-lookup-definition      ← calibrated addition
M-/       hippie-expand
M-o       ace-window
```

**C-h help:**
```
C-h f     helpful-callable    C-h v     helpful-variable    C-h k     helpful-key
C-h F     helpful-function    C-h C     helpful-command     C-h d     helpful-at-point
```

**C-c prefixes (full map):**
```
C-c a/c/l   org-agenda/capture/store-link
C-c B/b     compile/recompile
C-c H       show-keybindings-help
C-c L       lsp-status
C-c P       presentation-mode
C-c R       config-reload
C-c ?       which-key-show-top-level
F12         toggle-theme

C-c h w/b/g/l/p/t/d/u/r/s/h   hydra menus (see tools-hydra.el)

C-c x r/s/b/d/l/t/R            repl hub
C-c X f/p/./w/s/l               test runner (uppercase X)
C-c C-t / C-c C-T               test smart-dispatch / force-all

C-c r r/v/b/d / r C-r           recovery

C-c V s/i                       rest client

C-c t / C-c T                   vterm-here / vterm-other-window
C-c e                           eshell-here

C-c n n/f/i/b/d/D/g/p/c/s/B/F/ notes + org-roam

C-c W s/n/k/r/b/i/l/w/S/R      workspaces
M-1..9                          workspace switch by index

C-c D d/s/r/q                   project-detect + profiler
C-c p *                         projectile (see tools-project.el)

C-M-y                           dumb-jump-hydra   ← calibrated addition
```

---

## Lang Modules (lazy, zero boot cost)

| Module | Languages | LSP | Formatter | Test | REPL |
|---|---|---|---|---|---|
| `lang-python` | Python | pyright | black+isort | pytest | ipython |
| `lang-rust` | Rust | rust-analyzer | rustfmt | cargo test | evcxr |
| `lang-web` | JS/TS | tsserver | prettier | jest/vitest | node |
| `lang-go` | Go | gopls | gofmt/goimports | go test | gore |
| `lang-c` | C/C++/CUDA/CMake | clangd | clang-format | ctest | — |
| `lang-jvm` | Java/Kotlin/Scala/Groovy | jdtls/kotlin-ls/metals | gjf/ktlint/scalafmt | mvn/gradle/sbt | — |
| `lang-lua` | Lua | lua-language-server | stylua | busted | lua |
| `lang-shell` | Bash/Zsh/Fish | bash-ls | shfmt | bats | bash |
| `lang-sql` | SQL | sqls | pg_format | — | sql |
| `lang-data` | R/Julia | r-ls/julia-ls | styler | Rscript/Pkg.test | R/julia |
| `lang-functional` | Haskell/Clojure/Elixir/OCaml/Erlang | hls/clojure-lsp/elixir-ls/ocamllsp | ormolu/cljfmt/mix-fmt/ocamlformat | cabal/clj/mix | ghci/cider/erl |
| `lang-systems` | Zig/Nix/D/V | zls/nil | zigfmt/nixpkgs-fmt | zig test | nix repl |
| `lang-prose` | MD/YAML/TOML/JSON/Docker/Terraform | yaml-ls/vscode-json-ls/docker-ls/terraform-ls | prettier/taplo/terraform-fmt | — | — |

Each lang module: tree-sitter grammar via treesit-auto · LSP hook (classic + ts-mode) · formatter via apheleia · test runner registration · DAP template · REPL hub registration · `C-c C-c` compile/run binding.

---

## `config.yml` — Key Toggles

```yaml
lsp:
  enable: true          # false → entire tools-lsp.el skipped
  inlay-hints: true     # propagated to rust/lua/typescript individually

git:
  enable: true          # false → magit + diff-hl + forge do not load

debug:
  enable: true          # false → dap-mode does not load; helper cmds still available

editing:
  meow: false           # true → Meow modal editing

theme:
  auto-switch: false    # true → dark/light by dark-hour/light-hour

languages:              # false → lang module never loads
  python: true
  haskell: false        # opt-in (off by default)
  # … 50 total
```

---

## Diagnostics Quick Reference

```
M-x emacs-ide-spot-check               commands + keybindings + features integrity
M-x emacs-ide-run-tests                full ERT suite
M-x emacs-ide-diagnose                 module load + config validation
M-x emacs-ide-diagnose-lsp             LSP server PATH check
M-x emacs-ide-diagnose-languages       per-lang enable/load/LSP/formatter matrix
M-x emacs-ide-health-status            system tools + LSP + config health
M-x emacs-ide-health-auto-fix          attempt auto remediation
M-x emacs-ide-lsp-check-servers        which servers are on PATH
M-x emacs-ide-lsp-status               current buffer connection  (C-c L)
M-x emacs-ide-config-show              active variable values
M-x emacs-ide-startup-report           phase timings
M-x emacs-ide-early-init-report        early-init phase benchmark
M-x emacs-ide-package-report           top-20 slowest package loads
M-x emacs-ide-detect-show-status       project detection + pre-warmed langs
M-x emacs-ide-repl-status              live REPL hub registry
M-x emacs-ide-test-runner-status       registered per-lang test runners
M-x emacs-ide-workspace-status         perspective workspaces + buffer counts
M-x emacs-ide-security-check           TLS + auth-sources + GPG
M-x emacs-ide-recovery-report          crash count + log + disabled pkgs
M-x emacs-ide-telemetry-report         session command usage
M-x emacs-ide-check-formatters         formatter PATH status
```

---

## Recovery

```bash
EMACS_SAFE_MODE=1 emacs          # or: emacs --emacs-ide-safe
```
→ `M-x emacs-ide-recovery-view-log` → `M-x emacs-ide-recovery-disable-package` → restart → `M-x emacs-ide-recovery-reset-crash-count`

---

## Version

| | |
|---|---|
| **v3.3.1** | `exec-path-from-shell` (13 env vars) · `global-so-long-mode` · bidi perf defaults · `bookmark-save-flag 1` · `switch-to-buffer` display rules · `dumb-jump` xref fallback + hydra · `ibuffer-project` · `editorconfig` · `repeat-mode` · `treesit-auto` replaces manual grammar timers · `display-buffer-alist` for Help/Completions/Dictionary · `Man-notify-method aggressive` · `ediff-setup-windows-plain` · `M-#` dictionary · scroll quality settings |
| v3.2.2 | 15 bug fixes: modeline keymap · dashboard hook · ghost functions · crash counter · telemetry logging · inlay-hints/git.enable/debug.enable config flags · spot-check integrity |
| v3.2.1 | recovery timer · lsp-check-servers · diagnose-languages · repl-send-line · config reload hook |
| v3.0.0 | 50-lang lazy loading · ef-themes · nerd-icons · hydra menus · workspaces · REPL hub · Meow |
