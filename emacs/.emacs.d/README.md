# Enterprise Emacs IDE

**Version 3.0.0** — Production-grade Emacs configuration with lazy-loaded 50-language support,
LSP, DAP debugging, Hydra menus, unified REPL hub, perspective workspaces, ef-themes,
async formatting, Git integration, and full recovery system.

---

## What's New in v3

- **50-language lazy loading** — `modules/langs/` replaces monolithic `lang-core.el`. Zero boot cost — each lang module loads only when you open a file of that type, or when a project marker (`Cargo.toml`, `go.mod`, `package.json`, etc.) is detected.
- **ef-themes** replaces modus-themes — sharper, more opinionated office aesthetic. Toggle dark/light with `F12`.
- **nerd-icons** replaces all-the-icons — single font, faster load, richer symbol coverage.
- **Hydra menus** — 10 discoverable chord-free command menus (`C-c h w/b/g/l/p/t/d/u/r/s`). Emacs-ideology replacement for Vim leader bindings.
- **perspective.el workspaces** — named workspaces with isolated buffer lists. Auto-creates a workspace per project. Switch with `M-1..9`.
- **Unified REPL hub** — `C-c x r` dispatches to the right REPL for any language (Python, Node, Go, Rust, Clojure, Julia, R, Lua, Nix).
- **tools-project-detect** — reads project root markers + `config.yml` language flags, pre-warms only the lang tiers you use.
- **Optional Meow modal editing** — Emacs-native modal mode (not Vim/Evil). Off by default; enable with `editing.meow: true` in `config.yml`.

---

## Requirements

| Requirement | Minimum | Notes |
|---|---|---|
| Emacs | 29.1+ | Native compilation strongly recommended |
| Git | any | Required — health check fails without it |
| ripgrep (`rg`) | any | Recommended for project search |
| `fd` | any | Recommended for file finding |

**Fonts** (install once):

```bash
# Nerd-icons font (required for icons in dashboard, dired, corfu)
M-x nerd-icons-install-fonts
```

**LSP servers** (install only for languages you use):

```bash
# Tier 1 — daily
pip install pyright                              # Python (preferred over pylsp)
rustup component add rust-analyzer              # Rust
go install golang.org/x/tools/gopls@latest     # Go
npm install -g typescript-language-server       # JS / TS
# C/C++: clangd via system package manager (apt/brew/pacman)

# Tier 2 — regular
pip install python-lsp-server                   # Python fallback (pylsp)
npm install -g bash-language-server             # Shell
npm install -g yaml-language-server             # YAML
# Java: eclipse.jdt.ls (lsp-java installs automatically)
# Kotlin: kotlin-language-server (brew/sdkman)
# Lua: lua-language-server (brew/cargo)
# SQL: sqls (go install github.com/sqls-server/sqls@latest)

# Tier 3 — occasional
# Haskell: haskell-language-server (ghcup install hls)
# Clojure: clojure-lsp (brew install clojure-lsp/brew/clojure-lsp)
# Elixir: elixir-ls (see https://github.com/elixir-lsp/elixir-ls)
# Nix: nil (nix-env -i nil)
# Zig: zls (zig build or from zls releases)
```

**Formatters** (install only what you use):

```bash
# Tier 1
pip install black isort                         # Python
npm install -g prettier                         # JS/TS/HTML/CSS/JSON/YAML/MD
# rustfmt ships with rustup
go install golang.org/x/tools/cmd/goimports@latest  # Go
# clang-format: system package manager

# Tier 2
pip install pg_format                           # SQL (or apt install pgformatter)
go install mvdan.cc/sh/v3/cmd/shfmt@latest     # Shell
cargo install stylua                            # Lua
brew install ktlint                             # Kotlin
brew install google-java-format                 # Java

# Tier 3
cabal install ormolu                            # Haskell
nix-env -i nixpkgs-fmt                         # Nix
# Zig: zigfmt ships with zig
```

**Debug adapters:**

```bash
pip install debugpy                             # Python
go install github.com/go-delve/delve/cmd/dlv@latest  # Go
# LLDB/GDB: system package manager (C/C++/Rust)
# Node.js: dap-node installs automatically via npm
```

---

## Install

```bash
git clone <your-repo> ~/.emacs.d
emacs --init-directory ~/.emacs.d
```

On first launch straight.el bootstraps itself and installs all packages.
This takes 2–5 minutes. Subsequent startups are typically **< 2 seconds** in practice (lang modules are lazy). The `startup-time-target: 3.0` in `config.yml` is the warning threshold — a notification fires if startup exceeds it, not a goal in itself.

> **After upgrading Emacs** (e.g. 29 → 30), always purge stale bytecode
> before restarting — old `.elc`/`.eln` files cause all `M-x emacs-ide-*`
> commands to silently disappear:
> ```bash
> find ~/.emacs.d/core ~/.emacs.d/modules -name "*.elc" -delete
> find ~/.emacs.d/core ~/.emacs.d/modules -name "*.eln" -delete
> rm -rf ~/.emacs.d/var/eln-cache/
> ```
> Or from inside Emacs: `M-x emacs-ide-purge-bytecode-cache`
> This now also clears `modules/langs/` automatically.

To boot in safe mode (minimal config, no packages):

```bash
emacs --safe
# or: EMACS_SAFE_MODE=1 emacs
```

---

## Directory Structure

```
~/.emacs.d/                                     50 .el files + 1 .yml = 51 total
│
├── early-init.el              GC · frame · native-comp · JIT · TLS (before init)
├── init.el                    Bootstrap · module loader · startup tracking (v3.0.0)
├── config.yml                 All user config — edit this, not the .el files
│
├── core/                      8 files — infrastructure, always loaded at boot
│   ├── emacs-ide-config.el    YAML loader; (require)'d first, before core loop
│   ├── emacs-ide-health.el    Health check system                    ┐
│   ├── emacs-ide-package.el   Package load-time tracking             │
│   ├── emacs-ide-profiler.el  CPU / memory profiler                  ├ core module loop (6)
│   ├── emacs-ide-recovery.el  Crash tracking · safe mode · backup    │
│   ├── emacs-ide-security.el  TLS · auth-source · network security   │
│   ├── emacs-ide-telemetry.el Local usage analytics                  ┘
│   └── emacs-ide-test.el      ERT integration tests (M-x emacs-ide-run-tests)
│                              on load-path, not auto-loaded at startup
│
├── modules/                   26 files — feature modules, loaded eagerly in order
│   │
│   ├── ui-core.el             ef-themes · nerd-icons · visual enhancements
│   ├── ui-theme.el            ef-themes toggle (F12) · modus→ef auto-migration
│   ├── ui-modeline.el         doom-modeline + health segment
│   ├── ui-dashboard.el        Startup dashboard · workspace + health + actions
│   ├── ui-workspace.el        perspective.el workspaces · tab-bar · M-1..9  [NEW]
│   │
│   ├── completion-core.el     Vertico · Corfu · Consult · Embark · Orderless
│   ├── completion-snippets.el YASnippet + community snippets
│   │
│   ├── editing-core.el        Smartparens · undo-tree · mc · avy · Meow (opt)
│   │
│   ├── core-dev.el            Shared lang API · emacs-ide-dev-*  [NEW]
│   │
│   ├── tools-lsp.el           lsp-mode · lsp-ui · flycheck · dumb-jump
│   ├── tools-project.el       Projectile · Treemacs · Neotree
│   ├── tools-project-detect.el Project root → lang tier pre-warming  [NEW]
│   ├── tools-git.el           Magit · diff-hl · forge · git-timemachine
│   ├── tools-terminal.el      VTerm (C-c t) · Eshell · Docker
│   ├── tools-format.el        Apheleia base config
│   ├── apheleia-langs-patch.el 50-lang complete formatter map  [NEW]
│   ├── tools-repl.el          Unified REPL hub · C-c x r/s/b/d/t  [NEW]
│   ├── tools-org.el           Org-mode · agenda · capture · babel · export
│   ├── tools-spelling.el      Flyspell prose + flyspell-prog-mode code
│   ├── tools-notes.el         Denote / org-roam
│   ├── tools-rest.el          Restclient · Verb
│   ├── tools-test-runner-registry.el  Per-lang test registry · C-c X f/p/./w/s  [NEW]
│   ├── tools-test.el          Auto-detect fallback + history · C-c C-t
│   ├── debug-core.el          DAP adapters · F5-F9 keys
│   ├── tools-hydra.el         10 Hydra menus · C-c h w/b/g/l/p/t/d/u/r/s  [NEW]
│   └── keybindings.el         All global keys — always loads last
│   │
│   └── langs/                 13 files — lazy, zero boot cost  [NEW]
│       ├── lang-python.el     pyright · ipython · pytest · poetry · debugpy
│       ├── lang-rust.el       rust-analyzer · evcxr · cargo · LLDB
│       ├── lang-web.el        tsserver · Node REPL · jest · prettier
│       ├── lang-go.el         gopls · gore · go test · Delve
│       ├── lang-c.el          clangd · clang-format · CMake · LLDB
│       ├── lang-jvm.el        jdtls · kotlin-ls · Metals · Maven/Gradle
│       ├── lang-lua.el        lua-language-server · stylua · busted
│       ├── lang-shell.el      bash-ls · shellcheck · shfmt · bats
│       ├── lang-sql.el        sqls · pgformatter · ejc-sql
│       ├── lang-data.el       ESS/R · julia-repl · Jupyter
│       ├── lang-functional.el HLS · CIDER · lexical · Metals
│       ├── lang-systems.el    ZLS · nil · nix-mode
│       └── lang-prose.el      md · yaml · toml · json · docker · terraform
│
├── var/                       Auto-created on first launch — do not edit
│   ├── cache/                 Package and LSP caches
│   ├── backups/               Versioned file backups
│   ├── eln-cache/             Native-compiled .eln files
│   ├── persp-state            Workspace layout save file
│   ├── places                 Save-place history
│   ├── custom.el              Emacs custom-set-variables
│   └── telemetry.el           Local usage stats
│
└── snippets/                  Auto-created — add custom YASnippet snippets here
```

**File counts:**

| Location | Files | How loaded |
|---|---|---|
| root | 3 | `early-init.el` at startup · `init.el` bootstraps · `config.yml` via require |
| `core/` | 8 | `emacs-ide-config.el` first via require · 6 via core loop · `emacs-ide-test.el` on-demand |
| `modules/` | 26 | Eagerly in order at startup via `emacs-ide-feature-modules` list |
| `modules/langs/` | 13 | Lazily on file open or project switch via `tools-project-detect.el` |
| **Total** | **50** | |

`var/` and `snippets/` are created automatically — never commit them.

---

## Configuration

All settings live in **`config.yml`** — edit this, not the `.el` modules.

```bash
M-x emacs-ide-config-edit     # Open config in Emacs
M-x emacs-ide-config-reload   # Reload after editing (no restart needed)
M-x emacs-ide-config-show     # View currently active values
```

Key sections:

```yaml
general:
  theme: ef-dark               # ef-dark · ef-light · ef-cherie · ef-winter · …
  font: JetBrains Mono
  font-size: 11

editing:
  meow: false                  # Set true for Emacs-native modal editing

workspace:
  enable: true                 # perspective.el named workspaces

languages:
  python: true                 # Loads lang-python.el on first .py file
  rust: true
  go: true
  javascript: true
  haskell: false               # Set true to enable (off by default)

  python:
    lsp-server: pyright        # pyright or pylsp
    formatter: black

  rust:
    lsp-server: rust-analyzer
    cargo-watch: clippy
```

> **Languages are lazy by default.** Setting a language to `false` prevents
> its module from ever loading. Absent keys default to `true`.
> `tools-project-detect.el` reads this section before pre-warming any tier.

---

## How Lazy Loading Works

```
Emacs starts
  → early-init.el   (GC, frame, native-comp)
  → init.el         (core + 23 feature modules — no lang modules)
  → ready in ~1s

You open ~/work/api/main.py
  → find-file-hook fires
  → tools-project-detect scans root: finds pyproject.toml
  → checks config.yml: python: true
  → loads lang-python.el (0.5s idle, non-blocking)
  → LSP starts, pyright connects, pytest wired, REPL ready

You run M-x projectile-switch-project → ~/code/myapp (Cargo.toml)
  → tools-project-detect fires on project switch
  → loads lang-rust.el pre-emptively
  → rust-analyzer ready before you open the first .rs file
```

Adding a new language: create `modules/langs/lang-xyz.el` following the
`lang-python.el` template, add `xyz: true` to `config.yml`. Nothing else.

---

## Health & Diagnostics

Run these when something feels wrong — start from the top.

### Quick status commands (new in v3)

```
M-x emacs-ide-detect-show-status    — project detect state + pre-warmed langs
M-x emacs-ide-repl-status           — which REPLs are live
M-x emacs-ide-test-runner-status    — which test runners are registered
M-x emacs-ide-workspace-status      — perspective workspaces + buffer counts
```

### Full system check

```
M-x emacs-ide-health-check-all
```

Runs all registered checks — system tools, LSP servers, formatters, packages,
performance, security — and offers to auto-fix what it can.

### Startup integrity

```
M-x emacs-ide-startup-report
```

Shows every load phase and its elapsed time. Lang modules do not appear here
(they are lazy). Run `M-x emacs-ide-detect-show-status` for lang state.

### Config was loaded (not defaults)

```
M-x emacs-ide-config-show
```

Verify `gc-threshold` shows `16777216`, not `0`. If it shows defaults,
the YAML parser encountered a problem — check `M-x emacs-ide-recovery-view-log`.

### Package load times

```
M-x emacs-ide-package-report
```

Top 20 slowest packages. Anything over 0.5s is flagged.

### LSP health

```
M-x emacs-ide-lsp-status          — connection for current buffer
M-x emacs-ide-lsp-check-servers   — which servers are installed
```

Open a source file first, then run `lsp-status` — should show `✓ Connected`.

### Formatter status

```
M-x emacs-ide-check-formatters
```

Shows which formatters are on PATH and whether format-on-save is active.

### Test runner detection

```
M-x emacs-ide-test-run             — smart dispatch (registry → auto-detect)
M-x emacs-ide-test-runner-status   — all registered runners
M-x emacs-ide-test-report          — history of test runs this session
```

### Security audit

```
M-x emacs-ide-security-check
```

### Recovery status

```
M-x emacs-ide-recovery-report
```

### Telemetry

```
M-x emacs-ide-telemetry-report
```

All data is local — nothing is ever sent externally.

### Full integration test suite

```
M-x emacs-ide-run-tests
```

All green = fully healthy.

### Performance profiling

```
M-x emacs-ide-early-init-report    — early-init phase breakdown (target: <0.5s)
M-x emacs-ide-profile-startup      — deep startup profiler (requires esup)
M-x emacs-ide-profile-start        — CPU+memory profiler (built-in)
M-x emacs-ide-profile-report       — stop and show report
```

---

## Keybindings — Non-Obvious Reference

Vanilla Emacs defaults (`C-x C-s`, `C-x C-f`, `M-.`, `C-/`, etc.) work as always.
Run `C-h k` to look up any key. This lists only what is added or upgraded.

### Hydra menus (new in v3) — C-c h prefix

| Key | Hydra | What's inside |
|---|---|---|
| `C-c h w` | Window | split · resize · ace · winner |
| `C-c h b` | Buffer | switch · kill · scratch · ibuffer |
| `C-c h g` | Git | magit · diff · blame · stash · forge |
| `C-c h l` | LSP | rename · actions · refs · format · hover |
| `C-c h p` | Project | find · ripgrep · compile · treemacs |
| `C-c h t` | Test | file · project · at-point · watch · report |
| `C-c h d` | Debug | step · continue · breakpoints · inspect |
| `C-c h u` | Toggles | theme · line-nos · flycheck · dimmer · zen |
| `C-c h r` | REPL | launch · send region/buffer/defun · toggle |
| `C-c h s` | Search | ripgrep · occur · imenu · outline · symbol |
| `C-c h h` | Help | list all hydra entry points |

### REPL (new in v3) — C-c x prefix (lowercase x)

| Key | Command |
|---|---|
| `C-c x r` | Launch / switch to REPL for current language |
| `C-c x s` | Send region to REPL |
| `C-c x b` | Send buffer to REPL |
| `C-c x d` | Send defun at point to REPL |
| `C-c x t` | Toggle REPL window (bottom side window) |

### Test dispatch (updated in v3) — C-c X prefix (uppercase X)

| Key | Command |
|---|---|
| `C-c X f` | Run file tests |
| `C-c X p` | Run project tests |
| `C-c X .` | Run test at point |
| `C-c X w` | Watch mode |
| `C-c X s` | Show registered runners |
| `C-c X l` | Repeat last test run |
| `C-c x R` | Show test report (this session) |
| `C-c C-t` | Smart dispatch (same as before) |
| `C-c C-T` | Force full suite |

Supported frameworks (auto-detected): pytest · unittest · cargo test · go test ·
jest · vitest · npm test · rspec · minitest · mvn test · gradle test · mix test ·
cabal test · stack test · bats · ERT · ctest · make test.

### Workspaces (new in v3) — C-c W prefix

| Key | Command |
|---|---|
| `C-c W s` | Switch workspace |
| `C-c W n` | New workspace |
| `C-c W k` | Kill workspace |
| `C-c W r` | Rename workspace |
| `C-c W b` | Switch buffer within workspace |
| `C-c W S` | Save window layout (burly bookmark) |
| `C-c W R` | Restore window layout |
| `M-1..9` | Switch to workspace by index |

### Navigation

| Key | Command | Notes |
|---|---|---|
| `C-:` | `avy-goto-char` | Jump to any visible char |
| `C-'` | `avy-goto-char-2` | Jump by two chars |
| `M-g f` | `avy-goto-line` | |
| `M-g w` | `avy-goto-word-1` | |
| `M-o` | `ace-window` | Fast window jump/swap |
| `C-c left` | `winner-undo` | Restore window layout |

### Search — M-s prefix

| Key | Command |
|---|---|
| `M-s l` | `consult-line` |
| `M-s L` | `consult-line-multi` |
| `M-s r` | `consult-ripgrep` |
| `M-s g` | `consult-grep` |
| `M-s G` | `consult-git-grep` |
| `M-s f` | `consult-find` |

### Go-to — M-g prefix

| Key | Command |
|---|---|
| `M-g g` | `consult-goto-line` |
| `M-g i` | `consult-imenu` |
| `M-g I` | `consult-imenu-multi` |
| `M-g o` | `consult-outline` |
| `M-g s` | `consult-lsp-symbols` (workspace) |
| `M-g e` | `consult-lsp-diagnostics` |
| `M-g j` | `dumb-jump-go` |

### Editing

| Key | Command |
|---|---|
| `C->` | `mc/mark-next-like-this` |
| `C-<` | `mc/mark-previous-like-this` |
| `C-=` | `er/expand-region` |
| `C--` | `er/contract-region` |
| `M-↑ / M-↓` | `move-text-up / down` |
| `C-/` | `undo-tree-undo` |
| `C-?` | `undo-tree-redo` |
| `C-x u` | `undo-tree-visualize` |
| `C-.` | `embark-act` — contextual action hub |
| `C-;` | `embark-dwim` |
| `M-/` | `hippie-expand` |

### LSP (active in LSP buffers only)

| Key | Command |
|---|---|
| `C-c l r` | `lsp-rename` |
| `C-c l f` | `lsp-format-buffer` |
| `C-c l a` | `lsp-execute-code-action` |
| `C-c l R` | `lsp-find-references` |
| `C-c l i` | `lsp-find-implementation` |
| `C-c l t` | `lsp-find-type-definition` |
| `C-c l o` | `lsp-organize-imports` |
| `C-c l u` | `lsp-ui-doc-toggle` |
| `M-.` | `lsp-ui-peek-find-definitions` |
| `M-?` | `lsp-ui-peek-find-references` |

### Debugging (DAP)

| Key | Command |
|---|---|
| `F5` | Start debug session |
| `F6` | Restart |
| `F7` | Step into |
| `S-F7` | Step over |
| `M-F7` | Step out |
| `C-F7` | Continue |
| `F8` | Toggle breakpoint |
| `C-F8` | Conditional breakpoint |
| `S-F8` | Log message breakpoint |
| `C-S-F8` | Delete all breakpoints |
| `C-c h d` | Debug hydra (step/break/inspect/repl) |

### Git

| Key | Command |
|---|---|
| `C-x g` | `magit-status` |
| `C-x M-g` | `magit-dispatch` |
| `C-x v t` | `git-timemachine` |

### Project

| Key | Command |
|---|---|
| `C-c p f` | `projectile-find-file` |
| `C-c p p` | `projectile-switch-project` |
| `C-c p s r` | `projectile-ripgrep` |
| `C-c p c` | `projectile-compile-project` |
| `C-c p k` | `projectile-kill-buffers` |
| `F9` | Treemacs toggle |
| `C-c n` | Neotree toggle |
| `C-c D` | Project detect status |

### Build

| Key | Command |
|---|---|
| `C-c B` | `compile` |
| `C-c b` | `recompile` |

### Terminal

| Key | Command |
|---|---|
| `C-c t` | `vterm` |
| `C-c T` | `vterm-other-window` |
| `C-c M-t` | `multi-vterm` |

### Org

| Key | Command |
|---|---|
| `C-c a` | `org-agenda` |
| `C-c c` | `org-capture` |
| `C-c l` | `org-store-link` |

### UI & Utility

| Key | Command |
|---|---|
| `F12` | Toggle dark/light ef-theme |
| `C-c P` | Presentation mode |
| `C-c ?` | `which-key-show-top-level` |
| `C-c H` | Keybinding cheat sheet |
| `C-c R` | Reload config (`emacs-ide-config-reload`) |
| `C-c V` | REST prefix (`C-c V s` scratch · `C-c V i` insert) |
| `C-c L` | LSP status |
| `C-c w t/f/r` | `transpose/flip/rotate-frame` |

### Recovery

| Key | Command |
|---|---|
| `C-c r r` | Recovery report |
| `C-c r v` | View recovery log |
| `C-c r c` | Clear recovery log |
| `C-c r b` | Backup current config |
| `C-c r R` | Restore from backup |
| `C-c r d` | Disable problematic package |
| `C-c r C-r` | Reset crash counter |

---

## Safe Mode & Recovery

If Emacs crashes 3 times in succession it enters **safe mode** automatically.
Force it manually:

```bash
emacs --safe
EMACS_SAFE_MODE=1 emacs
```

To recover:

1. `M-x emacs-ide-recovery-view-log` — find the error
2. Fix config or disable: `M-x emacs-ide-recovery-disable-package`
3. `M-x emacs-ide-recovery-reset-crash-count`
4. Restart Emacs normally

Config backup/restore:

```
M-x emacs-ide-recovery-backup-config    — timestamped backup to var/backups/
M-x emacs-ide-recovery-restore-config   — restore from backup
```

---

## Module Load Order

```
early-init:  GC · frame · native-comp · JIT · TLS

core:        emacs-ide-health → emacs-ide-package → emacs-ide-profiler
             → emacs-ide-security → emacs-ide-telemetry → emacs-ide-recovery

features:    ui-core → ui-theme → ui-modeline → ui-dashboard → ui-workspace
             → completion-core → completion-snippets
             → editing-core
             → core-dev
             → tools-lsp → tools-project → tools-git → tools-terminal
             → tools-format → apheleia-langs-patch
             → tools-org → tools-spelling → tools-notes → tools-rest
             → tools-test-runner-registry → tools-test → debug-core
             → tools-repl → tools-project-detect → tools-hydra
             → keybindings   ← always last

langs/:      (not in this list — loaded lazily on file open or project switch)
             lang-python · lang-rust · lang-web · lang-go · lang-c
             lang-jvm · lang-lua · lang-shell · lang-sql · lang-data
             lang-functional · lang-systems · lang-prose
```

`keybindings.el` loads last so its global bindings win over any module's
`:bind` declarations. `core-dev.el` loads before `tools-lsp.el` so lang
modules can call `emacs-ide-dev-attach-lsp` safely.

---

## Synergy Checklist

Run after any significant change:

```
M-x emacs-ide-run-tests              ← all green?
M-x emacs-ide-health-check-all       ← no errors?
M-x emacs-ide-startup-report         ← all phases present?
M-x emacs-ide-config-show            ← gc-threshold = 16777216?
M-x emacs-ide-security-check         ← TLS verified?
M-x emacs-ide-detect-show-status     ← project detect working?
```

Open a Python/Rust/Go file and confirm:

- Tree-sitter syntax highlighting active (richer colors)
- LSP diagnostics appearing in the margin
- `C-c X f` runs the right test framework
- `C-c x r` opens the correct REPL
- `F5` launches the debugger, `F8` sets a breakpoint
- Saving triggers async formatting (no cursor jump)
- `C-c h l` opens the LSP hydra menu

---

## Version History

| Version | Notes |
|---|---|
| 3.0.2 | **Keybinding fixes** — `C-c R` void-function resolved (alias `emacs-ide-reload-config` → `emacs-ide-config-reload` added to `init.el`). REST prefix moved `C-c R` → `C-c V` (collision with reload). `split-string` omit-nulls added to `emacs-ide-setup-exec-path`. `emacs-ide--get-processor-count` double shell spawn eliminated (cached in `emacs-ide-processor-count`). Dead `treesit-enable`/`treesit-auto-install` keys removed from `config.yml languages:`. Formatter allowlist expanded: `mix-format`, `zigfmt`, `terraform-fmt`, `pg_format` (Elixir/Zig/Terraform/SQL apheleia formatting was silently skipped). README keybinding tables corrected: REPL `C-c X r` → `C-c x r`, test prefix `C-c t` → `C-c X`. |
| 3.0.1 | **Module patch** — `core-dev.el` string→symbol key fix (lang enable/disable was always true). Double LSP start on Python removed. `gofmt-before-save` apheleia guard. `tools-test-runner-registry.el` duplicate `emacs-ide-test-run` removed. Keybinding collisions resolved: `C-c x r` (repl), `C-c h r`/`C-c h i` (hydra vs REST). `ui-dashboard.el` premature defvar fixed. `lang-prose.el` duplicate `dockerfile-mode`/`restclient` removed. `allow-unsigned` now correctly interned as symbol. `tools-project-detect.el` `javascript`→`lang-web` module lookup fixed. |
| 3.0.0 | **50-lang lazy loading** — `lang-core.el` replaced by `modules/langs/` (13 modules, zero boot cost). **ef-themes** replaces modus-themes. **nerd-icons** replaces all-the-icons. **Hydra menus** (`tools-hydra.el`) — 10 discoverable menus. **perspective.el workspaces** (`ui-workspace.el`). **Unified REPL hub** (`tools-repl.el`). **tools-project-detect** pre-warms lang tiers from project markers. **tools-test-runner-registry** — per-lang test dispatch API. **apheleia-langs-patch** — complete 50-lang formatter map. Optional **Meow** modal editing. `init.el` v3.0.0: `langs/` added to load-path, 7 new feature modules, `lang-core` removed |
| 2.2.6 | **Emacs 30 fix** — `M-x emacs-ide-*` commands silently undefined after 29→30 upgrade. Root cause: stale `.elc`/`.eln` bytecode. Fixed with `nosuffix` load flag. Added `emacs-ide-purge-bytecode-cache` |
| 2.2.5 | **Startup fix** 58s → <3s: `straight-check-for-modifications` → `find-when-checking`; dashboard `agenda` item removed; `projectile-indexing-method` → `alien`; heavy packages deferred to `after-init-hook` |
| 2.2.4 | `straight--build-cache` fix; `electric-pair-mode`; backup policy reconciled |
| 2.2.3 | `tools-test.el` added; inline YAML comment parsing fixed |
| 2.2.2 | Config `when-let` guards; `lsp-idle-delay` fix; `C-c C-c` compile conflict resolved |
| 2.2.1 | `warning-minimum-level` restore timing; `horizontal-scroll-bar-mode` removed |
| 2.2.0 | Initial production release |
