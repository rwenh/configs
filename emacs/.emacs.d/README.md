# Enterprise Emacs IDE

**Version 2.2.3** — Production-grade Emacs configuration with LSP, DAP debugging,
language-aware test runner, async formatting, Git integration, and full recovery system.

---

## Requirements

| Requirement | Minimum | Notes |
|---|---|---|
| Emacs | 29.1+ | Native compilation recommended |
| Git | any | Required — health check will fail without it |
| ripgrep (`rg`) | any | Recommended for project search |
| `fd` | any | Recommended for file finding |

**LSP servers** (install only what you use):

```bash
pip install pyright              # Python
rustup component add rust-analyzer  # Rust
go install golang.org/x/tools/gopls@latest  # Go
npm install -g typescript-language-server   # JS/TS
# C/C++: install clangd via system package manager
```

**Formatters:**

```bash
pip install black                # Python
npm install -g prettier          # JS/TS/HTML/CSS/JSON/YAML/Markdown
# rustfmt ships with rustup
# gofmt ships with go
# clang-format: system package manager
go install mvdan.cc/sh/v3/cmd/shfmt@latest  # Shell
```

**Debug adapters:**

```bash
pip install debugpy              # Python
go install github.com/go-delve/delve/cmd/dlv@latest  # Go
# LLDB/GDB: system package manager (C/C++/Rust)
```

---

## Install

```bash
git clone <your-repo> ~/.emacs.d
emacs --init-directory ~/.emacs.d
```

On first launch straight.el bootstraps itself and installs all packages.
This takes 2–5 minutes. Subsequent startups target **< 2 seconds**.

To boot in safe mode (minimal config, no packages):

```bash
emacs --safe
# or set EMACS_SAFE_MODE=1 in your environment
```

---

## Directory Structure

```
~/.emacs.d/
  early-init.el          — GC, frame, native-comp setup (runs before init)
  init.el                — Bootstrap, module loader, startup tracking
  config.yml             — All user configuration (edit this, not the .el files)
  core/
    emacs-ide-config.el  — YAML config loader and accessor
    emacs-ide-health.el  — Health check system
    emacs-ide-recovery.el — Crash tracking, safe mode, config backup
    emacs-ide-package.el  — Package load-time tracking
    emacs-ide-profiler.el — CPU/memory profiler
    emacs-ide-security.el — TLS, auth-source, network security
    emacs-ide-telemetry.el — Local usage analytics
  modules/
    ui-core.el           — Theme, fonts, line numbers, visual enhancements
    ui-theme.el          — Theme toggle (F12)
    ui-modeline.el       — Doom-modeline + fallback
    ui-dashboard.el      — Startup dashboard
    completion-core.el   — Vertico + Corfu + Consult + Cape + Embark + Orderless
    completion-snippets.el — YASnippet + community snippets
    editing-core.el      — Smartparens, undo-tree, multiple-cursors, avy, expand-region
    tools-lsp.el         — LSP mode + lsp-ui + flycheck + dumb-jump
    tools-project.el     — Projectile + Treemacs + Neotree
    tools-git.el         — Magit + diff-hl + forge + git-timemachine
    tools-terminal.el    — VTerm + Eshell + Dired + Docker
    tools-format.el      — Apheleia (async) + format-all + editorconfig
    tools-org.el         — Org-mode, agenda, capture, babel, export
    tools-spelling.el    — Flyspell (prose) + flyspell-prog-mode (code)
    tools-test.el        — Language-aware test runner (NEW)
    lang-core.el         — 15+ language modes + tree-sitter grammars
    debug-core.el        — DAP debugging + GDB + edebug + realgud
    keybindings.el       — All global keybindings (always loads last)
  var/                   — Runtime data (cache, logs, backups) — do not edit
  snippets/              — Custom YASnippet snippets
```

---

## Configuration

All settings live in **`config.yml`** — edit this file, not the `.el` modules.

```bash
# Open config in Emacs
M-x emacs-ide-config-edit

# Reload after editing (no restart needed)
M-x emacs-ide-config-reload

# View currently active values
M-x emacs-ide-config-show
```

Key sections in `config.yml`:

```yaml
general:
  theme: modus-vivendi       # modus-vivendi (dark) or modus-operandi (light)
  font: JetBrains Mono
  font-size: 11

lsp:
  enable: true
  diagnostics-provider: flycheck

formatting:
  on-save: true
  python: black
  javascript: prettier

languages:
  python:
    lsp-server: pyright
  rust:
    lsp-server: rust-analyzer
```

> **Important:** Do not add inline comments on value lines (`key: value  # comment`).
> The YAML parser strips them but it's cleaner to keep values and comments separate.

---

## Health & Diagnostics

Run these when something feels wrong. Start from the top.

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

Shows every load phase and its elapsed time. A module that failed to load will
be absent or appear in the recovery log.

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

Top 20 slowest packages by load time. Anything over 0.5s is flagged.
Use this to find what's making startup slow.

### LSP health

```
M-x emacs-ide-lsp-status          # connection status for current buffer
M-x emacs-ide-lsp-check-servers   # which LSP servers are installed
```

Open a source file first, then run `lsp-status` — it should show `✓ Connected`.

### Formatter status

```
M-x emacs-ide-check-formatters
```

Shows which formatters are found on PATH and whether format-on-save is active.

### Test runner detection

Open any test file and run:

```
M-x emacs-ide-test-run       # should auto-detect and run
M-x emacs-ide-test-report    # history of all test runs this session
```

### Security audit

```
M-x emacs-ide-security-check
```

Checks TLS verification, package signatures, auth-source encryption, GPG, custom file.

### Recovery status

```
M-x emacs-ide-recovery-report
```

Shows crash count (should be 0), errors this session, and available recovery actions.

### Telemetry (usage stats)

```
M-x emacs-ide-telemetry-report
```

Most-used commands and session duration. Confirms post-command-hook is working.
All data is local — nothing is ever sent externally.

### Full integration test suite

```
M-x emacs-ide-run-tests
```

Runs all ERT assertions: Emacs version, directory structure, core modules loaded,
health/recovery systems present, completion active, editing modes on, theme loaded,
git available. **All green = fully healthy.**

### Early-init performance

```
M-x emacs-ide-early-init-report
```

Breakdown of early-init phases. Target: under 0.5s total.

### Startup profiler (deep)

```
M-x emacs-ide-profile-startup     # requires esup package
M-x emacs-ide-profile-start       # CPU+memory profiler
M-x emacs-ide-profile-report      # stop and show report
```

---

## Keybindings — Non-Obvious Reference

Vanilla Emacs defaults (`C-x C-s`, `C-x C-f`, `M-.`, `C-/`, etc.) work as always.
This lists only what is added or upgraded.

### Navigation

| Key | Command | Notes |
|---|---|---|
| `C-:` | `avy-goto-char` | Jump to any visible char |
| `C-'` | `avy-goto-char-2` | Jump by two chars |
| `M-g f` | `avy-goto-line` | Jump to line |
| `M-g w` | `avy-goto-word-1` | Jump to word |
| `M-o` | `ace-window` | Fast window jump/swap |
| `C-c left` | `winner-undo` | Restore window layout |
| `C-c right` | `winner-redo` | |

### Search (M-s prefix — Emacs native)

| Key | Command |
|---|---|
| `M-s l` | `consult-line` — search current buffer |
| `M-s L` | `consult-line-multi` — search all buffers |
| `M-s r` | `consult-ripgrep` — project-wide |
| `M-s g` | `consult-grep` |
| `M-s G` | `consult-git-grep` |
| `M-s f` | `consult-find` |

### Go-to (M-g prefix — Emacs native)

| Key | Command |
|---|---|
| `M-g g` | `consult-goto-line` |
| `M-g i` | `consult-imenu` — jump to symbol |
| `M-g I` | `consult-imenu-multi` |
| `M-g o` | `consult-outline` — jump to heading |
| `M-g j` | `dumb-jump-go` — fallback when no LSP |
| `M-g O` | `dumb-jump-go-other-window` |

### Editing

| Key | Command |
|---|---|
| `C->` | `mc/mark-next-like-this` — multiple cursors |
| `C-<` | `mc/mark-previous-like-this` |
| `C-=` | `er/expand-region` |
| `C--` | `er/contract-region` |
| `M-↑ / M-↓` | `move-text-up / down` |
| `C-/` | `undo-tree-undo` |
| `C-?` | `undo-tree-redo` |
| `C-x u` | `undo-tree-visualize` |
| `C-.` | `embark-act` — contextual actions |
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

### Testing

| Key | Command |
|---|---|
| `C-c C-t` | `emacs-ide-test-run` — auto-detect and run |
| `C-c C-T` | `emacs-ide-test-run-all` — force full suite |
| `C-c x p` | `emacs-ide-test-run-point` — test at cursor |
| `C-c x l` | `emacs-ide-test-run-last` — repeat last |
| `C-c x r` | `emacs-ide-test-report` — history |
| `C-c x h` | Test hydra menu |

Supported frameworks (auto-detected): pytest, unittest, cargo test, go test,
jest, vitest, npm test, rspec, minitest, mvn test, gradle test, mix test,
cabal test, stack test, bats, ERT, ctest, make test.

### Debugging (DAP)

| Key | Command |
|---|---|
| `F5` | Start debug session |
| `F6` | Restart |
| `F7` | Step into |
| `S-F7` | Step over |
| `M-F7` | Step out |
| `C-F7` | Continue |
| `F9` | Toggle breakpoint |
| `C-F9` | Conditional breakpoint |
| `S-F9` | Log message breakpoint |
| `C-S-F9` | Delete all breakpoints |
| `C-c d h` | Debug hydra (`n s o c b B L D u d l e U w R q`) |
| `C-c d ?` | Debug help |

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
| `F8` | Neotree toggle |

### Build & Compile

| Key | Command |
|---|---|
| `C-c B` | `compile` |
| `C-c b` | `recompile` |

### Terminal

| Key | Command |
|---|---|
| `C-c t` | `vterm` (current directory) |
| `C-c T` | `vterm-other-window` |
| `C-c M-t` | `multi-vterm` |
| `C-c e` | `eshell` (current directory) |

### Org

| Key | Command |
|---|---|
| `C-c a` | `org-agenda` |
| `C-c c` | `org-capture` |
| `C-c l` | `org-store-link` (global); LSP prefix in LSP buffers |

### Spelling

| Key | Command |
|---|---|
| `C-c S s` | `ispell-word` |
| `C-c S b` | flyspell whole buffer |
| `C-c S n` | next error |
| `C-c S t` | toggle flyspell |
| `C-c S c` | `flyspell-correct-wrapper` |

### UI & Utility

| Key | Command |
|---|---|
| `F12` | Toggle dark/light theme |
| `C-c P` | Presentation mode (enlarged font) |
| `C-c ?` | `which-key-show-top-level` |
| `C-c H` | This keybinding cheat sheet |
| `C-c R` | Reload config |
| `C-c L` | LSP status |
| `C-c w t` | `transpose-frame` |
| `C-c w f` | `flip-frame` |
| `C-c w r` | `rotate-frame-clockwise` |

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

### Profiling

| Key | Command |
|---|---|
| `C-c D s` | `profiler-start` |
| `C-c D r` | `profiler-report` |
| `C-c D q` | `profiler-stop` |

---

## Safe Mode & Recovery

If Emacs crashes 3 times in succession it enters **safe mode** automatically
(minimal config, no packages). You can also force it:

```bash
emacs --safe
EMACS_SAFE_MODE=1 emacs
```

To recover from safe mode:

1. `M-x emacs-ide-recovery-view-log` — find the error
2. Fix the offending config or disable the package: `M-x emacs-ide-recovery-disable-package`
3. `M-x emacs-ide-recovery-reset-crash-count`
4. Restart Emacs normally

To back up and restore config:

```
M-x emacs-ide-recovery-backup-config    # timestamped backup to var/backups/
M-x emacs-ide-recovery-restore-config   # restore from a backup directory
```

---

## Synergy Checklist

Run this after any significant change to confirm all modules are cooperating:

```
M-x emacs-ide-run-tests          ← all green?
M-x emacs-ide-health-check-all   ← no errors?
M-x emacs-ide-startup-report     ← all phases present, time under target?
M-x emacs-ide-config-show        ← gc-threshold = 16777216 (not 0)?
M-x emacs-ide-security-check     ← TLS verified?
```

Open a Python/Rust/Go file and confirm:
- Syntax highlighting via tree-sitter (colors richer than font-lock alone)
- LSP diagnostics appearing in the margin
- `C-c C-t` runs the right test framework
- `F9` sets a breakpoint, `F5` launches the debugger
- Saving triggers async formatting (no cursor jump, no blocking)

---

## Module Load Order

Modules load in this exact sequence (defined in `init.el`):

```
core:     emacs-ide-health → emacs-ide-package → emacs-ide-profiler
          → emacs-ide-security → emacs-ide-telemetry → emacs-ide-recovery

features: ui-core → ui-theme → ui-modeline → ui-dashboard
          → completion-core → completion-snippets
          → editing-core
          → tools-lsp → tools-project → tools-git → tools-terminal
          → tools-format → tools-org → tools-spelling
          → tools-test → lang-core → debug-core
          → keybindings   ← always last
```

`keybindings.el` loads last so its global bindings win over any module's
`:bind` declarations. This is intentional — see the commentary in that file.

---

## Version History

| Version | Notes |
|---|---|
| 2.2.3 | `tools-test.el` added; `init.el` registers it; inline YAML comment parsing fixed |
| 2.2.2 | Config `emacs-ide-config-apply` when-let guards fixed; `tools-lsp` lsp-idle-delay fixed; `keybindings` C-c C-c compile conflict resolved; `lang-core` duplicate org block removed |
| 2.2.1 | warning-minimum-level restore timing fixed; horizontal-scroll-bar-mode removed; native-comp var corrected |
| 2.2.0 | Initial production release |
