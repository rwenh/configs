# Enterprise Emacs IDE

**Version 3.2.2** — Production-grade Emacs configuration with lazy-loaded 50-language support,
LSP, DAP debugging, Hydra menus, unified REPL hub, perspective workspaces, ef-themes,
async formatting, Git integration, and full recovery system.

---

## What's New in v3.2.2

15 bugs fixed across 9 files. Every fix was identified by auditing the first-install log and cross-checking all module interdependencies.

| # | File | Bug | Fix |
|---|------|-----|-----|
| 1 | `ui-modeline.el` | `<nil> <mouse-1> is undefined` on every startup | Keymap only assigned when `define-key` succeeds |
| 2 | `ui-dashboard.el` | `kill-on-file-open` missing closing paren — `remove-hook` never ran, hook fired forever | Paren fixed; `remove-hook` always executes |
| 3 | `init.el` | `emacs-ide-health-check-startup` ghost function called but never defined — startup health check silently skipped | Replaced with real `emacs-ide-health-run-checks` |
| 4 | `emacs-ide-spot-check.el` | `emacs-ide-repl-send-line` and `C-c x l` missing from spot-check | Added to commands list and keybindings table |
| 5 | `emacs-ide-spot-check.el` | `emacs-ide-test-run-all` missing from commands list | Added |
| 6 | `emacs-ide-spot-check.el` | `C-c t` / `C-c e` always reported ✗ — spot-check used symbol equality against lambdas | `keybindings.el` now uses direct refs; entries restored to direct table |
| 7 | `emacs-ide-spot-check.el` | All 10 hydra bodies never verified | Added to commands list where `fboundp` works |
| 8 | `init.el` | `emacs-ide-telemetry-log-startup` defined but never called — startup telemetry log always empty | Called in `emacs-startup-hook` |
| 9 | `emacs-ide-recovery.el` | Crash counter never incremented — counter permanently 0, crash detection non-functional | Increments on startup, resets to 0 on clean exit |
| 10 | `lang-sql.el` | `emacs-ide-config-get-nested` ghost function — SQL dialect from `config.yml` silently ignored | Replaced with `emacs-ide-dev--config-lang-settings` |
| 11 | `emacs-ide-spot-check.el` | 19 public interactive commands not verified by spot-check | Added `emacs-ide-health-status`, `emacs-ide-select-theme`, telemetry toggles, spelling, vterm extras, project/debug helpers, and more |
| 12 | `tools-lsp.el` | `lsp-inlay-hints-enable` hardcoded `t` — `lsp.inlay-hints: false` in config had no effect | Now reads `emacs-ide-lsp-enable-inlay-hints` config variable; propagated to Rust, Lua, TypeScript per-language hints |
| 13 | `tools-git.el` | `emacs-ide-git-enable` guard missing — `git.enable: false` had no effect | Wrapped module body in `(when emacs-ide-git-enable ...)` |
| 14 | `debug-core.el` | `emacs-ide-debug-enable` guard missing — `debug.enable: false` had no effect | Wrapped `dap-mode` setup in guard; helper commands stay defined so hydra/keys always work |
| 15 | `emacs-ide-spot-check.el` | `C-c D s/r/q` and all `C-c h *` always reported ✗ — lambda-bound keys can't be symbol-checked | Split keybindings into two sections: direct (symbol equality) and lambda-bound (non-nil check) |

### What's New in v3.2.x (preceding patches)

- `emacs-ide-repl-send-line` added to REPL hub (`C-c x l`)
- `emacs-ide-test-run-all` added to test runner (`C-c C-T`)
- `emacs-ide-health-status` alias documented and verified
- `emacs-ide-lsp-check-servers` added to `tools-lsp.el`
- `emacs-ide-diagnose-languages` added to `emacs-ide-diagnose.el`
- Config reload hook now fires only on explicit reload, not initial load
- `emacs-ide-recovery--session-timer` declared properly — no void-variable warning on exit

### What's New in v3

- **50-language lazy loading** — `modules/langs/` replaces monolithic `lang-core.el`. Zero boot cost — each lang module loads only when you open a file of that type, or when a project marker (`Cargo.toml`, `go.mod`, `package.json`, etc.) is detected.
- **ef-themes** replaces modus-themes — sharper, more opinionated office aesthetic. Toggle dark/light with `F12`.
- **nerd-icons** replaces all-the-icons — single font, faster load, richer symbol coverage.
- **Hydra menus** — 10 discoverable chord-free command menus (`C-c h w/b/g/l/p/t/d/u/r/s`).
- **perspective.el workspaces** — named workspaces with isolated buffer lists. Auto-creates a workspace per project. Switch with `M-1..9`.
- **Unified REPL hub** — `C-c x r` dispatches to the right REPL for any language (Python, Node, Go, Rust, Clojure, Julia, R, Lua, Nix).
- **tools-project-detect** — reads project root markers + `config.yml` language flags, pre-warms only the lang tiers you use.
- **Optional Meow modal editing** — Emacs-native modal mode. Off by default; enable with `editing.meow: true` in `config.yml`.

---

## Requirements

| Requirement | Minimum | Notes |
|---|---|---|
| Emacs | 29.1+ | Native compilation strongly recommended |
| Git | any | Required — health check fails without it |
| ripgrep (`rg`) | any | Recommended for project search |
| `fd` | any | Recommended for file finding |
| Node.js + npm | any | Required for JS/TS LSP and prettier |
| Go | 1.21+ | Required for Go LSP/tools |
| Rust + cargo | any | Required for Rust LSP/tools |
| Python 3 + pip | 3.9+ | Required for Python LSP/tools |
| Java 21+ | 21 | Required for Java/Kotlin LSP |

---

## Install

```bash
git clone <your-repo> ~/.emacs.d
emacs --init-directory ~/.emacs.d
```

On first launch straight.el bootstraps itself and installs all packages. This takes 2–5 minutes. Subsequent startups are typically **< 2 seconds** in practice (lang modules are lazy). The `startup-time-target: 3.0` in `config.yml` is the warning threshold — a notification fires if startup exceeds it, not a goal in itself.

> **After upgrading Emacs** (e.g. 29 → 30), always purge stale bytecode before restarting:
> ```bash
> find ~/.emacs.d -name "*.elc" -delete
> ```
> Or from inside Emacs: `M-x emacs-ide-purge-bytecode-cache`
> (covers `core/`, `modules/`, `modules/langs/`, and `var/eln-cache/` automatically).

To boot in safe mode (minimal config, no packages):

```bash
emacs --emacs-ide-safe
# or: EMACS_SAFE_MODE=1 emacs
```

---

## Tool Installation by Distro

Install only the tools for languages you actually use. After installing, run
`M-x emacs-ide-diagnose` to verify setup or `M-x emacs-ide-health-status` for a health report.

### openSUSE Leap / Tumbleweed

```bash
# Base tools
sudo zypper install git ripgrep fd emacs

# Language runtimes
sudo zypper install nodejs npm go java-21-openjdk
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
echo 'export PATH="$HOME/.cargo/bin:$HOME/go/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc

# LSP servers — Tier 1
pip install pyright --break-system-packages
rustup component add rust-analyzer
go install golang.org/x/tools/gopls@latest
npm install -g typescript-language-server
sudo zypper install clang                        # C/C++ (clangd included)

# LSP servers — Tier 2
npm install -g bash-language-server yaml-language-server
go install github.com/sqls-server/sqls@latest
# Kotlin LSP
curl -sSLO https://github.com/fwcd/kotlin-language-server/releases/latest/download/server.zip
unzip server.zip -d ~/.local/kotlin-language-server
sudo ln -sf ~/.local/kotlin-language-server/server/bin/kotlin-language-server /usr/local/bin/
# Java LSP (jdtls) — lsp-mode does not auto-install it
VERSION=$(curl -sSL https://download.eclipse.org/jdtls/milestones/ | grep -oP '[\d]+\.[\d]+\.[\d]+' | sort -V | tail -1)
mkdir -p ~/.local/share/jdtls
FILENAME=$(curl -sSL "https://download.eclipse.org/jdtls/milestones/${VERSION}/" | grep -oP 'jdt-language-server-[\d.\-]+\.tar\.gz' | head -1)
curl -L "https://download.eclipse.org/jdtls/milestones/${VERSION}/${FILENAME}" | tar xz -C ~/.local/share/jdtls
printf '#!/bin/sh\nexec python3 ~/.local/share/jdtls/bin/jdtls -configuration ~/.cache/jdtls -data ~/.local/share/jdtls/workspace "$@"\n' | sudo tee /usr/local/bin/jdtls
sudo chmod +x /usr/local/bin/jdtls

# Formatters — Tier 1
pip install black isort --break-system-packages
npm install -g prettier
go install golang.org/x/tools/cmd/goimports@latest
sudo zypper install clang                        # clang-format included

# Formatters — Tier 2
pip install pgformatter --break-system-packages
go install mvdan.cc/sh/v3/cmd/shfmt@latest
cargo install stylua
curl -sSLO https://github.com/pinterest/ktlint/releases/latest/download/ktlint
chmod +x ktlint && sudo mv ktlint /usr/local/bin/
curl -sSLO https://github.com/google/google-java-format/releases/latest/download/google-java-format-all-deps.jar
sudo mkdir -p /usr/local/lib && sudo mv google-java-format-all-deps.jar /usr/local/lib/
printf '#!/bin/sh\nexec java -jar /usr/local/lib/google-java-format-all-deps.jar "$@"\n' | sudo tee /usr/local/bin/google-java-format
sudo chmod +x /usr/local/bin/google-java-format

# Debug adapters
pip install debugpy --break-system-packages
go install github.com/go-delve/delve/cmd/dlv@latest
sudo zypper install lldb
```

### Ubuntu / Debian

```bash
# Base tools
sudo apt install git ripgrep fd-find emacs

# Language runtimes
sudo apt install nodejs npm golang-go default-jdk-21
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
echo 'export PATH="$HOME/.cargo/bin:$HOME/go/bin:$PATH"' >> ~/.bashrc && source ~/.bashrc

# LSP servers — Tier 1
pip install pyright
rustup component add rust-analyzer
go install golang.org/x/tools/gopls@latest
npm install -g typescript-language-server
sudo apt install clangd

# LSP servers — Tier 2
npm install -g bash-language-server yaml-language-server
go install github.com/sqls-server/sqls@latest
# Kotlin LSP and Java LSP: same steps as openSUSE above

# Formatters — Tier 1
pip install black isort
npm install -g prettier
go install golang.org/x/tools/cmd/goimports@latest
sudo apt install clang-format

# Formatters — Tier 2
sudo apt install pgformatter
go install mvdan.cc/sh/v3/cmd/shfmt@latest
cargo install stylua
# ktlint and google-java-format: same steps as openSUSE above

# Debug adapters
pip install debugpy
go install github.com/go-delve/delve/cmd/dlv@latest
sudo apt install lldb
```

### macOS (Homebrew)

```bash
# Base tools
brew install git ripgrep fd emacs

# Language runtimes
brew install node go openjdk@21
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
echo 'export PATH="$HOME/.cargo/bin:$HOME/go/bin:$(brew --prefix openjdk@21)/bin:$PATH"' >> ~/.zshrc && source ~/.zshrc

# LSP servers — Tier 1
pip install pyright
rustup component add rust-analyzer
go install golang.org/x/tools/gopls@latest
npm install -g typescript-language-server
brew install llvm                                # provides clangd

# LSP servers — Tier 2
npm install -g bash-language-server yaml-language-server
go install github.com/sqls-server/sqls@latest
brew install kotlin-language-server
# Java LSP: same manual jdtls steps as openSUSE above

# Formatters — Tier 1
pip install black isort
npm install -g prettier
go install golang.org/x/tools/cmd/goimports@latest
brew install clang-format

# Formatters — Tier 2
pip install pgformatter
go install mvdan.cc/sh/v3/cmd/shfmt@latest
cargo install stylua
brew install ktlint google-java-format

# Debug adapters
pip install debugpy
go install github.com/go-delve/delve/cmd/dlv@latest
# LLDB ships with Xcode Command Line Tools
```

### Fonts (all distros)

After first Emacs launch, install the Nerd Icons font once:

```
M-x nerd-icons-install-fonts
```

### LSP servers not covered by `M-x lsp-install-server`

| Server | Language | Install method |
|---|---|---|
| `jdtls` | Java | Manual — see openSUSE steps above |

All other Tier 1/2 servers are detected automatically by `lsp-mode` once on PATH.

---

## Directory Structure

```
~/.emacs.d/                                     52 files total (51 .el + 1 .yml)
│
├── early-init.el              GC · frame · native-comp · JIT · TLS (before init)
├── init.el                    Bootstrap · module loader · startup tracking
├── config.yml                 All user config — edit this, not the .el files
│
├── core/                      10 files — infrastructure, always loaded at boot
│   ├── emacs-ide-config.el    YAML loader; (require)'d first, before core loop
│   ├── emacs-ide-health.el    Health check system + emacs-ide-health-status alias
│   ├── emacs-ide-diagnose.el  Diagnostics — M-x emacs-ide-diagnose / diagnose-lsp / diagnose-languages
│   ├── emacs-ide-package.el   Package load-time tracking
│   ├── emacs-ide-profiler.el  CPU / memory profiler
│   ├── emacs-ide-recovery.el  Crash tracking · safe mode · config backup/restore
│   ├── emacs-ide-security.el  TLS · auth-source · network security
│   ├── emacs-ide-telemetry.el Local usage analytics (startup metrics now logged)
│   ├── emacs-ide-test.el      ERT integration tests (M-x emacs-ide-run-tests)
│   └── emacs-ide-spot-check.el Command · keybinding · feature integrity check
│
├── modules/                   26 files — feature modules, loaded eagerly in order
│   ├── ui-core.el             ef-themes · nerd-icons · visual enhancements
│   ├── ui-theme.el            ef-themes toggle (F12) · auto dark/light by time
│   ├── ui-modeline.el         doom-modeline + health segment (mouse-1 fixed)
│   ├── ui-dashboard.el        Startup dashboard (kill-on-file-open hook fixed)
│   ├── ui-workspace.el        perspective.el workspaces · tab-bar · M-1..9
│   ├── completion-core.el     Vertico · Corfu · Consult · Embark · Orderless
│   ├── completion-snippets.el YASnippet + community snippets
│   ├── editing-core.el        Smartparens · undo-tree · mc · avy · Meow (opt)
│   ├── core-dev.el            Shared lang API · emacs-ide-dev-*
│   ├── tools-lsp.el           lsp-mode · lsp-ui · flycheck (inlay-hints config-driven)
│   ├── tools-project.el       Projectile · Treemacs · Neotree
│   ├── tools-project-detect.el Project root → lang tier pre-warming
│   ├── tools-git.el           Magit · diff-hl · forge · git-timemachine (git.enable guard added)
│   ├── tools-terminal.el      VTerm (C-c t) · Eshell · Docker
│   ├── tools-format.el        Apheleia base config
│   ├── apheleia-langs-patch.el 50-lang complete formatter map
│   ├── tools-repl.el          Unified REPL hub · C-c x r/s/b/d/l/t
│   ├── tools-org.el           Org-mode · agenda · capture · babel · export
│   ├── tools-spelling.el      Flyspell prose + flyspell-prog-mode code
│   ├── tools-notes.el         org-roam linked notes
│   ├── tools-rest.el          Restclient · Verb
│   ├── tools-test-runner-registry.el  Per-lang test registry · C-c X f/p/./w/s
│   ├── tools-test.el          Auto-detect fallback + history · C-c C-t / C-c C-T
│   ├── debug-core.el          DAP adapters (debug.enable guard added)
│   ├── tools-hydra.el         10 Hydra menus · C-c h w/b/g/l/p/t/d/u/r/s
│   └── keybindings.el         All global keys — always loads last
│
├── modules/langs/             13 files — lazy, zero boot cost
│   ├── lang-python.el         pyright · ipython · pytest · poetry · debugpy
│   ├── lang-rust.el           rust-analyzer · evcxr · cargo · LLDB
│   ├── lang-web.el            tsserver · Node REPL · jest · prettier
│   ├── lang-go.el             gopls · gore · go test · Delve
│   ├── lang-c.el              clangd · clang-format · CMake · LLDB
│   ├── lang-jvm.el            jdtls · kotlin-ls · Metals · Maven/Gradle
│   ├── lang-lua.el            lua-language-server · stylua · busted
│   ├── lang-shell.el          bash-ls · shellcheck · shfmt · bats
│   ├── lang-sql.el            sqls · pgformatter · ejc-sql (dialect reads config.yml)
│   ├── lang-data.el           ESS/R · julia-repl · Jupyter
│   ├── lang-functional.el     HLS · CIDER · Elixir · OCaml · Erlang
│   ├── lang-systems.el        ZLS · nil · nix-mode · Zig · D · V
│   └── lang-prose.el          md · yaml · toml · json · docker · terraform
│
├── var/                       Auto-created on first launch — do not commit
│   ├── backups/               Versioned config.yml backups
│   ├── eln-cache/             Native-compiled .eln files
│   ├── crash-history          Crash counter (increments on start, resets on clean exit)
│   ├── recovery.log           Recovery system log
│   ├── telemetry.log          Local startup metrics log
│   ├── persp-state            Workspace layout save file
│   ├── places                 Save-place history
│   └── custom.el              Emacs custom-set-variables
│
└── snippets/                  Auto-created — add custom YASnippet snippets here
```

| Location | Files | How loaded |
|---|---|---|
| root | 3 | `early-init.el` at startup · `init.el` bootstraps · `config.yml` parsed by config module |
| `core/` | 10 | `emacs-ide-config.el` first · 7 via core loop · `emacs-ide-test.el` + `emacs-ide-spot-check.el` eagerly at end of init |
| `modules/` | 26 | Eagerly in order at startup |
| `modules/langs/` | 13 | Lazily on file open or project switch |
| **Total** | **52** | |

`var/` and `snippets/` are auto-created. Never commit them.

---

## Configuration

All settings live in **`config.yml`** — edit this, not the `.el` modules.

```bash
M-x emacs-ide-config-edit     # Open config in Emacs
M-x emacs-ide-config-reload   # Reload after editing (no restart needed)
M-x emacs-ide-config-show     # View currently active values
```

Key sections and their effects:

```yaml
general:
  theme: ef-dark               # ef-dark · ef-light · ef-cherie · ef-winter · …
  font: JetBrains Mono
  font-size: 11

lsp:
  enable: true                 # false → tools-lsp.el skips all lsp-mode setup
  inlay-hints: true            # false → disables hints in all languages uniformly

git:
  enable: true                 # false → Magit and all git packages do not load

debug:
  enable: true                 # false → dap-mode does not load

editing:
  meow: false                  # true → Emacs-native modal editing

workspace:
  enable: true                 # perspective.el named workspaces

languages:
  python: true                 # Loads lang-python.el on first .py file
  rust: true
  go: true
  javascript: true
  haskell: false               # Set true to enable (off by default)

lang-settings:
  sql:
    dialect: postgres          # Read correctly since v3.2.2
  rust:
    lsp-server: rust-analyzer
    cargo-watch: clippy
  python:
    lsp-server: pyright
    formatter: black
```

> **Languages are lazy by default.** Setting a language to `false` prevents
> its module from ever loading. Keys absent from `config.yml` default to `true`.

---

## How Lazy Loading Works

```
Emacs starts
  → early-init.el   (GC, frame, native-comp)
  → init.el         (core + 26 feature modules — no lang modules)
  → ready in ~1.5-2s

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

### Quick Verification

Run these after setup or after any significant change:

```
M-x emacs-ide-spot-check            ← commands + keybindings + features integrity
M-x emacs-ide-run-tests             ← full ERT suite
M-x emacs-ide-diagnose              ← module load status + config validation
M-x emacs-ide-diagnose-lsp          ← LSP server availability
M-x emacs-ide-diagnose-languages    ← per-language enable/load/LSP/formatter status
M-x emacs-ide-health-status         ← system tools + LSP + config health
```

### Full Status Command Reference

```
M-x emacs-ide-health-status         health report (system tools, LSP, config)
M-x emacs-ide-health-auto-fix       attempt automatic remediation of warnings
M-x emacs-ide-lsp-check-servers     which LSP servers are on PATH
M-x emacs-ide-lsp-status            LSP connection status for current buffer
M-x emacs-ide-config-show           all active configuration variable values
M-x emacs-ide-startup-report        load phase breakdown and timings
M-x emacs-ide-package-report        package load times (slowest 20)
M-x emacs-ide-detect-show-status    project detection + pre-warmed languages
M-x emacs-ide-repl-status           live REPL hub status
M-x emacs-ide-test-runner-status    registered per-language test runners
M-x emacs-ide-workspace-status      perspective workspaces + buffer counts
M-x emacs-ide-security-check        TLS + auth sources + GPG audit
M-x emacs-ide-recovery-report       crash counter, log path, disabled packages
M-x emacs-ide-telemetry-report      local usage analytics
M-x emacs-ide-early-init-report     early-init phase benchmark
M-x emacs-ide-check-formatters      which formatters are on PATH
```

### Expected Spot-Check Output (clean install)

```
=== EMACS IDE SPOT CHECK ===

COMMANDS (M-x):
  ✓ emacs-ide-run-tests
  ✓ emacs-ide-diagnose
  ✓ emacs-ide-health-check-all
  ✓ emacs-ide-health-status
  ... (94 commands total — all ✓)

KEYBINDINGS (direct — verified by symbol equality):
  ✓ C-c x r      → emacs-ide-repl-launch
  ✓ C-c x l      → emacs-ide-repl-send-line
  ✓ C-c C-T      → emacs-ide-test-run-all
  ✓ C-c t        → emacs-ide-vterm-here
  ✓ C-c e        → emacs-ide-eshell-here
  ... (42 bindings total — all ✓)

KEYBINDINGS (lambda-bound — verified as non-nil):
  ✓ C-c h w      → lambda/compiled
  ✓ C-c D s      → lambda/compiled
  ... (13 bindings total — all ✓)

MODULE FEATURES PROVIDED:
  ✓ emacs-ide-config
  ✓ emacs-ide-health
  ... (all 37 features — all ✓)

=== SUMMARY ===
✓ ALL CHECKS PASSED — everything is working correctly.
```

---

## Keybindings — Non-Obvious Reference

Vanilla Emacs defaults (`C-x C-s`, `C-x C-f`, `M-.`, `C-/`, etc.) work as always.
Run `C-h k` to look up any key.

### Hydra menus — C-c h prefix

| Key | Hydra | What's inside |
|---|---|---|
| `C-c h w` | Window | split · resize · ace · winner |
| `C-c h b` | Buffer | switch · kill · scratch · ibuffer |
| `C-c h g` | Git | magit · diff · blame · stash · forge |
| `C-c h l` | LSP | rename · actions · refs · format · hover |
| `C-c h p` | Project | find · ripgrep · compile · treemacs |
| `C-c h t` | Test | file · project · at-point · watch · report |
| `C-c h d` | Debug | step · continue · breakpoints · inspect · repl |
| `C-c h u` | Toggles | theme · line-nos · flycheck · dimmer · zen |
| `C-c h r` | REPL | launch · send region/buffer/defun · toggle |
| `C-c h s` | Search | ripgrep · occur · imenu · outline · symbol |
| `C-c h h` | Help | list all hydra entry points |

### REPL — C-c x prefix (lowercase x)

| Key | Command |
|---|---|
| `C-c x r` | Launch / switch to REPL for current language |
| `C-c x s` | Send region to REPL |
| `C-c x b` | Send buffer to REPL |
| `C-c x d` | Send defun at point to REPL |
| `C-c x l` | Send current line to REPL and advance |
| `C-c x t` | Toggle REPL window (bottom side window) |
| `C-c x R` | Show test report (this session) |

### Tests — C-c X prefix (uppercase X)

| Key | Command |
|---|---|
| `C-c X f` | Run file tests |
| `C-c X p` | Run project tests |
| `C-c X .` | Run test at point |
| `C-c X w` | Watch mode |
| `C-c X s` | Show registered runners |
| `C-c X l` | Repeat last test run |
| `C-c C-t` | Smart dispatch (file → project → auto-detect) |
| `C-c C-T` | Force full project suite |

Supported frameworks (auto-detected): pytest · unittest · cargo test · go test ·
jest · vitest · npm test · rspec · mvn test · gradle test · mix test ·
cabal test · stack test · bats · ERT · ctest · make test.

### Workspaces — C-c W prefix

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

| Key | Command |
|---|---|
| `C-:` | `avy-goto-char` |
| `C-'` | `avy-goto-char-2` |
| `M-g f` | `avy-goto-line` |
| `M-g w` | `avy-goto-word-1` |
| `M-o` | `ace-window` |
| `C-c left` | `winner-undo` |

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
| `M-g s` | `consult-lsp-symbols` |
| `M-g e` | `consult-lsp-diagnostics` |

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
| `C-.` | `embark-act` |
| `C-;` | `embark-dwim` |
| `M-/` | `hippie-expand` |

### LSP (active in LSP buffers only)

| Key | Command |
|---|---|
| `C-c l r` | `lsp-rename` |
| `C-c l f` | `lsp-format-buffer` |
| `C-c l a` | `lsp-execute-code-action` |
| `M-.` | `lsp-ui-peek-find-definitions` |
| `M-?` | `lsp-ui-peek-find-references` |

### Debugging (DAP)

| Key | Command |
|---|---|
| `F5` | Start debug session |
| `F8` | Toggle breakpoint |
| `C-c h d` | Debug hydra (full step/break/inspect/repl menu) |

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
| `F9` | Treemacs toggle |
| `C-c n` | Neotree toggle |
| `C-c D d` | Project detect status |
| `C-c D s` | Profiler start |
| `C-c D r` | Profiler report |

### Terminal

| Key | Command |
|---|---|
| `C-c t` | `emacs-ide-vterm-here` (current directory) |
| `C-c T` | `vterm-other-window` |
| `C-c e` | `emacs-ide-eshell-here` (current directory) |

### UI & Utility

| Key | Command |
|---|---|
| `F12` | Toggle dark/light ef-theme |
| `C-c P` | Presentation mode |
| `C-c ?` | `which-key-show-top-level` |
| `C-c H` | Keybinding cheat sheet |
| `C-c R` | Reload config |
| `C-c L` | LSP status |
| `C-c V s` | REST scratch buffer |
| `C-c V i` | Insert REST request template |

### Recovery — C-c r prefix

| Key | Command |
|---|---|
| `C-c r r` | Recovery report |
| `C-c r v` | View recovery log |
| `C-c r b` | Backup config.yml |
| `C-c r d` | Disable problematic package |
| `C-c r C-r` | Reset crash counter |

---

## Safe Mode & Recovery

The crash counter increments on every Emacs startup and resets to zero on a clean exit via `kill-emacs-hook`. If Emacs crashes, the hook never runs and the incremented count persists. The next startup finds count > 0, which you can inspect with `M-x emacs-ide-recovery-report`.

Force safe mode manually:

```bash
emacs --emacs-ide-safe
# or: EMACS_SAFE_MODE=1 emacs
```

Recovery workflow:

1. `M-x emacs-ide-recovery-report` — see crash count and disabled packages
2. `M-x emacs-ide-recovery-view-log` — find the error
3. `M-x emacs-ide-recovery-disable-package` — isolate a broken package
4. `M-x emacs-ide-recovery-reset-crash-count` — reset counter once resolved
5. Restart Emacs normally

Config backup/restore:

```
M-x emacs-ide-recovery-backup-config    — timestamped backup to var/backups/
M-x emacs-ide-recovery-restore-config   — interactively restore from backup
```

---

## Module Load Order

```
early-init:  GC · frame · native-comp · JIT · TLS

core:        emacs-ide-config (first, via require)
             → emacs-ide-health → emacs-ide-diagnose → emacs-ide-package
             → emacs-ide-profiler → emacs-ide-security → emacs-ide-telemetry
             → emacs-ide-recovery
             → emacs-ide-test + emacs-ide-spot-check  (eager, at end of init)

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

langs/:      loaded lazily on file open or project switch
             lang-python · lang-rust · lang-web · lang-go · lang-c
             lang-jvm · lang-lua · lang-shell · lang-sql · lang-data
             lang-functional · lang-systems · lang-prose
```

Load order notes:

- `emacs-ide-config.el` loads first so config values are available to all downstream modules
- `core-dev.el` loads before `tools-lsp.el` so language modules can call `emacs-ide-dev-*`
- `keybindings.el` loads last so its global bindings win over any module's `:bind` declarations
- `tools-hydra.el` wraps all hydra definitions in `(with-eval-after-load 'hydra)` — hydra is `:demand t` so bodies are always available by the time keybindings loads
- All `langs/` modules are deferred — no hook, lazy-loaded on file open or project switch

---

## Synergy Checklist

Run after any significant change or fresh install:

```
M-x emacs-ide-spot-check            all ✓? (94 commands, 42 direct bindings, 13 lambda bindings, 37 features)
M-x emacs-ide-run-tests             all ERT tests green?
M-x emacs-ide-health-status         health report OK?
M-x emacs-ide-diagnose              module load status + config validation
M-x emacs-ide-startup-report        all phases present?
M-x emacs-ide-config-show           gc-threshold = 16777216?
M-x emacs-ide-security-check        TLS verified?
M-x emacs-ide-detect-show-status    project detect working?
```

Open a Python/Rust/Go file and confirm:

- Tree-sitter syntax highlighting active (richer colors)
- LSP diagnostics appearing in the margin
- Inlay hints visible (controlled by `lsp.inlay-hints` in config.yml)
- `C-c X f` runs the right test framework
- `C-c x r` opens the correct REPL
- `C-c x l` sends the current line to the REPL
- Saving triggers async formatting (no cursor jump)
- `C-c h l` opens the LSP hydra menu

---

## Support & Troubleshooting

### IDE Won't Start

```bash
EMACS_SAFE_MODE=1 emacs
```

Then:

```
M-x emacs-ide-recovery-view-log
M-x emacs-ide-diagnose
```

### `<nil> <mouse-1> is undefined` on startup

This was a bug in `ui-modeline.el` fixed in v3.2.2. Hot-swap the fixed `ui-modeline.el` into `~/.emacs.d/modules/`.

### LSP Not Working

```
M-x emacs-ide-lsp-status            current buffer LSP connection
M-x emacs-ide-lsp-check-servers     which servers are on PATH
M-x emacs-ide-diagnose-lsp          full LSP diagnostics
```

Check `config.yml`: `lsp.enable` must be `true`. Verify `lsp.inlay-hints` matches your preference.

### SQL Dialect Not Respected

Fixed in v3.2.2 (`lang-sql.el`). Hot-swap the fixed file into `~/.emacs.d/modules/langs/`.

### git.enable / debug.enable / lsp.inlay-hints Not Respected

Fixed in v3.2.2 (`tools-git.el`, `debug-core.el`, `tools-lsp.el`). Hot-swap the three fixed files.

### Slow Startup

```
M-x emacs-ide-startup-report
M-x emacs-ide-package-report
M-x emacs-ide-early-init-report
```

### Spot-Check Reports Failures

```
M-x emacs-ide-spot-check
```

If commands show ✗: module failed to load — check `*Messages*` for errors and try `M-x emacs-ide-health-auto-fix`.  
If lambda-bound keys show UNBOUND: `tools-hydra.el` or `keybindings.el` failed to load.

### Configuration Not Applied

```
M-x emacs-ide-config-reload
M-x emacs-ide-config-show
```

Verify `gc-cons-threshold` is 16777216 in the output.

---

## Version History

| Version | Notes |
|---|---|
| 3.2.2 | **15 bug fixes across 9 files.** `<nil> mouse-1` startup error. `kill-on-file-open` hook. Ghost functions (`emacs-ide-health-check-startup`, `emacs-ide-config-get-nested`). Crash counter now functional. Startup telemetry now logged. `lsp.inlay-hints` / `git.enable` / `debug.enable` config flags now respected. Spot-check: 19 missing commands added, lambda-bound key handling fixed, C-c C-T added. |
| 3.2.1 | `emacs-ide-recovery--session-timer` declared. `emacs-ide-lsp-check-servers` added. `emacs-ide-diagnose-languages` added. `emacs-ide-repl-send-line` added (`C-c x l`). Config reload hook fires only on explicit reload. |
| 3.2.0 | `emacs-ide-health-auto-fix` and `emacs-ide-health-run-check` added. Periodic health timer repeat arg fixed. `emacs-ide-config-apply` uses `alist-get`. `emacs-ide-spot-check.el` infinite recursion in `init.el` fixed. |
| 3.0.4 | Config reload hook. YAML level-3 parser. LSP `:init` → `:config` for Rust/Web/Lua. Health check periodic timer. `hydra-debug` rename. Taplo TOML formatter. LSP `setq` odd-arg crash. `core-dev` language cache. `emacs-ide-diagnose.el` added. |
| 3.0.3 | `emacs-ide-spot-check.el` added. `emacs-ide-test.el` fixed for Emacs 30. |
| 3.0.2 | `C-c R` void-function. REST prefix `C-c R` → `C-c V`. Formatter allowlist expanded. |
| 3.0.1 | `core-dev.el` string→symbol key fix. Double LSP start on Python removed. |
| 3.0.0 | 50-lang lazy loading. ef-themes. nerd-icons. Hydra menus. perspective.el workspaces. Unified REPL hub. Optional Meow modal editing. |
| 2.2.7 | `early-init.el` TTY guards. `warning-suppress-log-types` pre-bind. |
| 2.2.6 | Emacs 30 stale bytecode fix. `emacs-ide-purge-bytecode-cache` added. |
| 2.2.5 | Startup 58s → <3s — straight.el tuning, dashboard optimization, projectile indexing. |

---

## License

[Your License Here]

---

**Enterprise Emacs IDE v3.2.2** — Production Ready ✅
