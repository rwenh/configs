# Enterprise Emacs IDE - Production Deployment Guide

> **Version 2.0.0** | **Target**: Production Teams & Enterprise Developers

---

## 🏗️ Complete Directory Structure

```
~/.emacs.d/
├── early-init.el              # Performance-optimized early init
├── init.el                    # Main bootstrap file
├── config.yml                 # User configuration (optional)
├── .emacs-version-lock        # Package version lock
│
├── core/                      # Core system (DO NOT MODIFY)
│   ├── emacs-ide-config.el    # Configuration management
│   ├── emacs-ide-health.el    # Health check system ✓
│   ├── emacs-ide-package.el   # Package utilities
│   ├── emacs-ide-profiler.el  # Performance profiling
│   ├── emacs-ide-recovery.el  # Error recovery ✓
│   ├── emacs-ide-security.el  # Security hardening
│   └── emacs-ide-telemetry.el # Usage analytics
│
├── modules/                   # Feature modules
│   ├── completion/
│   │   ├── completion-core.el
│   │   └── completion-snippets.el
│   ├── editing/
│   │   ├── editing-core.el
│   │   └── editing-nav.el
│   ├── ui/
│   │   ├── ui-core.el
│   │   ├── ui-theme.el
│   │   ├── ui-modeline.el
│   │   └── ui-dashboard.el
│   ├── tools/
│   │   ├── tools-lsp.el
│   │   ├── tools-project.el
│   │   ├── tools-git.el
│   │   └── tools-terminal.el
│   ├── languages/
│   │   └── lang-config.el
│   ├── debug/
│   │   └── debug-core.el
│   └── keybindings.el
│
├── lib/                       # Utility libraries
│   ├── lib-utils.el           # Common utilities
│   └── lib-wayland.el         # Wayland support
│
├── var/                       # Runtime data (auto-created)
│   ├── custom.el              # Customization file
│   ├── recovery.log           # Recovery events
│   ├── crash-history          # Crash tracking
│   ├── eln-cache/             # Native compilation cache
│   ├── tree-sitter/           # Tree-sitter grammars
│   ├── backups/               # Configuration backups
│   │   └── config-YYYYMMDD-HHMMSS/
│   └── cache/                 # Package & temp cache
│
├── snippets/                  # YASnippet templates
│   ├── python-mode/
│   ├── go-mode/
│   └── ...
│
├── straight/                  # Package manager (auto-created)
│   ├── repos/
│   └── build/
│
├── test/                      # Test suite
│   ├── emacs-ide-test.el      # Unit tests
│   ├── benchmark.el           # Performance benchmarks
│   └── integration/           # Integration tests
│
├── docs/                      # Documentation
│   ├── COMMANDS.md            # Auto-generated command reference
│   ├── KEYBINDINGS.md         # Keybinding cheat sheet
│   └── TROUBLESHOOTING.md     # Common issues
│
├── scripts/                   # Deployment scripts
│   ├── install.sh             # Installation script
│   ├── update.sh              # Update script
│   ├── test.sh                # Test runner
│   └── ci.sh                  # CI/CD script
│
└── .github/                   # GitHub integration
    └── workflows/
        └── test.yml           # CI/CD pipeline
```

---

## 🚀 Quick Start Installation

### Prerequisites

**Required:**
- Emacs 29.1+ (with native compilation recommended)
- Git
- Internet connection (first run only)

**Recommended:**
```bash
# For best experience, install these:
sudo apt install ripgrep fd-find  # Debian/Ubuntu
brew install ripgrep fd           # macOS
```

### Step 1: Backup Existing Configuration

```bash
# Backup your current config
mv ~/.emacs.d ~/.emacs.d.backup-$(date +%Y%m%d)
mv ~/.emacs ~/.emacs.backup 2>/dev/null || true
```

### Step 2: Clone Repository

```bash
# Clone the enterprise configuration
git clone https://github.com/yourusername/emacs-ide-enterprise.git ~/.emacs.d
cd ~/.emacs.d
```

### Step 3: First Launch

```bash
# Launch Emacs (packages will auto-install)
emacs
```

**What happens on first launch:**
1. Straight.el bootstraps automatically
2. Core modules load with health checks
3. Feature modules load with fallbacks
4. Health check runs after 2 seconds
5. Auto-fix attempts to resolve issues

**Expected first-run time:** 2-5 minutes (packages installing)

---

## ⚙️ Configuration

### Method 1: YAML Configuration (Recommended)

Create `~/.emacs.d/config.yml`:

```yaml
general:
  theme: modus-vivendi        # or modus-operandi
  font: JetBrains Mono
  font_size: 11
  safe_mode: false

completion:
  backend: corfu              # or company
  delay: 0.1
  snippet_expansion: true

lsp:
  enable: true
  inlay_hints: true
  large_file_threshold: 100000
  semantic_tokens: true

performance:
  gc_threshold: 16777216      # 16MB
  startup_time_target: 2.0
  native_comp_jobs: 4

features:
  dashboard: true
  which_key: true
  beacon: true
  rainbow_delimiters: true

security:
  tls_verify: true
  package_signatures: allow-unsigned  # or t for strict

telemetry:
  enabled: true               # Local only, never sent
  usage_stats: true
```

### Method 2: Elisp Configuration

Create `~/.emacs.d/var/custom.el`:

```elisp
(custom-set-variables
 '(emacs-ide-theme 'modus-vivendi)
 '(emacs-ide-font "JetBrains Mono-11")
 '(emacs-ide-completion-backend 'corfu)
 '(emacs-ide-lsp-enable-inlay-hints t))
```

---

## 🏥 Health Check System

### Run Health Check

```elisp
M-x emacs-ide-health
```

**Checks performed:**
- ✓ System tools (git, grep, find, ripgrep)
- ✓ LSP servers (pyright, rust-analyzer, gopls, etc.)
- ✓ Code formatters (black, prettier, rustfmt)
- ✓ Tree-sitter grammars
- ✓ Performance metrics
- ✓ Package integrity
- ✓ Security configuration

### Auto-Fix Issues

```elisp
M-x emacs-ide-health-fix
```

Health checks run automatically:
- On startup (after 2 seconds)
- After package updates
- Can be scheduled hourly

---

## 🛡️ Error Recovery System

### Features

1. **Crash Tracking**: Counts crashes across sessions
2. **Safe Mode**: Enters minimal config after 3+ crashes
3. **Package Fallbacks**: Automatically tries alternatives
4. **Configuration Backup**: Auto-backup before major changes
5. **Error Logging**: Comprehensive logging to `var/recovery.log`

### Recovery Commands

```elisp
C-c r r    ; Recovery report
C-c r v    ; View recovery log
C-c r b    ; Backup current config
C-c r R    ; Restore from backup
C-c r d    ; Disable problematic package
C-c r C-r  ; Reset crash counter
```

### Safe Mode

**Trigger safe mode manually:**
```bash
emacs --eval "(setq emacs-ide-safe-mode t)"
# or
EMACS_SAFE_MODE=1 emacs
```

**Exit safe mode:**
```elisp
M-x emacs-ide-recovery-reset-crash-count
```

---

## 📊 Performance Monitoring

### Startup Performance

```elisp
M-x emacs-ide-startup-report
```

Shows:
- Total startup time
- Per-phase breakdown
- GC count during startup
- Package count

**Target:** < 2.0 seconds

### Runtime Profiling

```elisp
M-x profiler-start     ; Start profiling
; ... do work ...
M-x profiler-report    ; View report
M-x profiler-stop
```

### Package Load Times

```elisp
M-x use-package-report
```

Shows slowest packages to load.

---

## 🔄 Updates & Maintenance

### Update All Packages

```elisp
M-x emacs-ide-update
```

Or from command line:
```bash
cd ~/.emacs.d
./scripts/update.sh
```

### Version Locking

**Lock current versions:**
```elisp
M-x emacs-ide-freeze-versions
```

Creates `versions.lock` with exact package commits.

**Use locked versions:**
```elisp
(setq straight-current-profile 'pinned)
```

### Backup Before Updates

```bash
# Automatic backup before major updates
./scripts/backup.sh

# Manual backup
M-x emacs-ide-recovery-backup-config
```

---

## 🧪 Testing

### Run Test Suite

```bash
cd ~/.emacs.d
./scripts/test.sh
```

Tests:
- Startup time < 2s
- All critical packages load
- LSP available for major languages
- No byte-compilation warnings

### Manual Tests

```elisp
M-x ert                         ; Run all tests
M-x ert-run-tests-interactively ; Interactive testing
```

### CI/CD Integration

GitHub Actions automatically tests:
- Multiple Emacs versions (29.1, 29.2, 30.0)
- Startup time benchmarks
- Byte-compilation
- Package installation

---

## 🔒 Security Hardening

### Credential Management

```elisp
;; Store credentials encrypted
;; Create ~/.authinfo.gpg
machine github.com login YOUR_USERNAME password YOUR_TOKEN
```

### TLS Verification

```elisp
;; Already configured in security module
(setq gnutls-verify-error t
      tls-checktrust t)
```

### Package Signatures

```elisp
;; Strict mode (verify all packages)
(setq package-check-signature t)

;; Or in config.yml:
security:
  package_signatures: t
```

---

## 👥 Team Deployment

### Shared Configuration

1. **Fork repository**
2. **Customize** for your team in `config.yml`
3. **Lock versions**: `M-x emacs-ide-freeze-versions`
4. **Commit** `versions.lock`
5. **Deploy** to team members

### Team Member Installation

```bash
git clone https://github.com/yourorg/emacs-ide-config.git ~/.emacs.d
cd ~/.emacs.d
emacs  # First run installs locked versions
```

Everyone gets identical configuration.

### Environment-Specific Configs

```yaml
# config.yml
environments:
  work:
    org_directory: ~/work/org
    project_paths:
      - ~/work/projects
  home:
    org_directory: ~/personal/org
    project_paths:
      - ~/code
```

Automatically detects environment based on hostname or ENV vars.

---

## 🐛 Troubleshooting

### Slow Startup

```elisp
M-x emacs-ide-startup-report   ; Check phase times
M-x use-package-report         ; Find slow packages
M-x esup                       ; Detailed profiling
```

**Common fixes:**
- Disable unused languages in `lang-config.el`
- Increase `gc-cons-threshold`
- Use `native-comp` if available

### LSP Not Working

```elisp
M-x emacs-ide-health           ; Check LSP servers installed
M-x lsp-install-server         ; Install missing servers
C-c L                          ; LSP status
```

### Package Fails to Load

```elisp
C-c r v    ; View recovery log
C-c r d    ; Disable package
```

Recovery system automatically tries fallbacks.

### Reset to Defaults

```bash
# Nuclear option: start fresh
rm -rf ~/.emacs.d/straight
rm -rf ~/.emacs.d/var
emacs  # Reinstalls everything
```

---

## 📈 Monitoring Dashboard

### Usage Statistics

```elisp
M-x emacs-ide-telemetry-report
```

Shows:
- Top 20 commands used
- Most edited file types
- Average session duration
- Crash rate

**Privacy:** All data stored locally, never sent externally.

---

## 🎯 Production Checklist

Before deploying to production team:

- [ ] Health check passes with 0 errors
- [ ] Startup time < 2 seconds
- [ ] All required LSP servers installed
- [ ] Version lock created (`versions.lock`)
- [ ] CI/CD pipeline passing
- [ ] Documentation updated
- [ ] Team tested on different OSes
- [ ] Backup/restore tested
- [ ] Recovery system tested (simulate crash)
- [ ] Security audit passed

---

## 🆘 Support

**Health Check:** `M-x emacs-ide-health`
**Recovery:** `C-c r r`
**Version:** `M-x emacs-ide-show-version`
**Logs:** `~/.emacs.d/var/recovery.log`

**Community:**
- GitHub Issues: Report bugs
- Discussions: Ask questions
- Wiki: Extended documentation

---

## 📝 License

MIT License - Production ready for commercial use.

---

**Built for reliability. Designed for teams. Optimized for performance.**
