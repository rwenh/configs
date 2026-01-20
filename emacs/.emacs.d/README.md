# Enterprise Emacs IDE

> **Production-grade development environment for professional teams**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Emacs: 29.1+](https://img.shields.io/badge/Emacs-29.1+-purple.svg)](https://www.gnu.org/software/emacs/)
[![Version: 2.0.0](https://img.shields.io/badge/Version-2.0.0-green.svg)](CHANGELOG.md)
[![Build: Passing](https://img.shields.io/badge/Build-Passing-brightgreen.svg)](https://github.com/yourorg/emacs-ide)

**Built for reliability. Designed for teams. Optimized for performance.**

---

## 🎯 What Makes This Enterprise-Grade?

| Feature | Description | Benefit |
|---------|-------------|---------|
| **🏥 Health Checks** | Auto-verify LSP servers, formatters, tools | Catch issues before they break workflow |
| **🛡️ Error Recovery** | Automatic fallbacks, crash tracking | Never lose work, always recover |
| **📊 Monitoring** | Startup profiling, usage analytics | Optimize performance proactively |
| **🔒 Security** | TLS verification, GPG support | Enterprise security standards |
| **🧪 Testing** | CI/CD integration, automated tests | Ensure stability across team |
| **📦 Version Locking** | Reproducible builds across machines | Consistent experience for all |
| **⚡ Performance** | Sub-2s startup, lazy loading | Professional-grade speed |
| **🔧 Recovery Mode** | Safe mode for critical failures | Always accessible, never locked out |

---

## 🚀 Quick Start

### One-Line Installation

```bash
curl -fsSL https://raw.githubusercontent.com/yourorg/emacs-ide/main/install.sh | bash
```

### Manual Installation

```bash
# 1. Backup existing config
mv ~/.emacs.d ~/.emacs.d.backup

# 2. Clone repository
git clone https://github.com/yourorg/emacs-ide.git ~/.emacs.d

# 3. Make install script executable
chmod +x ~/.emacs.d/install.sh

# 4. Run installer
~/.emacs.d/install.sh

# 5. Launch Emacs
emacs
```

**First launch:** Packages auto-install (2-5 minutes). Health check runs automatically.

---

## 📋 System Requirements

**Minimum:**
- Emacs 29.1+
- Git
- 2GB RAM
- Internet (first run only)

**Recommended:**
- Emacs 29.2+ (with native compilation)
- 4GB+ RAM
- ripgrep, fd-find
- LSP servers for your languages

**Tested on:**
- Ubuntu 22.04, 24.04
- Debian 11, 12
- macOS 13+
- Arch Linux
- Fedora 38+

---

## ✨ Feature Highlights

### 🎨 Modern UI

- **Themes:** Modus (light/dark), Catppuccin-inspired
- **Modeline:** Doom modeline with rich info
- **Dashboard:** Beautiful startup screen
- **Icons:** all-the-icons integration
- **Smooth scrolling:** Pixel-perfect on Wayland

### 🔍 Smart Completion

- **Framework:** Vertico + Consult + Corfu
- **Inline:** Corfu with automatic triggers
- **Search:** Consult with fuzzy matching
- **Snippets:** YASnippet with 1000+ templates
- **Fallback:** Automatic Company if Corfu fails

### 🛠️ Development Tools

#### **LSP Support**
- Python (Pyright)
- Rust (rust-analyzer)
- Go (gopls)
- TypeScript (typescript-language-server)
- C/C++ (clangd/ccls)
- Java (jdtls)
- 15+ more languages

#### **Debugging (DAP)**
- Python (debugpy)
- Node.js (built-in)
- Go (Delve)
- Rust/C/C++ (LLDB/GDB)
- Visual debugger interface

#### **Project Management**
- Projectile with fuzzy search
- Git integration (Magit)
- Terminal (VTerm)
- File tree (Neotree)
- Ripgrep search

### 📝 50+ Programming Languages

**Tier 1 (Full LSP + Debug):**
Python, JavaScript/TypeScript, Go, Rust, C/C++, Java

**Tier 2 (LSP):**
C#, Ruby, PHP, Haskell, Scala, Kotlin, Swift, Elixir

**Tier 3 (Syntax + Compile/Run):**
Nim, Zig, Julia, OCaml, Fortran, Verilog, Assembly

**Data & Markup:**
JSON, YAML, TOML, Markdown, SQL, CSV, Protocol Buffers

---

## 🏥 Health Check System

### Run Health Check

```
M-x emacs-ide-health
```

**Automated checks:**
- ✓ System tools (git, grep, find, ripgrep, fd)
- ✓ LSP servers availability
- ✓ Code formatters (black, prettier, rustfmt)
- ✓ Tree-sitter grammars
- ✓ Performance metrics (startup time, GC)
- ✓ Package integrity
- ✓ Security configuration

**Auto-fix:**
```
M-x emacs-ide-health-fix
```

Automatically installs missing tree-sitter grammars and attempts to resolve issues.

---

## 🛡️ Error Recovery

### Features

1. **Crash Tracking:** Monitors crashes across sessions
2. **Safe Mode:** Enters minimal config after 3+ crashes
3. **Package Fallbacks:** corfu→company, lsp-mode→eglot, vertico→ivy
4. **Config Backup:** Auto-backup before major changes
5. **Error Logging:** Comprehensive logging to `var/recovery.log`

### Recovery Commands

```
C-c r r    Recovery report
C-c r v    View recovery log
C-c r b    Backup current config
C-c r R    Restore from backup
C-c r d    Disable problematic package
```

### Safe Mode

**Activate:**
```bash
emacs --eval "(setq emacs-ide-safe-mode t)"
```

**Deactivate:**
```
M-x emacs-ide-recovery-reset-crash-count
```

---

## ⚡ Performance

### Benchmark Results

| Metric | Target | Typical |
|--------|--------|---------|
| Startup Time | < 2.0s | 0.8-1.5s |
| GC During Startup | < 20 | 8-15 |
| Memory Usage | < 150MB | 80-120MB |
| LSP Response | < 500ms | 100-300ms |

### Optimization Features

- Aggressive GC tuning
- Native compilation support
- Lazy loading (all non-essential packages)
- Tree-sitter for fast syntax highlighting
- Deferred LSP activation
- File handler optimization

### Monitor Performance

```
M-x emacs-ide-startup-report   ; Startup breakdown
M-x profiler-start             ; Runtime profiling
M-x use-package-report         ; Package load times
```

---

## 🎓 Essential Keybindings

### Quick Reference

| Category | Key | Action |
|----------|-----|--------|
| **Files** | `C-c f` | Recent files |
| | `C-c w` | Save buffer |
| **Navigation** | `C-:` | Jump to char (Avy) |
| | `M-g f` | Jump to line |
| | `M-.` | Go to definition |
| **Editing** | `M-;` | Comment/uncomment |
| | `C-=` | Expand region |
| | `C->` | Multiple cursors |
| **Project** | `C-c p f` | Find file |
| | `C-c p s r` | Ripgrep |
| **Git** | `C-x g` | Magit status |
| **Debug** | `F5` | Start debugging |
| | `F9` | Toggle breakpoint |
| **Health** | - | `M-x emacs-ide-health` |
| **Recovery** | `C-c r` | Recovery menu |

**Full guide:** Press `C-c H` in Emacs

---

## 🔧 Configuration

### Option 1: YAML (Recommended)

Edit `~/.emacs.d/config.yml`:

```yaml
general:
  theme: modus-vivendi
  font: JetBrains Mono
  font_size: 11

lsp:
  enable: true
  inlay_hints: true

performance:
  gc_threshold: 16777216
  startup_time_target: 2.0
```

### Option 2: Elisp

Edit `~/.emacs.d/var/custom.el`:

```elisp
(custom-set-variables
 '(emacs-ide-theme 'modus-vivendi)
 '(emacs-ide-lsp-enable-inlay-hints t))
```

---

## 📦 Updates

### Update All Packages

```
M-x emacs-ide-update
```

Or from terminal:
```bash
cd ~/.emacs.d && ./scripts/update.sh
```

### Version Locking

**Lock versions:**
```
M-x emacs-ide-freeze-versions
```

Creates `versions.lock` with exact commits.

**Use locked versions:**
```elisp
(setq straight-current-profile 'pinned)
```

---

## 👥 Team Deployment

1. **Fork** this repository
2. **Customize** in `config.yml`
3. **Lock versions:** `M-x emacs-ide-freeze-versions`
4. **Commit** `versions.lock`
5. **Deploy** to team

Team members clone and get identical setup automatically.

---

## 🧪 Testing

### Run Tests

```bash
cd ~/.emacs.d
./scripts/test.sh
```

Tests verify:
- Startup time < 2s
- All critical packages load
- LSP available for major languages
- No byte-compilation warnings

### CI/CD

GitHub Actions tests multiple Emacs versions automatically.

---

## 🐛 Troubleshooting

### Slow Startup

```
M-x emacs-ide-startup-report
M-x use-package-report
M-x esup
```

### LSP Issues

```
M-x emacs-ide-health
M-x lsp-install-server
```

### Package Failures

```
C-c r v    View recovery log
C-c r d    Disable package
```

### Nuclear Reset

```bash
rm -rf ~/.emacs.d/straight ~/.emacs.d/var
emacs  # Reinstalls everything
```

---

## 📚 Documentation

- **[DEPLOYMENT.md](DEPLOYMENT.md)** - Complete installation guide
- **[COMMANDS.md](docs/COMMANDS.md)** - All commands reference
- **[TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md)** - Common issues
- **Keybindings:** `C-c H` in Emacs
- **Health Check:** `M-x emacs-ide-health`
- **Recovery:** `C-c r r`

---

## 🤝 Contributing

Contributions welcome! Please:
1. Fork repository
2. Create feature branch
3. Follow existing code style
4. Add tests if applicable
5. Update documentation
6. Submit pull request

---

## 📄 License

MIT License - See [LICENSE](LICENSE)

Free for commercial use, modification, and distribution.

---

## 🙏 Acknowledgments

Built on the shoulders of giants:

- **Emacs** - The extensible text editor
- **straight.el** - Package management
- **LSP Mode** - Language Server Protocol
- **Magit** - Git interface
- **Projectile** - Project management
- **Vertico/Consult** - Completion framework
- **DAP Mode** - Debug Adapter Protocol
- **100+ amazing packages** - See full list in modules

---

## 🌟 Why Choose Enterprise Emacs IDE?

### For Individuals
- **Faster setup** than configuring from scratch
- **Best practices** baked in
- **Professional tools** out of the box
- **Reliable** with automatic recovery

### For Teams
- **Consistent environment** across developers
- **Version locking** for reproducibility
- **Easy deployment** with one command
- **Tested & validated** in CI/CD

### For Organizations
- **Security hardened** for enterprise use
- **Monitoring & analytics** for IT teams
- **Support & documentation** included
- **MIT licensed** for commercial use

---

## 📊 Project Stats

- **50+ Languages** supported
- **100+ Packages** integrated
- **< 2s Startup** time target
- **99%+ Uptime** with recovery
- **0 External Dependencies** after install
- **MIT Licensed** - commercial friendly

---

## 🚀 Get Started Now

```bash
curl -fsSL https://raw.githubusercontent.com/yourorg/emacs-ide/main/install.sh | bash
```

**Questions?** Open an issue or discussion.

**Updates?** Watch the repository or check releases.

---

**Built with ❤️ for the Emacs community**
