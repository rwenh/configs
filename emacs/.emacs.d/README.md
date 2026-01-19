# Emacs IDE - Professional Development Environment

> **A hyper-optimized, full-featured IDE built on Emacs with sub-second startup time**

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Emacs](https://img.shields.io/badge/Emacs-29.1+-purple.svg)
![Languages](https://img.shields.io/badge/languages-50+-green.svg)

## 🚀 Features

### Core Capabilities
- ✅ **50+ Programming Languages** with LSP support
- ✅ **Professional Debugging** (DAP for Python, Node, Go, Rust, C/C++, Java, etc.)
- ✅ **Modern Completion** (Vertico, Consult, Corfu, Company)
- ✅ **Advanced Editing** (Multiple cursors, expand-region, smart navigation)
- ✅ **Git Integration** (Magit, git-gutter, time-machine)
- ✅ **Project Management** (Projectile with ripgrep/grep)
- ✅ **Terminal Integration** (VTerm with multi-terminal support)
- ✅ **Wayland Native** (Full clipboard, screenshot, optimization support)

### Performance
- **Sub-second startup** (~0.5-1.5s with 100+ packages)
- Native compilation support
- Aggressive GC optimization
- Lazy loading for all non-essential packages
- Tree-sitter integration for fast syntax highlighting

### User Experience
- Beautiful UI (Modus themes, Doom modeline)
- Ergonomic keybindings with help system
- Context-aware completion (Company + Corfu + Cape)
- Visual enhancements (rainbow delimiters, indent guides, beacon)
- Professional debugging interface

---

## 📦 Installation

### Prerequisites

**Required:**
- Emacs 29.1+ (with native compilation recommended)
- Git

**Recommended External Tools:**
```bash
# Search tools (for projectile/consult)
sudo apt install ripgrep fd-find  # Debian/Ubuntu
brew install ripgrep fd           # macOS

# Language servers (install as needed)
npm install -g typescript-language-server
pip install python-lsp-server
go install golang.org/x/tools/gopls@latest
cargo install rust-analyzer

# Debuggers
pip install debugpy                                    # Python
go install github.com/go-delve/delve/cmd/dlv@latest  # Go
# node (built-in debugger)
# lldb/gdb (system package)

# Formatters
pip install black isort flake8 mypy
npm install -g prettier
cargo install rustfmt
```

### Step 1: Backup Current Config
```bash
mv ~/.emacs.d ~/.emacs.d.backup
mv ~/.emacs ~/.emacs.backup 2>/dev/null || true
```

### Step 2: Clone Repository
```bash
git clone https://github.com/yourusername/emacs-ide.git ~/.emacs.d
```

### Step 3: Install Fonts
**JetBrains Mono** (recommended):
```bash
# Debian/Ubuntu
sudo apt install fonts-jetbrains-mono

# macOS
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono

# Manual install
wget https://download.jetbrains.com/fonts/JetBrainsMono-2.304.zip
unzip JetBrainsMono-2.304.zip -d ~/.local/share/fonts/
fc-cache -fv
```

**Alternative fonts:** Fira Code, Cascadia Code, Iosevka, Source Code Pro

### Step 4: First Launch
```bash
emacs
```

On first launch:
1. straight.el will bootstrap automatically
2. Packages will be installed (takes 2-5 minutes)
3. Native compilation will occur in background
4. Restart Emacs after initial setup completes

---

## 📁 Directory Structure

```
~/.emacs.d/
├── early-init.el          # Pre-initialization optimizations
├── init.el                # Main configuration entry point
├── custom.el              # Auto-generated customizations
├── modules/               # Modular configuration files
│   ├── ui-config.el       # UI/appearance settings
│   ├── completion-config.el # Completion framework
│   ├── editing-config.el  # Text editing features
│   ├── tools-config.el    # LSP, Flycheck, Git, etc.
│   ├── lang-config.el     # Language-specific configs
│   ├── debug-config.el    # Debugging setup (DAP)
│   └── keybindings.el     # All keybindings
├── snippets/              # YASnippet templates
├── straight/              # Package manager (auto-created)
├── var/                   # Cache, backups (auto-created)
└── undo-tree-hist/        # Undo history (auto-created)
```

---

## ⌨️ Essential Keybindings

### Quick Reference (Press `C-c H` for full help)

| Category | Key | Action |
|----------|-----|--------|
| **Files** | `C-c f` | Recent files |
| | `C-c w` | Save buffer |
| | `C-c q` | Kill buffer & window |
| **Navigation** | `C-:` | Jump to char (Avy) |
| | `M-g f` | Jump to line |
| | `M-s l` | Search in buffer (Consult) |
| | `M-.` | Go to definition (LSP) |
| | `M-,` | Pop back |
| **Editing** | `M-;` | Comment/uncomment |
| | `C-=` | Expand region |
| | `C->` | Multiple cursors (next) |
| | `M-↑/↓` | Move line up/down |
| **Project** | `C-c p f` | Find file in project |
| | `C-c p p` | Switch project |
| | `C-c p s r` | Ripgrep in project |
| **Git** | `C-x g` | Magit status |
| | `C-c g b` | Git blame |
| | `C-x v t` | Git time machine |
| **Debug** | `F5` | Start debugging |
| | `F7/S-F7/M-F7` | Step in/over/out |
| | `F9` | Toggle breakpoint |
| | `C-c d h` | Debug hydra |
| **LSP** | `C-c l r` | Rename symbol |
| | `C-c l f` | Format buffer |
| | `C-c l a` | Code actions |
| **Windows** | `M-o` | Ace window |
| | `C-x 2/3` | Split horizontal/vertical |
| **Utility** | `F8` | Toggle file tree |
| | `C-c t` | Terminal (VTerm) |
| | `F12` | Toggle theme |

---

## 🔧 Configuration

### Change Theme
```elisp
;; In ui-config.el, modify:
(load-theme 'modus-vivendi t)  ; Dark theme
;; or
(load-theme 'modus-operandi t) ; Light theme
```

### Add Language Support
Language configurations are in `modules/lang-config.el`. Example:
```elisp
(use-package python
  :mode "\\.py\\'"
  :config
  (add-hook 'python-mode-hook #'lsp-deferred))
```

### Custom Keybindings
Add to `modules/keybindings.el`:
```elisp
(global-set-key (kbd "C-c x") 'your-custom-function)
```

### Performance Tuning
Adjust in `early-init.el`:
```elisp
(setq gc-cons-threshold (* 16 1024 1024))  ; Increase for more RAM
(setq native-comp-async-jobs-number 8)    ; CPU cores for compilation
```

---

## 🌍 Wayland Support

Full native Wayland support included:

**Clipboard Integration:**
- `C-c C-w` - Copy to Wayland clipboard (wl-copy)
- `C-c C-y` - Paste from Wayland clipboard (wl-paste)

**Screenshot:**
- `C-c s` - Take screenshot (Sway/grim/slurp)

**Optimizations:**
- Pixel-perfect scrolling
- Native clipboard handling
- Frame optimization for compositors

---

## 🐛 Debugging Setup

### Python
```bash
pip install debugpy
```
Press `F5` in Python file → Select "Python :: Run File"

### Node.js
Built-in Node debugger (no installation needed)
Press `F5` in .js file → Select "Node :: Run File"

### Go
```bash
go install github.com/go-delve/delve/cmd/dlv@latest
```
Press `F5` in .go file → Select "Go :: Run File"

### C/C++/Rust
Uses LLDB or GDB (system package):
```bash
sudo apt install lldb gdb  # Debian/Ubuntu
brew install lldb          # macOS
```

### Check Debug Tools
Press `C-c d ?` to see debug help and check tool availability

---

## 📚 Language Support

### Tier 1 (Full LSP + Debug)
Python, JavaScript/TypeScript, Go, Rust, C/C++, Java

### Tier 2 (LSP Support)
C#, Ruby, PHP, Haskell, Scala, Kotlin, Swift, Elixir, Lua

### Tier 3 (Syntax + Compile/Run)
Nim, Zig, Julia, OCaml, Fortran, Verilog, VHDL, Assembly

### Markup & Data
JSON, YAML, TOML, Markdown, Org, SQL, CSV, Protocol Buffers, GraphQL

### Full list in `modules/lang-config.el`

---

## 🎯 Usage Patterns

### Quick Code Workflow
1. `C-c p f` - Open project file
2. Edit with LSP completion
3. `C-c l f` - Format code
4. `F5` - Debug or `C-c C-c` - Run
5. `C-x g` - Commit with Magit

### Debugging Workflow
1. Set breakpoint with `F9`
2. Start debugging with `F5`
3. Step through code (`F7`/`S-F7`/`M-F7`)
4. Inspect variables in sidebars
5. `C-F7` to continue

### Project Search
1. `C-c p s r` - Ripgrep search
2. Navigate results with `C-n/C-p`
3. `RET` to jump to match
4. `M-.` for definition lookup

---

## 🚑 Troubleshooting

### Slow Startup
```elisp
;; Check startup time breakdown
M-x esup

;; Profile current session
C-c D s  ; Start profiler
; ... do slow operation ...
C-c D r  ; View report
```

### LSP Not Working
```elisp
;; Check LSP status
C-c L

;; Restart workspace
C-c l w

;; Check server installation
M-x lsp-install-server
```

### Missing Icons
```bash
# Install all-the-icons fonts
M-x all-the-icons-install-fonts
```

### Native Compilation Warnings
Already suppressed, but can check:
```elisp
(setq native-comp-async-report-warnings-errors 'silent)
```

---

## 🔄 Updating

```bash
cd ~/.emacs.d
git pull origin main

# Update packages
emacs --batch -l init.el -f straight-pull-all
```

Or inside Emacs:
```
M-x straight-pull-all
M-x straight-rebuild-all
```

---

## 🎨 Customization Examples

### Add Custom Snippets
```bash
mkdir -p ~/.emacs.d/snippets/python-mode
```

Create `~/.emacs.d/snippets/python-mode/main`:
```
# -*- mode: snippet -*-
# name: main
# key: main
# --
if __name__ == "__main__":
    ${1:main()}
```

### Custom LSP Settings
```elisp
;; In modules/tools-config.el
(with-eval-after-load 'lsp-mode
  (setq lsp-python-ms-python-executable "python3.11"))
```

---

## 📖 Learning Resources

- **Emacs Tutorial:** `C-h t`
- **Function Help:** `C-h f function-name`
- **Variable Help:** `C-h v variable-name`
- **Keybinding Help:** `C-h k` then press key
- **This Config Help:** `C-c H`
- **Which-Key:** `C-c ?` (shows available commands)

---

## 🤝 Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create feature branch
3. Follow existing code style
4. Test thoroughly
5. Submit pull request

---

## 📄 License

MIT License - See LICENSE file

---

## 🙏 Acknowledgments

Built with these amazing packages:
- **straight.el** - Package management
- **LSP Mode** - Language Server Protocol
- **Magit** - Git interface
- **Projectile** - Project management
- **Vertico/Consult** - Completion framework
- **DAP Mode** - Debug Adapter Protocol
- And 100+ other fantastic packages

---
