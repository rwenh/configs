# Enhanced Emacs IDE - Installation & Setup Guide

A complete, production-ready IDE configuration supporting 50+ programming languages with LSP, Tree-sitter, debugging, and advanced features.

## Quick Start

```bash
# Backup existing config
mv ~/.emacs.d ~/.emacs.d.backup

# Create directory structure
mkdir -p ~/.emacs.d/modules

# Place all configuration files
# - early-init.el → ~/.emacs.d/early-init.el
# - init.el → ~/.emacs.d/init.el
# - All module files → ~/.emacs.d/modules/

# Start Emacs (packages will auto-install)
emacs
```

## File Structure

```
~/.emacs.d/
├── early-init.el          # Performance optimizations (loaded first)
├── init.el                # Core configuration & module loader
├── modules/
│   ├── ui-config.el       # Themes, fonts, visual elements
│   ├── completion-config.el # Company, yasnippet, IDO
│   ├── editing-config.el  # Custom functions, editing enhancements
│   ├── tools-config.el    # LSP, projectile, magit, vterm
│   ├── lang-config.el     # Language configurations
│   ├── debug-config.el    # DAP debugging support
│   └── keybindings.el     # Global keybindings
└── custom.el              # Auto-generated customizations
```

## System Requirements

### Essential
- **Emacs 29+** (with tree-sitter support recommended)
- **Git** - Version control
- **GCC/G++** - C/C++ compilation
- **Python 3.8+** - For scripts and debugging

### Build Emacs with Tree-sitter (Ubuntu/Debian)

```bash
# Install dependencies
sudo apt install -y build-essential libgtk-3-dev libgnutls28-dev \
    libtree-sitter-dev libjansson-dev libncurses5-dev texinfo \
    libgccjit-10-dev

# Clone Emacs
git clone --depth 1 --branch emacs-29 https://github.com/emacs-mirror/emacs.git
cd emacs

# Configure with tree-sitter and native compilation
./autogen.sh
./configure --with-tree-sitter --with-native-compilation \
    --with-json --with-modules --with-pgtk

# Build and install
make -j$(nproc)
sudo make install
```

## Language Server Installation

### Essential LSP Servers

```bash
# Python
pip3 install python-lsp-server pyright debugpy black flake8 mypy

# JavaScript/TypeScript
npm install -g typescript typescript-language-server \
    prettier eslint

# Rust
rustup component add rust-analyzer rustfmt clippy

# Go
go install golang.org/x/tools/gopls@latest
go install github.com/go-delve/delve/cmd/dlv@latest

# C/C++
# Option 1: clangd
sudo apt install clangd-14
# Option 2: ccls
sudo apt install ccls

# Java
# Install Eclipse JDT Language Server via lsp-java (automatic)

# Bash
npm install -g bash-language-server

# Docker
npm install -g dockerfile-language-server-nodejs

# YAML
npm install -g yaml-language-server

# HTML/CSS
npm install -g vscode-langservers-extracted

# JSON
npm install -g vscode-json-languageserver
```

### Additional Language Tools

```bash
# Haskell
ghcup install hls

# Ruby
gem install solargraph

# Lua
luarocks install lua-lsp

# PHP
composer global require felixfbecker/language-server

# Elixir
mix archive.install github elixir-lsp/elixir-ls

# Terraform
brew install terraform-ls  # or download from releases
```

## Tree-sitter Grammars

Emacs will prompt to install tree-sitter grammars automatically. To install manually:

```elisp
;; In Emacs:
M-x treesit-install-language-grammar

;; Then enter the language name when prompted
;; Common languages: c, cpp, python, rust, go, javascript, typescript, etc.
```

## Debugging Setup

### Python
```bash
pip3 install debugpy pytest
```

### Node.js
```bash
npm install -g node-inspect
```

### Go
```bash
go install github.com/go-delve/delve/cmd/dlv@latest
```

### C/C++/Rust
```bash
# Install LLDB or GDB
sudo apt install lldb gdb
```

## Formatters

```bash
# Python
pip3 install black

# JavaScript/TypeScript
npm install -g prettier

# Rust
rustup component add rustfmt

# Go
# gofmt is included with Go

# C/C++
sudo apt install clang-format

# SQL
pip3 install sqlparse

# Shell
sudo apt install shfmt

# Haskell
cabal install ormolu

# OCaml
opam install ocamlformat

# Zig
# zig fmt is included with Zig
```

## Docker & Kubernetes (Optional)

```bash
# Docker
sudo apt install docker.io
sudo usermod -aG docker $USER

# Kubernetes
sudo apt install kubectl
```

## Wayland-specific Tools (Optional)

```bash
sudo apt install wl-clipboard grim slurp
```

## Verification

After installation, run these commands in Emacs:

```elisp
;; Check tool availability
M-x emacs-ide-check-tools

;; Check LSP status (open a source file first)
M-x emacs-ide-lsp-status

;; View startup time
M-x emacs-ide-show-startup-time

;; Show keybindings help
M-x emacs-ide-show-common-keybindings

;; Debug adapter installation guide
M-x emacs-ide-install-debug-adapters
```

## Supported Languages

### Tier 1 (Full LSP + Debugging + Formatting)
- Python, JavaScript/TypeScript, Rust, Go, C/C++, Java

### Tier 2 (LSP + Formatting)
- C#, Haskell, Ruby, PHP, Kotlin, Scala, Swift, Elixir

### Tier 3 (Syntax + Basic Features)
- Lua, Perl, Shell, Fortran, Verilog, VHDL, Assembly
- Julia, Nim, Zig, OCaml, Erlang, Ada, CMake
- SQL, JSON, YAML, TOML, Dockerfile, Terraform

## Configuration Customization

### Change Theme
```bash
export EMACS_THEME=light  # or dark (default)
```

Or toggle with `F12` inside Emacs.

### Custom Snippets
Create snippets in `~/.emacs.d/snippets/<mode-name>/`

### Project Search Paths
Edit in `init.el`:
```elisp
(setq projectile-project-search-path '("~/projects" "~/work" "~/code"))
```

### Font Configuration
Edit `ui-config.el` to change preferred fonts.

## Troubleshooting

### LSP Not Starting
1. Check if language server is installed: `M-x emacs-ide-check-tools`
2. Verify file is in a project: `M-x projectile-project-root`
3. Check LSP logs: `M-x lsp-describe-session`

### Tree-sitter Missing Grammars
```elisp
M-x treesit-install-language-grammar
```

### Slow Startup
```elisp
;; Profile startup
M-x emacs-ide-profile-startup
```

### Package Installation Failures
```elisp
;; Refresh and retry
M-x package-refresh-contents
M-x package-install RET <package-name>
```

## Performance Optimization

The configuration is already optimized for performance:
- Garbage collection tuned for startup and runtime
- Native compilation enabled
- Deferred package loading
- File watchers limited
- Large file optimizations

Expected startup time: 2-4 seconds with all packages installed.

## Keybinding Philosophy

- `C-c` prefix: Custom commands
- `C-c l` prefix: LSP commands
- `C-c p` prefix: Project commands (Projectile)
- Function keys: Compilation and debugging
- `M-o`: Window navigation (Ace Window)
- `M-.`: Go to definition (standard)
- `M-,`: Pop back (standard)

## Getting Help

- `C-c ?`: Show all available keybindings
- `C-c H`: Show common keybindings cheat sheet
- `C-h f`: Describe function (Helpful)
- `C-h v`: Describe variable (Helpful)
- `C-h k`: Describe key (Helpful)

## Next Steps

1. Open a project: `C-c p p`
2. Find files: `C-c p f`
3. Start coding with LSP autocompletion
4. Set breakpoints and debug: `C-<f9>`, then `F5`
5. Manage Git with Magit: `C-x g`

## License

This configuration is released under the MIT License.

## Contributing

Feel free to customize and extend this configuration for your needs.