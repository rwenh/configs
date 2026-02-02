# OpenSUSE Leap - Neovim IDE Prerequisites

## 🚀 Complete Setup (Copy-Paste Ready)

### Step 1: Install System Packages
```bash
# Core system packages
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip neovim nodejs npm python3 python3-pip python3-devel rust cargo go ripgrep fd lazygit tree-sitter ShellCheck ruby ruby-devel erlang elixir gnucobol ghdl gtkwave sqlite3 xclip xsel wl-clipboard
```

### Step 2: Install Language-Specific Tools
```bash
# Python packages
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv

# Node packages
npm install -g typescript ts-node tsx prettier eslint_d neovim

# Ruby gems
gem install solargraph rubocop debug

# Rust packages
cargo install stylua vhdl_ls
```

### Step 3: Install Nerd Font (for icons)
```bash
mkdir -p ~/.local/share/fonts && cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip FiraCode.zip && fc-cache -fv
cd ~
```

### Step 4: Clone & Setup Dotfiles
```bash
# Clone your dotfiles repository
git clone https://github.com/rwenh/configs.git ~/dotfiles
cd ~/dotfiles

# Backup existing configs (optional but recommended)
[ -f ~/.vimrc ] && mv ~/.vimrc ~/.vimrc.backup
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.backup

# Symlink configurations
ln -sf ~/dotfiles/.vimrc ~/.vimrc
ln -sf ~/dotfiles/nvim ~/.config/nvim
```

### Step 5: Start Neovim
```bash
# First launch - plugins will auto-install
nvim

# Inside Neovim, install Mason packages (run this command):
# :MasonInstallAll

# Check health
# :checkhealth
```

## 🎯 All-in-One Installation Script

Copy and paste this entire block:

```bash
#!/bin/bash
set -e

echo "🚀 Installing Neovim IDE prerequisites for openSUSE Leap 16.0..."

# System packages
echo "📦 Installing system packages..."
sudo zypper in -y gcc gcc-c++ make cmake ninja git curl wget unzip neovim nodejs npm \
  python3 python3-pip python3-devel rust cargo go ripgrep fd lazygit tree-sitter \
  ShellCheck ruby ruby-devel erlang elixir gnucobol ghdl gtkwave sqlite3 xclip xsel wl-clipboard

# Python packages
echo "🐍 Installing Python packages..."
pip3 install --user pynvim debugpy black isort ruff pytest ipython virtualenv

# Node packages
echo "📦 Installing Node packages..."
npm install -g typescript ts-node tsx prettier eslint_d neovim

# Ruby gems
echo "💎 Installing Ruby gems..."
gem install solargraph rubocop debug

# Rust packages
echo "🦀 Installing Rust packages..."
cargo install stylua vhdl_ls

# Nerd Font
echo "🔤 Installing Nerd Font..."
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip -q FiraCode.zip
rm FiraCode.zip
fc-cache -fv
cd ~

# Clone dotfiles
echo "📁 Cloning dotfiles..."
if [ -d ~/dotfiles ]; then
  echo "⚠️  ~/dotfiles already exists, skipping clone"
else
  git clone https://github.com/rwenh/configs.git ~/dotfiles
fi

# Backup existing configs
echo "💾 Backing up existing configs..."
[ -f ~/.vimrc ] && mv ~/.vimrc ~/.vimrc.backup.$(date +%Y%m%d_%H%M%S)
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config/nvim.backup.$(date +%Y%m%d_%H%M%S)

# Symlink configs
echo "🔗 Creating symlinks..."
cd ~/dotfiles
ln -sf ~/dotfiles/.vimrc ~/.vimrc
ln -sf ~/dotfiles/nvim ~/.config/nvim

echo "✅ Installation complete!"
echo ""
echo "📝 Next steps:"
echo "1. Start Neovim: nvim"
echo "2. Wait for plugins to install (automatic)"
echo "3. Run: :MasonInstallAll"
echo "4. Run: :checkhealth"
echo "5. Restart Neovim"
echo ""
echo "🎉 Enjoy your IDE!"
```

Save this as `install.sh`, make it executable, and run:

```bash
chmod +x install.sh
./install.sh
```

```bash
# Core development tools
sudo zypper in gcc gcc-c++ make cmake ninja git curl wget unzip

# Neovim (latest version recommended)
sudo zypper in neovim

# Node.js and npm (for LSP servers)
sudo zypper in nodejs npm

# Python development
sudo zypper in python3 python3-pip python3-devel

# Rust toolchain
sudo zypper in rust cargo

# Go
sudo zypper in go

# Java
sudo zypper in java-11-openjdk java-11-openjdk-devel

# C/C++ tools
sudo zypper in clang clang-tools llvm gdb valgrind

# Ripgrep (for telescope search)
sudo zypper in ripgrep

# fd (for telescope file finder)
sudo zypper in fd

# Git and lazygit
sudo zypper in git lazygit

# Tree-sitter CLI
sudo zypper in tree-sitter

# Shellcheck (bash linting)
sudo zypper in ShellCheck

# Ruby
sudo zypper in ruby ruby-devel

# Elixir/Erlang
sudo zypper in erlang elixir

# COBOL
sudo zypper in gnucobol

# VHDL tools
sudo zypper in ghdl gtkwave

# SQLite (for database plugins)
sudo zypper in sqlite3 sqlite3-devel

# Clipboard support
sudo zypper in xclip xsel wl-clipboard

# Fonts (for icons)
sudo zypper in google-noto-sans-fonts google-noto-serif-fonts
```

## 📦 Language-Specific Package Managers

### Python packages
```bash
# Core Python tools
pip3 install --user pynvim

# Python LSP and formatters
pip3 install --user debugpy black isort ruff pyright pytest ipython

# For venv-selector
pip3 install --user virtualenv
```

### Node.js packages
```bash
# TypeScript and formatters
npm install -g typescript ts-node tsx prettier eslint_d

# Additional LSP servers (Mason will install most)
npm install -g neovim
```

### Ruby gems
```bash
# Ruby development tools
gem install solargraph rubocop debug

# Rails (if needed)
gem install rails
```

### Cargo (Rust) packages
```bash
# Rust tools
cargo install stylua

# VHDL Language Server
cargo install vhdl_ls

# Alternative formatters
cargo install rustfmt
```

### Go tools
```bash
# Go formatters
go install mvdan.cc/sh/v3/cmd/shfmt@latest
go install github.com/segmentio/golines@latest
go install golang.org/x/tools/cmd/goimports@latest
```

## 🎯 Optional but Recommended

### Delta (better git diff)
```bash
# Download latest release
wget https://github.com/dandavison/delta/releases/download/0.17.0/git-delta-0.17.0-x86_64-unknown-linux-musl.tar.gz
tar -xzf git-delta-0.17.0-x86_64-unknown-linux-musl.tar.gz
sudo mv git-delta-0.17.0-x86_64-unknown-linux-musl/delta /usr/local/bin/
```

### Lua Language Server (if not via Mason)
```bash
# Download pre-built binary
wget https://github.com/LuaLS/lua-language-server/releases/download/3.7.4/lua-language-server-3.7.4-linux-x64.tar.gz
mkdir -p ~/.local/share/lua-language-server
tar -xzf lua-language-server-3.7.4-linux-x64.tar.gz -C ~/.local/share/lua-language-server
```

### Nerd Fonts
```bash
# Download and install a Nerd Font (for icons)
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/FiraCode.zip
unzip FiraCode.zip
fc-cache -fv
```

## 🔍 Verification Commands

```bash
# Check installations
nvim --version          # Should be 0.9.0+
node --version          # Should be 18+
npm --version
python3 --version       # Should be 3.8+
pip3 --version
cargo --version
go version
ruby --version
git --version
rg --version           # ripgrep
fd --version

# Check installed tools
which lazygit
which ghdl
which cobc
which rustc
which clang
which tree-sitter

# Python packages
pip3 list | grep -E "debugpy|black|isort|ruff"

# Ruby gems
gem list | grep -E "solargraph|rubocop"

# Cargo packages
cargo install --list | grep -E "stylua|vhdl_ls"
```

## 🚀 Post-Installation

After installing all prerequisites:

```bash
# 1. Start Neovim
nvim

# 2. Let Lazy install all plugins (automatic on first run)
# Wait for all plugins to install

# 3. Install Mason packages
:MasonInstallAll

# Or individually:
:MasonInstall lua-language-server pyright rust-analyzer
:MasonInstall typescript-language-server html-lsp css-lsp
:MasonInstall json-lsp yaml-language-server clangd gopls
:MasonInstall solargraph elixir-ls kotlin-language-server zls vhdl-ls
:MasonInstall debugpy codelldb delve js-debug-adapter
:MasonInstall black stylua prettier shfmt rubocop ktlint
:MasonInstall java-debug-adapter java-test

# 4. Check health
:checkhealth

# 5. Restart Neovim
:qa
nvim
```

## 📝 OpenSUSE-Specific Notes

### If Neovim is too old in default repos:
```bash
# Add OBS repository for latest Neovim
sudo zypper ar -f https://download.opensuse.org/repositories/editors/openSUSE_Leap_15.5/ editors
sudo zypper ref
sudo zypper in neovim
```

### For newer Node.js:
```bash
# Add NodeJS repository
sudo zypper ar https://rpm.nodesource.com/pub_20.x/nodistro/repo/nodesource-nodistro.repo
sudo zypper ref
sudo zypper in nodejs
```

### If package not found:
```bash
# Search for package
zypper search <package-name>

# Or use opi (OpenSUSE Package Installer)
sudo zypper in opi
opi <package-name>
```

## ✅ Minimal Essential List

If you want just the essentials:

```bash
# Absolute minimum
sudo zypper in neovim git gcc gcc-c++ make cmake \
  nodejs npm python3 python3-pip ripgrep fd \
  xclip rust cargo

# Python essentials
pip3 install --user pynvim debugpy black

# Node essentials
npm install -g neovim typescript prettier

# Rust essentials
cargo install stylua
```

## 🎨 Terminal Emulator Recommendations

For best experience, use one of these:

```bash
# Alacritty (GPU-accelerated)
sudo zypper in alacritty

# Kitty (feature-rich)
sudo zypper in kitty

# WezTerm (modern)
# Download from: https://wezfurlong.org/wezterm/install/linux.html
```

Then set a Nerd Font in your terminal config!

---

**Done!** Your OpenSUSE Leap system is ready for the ultimate Neovim IDE experience! 🚀
