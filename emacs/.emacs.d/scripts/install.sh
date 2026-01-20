#!/usr/bin/env bash
# Enterprise Emacs IDE - Production Installation Script
# Version: 2.0.0

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
EMACS_MIN_VERSION="29.1"
EMACS_DIR="${HOME}/.emacs.d"
BACKUP_DIR="${HOME}/.emacs.d.backup-$(date +%Y%m%d-%H%M%S)"
REPO_URL="https://github.com/yourusername/emacs-ide-enterprise.git"

# Logging
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Version comparison
version_ge() {
    # Returns 0 if $1 >= $2
    [ "$(printf '%s\n' "$1" "$2" | sort -V | head -n1)" = "$2" ]
}

# Detect OS
detect_os() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
        echo "windows"
    else
        echo "unknown"
    fi
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    # Check Emacs
    if ! command_exists emacs; then
        log_error "Emacs not found. Please install Emacs ${EMACS_MIN_VERSION} or higher."
        exit 1
    fi
    
    # Check Emacs version
    EMACS_VERSION=$(emacs --version | head -n1 | awk '{print $3}')
    if ! version_ge "$EMACS_VERSION" "$EMACS_MIN_VERSION"; then
        log_error "Emacs ${EMACS_VERSION} found, but ${EMACS_MIN_VERSION}+ required."
        exit 1
    fi
    log_success "Emacs ${EMACS_VERSION} found"
    
    # Check Git
    if ! command_exists git; then
        log_error "Git not found. Please install Git."
        exit 1
    fi
    log_success "Git found"
    
    # Check optional tools
    local missing_tools=()
    
    if ! command_exists rg; then
        missing_tools+=("ripgrep")
    fi
    
    if ! command_exists fd; then
        missing_tools+=("fd-find")
    fi
    
    if [ ${#missing_tools[@]} -gt 0 ]; then
        log_warning "Recommended tools missing: ${missing_tools[*]}"
        log_info "Install for better experience (optional)"
    fi
}

# Backup existing configuration
backup_existing_config() {
    if [ -d "$EMACS_DIR" ]; then
        log_info "Backing up existing configuration to ${BACKUP_DIR}"
        mv "$EMACS_DIR" "$BACKUP_DIR"
        log_success "Backup created at ${BACKUP_DIR}"
    fi
    
    if [ -f "${HOME}/.emacs" ]; then
        log_info "Backing up ${HOME}/.emacs"
        mv "${HOME}/.emacs" "${HOME}/.emacs.backup-$(date +%Y%m%d)"
    fi
}

# Clone repository
clone_repository() {
    log_info "Cloning Enterprise Emacs IDE repository..."
    
    if git clone "$REPO_URL" "$EMACS_DIR"; then
        log_success "Repository cloned successfully"
    else
        log_error "Failed to clone repository"
        exit 1
    fi
}

# Create directory structure
create_directories() {
    log_info "Creating directory structure..."
    
    local dirs=(
        "${EMACS_DIR}/var"
        "${EMACS_DIR}/var/cache"
        "${EMACS_DIR}/var/backups"
        "${EMACS_DIR}/var/eln-cache"
        "${EMACS_DIR}/var/tree-sitter"
        "${EMACS_DIR}/snippets"
    )
    
    for dir in "${dirs[@]}"; do
        mkdir -p "$dir"
    done
    
    log_success "Directory structure created"
}

# Install LSP servers
install_lsp_servers() {
    log_info "Installing LSP servers..."
    
    local os=$(detect_os)
    
    # Check for package managers
    if command_exists npm; then
        log_info "Installing Node LSP servers..."
        npm install -g typescript-language-server \
                      bash-language-server \
                      vscode-langservers-extracted || log_warning "Some npm packages failed"
    else
        log_warning "npm not found. Skipping Node LSP servers."
    fi
    
    if command_exists pip3 || command_exists pip; then
        log_info "Installing Python LSP servers..."
        (pip3 install python-lsp-server || pip install python-lsp-server) || log_warning "Failed to install pylsp"
    else
        log_warning "pip not found. Skipping Python LSP servers."
    fi
    
    if command_exists go; then
        log_info "Installing Go LSP server..."
        go install golang.org/x/tools/gopls@latest || log_warning "Failed to install gopls"
    fi
    
    if command_exists rustup; then
        log_info "Installing Rust analyzer..."
        rustup component add rust-analyzer || log_warning "Failed to install rust-analyzer"
    fi
    
    log_success "LSP server installation complete (check warnings for missing servers)"
}

# Install formatters
install_formatters() {
    log_info "Installing code formatters..."
    
    if command_exists pip3 || command_exists pip; then
        (pip3 install black isort flake8 || pip install black isort flake8) || log_warning "Failed to install Python formatters"
    fi
    
    if command_exists npm; then
        npm install -g prettier || log_warning "Failed to install prettier"
    fi
    
    if command_exists rustup; then
        rustup component add rustfmt || log_warning "Failed to install rustfmt"
    fi
    
    log_success "Formatter installation complete"
}

# Install fonts
install_fonts() {
    log_info "Checking fonts..."
    
    local os=$(detect_os)
    
    if [[ "$os" == "linux" ]]; then
        if command_exists fc-list; then
            if fc-list | grep -qi "JetBrains Mono"; then
                log_success "JetBrains Mono font already installed"
            else
                log_warning "JetBrains Mono font not found"
                log_info "Install with: sudo apt install fonts-jetbrains-mono (Debian/Ubuntu)"
            fi
        fi
    elif [[ "$os" == "macos" ]]; then
        if command_exists brew; then
            if brew list --cask | grep -q font-jetbrains-mono; then
                log_success "JetBrains Mono font already installed"
            else
                log_info "Installing JetBrains Mono font..."
                brew tap homebrew/cask-fonts
                brew install --cask font-jetbrains-mono || log_warning "Failed to install font"
            fi
        fi
    fi
}

# First launch setup
first_launch() {
    log_info "Performing first launch setup..."
    
    cat > /tmp/emacs-first-launch.el <<'EOF'
(message "Enterprise Emacs IDE - First Launch Setup")
(message "Installing packages... This may take several minutes.")

;; Kill Emacs after setup
(run-with-idle-timer
 10 nil
 (lambda ()
   (when (yes-or-no-p "Initial setup complete. Exit Emacs? ")
     (save-buffers-kill-terminal))))
EOF
    
    log_info "Launching Emacs for initial setup..."
    emacs --batch -l /tmp/emacs-first-launch.el 2>&1 | grep -v "^Loading" || true
    
    rm -f /tmp/emacs-first-launch.el
    
    log_success "First launch setup complete"
}

# Create default configuration
create_default_config() {
    local config_file="${EMACS_DIR}/config.yml"
    
    if [ ! -f "$config_file" ]; then
        log_info "Creating default configuration..."
        
        cat > "$config_file" <<'EOF'
# Enterprise Emacs IDE Configuration
# Edit this file to customize your setup

general:
  theme: modus-vivendi
  font: JetBrains Mono
  font_size: 11
  safe_mode: false

completion:
  backend: corfu
  delay: 0.1

lsp:
  enable: true
  inlay_hints: true
  large_file_threshold: 100000

performance:
  gc_threshold: 16777216
  startup_time_target: 2.0

features:
  dashboard: true
  which_key: true

security:
  tls_verify: true

telemetry:
  enabled: true
EOF
        
        log_success "Default configuration created at ${config_file}"
    fi
}

# Print next steps
print_next_steps() {
    echo ""
    log_success "====================================="
    log_success "  Installation Complete! 🚀"
    log_success "====================================="
    echo ""
    echo "Next steps:"
    echo ""
    echo "  1. Launch Emacs:"
    echo "     $ emacs"
    echo ""
    echo "  2. Run health check:"
    echo "     M-x emacs-ide-health"
    echo ""
    echo "  3. View keybindings:"
    echo "     C-c H"
    echo ""
    echo "  4. Customize configuration:"
    echo "     Edit ~/.emacs.d/config.yml"
    echo ""
    echo "Documentation:"
    echo "  - Deployment Guide: ~/.emacs.d/DEPLOYMENT.md"
    echo "  - Recovery System: C-c r r"
    echo "  - Version Info: M-x emacs-ide-show-version"
    echo ""
    
    if [ -d "$BACKUP_DIR" ]; then
        echo "Your previous configuration was backed up to:"
        echo "  $BACKUP_DIR"
        echo ""
    fi
}

# Rollback on failure
rollback() {
    log_error "Installation failed. Rolling back..."
    
    if [ -d "$EMACS_DIR" ]; then
        rm -rf "$EMACS_DIR"
    fi
    
    if [ -d "$BACKUP_DIR" ]; then
        mv "$BACKUP_DIR" "$EMACS_DIR"
        log_success "Restored previous configuration"
    fi
    
    exit 1
}

# Main installation
main() {
    echo ""
    echo "╔════════════════════════════════════════════╗"
    echo "║  Enterprise Emacs IDE Installer v2.0.0    ║"
    echo "║  Production-Grade Development Environment ║"
    echo "╚════════════════════════════════════════════╝"
    echo ""
    
    # Set trap for errors
    trap rollback ERR
    
    # Run installation steps
    check_prerequisites
    backup_existing_config
    clone_repository
    create_directories
    create_default_config
    
    # Optional installations
    read -p "Install LSP servers? (recommended) [Y/n] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        install_lsp_servers
    fi
    
    read -p "Install code formatters? (recommended) [Y/n] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        install_formatters
    fi
    
    install_fonts
    
    # Print completion message
    print_next_steps
    
    # Offer to launch
    read -p "Launch Emacs now? [Y/n] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        log_info "Launching Emacs..."
        emacs &
    fi
}

# Run main function
main "$@"
