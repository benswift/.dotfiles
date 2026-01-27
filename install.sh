#!/usr/bin/env bash
#
# Bootstrap dotfiles on a new machine
# Usage: curl -fsSL https://raw.githubusercontent.com/benswift/.dotfiles/main/install.sh | bash
#
set -euo pipefail

DOTFILES_REPO="${DOTFILES_REPO:-https://github.com/benswift/.dotfiles.git}"
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info() { echo -e "${GREEN}[info]${NC} $1"; }
warn() { echo -e "${YELLOW}[warn]${NC} $1"; }
error() { echo -e "${RED}[error]${NC} $1" >&2; }

command_exists() { command -v "$1" &>/dev/null; }

detect_platform() {
    case "$(uname -s)" in
        Darwin) echo "macos" ;;
        Linux)  echo "linux" ;;
        *)      echo "unknown" ;;
    esac
}

install_homebrew() {
    if command_exists brew; then
        info "Homebrew already installed"
        return
    fi

    info "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Set up brew for this session
    if [[ -f /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -f /usr/local/bin/brew ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    elif [[ -f /home/linuxbrew/.linuxbrew/bin/brew ]]; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    fi
}

install_mise() {
    if command_exists mise; then
        info "mise already installed"
        return
    fi

    info "Installing mise..."
    curl https://mise.run | sh

    # Add mise to path for this session
    export PATH="$HOME/.local/bin:$PATH"

    if ! command_exists mise; then
        error "mise installation failed"
        exit 1
    fi
}

clone_dotfiles() {
    if [[ -d "$DOTFILES_DIR" ]]; then
        info "Dotfiles already exist at $DOTFILES_DIR"
        info "Pulling latest changes..."
        git -C "$DOTFILES_DIR" pull --rebase || warn "Could not pull latest (maybe local changes?)"
    else
        info "Cloning dotfiles to $DOTFILES_DIR..."
        git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    fi
}

setup_symlinks() {
    info "Setting up symlinks..."
    "$DOTFILES_DIR/create_symlinks.sh"
}

install_mise_tools() {
    info "Installing mise-managed tools..."
    eval "$(mise activate bash)"
    mise install --yes
}

main() {
    echo ""
    echo "╔═══════════════════════════════════════╗"
    echo "║       Dotfiles Bootstrap Script       ║"
    echo "╚═══════════════════════════════════════╝"
    echo ""

    local platform
    platform=$(detect_platform)
    info "Detected platform: $platform"

    if [[ "$platform" == "unknown" ]]; then
        error "Unsupported platform: $(uname -s)"
        exit 1
    fi

    # Check for required tools
    if ! command_exists git; then
        error "git is required but not installed"
        exit 1
    fi

    if ! command_exists curl; then
        error "curl is required but not installed"
        exit 1
    fi

    install_homebrew
    install_mise
    clone_dotfiles
    setup_symlinks
    install_mise_tools

    echo ""
    info "Bootstrap complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Restart your shell or run: source ~/.zshrc"
    echo "  2. Run 'dotfiles doctor' to verify setup"
    echo "  3. Install additional tools as needed with 'brew install' or 'mise use'"
    echo ""
}

main "$@"
