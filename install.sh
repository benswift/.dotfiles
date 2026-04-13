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

install_claude() {
    if command_exists claude; then
        info "Claude Code already installed"
        return
    fi

    info "Installing Claude Code..."
    curl -fsSL https://claude.ai/install.sh | bash
}

install_agent_skills() {
    mkdir -p "$HOME/.agents"
    ln -sfn "$HOME/.agents" "$DOTFILES_DIR/.agents"

    info "Installing agent-browser skill..."
    bunx skills add vercel-labs/agent-browser
}

# Clone (or pull) the private ben Claude Code plugin. Non-fatal: on a fresh
# machine without SSH set up, this warns and lets the rest of bootstrap
# continue. Re-run 'dotfiles update' once SSH auth works.
install_personal_plugin() {
    local plugin_repo="git@github.com:benswift/claude-plugin-personal.git"
    local plugin_dir="$DOTFILES_DIR/claude-plugins/ben"

    if [[ -d "$plugin_dir/.git" ]]; then
        info "Updating ben plugin..."
        git -C "$plugin_dir" pull --rebase --autostash || warn "Could not pull ben plugin (offline?)"
    else
        info "Cloning ben plugin..."
        mkdir -p "$(dirname "$plugin_dir")"
        if ! git clone "$plugin_repo" "$plugin_dir"; then
            warn "Could not clone ben plugin --- check SSH auth to GitHub"
            warn "Re-run 'dotfiles update' after setting up SSH"
        fi
    fi

    # Claude Code clones its own copy of the ben plugin separately from the
    # dev checkout above (the github marketplace source handles this). Trigger
    # install here so the plugin is ready on first claude launch.
    if command -v claude &>/dev/null; then
        claude plugin install ben@ben 2>/dev/null || true
    fi
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

    if [[ "$platform" == "macos" ]]; then
        install_homebrew
    fi
    install_mise
    clone_dotfiles
    install_personal_plugin
    setup_symlinks
    install_mise_tools
    install_claude
    install_agent_skills

    echo ""
    info "Bootstrap complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Restart your shell or run: source ~/.zshrc"
    echo "  2. Run 'dotfiles doctor' to verify setup"
    if [[ "$platform" == "macos" ]]; then
        echo "  3. Install additional tools as needed with 'brew install' or 'mise use'"
    else
        echo "  3. Install additional tools as needed with your package manager or 'mise use'"
    fi
    echo ""
}

main "$@"
