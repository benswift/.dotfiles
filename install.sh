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

# Claude Code plugins declared in claude/settings.json. Each entry is
# "github-repo plugin-id". The marketplace declaration in settings.json is
# necessary but not sufficient --- claude plugin subcommands don't auto-register
# from it, so we run add/install here. Non-fatal: on a fresh machine without
# SSH set up, these warn and let the rest of bootstrap continue. Re-run
# 'dotfiles update' once SSH auth works.
CLAUDE_PLUGINS=(
    "benswift/claude-plugin-personal ben@ben"
    "pbakaus/impeccable impeccable@impeccable"
)

bootstrap_claude_plugins() {
    if ! command -v claude &>/dev/null; then
        warn "claude not installed --- skipping claude plugin bootstrap"
        warn "Re-run 'dotfiles update' after installing claude"
        return
    fi

    info "Setting up claude plugins..."
    local entry repo plugin_id
    for entry in "${CLAUDE_PLUGINS[@]}"; do
        read -r repo plugin_id <<<"$entry"
        claude plugin marketplace add "$repo" 2>/dev/null \
            || warn "Could not add marketplace $repo --- check SSH auth to GitHub"
        claude plugin install --scope user "$plugin_id" 2>/dev/null \
            || warn "Could not install $plugin_id (marketplace may not be available yet)"
    done

    # ben-plugin-specific: point codex at claude's marketplace clone so both
    # tools read the same directory. Doing it here (not in create_symlinks.sh)
    # because the target doesn't exist until the marketplace has been cloned.
    local codex_skills_target="$HOME/.claude/plugins/marketplaces/ben/skills"
    local codex_skills_link="$HOME/.codex/skills"
    if [[ -d "$codex_skills_target" ]]; then
        mkdir -p "$(dirname "$codex_skills_link")"
        ln -sfn "$codex_skills_target" "$codex_skills_link"
        info "Linked codex skills -> $codex_skills_target"
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
    bootstrap_claude_plugins
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
