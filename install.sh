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
    Linux) echo "linux" ;;
    *) echo "unknown" ;;
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

# Mail tooling, per the tiers in mail/README.md. macOS is the full tier:
# isync (mbsync) with OAuth2 --- homebrew-core's isync is built without SASL,
# so it can't authenticate to Office365 over XOAUTH2, while the benswift/tap
# build links a cyrus-sasl that bundles the XOAUTH2 plugin (and survives
# `brew upgrade`, unlike the old self-compiled binary). Linux is read-only
# plus send: mu to index a maildir and msmtp for mail-compose, no mbsync (the
# distro isync is too old for the config anyway, and only one host syncs).
install_mail_sync() {
    if [[ "$platform" == "macos" ]]; then
        if brew list isync &>/dev/null; then
            info "isync already installed (mbsync with XOAUTH2)"
            return
        fi
        info "Installing isync (mbsync) with XOAUTH2 support..."
        brew tap benswift/tap
        brew install benswift/tap/isync
    elif command_exists apt-get; then
        if command_exists mu && command_exists msmtp; then
            info "mu and msmtp already installed"
            return
        fi
        info "Installing mu and msmtp (read-only mail tier)..."
        sudo apt-get install -y maildir-utils msmtp
    else
        warn "no known package manager --- install mu and msmtp manually"
    fi
}

# mosh: mitigates high-latency SSH (predictive echo, frame sync, roaming).
# No prebuilt binaries exist, so it can't come from mise --- platform package
# manager on both OSes (installs both the client and mosh-server).
install_mosh() {
    if command_exists mosh; then
        info "mosh already installed"
        return
    fi

    info "Installing mosh..."
    if [[ "$platform" == "macos" ]]; then
        brew install mosh
    elif command_exists apt-get; then
        sudo apt-get install -y mosh
    elif command_exists dnf; then
        sudo dnf install -y mosh
    else
        warn "no known package manager --- install mosh manually"
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

install_helix() {
    info "Building helix from source..."
    eval "$(mise activate bash)"
    # Not a mise tool: upstream has cut no release since 25.07.1 and ships no
    # nightly, so bin/helix-build compiles the commit pinned in
    # helix/pinned-rev. A first run also builds ~300 tree-sitter grammars.
    # --no-test because the integration suite wants the language servers, and
    # install_python_tools has not run yet.
    "$DOTFILES_DIR/bin/helix-build" --no-test ||
        warn "helix build failed --- run 'helix-build' once the machine is set up"
}

setup_previewers() {
    info "Installing yazi plugins and tree-sitter parsers..."
    eval "$(mise activate bash)"
    # piper.yazi, pinned in yazi/package.toml; yazi/yazi.toml routes previews
    # through it into bin/ts-cat
    ya pkg install || warn "ya pkg install failed --- yazi previews will use the built-in previewer"
    "$DOTFILES_DIR/bin/lumis-parsers" || warn "parser fetch failed --- ts-cat will fall back to bat"
}

install_python_tools() {
    info "Installing Python tools from mail/utils..."
    eval "$(mise activate bash)"
    # Puts mail-compose, mail-dedupe, student-db, mutt-compose-lsp and the rest
    # on PATH. Editable, so the checkout stays the live source. Not optional:
    # helix/languages.toml configures mutt-compose-lsp as a language server, so
    # without this a fresh machine has a broken editor integration and no
    # obvious sign of why. --force makes it idempotent and picks up dependency
    # changes on re-runs.
    uv tool install --force -e "$DOTFILES_DIR/mail/utils" ||
        warn "mail-utils install failed --- mail-* commands and mutt-compose-lsp will be missing"
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

    info "Installing agent-browser skill..."
    # mise exec rather than bare bunx: in this non-interactive shell mise's
    # activation hook never fires, so mise-managed bun isn't on PATH yet
    mise exec -- bunx skills add vercel-labs/agent-browser
}

sync_agent_config() {
    info "Synchronising AI agent configuration..."
    "$DOTFILES_DIR/bin/sync-agent-config"
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
    install_mail_sync
    install_mosh
    install_mise
    clone_dotfiles
    setup_symlinks
    install_mise_tools
    install_helix
    setup_previewers
    install_python_tools
    install_claude
    install_agent_skills
    sync_agent_config

    echo ""
    info "Bootstrap complete!"
    echo ""
    echo "Next steps:"
    echo "  1. Restart your shell or run: source ~/.zshrc"
    echo "  2. Run 'dotfiles doctor' to verify setup"
    echo "     (note: gitconfig rewrites github https URLs to ssh, so add this"
    echo "      machine's ssh key to GitHub before running 'dotfiles update')"
    if [[ "$platform" == "macos" ]]; then
        echo "  3. Install additional tools as needed with 'brew install' or 'mise use'"
    else
        echo "  3. Install additional tools as needed with your package manager or 'mise use'"
    fi
    echo ""
}

main "$@"
