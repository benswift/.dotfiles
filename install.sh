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

# vivid (zshrc's LS_COLORS): mise covers Linux, but upstream ships no arm64
# macOS binary, so the Mac gets Homebrew's.
install_vivid() {
    [[ "$platform" == "macos" ]] || return 0
    if command_exists vivid; then
        info "vivid already installed"
        return
    fi
    info "Installing vivid..."
    brew install vivid
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
    # setup_private has not run yet.
    "$DOTFILES_DIR/bin/helix-build" --no-test ||
        warn "helix build failed --- run 'helix-build' once the machine is set up"
}

install_wiz() {
    info "Building the wiz engine from source..."
    eval "$(mise activate bash)"
    # Built at the commit pinned in wiz/pinned-rev rather than installed from
    # a vault's pin; bin/wiz-build's header says why.
    "$DOTFILES_DIR/bin/wiz-build" ||
        warn "wiz build failed --- run 'wiz-build' once the machine can reach gitlab.anu.edu.au"
}

setup_previewers() {
    info "Installing yazi plugins and tree-sitter parsers..."
    eval "$(mise activate bash)"
    # piper.yazi, pinned in yazi/package.toml; yazi/yazi.toml routes previews
    # through it into bin/ts-cat
    ya pkg install || warn "ya pkg install failed --- yazi previews will use the built-in previewer"
    "$DOTFILES_DIR/bin/lumis-parsers" || warn "parser fetch failed --- ts-cat will fall back to bat"
}

# The private overlay (lib/private.sh): cloned only if this machine's ssh key
# can already read it, so a fresh machine bootstraps without GitHub auth and
# picks it up on a later `dotfiles update`.
clone_private() {
    info "Looking for the private overlay..."
    source "$DOTFILES_DIR/lib/private.sh"
    private_sync
}

setup_private() {
    eval "$(mise activate bash)"
    private_setup
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
    # --global --agent --yes answer the three prompts (scope, agents, confirm):
    # without them the CLI refuses to run off a terminal, and installs nothing.
    # "universal" is ~/.agents/skills, the directory Codex and Matilda read.
    mise exec -- bunx skills add vercel-labs/agent-browser \
        --global --agent universal --yes
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
    install_mosh
    install_vivid
    install_mise
    clone_dotfiles
    clone_private
    setup_symlinks
    install_mise_tools
    install_helix
    install_wiz
    setup_previewers
    setup_private
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
    echo "      machine's ssh key to GitHub before running 'dotfiles update';"
    echo "      that update also clones the private overlay if it was skipped)"
    if [[ "$platform" == "macos" ]]; then
        echo "  3. Install additional tools as needed with 'brew install' or 'mise use'"
    else
        echo "  3. Install additional tools as needed with your package manager or 'mise use'"
    fi
    echo ""
}

main "$@"
