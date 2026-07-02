# shellcheck shell=bash
#
# Shared colour codes and logging helpers for the dotfiles scripts.
# install.sh deliberately does NOT source this --- it runs via curl | bash
# before the repo exists, so it keeps its own copy.

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

info() { echo -e "${GREEN}[info]${NC} $1"; }
warn() { echo -e "${YELLOW}[warn]${NC} $1"; }
error() { echo -e "${RED}[error]${NC} $1" >&2; }
