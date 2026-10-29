# shellcheck shell=bash
#
# The private overlay: benswift/dotfiles-private, cloned inside this checkout
# at private/ (gitignored here, and re-included for rg, fd and helix by
# .ignore, so it edits and searches as one tree). It holds the config that
# names institutional accounts and infrastructure: mail, weddle's backups, ssh
# hosts. Everything public works without it, so a fresh machine bootstraps
# with no GitHub auth and picks the overlay up once its ssh key can read the
# repo.
#
# The overlay plugs in at fixed points, each optional:
#
#   private/manifest.sh    appends to SYMLINK_MANIFEST (lib/symlink-manifest.sh)
#   private/setup          installs what it needs (install.sh, dotfiles update)
#   private/lib/doctor.sh  defines check_private (dotfiles doctor)
#   private/bin/           on PATH (zshenv, bin/launchd-run, wiz-due.service)
#   private/ssh_config     Included by ssh_config

PRIVATE_DIR="$DOTFILES_DIR/private"
PRIVATE_REPO="${DOTFILES_PRIVATE_REPO:-git@github.com:benswift/dotfiles-private.git}"

# Clone the overlay if this machine can read it, else pull it. Never fails: a
# machine without access carries on without the overlay.
private_sync() {
    if [[ -d "$PRIVATE_DIR/.git" ]]; then
        git -C "$PRIVATE_DIR" pull --rebase --autostash ||
            echo "private overlay: pull failed --- sort out $PRIVATE_DIR by hand" >&2
    elif GIT_SSH_COMMAND="ssh -o BatchMode=yes" git clone --quiet "$PRIVATE_REPO" "$PRIVATE_DIR" 2>/dev/null; then
        echo "private overlay: cloned into $PRIVATE_DIR"
    else
        echo "private overlay: $PRIVATE_REPO isn't readable from this machine --- skipped" >&2
    fi
}

# Run the overlay's setup hook, if there is an overlay.
private_setup() {
    [[ -x "$PRIVATE_DIR/setup" ]] || return 0
    "$PRIVATE_DIR/setup" || echo "private overlay: setup failed" >&2
}
