# shellcheck shell=bash
# shellcheck disable=SC2034  # the manifest arrays are consumed by sourcing scripts
#
# Single source of truth for every symlink this repo installs. Sourced by
# create_symlinks.sh (which creates them) and bin/dotfiles (whose doctor
# verifies them) --- add new links here and both stay in sync.
#
# Entries are "repo-relative-source:target"; a leading ~ in the target is
# expanded to $HOME by the consumer. Sources may be files or directories.
#
# Not listed here: ~/.agents/skills. bin/sync-agent-config populates it per-skill
# from the ben marketplace clone (which doesn't exist until claude has run),
# while preserving skills installed independently by other tools.

SYMLINK_MANIFEST=(
    # home directory dotfiles
    "Rprofile:~/.Rprofile"
    "gitattributes:~/.gitattributes"
    "gitconfig:~/.gitconfig"
    "gitignore:~/.gitignore"
    "tmux.conf:~/.tmux.conf"
    "zshenv:~/.zshenv"
    "zshrc:~/.zshrc"
    # ssh
    "ssh_config:~/.ssh/config"
    # mail
    "mail/mbsyncrc:~/.mbsyncrc"
    "mail/msmtprc:~/.msmtprc"
    "mail/neomutt:~/.config/neomutt"
    # zed
    "zed/keymap.json:~/.config/zed/keymap.json"
    "zed/settings.json:~/.config/zed/settings.json"
    "zed/tasks.json:~/.config/zed/tasks.json"
    "zed/snippets:~/.config/zed/snippets"
    # AI agents
    "claude/CLAUDE.md:~/.claude/CLAUDE.md"
    "claude/settings.json:~/.claude/settings.json"
    "claude/CLAUDE.md:~/.codex/instructions.md"
    # tools
    "mise/config.toml:~/.config/mise/config.toml"
    # yazi: linked file-by-file rather than as a directory, because
    # ~/.config/yazi/plugins is owned by `ya pkg`
    "yazi/yazi.toml:~/.config/yazi/yazi.toml"
    # systemd user units (linked individually because
    # ~/.config/systemd/user/ holds units from several repos). Run
    # `systemctl --user daemon-reload && systemctl --user enable --now
    # <name>.timer` after first install.
    "systemd/user/ship-claude-logs.service:~/.config/systemd/user/ship-claude-logs.service"
    "systemd/user/ship-claude-logs.timer:~/.config/systemd/user/ship-claude-logs.timer"
    "systemd/user/ingest-claude-logs.service:~/.config/systemd/user/ingest-claude-logs.service"
    "systemd/user/ingest-claude-logs.timer:~/.config/systemd/user/ingest-claude-logs.timer"
    # pkb-agent: enable the timer on weddle ONLY (run stamps are
    # machine-local, so a second machine would re-run every task)
    "systemd/user/pkb-agent.service:~/.config/systemd/user/pkb-agent.service"
    "systemd/user/pkb-agent.timer:~/.config/systemd/user/pkb-agent.timer"
    # whole-directory configs
    "ghostty:~/.config/ghostty"
    "helix:~/.config/helix"
    "zellij:~/.config/zellij"
)

# Targets that tools rewrite in place (so a real file may already exist
# there): back up the existing file before linking.
SYMLINK_MANIFEST_WITH_BACKUP=(
    # Codex writes trust records and UI state to config.toml. The portable
    # layer is a profile, selected by the zsh aliases and codex-zellij.
    "codex/config.toml:~/.codex/dotfiles.config.toml"
    "gemini/settings.json:~/.gemini/settings.json"
    # `ya pkg add` rewrites this to record each plugin's rev and hash
    "yazi/package.toml:~/.config/yazi/package.toml"
)

# nb plugins are globbed rather than listed: one link per *.nb-plugin file,
# picked up by nb on its next invocation. Emits "source:target" lines in
# the same format as the arrays above. Requires DOTFILES_DIR to be set.
nb_plugin_links() {
    local plugin
    for plugin in "$DOTFILES_DIR"/nb/*.nb-plugin; do
        [[ -e "$plugin" ]] || continue
        echo "nb/$(basename "$plugin"):~/.nb/.plugins/$(basename "$plugin")"
    done
}

# Expand a manifest entry into its two parts. Usage:
#   manifest_source "entry"  -> absolute path into the repo
#   manifest_target "entry"  -> absolute path with ~ expanded
manifest_source() { echo "$DOTFILES_DIR/${1%%:*}"; }
manifest_target() { local t="${1##*:}"; echo "${t/#\~/$HOME}"; }
