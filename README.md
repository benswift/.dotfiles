# Ben's dotfiles

My dotfiles. These days, [Zed](https://zed.dev) is the bright star around which most of the rest of it revolves.

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Installation

The best way to use this stuff is probably to symlink (or even just move/copy)
the config files into the places where their respective programs expect them to
live.

```bash
ln -s ~/.dotfiles/zed-keymap.json ~/.config/zed/keymap.json
ln -s ~/.dotfiles/zed-settings.json ~/.config/zed/settings.json
ln -s ~/.dotfiles/zed-tasks.json ~/.config/zed/tasks.json
```

### Windows

Make sure that your `GIT_SSH` environment variable points to the right
place (e.g. `C:\Windows\System32\OpenSSH\ssh.exe`), and that `ssh-agent.exe` is
running.

# Licence

(c) 2012-2024 Ben Swift

MIT Licence
