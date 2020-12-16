# Ben's dotfiles

My (Emacs-centric) dotfiles. On macOS, I use railwaycat's
[emacs-mac-port](https://github.com/railwaycat/emacs-mac-port) (installed
through [homebrew](http://brew.sh)), and then I use
[Spacemacs](https://spacemacs.org).

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Installation

The best way to use this stuff is probably to symlink (or even just move/copy)
the config files into the places where their respective programs expect them to
live. Once you've got the `.spacemacs` config working, there's a
`ben-symlink-dotfiles` helper elisp function (in `ben-utils.el:759`) which will
help you with the rest.

### Windows

If you're on Windows and can't remember the correct link command syntax, here's
the magic invocations (assuming you're in a powershell).

```
cmd /c mklink %HOME%\.spacemacs %HOME%\.dotfiles\spacemacs
cmd /c mklink %HOME%\.gitconfig %HOME%\.dotfiles\gitconfig
cmd /c mklink %HOME%\.gitignore %HOME%\.dotfiles\gitignore
```

Also, make sure that your `GIT_SSH` environment variable points to the right
place (e.g. `C:\Windows\System32\OpenSSH\ssh.exe`), and that `ssh-agent.exe` is
running.

# Licence

(c) 2012-2020 Ben Swift

MIT Licence
