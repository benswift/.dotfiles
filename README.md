# Ben's dotfiles

My (Emacs-centric) dotfiles. On macOS, I use railwaycat's
[emacs-mac-port](https://github.com/railwaycat/emacs-mac-port) (installed
through [homebrew](http://brew.sh)), and then I use
[spacemacs](https://spacemacs.org).

I've also got some config for the Emacs `extempore-mode` which is probably the
most up-to-date resource on how to get [Extempore](http://extempore.moso.com.au)
and Emacs/Spacemacs to play nicely together (since I'm the author of
`extempore-mode`).

If I've pinched any of the snippets off the web (and I have in a lot
of cases) I've tried to give appropriate credit.  If I've missed
giving you a shout out, then let me know and I'll gladly add one in.

## Installation

The best way to use this stuff is probably to symlink (or even just move/copy)
the config files into the places where their respective programs expect them to
live.

## Windows

If you're on Windows and can't remember the correct link command syntax, here's
the magic invocations (assuming you're in a powershell).

```
cmd /c mklink %HOME%\.spacemacs %HOME%\.dotfiles\spacemacs
cmd /c mklink %HOME%\.gitconfig %HOME%\.dotfiles\gitconfig
cmd /c mklink %HOME%\.gitignore %HOME%\.dotfiles\gitignore
```

Also, make sure that your `GIT_SSH` environment variable points to the right
place (e.g. `C:\Windows\System32\OpenSSH\ssh.exe`).

# Licence

(c) 2012-2020 Ben Swift

MIT Licence
