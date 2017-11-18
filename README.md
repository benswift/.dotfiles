# Ben's dotfiles

My (Emacs-centric) dotfiles. On macOS, I use railwaycat's
[emacs-mac-port](https://github.com/railwaycat/emacs-mac-port) (installed
through [homebrew](http://brew.sh)), and then I use
[spacemacs](https://spacemacs.org).

I've also taken some `git` & macOS customisations from Mathias Bynens' [dotfiles
repo](https://github.com/mathiasbynens/dotfiles).

I've also got some config for the Emacs `extempore-mode` which is probably the
most up-to-date resource on how to get [Extempore](http://extempore.moso.com.au)
and Emacs/Spacemacs to play nicely together (since I'm the author of
`extempore-mode`).

If I've pinched any of the snippets off the web (and I have in a lot
of cases) I've tried to give appropriate credit.  If I've missed
giving you a shout out, then let me know and I'll gladly add one in.

# Installation

## macOS

```shell
$ cd .dotfiles
$ make macos
```

## Linux

```shell
$ cd .dotfiles
$ make linux
```

## Windows

```shell
$ cd .dotfiles
$ make.bat
```

Alternately, you can manually link (or even just move/copy) the config
files into the places where their respective programs expect them to
live.

# Uninstallation

On macOS or Linux, there's a `clean` target:

```shell
make clean
```

On Windows, just delete the symlinks manually.

Then, delete the `.dotfiles` directory and you're done.

# Licence

(c) 2012-2017 Ben Swift

MIT Licence
