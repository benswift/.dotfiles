# Description

My dotfiles, mostly centred around emacs and zsh. Emacs-wise, I use
Phil Hagelberg's
[emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit/)
with several customisations added for the modes I use regularly. I've
also taken a lot of git & osx customisations from Mathias Bynens'
[dotfiles repo](https://github.com/mathiasbynens/dotfiles).

If I've pinched any of the snippets off the web (and I have in a lot
of cases) I've tried to give appropriate credit.  If I've missed
giving you a shout out, then let me know and I'll gladly add one in.

# Installation

The `Makefile` (on OSX and Linux) or `make.bat` on Windows 7
(providing some GNU utilities are present) links all the files into
the places where they should be.  Alternately, you can manually link
(or even just move) the files into the places where the respective
programs expect them to live.

So, on OSX

```shell
$ cd .dotfiles
$ make osx
```

Linux

```shell
$ cd .dotfiles
$ make linux
```

Windows (must be in home directory)

```shell
$ cd .dotfiles
$ make.bat
```
