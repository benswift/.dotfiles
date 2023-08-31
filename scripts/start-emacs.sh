#!/bin/bash

# from https://github.com/railwaycat/homebrew-emacsmacport/blob/master/docs/emacs-start-helpers.md
exec $(brew --prefix)/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs.sh "$@"
