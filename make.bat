mklink "C:%HOMEPATH%\.gitconfig" "C:%HOMEPATH%\.dotfiles\gitconfig"
mklink "C:%HOMEPATH%\.gitignore" "C:%HOMEPATH%\.dotfiles\gitignore"

mklink "C:%HOMEPATH%\.emacs" "C:%HOMEPATH%\.dotfiles\emacs"
mklink /D "C:%HOMEPATH%\.emacs.d\snippets" "C:%HOMEPATH%\.dotfiles\snippets"

mklink "C:%HOMEPATH%\.RProfile" "C:%HOMEPATH%\.dotfiles\RProfile"

mklink "C:%HOMEPATH%\Documents\AutoHotkey.ahk" "C:%HOMEPATH%\.dotfiles\AutoHotkey.ahk"
