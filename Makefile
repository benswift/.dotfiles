DOTFILES_DIR=$(shell pwd)

link:
	# zsh bits		
	ln -sfF $(DOTFILES_DIR)/zshrc ~/.zshrc
	ln -sfF $(DOTFILES_DIR)/exports ~/.exports
	ln -sfF $(DOTFILES_DIR)/aliases ~/.aliases
	# git
	ln -sfF $(DOTFILES_DIR)/gitconfig ~/.gitconfig
	ln -sfF $(DOTFILES_DIR)/gitignore ~/.gitignore
	# emacs
	ln -sfF $(DOTFILES_DIR)/emacs.d ~/.emacs.d
	# misc
	ln -sfF $(DOTFILES_DIR)/osx ~/.osx
	ln -sfF $(DOTFILES_DIR)/brew ~/.brew
	ln -sfF $(DOTFILES_DIR)/RProfile ~/.RProfile
	ln -sfF $(DOTFILES_DIR)/ssh_config ~/.ssh/config
