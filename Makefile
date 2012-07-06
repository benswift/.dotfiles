DOTFILES_DIR=$(shell pwd)

link:
	# zsh bits		
	ln -sf $(DOTFILES_DIR)/zshrc ~/.zshrc
	ln -sf $(DOTFILES_DIR)/exports ~/.exports
	ln -sf $(DOTFILES_DIR)/aliases ~/.aliases
	# git
	ln -sf $(DOTFILES_DIR)/gitconfig ~/.gitconfig
	ln -sf $(DOTFILES_DIR)/gitignore ~/.gitignore
	# emacs
	ln -sf $(DOTFILES_DIR)/emacs ~/.emacs
	# misc
	ln -sf $(DOTFILES_DIR)/osx ~/.osx
	ln -sf $(DOTFILES_DIR)/brew ~/.brew
	ln -sf $(DOTFILES_DIR)/RProfile ~/.RProfile
	ln -sf $(DOTFILES_DIR)/ssh_config ~/.ssh/config
