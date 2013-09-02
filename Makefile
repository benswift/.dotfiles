DOTFILES_DIR=$(shell pwd)

nix:
	ln -sf $(DOTFILES_DIR)/bash_profile ~/.bash_profile

	ln -sf $(DOTFILES_DIR)/gitconfig ~/.gitconfig
	ln -sf $(DOTFILES_DIR)/gitignore ~/.gitignore

	ln -sf $(DOTFILES_DIR)/emacs ~/.emacs
	ln -sf $(DOTFILES_DIR)/emacs.elc ~/.emacs.elc
	ln -sf $(DOTFILES_DIR)/snippets ~/.emacs.d/

	ln -sf $(DOTFILES_DIR)/RProfile ~/.RProfile
	ln -sf $(DOTFILES_DIR)/ssh_config ~/.ssh/config

osx:
	ln -sf $(DOTFILES_DIR)/osx ~/.osx
	ln -sf $(DOTFILES_DIR)/brew ~/.brew
	make nix

linux:
	make nix
