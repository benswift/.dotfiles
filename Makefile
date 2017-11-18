# symlink all the things!

unix:
	ln -sf RProfile ~/.RProfile
	ln -sf gitconfig ~/.gitconfig
	ln -sf gitignore ~/.gitignore
	ln -sf lldbinit ~/.lldbinit
	ln -sf mbsyncrc ~/.mbsyncrc
	ln -sf profiles.clj ~/.lein/
	ln -sf scripts ~/bin
	ln -sf spacemacs ~/.spacemacs
	ln -sf ssh_config ~/.ssh/config

macos: unix
	ln -sf bash_profile.osx ~/.bash_profile
	ln -sf vscode-settings.json ~/Library/Application Support/Code/settings.json

linux: unix
	ln -sf bash_profile.linux ~/.bash_profile

clean:
	rm -r ~/bin
	rm ~/.RProfile
	rm ~/.bash_profile
	rm ~/.gitconfig
	rm ~/.gitignore
	rm ~/.lldbinit
	rm ~/.mbsyncrc
	rm ~/.spacemacs
	rm ~/.ssh/config
