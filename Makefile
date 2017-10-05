unix:
	ln -sf mbsyncrc ~/.mbsyncrc

	ln -sf lldbinit ~/.lldbinit

	ln -sf gitconfig ~/.gitconfig
	ln -sf gitignore ~/.gitignore

	ln -sf RProfile ~/.RProfile
	ln -sf profiles.clj ~/.lein/
	ln -sf ssh_config ~/.ssh/config

	ln -sf scripts ~/bin

osx: unix
	ln -sf bash_profile.osx ~/.bash_profile

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
