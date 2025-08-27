# Ben's dotfiles repo

This repo contains all of the (public) config files that I use on my machines.

To configure a new machine the way I like it, run `./create_symlinks.sh` and it
will link all the files into their expected locations.

## Zed

My zed editor config is in @zed/

## Claude Code

My Claude Code editor config is in @claude/

## Email

The email config lives in @mail/ and includes:

- @mail/neomutt/ - neomutt email client config
- @mail/msmtprc - SMTP configuration
- @mail/mbsyncrc - IMAP sync configuration
- OAuth scripts and authentication helpers

See @mail/README.md for detailed setup instructions.
