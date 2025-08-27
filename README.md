# Ben's dotfiles

My dotfiles. These days, [Zed](https://zed.dev) is the bright star around which
most of the rest of it revolves.

Where I've taken anything from the web I've tried to give credit appropriate to
the licence. If I've missed giving you a shout out, then let me know and I'll
gladly add one in.

## Installation

Run the included script to create symlinks for all config files:

```bash
./create_symlinks.sh
```

This will automatically link all dotfiles and Zed configuration files to their
expected locations.

## Email configuration

All email-related configuration has been organized into the `mail/` directory. This includes configurations for mbsync (IMAP sync), msmtp (SMTP), neomutt (email client), and OAuth2 authentication scripts.

For detailed setup instructions, see [`mail/README.md`](mail/README.md).

# License

(c) 2012-2025 Ben Swift

MIT License
