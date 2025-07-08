# .claude folder

I like keeping my user `CLAUDE.md` under version control---I basically consider
it another one of my dotfiles. So it's in this repo, and the
`create-symlinks.sh` script links the various files into the places where Claude
Code expects to find them.

One other tip for Elixir devs: the
[Usage Rules](https://hexdocs.pm/usage_rules/) package is really great for
injecting project-specific rules into your project's `CLAUDE.md` file. Here's an
invocation I've found particularly helpful for working with Claude Code:

```
mix usage_rules.sync CLAUDE.md --all --inline usage_rules:all --link-to-folder deps --link-style at --remove-missing
```
