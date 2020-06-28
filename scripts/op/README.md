# 1Password "get password" scripts

> NOTE: I'm not actually using this, because the CLI latency **suuuuuucks**.
> C'mon java, you're letting the team down.

Since the [1Password CLI](https://support.1password.com/command-line/) gives
results in JSON, it's necessary to munge the output with
[jq](https://stedolan.github.io/jq/) to get the fields out. No-one could ever
remember these jq filters of the top of their head; hence the scripts in this
repo.

Each one prints the password to stdout, assuming you've got valid 1Password
credentials (which are not in the repo, obviously 😉).
