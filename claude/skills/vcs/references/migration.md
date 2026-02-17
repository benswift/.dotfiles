# Migrating a published git repo to jj

This is the recommended approach for repos already published on GitHub. It uses
colocated mode so that `.jj` and `.git` coexist, preserving full compatibility
with GitHub, CI, and collaborators using git.

## Step 1: initialise jj in the existing repo

```
cd /path/to/repo
jj git init --colocate
```

This creates a `.jj/` directory alongside the existing `.git/`. All existing git
history, branches, and tags are imported automatically.

## Step 2: verify

```
jj log                 # should show full history
jj bookmark list       # should show all git branches as bookmarks
jj st                  # should show clean working copy
```

## Step 3: configure gitignore

Add `.jj/` to the project's `.gitignore`. jj sometimes does this automatically
on init, but you must verify it's present:

```
grep -q '^\\.jj/' .gitignore || echo '.jj/' >> .gitignore
```

This prevents the `.jj/` directory from being tracked by git, which is essential
for colocated repos.

## Step 4: update agent instructions (if applicable)

If the project has an `AGENTS.md`, `CLAUDE.md`, or similar agent instruction
file, add a note that agents should use `jj` for version control, not raw `git`
commands. For example:

```markdown
## Version control

This repo uses jj (Jujutsu) as a colocated repo. **Always use `jj` for version
control operations, never raw `git` commands.** The `gh` CLI is still fine for
GitHub API operations (PRs, issues, etc.).
```

This prevents agents from accidentally using git commands that could conflict
with jj's operation log or cause bookmark synchronisation issues.

## Step 5: start using jj

From this point, use `jj` for all VCS operations. The colocated mode
automatically syncs between jj and git on every command, so:

- `jj git push` updates the remote git branches
- `jj git fetch` imports remote changes
- collaborators using plain git see normal git history
- CI/CD works unchanged
- GitHub PRs work via bookmarks

## Limitations of colocated mode

- no submodule support (they won't appear in working copy but aren't lost)
- `.gitattributes` are not supported
- git hooks are not triggered by jj commands
- interleaving `jj` and `git` commands on the same repo can cause bookmark
  conflicts; prefer using only `jj` for VCS operations
- shared filesystem scenarios (NFS, Dropbox) can cause issues with concurrent
  access

## Rolling back

If you decide to stop using jj, simply delete the `.jj/` directory. The git
repo is fully intact and independent.
