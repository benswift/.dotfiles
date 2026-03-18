---
name: vcs
description:
  Version control using Git. Covers daily workflow, branching, rebasing,
  conflict resolution, and GitHub integration via gh CLI. Use when working with
  version control, git, or GitHub.
---

You are an expert in version control. Use `git` for all version control
operations and `gh` CLI for GitHub API operations (PRs, issues, etc.).

---

# Git

The goal is a clean, linear history.

## Rules

- **Always rebase, never merge.** Only merge when genuinely unavoidable (e.g.
  integrating a long-lived divergent branch where rebase would lose meaningful
  context), and ask for confirmation first.
- **Always `git pull --rebase`**, never plain `git pull`.
- **Stage specific files** by name, not `git add .` or `git add -A`.
- **Use `--force-with-lease`** when force-pushing after a rebase, never bare
  `--force`.
- **Write concise commit messages** in imperative mood ("add feature", not
  "added feature").
- **Make small, focused commits** that each do one thing.
- **Don't commit** generated files, build artefacts, or secrets.
- **Clean up feature branches** after merging.
- **Avoid interactive flags** (`-i`) --- they require a TTY and will fail in
  non-interactive environments.

## Daily workflow

```
git status                             # working tree status
git diff                               # unstaged changes
git diff --staged                      # staged changes
git log --oneline -20                  # recent history
git add <file>                         # stage specific file
git commit -m "message"                # commit
git stash                              # stash working changes
git stash pop                          # restore stashed changes
```

## Branching

```
git branch <name>                      # create branch
git checkout <name>                    # switch branch
git checkout -b <name>                 # create and switch
git branch -d <name>                   # delete merged branch
```

## Syncing with upstream

```
git fetch origin
git rebase origin/main                 # rebase current branch onto main
git pull --rebase                      # shorthand: fetch + rebase
git push --force-with-lease            # force push safely after rebase
```

## Undoing things

```
git commit --amend                     # amend the last commit
git reset HEAD~1                       # undo last commit, keep changes staged
git reset --mixed HEAD~1               # undo last commit, unstage changes
git reflog                             # find lost commits
```

---

# GitHub (gh CLI)

Use `gh` for all GitHub API interactions. Prefer `--rebase` when merging PRs via
`gh pr merge`.
