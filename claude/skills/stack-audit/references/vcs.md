# Version control preference checklist

For detailed jj and git workflows, see the **vcs** skill.

## Structural

| Check | Preference | Anti-pattern |
|---|---|---|
| VCS tool | jj (Jujutsu) in colocated mode (`.jj/` + `.git/`) | plain git only |
| GitHub operations | `gh` CLI | manual browser workflows, raw API calls |

## Soft

| Check | Preference | Anti-pattern |
|---|---|---|
| History style | linear (rebase, never merge) | merge commits for routine integration |
| Commit messages | concise, imperative mood | past tense, verbose descriptions |
| Change size | small, focused changes | large multi-concern changes |
| Force push | `--force-with-lease` (git) | bare `--force` |
| Branch naming | jj bookmarks | long-lived feature branches |
