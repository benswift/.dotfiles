# pkb-tools

The scheduled side of Ben's personal knowledge base. Three commands:

- `pkb-agent` --- runs the task files in `~/.nb/home/tasks/` on their cron
  schedules (one systemd timer on weddle polls it every 15 minutes)
- `pkb-health` --- freshness and failure checks over the other scheduled jobs,
  paging on a new problem
- `pkb-todo` --- the shell's `todo`: open todos in three sections (todos,
  blocked, bot), plus do/undo/block/unblock and quote-free creation

Installed editable by `install.sh` and `dotfiles update`
(`uv tool install --force -e ~/.dotfiles/pkb`). The notebook's own `CLAUDE.md`
documents the task-file format.

```sh
uv run --group dev pytest        # tests
uv run --group dev ty check src/ # types
```
