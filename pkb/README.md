# pkb-tools

The scheduled "EA" side of Ben's personal knowledge base. Four commands, one
generated file:

- `pkb-agent` --- runs the task files in `~/.nb/home/tasks/` on their cron
  schedules (one systemd timer on weddle polls it every 15 minutes)
- `pkb-triage` --- deterministic-first inbox triage with drafted replies
- `pkb-health` --- freshness and failure checks over the other scheduled jobs
- `pkb-briefing` --- reads and replaces named sections of
  `~/.nb/home/briefing.md`, the single file the loops write into

Installed editable by `install.sh` and `dotfiles update`
(`uv tool install --force -e ~/.dotfiles/pkb`). The notebook's own `CLAUDE.md`
documents the task-file format and the briefing from the reader's side;
`~/.nb/home/ea-workflow.md` is the human-facing walkthrough.

```sh
uv run --group dev pytest        # tests
uv run --group dev ty check src/ # types
```
