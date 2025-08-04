---
id: task-001
title: remove ash_ai stuff from tidewave-proxy script
status: In Progress
assignee: []
created_date: "2025-08-04 22:37"
labels: []
dependencies: []
---

## Description

Tidewave is _awesome_, but for the work I'm doing I haven't really found a need
for AshAI yet.

Remove all AshAI references from the tidewave-proxy script.

## Progress Notes

### 2025-08-05

Successfully removed all ash_ai references from `/Users/ben/.dotfiles/bin/tidewave-proxy.sh`:

- Removed ash_ai endpoint configuration variable (`ASH_AI_ENDPOINT`)
- Removed ash_ai PID file variable and cleanup handling
- Removed ash_ai mcp-proxy startup call
- Updated help text to remove ash_ai endpoint references
- Updated all output messages to remove ash_ai mentions
- Updated script comments to reflect single tidewave proxy

The script now only handles the Tidewave MCP proxy and no longer references ash_ai anywhere.
