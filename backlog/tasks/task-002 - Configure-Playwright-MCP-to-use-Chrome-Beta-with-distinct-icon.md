---
id: task-002
title: Configure Playwright MCP to use Chrome Beta with distinct icon
status: Done
assignee: []
created_date: "2025-08-19 04:04"
labels: []
dependencies: []
---

WONTFIX: this isn't a good idea.

## Description

Set up Playwright MCP server to use Chrome Beta instead of regular Chrome for
better visual distinction in macOS app switcher

## Background

When using the Playwright MCP server on projects, it opens Chrome by default.
This makes it hard to distinguish from regular Chrome in the command+tab app
switcher on macOS. Using Chrome Beta (with its blue icon) would provide better
visual distinction.

## Implementation steps

1. **Install Chrome Beta**

   ```bash
   brew install --cask google-chrome-beta
   ```

2. **Create wrapper script**

   - Create a `playwright-mcp-beta` wrapper script in `~/bin` (which is already
     in PATH via zshenv)
   - Script should call `npx @playwright/mcp@latest` with Chrome Beta executable
     path
   - Make script executable

3. **Integration with dotfiles**

   - Decide whether to:
     - a) Add the wrapper script directly to `.dotfiles/bin/` (tracked in git),
       or
     - b) Create it via setup script, or
     - c) Add creation commands to zshrc/zshenv
   - Consider existing structure:
     - `zshenv` already adds `$HOME/.dotfiles/bin` to PATH
     - `zshrc` handles aliases and tool activation

4. **Wrapper script content**

   ```bash
   #!/bin/bash
   npx @playwright/mcp@latest --browser chrome --executable-path "/Applications/Google Chrome Beta.app/Contents/MacOS/Google Chrome Beta" "$@"
   ```

5. **Usage** After setup, Claude settings for each project can use:
   ```json
   {
     "mcpServers": {
       "playwright": {
         "command": "playwright-mcp-beta"
       }
     }
   }
   ```

## Notes

- Chrome Beta is more stable than Dev or Canary channels
- Beta has a distinct blue icon vs regular Chrome's red/green/yellow icon
- No global environment variable exists for Playwright to specify executable
  path
- This approach avoids editing Claude settings for each individual project
