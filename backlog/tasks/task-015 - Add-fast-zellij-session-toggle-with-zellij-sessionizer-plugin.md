---
id: TASK-015
title: Add fast zellij session toggle with zellij-sessionizer plugin
status: Done
assignee: []
created_date: '2026-01-27 09:51'
updated_date: '2026-01-27 22:45'
labels:
  - zellij
  - shell
  - dotfiles
dependencies: []
references:
  - 'https://github.com/cunialino/zellij-sessionizer'
  - 'https://github.com/mostafaqanbaryan/zellij-switch'
  - 'https://ghostty.org/docs/config/keybind/reference'
documentation:
  - zshrc
  - ghostty/config
  - zellij/config.kdl
  - install.sh
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Implement low-friction session switching for zellij using the cunialino/zellij-sessionizer WASM plugin, which provides a native fuzzy-finder UI for discovering project directories and creating/switching sessions --- all from a single keybinding that works from any context (including inside helix).

Currently switching zellij sessions requires either the session-manager plugin UI (Ctrl+o then w, or Alt+s) which is clunky, or detaching and reattaching from the CLI. Neither is fast enough for an alt-tab-style workflow.

The zellij-sessionizer plugin (https://github.com/cunialino/zellij-sessionizer) is a native WASM plugin with built-in fuzzy finding over project directories. It handles both session creation and switching, works from any zellij keybinding (no shell prompt needed), and only requires `fd` as an external dependency.

Optionally, a `zz` shell function can be added later for instant toggle to the previous session without the fuzzy finder UI.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [x] #1 Zellij-sessionizer WASM plugin installed and configured with project directories
- [x] #2 Keybinding (e.g. Alt+s) opens the sessionizer fuzzy finder from any context including inside helix
- [x] #3 Selecting a directory creates a new session or switches to an existing one
- [x] #4 Alt+s session-manager keybinding moved to a different key or replaced by sessionizer
<!-- AC:END -->

## Implementation Notes

<!-- SECTION:NOTES:BEGIN -->
## Implementation plan

### 1. Install zellij-sessionizer plugin

The plugin is a WASM binary. Two options:

- Option A (remote): reference the release URL directly in the plugin config --- zellij downloads and caches it automatically
- Option B (local): download `sessionizer.wasm` to `~/.config/zellij/plugins/` for offline use

Remote is fine for initial setup; can switch to local later if needed.

Release URL: `https://github.com/cunialino/zellij-sessionizer/releases/download/v0.1.0/sessionizer.wasm`

### 2. Configure plugin in zellij/config.kdl

Add a plugin alias in the `plugins` block:

```kdl
plugins {
    sessionizer location="https://github.com/cunialino/zellij-sessionizer/releases/download/v0.1.0/sessionizer.wasm" {
        cwd "~/"
        default_dirs "~/src;~/work"
        find_cmd "fd"
    }
}
```

Adjust `default_dirs` to match actual project directory layout.

### 3. Add keybinding

Replace the current Alt+s session-manager binding with the sessionizer:

```kdl
bind "Alt s" {
    LaunchOrFocusPlugin "sessionizer" {
        floating true
        move_to_focused_tab true
    }
}
```

This works from any context --- shell prompt, helix, or any other TUI.

### 4. Ensure `fd` is available

Check that `fd` is installed (likely already via mise or homebrew). The plugin uses it to discover directories.

### 5. Optional: add `zz` toggle function later

If instant previous-session toggle is still wanted after trying the sessionizer, add the `zz` shell function from the original plan (uses zellij-switch plugin + state file). This is complementary, not a replacement.

### 6. Files to modify

- `zellij/config.kdl` --- add plugin alias, update Alt+s keybinding
- Possibly `install.sh` --- add `fd` to prerequisites check

### 7. Testing checklist

- Verify Alt+s opens the sessionizer floating pane
- Verify directory fuzzy search works and shows project directories
- Verify selecting a directory creates a new session with correct cwd
- Verify selecting a directory with an existing session switches to it
- Verify the plugin works when triggered from inside helix (not just shell)

### Research references

- cunialino/zellij-sessionizer: https://github.com/cunialino/zellij-sessionizer
- victor-falcon/zellij-sessionizer (shell script alternative, not used): https://github.com/victor-falcon/zellij-sessionizer
- zellij-switch (not needed --- sessionizer handles switching internally): https://github.com/mostafaqanbaryan/zellij-switch
<!-- SECTION:NOTES:END -->

## Final Summary

<!-- SECTION:FINAL_SUMMARY:BEGIN -->
Added laperlej/zellij-sessionizer WASM plugin (v0.4.4) to `zellij/config.kdl`:

- Plugin alias configured with `/Users/ben/code` as root directory (no external dependencies)
- `Alt+s` now launches sessionizer (fuzzy session finder) instead of the built-in session-manager
- Session-manager remains accessible via `Ctrl+o` then `w` (session mode)
- Uses remote URL --- zellij downloads and caches the WASM binary automatically
- Supports per-project `layout.kdl` overrides

Changes in `zellij/config.kdl`:
1. Added `sessionizer` plugin alias pointing to laperlej v0.4.4 release URL
2. Changed `Alt s` binding from `session-manager` to `sessionizer`

Restart zellij to pick up the changes.
<!-- SECTION:FINAL_SUMMARY:END -->
