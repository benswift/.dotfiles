-- Rex config, read by the Rex server on each machine (Rex.app's own on daysy,
-- the systemd unit's on weddle). Everything not bound here is Rex's default.
-- After editing: `rex config check`, then `rex config reload` on that machine.

-- zellij's Alt layer: terminal as a tiling window manager.
rex.bind("alt+n", "pane.split", { direction = "auto" })
rex.bind("alt+h", "pane.focus", { direction = "left" })
rex.bind("alt+l", "pane.focus", { direction = "right" })
rex.bind("alt+j", "pane.focus", { direction = "down" })
rex.bind("alt+k", "pane.focus", { direction = "up" })
rex.bind("alt+f", "pane.minimize_all_floating")

-- Brackets by physical key: a bare `[` opens Rex's physical-key syntax.
rex.bind("alt+[BracketLeft]", "session.previous")
rex.bind("alt+[BracketRight]", "session.next")
