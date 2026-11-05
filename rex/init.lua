-- Rex config, read by the Rex server on each machine (Rex.app's own on daysy,
-- the systemd unit's on weddle). Everything not bound here is Rex's default.
-- After editing: `rex config check`, then `rex config reload` on that machine.

-- zellij's Alt layer: terminal as a tiling window manager.
rex.bind("alt+n", "pane.split", { direction = "auto" })
rex.bind("alt+h", "pane.focus", { direction = "left" })
rex.bind("alt+l", "pane.focus", { direction = "right" })
rex.bind("alt+j", "pane.focus", { direction = "down" })
rex.bind("alt+k", "pane.focus", { direction = "up" })
rex.bind("alt+f", "toggle_floating")

-- Sidebar order, not most-recent: Rex can't tell config which session the app
-- is showing, so a last-session toggle isn't possible.
rex.bind("ctrl+j", "session.next")
rex.bind("ctrl+k", "session.previous")

-- zellij's Alt f: toggle the active window's floating panes, opening one
-- (in the focused pane's cwd) when there are none.
rex.action{
  name  = "toggle_floating",
  title = "Toggle Floating Panes",
  run   = function(ctx, args)
    local view, err = rex.session.view{}
    if err ~= nil then error(err, 0) end
    local window
    for _, w in ipairs(view.windows) do
      if w.window_id == view.active_window_id then window = w end
    end
    if window == nil then error("no active window", 0) end

    local floats, shown = {}, false
    for _, l in ipairs(window.layers) do
      if l.kind ~= "tiled" then
        table.insert(floats, l)
        if l.state ~= "minimized" then shown = true end
      end
    end

    if #floats == 0 then
      local cwd
      local p = rex.block.call("com.superlogical.terminal", "process",
        { block_id = window.focused_block_id })
      if p ~= nil and p.foreground ~= nil then cwd = p.foreground.cwd end
      local _, lerr = rex.session.new_layer{
        window_id = window.window_id,
        bounds = { 0.15, 0.15, 0.7, 0.7 },
        focus = true,
        layout = rex.layout.block{
          flavor = "com.superlogical.terminal.shell",
          options = { cwd = cwd },
        },
      }
      if lerr ~= nil then error(lerr, 0) end
      return
    end

    for i, l in ipairs(floats) do
      local _, serr = rex.session.set_layer_state{
        layer_id = l.layer_id,
        state = shown and "minimized" or "normal",
        focus = (not shown) and i == #floats,
      }
      if serr ~= nil then error(serr, 0) end
    end
  end,
}
