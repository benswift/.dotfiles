# Voyager keyboard

Setup notes for my ZSA Voyager. The layout and all of its tap-hold behaviour
live in Oryx (ZSA's web configurator) --- there's no local build step, no
custom firmware code, and no flashing script: I flash with ZSA's Keymapp app.

> Previously this directory held `thumb-fix.c`, a per-key QMK patch applied at
> build time, because Oryx only exposed "Hold On Other Key Press" and "Tap
> Flow" as global switches. Oryx now exposes the tap-hold options natively and
> the layout has been reworked so none of those per-key overrides are needed,
> so the patch (and the `flash-voyager` build/flash pipeline that applied it)
> is gone. See the git history if you ever need it back.

## Tap-hold setup (all in Oryx)

Under Oryx's tap-hold / "Holding" settings:

- **Chordal Hold: on** --- the "opposite hands" rule. A dual-function key
  chorded with a *same-hand* key resolves as a tap (so fast rolls never
  misfire); chorded with the *opposite* hand it can resolve as a hold.
- **Permissive Hold: on** --- an opposite-hand chord settles as a hold as soon
  as the other key is pressed-and-released, so intentional chords are instant.
- **Tap Flow: off** --- the key change from the old setup. Tap Flow biased
  dual-function keys toward their tap during fast typing, which is what made
  the Cmd home-row mods drop on quick rolls.
- **Tapping term: ~200 ms** --- ZSA's recommended range with Chordal Hold.
  Intentional chords resolve on the nested keypress (Permissive Hold) rather
  than waiting this out, so the term mostly just sets the accidental-mod
  margin; the Cmd keys can sit a little higher, Shift a little lower.

### Modifiers

The design rule: chord every modifier with the **opposite hand**. Same-hand
presses are always letters, so fast typing is never obstructed.

- **Cmd (GUI)** on the pinky home keys, `a` / `;`.
- **Shift** on the index home keys, `f` / `j` (left = Left Shift, right = Right
  Shift). Capitalise with the opposite-hand Shift: capital B -> right Shift
  (`j`) + `b`; capital U -> left Shift (`f`) + `u`. A misfire here is just a
  stray capital, not a destructive backspace.
- **Ctrl / Tab** stay on the left-inner thumb (hold = Ctrl, tap = Tab), so
  one-handed Cmd+Tab (`a` + thumb Tab) and zellij's Ctrl chords are unchanged.
- **Left-outer thumb is plain Backspace** --- no longer a Shift/Backspace
  dual-function, which is what used to eat capitals.

### Combos

- `j`+`k` -> Esc.
- `f`+`j` -> Caps Word (both Shift keys), for acronyms and ALL-CAPS runs.

### The one trade-off

Tap Flow is global in Oryx, so turning it off turns it off everywhere. The
cost is that a few fast opposite-hand rolls through a home-row mod can misfire
(e.g. "ah" -> Cmd+H, or a stray capital). They're rare and, for Shift,
harmless. If they ever get annoying, the only fix that keeps Tap Flow off for
the Space/layer thumb is a small per-key `get_flow_tap_term` callback --- i.e.
bringing back a trimmed `thumb-fix.c`. Not expected to be necessary.

## Flashing

Oryx compiles server-side, so there's no toolchain to install. Flash with
**Keymapp** (ZSA's desktop app): open it, put the board into the bootloader
(tap `TG(2)` top-outer-left, then `QK_BOOT` top-outer-right, or press the reset
button on the top edge), and click flash.
