# Voyager firmware

A build-time fix for the ZSA Voyager's thumb-key tap/hold behaviour, plus the
`flash-voyager` script that applies it.

## Why this exists

Oryx exposes "Hold On Other Key Press" and "Tap Flow" only as global switches,
but two of the Voyager's thumb dual-function keys need *per-key* behaviour:

- `MT(MOD_LSFT, KC_BSPC)` (Shift / Backspace) --- needs hold-on-other-key-press
  so a fast Shift+letter roll produces a capital, not Backspace + lowercase.
- `LT(1, KC_SPACE)` (Layer 1 / Space) --- needs exemption from Tap Flow so the
  layer hold isn't demoted to Space during fast typing.

`thumb-fix.c` supplies both as QMK per-key callbacks. It is layout-independent
--- it keys off the thumb keycodes, not the layout array --- so Oryx stays the
source of truth for the layout and the fix is re-applied mechanically on every
build. Editing the layout in Oryx never invalidates it, and you keep using the
Oryx GUI as normal.

## One-time setup

```sh
flash-voyager --setup
```

Installs the QMK CLI (`brew install qmk/qmk/qmk`) and clones ZSA's QMK fork
(`zsa/qmk_firmware`, branch `firmware25`) to `~/qmk_firmware`. The clone is
large (~1 GB) and takes a few minutes.

If Oryx later bumps the firmware version, re-run setup with the matching
branch, e.g. `ZSA_BRANCH=firmware26 flash-voyager --setup`.

## Usage

After editing the layout in Oryx, click Compile, then copy the "Download
Source" link (or download the `.zip`):

```sh
flash-voyager 'https://oryx.zsa.io/source/XXXXXX'   # URL, or a path to the .zip
flash-voyager                                       # reuse the last source
flash-voyager --compile-only 'https://...'           # build without flashing
```

Flashing requires the board in bootloader mode first --- easiest is the
keymap's `QK_BOOT` keycode: tap `TG(2)` (top-outer-left), then `QK_BOOT`
(top-outer-right); the keyboard goes quiet and waits. Alternatively, press
the reset button on the Voyager's top edge just before running. The script
locates the bootloader by vid:pid and invokes `dfu-util` directly, because
`qmk flash` can't disambiguate when both the Voyager's run-time DFU
interface and the bootloader are visible at once.

The Oryx source URL is keyed by build revision, not by the stable layout hash,
so it changes every time you recompile in Oryx --- hence passing it each time.

## Keep this baseline in Oryx

The snippet assumes **Hold On Other Key Press OFF** and **Tap Flow ON**. Leave
Tap Flow on --- you want it for the home-row mods; the snippet just carves out
the thumbs.

## If you rebind a thumb key

`thumb-fix.c` hardcodes `MT(MOD_LSFT, KC_BSPC)` and `LT(1, KC_SPACE)`. If you
change either thumb assignment in Oryx, update the `case` labels to match.
`flash-voyager` prints a warning if those keycodes are no longer in the layout.
