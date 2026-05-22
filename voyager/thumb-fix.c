/*
 * Voyager thumb-key tap/hold fix.
 *
 * Appended to the Oryx-generated keymap.c by the `flash-voyager` script.
 * Oryx exposes "Hold On Other Key Press" and "Tap Flow" only as global
 * switches; these per-key QMK callbacks override them for the thumb
 * dual-function keys without touching the layout itself.
 *
 * Assumes the Oryx baseline: HOOKP off, Tap Flow on.
 * See voyager/README.md in the dotfiles repo.
 */

#ifndef FLOW_TAP_TERM
#    define FLOW_TAP_TERM 150
#endif

/*
 * Left inner thumb --- Shift (hold) / Backspace (tap).
 * Commit the hold the instant another key is pressed, so a fast
 * Shift+letter roll yields a capital instead of Backspace + lowercase.
 * HOOKP stays off everywhere else --- it would re-break the home-row mods.
 */
bool get_hold_on_other_key_press(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case MT(MOD_LSFT, KC_BSPC):
            return true;
        default:
            return false;
    }
}

/*
 * Exempt the thumb dual-function keys from Tap Flow, which otherwise
 * demotes their hold to a tap during fast typing. Every other key keeps
 * the normal term, so the home-row mods still get Tap Flow.
 */
uint16_t get_flow_tap_term(uint16_t keycode, keyrecord_t *record,
                           uint16_t prev_keycode) {
    switch (keycode) {
        case MT(MOD_LSFT, KC_BSPC):  /* Shift / Backspace */
        case LT(1, KC_SPACE):        /* Layer 1 / Space   */
            return 0;
        default:
            return FLOW_TAP_TERM;
    }
}
