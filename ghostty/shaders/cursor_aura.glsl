// Cursor aura: a halo tinted by cursor shape, plus a ring that pulses out from
// the cursor whenever the window regains focus. Runs after cursor_warp.glsl,
// so iChannel0 here is the warp shader's output.
//
// The halo shows only while the real cursor is visible. Helix hides it in
// normal mode and draws its own block, so there the halo is absent; the bar
// (insert, shell prompt) and underline (select) get one.

// --- CONFIGURATION ---
// Sizes are in cursor-cell heights, so they scale with font size and DPI.
const float GLOW_RADIUS = 0.6;
const float GLOW_STRENGTH = 0.18;

const float PULSE_DURATION = 0.45;
const float PULSE_RADIUS = 4.0;
const float PULSE_THICKNESS = 0.25;
const float PULSE_STRENGTH = 0.5;

// Palette slots for each cursor shape (Catppuccin Mocha: green, pink, blue).
const int TINT_BAR = 2;
const int TINT_UNDERLINE = 5;
const int TINT_BLOCK = 4;

float sdfRect(vec2 p, vec2 center, vec2 halfSize) {
    vec2 d = abs(p - center) - halfSize;
    return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

vec3 shapeTint(int style) {
    if (style == CURSORSTYLE_BAR) return iPalette[TINT_BAR];
    if (style == CURSORSTYLE_UNDERLINE) return iPalette[TINT_UNDERLINE];
    return iPalette[TINT_BLOCK];
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    fragColor = texture(iChannel0, fragCoord / iResolution.xy);
    if (iFocus == 0) return;

    // iCurrentCursor.xy is the top-left corner in y-up pixels (as in
    // cursor_warp.glsl). A bar is a few pixels wide, so measure the halo from
    // a full-cell-height box rather than the bar itself.
    vec2 size = iCurrentCursor.zw;
    float cell = max(size.y, 1.0);
    vec2 center = iCurrentCursor.xy + vec2(size.x, -size.y) * 0.5;
    float d = sdfRect(fragCoord, center, size * 0.5) / cell;

    vec3 color = fragColor.rgb;

    if (iCursorVisible != 0 && d > 0.0) {
        float glow = GLOW_STRENGTH * exp(-d / GLOW_RADIUS * 3.0);
        color = mix(color, shapeTint(iCurrentCursorStyle), glow);
    }

    float t = (iTime - iTimeFocus) / PULSE_DURATION;
    if (t >= 0.0 && t < 1.0) {
        float eased = 1.0 - pow(1.0 - t, 3.0);
        float ring = abs(length(fragCoord - center) / cell - eased * PULSE_RADIUS);
        float alpha = PULSE_STRENGTH * (1.0 - t) * (1.0 - t)
            * (1.0 - smoothstep(0.0, PULSE_THICKNESS, ring));
        color = mix(color, iCurrentCursorColor.rgb, alpha);
    }

    fragColor = vec4(color, fragColor.a);
}
