# svg-gen skill design

Date: 2026-04-16 Status: approved (design), pending implementation Owner: Ben

## Goal

A new skill, `ben:svg-gen`, for generating SVG illustrations directly from
Claude (no external API). Primary use case: illustrations for blog posts and
slide decks. Aesthetic goal: flowing, curvilinear, impressionistic --- the
opposite of AI-slop infographics, stock tech illustrations, or scientific
diagrams.

Complements the existing `ben:styled-image-gen` skill (which handles raster
generation via Replicate). No changes to that skill.

## Non-goals

- Not a diagramming tool (no boxes-and-arrows, flowcharts, UML).
- Not minification (SVGs stay readable with 2-space indent).
- Not tracing photographs --- raster references serve as mood/composition cues,
  not input to be imitated literally.
- Not a revision/iteration workflow on existing SVGs. Each run produces a new
  SVG. (YAGNI until asked.)

## Architecture

Two artefacts:

1. **Skill**: `~/.claude/plugins/marketplaces/ben/skills/svg-gen/SKILL.md`.
   Exposed to the model as `ben:svg-gen`. Edits and commits happen in the
   marketplace clone (per dotfiles README).
2. **Validator script**: `~/.dotfiles/bin/svg_validate.py`. A single-file uv
   script in the same style as `styled_image_gen.py`. On PATH. Deps: `lxml`,
   `typer`.

No separate "generator" script. Claude produces SVG inline via the Write tool,
then runs the validator to check + pretty-print.

## Workflow (what SKILL.md teaches)

When the user asks for an SVG illustration:

1. **Read project style.** Open project `CLAUDE.md`, look for
   `## SVG illustration style`. If absent and the project could use one, offer
   to add it --- otherwise proceed with the user's inline prompt alone.
2. **Pick 1--4 references** from the configured `References:` dir (if any). Mix
   of `.svg` and raster files is fine.
   - SVG refs: Read as text; absorb structure, palette, path style.
   - Raster refs: Read multimodally (local paths only --- if the user provides a
     URL, download it to a working dir first, then Read). Raster refs inform
     composition/mood, not literal tracing.
3. **Choose aspect ratio + viewBox.** `--aspect-ratio W:H` or accept defaults:
   - `16:9` (default) --- slide backgrounds, hero images
   - `3:4` / `9:16` --- portrait, split layouts
   - `1:1` --- tiles, avatars
   - Arbitrary `W:H` accepted. Base unit: 1600 on the short edge. So `16:9` →
     `viewBox="0 0 2844 1600"`; `3:4` → `viewBox="0 0 1600 2133"`.
4. **Compose the SVG** via Write. Structure:
   - Root `<svg viewBox="...">` with no fixed `width`/`height` (so it scales).
   - `<defs>` declaring palette colours as gradients or named swatches.
   - Layered `<g>` groups back-to-front (backdrop → mid → detail).
   - Cubic Bézier paths (`C`/`S` commands) for organic contours.
   - 2-space indent.
5. **Validate.** Run `svg_validate.py --fix <path>`, passing the CLAUDE.md
   palette (if any) as `--palette`. On errors, fix via Edit and re-run. On
   warnings, apply judgement --- warnings are advisory, not blocking.
6. **Report the final path** to the user.

## Output conventions

Mirror `styled-image-gen`:

- Default: `svg_gen_output/<iso-timestamp>/<slugified-prompt>.svg`
- With `--output-filename <name>`: `<output-dir>/<name>.svg`
- The validator's `--fix` owns formatting: saved files are always
  pretty-printed. Claude's initial `Write` output format doesn't matter --- the
  validator rewrites the file immediately after.

## CLAUDE.md convention

Separate from the existing `## Image generation style` section. Example:

```markdown
## SVG illustration style

Palette: #b58900 #cb4b16 #dc322f #268bd2 #6c71c4 Prompt suffix: flowing curves,
risograph overprint, organic silhouettes References: src/assets/illustrations/
```

All fields optional. Parsing is informal, line-based, matching the existing
raster convention.

Field rules:

- **Palette**: space- or comma-separated hex codes. OR a named token (e.g.
  `solarized-warm`, `gruvbox-soft`). Unknown names: Claude treats the token as a
  keyword in the prompt suffix rather than a failure. When present, the
  validator's `--palette` flag is populated with the hex list so it can check
  colour fidelity.
- **Prompt suffix**: free text, appended to the user's inline prompt.
- **References**: a directory path, or comma-separated list of paths. Claude
  scans it for `.svg` and raster files (`.jpg`, `.jpeg`, `.png`, `.webp`,
  `.avif`).

If no section exists, the skill still works --- just with no declared palette or
references.

## Validator (`svg_validate.py`)

Single-file uv script. Mirrors `styled_image_gen.py` shape: typer CLI, clear
error messages, non-zero exit on failure, stderr warnings.

CLI:

```
svg_validate.py <path> [--fix] [--palette HEX[,HEX,...]]
                        [--max-nodes N] [--strict]
```

Options:

- `--fix`: reformat in place with `lxml.etree.tostring(pretty_print=True)`.
  2-space indent, attribute order preserved. Writes back to the same path.
- `--palette`: comma-separated hex list to check against.
- `--max-nodes`: override default 500.
- `--strict`: promote all warnings to errors. For CI use.

### Invariants

Exit 1 on error. Exit 0 with warnings on stderr otherwise (or exit 1 if
`--strict`).

| Check                                                                    | Error / Warn |
| ------------------------------------------------------------------------ | ------------ |
| XML well-formed                                                          | Error        |
| Root element is `<svg>`                                                  | Error        |
| `viewBox` attribute present on root                                      | Error        |
| No `<script>` elements                                                   | Error        |
| No `javascript:`, `data:text/html`, cross-origin `href`                  | Error        |
| External `href` (absolute URL, `file://`)                                | Warn         |
| Embedded raster `<image>` element                                        | Warn         |
| Element count (excluding whitespace text nodes) exceeds `--max-nodes`    | Warn         |
| Duplicate `<path>` elements (same `d`, `fill`, `stroke`, `stroke-width`) | Warn         |
| Fill/stroke colours outside declared palette                             | Warn         |

**Palette comparison**: any colour within ΔE-76 ≤ 3 of a palette entry counts as
in-palette. Pure `none` and `currentColor` always pass. Colours inside
`url(#gradient-id)` refs are not checked directly --- only the gradient stops
are.

**Output format** (on stdout):

```
$ svg_validate.py --fix illustrations/loom.svg
✓ well-formed XML
✓ root is <svg> with viewBox="0 0 1600 900"
✓ no <script> / no external href
✓ palette: 4 unique colours, all within declared swatches
⚠ warn: 2 near-duplicate <path> subtrees (possible repetition)
✓ node count: 142 (limit 500)
formatted: 2-space indent
```

## Aesthetic guidance baked into SKILL.md

One page, three parts: philosophy paragraph, do/don't table, three tiny code
fragments.

### Philosophy (draft)

> These are illustrations, not diagrams. Prefer flow to corners, layers to flat
> compositions, organic asymmetry to grid-aligned order. Reach for cubic Béziers
> by default, extend silhouettes past the edges of the viewBox, and declare a
> tight palette in `<defs>` rather than picking colours ad hoc per path. Treat
> the reference set as a mood palette: you are reinterpreting, not tracing.

### Do / don't table

| Do                                        | Don't                                         |
| ----------------------------------------- | --------------------------------------------- |
| Cubic Bézier `C`/`S` for organic contours | `<line>` or polyline for living things        |
| 3--6 palette colours declared in `<defs>` | Random per-path hex                           |
| Extend shapes past viewBox edges          | 10% inner margin with everything tucked in    |
| 2--7° rotations, offset group centres     | Strict symmetry, grid layout                  |
| Linear/radial gradients, layered `<g>`    | Drop-shadow icon slop, stock lozenges         |
| A few hand-tuned anchor points            | Dense node counts from autotrace-style output |

### Code fragments (illustrative, to be included verbatim)

- Palette in `<defs>` (linearGradient + solid swatches via `<rect fill>`).
- A single organic silhouette via cubic Bézier curves.
- Layered `<g>` groups with small rotation (`transform="rotate(3 400 400)"`).

## Acceptance / dogfood

Once skill + validator are built, verify against two targets:

1. **Fresh generation**: an SVG prompted from scratch against a palette but no
   references. Verify validator passes, output reads well visually.
2. **Reinterpreted headshot**: using Ben's line-drawing headshot
   (`https://benswift.me/assets/images/headshots/headshot-line-drawing.webp`) as
   a raster reference. Download locally, run the skill to produce an SVG
   "interpretation" in a curvilinear/impressionistic style. This is the concrete
   dogfood artefact demonstrating raster → SVG reinterpretation.

## Open questions (resolved in brainstorming)

- Plugin placement: `ben` plugin. ✓
- CLAUDE.md section: separate `## SVG illustration style`. ✓
- Validator: tiny custom script (not svgo, not plain xmllint). ✓
- Aesthetic guidance: principles + do/don't + tiny fragments. ✓
- Reference types: `.svg` and raster both accepted. ✓

## Out of scope

- A scaffolder CLI that emits starter SVGs (considered and rejected --- fights
  the "not boxes and lines" goal).
- Minification.
- Revision workflow on existing SVGs.
- Integration with astromotion decks beyond the CLAUDE.md convention (decks can
  use generated SVGs like any other asset).
