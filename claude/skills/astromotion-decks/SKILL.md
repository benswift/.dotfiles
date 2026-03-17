---
name: astromotion-decks
description:
  Authors and edits slide decks using the astromotion package (Astro + Svelte +
  Animotion/Reveal.js + Marp-inspired markdown syntax). Use when working with
  .deck.svx files, slide presentations, or when the user mentions decks,
  slides, or astromotion.
---

You author slide decks as `.deck.svx` files using the astromotion package.
Decks are markdown-first with optional Svelte/Animotion interactivity, compiled
into Reveal.js presentations at build time.

## File conventions

Decks live in `src/decks/`. Top-level files use the filename stem as the slug:

- `my-talk.deck.svx` -> `/decks/my-talk/`

Subdirectories also work for grouping related decks:

- `my-series/slides.deck.svx` -> `/decks/my-series/`
- `my-series/bonus.deck.svx` -> `/decks/my-series/bonus/`

Assets go in `src/decks/assets/` and are referenced with `./assets/` from deck
files.

## Deck file structure

```markdown
---
title: Deck Title
description: Optional description for meta tags
---

# First slide

Content here

---

## Second slide

More content

---

<script lang="ts">
  // optional: only needed for animotion interactivity
  import { tween } from "@animotion/motion";
  let cx = tween(100);
</script>

<h2>Interactive slide</h2>

<Action do={() => cx.to(300)}>
  Animate
</Action>
```

### Frontmatter

YAML frontmatter between `---` fences at the top of the file. Fields: `title`
(required), `description`, `author`, `image` (OG image path).

### Slide separators

Separate slides with a blank line, three dashes, blank line (`\n---\n`). This is
a markdown thematic break --- the same separator used by Marp.

## Markdown slides

Most slides are plain markdown. The preprocessor converts them to HTML and wraps
each in `<Slide>` components. Supported:

- Standard markdown: headings, lists, bold, italic, links, inline code
- GFM extensions: tables, strikethrough, task lists
- Fenced code blocks: converted to `<Code lang="..." theme="poimandres" />`
  animotion components with syntax highlighting
- Smart typography applied automatically (smart quotes, em dashes)

## Slide directives (HTML comments)

- `<!-- _class: impact -->` --- set the slide's CSS class
- `<!-- notes: Speaker notes here -->` --- presenter notes (visible in speaker
  view)

Place directives at the top of a slide (after the `---` separator).

### Available slide classes

- `impact` --- large, bold text for key statements
- `banner` --- full-bleed background with overlay text
- `quote` --- styled for quotations
- `centered` --- vertically and horizontally centred content
- `anu-logo` --- generates an animated ANU logo slide (no other content needed)
- `socy-logo` --- generates an animated School of Cybernetics logo slide

## Background images

Marp-inspired syntax using `![bg](url)` at the start of a slide:

```markdown
![bg](./assets/photo.avif)
```

### Variants

- `![bg](url)` --- full-bleed background (cover)
- `![bg contain](url)` --- contained background
- `![bg left:50%](url)` --- split layout, image on left taking 50% width
- `![bg right:40%](url)` --- split layout, image on right taking 40% width
- `![bg blur:5px brightness:0.7](url)` --- CSS filters on the background

### Image path resolution

- Relative paths (`./assets/photo.avif`) --- resolved as Vite imports from the
  deck file's location (typically `src/decks/assets/`)
- Absolute paths (`/images/photo.jpg`) --- reference files in `public/`
- Remote URLs (`https://...`) --- used directly

## QR codes

```markdown
![qr](https://example.com)
```

Generates an animated inline SVG QR code linking to the URL.

## Animotion interactive slides

Slides containing animotion components (`<Action>`, `<Code>`, `<Transition>`,
`<Embed>`, `<Recorder>`, `<Slides>`) skip markdown processing and pass through
as raw Svelte. Write these slides in HTML/Svelte, not markdown.

### Script blocks

Add a `<script>` block (with optional `lang="ts"`) anywhere in the file. It
applies to the whole deck. Auto-imports are added for animotion components you
use (`Presentation`, `Slide`, `Action`, `Code`, `Notes`, `Transition`,
`getPresentation`), so you only need to import extra dependencies.

Common imports for interactive slides:

```ts
import { tween } from "@animotion/motion";  // animated values
```

### Code component

```svelte
<Code
  bind:this={codeRef}
  lang="typescript"
  theme="poimandres"
  code={`const x = 1;`}
/>

<Action do={() => codeRef.update`const x = 2;`}>
  Update code
</Action>

<Action do={() => codeRef.selectLines`2`}>
  Highlight line 2
</Action>

<Action do={() => codeRef.selectToken`x`}>
  Highlight token
</Action>

<Action do={() => codeRef.selectLines`*`}>
  Clear selection
</Action>
```

### Tween animations

```svelte
<script lang="ts">
  import { tween } from "@animotion/motion";
  let x = tween(0);
  let color = tween("#be830e");
</script>

<svg viewBox="0 0 600 300" style="width: 100%; max-height: 50vh;">
  <circle cx={x.current} cy={150} r={40} fill={color.current} />
</svg>

<Action do={() => x.to(300)}>Move</Action>
<Action do={() => x.to(500).then(() => color.to("#0085ad"))}>Chain</Action>
```

### Style blocks

Add a `<style>` block for deck-scoped CSS. Placed after all slides in the
output.

## Layout CSS

These CSS classes are available in slide content:

- `.columns` --- two-column grid layout within a slide

Structural classes (set by the preprocessor, available for theme overrides):
`.slide-bg`, `.split-layout`, `.split-image`, `.split-content`, `.logo-svg`,
`.qr-code`.

## Theme

Decks use their own theme CSS, independent of the site's `global.css`. The theme
is configured in `astro.config.mjs` via `astromotion({ theme: "./path/to/theme.css" })`.
The theme file typically imports Tailwind (scoped to deck files only) and
animotion's base theme, then adds Reveal.js CSS variables and slide class styles.

## PDF export

```sh
node node_modules/astromotion/scripts/deck-pdf.mjs <slug> output.pdf
```

Builds the site, starts a preview server, and uses decktape to capture slides.

## Generating background images

Use `styled_image_gen.py` (the `styled-image-gen` skill) to generate background
images for slides. Output images directly into the deck's `assets/` directory.

```bash
styled_image_gen.py "abstract geometric network" \
  --preset anu \
  --aspect-ratio 16:9 \
  --output-dir src/decks/assets \
  --output-filename slide-01
```

- Use `--output-filename` to get predictable filenames without timestamp
  subdirectories
- Use `--aspect-ratio 16:9` for full-bleed backgrounds (`![bg]`)
- Use `--aspect-ratio 3:4` or `9:16` for split layouts (`![bg left:50%]`,
  `![bg right:50%]`) --- taller images fill the split panel better
- Use `--resolution 4K` (the default) --- high-res images look sharp on large screens
- Default AVIF output is ideal for decks (small file size, good quality)
- Use `--preset anu` for ANU-branded geometric/generative visuals

### Style prompts

Sites and decks can specify default prompt fragments that get appended to every
image generation prompt, ensuring a consistent aesthetic.

- **Site-wide**: add an `## Image generation style` section to the project's
  `CLAUDE.md` with the default prompt fragment (e.g. "watercolour illustration
  with muted earth tones and soft edges")
- **Per-deck**: create an `image-style.txt` file in the deck directory
  (`src/decks/image-style.txt`) containing the prompt fragment

When generating images, concatenate: `<slide-specific prompt>, <deck style>, <site style>`. Per-deck style overrides site-wide if both exist, or they can
be combined if complementary.

Then reference the generated image in a slide:

```markdown
![bg](./assets/slide-01.avif)

# Slide title
```

For batch generation across multiple slides, run multiple commands with different
prompts and `--output-filename` values, all targeting the same `assets/`
directory.

## Key constraints

- No SSR --- decks render client-side only (`client:only="svelte"`)
- No `<ClientRouter />` --- would conflict with Reveal.js keyboard navigation
- Tailwind is scoped to CSS files containing `@import "tailwindcss"` --- the
  rest of the Astro site is unaffected
- Presentation dimensions are 1280x720 with zero margin
- The upstream animotion docs are at https://animotion.pages.dev/llms.txt
