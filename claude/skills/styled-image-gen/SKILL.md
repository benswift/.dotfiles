# Styled image generation

Generate styled images using Replicate models. Useful for creating hero images,
slide backgrounds, illustrations, and stylised visuals for any project.

## Quick start

```bash
styled_image_gen.py "your prompt here"
styled_image_gen.py "your prompt" --preset anu --model banana
```

The script is at `~/.dotfiles/bin/styled_image_gen.py` (on PATH via `bin/`).

Requires `REPLICATE_API_KEY` environment variable.

## Models

| Model | `--model` | Best for | Image input | Max resolution |
|---|---|---|---|---|
| Google Nano Banana Pro | `banana` (default) | Style transfer with reference images | Up to 14 | 4K |
| Qwen Image | `qwen` | Single-image transformation | 1 only | quality/speed toggle |
| Google Imagen 4 Ultra | `imagen` | Text-to-image (no image input) | None | 2K |
| FLUX 2 Max | `flux` | Multi-reference generation | Up to 8 | 4 MP |

### Model trade-offs

- **banana** --- best balance of style transfer quality and resolution. Use with
  the `anu` preset for ANU-branded visuals. Supports up to 14 reference images,
  so you can combine preset + custom images.
- **qwen** --- fast single-image transformation. Only takes one input image. Use
  when you have a specific source image to restyle.
- **imagen** --- highest quality text-to-image but no image input at all. Use
  for prompts that don't need reference images. Caps at 2K.
- **flux** --- good multi-reference support (up to 8 images) with resolution in
  megapixels. Alternative to banana when you need different style characteristics.

## Common options

```
--model MODEL        banana (default), qwen, imagen, flux
--resolution RES     1K, 2K, or 4K (default: 4K)
--aspect-ratio AR    e.g. 16:9 (default), 4:3, 1:1, 9:16
--preset NAME        load reference images from a preset (e.g. anu)
--input-image PATH   add a reference image (repeatable)
--output-dir DIR     output directory (default: styled_image_gen_output)
--output-filename    custom filename (without extension)
--jpg                keep JPG instead of converting to AVIF
--safety-filter-level  block_low_and_above, block_medium_and_above, block_only_high
```

## Presets

Presets are directories of reference images in
`~/.dotfiles/bin/styled_image_gen_presets/`.

### `anu` preset

Three ANU-style reference images (bucket training, grid trigram, sampling
pattern). Produces visuals in the ANU School of Cybernetics aesthetic ---
geometric, generative, data-informed patterns.

## Output format

Images are converted to AVIF by default (quality 60) for excellent compression.
Use `--jpg` to keep the original JPG format. Requires `avifenc` (install with
`brew install libavif`).

Output structure: `<output-dir>/<iso-timestamp>/<slugified-prompt>.avif`

## Prompting guide

For best results with style transfer (banana, flux, qwen with image input):

- Describe the **desired output style**, not the reference images
- Be specific about colour palette, texture, and composition
- For abstract/geometric styles (like the ANU preset), prompts like "abstract
  geometric pattern with warm earth tones" work well
- Keep prompts concise --- 1--2 sentences is usually enough

## Marp slide deck workflow

When generating background images for Marp presentations:

### Aspect ratios for common slide sizes

| Slide size | Aspect ratio |
|---|---|
| 16:9 (default) | `--aspect-ratio 16:9` |
| 4:3 | `--aspect-ratio 4:3` |

### Using generated images as Marp backgrounds

```markdown
---
_backgroundImage: url(./styled_image_gen_output/2025-01-15T12-00-00Z/your-image.avif)
_backgroundSize: cover
---
```

### Batch workflow for slide decks

Generate multiple images for different slides, each with a different prompt but
the same preset and aspect ratio:

```bash
styled_image_gen.py "abstract network topology" --preset anu --output-filename slide-01
styled_image_gen.py "data flow visualisation" --preset anu --output-filename slide-02
```

Use `--output-filename` to get predictable filenames without timestamp
subdirectories.
