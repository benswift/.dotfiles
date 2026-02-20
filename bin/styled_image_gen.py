#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["replicate", "httpx", "typer"]
# ///
"""Generate styled images using various models via Replicate API.

Environment:
    REPLICATE_API_KEY: Your Replicate API token (required)

Supported models:
    banana  - Google Nano Banana Pro (default): style transfer, up to 14 ref images, 4K
    qwen    - Qwen Image: quality/speed toggle, 1 input image only
    imagen  - Google Imagen 4 Ultra: text-to-image only, max 2K
    flux    - FLUX 2 Max: up to 8 ref images, resolution in MP

Provide a single prompt. Optionally include one or more input images with
--input-image (repeatable) to transform or use as reference. Use --preset to
include predefined reference images from a preset.

Generated images will be saved to:
    <output-dir>/<iso-timestamp>/<slugified-image-prompt>.avif
"""

import datetime
import os
import re
import subprocess
import sys
from contextlib import ExitStack
from pathlib import Path
from typing import Annotated, Any, NoReturn

import httpx
import replicate
import typer
from replicate.exceptions import ReplicateError

MODELS: dict[str, str] = {
    "banana": "google/nano-banana-pro",
    "qwen": "qwen/qwen-image",
    "imagen": "google/imagen-4-ultra",
    "flux": "black-forest-labs/flux-2-max",
}

PRESETS_DIR = Path(__file__).parent / "styled_image_gen_presets"

app = typer.Typer()


def get_preset_images(preset_name: str) -> list[Path]:
    preset_dir = PRESETS_DIR / preset_name
    if not preset_dir.is_dir():
        available = [d.name for d in PRESETS_DIR.iterdir() if d.is_dir()]
        raise FileNotFoundError(
            f"Preset '{preset_name}' not found. Available: {', '.join(available) or 'none'}"
        )
    image_extensions = {".jpg", ".jpeg", ".png", ".webp"}
    images = [
        p for p in preset_dir.iterdir() if p.suffix.lower() in image_extensions
    ]
    return sorted(images)


def error_exit(message: str) -> NoReturn:
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(1)


def get_api_token() -> str:
    try:
        return os.environ["REPLICATE_API_KEY"]
    except KeyError:
        raise KeyError(
            "REPLICATE_API_KEY environment variable not set. "
            "Get your token at https://replicate.com/account/api-tokens"
        )


def slugify(text: str, max_words: int = 6) -> str:
    words = text.lower().split()[:max_words]
    slug = "-".join(words)
    slug = re.sub(r"[^a-z0-9-]", "-", slug)
    slug = re.sub(r"-+", "-", slug)
    slug = slug.strip("-")
    return slug if slug else "untitled"


def download_image(url: str, output_path: Path) -> None:
    response = httpx.get(url, timeout=60.0, follow_redirects=True)
    response.raise_for_status()
    output_path.write_bytes(response.content)


AVIF_QUALITY = 60


def convert_to_avif(jpg_path: Path, avif_path: Path) -> None:
    subprocess.run(
        ["avifenc", "-q", str(AVIF_QUALITY), str(jpg_path), str(avif_path)],
        check=True,
        capture_output=True,
    )
    jpg_path.unlink()


def warn(message: str) -> None:
    print(f"Warning: {message}", file=sys.stderr)


def build_model_input(
    model: str,
    prompt: str,
    aspect_ratio: str,
    resolution: str,
    output_format: str,
    safety_filter_level: str,
    image_paths: list[Path],
    stack: ExitStack,
) -> dict[str, Any]:
    params: dict[str, Any] = {
        "prompt": prompt,
        "aspect_ratio": aspect_ratio,
        "output_format": output_format,
    }

    if model == "banana":
        params["resolution"] = resolution
        params["safety_filter_level"] = safety_filter_level
        if image_paths:
            params["image_input"] = [
                stack.enter_context(open(p, "rb")) for p in image_paths
            ]

    elif model == "qwen":
        if resolution in ("2K", "4K"):
            params["image_size"] = "optimize_for_quality"
        else:
            params["image_size"] = "optimize_for_speed"
        if safety_filter_level == "block_only_high":
            params["disable_safety_checker"] = True
        else:
            params["disable_safety_checker"] = False
        if image_paths:
            if len(image_paths) > 1:
                warn("qwen model supports only 1 input image; using the first")
            params["image"] = stack.enter_context(open(image_paths[0], "rb"))

    elif model == "imagen":
        if resolution == "4K":
            warn("imagen model caps at 2K; using 2K instead")
            params["image_size"] = "2K"
        else:
            params["image_size"] = resolution
        params["safety_filter_level"] = safety_filter_level
        if image_paths:
            warn("imagen model does not support image input; ignoring input images")

    elif model == "flux":
        mp_map = {"1K": "1 MP", "2K": "2 MP", "4K": "4 MP"}
        params["resolution"] = mp_map.get(resolution, "4 MP")
        tolerance_map = {
            "block_low_and_above": 1,
            "block_medium_and_above": 3,
            "block_only_high": 5,
        }
        params["safety_tolerance"] = tolerance_map.get(safety_filter_level, 5)
        if image_paths:
            if len(image_paths) > 8:
                warn("flux model supports up to 8 input images; using first 8")
                image_paths = image_paths[:8]
            params["input_images"] = [
                stack.enter_context(open(p, "rb")) for p in image_paths
            ]

    return params


def extract_output(model: str, output: Any) -> str:
    if model == "qwen":
        return str(output[0])
    return str(output)


def generate_image(
    model: str,
    prompt: str,
    output_path: Path,
    client: replicate.Client,
    aspect_ratio: str,
    resolution: str,
    output_format: str,
    safety_filter_level: str,
    input_image_paths: list[Path] | None = None,
) -> None:
    image_paths = input_image_paths or []
    with ExitStack() as stack:
        model_input = build_model_input(
            model,
            prompt,
            aspect_ratio,
            resolution,
            output_format,
            safety_filter_level,
            image_paths,
            stack,
        )
        output = client.run(MODELS[model], input=model_input)

    if not output:
        raise ValueError("Model returned empty output")

    image_url = extract_output(model, output)
    download_image(image_url, output_path)


@app.command()
def _main_impl(
    prompt: Annotated[str, typer.Argument(help="Image prompt")],
    model: Annotated[
        str,
        typer.Option(help="Model to use: banana, qwen, imagen, flux"),
    ] = "banana",
    output_dir: Annotated[Path, typer.Option(help="Output directory")] = Path(
        "styled_image_gen_output"
    ),
    input_image: Annotated[
        list[Path],
        typer.Option(
            "--input-image",
            help="Input image(s) for reference (repeat for multiple)",
            exists=True,
            dir_okay=False,
        ),
    ] = [],
    preset: Annotated[
        str | None,
        typer.Option(
            help="Load reference images from a preset (added before --input-image)"
        ),
    ] = None,
    aspect_ratio: Annotated[
        str,
        typer.Option(help="Aspect ratio for generated images"),
    ] = "16:9",
    resolution: Annotated[
        str,
        typer.Option(help="Resolution: 1K, 2K, or 4K"),
    ] = "4K",
    jpg: Annotated[
        bool,
        typer.Option("--jpg", help="Keep original JPG instead of converting to AVIF"),
    ] = False,
    output_filename: Annotated[
        str | None,
        typer.Option(
            help="Output filename (without extension, defaults to slugified prompt)"
        ),
    ] = None,
    safety_filter_level: Annotated[
        str,
        typer.Option(
            help="Safety filter: block_low_and_above, block_medium_and_above, block_only_high"
        ),
    ] = "block_only_high",
) -> None:
    """Generate a styled image using Replicate models.

    Examples:

      styled_image_gen.py "sunrise over a misty valley" --resolution 2K

      styled_image_gen.py "studio portrait" --model flux --input-image ref.jpg

      styled_image_gen.py "abstract pattern" --preset anu --model banana
    """
    if model not in MODELS:
        error_exit(f"Unknown model '{model}'. Choose from: {', '.join(MODELS)}")

    all_input_images: list[Path] = []
    if preset:
        all_input_images.extend(get_preset_images(preset))
    all_input_images.extend(input_image)

    api_token = get_api_token()

    if output_filename:
        target_dir = output_dir
    else:
        timestamp = datetime.datetime.now(datetime.timezone.utc).strftime(
            "%Y-%m-%dT%H-%M-%SZ"
        )
        target_dir = output_dir / timestamp
    target_dir.mkdir(parents=True, exist_ok=True)

    client = replicate.Client(api_token=api_token)

    output_format = "jpg" if jpg else "avif"
    filename = output_filename if output_filename else slugify(prompt)

    print(f"Model: {MODELS[model]} ({model})")
    print(f"Aspect ratio: {aspect_ratio}")
    print(f"Resolution: {resolution}")
    print(f"Format: {output_format}")
    print(f"Safety filter: {safety_filter_level}")
    if preset:
        print(f"Preset: {preset}")
    if all_input_images:
        print(f"Input images: {', '.join(str(p) for p in all_input_images)}")
    print(f"Output directory: {target_dir}")
    print("Generating 1 image...")
    print()

    jpg_path = target_dir / f"{filename}.jpg"
    final_path = target_dir / f"{filename}.{output_format}"

    print(f"Prompt: {prompt}")
    print(f"Output: {final_path}")

    generate_image(
        model,
        prompt,
        jpg_path,
        client,
        aspect_ratio,
        resolution,
        "jpg",
        safety_filter_level,
        all_input_images if all_input_images else None,
    )

    if not jpg:
        print("Converting to AVIF...")
        convert_to_avif(jpg_path, final_path)

    print()
    print("Image generated successfully")


if __name__ == "__main__":
    try:
        app()
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        sys.exit(130)
    except KeyError as e:
        error_exit(str(e))
    except ValueError as e:
        error_exit(str(e))
    except FileNotFoundError as e:
        if "avifenc" in str(e):
            error_exit("avifenc not found. Install with: brew install libavif")
        error_exit(f"File not found: {e}")
    except subprocess.CalledProcessError as e:
        error_exit(f"AVIF conversion failed: {e.stderr.decode() if e.stderr else e}")
    except OSError as e:
        error_exit(f"File operation failed: {e}")
    except httpx.HTTPError as e:
        error_exit(f"HTTP request failed: {e}")
    except ReplicateError as e:
        error_exit(f"Replicate API error: {e}")
