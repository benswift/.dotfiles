#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11,<3.14"
# dependencies = ["replicate", "httpx", "typer"]
# ///
"""Generate styled images using various models via Replicate API.

Environment:
    REPLICATE_API_TOKEN: Your Replicate API token (required)

Supported models:
    banana    - Google Nano Banana Pro (default): style transfer, up to 14 ref images, 4K
    qwen      - Qwen Image 2 Pro: single reference image, strong text rendering, native 2K
    flux      - FLUX 2 Max: up to 8 ref images, resolution in MP
    seedream  - ByteDance Seedream 4.5: cinematic aesthetics, up to 14 ref images, 2K/4K
    gpt       - OpenAI GPT Image 1.5: all-round text-to-image + editing (limited aspect ratios)

Provide a single prompt. Optionally include one or more input images with
--input-image (repeatable) to transform or use as reference.

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
from urllib.parse import urlparse

MODELS: dict[str, str] = {
    "banana": "google/nano-banana-pro",
    "qwen": "qwen/qwen-image-2-pro",
    "flux": "black-forest-labs/flux-2-max",
    "seedream": "bytedance/seedream-4.5",
    "gpt": "openai/gpt-image-1.5",
}

app = typer.Typer()


def error_exit(message: str) -> NoReturn:
    print(f"Error: {message}", file=sys.stderr)
    sys.exit(1)


def get_api_token() -> str:
    token = os.environ.get("REPLICATE_API_TOKEN")
    if token:
        return token
    try:
        result = subprocess.run(
            ["fnox", "-P", "lazy", "get", "REPLICATE_API_TOKEN"],
            check=True,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        raise KeyError(
            "REPLICATE_API_TOKEN not set and fnox not installed. "
            "Get your token at https://replicate.com/account/api-tokens"
        )
    except subprocess.CalledProcessError as e:
        raise KeyError(
            f"fnox could not resolve REPLICATE_API_TOKEN: {e.stderr.strip()}"
        )
    return result.stdout.strip()


def slugify(text: str, max_words: int = 6) -> str:
    words = text.lower().split()[:max_words]
    slug = "-".join(words)
    slug = re.sub(r"[^a-z0-9-]", "-", slug)
    slug = re.sub(r"-+", "-", slug)
    slug = slug.strip("-")
    return slug if slug else "untitled"


def download_image(url: str, target_dir: Path, base_name: str) -> Path:
    ext = Path(urlparse(url).path).suffix.lower()
    if ext == ".jpeg":
        ext = ".jpg"
    elif ext not in (".jpg", ".png", ".webp"):
        ext = ".jpg"
    target_path = target_dir / f"{base_name}{ext}"
    response = httpx.get(url, timeout=60.0, follow_redirects=True)
    response.raise_for_status()
    target_path.write_bytes(response.content)
    return target_path


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
        params.pop("output_format", None)
        if image_paths:
            if len(image_paths) > 1:
                warn("qwen model supports only 1 input image; using the first")
            params["image"] = stack.enter_context(open(image_paths[0], "rb"))

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

    elif model == "seedream":
        params.pop("output_format", None)
        if resolution == "1K":
            warn("seedream does not support 1K; using 2K instead")
            params["size"] = "2K"
        else:
            params["size"] = resolution
        params["disable_safety_checker"] = safety_filter_level == "block_only_high"
        if image_paths:
            if len(image_paths) > 14:
                warn("seedream supports up to 14 input images; using first 14")
                image_paths = image_paths[:14]
            params["image_input"] = [
                stack.enter_context(open(p, "rb")) for p in image_paths
            ]

    elif model == "gpt":
        gpt_ratio_map = {
            "1:1": "1:1",
            "3:2": "3:2",
            "2:3": "2:3",
            "16:9": "3:2",
            "4:3": "3:2",
            "9:16": "2:3",
            "3:4": "2:3",
        }
        mapped = gpt_ratio_map.get(aspect_ratio, "1:1")
        if mapped != aspect_ratio:
            warn(f"gpt model does not support aspect ratio {aspect_ratio}; using {mapped}")
        params["aspect_ratio"] = mapped
        params["output_format"] = "jpeg" if output_format == "jpg" else output_format
        quality_map = {"1K": "medium", "2K": "high", "4K": "high"}
        params["quality"] = quality_map.get(resolution, "high")
        params["moderation"] = "low" if safety_filter_level == "block_only_high" else "auto"
        if image_paths:
            params["input_images"] = [
                stack.enter_context(open(p, "rb")) for p in image_paths
            ]

    return params


def extract_output(output: Any) -> str:
    if isinstance(output, list):
        return str(output[0])
    return str(output)


def generate_image(
    model: str,
    prompt: str,
    target_dir: Path,
    base_name: str,
    client: replicate.Client,
    aspect_ratio: str,
    resolution: str,
    output_format: str,
    safety_filter_level: str,
    input_image_paths: list[Path] | None = None,
) -> Path:
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

    image_url = extract_output(output)
    return download_image(image_url, target_dir, base_name)


@app.command()
def _main_impl(
    prompt: Annotated[str, typer.Argument(help="Image prompt")],
    model: Annotated[
        str,
        typer.Option(help="Model to use: banana, qwen, flux, seedream, gpt"),
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

      styled_image_gen.py "abstract pattern" --input-image refs/a.jpg --input-image refs/b.jpg
    """
    if model not in MODELS:
        error_exit(f"Unknown model '{model}'. Choose from: {', '.join(MODELS)}")

    all_input_images: list[Path] = list(input_image)

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
    if all_input_images:
        print(f"Input images: {', '.join(str(p) for p in all_input_images)}")
    print(f"Output directory: {target_dir}")
    print("Generating 1 image...")
    print()

    print(f"Prompt: {prompt}")

    downloaded_path = generate_image(
        model,
        prompt,
        target_dir,
        filename,
        client,
        aspect_ratio,
        resolution,
        "jpg",
        safety_filter_level,
        all_input_images if all_input_images else None,
    )

    final_path = downloaded_path
    if not jpg:
        if downloaded_path.suffix.lower() in (".jpg", ".jpeg", ".png"):
            avif_path = target_dir / f"{filename}.avif"
            print("Converting to AVIF...")
            convert_to_avif(downloaded_path, avif_path)
            final_path = avif_path
        else:
            warn(f"Cannot convert {downloaded_path.suffix} to AVIF; keeping as downloaded")

    print()
    print(f"Image generated: {final_path}")


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
