#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.14"
# dependencies = [
#     "typer",
#     "transformers",
#     "torch",
#     "accelerate",
#     "soundfile",
# ]
#
# [tool.uv]
# extra-index-url = ["https://download.pytorch.org/whl/cu124"]
# ///
# nb-capture-todo.py --- create nb todos with optional audio transcription
#
# Runs on weddle as a fire-and-forget script invoked from an iOS Shortcut
# via SSH. Accepts a title plus optional text body or audio file. Audio is
# transcribed locally using Whisper large-v3 via HuggingFace Transformers
# on GPU.
#
# On first run, the model weights are downloaded from HuggingFace (~3 GB).
# Subsequent runs use the cached model.
#
# Requires: CUDA GPU, nb CLI in PATH.
#
# All arguments are URL-encoded by the iOS shortcut to avoid shell quoting
# issues. The script decodes them automatically.
#
# On any failure, an error TODO is created so nothing is silently lost.

import subprocess
import sys
from pathlib import Path
from typing import Annotated
from urllib.parse import unquote

import typer

app = typer.Typer(add_completion=False)

WHISPER_INITIAL_PROMPT = (
    "Vite, uv, Nix, Elixir, Phoenix, LiveView, Tailwind, polars, pydantic, "
    "Zellij, mise, nb, Tailscale, Astro, Svelte, PostgreSQL, SQLite, "
    "FastAPI, httpx, pytest, Playwright, Claude, Anthropic"
)


def transcribe(audio_path: Path) -> str:
    import torch
    from transformers import AutoModelForSpeechSeq2Seq, AutoProcessor, pipeline

    model_id = "openai/whisper-large-v3"
    device = "cuda" if torch.cuda.is_available() else "cpu"
    torch_dtype = torch.float16 if device == "cuda" else torch.float32

    model = AutoModelForSpeechSeq2Seq.from_pretrained(
        model_id, dtype=torch_dtype, low_cpu_mem_usage=True
    ).to(device)
    processor = AutoProcessor.from_pretrained(model_id)

    pipe = pipeline(
        "automatic-speech-recognition",
        model=model,
        tokenizer=processor.tokenizer,
        feature_extractor=processor.feature_extractor,
        dtype=torch_dtype,
        device=device,
    )

    prompt_ids = processor.get_prompt_ids(WHISPER_INITIAL_PROMPT, return_tensors="pt").to(device)
    result = pipe(
        str(audio_path),
        generate_kwargs={
            "language": "en",
            "task": "transcribe",
            "prompt_ids": prompt_ids,
        },
    )
    return result["text"].strip()


def run_nb(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["nb", *args], capture_output=True, text=True, check=True)


def create_error_todo(error: str) -> None:
    try:
        subprocess.run(
            [
                "nb", "todo", "add", "nb-capture-todo processing failed",
                "--description", error, "--tags", "inbox,error",
            ],
            check=False,
            capture_output=True,
        )
        subprocess.run(["nb", "sync"], check=False, capture_output=True)
    except Exception:
        print(f"FATAL: could not create error todo: {error}", file=sys.stderr)


@app.command()
def main(
    title: Annotated[str, typer.Option(help="Todo title (URL-encoded)")],
    body: Annotated[str | None, typer.Option(help="Text body (URL-encoded)")] = None,
    audio: Annotated[
        Path | None, typer.Option(help="Path to audio file for transcription")
    ] = None,
    url: Annotated[str | None, typer.Option(help="URL to include (URL-encoded)")] = None,
) -> None:
    """Create an nb todo with optional audio transcription."""
    title = unquote(title)
    if body:
        body = unquote(body)
    if url:
        url = unquote(url)

    try:
        description_parts: list[str] = []

        if audio:
            transcription = transcribe(audio)
            description_parts.append(transcription)
            audio.unlink(missing_ok=True)
        elif body:
            description_parts.append(body)

        if url:
            description_parts.append(f"\nurl: {url}")

        args = ["todo", "add", title, "--tags", "inbox"]
        description = "\n".join(description_parts).strip()
        if description:
            args.extend(["--description", description])

        run_nb(*args)
        run_nb("sync")
    except Exception as e:
        create_error_todo(f"title: {title}\nerror: {e}")
        sys.exit(1)


if __name__ == "__main__":
    app()
