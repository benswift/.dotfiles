"""mu (maildir-utils) integration."""

import subprocess


def find_message_path(message_id: str) -> str | None:
    """Find the file path of a message by its Message-ID using mu.

    Returns the file path or None if not found.
    """
    result = subprocess.run(
        ["mu", "find", "-f", "l", f"msgid:{message_id}"],
        capture_output=True,
        text=True,
    )

    if result.returncode == 0 and result.stdout.strip():
        return result.stdout.strip().split("\n")[0].strip()

    return None
