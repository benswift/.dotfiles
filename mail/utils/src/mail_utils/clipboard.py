"""Cross-platform clipboard operations."""

import subprocess


def copy_to_clipboard(text: str) -> bool:
    """Copy text to the system clipboard.

    Tries platform-specific clipboard commands in order:
    - pbcopy (macOS)
    - xclip (X11 Linux)
    - wl-copy (Wayland Linux)

    Returns True if successful, False otherwise.
    """
    commands = [
        ["pbcopy"],
        ["xclip", "-selection", "clipboard"],
        ["wl-copy"],
    ]

    for cmd in commands:
        try:
            subprocess.run(cmd, input=text, text=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            continue

    return False
