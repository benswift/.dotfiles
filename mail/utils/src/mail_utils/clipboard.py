"""Cross-platform clipboard operations."""

import base64
import os
import subprocess


def _osc52_sequence(text: str) -> str:
    b64 = base64.b64encode(text.encode()).decode("ascii")
    seq = f"\033]52;c;{b64}\a"
    # tmux and GNU screen need the sequence wrapped in their passthrough escape.
    if os.environ.get("TMUX"):
        seq = f"\033Ptmux;\033{seq}\033\\"
    elif os.environ.get("TERM", "").startswith("screen"):
        seq = f"\033P{seq}\033\\"
    return seq


def copy_to_clipboard(text: str) -> bool:
    """Copy text to the system clipboard.

    Prefers an OSC 52 escape sequence written to the controlling terminal,
    which works both locally and over SSH in terminals that support it (e.g.
    ghostty). Falls back to native clipboard commands when there is no
    terminal to receive the sequence:

    - pbcopy (macOS)
    - xclip (X11 Linux)
    - wl-copy (Wayland Linux)

    Returns True if successful, False otherwise.
    """
    try:
        with open("/dev/tty", "w") as tty:
            tty.write(_osc52_sequence(text))
            tty.flush()
        return True
    except OSError:
        pass

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
