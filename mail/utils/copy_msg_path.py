#!/usr/bin/env -S uv run --script
# /// script
# dependencies = []
# ///
"""Copy the file path of an email message to the clipboard.

Reads an email from stdin, extracts the Message-ID, and uses mu to find
the corresponding file path in the maildir.
"""

import email
import subprocess
import sys
from email.policy import default


def get_message_path(msg: email.message.EmailMessage) -> str | None:
    message_id = msg.get("Message-ID")
    if not message_id:
        return None

    message_id = message_id.strip().strip("<>")

    result = subprocess.run(
        ["mu", "find", "-f", "l", f"msgid:{message_id}"],
        capture_output=True,
        text=True,
    )

    if result.returncode == 0 and result.stdout.strip():
        return result.stdout.strip().split("\n")[0].strip()
    return None


def copy_to_clipboard(text: str) -> bool:
    for cmd in [["pbcopy"], ["xclip", "-selection", "clipboard"], ["wl-copy"]]:
        try:
            subprocess.run(cmd, input=text, text=True, check=True)
            return True
        except (subprocess.CalledProcessError, FileNotFoundError):
            continue
    return False


def main():
    msg = email.message_from_file(sys.stdin, policy=default)
    path = get_message_path(msg)

    if path is None:
        print("Could not find message path", file=sys.stderr)
        sys.exit(1)
        return

    if copy_to_clipboard(path):
        print(f"Copied: {path}", file=sys.stderr)
    else:
        print(path)


if __name__ == "__main__":
    main()
