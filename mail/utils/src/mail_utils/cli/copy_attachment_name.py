"""Save an email attachment to /tmp and copy its path to the clipboard."""

import re
import subprocess
import sys
from pathlib import Path

from mail_utils.clipboard import copy_to_clipboard

_SHELL_META = re.compile(r"""([ \t\\'"()\[\]{}!#$&*?<>|;`~])""")


def _shell_escape(path: str) -> str:
    return _SHELL_META.sub(r"\\\1", path)


def main():
    from mail_utils.email import read_email_from_bytes_lenient

    data = sys.stdin.buffer.read()
    msg = read_email_from_bytes_lenient(data)

    attachments = [(fn, part) for part in msg.walk() if (fn := part.get_filename())]
    if not attachments:
        sys.exit(1)

    if len(attachments) == 1:
        filename, part = attachments[0]
    else:
        filenames = [fn for fn, _ in attachments]
        result = subprocess.run(
            ["fzf", "--prompt=attachment: "],
            input="\n".join(filenames),
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            sys.exit(1)
        selected = result.stdout.strip()
        filename, part = next((fn, p) for fn, p in attachments if fn == selected)

    dest = Path("/tmp") / filename
    payload = part.get_payload(decode=True)
    if isinstance(payload, bytes):
        dest.write_bytes(payload)
    else:
        content = part.get_content()
        if isinstance(content, str):
            dest.write_text(content)
        else:
            dest.write_bytes(content)

    escaped = _shell_escape(str(dest))
    if copy_to_clipboard(escaped):
        print(f"Copied: {escaped}", file=sys.stderr)
    else:
        print(escaped)


if __name__ == "__main__":
    main()
