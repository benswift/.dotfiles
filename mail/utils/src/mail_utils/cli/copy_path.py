"""Copy the file path of an email message to the clipboard."""

import sys

from mail_utils.clipboard import copy_to_clipboard
from mail_utils.email import get_message_id, read_email_from_stdin
from mail_utils.mu import find_message_path


def main():
    msg = read_email_from_stdin()
    message_id = get_message_id(msg)

    if message_id is not None:
        path = find_message_path(message_id)
    else:
        print("No Message-ID found", file=sys.stderr)
        sys.exit(1)

    if path is None:
        print("Could not find message path", file=sys.stderr)
        sys.exit(1)
    elif copy_to_clipboard(path):
        print(f"Copied: {path}", file=sys.stderr)
    else:
        print(path)


if __name__ == "__main__":
    main()
