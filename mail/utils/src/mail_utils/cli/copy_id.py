"""Copy the Message-ID of an email to the clipboard."""

import sys

from mail_utils.clipboard import copy_to_clipboard
from mail_utils.email import get_message_id, read_email_from_stdin


def main():
    msg = read_email_from_stdin()
    message_id = get_message_id(msg)

    if not message_id:
        print("No Message-ID found", file=sys.stderr)
        sys.exit(1)

    if copy_to_clipboard(message_id):
        print(f"Copied: {message_id}", file=sys.stderr)
    else:
        print(message_id)


if __name__ == "__main__":
    main()
