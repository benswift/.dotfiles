"""Copy an email's Message-ID to the clipboard as a mu query.

The result, `msgid:<id>`, is what `mu find` takes, so pasted into an agent
session it names the message unambiguously on any host with the mail mirror
--- unlike the file path, which changes whenever mbsync renames the file.
"""

import sys
from email.message import EmailMessage

from mail_utils.clipboard import copy_to_clipboard
from mail_utils.email import get_message_id, read_email_from_stdin


def mu_query(msg: EmailMessage) -> str | None:
    message_id = get_message_id(msg)
    return f"msgid:{message_id}" if message_id else None


def main() -> None:
    query = mu_query(read_email_from_stdin())
    if query is None:
        print("No Message-ID found", file=sys.stderr)
        sys.exit(1)
    if copy_to_clipboard(query):
        print(f"Copied: {query}", file=sys.stderr)
    else:
        print(query)


if __name__ == "__main__":
    main()
