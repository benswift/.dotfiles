#!/bin/bash
# Link the whole thread of the message on stdin into ~/.mu/results, from every
# folder and account. neomutt's own threading only sees the current folder,
# and <entire-thread> needs notmuch; mu's --include-related follows the
# References chain instead. The ,t macro pipes the message raw (pipe_decode
# unset) and then opens the results folder.
#
# A thread is only as complete as its headers: mail that sets no
# In-Reply-To/References (ServiceNow's ticket notifications, for one) comes
# back as just the one message.

# Header block only, with folded continuation lines joined, then drain the
# body so neomutt's pipe isn't cut short.
headers="$(sed '/^$/q')"
cat >/dev/null

msgid="$(printf '%s\n' "$headers" | awk '
    /^[ \t]/ { if (inid) id = id $0; next }
    { inid = tolower($0) ~ /^message-id:/; if (inid) id = $0 }
    END { print id }
' | sed -E 's/^[^:]*:[[:space:]]*<?//; s/>?[[:space:]]*$//')"

if [ -z "$msgid" ]; then
    echo "No Message-ID header on this message"
    exit 1
fi

if ! mu find --clearlinks --format=links --linksdir="$HOME/.mu/results" \
    --include-related --skip-dups "msgid:\"$msgid\""; then
    echo "mu find failed for $msgid (not indexed yet? run mu index)"
    exit 1
fi
