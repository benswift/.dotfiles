#!/bin/bash

# Script to mark duplicate messages for deletion in maildir
# Keeps the oldest copy of each duplicate
#
# NOTE this is just for ANU Sent Items (for now, could be extended later)

echo "Finding duplicates in anu/Sent Items..."

# Get list of duplicate message IDs
for msgid in $(mu find --fields="i" "" | sort | uniq -d); do
    # Get all file paths for this message ID in Sent Items
    paths=$(mu find "msgid:\"$msgid\"" --fields="l" | grep "anu/Sent Items")

    # Only process if there are multiple copies in Sent Items
    if [ $(echo "$paths" | wc -l) -gt 1 ]; then
        echo "Processing duplicate: $msgid"

        # Sort by filename (which includes timestamp) and skip the first (oldest)
        echo "$paths" | sort | tail -n +2 | while read filepath; do
            if [ -f "$filepath" ]; then
                # Get the base filename
                filename=$(basename "$filepath")
                dirname=$(dirname "$filepath")

                # Check if already marked for deletion (has T flag)
                if [[ "$filename" == *":2,"*"T"* ]]; then
                    echo "  Already marked for deletion: $filename"
                else
                    # Add T (Trashed) flag to the filename
                    # Maildir flags are after :2, and must be sorted alphabetically
                    if [[ "$filename" == *":2,"* ]]; then
                        # Extract base and flags
                        base="${filename%%:2,*}"
                        flags="${filename#*:2,}"

                        # Add T to flags and sort them
                        newflags=$(echo "${flags}T" | grep -o . | sort | tr -d '\n')
                        newname="${base}:2,${newflags}"
                    else
                        # No flags yet, add :2,T
                        newname="${filename}:2,T"
                    fi

                    echo "  Marking for deletion: $filename"
                    mv "$filepath" "$dirname/$newname"
                fi
            fi
        done
    fi
done

echo ""
echo "Duplicates marked for deletion. Now run:"
echo "  mbsync anu"
echo "to sync deletions to the server."
echo ""
echo "After sync, you can expunge deleted messages locally with:"
echo "  mu remove"
