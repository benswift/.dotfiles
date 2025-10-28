# Maildir utility scripts

This directory contains utility scripts for working with maildir folders.

## Guidelines for maildir scripts

When working with maildir folders, follow these best practices:

### For Python scripts: Use the mailbox module

The `mailbox.Maildir` class provides proper handling of maildir semantics:

- Automatic management of cur/, new/, tmp/ subdirectories
- Proper handling of maildir info strings (flags after `:2,`)
- Safe message removal and manipulation
- Built-in locking and consistency guarantees

### Example usage

```python
import mailbox

# Open a maildir (create=False to avoid creating if it doesn't exist)
mbox = mailbox.Maildir('/path/to/maildir', create=False)

# Iterate through messages
for key in mbox.keys():
    msg = mbox[key]
    msg_id = msg.get('Message-ID')
    # Process message...

# Remove a message safely
mbox.remove(key)

# Flush changes to disk
mbox.flush()
mbox.close()
```

### General principles

1. **Use message keys, not file paths** - The mailbox module abstracts away the
   filesystem details
2. **Always flush after modifications** - Ensures changes are written to disk
3. **Handle exceptions gracefully** - Email parsing can fail on malformed
   messages
4. **Preserve maildir flags** - The module handles this automatically
5. **Sort by key for consistency** - Keys sort naturally by timestamp

### For shell scripts: Handle maildir structure properly

When working with maildir in shell scripts:

#### Understand maildir structure

```bash
maildir/
├── cur/     # Read messages
├── new/     # Unread messages
├── tmp/     # Temporary files during delivery
```

#### Filename format

Maildir filenames follow this pattern:

```
unique_name:2,FLAGS
```

Where FLAGS can include:

- `D` - Draft
- `F` - Flagged
- `P` - Passed (forwarded)
- `R` - Replied
- `S` - Seen (read)
- `T` - Trashed (marked for deletion)

#### Safe operations

```bash
# Finding messages (search both cur/ and new/)
find "$MAILDIR"/{cur,new} -type f -name '*' 2>/dev/null

# Count messages
msg_count=$(find "$MAILDIR"/{cur,new} -type f | wc -l)

# Add a flag (e.g., mark as read)
for msg in "$MAILDIR"/new/*; do
    [ -f "$msg" ] || continue
    basename=$(basename "$msg")
    # Move to cur/ and add S flag
    mv "$msg" "$MAILDIR/cur/${basename}:2,S"
done

# Remove duplicates by Message-ID (using formail from procmail)
formail -D 99999999 msgid.cache < message.eml

# Parse headers safely
grep -m1 '^Message-ID:' "$file" | sed 's/^Message-ID: *//'
```

#### Important rules

1. **Never modify tmp/** - It's for mail delivery agents only
2. **Atomic operations** - Use `mv` instead of `cp` then `rm`
3. **Preserve timestamps** - Use `cp -p` or `rsync -a` when copying
4. **Handle both cur/ and new/** - Messages can be in either
5. **Respect locking** - Check for `.lock` files before bulk operations
6. **Quote filenames** - Maildir names can contain special characters

#### mbsync integration

When working with mbsync-managed maildirs:

```bash
# Never delete .mbsyncstate* files
# Never delete messages directly - mark with T flag instead
mv "$file" "${file}T"  # Add T flag
mbsync channel         # Sync deletion to server

# Check for journal corruption
if [ -f "$MAILDIR/.mbsyncstate.journal" ]; then
    size=$(stat -f%z "$MAILDIR/.mbsyncstate.journal" 2>/dev/null || stat -c%s "$MAILDIR/.mbsyncstate.journal")
    if [ "$size" -gt 1048576 ]; then  # > 1MB
        echo "Warning: Large journal file indicates corruption"
    fi
fi
```

## Current scripts

- `deduplicate_maildir.py` - Remove duplicate messages based on Message-ID
  headers
