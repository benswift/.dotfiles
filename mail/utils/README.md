# Maildir utility scripts

This directory contains utility scripts for working with maildir folders.

## Guidelines for maildir scripts

When working with maildir folders, follow these best practices:

### Use Python's mailbox module

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

1. **Use message keys, not file paths** - The mailbox module abstracts away the filesystem details
2. **Always flush after modifications** - Ensures changes are written to disk
3. **Handle exceptions gracefully** - Email parsing can fail on malformed messages
4. **Preserve maildir flags** - The module handles this automatically
5. **Sort by key for consistency** - Keys sort naturally by timestamp

## Current scripts

- `deduplicate_maildir.py` - Remove duplicate messages based on Message-ID headers