#!/bin/bash
# Safe test to verify date preservation during upload

set -e

echo "=== Testing Date Preservation During Upload ==="
echo

# The test email - from January 2021
TEST_EMAIL="1651988638.19625_7.mitch,U=1:2,RS"
ARCHIVE_CUR="$HOME/Maildir/anu/Archive/cur"
SOURCE_PATH="$ARCHIVE_CUR/$TEST_EMAIL"

# Check the email exists and show its date
if [ ! -f "$SOURCE_PATH" ]; then
    echo "Error: Test email not found at $SOURCE_PATH"
    exit 1
fi

echo "Test email: $TEST_EMAIL"
EMAIL_DATE=$(grep "^Date:" "$SOURCE_PATH" | head -1)
EMAIL_SUBJECT=$(grep "^Subject:" "$SOURCE_PATH" | head -1)
echo "Original date: $EMAIL_DATE"
echo "Subject: $EMAIL_SUBJECT"
echo

# Safety check - Archive should be empty on server
echo "Checking server Archive is empty..."
SERVER_COUNT=$(mbsync --list --pull anu:Archive 2>&1 | grep -v "^Archive$" | wc -l | tr -d ' ')
if [ "$SERVER_COUNT" != "0" ]; then
    echo "ERROR: Server Archive is not empty (has $SERVER_COUNT messages)"
    echo "Please empty it before running this test"
    exit 1
fi
echo "✓ Server Archive is empty"
echo

# Create temporary backup directory
BACKUP_DIR="$HOME/Maildir/anu/Archive-backup-temp"
echo "Creating temporary backup directory..."
mkdir -p "$BACKUP_DIR"

# Move ALL files EXCEPT our test email to backup
echo "Moving all other files to temporary backup (this will take a moment)..."
cd "$ARCHIVE_CUR"
for file in *; do
    if [ "$file" != "$TEST_EMAIL" ]; then
        mv "$file" "$BACKUP_DIR/" 2>/dev/null || true
    fi
done

# Verify we only have one file left
FILE_COUNT=$(ls -1 | wc -l | tr -d ' ')
echo "Files remaining in Archive/cur: $FILE_COUNT (should be 1)"

if [ "$FILE_COUNT" != "1" ]; then
    echo "ERROR: Expected 1 file, found $FILE_COUNT"
    echo "Restoring files..."
    mv "$BACKUP_DIR"/* "$ARCHIVE_CUR/" 2>/dev/null || true
    rmdir "$BACKUP_DIR"
    exit 1
fi

# Now sync just this one email
echo
echo "Syncing test email to server..."
echo "Running: mbsync --push anu:Archive"
mbsync --push anu:Archive

echo
echo "Upload complete!"
echo
echo "Now check in Outlook (Web or Mac):"
echo "1. Look in the Archive folder"
echo "2. You should see ONE email with subject: 'RE: Timetabling | COMP2710/6470'"
echo "3. Check if the date shows as: 'Fri, 22 Jan 2021' (original)"
echo "   OR if it shows as today's date"
echo
echo "Press Enter after checking the date in Outlook..."
read

# Restore all files
echo "Restoring all files to Archive/cur..."
mv "$BACKUP_DIR"/* "$ARCHIVE_CUR/" 2>/dev/null || true
rmdir "$BACKUP_DIR"

# Delete the test email from server to clean up
echo "Cleaning up test email from server..."
# Mark test email for deletion
mv "$ARCHIVE_CUR/$TEST_EMAIL" "$ARCHIVE_CUR/${TEST_EMAIL%,*},T"
# Sync deletion to server
mbsync --push anu:Archive
# Remove T flag locally
mv "$ARCHIVE_CUR/${TEST_EMAIL%,*},T" "$ARCHIVE_CUR/$TEST_EMAIL"

echo
echo "=== Test Complete ==="
echo
echo "If the date showed as Jan 22, 2021, dates are preserved correctly!"
echo "If it showed as today's date, we need to investigate before proceeding."