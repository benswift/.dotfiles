#!/bin/bash
# Test script to verify date preservation during nuclear cleanup

set -e

echo "=== Testing Date Preservation During Upload ==="
echo

# Create a test folder on server
echo "1. Creating test folder Archive-TEST on server..."
echo "   Please create 'Archive-TEST' folder in Outlook now"
echo "   Press Enter when done..."
read

# Pick an old email to test with
echo "2. Selecting an old test email..."
TEST_EMAIL=$(find ~/Maildir/anu/Archive/cur -type f | head -1)
echo "   Test email: $(basename "$TEST_EMAIL")"

# Extract date from the email
EMAIL_DATE=$(grep "^Date:" "$TEST_EMAIL" | head -1)
echo "   Original date: $EMAIL_DATE"

# Create temporary test maildir
TEST_DIR=~/Maildir/anu/Archive-TEST
mkdir -p "$TEST_DIR/cur"
mkdir -p "$TEST_DIR/new"
mkdir -p "$TEST_DIR/tmp"

# Copy test email
cp "$TEST_EMAIL" "$TEST_DIR/cur/"
echo "   Copied to test maildir"

# Sync to server
echo
echo "3. Syncing test email to server..."
echo "   Running: mbsync anu:Archive-TEST"
mbsync anu:Archive-TEST

echo
echo "4. Check the email in Outlook:"
echo "   - Look in Archive-TEST folder"
echo "   - Check if the date shows as: $EMAIL_DATE"
echo "   - Or if it shows as today's date"
echo
echo "Press Enter after checking..."
read

# Cleanup
echo "5. Cleaning up test..."
rm -rf "$TEST_DIR"
echo "   Local test directory removed"
echo "   Please delete Archive-TEST folder in Outlook"
echo
echo "=== Test Complete ==="
echo
echo "If the date was preserved correctly, it's safe to proceed with nuclear cleanup."
echo "If not, we need to investigate further before proceeding."