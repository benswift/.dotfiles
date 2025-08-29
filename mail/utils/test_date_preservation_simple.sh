#!/bin/bash
# Simple test to verify date preservation during upload

set -e

echo "=== Testing Date Preservation During Upload ==="
echo

# The test email - from January 2021
TEST_EMAIL="1651988638.19625_7.mitch,U=1:2,RS"
SOURCE_PATH="$HOME/Maildir/anu/Archive/cur/$TEST_EMAIL"

# Check the email exists and show its date
if [ ! -f "$SOURCE_PATH" ]; then
    echo "Error: Test email not found at $SOURCE_PATH"
    exit 1
fi

echo "Test email: $TEST_EMAIL"
EMAIL_DATE=$(grep "^Date:" "$SOURCE_PATH" | head -1)
echo "Original date: $EMAIL_DATE"
echo

# Create temporary test maildir
TEST_DIR="$HOME/Maildir/anu/Archive-TEST"
echo "Creating test maildir at $TEST_DIR..."
mkdir -p "$TEST_DIR/cur"
mkdir -p "$TEST_DIR/new"
mkdir -p "$TEST_DIR/tmp"

# Copy test email
cp "$SOURCE_PATH" "$TEST_DIR/cur/"
echo "Copied test email to test maildir"
echo

# Sync to server
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
echo "After checking, we'll clean up the test."
echo "Press Enter when you've checked the date in Outlook..."
read

# Clean up local test dir
echo "Cleaning up local test directory..."
rm -rf "$TEST_DIR"

echo
echo "=== Test Complete ==="
echo
echo "If the date showed as Jan 22, 2021, dates are preserved correctly!"
echo "If it showed as today's date, we need to investigate before proceeding."