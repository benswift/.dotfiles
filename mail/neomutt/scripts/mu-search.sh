#!/bin/bash
# Helper script for neomutt mu search with automatic results navigation

# Get the maildir restriction if provided (for account-specific searches)
MAILDIR_FILTER="$1"

# Prompt for search query
echo -n "Search${MAILDIR_FILTER:+ in }$MAILDIR_FILTER: "
read -r QUERY

# Exit if no query provided
if [ -z "$QUERY" ]; then
    echo "No search query provided"
    exit 1
fi

# Build the mu find command
MU_CMD="mu find --clearlinks --format=links --linksdir=$HOME/.mu/results"

# Add maildir filter if provided
if [ -n "$MAILDIR_FILTER" ]; then
    MU_CMD="$MU_CMD maildir:$MAILDIR_FILTER"
fi

# Execute the search
$MU_CMD "$QUERY"

# Check if search succeeded
if [ $? -eq 0 ]; then
    # Tell neomutt to change to results folder
    # We need to use the neomutt FIFO if available, or just exit successfully
    # The macro will handle the folder change
    exit 0
else
    echo "Search failed"
    exit 1
fi