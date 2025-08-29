#!/bin/bash
# Retry mbsync with proper throttling backoff for Office365
# Handles OAuth token expiry and aggressive throttling
# Designed specifically for syncing large T-flag deletion operations

MAX_RETRIES=20  # Increased for large operations
RETRY_COUNT=0
DEFAULT_BACKOFF=60  # Start with 60 seconds for Office365
MIN_BACKOFF=30      # Minimum backoff
MAX_BACKOFF=1800    # Maximum backoff (30 minutes)

echo "=================================================="
echo "mbsync with automatic retry and throttling support"
echo "=================================================="
echo ""
echo "This will sync T-flagged messages for deletion on the server"
echo "Office365 throttling is expected - this script will handle it"
echo ""

# Check current state first
echo "Current Archive state:"
TOTAL_FILES=$(find ~/Maildir/anu/Archive/cur -type f 2>/dev/null | wc -l)
T_FILES=$(find ~/Maildir/anu/Archive/cur -type f -name '*:2,*T*' 2>/dev/null | wc -l)
echo "  Total files: $TOTAL_FILES"
echo "  T-flagged for deletion: $T_FILES"
echo ""

if [ "$T_FILES" -eq 0 ]; then
    echo "No T-flagged files found. Nothing to sync."
    exit 0
fi

if [ "$T_FILES" -gt 10000 ]; then
    echo "WARNING: $T_FILES files to delete - this will take a LONG time"
    echo "Office365 will likely throttle heavily"
    echo "Consider running during off-peak hours (late night/early morning)"
    echo ""
    read -p "Continue anyway? (yes/no): " confirm
    if [ "$confirm" != "yes" ]; then
        exit 0
    fi
fi

# Function to extract backoff time from error message
extract_backoff_time() {
    local error_msg="$1"
    # Look for "Suggested Backoff Time: XXXXX milliseconds"
    if echo "$error_msg" | grep -q "Suggested Backoff Time:"; then
        local ms=$(echo "$error_msg" | sed -n 's/.*Suggested Backoff Time: \([0-9]*\) milliseconds.*/\1/p')
        if [ -n "$ms" ]; then
            # Convert milliseconds to seconds (round up)
            echo $(( (ms + 999) / 1000 ))
        else
            echo $DEFAULT_BACKOFF
        fi
    # Also check for "Please retry after" pattern
    elif echo "$error_msg" | grep -q "retry after"; then
        # Extract seconds if present
        local secs=$(echo "$error_msg" | sed -n 's/.*retry after \([0-9]*\).*/\1/p')
        if [ -n "$secs" ]; then
            echo $secs
        else
            echo $DEFAULT_BACKOFF
        fi
    else
        echo $DEFAULT_BACKOFF
    fi
}

# Function to check OAuth token
check_oauth_token() {
    echo "Checking OAuth token validity..."
    OUTPUT=$(mbsync --list anu:Archive 2>&1)
    
    if echo "$OUTPUT" | grep -q "AccessTokenExpired\|OAuth\|401"; then
        echo "OAuth token expired or invalid"
        echo "Please re-authenticate:"
        echo "  cd ~/.dotfiles/mail && ./reauth-anu-oauth.sh"
        return 1
    fi
    
    echo "OAuth token appears valid"
    return 0
}

# Check OAuth before starting
if ! check_oauth_token; then
    exit 1
fi

# Main retry loop
CONSECUTIVE_FAILURES=0
while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
    echo ""
    echo "[Attempt $((RETRY_COUNT + 1))/$MAX_RETRIES] Running: mbsync anu:Archive"
    echo "Time: $(date '+%Y-%m-%d %H:%M:%S')"
    
    # Capture both stdout and stderr with timeout
    # Use longer timeout for large operations
    TIMEOUT_SECONDS=600  # 10 minutes
    OUTPUT=$(timeout $TIMEOUT_SECONDS mbsync anu:Archive 2>&1)
    EXIT_CODE=$?
    
    # Show last 20 lines of output (to avoid flooding terminal)
    echo "$OUTPUT" | tail -20
    
    if [ $EXIT_CODE -eq 0 ]; then
        echo ""
        echo "=================================================="
        echo "SUCCESS! mbsync completed successfully"
        echo "=================================================="
        echo ""
        
        # Show final statistics
        echo "Final Archive statistics:"
        FINAL_TOTAL=$(find ~/Maildir/anu/Archive/cur -type f 2>/dev/null | wc -l)
        FINAL_T=$(find ~/Maildir/anu/Archive/cur -type f -name '*:2,*T*' 2>/dev/null | wc -l)
        echo "  Total files: $FINAL_TOTAL"
        echo "  T-flagged remaining: $FINAL_T"
        echo "  Successfully synced: $((T_FILES - FINAL_T)) deletions"
        
        if [ "$FINAL_T" -gt 0 ]; then
            echo ""
            echo "Note: $FINAL_T T-flagged files remain"
            echo "These can be removed locally with:"
            echo "  find ~/Maildir/anu/Archive/cur -name '*:2,*T*' -delete"
        fi
        
        exit 0
    elif [ $EXIT_CODE -eq 124 ]; then
        echo ""
        echo "mbsync timed out after $TIMEOUT_SECONDS seconds"
        BACKOFF=$DEFAULT_BACKOFF
    else
        echo ""
        echo "mbsync failed with exit code: $EXIT_CODE"
        
        # Analyze the error
        if echo "$OUTPUT" | grep -q "Request is throttled\|throttling\|429\|TooManyRequests"; then
            BACKOFF=$(extract_backoff_time "$OUTPUT")
            echo "Server throttling detected. Suggested backoff: ${BACKOFF} seconds"
            
            # Exponential backoff for consecutive throttling
            CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
            if [ $CONSECUTIVE_FAILURES -gt 3 ]; then
                BACKOFF=$(( BACKOFF * CONSECUTIVE_FAILURES ))
                echo "Multiple consecutive throttles - increasing backoff to ${BACKOFF} seconds"
            fi
            
        elif echo "$OUTPUT" | grep -q "AccessTokenExpired\|OAuth\|401"; then
            echo "OAuth token expired during operation"
            
            # Try to re-check token
            if ! check_oauth_token; then
                exit 1
            fi
            
            # Small backoff to retry
            BACKOFF=$MIN_BACKOFF
            
        elif echo "$OUTPUT" | grep -q "timeout\|timed out\|Connection reset"; then
            # Network timeout, use exponential backoff
            BACKOFF=$(( DEFAULT_BACKOFF * (2 ** (CONSECUTIVE_FAILURES / 2)) ))
            CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
            echo "Network timeout. Using backoff: ${BACKOFF} seconds"
            
        elif echo "$OUTPUT" | grep -q "unexpected tag\|journal"; then
            echo "ERROR: Sync state corruption detected"
            echo "The .mbsyncstate files may be corrupted"
            echo ""
            echo "To fix:"
            echo "1. Backup the state files:"
            echo "   mv ~/Maildir/anu/Archive/.mbsyncstate* ~/backup/"
            echo "2. Try syncing again"
            exit 1
            
        else
            # Other error, use minimal backoff but increase on consecutive failures
            CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
            BACKOFF=$(( MIN_BACKOFF * CONSECUTIVE_FAILURES ))
            echo "Unknown error. Using backoff: ${BACKOFF} seconds"
        fi
        
        # Cap at maximum backoff
        if [ $BACKOFF -gt $MAX_BACKOFF ]; then
            BACKOFF=$MAX_BACKOFF
            echo "Capping backoff at maximum: ${BACKOFF} seconds"
        fi
    fi
    
    # Reset consecutive failures on any success or different error type
    if [ $EXIT_CODE -eq 0 ] || [ $CONSECUTIVE_FAILURES -eq 0 ]; then
        CONSECUTIVE_FAILURES=0
    fi
    
    RETRY_COUNT=$((RETRY_COUNT + 1))
    
    if [ $RETRY_COUNT -lt $MAX_RETRIES ]; then
        echo ""
        echo "Waiting ${BACKOFF} seconds before retry $((RETRY_COUNT + 1))..."
        echo "Press Ctrl+C to abort"
        
        # Show countdown for long waits
        if [ $BACKOFF -gt 60 ]; then
            MINS=$((BACKOFF / 60))
            echo "That's approximately $MINS minutes"
        fi
        
        sleep $BACKOFF
    fi
done

echo ""
echo "=================================================="
echo "ERROR: mbsync failed after $MAX_RETRIES attempts"
echo "=================================================="
echo ""
echo "Troubleshooting suggestions:"
echo "1. Wait several hours for throttling to clear"
echo "2. Try again during off-peak hours (2-6 AM)"
echo "3. Process in smaller batches:"
echo "   - Mark fewer files with T flag at a time"
echo "   - Sync between batches"
echo "4. Check OAuth token: cd ~/.dotfiles/mail && ./reauth-anu-oauth.sh"
echo "5. Check for sync state corruption in ~/Maildir/anu/Archive/.mbsyncstate*"
echo ""
echo "Current state:"
CURRENT_T=$(find ~/Maildir/anu/Archive/cur -type f -name '*:2,*T*' 2>/dev/null | wc -l)
echo "  T-flagged files still to sync: $CURRENT_T"

exit 1