#!/bin/bash
# Retry mbsync with proper throttling backoff
# Handles OAuth token expiry and Office365 throttling

MAX_RETRIES=10
RETRY_COUNT=0
DEFAULT_BACKOFF=30  # Default backoff in seconds
MIN_BACKOFF=5       # Minimum backoff
MAX_BACKOFF=600     # Maximum backoff (10 minutes)

echo "Starting mbsync with automatic retry and throttling support..."
echo "This will sync T-flagged messages for deletion on the server"
echo ""

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
    else
        echo $DEFAULT_BACKOFF
    fi
}

while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
    echo "[Attempt $((RETRY_COUNT + 1))/$MAX_RETRIES] Running: mbsync anu:Archive"
    
    # Capture both stdout and stderr
    OUTPUT=$(mbsync anu:Archive 2>&1)
    EXIT_CODE=$?
    
    echo "$OUTPUT"
    
    if [ $EXIT_CODE -eq 0 ]; then
        echo ""
        echo "SUCCESS! mbsync completed successfully"
        echo "Archive folder now synced with server"
        echo ""
        echo "Statistics:"
        echo "  Total files: $(find ~/Maildir/anu/Archive/cur -type f | wc -l)"
        echo "  T-flagged files: $(find ~/Maildir/anu/Archive/cur -type f -name '*T' | wc -l)"
        exit 0
    else
        echo ""
        echo "mbsync failed with exit code: $EXIT_CODE"
        
        # Check for throttling
        if echo "$OUTPUT" | grep -q "Request is throttled"; then
            BACKOFF=$(extract_backoff_time "$OUTPUT")
            echo "Server throttling detected. Suggested backoff: ${BACKOFF} seconds"
            
            # Apply exponential backoff on subsequent retries
            if [ $RETRY_COUNT -gt 0 ]; then
                BACKOFF=$(( BACKOFF * 2 ))
            fi
            
            # Cap at maximum backoff
            if [ $BACKOFF -gt $MAX_BACKOFF ]; then
                BACKOFF=$MAX_BACKOFF
            fi
        elif echo "$OUTPUT" | grep -q "AccessTokenExpired\|OAuth"; then
            echo "OAuth token expired. Please re-authenticate:"
            echo "  cd ~/Maildir/mail && ./reauth-anu-oauth.sh"
            exit 1
        elif echo "$OUTPUT" | grep -q "timeout"; then
            # Network timeout, use exponential backoff
            BACKOFF=$(( DEFAULT_BACKOFF * (2 ** RETRY_COUNT) ))
            if [ $BACKOFF -gt $MAX_BACKOFF ]; then
                BACKOFF=$MAX_BACKOFF
            fi
            echo "Network timeout. Using backoff: ${BACKOFF} seconds"
        else
            # Other error, use minimal backoff
            BACKOFF=$MIN_BACKOFF
        fi
        
        if [ $RETRY_COUNT -lt $((MAX_RETRIES - 1)) ]; then
            echo "Retrying in ${BACKOFF} seconds..."
            sleep $BACKOFF
        fi
        
        RETRY_COUNT=$((RETRY_COUNT + 1))
    fi
done

echo ""
echo "ERROR: mbsync failed after $MAX_RETRIES attempts"
echo "Check the logs and consider:"
echo "1. Waiting longer for throttling to clear (several hours)"
echo "2. Running during off-peak hours (late night)"
echo "3. Processing in smaller batches"
exit 1