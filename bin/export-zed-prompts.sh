#!/bin/zsh

# Export Zed prompts from LMDB database to individual markdown files
# Usage: ./export-zed-prompts.sh [output_directory]

set -uo pipefail

# Default output directory
OUTPUT_DIR="${1:-./zed-prompts-export}"

# Zed prompts database path
ZED_PROMPTS_DB="$HOME/.config/zed/prompts/prompts-library-db.0.mdb"

# Check if database exists
if [[ ! -d "$ZED_PROMPTS_DB" ]]; then
    echo "Error: Zed prompts database not found at $ZED_PROMPTS_DB"
    exit 1
fi

# Check if required tools are available
for tool in strings jq; do
    if ! command -v "$tool" &> /dev/null; then
        echo "Error: '$tool' command not found. Please install it."
        exit 1
    fi
done

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "Exporting Zed prompts to $OUTPUT_DIR..."

# Extract raw data from LMDB
TEMP_RAW=$(mktemp)
strings "$ZED_PROMPTS_DB/data.mdb" > "$TEMP_RAW"

# Extract metadata and get UUID-title pairs
METADATA_TEMP=$(mktemp)
grep -E '\{"id":\{"kind":"User"' "$TEMP_RAW" | \
    sed 's/.*\({"id":{"kind":"User"[^}]*}[^}]*}\).*/\1/' | \
    jq -r '"\(.id.uuid)|\(.title)"' | \
    sort -u > "$METADATA_TEMP"

echo "Found $(wc -l < "$METADATA_TEMP") prompts"

# Process each prompt
exported_count=0
while IFS='|' read -r uuid title; do
    [[ -z "$uuid" || -z "$title" ]] && continue

    echo "Processing: $title"

    # Extract content for this UUID
    # Find the line that contains content (not just metadata)
    content_line=$(grep -E "^{\"kind\":\"User\",\"uuid\":\"$uuid\"}[^{]" "$TEMP_RAW" | head -1)

    if [[ -n "$content_line" ]]; then
        # Remove the UUID prefix to get the content
        content=$(echo "$content_line" | sed "s/^{\"kind\":\"User\",\"uuid\":\"$uuid\"}//" | sed 's/{"id":.*$//')

        # Get the line number to extract subsequent lines
        line_num=$(grep -n -E "^{\"kind\":\"User\",\"uuid\":\"$uuid\"}[^{]" "$TEMP_RAW" | head -1 | cut -d: -f1)

        # Collect additional content lines
        additional_content=""
        if [[ -n "$line_num" ]]; then
            next_line=$((line_num + 1))
            while IFS= read -r line; do
                # Stop if we hit delimiters
                if [[ $line =~ ^\{\"kind\":\"User\",\"uuid\": ]] || \
                   [[ $line =~ ^\{\"id\": ]] || \
                   [[ $line =~ ^(bodies\.v2|metadata|j=) ]] || \
                   [[ -z "$line" ]]; then
                    break
                fi

                if [[ -n "$additional_content" ]]; then
                    additional_content="$additional_content"$'\n'"$line"
                else
                    additional_content="$line"
                fi
            done < <(tail -n +$next_line "$TEMP_RAW")
        fi

        # Combine content
        if [[ -n "$additional_content" ]]; then
            content="$content"$'\n'"$additional_content"
        fi
    else
        content=""
    fi

    if [[ -n "$content" ]]; then
        # Create a slugified filename
        filename=$(echo "$title" | \
                   sed 's/[^a-zA-Z0-9 -]//g' | \
                   sed 's/ /-/g' | \
                   sed 's/--*/-/g' | \
                   sed 's/^-\|-$//g' | \
                   tr '[:upper:]' '[:lower:]')

        if [[ -z "$filename" ]]; then
            filename="prompt-$uuid"
        fi

        filepath="$OUTPUT_DIR/${filename}.md"

        # Write the markdown file
        {
            echo "# $title"
            echo ""
            echo "$content"
        } > "$filepath"

        echo "Exported: $title -> $filepath"
        ((exported_count++))
    else
        echo "Warning: No content found for prompt '$title'"
    fi

done < "$METADATA_TEMP"

# Clean up
rm "$TEMP_RAW" "$METADATA_TEMP"

echo ""
echo "Export complete! Exported $exported_count prompts to $OUTPUT_DIR"

# List the exported files
echo ""
echo "Exported files:"
ls -la "$OUTPUT_DIR"/*.md 2>/dev/null || echo "No markdown files found in output directory"
