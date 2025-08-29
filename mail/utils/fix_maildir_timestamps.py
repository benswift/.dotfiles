#!/usr/bin/env python3
"""
Fix maildir file timestamps to match email Date headers.
This ensures mbsync uses the correct INTERNALDATE when uploading.
"""

import os
import email
import email.utils
import time
from pathlib import Path
from datetime import datetime
import argparse
import logging

def setup_logging(verbose=False):
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    return logging.getLogger(__name__)

def fix_email_timestamp(file_path, dry_run=False):
    """Fix a single email file's timestamp to match its Date header."""
    try:
        with open(file_path, 'rb') as f:
            msg = email.message_from_binary_file(f)
            date_header = msg.get('Date')
            
            if not date_header:
                return False, "No Date header"
            
            # Parse the date header
            date_tuple = email.utils.parsedate_to_datetime(date_header)
            if not date_tuple:
                return False, f"Could not parse date: {date_header}"
            
            # Convert to timestamp
            timestamp = date_tuple.timestamp()
            
            # Get current file timestamp
            current_mtime = os.path.getmtime(file_path)
            
            # Check if update needed
            if abs(current_mtime - timestamp) < 60:  # Within 1 minute
                return True, "Already correct"
            
            if not dry_run:
                # Set both access time and modification time
                os.utime(file_path, (timestamp, timestamp))
                return True, f"Updated from {datetime.fromtimestamp(current_mtime)} to {date_tuple}"
            else:
                return True, f"[DRY RUN] Would update from {datetime.fromtimestamp(current_mtime)} to {date_tuple}"
                
    except Exception as e:
        return False, str(e)

def main():
    parser = argparse.ArgumentParser(description='Fix maildir file timestamps to match Date headers')
    parser.add_argument('path', nargs='?', default='~/Maildir/anu/Archive/cur',
                        help='Path to maildir cur directory')
    parser.add_argument('--dry-run', action='store_true',
                        help='Show what would be changed without making changes')
    parser.add_argument('--verbose', action='store_true',
                        help='Enable verbose logging')
    parser.add_argument('--limit', type=int, default=0,
                        help='Process only this many files (0 for all)')
    
    args = parser.parse_args()
    logger = setup_logging(args.verbose)
    
    # Expand path
    maildir_path = Path(args.path).expanduser()
    if not maildir_path.exists():
        logger.error(f"Path does not exist: {maildir_path}")
        return 1
    
    # Process files
    files = list(maildir_path.glob('*'))
    total = len(files)
    
    if args.limit > 0:
        files = files[:args.limit]
        logger.info(f"Processing first {args.limit} of {total} files")
    else:
        logger.info(f"Processing {total} files")
    
    updated = 0
    already_correct = 0
    errors = 0
    
    for i, file_path in enumerate(files, 1):
        if i % 1000 == 0:
            logger.info(f"Progress: {i}/{len(files)}")
        
        success, message = fix_email_timestamp(file_path, args.dry_run)
        
        if success:
            if "Already correct" in message:
                already_correct += 1
            else:
                updated += 1
                if args.verbose:
                    logger.debug(f"{file_path.name}: {message}")
        else:
            errors += 1
            if args.verbose:
                logger.warning(f"{file_path.name}: {message}")
    
    # Summary
    logger.info("=" * 50)
    logger.info(f"{'DRY RUN ' if args.dry_run else ''}SUMMARY:")
    logger.info(f"  Total files: {len(files)}")
    logger.info(f"  Updated: {updated}")
    logger.info(f"  Already correct: {already_correct}")
    logger.info(f"  Errors: {errors}")
    
    return 0

if __name__ == "__main__":
    exit(main())