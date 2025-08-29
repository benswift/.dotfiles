#!/usr/bin/env python3
"""
Extract unique messages from Archive based on deduplication plan.
Creates backup of current state, then keeps only unique messages.
"""

import json
import shutil
import sys
from pathlib import Path
from datetime import datetime

def main():
    # Paths
    maildir_base = Path.home() / "Maildir" / "anu"
    archive_path = maildir_base / "Archive"
    archive_cur = archive_path / "cur"
    backup_path = maildir_base / f"Archive-with-dupes-{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    plan_file = Path.home() / "deduplication_plan.json"
    
    # Check plan exists
    if not plan_file.exists():
        print("ERROR: No deduplication plan found at ~/deduplication_plan.json")
        print("Run: python3 deduplicate_maildir.py --dry-run")
        sys.exit(1)
    
    # Load plan
    print("Loading deduplication plan...")
    with open(plan_file, 'r') as f:
        plan = json.load(f)
    
    duplicates = set(plan['files_to_delete'])
    expected_unique = plan['statistics']['total_files'] - len(duplicates)
    
    print(f"Plan summary:")
    print(f"  Total files: {plan['statistics']['total_files']}")
    print(f"  Duplicates to remove: {len(duplicates)}")
    print(f"  Expected unique messages: {expected_unique}")
    
    # Create backup
    print(f"\nCreating backup at {backup_path}...")
    if backup_path.exists():
        print(f"ERROR: Backup already exists at {backup_path}")
        sys.exit(1)
    
    shutil.copytree(archive_path, backup_path)
    print(f"Backup created with {len(list((backup_path / 'cur').glob('*')))} files")
    
    # Get list of unique files to keep
    print("\nIdentifying unique messages to keep...")
    files_to_keep = []
    for f in archive_cur.glob('*'):
        if str(f) not in duplicates:
            files_to_keep.append(f)
    
    print(f"Found {len(files_to_keep)} unique messages to keep")
    
    if len(files_to_keep) != expected_unique:
        print(f"WARNING: Expected {expected_unique} but found {len(files_to_keep)}")
        response = input("Continue anyway? (yes/no): ")
        if response.lower() != 'yes':
            print("Aborted. Backup is at:", backup_path)
            sys.exit(1)
    
    # Clear current Archive and copy only unique messages
    print("\nCleaning Archive folder...")
    for f in archive_cur.glob('*'):
        f.unlink()
    
    print("Copying unique messages back...")
    for i, f in enumerate(files_to_keep, 1):
        if i % 1000 == 0:
            print(f"  Copied {i}/{len(files_to_keep)} files...")
        shutil.copy2(f, archive_cur / f.name)
    
    # Final verification
    final_count = len(list(archive_cur.glob('*')))
    print(f"\nDone! Archive now contains {final_count} unique messages")
    print(f"Backup with duplicates saved at: {backup_path}")
    
    # Clean up state files
    for state_file in archive_path.glob('.mbsyncstate*'):
        state_file.unlink()
        print(f"Removed state file: {state_file.name}")

if __name__ == "__main__":
    main()