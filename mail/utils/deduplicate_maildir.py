#!/usr/bin/env python3
"""
Comprehensive maildir deduplication script for ANU Archive folder.
Safely removes duplicate emails while preserving all unique messages.
Handles Office365 throttling with batched operations.
"""

import os
import sys
import hashlib
import email
import email.parser
import email.policy
import shutil
import argparse
import logging
import json
import time
import subprocess
from pathlib import Path
from datetime import datetime
from collections import defaultdict
from typing import Dict, List, Tuple, Optional

# Configuration
MAILDIR_PATH = Path.home() / "Maildir" / "anu" / "Archive"
BACKUP_DIR = Path.home() / f"maildir_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
LOG_FILE = Path.home() / f"deduplicate_maildir_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
PLAN_FILE = Path.home() / "deduplication_plan.json"
BATCH_STATE_FILE = Path.home() / "deduplication_batch_state.json"

# Expected counts based on current broken state
CURRENT_FILE_COUNT = 92197  # Current broken state
EXPECTED_UNIQUE_COUNT = 30279  # Target after deduplication
EXPECTED_DUPLICATES = 60000  # Approximate duplicates to remove

# Batch processing configuration
DEFAULT_BATCH_SIZE = 1000  # Process 1000 files per batch
MAX_BATCH_SIZE = 5000  # Maximum batch size to avoid overwhelming

# Setup logging
def setup_logging(verbose: bool = False):
    """Configure logging for the script."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(LOG_FILE),
            logging.StreamHandler()
        ]
    )
    return logging.getLogger(__name__)

class MaildirDeduplicator:
    """Handles maildir deduplication operations."""
    
    def __init__(self, maildir_path: Path, dry_run: bool = True):
        self.maildir_path = maildir_path
        self.cur_path = maildir_path / "cur"
        self.dry_run = dry_run
        self.logger = logging.getLogger(__name__)
        self.messages: Dict[str, List[Path]] = defaultdict(list)
        self.content_hashes: Dict[str, List[Path]] = defaultdict(list)
        
    def parse_maildir_filename(self, filename: str) -> Tuple[Optional[int], str]:
        """Extract timestamp and flags from maildir filename."""
        parts = filename.split(',')
        if parts:
            base = parts[0]
            flags = parts[1] if len(parts) > 1 else ''
            
            # Extract timestamp (usually first part before first dot)
            timestamp_parts = base.split('.')
            if timestamp_parts:
                try:
                    timestamp = int(timestamp_parts[0])
                    return timestamp, flags
                except (ValueError, IndexError):
                    pass
        return None, ''
    
    def get_message_id(self, file_path: Path) -> Optional[str]:
        """Extract Message-ID from email file."""
        try:
            with open(file_path, 'rb') as f:
                msg = email.message_from_binary_file(f, policy=email.policy.default)
                msg_id = msg.get('Message-ID', '')
                if msg_id:
                    return msg_id.strip()
        except Exception as e:
            self.logger.warning(f"Error reading {file_path}: {e}")
        return None
    
    def get_content_hash(self, file_path: Path) -> Optional[str]:
        """Generate hash of email content (excluding headers that might vary)."""
        try:
            with open(file_path, 'rb') as f:
                msg = email.message_from_binary_file(f, policy=email.policy.default)
                
                # Create hash from stable parts of the message
                hasher = hashlib.sha256()
                
                # Hash key headers
                for header in ['From', 'To', 'Subject', 'Date']:
                    value = msg.get(header, '')
                    if value:
                        hasher.update(value.encode('utf-8', errors='ignore'))
                
                # Hash body
                body = msg.get_body(preferencelist=('plain', 'html'))
                if body:
                    content = body.get_content()
                    if content:
                        hasher.update(content.encode('utf-8', errors='ignore'))
                
                return hasher.hexdigest()
        except Exception as e:
            self.logger.warning(f"Error hashing {file_path}: {e}")
        return None
    
    def analyze_duplicates(self) -> Dict:
        """Analyze all messages and identify duplicates."""
        self.logger.info(f"Analyzing messages in {self.cur_path}")
        
        if not self.cur_path.exists():
            raise FileNotFoundError(f"Maildir cur directory not found: {self.cur_path}")
        
        files = list(self.cur_path.glob("*"))
        total_files = len(files)
        self.logger.info(f"Found {total_files} files to analyze")
        
        # Process all files
        for i, file_path in enumerate(files, 1):
            if i % 1000 == 0:
                self.logger.info(f"Progress: {i}/{total_files} files processed")
            
            # Try Message-ID first
            msg_id = self.get_message_id(file_path)
            if msg_id:
                self.messages[msg_id].append(file_path)
            else:
                # Fall back to content hash
                content_hash = self.get_content_hash(file_path)
                if content_hash:
                    self.content_hashes[content_hash].append(file_path)
                else:
                    self.logger.warning(f"Could not process: {file_path}")
        
        # Calculate statistics
        unique_by_msgid = len(self.messages)
        unique_by_hash = len(self.content_hashes)
        duplicates_by_msgid = sum(len(files) - 1 for files in self.messages.values() if len(files) > 1)
        duplicates_by_hash = sum(len(files) - 1 for files in self.content_hashes.values() if len(files) > 1)
        
        stats = {
            'total_files': total_files,
            'unique_by_message_id': unique_by_msgid,
            'unique_by_content_hash': unique_by_hash,
            'total_unique': unique_by_msgid + unique_by_hash,
            'duplicates_by_message_id': duplicates_by_msgid,
            'duplicates_by_content_hash': duplicates_by_hash,
            'total_duplicates': duplicates_by_msgid + duplicates_by_hash
        }
        
        self.logger.info(f"Analysis complete: {stats}")
        return stats
    
    def create_deduplication_plan(self) -> List[Path]:
        """Create a plan for which files to delete."""
        files_to_delete = []
        
        # Process Message-ID duplicates
        for msg_id, file_list in self.messages.items():
            if len(file_list) > 1:
                # Sort by timestamp (keep oldest)
                sorted_files = []
                for f in file_list:
                    timestamp, _ = self.parse_maildir_filename(f.name)
                    sorted_files.append((timestamp or float('inf'), f))
                sorted_files.sort(key=lambda x: x[0])
                
                # Keep the first (oldest), mark others for deletion
                for _, f in sorted_files[1:]:
                    files_to_delete.append(f)
        
        # Process content hash duplicates
        for content_hash, file_list in self.content_hashes.items():
            if len(file_list) > 1:
                # Sort by timestamp (keep oldest)
                sorted_files = []
                for f in file_list:
                    timestamp, _ = self.parse_maildir_filename(f.name)
                    sorted_files.append((timestamp or float('inf'), f))
                sorted_files.sort(key=lambda x: x[0])
                
                # Keep the first (oldest), mark others for deletion
                for _, f in sorted_files[1:]:
                    files_to_delete.append(f)
        
        # Remove duplicates from deletion list (in case a file appears in both categories)
        files_to_delete = list(set(files_to_delete))
        
        self.logger.info(f"Plan created: {len(files_to_delete)} files to delete")
        return files_to_delete
    
    def save_plan(self, files_to_delete: List[Path], stats: Dict):
        """Save the deduplication plan to a JSON file."""
        plan = {
            'timestamp': datetime.now().isoformat(),
            'statistics': stats,
            'files_to_delete': [str(f) for f in files_to_delete],
            'expected_remaining': stats['total_files'] - len(files_to_delete)
        }
        
        with open(PLAN_FILE, 'w') as f:
            json.dump(plan, f, indent=2)
        
        self.logger.info(f"Plan saved to {PLAN_FILE}")
        return plan
    
    def load_plan(self) -> Optional[Dict]:
        """Load a previously saved deduplication plan."""
        if PLAN_FILE.exists():
            with open(PLAN_FILE, 'r') as f:
                return json.load(f)
        return None
    
    def create_backup(self):
        """Create a complete backup of the Archive folder."""
        self.logger.info(f"Creating backup at {BACKUP_DIR}")
        
        if BACKUP_DIR.exists():
            self.logger.error(f"Backup directory already exists: {BACKUP_DIR}")
            raise FileExistsError("Backup directory already exists")
        
        # Copy entire Archive folder
        shutil.copytree(self.maildir_path, BACKUP_DIR)
        
        # Verify backup
        backup_cur = BACKUP_DIR / "cur"
        backup_files = list(backup_cur.glob("*"))
        original_files = list(self.cur_path.glob("*"))
        
        if len(backup_files) != len(original_files):
            raise ValueError(f"Backup verification failed: {len(backup_files)} != {len(original_files)}")
        
        self.logger.info(f"Backup created successfully: {len(backup_files)} files")
    
    def check_sync_state_health(self) -> bool:
        """Check if mbsync state files are healthy."""
        journal_file = self.maildir_path / ".mbsyncstate.journal"
        if journal_file.exists():
            size_mb = journal_file.stat().st_size / (1024 * 1024)
            if size_mb > 1:  # If journal is > 1MB, likely corrupted
                self.logger.error(f"Large journal file detected: {size_mb:.1f}MB - indicates corrupted sync state")
                self.logger.error("This must be fixed before proceeding. Consider:")
                self.logger.error("1. Backing up and removing the journal file")
                self.logger.error("2. Running mbsync with --pull to reset from server state")
                return False
        return True
    
    def mark_files_with_trash_flag(self, files_to_delete: List[Path], batch_size: Optional[int] = None, 
                                   start_idx: int = 0, save_progress: bool = True):
        """Mark files with T (Trash) flag for mbsync to delete on server.
        
        Args:
            files_to_delete: List of files to mark for deletion
            batch_size: Process only this many files (for batching)
            start_idx: Start processing from this index
            save_progress: Save progress to state file
        """
        if batch_size:
            end_idx = min(start_idx + batch_size, len(files_to_delete))
            batch_files = files_to_delete[start_idx:end_idx]
            self.logger.info(f"Processing batch: files {start_idx+1} to {end_idx} of {len(files_to_delete)} total")
        else:
            batch_files = files_to_delete
            end_idx = len(files_to_delete)
        
        self.logger.info(f"Marking {len(batch_files)} files with T flag for deletion")
        
        marked_count = 0
        already_marked = 0
        errors = 0
        
        for file_path in batch_files:
            if file_path.exists():
                filename = file_path.name
                
                # Check if already has T flag
                if 'T' in filename.split(':2,')[-1] if ':2,' in filename else False:
                    already_marked += 1
                    self.logger.debug(f"Already marked: {filename}")
                    continue
                
                # Add T flag to filename
                # Maildir format: unique_name:2,FLAGS
                # FLAGS are in alphabetical order: D,F,P,R,S,T
                if ':2,' in filename:
                    # Has flags already, add T in correct position
                    base, flags = filename.rsplit(':2,', 1)
                    # Add T to flags maintaining alphabetical order
                    flag_set = set(flags)
                    flag_set.add('T')
                    new_flags = ''.join(sorted(flag_set))
                    new_filename = f"{base}:2,{new_flags}"
                else:
                    # No flags yet, add :2,T
                    new_filename = f"{filename}:2,T"
                
                new_path = file_path.parent / new_filename
                
                if self.dry_run:
                    self.logger.debug(f"[DRY RUN] Would rename: {filename} -> {new_filename}")
                    marked_count += 1
                else:
                    try:
                        file_path.rename(new_path)
                        self.logger.debug(f"Renamed: {filename} -> {new_filename}")
                        marked_count += 1
                    except Exception as e:
                        self.logger.error(f"Failed to rename {filename}: {e}")
                        errors += 1
            else:
                self.logger.warning(f"File no longer exists: {file_path}")
        
        # Save progress if requested
        if save_progress and not self.dry_run:
            self.save_batch_progress(end_idx, len(files_to_delete), marked_count, errors)
        
        self.logger.info(f"{'[DRY RUN] Would mark' if self.dry_run else 'Marked'} {marked_count} files with T flag")
        if already_marked > 0:
            self.logger.info(f"Skipped {already_marked} files already marked with T flag")
        if errors > 0:
            self.logger.warning(f"Encountered {errors} errors during marking")
        
        return marked_count, already_marked, errors
    
    def save_batch_progress(self, processed_idx: int, total_files: int, marked: int, errors: int):
        """Save batch processing progress."""
        state = {
            'timestamp': datetime.now().isoformat(),
            'processed_index': processed_idx,
            'total_files': total_files,
            'marked_count': marked,
            'error_count': errors,
            'remaining': total_files - processed_idx
        }
        
        # Load existing state if it exists
        if BATCH_STATE_FILE.exists():
            with open(BATCH_STATE_FILE, 'r') as f:
                existing = json.load(f)
                state['total_marked'] = existing.get('total_marked', 0) + marked
                state['total_errors'] = existing.get('total_errors', 0) + errors
        else:
            state['total_marked'] = marked
            state['total_errors'] = errors
        
        with open(BATCH_STATE_FILE, 'w') as f:
            json.dump(state, f, indent=2)
        
        self.logger.info(f"Progress saved: {processed_idx}/{total_files} files processed")
    
    def load_batch_progress(self) -> Optional[Dict]:
        """Load batch processing progress."""
        if BATCH_STATE_FILE.exists():
            with open(BATCH_STATE_FILE, 'r') as f:
                return json.load(f)
        return None
    
    def verify_deduplication(self) -> bool:
        """Verify that deduplication was successful."""
        all_files = list(self.cur_path.glob("*"))
        total_count = len(all_files)
        
        # Count T-flagged files (check if T is in the flags part after :2,)
        t_flagged = []
        for f in all_files:
            if ':2,' in f.name:
                flags = f.name.split(':2,')[-1]
                if 'T' in flags:
                    t_flagged.append(f)
        
        t_flagged_count = len(t_flagged)
        
        # Count non-T-flagged files (these will remain)
        remaining_count = total_count - t_flagged_count
        
        self.logger.info(f"Verification: {total_count} total files")
        self.logger.info(f"  - {t_flagged_count} marked with T flag for deletion")
        self.logger.info(f"  - {remaining_count} will remain after sync")
        
        # Allow some tolerance (±100 files) due to potential new messages
        if abs(remaining_count - EXPECTED_UNIQUE_COUNT) <= 100:
            self.logger.info(f"SUCCESS: Remaining count {remaining_count} is close to expected ({EXPECTED_UNIQUE_COUNT})")
            return True
        else:
            self.logger.warning(f"File count mismatch: {remaining_count} != {EXPECTED_UNIQUE_COUNT}")
            return False
    
    def check_oauth_token(self) -> bool:
        """Check if OAuth token is still valid."""
        try:
            # Try a quick mbsync list operation to test auth
            result = subprocess.run(
                ['mbsync', '--list', 'anu:Archive'],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if 'AccessTokenExpired' in result.stderr or 'OAuth' in result.stderr:
                self.logger.warning("OAuth token expired or invalid")
                return False
            
            return result.returncode == 0
        except Exception as e:
            self.logger.warning(f"Could not check OAuth token: {e}")
            return True  # Assume it's OK and let later operations fail if needed
    
    def restore_from_backup(self):
        """Restore from backup if something goes wrong."""
        if not BACKUP_DIR.exists():
            self.logger.error("No backup found to restore from")
            return False
        
        self.logger.info(f"Restoring from backup: {BACKUP_DIR}")
        
        # Remove current Archive folder
        if self.maildir_path.exists():
            shutil.rmtree(self.maildir_path)
        
        # Restore from backup
        shutil.copytree(BACKUP_DIR, self.maildir_path)
        
        self.logger.info("Restore completed")
        return True

def main():
    parser = argparse.ArgumentParser(description='Deduplicate ANU Archive maildir with batching support')
    parser.add_argument('--dry-run', action='store_true', default=False,
                        help='Perform dry run without making changes')
    parser.add_argument('--execute', action='store_true',
                        help='Execute the deduplication (requires prior dry-run)')
    parser.add_argument('--verify', action='store_true',
                        help='Verify the deduplication results')
    parser.add_argument('--restore', action='store_true',
                        help='Restore from backup')
    parser.add_argument('--verbose', action='store_true',
                        help='Enable verbose logging')
    parser.add_argument('--no-backup', action='store_true',
                        help='Skip backup creation (not recommended)')
    parser.add_argument('--batch', action='store_true',
                        help='Process files in batches')
    parser.add_argument('--batch-size', type=int, default=DEFAULT_BATCH_SIZE,
                        help=f'Number of files per batch (default: {DEFAULT_BATCH_SIZE})')
    parser.add_argument('--batch-number', type=int, default=0,
                        help='Batch number to process (0-based)')
    parser.add_argument('--resume', action='store_true',
                        help='Resume from last batch position')
    parser.add_argument('--check-health', action='store_true',
                        help='Check sync state health before proceeding')
    
    args = parser.parse_args()
    
    # Setup logging
    logger = setup_logging(args.verbose)
    
    # Determine mode
    if args.restore:
        logger.info("=== RESTORE MODE ===")
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=False)
        success = dedup.restore_from_backup()
        sys.exit(0 if success else 1)
    
    if args.verify:
        logger.info("=== VERIFY MODE ===")
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=True)
        success = dedup.verify_deduplication()
        sys.exit(0 if success else 1)
    
    if args.check_health:
        logger.info("=== HEALTH CHECK ===")
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=True)
        if dedup.check_sync_state_health():
            logger.info("Sync state appears healthy")
            sys.exit(0)
        else:
            logger.error("Sync state issues detected - fix before proceeding")
            sys.exit(1)
    
    if args.execute:
        logger.info("=== EXECUTE MODE ===")
        
        # Load existing plan
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=False)
        
        # Check sync state health first
        if not dedup.check_sync_state_health():
            logger.error("Sync state is corrupted. Fix this before proceeding.")
            sys.exit(1)
        
        plan = dedup.load_plan()
        
        if not plan:
            logger.error("No deduplication plan found. Please run with --dry-run first")
            sys.exit(1)
        
        logger.info(f"Loaded plan: {plan['expected_remaining']} files will remain")
        
        # Check OAuth token
        if not dedup.check_oauth_token():
            logger.error("OAuth token expired. Please re-authenticate:")
            logger.error("  cd ~/.dotfiles/mail && ./reauth-anu-oauth.sh")
            sys.exit(1)
        
        # Create backup unless explicitly skipped
        if not args.no_backup and not args.batch:
            try:
                dedup.create_backup()
            except Exception as e:
                logger.error(f"Backup failed: {e}")
                sys.exit(1)
        
        files_to_delete = [Path(f) for f in plan['files_to_delete']]
        
        if args.batch:
            # Batch processing mode
            logger.info(f"=== BATCH MODE: size={args.batch_size} ===")
            
            if args.resume:
                # Resume from saved state
                progress = dedup.load_batch_progress()
                if progress:
                    start_idx = progress['processed_index']
                    logger.info(f"Resuming from index {start_idx}")
                else:
                    start_idx = 0
            else:
                start_idx = args.batch_number * args.batch_size
            
            if start_idx >= len(files_to_delete):
                logger.info("All files already processed!")
                sys.exit(0)
            
            # Process one batch
            marked, already, errors = dedup.mark_files_with_trash_flag(
                files_to_delete, 
                batch_size=args.batch_size,
                start_idx=start_idx
            )
            
            remaining = len(files_to_delete) - min(start_idx + args.batch_size, len(files_to_delete))
            logger.info(f"Batch complete. {remaining} files remaining to process")
            
            if remaining > 0:
                logger.info(f"Run again with --batch --resume to continue")
            else:
                logger.info("All batches complete! Run 'mbsync anu:Archive' to sync")
        else:
            # Process all at once (not recommended for large numbers)
            dedup.mark_files_with_trash_flag(files_to_delete)
            
            # Verify
            if dedup.verify_deduplication():
                logger.info("Deduplication completed successfully!")
                logger.info("Run 'mbsync anu:Archive' to sync changes with server")
            else:
                logger.warning("Verification shows unexpected file count")
                logger.warning("Review before syncing")
        
    else:
        # Default to dry-run
        logger.info("=== DRY RUN MODE ===")
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=True)
        
        # Analyze
        stats = dedup.analyze_duplicates()
        
        # Create plan
        files_to_delete = dedup.create_deduplication_plan()
        
        # Save plan
        plan = dedup.save_plan(files_to_delete, stats)
        
        # Report
        logger.info("=" * 50)
        logger.info("DRY RUN SUMMARY:")
        logger.info(f"  Current files: {stats['total_files']}")
        logger.info(f"  Unique messages: {stats['total_unique']}")
        logger.info(f"  Duplicates to remove: {len(files_to_delete)}")
        logger.info(f"  Files after dedup: {stats['total_files'] - len(files_to_delete)}")
        logger.info(f"  Expected count: {EXPECTED_UNIQUE_COUNT}")
        logger.info("=" * 50)
        
        if abs((stats['total_files'] - len(files_to_delete)) - EXPECTED_UNIQUE_COUNT) <= 100:
            logger.info("Plan looks good! Next steps:")
            logger.info("1. Run with --execute --batch to process in batches")
            logger.info("2. Or run with --execute to process all at once (may hit throttling)")
        else:
            logger.warning(f"File count mismatch - expected ~{EXPECTED_UNIQUE_COUNT}, will have {stats['total_files'] - len(files_to_delete)}")
            logger.warning("Review the plan carefully before proceeding")

if __name__ == "__main__":
    main()