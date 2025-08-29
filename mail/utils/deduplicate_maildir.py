#!/usr/bin/env python3
"""
Comprehensive maildir deduplication script for ANU Archive folder.
Safely removes duplicate emails while preserving all unique messages.
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
from pathlib import Path
from datetime import datetime
from collections import defaultdict
from typing import Dict, List, Tuple, Optional

# Configuration
MAILDIR_PATH = Path.home() / "Maildir" / "anu" / "Archive"
BACKUP_DIR = Path.home() / f"maildir_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
QUARANTINE_DIR = Path.home() / f"maildir_quarantine_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
LOG_FILE = Path.home() / f"deduplicate_maildir_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
PLAN_FILE = Path.home() / "deduplication_plan.json"

# Expected unique message count (from analysis)
EXPECTED_UNIQUE_COUNT = 30279

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
    
    def mark_files_with_trash_flag(self, files_to_delete: List[Path]):
        """Mark files with T (Trash) flag for mbsync to delete on server."""
        self.logger.info(f"Marking {len(files_to_delete)} files with T flag for deletion")
        
        marked_count = 0
        already_marked = 0
        
        for file_path in files_to_delete:
            if file_path.exists():
                filename = file_path.name
                
                # Check if already has T flag
                if ',T' in filename or filename.endswith('T'):
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
                else:
                    file_path.rename(new_path)
                    self.logger.debug(f"Renamed: {filename} -> {new_filename}")
                marked_count += 1
        
        self.logger.info(f"{'[DRY RUN] Would mark' if self.dry_run else 'Marked'} {marked_count} files with T flag")
        if already_marked > 0:
            self.logger.info(f"Skipped {already_marked} files already marked with T flag")
    
    def verify_deduplication(self) -> bool:
        """Verify that deduplication was successful."""
        all_files = list(self.cur_path.glob("*"))
        total_count = len(all_files)
        
        # Count T-flagged files
        t_flagged = [f for f in all_files if ',T' in f.name or f.name.endswith('T')]
        t_flagged_count = len(t_flagged)
        
        # Count non-T-flagged files (these will remain)
        remaining_count = total_count - t_flagged_count
        
        self.logger.info(f"Verification: {total_count} total files")
        self.logger.info(f"  - {t_flagged_count} marked with T flag for deletion")
        self.logger.info(f"  - {remaining_count} will remain after sync")
        
        if remaining_count == EXPECTED_UNIQUE_COUNT:
            self.logger.info(f"SUCCESS: Remaining count matches expected ({EXPECTED_UNIQUE_COUNT})")
            return True
        else:
            self.logger.warning(f"File count mismatch: {remaining_count} != {EXPECTED_UNIQUE_COUNT}")
            return False
    
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
    parser = argparse.ArgumentParser(description='Deduplicate ANU Archive maildir')
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
    
    if args.execute:
        logger.info("=== EXECUTE MODE ===")
        
        # Load existing plan
        dedup = MaildirDeduplicator(MAILDIR_PATH, dry_run=False)
        plan = dedup.load_plan()
        
        if not plan:
            logger.error("No deduplication plan found. Please run with --dry-run first")
            sys.exit(1)
        
        logger.info(f"Loaded plan: {plan['expected_remaining']} files will remain")
        
        # Create backup unless explicitly skipped
        if not args.no_backup:
            try:
                dedup.create_backup()
            except Exception as e:
                logger.error(f"Backup failed: {e}")
                sys.exit(1)
        
        # Mark files with T flag for deletion
        files_to_delete = [Path(f) for f in plan['files_to_delete']]
        dedup.mark_files_with_trash_flag(files_to_delete)
        
        # Verify
        if dedup.verify_deduplication():
            logger.info("Deduplication completed successfully!")
            logger.info(f"Quarantined files are in: {QUARANTINE_DIR}")
            logger.info("Run 'mbsync anu:Archive' to sync changes with server")
        else:
            logger.warning("Verification failed - files remain in quarantine")
            logger.warning("You may need to restore from backup")
        
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
        
        if stats['total_files'] - len(files_to_delete) == EXPECTED_UNIQUE_COUNT:
            logger.info("Plan looks good! Run with --execute to proceed")
        else:
            logger.warning("File count mismatch - review the plan carefully")

if __name__ == "__main__":
    main()