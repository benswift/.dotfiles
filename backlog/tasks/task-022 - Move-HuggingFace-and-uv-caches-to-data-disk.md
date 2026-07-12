---
id: TASK-022
title: Move HuggingFace and uv caches to /data disk
status: To Do
assignee: []
created_date: '2026-07-12 05:51'
labels:
  - storage
  - maintenance
dependencies: []
priority: medium
---

## Description

<!-- SECTION:DESCRIPTION:BEGIN -->
Weddle's root volume (/dev/nvme0n1p2, 1.8T) was ~91% full. A newly-discovered empty 3.7T second NVMe was formatted ext4 and mounted at /data (auto-mounts via fstab, UUID a35a7198-...). Ollama (40G), the received rclone backup (~/backup -> /data/backup symlink, 69G) and raspios-images (11G) have already been relocated/deleted, taking / down to ~84%. The two remaining large caches must MOVE to /data (not be deleted --- re-downloading HF weights over the network is slow). Both are deferred because the long-running panic_tda job (Elixir mix experiment.run, PID was 829468) has FLUX.2 weights mmap'd from ~/.cache/huggingface and uses a venv that may touch the uv cache; moving either mid-run risks breaking it. Do this once panic_tda has finished. ~/.cache/huggingface is ~692G and ~/.cache/uv is ~187G; relocating both drops / from ~84% to ~35%.
<!-- SECTION:DESCRIPTION:END -->

## Acceptance Criteria
<!-- AC:BEGIN -->
- [ ] #1 panic_tda job confirmed finished (no python/mix experiment process holding ~/.cache/huggingface open) before starting
- [ ] #2 ~/.cache/huggingface moved to /data (e.g. /data/huggingface) with HF_HOME set in zshenv so tools find it
- [ ] #3 ~/.cache/uv moved to /data (e.g. /data/uv-cache) with UV_CACHE_DIR set in zshenv
- [ ] #4 Integrity verified (file count + byte totals match) before deleting the originals
- [ ] #5 A subsequent huggingface/uv operation resolves the new location correctly and / usage has dropped to roughly 35%
<!-- AC:END -->
