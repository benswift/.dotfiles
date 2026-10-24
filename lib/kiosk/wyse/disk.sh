#!/bin/sh
# Runs in the Debian installer as partman/early_command: pick the install disk
# and preseed it. A Wyse 5070 has a soldered eMMC and sometimes an M.2 SATA
# SSD as well; prefer the SSD. USB devices are never candidates.
set -eu
disk=""
for dev in /sys/block/sd* /sys/block/nvme*n* /sys/block/vd* /sys/block/mmcblk[0-9]; do
    [ -e "$dev" ] || continue
    [ "$(cat "$dev/removable")" = 0 ] || continue
    case "$(readlink -f "$dev")" in */usb*) continue ;; esac
    disk="/dev/${dev##*/}"
    break
done
[ -n "$disk" ] || { echo "kiosk: no internal disk found" >&2; exit 1; }
debconf-set partman-auto/disk "$disk"
debconf-set grub-installer/bootdev "$disk"
