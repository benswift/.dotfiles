#!/bin/sh
# Runs in the Debian installer as preseed/late_command, with the new system
# mounted at /target. Seeds cloud-init so first boot configures the kiosk from
# the same user-data a Pi gets, then tells the provisioning server we're done.
set -eu
server="$1"

seed=/target/var/lib/cloud/seed/nocloud
mkdir -p "$seed"
wget -q -O "$seed/user-data" "$server/user-data"
wget -q -O "$seed/meta-data" "$server/meta-data"
chmod 600 "$seed/user-data"

# NetworkManager owns every interface on the deployed kiosk (wifi profiles come
# from user-data), so neither the installer's ifupdown stanza nor cloud-init's
# fallback DHCP config may claim the NIC first.
cat >/target/etc/cloud/cloud.cfg.d/90-kiosk.cfg <<'EOF'
datasource_list: [NoCloud, None]
network: {config: disabled}
EOF
cat >/target/etc/network/interfaces <<'EOF'
auto lo
iface lo inet loopback
EOF

# The account the installer insisted on (see preseed.cfg).
in-target userdel --remove installer

wget -q -O /dev/null "$server/installed" || true
