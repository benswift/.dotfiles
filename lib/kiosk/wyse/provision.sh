# shellcheck shell=bash
# shellcheck disable=SC2154  # main's locals (bin/kiosk), read by dynamic scope
#
# The wyse target of bin/kiosk: network-install one Dell Wyse 5070 cabled
# straight into --interface on this machine. Sourced by bin/kiosk.
#
# While it runs, the NIC is taken from NetworkManager and given
# $WYSE_SERVER_IP, and a private dnsmasq serves DHCP, DNS and TFTP on it, with
# NAT out of this machine's uplink so the installer and first boot can reach
# Debian's mirror and Tailscale. Everything is undone on exit, however it
# exits.
#
# The Wyse UEFI-PXE-boots Debian's signed netboot shim and GRUB; grub.cfg
# starts an unattended install (preseed.cfg, served over HTTP by
# lib/kiosk/serve.py with this box's user-data). late.sh seeds cloud-init and
# calls back, at which point grub.cfg becomes installed.cfg so a box that still
# boots the network first boots its disk. First boot phones home,
# reboots into the kiosk, and this checks over SSH that Chromium is up.
#
# Once per box, in the BIOS (F2; Dell's default BIOS password on Wyse units
# is often "Fireport"): set Power Management > AC Recovery to Power On, so the
# kiosk comes back after a power cut, and make sure UEFI network boot is
# enabled. Then F12 at power-on picks the onboard NIC for this one-off boot.

readonly WYSE_SERVER_IP=192.168.242.1
readonly WYSE_HTTP_PORT=8242
readonly WYSE_SUBNET=192.168.242.0/24
readonly DEBIAN_SUITE=trixie
readonly NETBOOT_URL="https://deb.debian.org/debian/dists/$DEBIAN_SUITE/main/installer-amd64/current/images"
readonly KIOSK_CACHE_DIR="${KIOSK_CACHE_DIR:-$HOME/.cache/kiosk}"
WYSE_ASSETS="$(dirname "${BASH_SOURCE[0]}")"
readonly WYSE_ASSETS

# Download $1 to $2 unless it is already there with sha256 $3; verify either way.
fetch_verified() {
    local url="$1" dest="$2" sha="$3"
    if [[ -f "$dest" ]] && sha256sum --quiet -c <<<"$sha  $dest" 2>/dev/null; then
        return 0
    fi
    warn "Downloading $url ..." >&2
    curl -fL -o "$dest.partial" "$url" >&2 || die "download failed: $url"
    sha256sum --quiet -c <<<"$sha  $dest.partial" || {
        rm -f "$dest.partial"
        die "checksum mismatch for $url"
    }
    mv "$dest.partial" "$dest"
}

# Debian's netboot tree, cached and keyed by checksum, so a Debian point
# release fetches afresh. Prints the tarball's path.
fetch_netboot() {
    local cache="$KIOSK_CACHE_DIR/debian-$DEBIAN_SUITE" sha
    mkdir -p "$cache"
    sha=$(curl -fsS "$NETBOOT_URL/SHA256SUMS" | awk '$2 == "./netboot/netboot.tar.gz" {print $1}')
    [[ -n "$sha" ]] || die "could not read Debian's SHA256SUMS"
    fetch_verified "$NETBOOT_URL/netboot/netboot.tar.gz" "$cache/netboot.tar.gz" "$sha"
    echo "$cache/netboot.tar.gz"
}

# --- network up/down ---------------------------------------------------------
# Each step records what it changed, so teardown undoes exactly that. Globals,
# not main's locals: the EXIT trap runs after main has returned.
wyse_iface=""
wyse_work=""
wyse_pxe=""
wyse_was_managed=false
wyse_set_forward=false
wyse_addr_added=false
wyse_nft_added=false
wyse_dnsmasq_pid=""
wyse_serve_pid=""

wyse_network_up() {
    local interface="$wyse_iface"
    if command -v nmcli >/dev/null 2>&1 &&
        nmcli -g GENERAL.STATE device show "$interface" 2>/dev/null | grep -qv unmanaged; then
        sudo nmcli device set "$interface" managed no
        wyse_was_managed=true
    fi
    sudo ip link set "$interface" up
    sudo ip addr add "$WYSE_SERVER_IP/24" dev "$interface"
    wyse_addr_added=true
    if [[ "$(cat /proc/sys/net/ipv4/ip_forward)" != 1 ]]; then
        sudo sysctl -q net.ipv4.ip_forward=1
        wyse_set_forward=true
    fi
    sudo nft -f - <<EOF
table ip kiosk_pxe {
    chain postrouting {
        type nat hook postrouting priority srcnat;
        ip saddr $WYSE_SUBNET oifname != "$interface" masquerade
    }
    chain forward {
        type filter hook forward priority filter;
        iifname "$interface" accept
        oifname "$interface" ct state established,related accept
    }
}
EOF
    wyse_nft_added=true
}

wyse_teardown() {
    set +e
    [[ -z "$wyse_serve_pid" ]] || kill "$wyse_serve_pid" 2>/dev/null
    [[ -z "$wyse_dnsmasq_pid" ]] || sudo kill "$wyse_dnsmasq_pid" 2>/dev/null
    [[ "$wyse_nft_added" == false ]] || sudo nft delete table ip kiosk_pxe
    [[ "$wyse_set_forward" == false ]] || sudo sysctl -q net.ipv4.ip_forward=0
    [[ "$wyse_addr_added" == false ]] || sudo ip addr del "$WYSE_SERVER_IP/24" dev "$wyse_iface"
    [[ "$wyse_was_managed" == false ]] || sudo nmcli device set "$wyse_iface" managed yes
    [[ -z "$wyse_pxe" ]] || sudo rm -rf "$wyse_pxe"
    [[ -z "$wyse_work" ]] || rm -rf "$wyse_work"
}

wyse_start_dnsmasq() {
    local interface="$wyse_iface" pxe="$wyse_pxe" upstream=/run/systemd/resolve/resolv.conf
    [[ -f "$upstream" ]] || upstream=/etc/resolv.conf
    cat >"$pxe/dnsmasq.conf" <<EOF
interface=$interface
bind-interfaces
except-interface=lo
listen-address=$WYSE_SERVER_IP
resolv-file=$upstream
dhcp-range=192.168.242.100,192.168.242.199,255.255.255.0,1h
dhcp-option=option:router,$WYSE_SERVER_IP
dhcp-option=option:dns-server,$WYSE_SERVER_IP
dhcp-authoritative
dhcp-leasefile=$pxe/leases
enable-tftp
tftp-root=$pxe/tftp
dhcp-match=set:efi64,option:client-arch,7
dhcp-match=set:efi64,option:client-arch,9
dhcp-boot=tag:efi64,debian-installer/amd64/bootnetx64.efi
log-dhcp
log-facility=$pxe/dnsmasq.log
pid-file=$pxe/dnsmasq.pid
EOF
    sudo dnsmasq --conf-file="$pxe/dnsmasq.conf" || die "dnsmasq failed to start (see $pxe/dnsmasq.log)"
    wyse_dnsmasq_pid=$(sudo cat "$pxe/dnsmasq.pid")
}

# Block until serve.py records event $1, or $2 seconds pass (0 = forever).
# Prints the client IP that triggered it.
wyse_wait_event() {
    local name="$1" timeout="$2" waited=0 line
    while :; do
        line=$(grep -m1 "^$name " "$wyse_work/events" 2>/dev/null || true)
        if [[ -n "$line" ]]; then
            echo "${line#* }"
            return 0
        fi
        kill -0 "$wyse_serve_pid" 2>/dev/null || die "the provisioning HTTP server died"
        ((timeout == 0 || waited < timeout)) || return 1
        sleep 5
        waited=$((waited + 5))
    done
}

# After the kiosk reboots: SSH in and report what Chromium is showing.
wyse_verify() {
    local ip="$1" key="${ssh_key_file%.pub}" deadline=$((SECONDS + 600)) out
    local ssh_opts=(-o BatchMode=yes -o ConnectTimeout=5 -o StrictHostKeyChecking=no
        -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR)
    [[ -f "$key" ]] && ssh_opts+=(-i "$key")
    while ((SECONDS < deadline)); do
        local shown=""
        if out=$(ssh "${ssh_opts[@]}" "$username@$ip" \
            "systemctl --user is-active chromium-kiosk.service >/dev/null &&
             curl -fsS http://127.0.0.1:9222/json/list" 2>/dev/null); then
            shown=$(python3 -c 'import json, sys
pages = [p for p in json.load(sys.stdin) if p["type"] == "page"]
print(pages[0]["url"] if pages else "")' <<<"$out" 2>/dev/null || true)
        fi
        if [[ -n "$shown" ]]; then
            info "✓ Chromium is up, showing $shown"
            return 0
        fi
        sleep 10
    done
    warn "Couldn't confirm Chromium over SSH ($username@$ip) --- check the screen"
    return 1
}

provision_wyse() {
    local cmd missing=()
    for cmd in dnsmasq nft ip python3 tar sha256sum curl ssh; do
        command -v "$cmd" >/dev/null 2>&1 || missing+=("$cmd")
    done
    ((${#missing[@]} == 0)) || die "missing required tools: ${missing[*]}"
    [[ -d "/sys/class/net/$interface" ]] || die "no such network interface: $interface"
    [[ "$(ip -4 route show default | awk '{print $5; exit}')" != "$interface" ]] ||
        die "$interface carries this machine's default route --- pick a spare NIC"
    ! ip -4 -o addr show dev "$interface" | grep -q inet ||
        die "$interface already has an IPv4 address --- is it in use? Pick a spare NIC"

    local netboot
    netboot=$(fetch_netboot)
    mint_tailscale_key
    generate_config "$work"
    # Now, not after first boot: by then the new node has joined under this name.
    [[ -z "$ts_bearer" ]] || ts_remove_stale_nodes "$ts_bearer" "$tailscale_tag" "$hostname"

    # $work (private) holds the HTTP root, whose user-data carries the wifi
    # and Tailscale secrets; $pxe is world-readable, because dnsmasq drops to
    # an unprivileged user before it serves TFTP.
    local pxe
    pxe=$(mktemp -d -t kiosk-pxe-XXXXX)
    chmod 755 "$pxe"
    wyse_iface="$interface" wyse_work="$work" wyse_pxe="$pxe"
    trap wyse_teardown EXIT
    trap 'exit 130' INT TERM
    mkdir "$pxe/tftp" "$work/http"
    tar -xzf "$netboot" -C "$pxe/tftp"
    # Shim fetches GRUB from the TFTP root, not from beside itself.
    cp "$pxe/tftp/debian-installer/amd64/grubx64.efi" "$pxe/tftp/"
    local grub_cfg="$pxe/tftp/debian-installer/amd64/grub/grub.cfg"
    cp "$work/grub.cfg" "$grub_cfg"
    chmod -R a+rX "$pxe/tftp"
    mv "$work/user-data" "$work/meta-data" "$work/preseed.cfg" "$work/http/"
    cp "$WYSE_ASSETS/disk.sh" "$WYSE_ASSETS/late.sh" "$work/http/"
    : >"$work/events"

    wyse_network_up
    wyse_start_dnsmasq
    python3 -I "$WYSE_ASSETS/../serve.py" "$WYSE_SERVER_IP" "$WYSE_HTTP_PORT" \
        "$work/http" "$work/events" &
    wyse_serve_pid=$!

    info "Ready on $interface. Power on the Wyse cabled to it, press F12, and"
    info "pick the onboard NIC (UEFI IPv4). Waiting..."
    local ip
    ip=$(wyse_wait_event preseed 0)
    info "✓ Installer booted on $ip --- installing Debian (10--20 minutes)"
    ip=$(wyse_wait_event installed 3600) ||
        die "no install after an hour --- watch the Wyse's screen (dnsmasq log: $pxe/dnsmasq.log)"
    cp "$WYSE_ASSETS/installed.cfg" "$grub_cfg"
    chmod a+r "$grub_cfg"
    info "✓ Installed --- rebooting into first boot (cloud-init, Tailscale)"
    ip=$(wyse_wait_event phone-home 2700) ||
        die "first boot didn't finish in 45 minutes --- ssh $username@$ip and check 'cloud-init status --long'"
    info "✓ First boot configured --- rebooting into the kiosk"
    wyse_verify "$ip" || true

    echo
    info "✓ $hostname is ready --- unplug it and deploy."
    if [[ -n "$tailscale_authkey" ]]; then
        echo "Then: tailscale ssh $username@$hostname"
    fi
    echo "Change the URL later with: sudo kiosk-set-url <url>"
}
