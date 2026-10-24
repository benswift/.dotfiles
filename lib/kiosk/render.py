#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pyyaml==6.0.3"]
# ///
"""Render cloud-init data; executable assets never contain substituted values.

bin/kiosk supplies the ordered fields below as NUL-delimited UTF-8 on stdin.
NetworkManager keyfiles use GLib string escaping, not shell or YAML quoting.
"""

from __future__ import annotations

import base64
import re
import sys
from pathlib import Path

import yaml

ASSETS = Path(__file__).parent
FIELDS = [
    "hostname",
    "username",
    "passwd_hash",
    "lock_passwd",
    "timezone",
    "ssh_key",
    "url",
    "wifi_ssid",
    "wifi_password",
    "wifi_user",
    "wifi_pass",
    "wifi_country",
    "wifi_ca",
    "wifi_domain",
    "tailscale_key",
]


def keyfile_string(value: str) -> str:
    return "".join(
        {"\\": "\\\\", "\n": "\\n", "\r": "\\r", "\t": "\\t", " ": "\\s"}.get(c, c)
        for c in value
    )


def render(values: dict[str, str], output: Path) -> None:
    user = values["username"]
    host = values["hostname"]
    country = values["wifi_country"]
    for label, value, pattern in (
        ("username", user, r"[a-z_][a-z0-9_-]{0,31}"),
        ("hostname", host, r"[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"),
        ("wifi-country", country, r"[A-Z]{2}"),
    ):
        if re.fullmatch(pattern, value) is None:
            raise SystemExit(f"invalid --{label}")
    files: list[dict] = []

    def write_file(
        path: str,
        content: str,
        *,
        mode: str = "0644",
        owned: bool = False,
        **extra: str,
    ) -> None:
        item = {
            "path": path,
            "content": content,
            "permissions": mode,
            "owner": f"{user}:{user}" if owned else "root:root",
            **extra,
        }
        if owned:
            item["defer"] = True
        files.append(item)

    home = f"/home/{user}/.config"
    for asset in ("autostart", "rc.xml"):
        write_file(
            f"{home}/labwc/{asset}",
            (ASSETS / asset).read_text(),
            mode="0755" if asset == "autostart" else "0644",
            owned=True,
        )
    units = sorted(ASSETS.glob("chromium-*.*"))
    for asset in units:
        write_file(f"{home}/systemd/user/{asset.name}", asset.read_text(), owned=True)
    for asset, target in (
        ("kiosk-watchdog.py", "kiosk-watchdog"),
        ("kiosk-browser", "kiosk-browser"),
        ("kiosk-set-url", "kiosk-set-url"),
    ):
        write_file(
            f"/usr/local/bin/{target}", (ASSETS / asset).read_text(), mode="0755"
        )
    write_file("/etc/kiosk-url.txt", values["url"] + "\n")
    commands = [
        ["systemctl", "enable", "--now", "ssh"],
        ["rm", "-f", "/etc/xdg/autostart/piwiz.desktop"],
        ["raspi-config", "nonint", "do_boot_behaviour", "B4"],
    ]
    if values["wifi_ssid"]:
        commands.extend(
            [
                ["rfkill", "unblock", "wifi"],
                ["raspi-config", "nonint", "do_wifi_country", country],
                ["nmcli", "connection", "reload"],
            ]
        )
    unit_dir = f"{home}/systemd/user"
    commands.append(
        [
            "install",
            "-d",
            "-o",
            user,
            "-g",
            user,
            f"{unit_dir}/default.target.wants",
            f"{unit_dir}/timers.target.wants",
        ]
    )
    for name in (
        "chromium-kiosk.service",
        "chromium-restart.timer",
        "chromium-watchdog.timer",
    ):
        target = "timers" if name.endswith(".timer") else "default"
        link = f"{unit_dir}/{target}.target.wants/{name}"
        commands.extend(
            [["ln", "-sf", f"../{name}", link], ["chown", "-h", f"{user}:{user}", link]]
        )
    commands.append(
        [
            "sh",
            "-c",
            "if [ -f /usr/share/icons/PiXflat/cursors/left_ptr ]; then mv /usr/share/icons/PiXflat/cursors/left_ptr /usr/share/icons/PiXflat/cursors/left_ptr.bak; fi",
        ]
    )
    if values["wifi_user"]:
        ca_path = "/etc/ssl/certs/kiosk-wifi-ca.pem"
        write_file(
            ca_path,
            base64.b64encode(Path(values["wifi_ca"]).read_bytes()).decode(),
            encoding="b64",
        )
        sections = {
            "connection": {
                "id": values["wifi_ssid"],
                "type": "wifi",
                "interface-name": "wlan0",
                "autoconnect": "true",
            },
            "wifi": {"mode": "infrastructure", "ssid": values["wifi_ssid"]},
            "wifi-security": {"key-mgmt": "wpa-eap"},
            "802-1x": {
                "eap": "peap",
                "identity": values["wifi_user"],
                "password": values["wifi_pass"],
                "phase2-auth": "mschapv2",
                "ca-cert": ca_path,
                "domain-suffix-match": values["wifi_domain"],
            },
            "ipv4": {"method": "auto"},
            "ipv6": {"method": "auto"},
        }
        profile = "\n".join(
            f"[{section}]\n"
            + "".join(
                f"{key}={keyfile_string(value)}\n" for key, value in entries.items()
            )
            for section, entries in sections.items()
        )
        write_file(
            "/etc/NetworkManager/system-connections/kiosk-wifi.nmconnection",
            profile,
            mode="0600",
        )
    elif values["wifi_ssid"]:
        network = {
            "version": 2,
            "wifis": {
                "wlan0": {
                    "dhcp4": True,
                    "optional": True,
                    "access-points": {
                        values["wifi_ssid"]: {"password": values["wifi_password"]}
                    },
                }
            },
        }
        (output / "network-config").write_text(yaml.safe_dump(network, sort_keys=False))
    if values["tailscale_key"]:
        write_file("/etc/kiosk-tailscale-authkey", values["tailscale_key"], mode="0600")
        write_file(
            "/usr/local/bin/kiosk-tailscale",
            (ASSETS / "kiosk-tailscale").read_text(),
            mode="0700",
        )
        commands.append(["/usr/local/bin/kiosk-tailscale"])
    config = {
        "hostname": host,
        "manage_etc_hosts": True,
        "users": [
            {
                "name": user,
                "gecos": "Kiosk",
                "shell": "/bin/bash",
                "groups": [
                    "adm",
                    "sudo",
                    "video",
                    "render",
                    "input",
                    "audio",
                    "tty",
                    "netdev",
                    "gpio",
                    "i2c",
                    "spi",
                ],
                "lock_passwd": values["lock_passwd"] == "true",
                "passwd": values["passwd_hash"],
                "sudo": "ALL=(ALL) NOPASSWD:ALL",
                "ssh_authorized_keys": [values["ssh_key"]],
            }
        ],
        "ssh_pwauth": False,
        "timezone": values["timezone"],
        "write_files": files,
        "runcmd": commands,
        "power_state": {
            "mode": "reboot",
            "message": "kiosk first-boot config complete; rebooting into the kiosk",
        },
        "final_message": "kiosk setup complete after $UPTIME seconds",
    }
    (output / "user-data").write_text(
        "#cloud-config\n" + yaml.safe_dump(config, sort_keys=False)
    )
    (output / "kiosk-url.txt").write_text(values["url"] + "\n")


if __name__ == "__main__":
    fields = sys.stdin.buffer.read().decode().split("\0")
    if fields.pop() != "":
        raise SystemExit("expected NUL-terminated configuration")
    render(dict(zip(FIELDS, fields, strict=True)), Path(sys.argv[1]))
