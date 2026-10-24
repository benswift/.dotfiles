#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pyyaml==6.0.3"]
# ///
"""Render cloud-init data; executable assets never contain substituted values.

bin/kiosk supplies the ordered fields below as NUL-delimited UTF-8 on stdin.
NetworkManager keyfiles use GLib string escaping, not shell or YAML quoting.

Both targets get the same kiosk session. The pi target configures stock
Raspberry Pi OS; the wyse target is a bare Debian install (see wyse/preseed.cfg)
that also needs the session's packages, greetd autologin, and a phone-home so
bin/kiosk knows first boot finished.
"""

from __future__ import annotations

import base64
import re
import secrets
import sys
from pathlib import Path

import yaml

ASSETS = Path(__file__).parent
FIELDS = [
    "target",
    "server",
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


# Installed into the bare Debian of the wyse target. Raspberry Pi OS's desktop
# image already ships the session half of this.
WYSE_PACKAGES = [
    "openssh-server",
    "sudo",
    "cloud-init",
    "curl",
    "ca-certificates",
    "python3",
    "systemd-timesyncd",
    "network-manager",
    "wpasupplicant",
    "rfkill",
    "greetd",
    "labwc",
    "chromium",
    "fonts-noto-core",
    "fonts-noto-color-emoji",
    "intel-media-va-driver",
    "firmware-misc-nonfree",
    "firmware-realtek",
    "firmware-iwlwifi",
]


def wifi_profile(
    ssid: str, security: dict[str, dict[str, str]], *, pin_wlan0: bool
) -> str:
    """A NetworkManager keyfile. Only the Pi's wifi is reliably called wlan0;
    on x86 the name is derived from the card's bus position (wlp1s0, ...)."""
    sections = {
        "connection": {
            "id": ssid,
            "type": "wifi",
            **({"interface-name": "wlan0"} if pin_wlan0 else {}),
            "autoconnect": "true",
        },
        "wifi": {"mode": "infrastructure", "ssid": ssid},
        **security,
        "ipv4": {"method": "auto"},
        "ipv6": {"method": "auto"},
    }
    return "\n".join(
        f"[{section}]\n"
        + "".join(f"{key}={keyfile_string(value)}\n" for key, value in entries.items())
        for section, entries in sections.items()
    )


def fill(template: str, **values: str) -> str:
    for name, value in values.items():
        template = template.replace(f"@{name.upper()}@", value)
    return template


def render(values: dict[str, str], output: Path) -> None:
    target = values["target"]
    user = values["username"]
    host = values["hostname"]
    country = values["wifi_country"]
    wyse = target == "wyse"
    if target not in ("pi", "wyse"):
        raise SystemExit("invalid --target")
    for label, value, pattern in (
        ("username", user, r"[a-z_][a-z0-9_-]{0,31}"),
        ("hostname", host, r"[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"),
        ("wifi-country", country, r"[A-Z]{2}"),
        ("timezone", values["timezone"], r"[A-Za-z0-9_+-]+(?:/[A-Za-z0-9_+-]+)*"),
        ("server", values["server"], r"(?:[0-9]{1,3}\.){3}[0-9]{1,3}:[0-9]{1,5}|"),
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
    if wyse:
        # Wifi country is a kernel option on this target (see preseed.cfg).
        write_file(
            "/etc/greetd/config.toml",
            fill((ASSETS / "wyse/greetd.toml").read_text(), user=user),
        )
        commands = [
            ["systemctl", "enable", "ssh", "greetd"],
            ["systemctl", "set-default", "graphical.target"],
        ]
    else:
        commands = [
            ["systemctl", "enable", "--now", "ssh"],
            ["rm", "-f", "/etc/xdg/autostart/piwiz.desktop"],
            ["raspi-config", "nonint", "do_boot_behaviour", "B4"],
        ]
    if values["wifi_ssid"]:
        commands.append(["rfkill", "unblock", "wifi"])
        if not wyse:
            commands.append(["raspi-config", "nonint", "do_wifi_country", country])
        commands.append(["nmcli", "connection", "reload"])
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
    if not wyse:
        commands.append(
            [
                "sh",
                "-c",
                "if [ -f /usr/share/icons/PiXflat/cursors/left_ptr ]; then mv /usr/share/icons/PiXflat/cursors/left_ptr /usr/share/icons/PiXflat/cursors/left_ptr.bak; fi",
            ]
        )
    profile_path = "/etc/NetworkManager/system-connections/kiosk-wifi.nmconnection"
    if values["wifi_user"]:
        ca_path = "/etc/ssl/certs/kiosk-wifi-ca.pem"
        write_file(
            ca_path,
            base64.b64encode(Path(values["wifi_ca"]).read_bytes()).decode(),
            encoding="b64",
        )
        profile = wifi_profile(
            values["wifi_ssid"],
            {
                "wifi-security": {"key-mgmt": "wpa-eap"},
                "802-1x": {
                    "eap": "peap",
                    "identity": values["wifi_user"],
                    "password": values["wifi_pass"],
                    "phase2-auth": "mschapv2",
                    "ca-cert": ca_path,
                    "domain-suffix-match": values["wifi_domain"],
                },
            },
            pin_wlan0=not wyse,
        )
        write_file(profile_path, profile, mode="0600")
    elif values["wifi_ssid"] and wyse:
        # cloud-init networking is disabled on this target (see wyse/late.sh),
        # so PSK wifi is a keyfile like enterprise wifi rather than netplan.
        profile = wifi_profile(
            values["wifi_ssid"],
            {"wifi-security": {"key-mgmt": "wpa-psk", "psk": values["wifi_password"]}},
            pin_wlan0=False,
        )
        write_file(profile_path, profile, mode="0600")
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
    config: dict = {
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
                    # Raspberry Pi OS hardware groups, absent on Debian
                    *([] if wyse else ["gpio", "i2c", "spi"]),
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
    if wyse:
        # Runs after runcmd and before the reboot; bin/kiosk waits for it.
        config["phone_home"] = {
            "url": f"http://{values['server']}/phone-home",
            "post": ["hostname", "instance_id"],
            "tries": 10,
        }
    (output / "user-data").write_text(
        "#cloud-config\n" + yaml.safe_dump(config, sort_keys=False)
    )
    (output / "kiosk-url.txt").write_text(values["url"] + "\n")
    if wyse:
        (output / "meta-data").write_text(
            yaml.safe_dump({"instance-id": f"kiosk-{host}", "local-hostname": host})
        )
        placeholders = {
            "hostname": host,
            "timezone": values["timezone"],
            "server": values["server"],
            "country": country,
            "packages": " ".join(WYSE_PACKAGES),
            # Never used: late.sh deletes the account it unlocks.
            "installer_password": secrets.token_urlsafe(24),
        }
        for name in ("preseed.cfg", "grub.cfg"):
            text = fill((ASSETS / "wyse" / name).read_text(), **placeholders)
            (output / name).write_text(text)


if __name__ == "__main__":
    fields = sys.stdin.buffer.read().decode().split("\0")
    if fields.pop() != "":
        raise SystemExit("expected NUL-terminated configuration")
    render(dict(zip(FIELDS, fields, strict=True)), Path(sys.argv[1]))
