#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["pytest", "pytest-xdist", "pyyaml"]
# ///
"""Check generated kiosk security settings without flashing or network access."""

from __future__ import annotations

import ast
import base64
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "kiosk"

# pi-kiosk refuses to run off Linux (it flashes raw devices), even in --test.
pytestmark = pytest.mark.skipif(
    not sys.platform.startswith("linux"), reason="pi-kiosk is Linux only"
)


@pytest.fixture
def public_key(tmp_path: Path) -> Path:
    key = tmp_path / "id_ed25519"
    subprocess.run(
        ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(key)],
        check=True,
    )
    return key.with_suffix(".pub")


def generate(
    tmp_path: Path, *args: str, target: str = "pi"
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [
            "bash",
            str(SCRIPT),
            "--test",
            "--target",
            target,
            "--url",
            "https://example.com/",
            *args,
        ],
        env={**os.environ, "TMPDIR": str(tmp_path)},
        capture_output=True,
        text=True,
        check=False,
    )


def output_dir(result: subprocess.CompletedProcess[str]) -> Path:
    assert result.returncode == 0, result.stderr
    match = re.search(r"config generated in (\S+)", result.stdout)
    assert match is not None
    return Path(match[1])


def config(result: subprocess.CompletedProcess[str]) -> dict:
    return yaml.safe_load((output_dir(result) / "user-data").read_text())


def test_requires_known_target(tmp_path: Path, public_key: Path) -> None:
    result = generate(tmp_path, "--ssh-key", str(public_key), target="")
    assert result.returncode != 0
    assert "--target is required" in result.stderr
    result = generate(tmp_path, "--ssh-key", str(public_key), target="x86")
    assert result.returncode != 0
    assert "unknown --target" in result.stderr


def test_requires_public_key(tmp_path: Path) -> None:
    result = generate(tmp_path)
    assert result.returncode != 0
    assert "--ssh-key" in result.stderr


def test_rejects_private_key(tmp_path: Path, public_key: Path) -> None:
    result = generate(tmp_path, "--ssh-key", str(public_key.with_suffix("")))
    assert result.returncode != 0
    assert "not a private key" in result.stderr


@pytest.mark.parametrize("target", ["pi", "wyse"])
@pytest.mark.parametrize("password", [None, "test-console-password"])
def test_ssh_stays_key_only(
    tmp_path: Path, public_key: Path, password: str | None, target: str
) -> None:
    args = ["--ssh-key", str(public_key)]
    if password is not None:
        args.extend(["--password", password])
    data = config(generate(tmp_path, *args, target=target))
    assert data["ssh_pwauth"] is False
    user = data["users"][0]
    assert user["lock_passwd"] is (password is None)
    assert user["ssh_authorized_keys"][0].strip() == public_key.read_text().strip()
    if password is None:
        assert user["passwd"] == "!"
    else:
        assert user["passwd"].startswith("$6$")
        assert password not in user["passwd"]


def enterprise_args(public_key: Path) -> list[str]:
    return [
        "--ssh-key",
        str(public_key),
        "--wifi-ssid",
        "AUDIT-NONREAL",
        "--wifi-enterprise-user",
        "audit@example.invalid",
        "--wifi-enterprise-pass",
        "synthetic-password",
    ]


def test_enterprise_requires_trust_settings(tmp_path: Path, public_key: Path) -> None:
    result = generate(tmp_path, *enterprise_args(public_key))
    assert result.returncode != 0
    assert "--wifi-ca-cert and --wifi-domain-suffix" in result.stderr


def test_enterprise_embeds_ca_and_checks_server(
    tmp_path: Path, public_key: Path
) -> None:
    cert = tmp_path / "ca.pem"
    subprocess.run(
        [
            "openssl",
            "req",
            "-x509",
            "-newkey",
            "rsa:2048",
            "-noenc",
            "-keyout",
            str(tmp_path / "ca.key"),
            "-out",
            str(cert),
            "-days",
            "1",
            "-subj",
            "/CN=Test CA",
        ],
        check=True,
        capture_output=True,
    )
    args = [*enterprise_args(public_key), "--wifi-ca-cert", str(cert)]
    result = generate(tmp_path, *args, "--wifi-domain-suffix", "radius.example.edu")
    data = config(result)
    files = {entry["path"]: entry for entry in data["write_files"]}
    ca = files["/etc/ssl/certs/kiosk-wifi-ca.pem"]
    assert base64.b64decode(ca["content"]) == cert.read_bytes()
    profile = files["/etc/NetworkManager/system-connections/kiosk-wifi.nmconnection"]
    assert "ca-cert=/etc/ssl/certs/kiosk-wifi-ca.pem" in profile["content"]
    assert "domain-suffix-match=radius.example.edu" in profile["content"]
    result = generate(
        tmp_path, *args, "--wifi-domain-suffix", "radius.example.edu\ninjected=yes"
    )
    assert result.returncode != 0
    cert.write_text("not a certificate")
    result = generate(tmp_path, *args, "--wifi-domain-suffix", "radius.example.edu")
    assert result.returncode != 0
    assert "valid PEM CA certificate" in result.stderr


@pytest.mark.parametrize(
    "url",
    [
        'https://example.com/"; print("INJECTED"); #',
        "https://example.com/'; touch /tmp/INJECTED; #",
        "https://example.com/$(id)?a=1&b=@USERNAME@",
        "https://example.com/\nruncmd: [injected]",
    ],
)
def test_url_stays_data(tmp_path: Path, public_key: Path, url: str) -> None:
    data = config(generate(tmp_path, "--ssh-key", str(public_key), "--url", url))
    files = {entry["path"]: entry["content"] for entry in data["write_files"]}
    assert files["/etc/kiosk-url.txt"] == url + "\n"
    assets = SCRIPT.parent.parent / "lib/kiosk"
    assert (
        files["/usr/local/bin/kiosk-browser"] == (assets / "kiosk-browser").read_text()
    )
    watchdog = files["/usr/local/bin/kiosk-watchdog"]
    assert watchdog == (assets / "kiosk-watchdog.py").read_text()
    ast.parse(watchdog)
    assert all(url not in str(command) for command in data["runcmd"])


def test_wifi_and_tailscale_values_stay_data(tmp_path: Path, public_key: Path) -> None:
    ssid = 'WiFi: "quoted" # test'
    password = 'pass\\word\n"quoted" $(id) @URL@'
    key = 'synthetic; echo "INJECTED"'
    result = generate(
        tmp_path,
        "--ssh-key",
        str(public_key),
        "--wifi-ssid",
        ssid,
        "--wifi-password",
        password,
        "--tailscale-authkey",
        key,
    )
    data = config(result)
    match = re.search(r"config generated in (\S+)", result.stdout)
    assert match is not None
    network = yaml.safe_load((Path(match[1]) / "network-config").read_text())
    assert network["wifis"]["wlan0"]["access-points"] == {ssid: {"password": password}}
    files = {entry["path"]: entry["content"] for entry in data["write_files"]}
    assert files["/etc/kiosk-tailscale-authkey"] == key
    assert key not in files["/usr/local/bin/kiosk-tailscale"]
    assert ["/usr/local/bin/kiosk-tailscale"] in data["runcmd"]


@pytest.mark.parametrize("target", ["pi", "wyse"])
@pytest.mark.parametrize("option", ["--hostname", "--username", "--wifi-country"])
def test_rejects_invalid_system_names(
    tmp_path: Path, public_key: Path, option: str, target: str
) -> None:
    result = generate(
        tmp_path,
        "--ssh-key",
        str(public_key),
        option,
        "bad\ninjected",
        target=target,
    )
    assert result.returncode != 0
    assert "invalid" in result.stderr


def test_target_options_do_not_cross(tmp_path: Path, public_key: Path) -> None:
    key = ["--ssh-key", str(public_key)]
    result = generate(tmp_path, *key, "--interface", "eno1", target="pi")
    assert result.returncode != 0
    assert "--interface is for --target wyse" in result.stderr
    result = generate(tmp_path, *key, "--device", "/dev/sdz", target="wyse")
    assert result.returncode != 0
    assert "--device is for --target pi" in result.stderr


def test_wyse_installs_and_seeds_the_same_session(
    tmp_path: Path, public_key: Path
) -> None:
    out = output_dir(
        generate(
            tmp_path,
            "--ssh-key",
            str(public_key),
            "--hostname",
            "lobby-1",
            target="wyse",
        )
    )
    data = yaml.safe_load((out / "user-data").read_text())
    pi = config(generate(tmp_path, "--ssh-key", str(public_key), target="pi"))
    session = {
        entry["path"]: entry["content"]
        for entry in data["write_files"]
        if "/labwc/" in entry["path"] or "/systemd/user/" in entry["path"]
    }
    assert session == {
        entry["path"]: entry["content"]
        for entry in pi["write_files"]
        if "/labwc/" in entry["path"] or "/systemd/user/" in entry["path"]
    }
    assert not any("raspi-config" in command for command in data["runcmd"])
    assert {"gpio", "i2c", "spi"}.isdisjoint(data["users"][0]["groups"])
    assert data["phone_home"]["url"] == "http://192.168.242.1:8242/phone-home"
    files = {entry["path"]: entry["content"] for entry in data["write_files"]}
    assert 'user = "kiosk"' in files["/etc/greetd/config.toml"]
    assert yaml.safe_load((out / "meta-data").read_text())["local-hostname"] == (
        "lobby-1"
    )

    preseed = (out / "preseed.cfg").read_text()
    grub = (out / "grub.cfg").read_text()
    for text in (preseed, grub):
        body = "\n".join(line for line in text.splitlines() if not line.startswith("#"))
        assert "@" not in body
    assert "d-i netcfg/get_hostname string lobby-1" in preseed
    assert "http://192.168.242.1:8242/late.sh" in preseed
    assert "chromium" in preseed and "cloud-init" in preseed
    assert "url=http://192.168.242.1:8242/preseed.cfg hostname=lobby-1 " in grub


def test_wyse_psk_wifi_is_an_unpinned_keyfile(tmp_path: Path, public_key: Path) -> None:
    ssid = 'Home "Net"'
    password = "pass word\\ $(id)"
    result = generate(
        tmp_path,
        "--ssh-key",
        str(public_key),
        "--wifi-ssid",
        ssid,
        "--wifi-password",
        password,
        target="wyse",
    )
    data = config(result)
    assert not (output_dir(result) / "network-config").exists()
    files = {entry["path"]: entry for entry in data["write_files"]}
    profile = files["/etc/NetworkManager/system-connections/kiosk-wifi.nmconnection"]
    assert profile["permissions"] == "0600"
    assert "interface-name" not in profile["content"]
    assert "psk=pass\\sword\\\\\\s$(id)\n" in profile["content"]


if __name__ == "__main__":
    sys.exit(pytest.main([__file__, "-v", "-n", "auto"]))
