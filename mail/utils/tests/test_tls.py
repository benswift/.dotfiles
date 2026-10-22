"""Sent copies authenticate the IMAP server before releasing credentials."""

import imaplib
import json
import runpy
import socket
import ssl
import subprocess
from concurrent.futures import ThreadPoolExecutor
from dataclasses import replace
from datetime import datetime, timedelta
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
from mail_utils.accounts import Account, SentAppend, get_account_config
from mail_utils.compose import append_to_sent, build_email


@pytest.fixture
def certificate(tmp_path: Path) -> tuple[Path, Path]:
    cert, key = tmp_path / "cert.pem", tmp_path / "key.pem"
    subprocess.run(
        [
            "openssl",
            "req",
            "-x509",
            "-newkey",
            "rsa:2048",
            "-nodes",
            "-keyout",
            str(key),
            "-out",
            str(cert),
            "-days",
            "1",
            "-subj",
            "/CN=localhost",
            "-addext",
            "subjectAltName=DNS:localhost",
        ],
        check=True,
        capture_output=True,
    )
    return cert, key


@pytest.mark.parametrize(
    "trust,host", [(False, "localhost"), (True, "127.0.0.1"), (True, "localhost")]
)
def test_sent_copy_tls(certificate: tuple[Path, Path], trust: bool, host: str) -> None:
    cert, key = certificate
    server_context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    server_context.load_cert_chain(cert, key)
    # A trusted test CA changes only trust roots, never verification flags.
    client_context = ssl.create_default_context(cafile=str(cert)) if trust else None
    observed: list[bytes] = []
    imap_client = imaplib.IMAP4_SSL
    config = replace(
        get_account_config(Account.personal),
        sent_append=SentAppend(host, "dummy@example.invalid", "dummy", "dummy"),
    )
    message = build_email(
        "sender@example.invalid", "to@example.invalid", "TLS test", "Test body"
    )

    with socket.socket() as listener, ThreadPoolExecutor(max_workers=1) as pool:
        listener.bind(("127.0.0.1", 0))
        listener.listen()
        listener.settimeout(5)
        port = listener.getsockname()[1]

        def serve() -> None:
            raw, _ = listener.accept()
            raw.settimeout(5)
            try:
                conn = server_context.wrap_socket(raw, server_side=True)
            except ssl.SSLError:
                raw.close()
                return
            with conn, conn.makefile("rwb") as stream:
                stream.write(b"* OK test server\r\n")
                stream.flush()
                while line := stream.readline():
                    tag, command, *_ = line.split()
                    observed.append(command)
                    if command == b"CAPABILITY":
                        stream.write(b"* CAPABILITY IMAP4rev1\r\n")
                    elif command == b"APPEND":
                        size = int(line.rsplit(b"{", 1)[1].rstrip(b"}\r\n"))
                        stream.write(b"+ send literal\r\n")
                        stream.flush()
                        observed.append(stream.read(size))
                        stream.readline()
                    elif command == b"LOGOUT":
                        stream.write(b"* BYE\r\n" + tag + b" OK logout\r\n")
                        stream.flush()
                        return
                    stream.write(tag + b" OK accepted\r\n")
                    stream.flush()

        def connect(hostname: str, **kwargs) -> imaplib.IMAP4_SSL:
            return imap_client(hostname, port, timeout=5, **kwargs)

        future = pool.submit(serve)
        with (
            patch("mail_utils.compose.get_account_config", return_value=config),
            patch(
                "mail_utils.compose.subprocess.run",
                return_value=subprocess.CompletedProcess([], 0, "dummy-secret"),
            ),
            patch("mail_utils.compose.imaplib.IMAP4_SSL", side_effect=connect),
        ):
            if trust:
                with patch(
                    "mail_utils.compose.ssl.create_default_context",
                    return_value=client_context,
                ):
                    if host == "localhost":
                        append_to_sent(message, Account.personal)
                    else:
                        with pytest.raises(ssl.SSLCertVerificationError):
                            append_to_sent(message, Account.personal)
            else:
                with pytest.raises(ssl.SSLCertVerificationError):
                    append_to_sent(message, Account.personal)
        future.result(timeout=5)

    if trust and host == "localhost":
        assert b"LOGIN" in observed
        assert b"APPEND" in observed
        assert any(b"Test body" in item for item in observed)
    else:
        assert observed == []


def test_oauth_diagnostics_require_verified_tls(tmp_path: Path, capsys) -> None:
    """The diagnostic login paths must enforce the same TLS guarantees."""
    tokenfile = tmp_path / "token"
    tokenfile.touch(mode=0o600)
    token = {
        "registration": "microsoft",
        "authflow": "devicecode",
        "email": "dummy@example.invalid",
        "client_id": "dummy-client",
        "client_secret": "",
        "access_token": "dummy-access-token",
        "access_token_expiration": (datetime.now() + timedelta(days=1)).isoformat(),
    }
    script = Path(__file__).parents[2] / "mutt_oauth2.py"
    imap, pop, smtp = MagicMock(), MagicMock(), MagicMock()
    with (
        patch("sys.argv", [str(script), "--test", str(tokenfile)]),
        patch(
            "subprocess.run",
            return_value=subprocess.CompletedProcess([], 0, json.dumps(token).encode()),
        ),
        patch("imaplib.IMAP4_SSL", imap),
        patch("poplib.POP3_SSL", pop),
        patch("smtplib.SMTP", smtp),
    ):
        runpy.run_path(str(script), run_name="__main__")

    contexts = (
        imap.call_args.kwargs["ssl_context"],
        pop.call_args.kwargs["context"],
        smtp.return_value.starttls.call_args.kwargs["context"],
    )
    for context in contexts:
        assert context.verify_mode == ssl.CERT_REQUIRED
        assert context.check_hostname
    assert imap.return_value.authenticate.called
    assert pop.return_value._shortcmd.called
    assert smtp.return_value.auth.called
    capsys.readouterr()
