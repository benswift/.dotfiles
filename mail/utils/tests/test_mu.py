"""Tests for mu integration."""

from unittest.mock import MagicMock, patch


from mail_utils.mu import find_message_path


class TestFindMessagePath:
    def test_returns_path_when_found(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(
                returncode=0,
                stdout="/home/user/Maildir/INBOX/cur/1234567890.R123.host,U=1:2,S\n",
            )
            result = find_message_path("test-id@example.com")
            assert result == "/home/user/Maildir/INBOX/cur/1234567890.R123.host,U=1:2,S"

    def test_calls_mu_with_correct_args(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(returncode=0, stdout="/path/to/msg\n")
            find_message_path("my-message-id@domain.com")
            mock_run.assert_called_once()
            args = mock_run.call_args[0][0]
            assert args == ["mu", "find", "-f", "l", "msgid:my-message-id@domain.com"]

    def test_returns_none_when_not_found(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(returncode=1, stdout="")
            result = find_message_path("nonexistent@example.com")
            assert result is None

    def test_returns_none_for_empty_output(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(returncode=0, stdout="   \n")
            result = find_message_path("empty@example.com")
            assert result is None

    def test_returns_first_path_when_multiple(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(
                returncode=0,
                stdout="/path/first\n/path/second\n/path/third\n",
            )
            result = find_message_path("multi@example.com")
            assert result == "/path/first"
