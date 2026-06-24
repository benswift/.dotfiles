"""Tests for clipboard operations."""

import base64
from unittest.mock import MagicMock, mock_open, patch


from mail_utils.clipboard import copy_to_clipboard


class TestCopyToClipboard:
    def test_writes_osc52_to_tty(self):
        m = mock_open()
        with patch("builtins.open", m), patch.dict("os.environ", {}, clear=True):
            result = copy_to_clipboard("test text")
        assert result is True
        m.assert_called_once_with("/dev/tty", "w")
        written = m().write.call_args[0][0]
        b64 = base64.b64encode(b"test text").decode("ascii")
        assert written == f"\033]52;c;{b64}\a"

    def test_wraps_sequence_for_tmux(self):
        m = mock_open()
        with (
            patch("builtins.open", m),
            patch.dict("os.environ", {"TMUX": "x"}, clear=True),
        ):
            copy_to_clipboard("hi")
        written = m().write.call_args[0][0]
        assert written.startswith("\033Ptmux;\033\033]52;c;")
        assert written.endswith("\033\\")

    def test_falls_back_to_pbcopy_without_tty(self):
        with (
            patch("builtins.open", side_effect=OSError),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.return_value = MagicMock(returncode=0)
            result = copy_to_clipboard("test text")
            assert result is True
            assert mock_run.call_args[0][0] == ["pbcopy"]
            assert mock_run.call_args[1]["input"] == "test text"
            assert mock_run.call_args[1]["text"] is True

    def test_falls_back_to_xclip(self):
        with (
            patch("builtins.open", side_effect=OSError),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.side_effect = [
                FileNotFoundError(),
                MagicMock(returncode=0),
            ]
            result = copy_to_clipboard("test text")
            assert result is True
            assert mock_run.call_count == 2
            assert mock_run.call_args_list[1][0][0] == [
                "xclip",
                "-selection",
                "clipboard",
            ]

    def test_falls_back_to_wl_copy(self):
        with (
            patch("builtins.open", side_effect=OSError),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.side_effect = [
                FileNotFoundError(),
                FileNotFoundError(),
                MagicMock(returncode=0),
            ]
            result = copy_to_clipboard("test text")
            assert result is True
            assert mock_run.call_count == 3
            assert mock_run.call_args_list[2][0][0] == ["wl-copy"]

    def test_returns_false_when_all_fail(self):
        with (
            patch("builtins.open", side_effect=OSError),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.side_effect = FileNotFoundError()
            result = copy_to_clipboard("test text")
            assert result is False
            assert mock_run.call_count == 3
