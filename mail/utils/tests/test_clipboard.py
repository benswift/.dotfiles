"""Tests for clipboard operations."""

from unittest.mock import MagicMock, patch


from mail_utils.clipboard import copy_to_clipboard


class TestCopyToClipboard:
    def test_uses_pbcopy_on_macos(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(returncode=0)
            result = copy_to_clipboard("test text")
            assert result is True
            mock_run.assert_called_once()
            assert mock_run.call_args[0][0] == ["pbcopy"]

    def test_falls_back_to_xclip(self):
        with patch("subprocess.run") as mock_run:
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
        with patch("subprocess.run") as mock_run:
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
        with patch("subprocess.run") as mock_run:
            mock_run.side_effect = FileNotFoundError()
            result = copy_to_clipboard("test text")
            assert result is False
            assert mock_run.call_count == 3

    def test_passes_text_as_input(self):
        with patch("subprocess.run") as mock_run:
            mock_run.return_value = MagicMock(returncode=0)
            copy_to_clipboard("test content")
            mock_run.assert_called_once()
            assert mock_run.call_args[1]["input"] == "test content"
            assert mock_run.call_args[1]["text"] is True
