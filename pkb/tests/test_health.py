from __future__ import annotations

from datetime import datetime, timedelta
from pathlib import Path

import pytest
from pkb_tools import health

NOW = datetime(2026, 9, 7, 7, 0).astimezone()


def status(**overrides: object) -> health.UnitStatus:
    base: dict[str, object] = {
        "timer_active": True,
        "result": "success",
        "active_state": "inactive",
        "last_exit": NOW - timedelta(hours=2),
        "last_trigger": NOW - timedelta(hours=2),
    }
    base.update(overrides)
    return health.UnitStatus(**base)  # type: ignore[arg-type]


def test_healthy_unit_has_no_problems() -> None:
    assert health.evaluate_unit("job", timedelta(hours=26), status(), NOW) == []


def test_stale_unit_is_reported() -> None:
    problems = health.evaluate_unit("job", timedelta(hours=1), status(), NOW)
    assert [p.key for p in problems] == ["job:stale"]
    assert "2h ago" in problems[0].message


def test_inactive_timer_and_failed_result() -> None:
    problems = health.evaluate_unit(
        "job",
        timedelta(hours=26),
        status(timer_active=False, result="exit-code", active_state="failed"),
        NOW,
    )
    assert {p.key for p in problems} == {"job:timer", "job:failed"}


def test_running_unit_is_not_stale() -> None:
    problems = health.evaluate_unit(
        "job",
        timedelta(hours=1),
        status(active_state="activating", last_exit=NOW - timedelta(days=3)),
        NOW,
    )
    assert problems == []


def test_never_run() -> None:
    problems = health.evaluate_unit(
        "job", timedelta(hours=1), status(last_exit=None, last_trigger=None), NOW
    )
    assert [p.key for p in problems] == ["job:never"]


def test_parse_systemd_time() -> None:
    when = health.parse_systemd_time("Mon 2026-09-07 07:08:24 AEST")
    assert when is not None and (when.hour, when.minute) == (7, 8)
    assert health.parse_systemd_time("") is None


def test_ssh_expect_regex_and_empty() -> None:
    check = health.SshCheck(name="proxy", command="curl", expect="^[0-9]{3}$")
    assert health.evaluate_ssh("h", check, 0, "200\n") is None
    assert health.evaluate_ssh("h", check, 0, "curl: (7) refused") is not None
    empty = health.SshCheck(
        name="failed", command="systemctl", expect_empty=True, ignore=["logrotate"]
    )
    assert (
        health.evaluate_ssh("h", empty, 0, "logrotate.service loaded failed\n") is None
    )
    problem = health.evaluate_ssh(
        "h", empty, 0, "logrotate.service failed\nfoo.service failed\n"
    )
    assert problem is not None and "foo.service" in problem.message


def test_config_round_trip(tmp_path: Path) -> None:
    cfg = tmp_path / "h.toml"
    cfg.write_text(
        '[[unit]]\nname = "a"\nmax_age = "26h"\n'
        '[[ssh]]\nhost = "x"\n[[ssh.check]]\nname = "p"\ncommand = "true"\nexpect = "."\n'
    )
    config = health.load_config(cfg)
    assert config.units == {"a": timedelta(hours=26)}
    assert config.hosts["x"][0].name == "p"


def test_bad_duration() -> None:
    with pytest.raises(ValueError, match="bad duration"):
        health.parse_duration("26 hours")


def test_paging_only_for_new_keys(
    notebook: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    sent: list[str] = []
    monkeypatch.setattr(
        health.subprocess, "run", lambda *_a, **k: sent.append(str(k.get("input")))
    )
    health.page_new_problems([health.Problem("a:stale", "**a**: stale")])
    health.page_new_problems([health.Problem("a:stale", "**a**: stale")])
    health.page_new_problems(
        [health.Problem("a:stale", "x"), health.Problem("b:failed", "**b**: failed")]
    )
    assert len(sent) == 2 and "b: failed" in sent[1]


def test_render_all_ok_is_one_line() -> None:
    assert "\n" not in health.render([], 5, 3)
