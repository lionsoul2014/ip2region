import shutil
from itertools import islice
from pathlib import Path

import pytest

from ip2region_ci import data
from ip2region_ci.config import repository_root

pytestmark = pytest.mark.skipif(
    shutil.which("go") is None, reason="maker/golang needs Go"
)


def test_sync_rebuilds_every_xdb_and_warns_about_stale_ones(
    tmp_path: Path,
    capfd: pytest.CaptureFixture[str],
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.delenv("GITHUB_ACTIONS", raising=False)
    root = repository_root()
    (tmp_path / "maker").mkdir()
    (tmp_path / data.MAKER).symlink_to(root / data.MAKER)
    (tmp_path / "data").mkdir()
    for _, source, name in data.DATASETS:
        with (root / "data" / source).open(encoding="utf-8") as file:
            head = "".join(islice(file, 50))
        (tmp_path / "data" / source).write_text(head, encoding="utf-8")
        (tmp_path / "data" / name).write_bytes(b"")

    data.sync(tmp_path, tmp_path / "stale")
    for _, _, name in data.DATASETS:
        shutil.copy(tmp_path / "stale" / name, tmp_path / "data" / name)
    stale = capfd.readouterr().out
    data.sync(tmp_path, tmp_path / "current")
    current = capfd.readouterr().out

    assert stale.count("warning: data/") == 2
    assert "warning" not in current
