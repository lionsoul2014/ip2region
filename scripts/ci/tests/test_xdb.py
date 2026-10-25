from pathlib import Path

import pytest

from ip2region_ci.xdb import first_difference

HEADER = bytes(range(16))


@pytest.fixture
def expected(tmp_path: Path) -> Path:
    path = tmp_path / "expected.xdb"
    path.write_bytes(HEADER + b"data")
    return path


def write(tmp_path: Path, content: bytes) -> Path:
    path = tmp_path / "actual.xdb"
    path.write_bytes(content)
    return path


def test_ignores_creation_time(tmp_path: Path, expected: Path) -> None:
    actual = HEADER[:4] + b"\xff" * 4 + HEADER[8:] + b"data"

    assert first_difference(expected, write(tmp_path, actual)) is None


def test_reports_first_difference(tmp_path: Path, expected: Path) -> None:
    actual = HEADER[:8] + b"\xff" + HEADER[9:] + b"data"

    assert first_difference(expected, write(tmp_path, actual)) == 8


def test_reports_truncation(tmp_path: Path, expected: Path) -> None:
    actual = HEADER + b"da"

    assert first_difference(expected, write(tmp_path, actual)) == 18
