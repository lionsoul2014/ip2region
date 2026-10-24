from pathlib import Path

# Header bytes holding the creation time, which differs on every build.
_CREATED_AT = slice(4, 8)


def first_difference(expected: Path, actual: Path) -> int | None:
    """Returns the first offset where two xdb files differ, ignoring the
    creation time, or None when they match."""
    left, right = _masked(expected), _masked(actual)
    if left == right:
        return None
    return next(
        (
            offset
            for offset, (a, b) in enumerate(zip(left, right, strict=False))
            if a != b
        ),
        min(len(left), len(right)),
    )


def _masked(path: Path) -> bytearray:
    data = bytearray(path.read_bytes())
    data[_CREATED_AT] = bytes(_CREATED_AT.stop - _CREATED_AT.start)
    return data
