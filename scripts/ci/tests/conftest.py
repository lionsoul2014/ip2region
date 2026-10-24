from collections.abc import Callable
from pathlib import Path
from textwrap import dedent

import pytest

SETTINGS = """
component_roots = ["binding", "maker"]
global_paths = ["data", "scripts/ci"]

[runners]
linux-x64 = "ubuntu-x64"
linux-arm64 = "ubuntu-arm64"
macos-x64 = "macos-x64"
macos-arm64 = "macos-arm64"
windows-x64 = "windows-x64"
windows-arm64 = "windows-arm64"

[skip]
"maker/c" = "Holds documentation only."
"""

GO = """
toolchain = { name = "go" }
platforms = ["linux", "windows"]

[[lint]]
name = "Vet"
run = ["go", "vet", "./..."]

[[test]]
name = "Unit tests"
run = ["go", "test", "./..."]
"""

type Writer = Callable[[str, str], Path]


@pytest.fixture
def write(tmp_path: Path) -> Writer:
    def write(path: str, content: str) -> Path:
        file = tmp_path / path
        file.parent.mkdir(parents=True, exist_ok=True)
        file.write_text(dedent(content))
        return file

    return write


@pytest.fixture
def repo(tmp_path: Path, write: Writer) -> Path:
    write("scripts/ci/ci.toml", SETTINGS)
    write("binding/golang/ci.toml", GO)
    write("binding/c/ci.toml", GO.replace('"linux", "windows"', '"linux"'))
    write("maker/c/README.md", "")
    return tmp_path
