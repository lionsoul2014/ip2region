import hashlib
import io
import tarfile
import zipfile
from collections.abc import Callable
from pathlib import Path

import pytest

from ip2region_ci import cangjie
from ip2region_ci.errors import CiError

ENVSETUP = b'export PATH="$PWD/bin:$PATH"\n'


def sdk(archive: Path) -> cangjie.Sdk:
    content = archive.read_bytes()
    return cangjie.Sdk(
        file_name=archive.name,
        object_key="test",
        sha256=hashlib.sha256(content).hexdigest(),
        size=len(content),
    )


def tar_sdk(directory: Path) -> cangjie.Sdk:
    archive = directory / "cangjie.tar.gz"
    with tarfile.open(archive, "w:gz") as file:
        member = tarfile.TarInfo("cangjie/envsetup.sh")
        member.size = len(ENVSETUP)
        file.addfile(member, io.BytesIO(ENVSETUP))
    return sdk(archive)


def zip_sdk(directory: Path) -> cangjie.Sdk:
    archive = directory / "cangjie.zip"
    with zipfile.ZipFile(archive, "w") as file:
        file.writestr("cangjie/envsetup.sh", ENVSETUP)
        file.writestr("cangjie/runtime/envsetup.sh", ENVSETUP)
    return sdk(archive)


@pytest.fixture(autouse=True)
def local_downloads(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(
        cangjie.Sdk,
        "url",
        property(lambda sdk: (tmp_path / sdk.file_name).as_uri()),
    )


@pytest.mark.parametrize("archive", [tar_sdk, zip_sdk])
def test_installs_and_reuses_sdk(
    tmp_path: Path, archive: Callable[[Path], cangjie.Sdk]
) -> None:
    package = archive(tmp_path)
    destination = tmp_path / "installed"

    envsetup = cangjie.install(destination, package)

    assert envsetup == destination / "cangjie" / "envsetup.sh"
    assert cangjie.install(destination, package) == envsetup


def test_rejects_digest_mismatch(tmp_path: Path) -> None:
    package = tar_sdk(tmp_path)
    tampered = cangjie.Sdk(package.file_name, "test", "0" * 64, package.size)

    with pytest.raises(CiError, match="SHA-256 mismatch"):
        cangjie.install(tmp_path / "installed", tampered)
    assert not (tmp_path / "installed").exists()


def test_pins_every_official_build() -> None:
    assert set(cangjie.SDKS) == {
        "linux-x64",
        "linux-arm64",
        "macos-arm64",
        "windows-x64",
    }
    assert all(len(sdk.sha256) == 64 for sdk in cangjie.SDKS.values())
