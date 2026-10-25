import hashlib
import shutil
import tarfile
import tempfile
import zipfile
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from urllib.request import Request, urlopen

from ip2region_ci.errors import CiError
from ip2region_ci.run import current_target

VERSION = "1.1.3"


@dataclass(frozen=True)
class Sdk:
    file_name: str
    object_key: str
    sha256: str
    size: int

    @property
    def url(self) -> str:
        return (
            "https://cangjie-lang.cn/v1/files/auth/downLoad?nsId=142267"
            f"&fileName={self.file_name}&objectKey={self.object_key}"
        )


# Official builds; there are none for macos-x64 and windows-arm64.
SDKS: Mapping[str, Sdk] = {
    "linux-x64": Sdk(
        file_name=f"cangjie-sdk-linux-x64-{VERSION}.tar.gz",
        object_key="6a19349d21f5a8178d6fd22b",
        sha256="2b68905afc466e665ae181595c63f96c18d75fd2c1fb6c6f0cb64e179c28d61a",
        size=403_288_797,
    ),
    "linux-arm64": Sdk(
        file_name=f"cangjie-sdk-linux-aarch64-{VERSION}.tar.gz",
        object_key="6a19350321f5a8178d6fd22c",
        sha256="0b0ce42ce2492f654b00029687229188c82751418cb34df7990efd43eaf9c41f",
        size=395_740_749,
    ),
    "macos-arm64": Sdk(
        file_name=f"cangjie-sdk-mac-aarch64-{VERSION}.tar.gz",
        object_key="6a19312721f5a8178d6fd225",
        sha256="7eff8d3b9119535cf3d05cb81526c5df391137dce908d744449f4edb99da3199",
        size=245_449_232,
    ),
    "windows-x64": Sdk(
        file_name=f"cangjie-sdk-windows-x64-{VERSION}.zip",
        object_key="6a192b2521f5a8178d6fd222",
        sha256="8bb65499c1b30271eaca422568bc9537677bd25debd8f5208e780a22c917da72",
        size=392_128_226,
    ),
}


def install(destination: Path, sdk: Sdk | None = None) -> Path:
    """Installs a verified SDK, or reuses a complete one, and returns the
    path of its top-level envsetup.sh."""
    if destination.is_dir():
        return _envsetup(destination)
    sdk = sdk or _host_sdk()
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=destination.parent) as scratch:
        archive = Path(scratch) / sdk.file_name
        extracted = Path(scratch) / "sdk"
        _download(sdk, archive)
        try:
            if archive.suffix == ".zip":
                with zipfile.ZipFile(archive) as file:
                    file.extractall(extracted)
            else:
                with tarfile.open(archive) as file:
                    file.extractall(extracted, filter="data")
        except (tarfile.TarError, zipfile.BadZipFile) as error:
            raise CiError(f"{sdk.file_name}: {error}") from error
        envsetup = _envsetup(extracted).relative_to(extracted)
        shutil.move(extracted, destination)
    return destination / envsetup


def _host_sdk() -> Sdk:
    target = current_target()
    try:
        return SDKS[target]
    except KeyError:
        raise CiError(f"no Cangjie SDK {VERSION} for {target}") from None


def _download(sdk: Sdk, destination: Path) -> None:
    request = Request(sdk.url, headers={"User-Agent": "ip2region-ci"})
    digest = hashlib.sha256()
    size = 0
    try:
        with (
            urlopen(request, timeout=60) as response,
            destination.open("wb") as out,
        ):
            while chunk := response.read(1 << 20):
                out.write(chunk)
                digest.update(chunk)
                size += len(chunk)
    except OSError as error:
        raise CiError(f"{sdk.file_name}: {error}") from error
    if (size, digest.hexdigest()) != (sdk.size, sdk.sha256):
        raise CiError(f"{sdk.file_name}: size or SHA-256 mismatch")


def _envsetup(directory: Path) -> Path:
    found = sorted(
        directory.rglob("envsetup.sh"), key=lambda path: len(path.parts)
    )
    top = [path for path in found if len(path.parts) == len(found[0].parts)]
    if len(top) != 1:
        raise CiError(f"{directory}: expected one top-level envsetup.sh")
    return top[0]
