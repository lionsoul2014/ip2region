from pathlib import Path

import pytest
from conftest import Writer

from ip2region_ci.config import Phase, load_catalog, repository_root
from ip2region_ci.errors import CiError


def test_repository_components_are_valid() -> None:
    catalog = load_catalog(repository_root())

    assert "binding/golang" in catalog.components
    assert all(component.test for component in catalog.components.values())


def test_loads_components_in_path_order(repo: Path) -> None:
    catalog = load_catalog(repo)

    assert list(catalog.components) == ["binding/c", "binding/golang"]
    golang = catalog.component("binding/golang")
    assert golang.targets == (
        "linux-arm64",
        "linux-x64",
        "windows-arm64",
        "windows-x64",
    )
    assert [step.name for step in golang.steps(Phase.LINT)] == ["Vet"]


def test_reports_every_missing_config(repo: Path, write: Writer) -> None:
    write("binding/rust/src/lib.rs", "")
    write("maker/rust/Cargo.toml", "")

    with pytest.raises(CiError) as error:
        load_catalog(repo)

    assert str(error.value).splitlines() == [
        "binding/rust: missing ci.toml",
        "maker/rust: missing ci.toml",
    ]


def test_rejects_config_in_skipped_directory(repo: Path, write: Writer) -> None:
    write("maker/c/ci.toml", "")

    with pytest.raises(CiError, match=r"skipped component has a ci\.toml"):
        load_catalog(repo)


def test_rejects_unknown_keys(repo: Path, write: Writer) -> None:
    write("binding/golang/ci.toml", 'toolchain = { name = "go" }\nos = []\n')

    with pytest.raises(CiError, match=r"binding/golang/ci\.toml"):
        load_catalog(repo)


def test_rejects_step_platform_outside_component(
    repo: Path, write: Writer
) -> None:
    write(
        "binding/golang/ci.toml",
        """
        toolchain = { name = "go" }
        platforms = ["linux"]

        [[test]]
        name = "Unit tests"
        run = ["go", "test"]
        platforms = ["macos"]
        """,
    )

    with pytest.raises(CiError, match="unsupported platform"):
        load_catalog(repo)


@pytest.mark.parametrize(
    ("setting", "message"),
    [
        ('exclude = ["linux-x64", "linux-arm64"]', "some, not all"),
        ('exclude = ["macos-x64"]', "some, not all"),
    ],
)
def test_rejects_bad_exclude(
    repo: Path, write: Writer, setting: str, message: str
) -> None:
    write(
        "binding/golang/ci.toml",
        f"""
        toolchain = {{ name = "go" }}
        platforms = ["linux"]
        {setting}

        [[test]]
        name = "Unit tests"
        run = ["go", "test"]
        """,
    )

    with pytest.raises(CiError, match=message):
        load_catalog(repo)


def test_target_settings(repo: Path, write: Writer) -> None:
    write(
        "binding/golang/ci.toml",
        """
        toolchain = { name = "go" }
        platforms = ["macos"]

        [target.macos-x64]
        host = "macos-arm64"
        env = { GOARCH = "amd64" }

        [target.macos-arm64]
        env = { GOFLAGS = "-ldflags=-B=gobuildid" }

        [[test]]
        name = "Unit tests"
        run = ["go", "test"]
        """,
    )

    golang = load_catalog(repo).component("binding/golang")

    assert golang.host("macos-x64") == "macos-arm64"
    assert golang.host("macos-arm64") == "macos-arm64"
    assert golang.env("macos-x64") == {"GOARCH": "amd64"}
    assert golang.env("macos-arm64") == {"GOFLAGS": "-ldflags=-B=gobuildid"}


@pytest.mark.parametrize(
    ("target", "host", "error"),
    [
        ("linux-x64", None, "not one of the component's targets"),
        ("macos-arm64", "macos-x64", "cannot run macos-arm64 jobs"),
    ],
)
def test_rejects_bad_target_settings(
    repo: Path, write: Writer, target: str, host: str | None, error: str
) -> None:
    host_line = f'host = "{host}"' if host else ""
    write(
        "binding/golang/ci.toml",
        f"""
        toolchain = {{ name = "go" }}
        platforms = ["macos"]

        [target.{target}]
        {host_line}
        env = {{ GOARCH = "amd64" }}

        [[test]]
        name = "Unit tests"
        run = ["go", "test"]
        """,
    )

    with pytest.raises(CiError, match=error):
        load_catalog(repo)


@pytest.mark.parametrize("directory", ["../c", "/opt/sdk", "a/../.."])
def test_rejects_working_directory_outside_component(
    repo: Path, write: Writer, directory: str
) -> None:
    write(
        "binding/golang/ci.toml",
        f"""
        toolchain = {{ name = "go" }}
        platforms = ["linux"]

        [[test]]
        name = "Unit tests"
        run = ["go", "test"]
        working_directory = "{directory}"
        """,
    )

    with pytest.raises(CiError, match="leaves the component directory"):
        load_catalog(repo)


def test_rejects_missing_dependency(repo: Path, write: Writer) -> None:
    config = repo / "binding/golang/ci.toml"
    write(
        "binding/golang/ci.toml",
        f'depends_on = ["binding/gone"]\n{config.read_text()}',
    )

    with pytest.raises(CiError, match="depends_on path does not exist"):
        load_catalog(repo)


def test_unknown_component(repo: Path) -> None:
    with pytest.raises(CiError, match="unknown component: binding/nope"):
        load_catalog(repo).component("binding/nope")
