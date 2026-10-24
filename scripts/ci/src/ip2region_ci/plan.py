import subprocess
from collections.abc import Iterable
from pathlib import PurePosixPath

from ip2region_ci import data
from ip2region_ci.config import Catalog
from ip2region_ci.errors import CiError

LINT_TARGET = "linux-x64"

type Job = dict[str, str]


def changed_files(catalog: Catalog, since: str) -> tuple[str, ...] | None:
    """Lists files changed after `since`, or None when every file counts."""
    if not since.strip("0"):
        return None
    command = ("git", "diff", "--name-only", f"{since}...HEAD")
    try:
        output = subprocess.run(
            command,
            cwd=catalog.root,
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    except subprocess.CalledProcessError as error:
        raise CiError(f"{' '.join(command)}: {error.stderr.strip()}") from error
    return tuple(output.splitlines())


def affected(catalog: Catalog, changed: Iterable[str] | None) -> list[str]:
    """Selects the components whose own or watched paths changed."""
    if changed is None:
        return list(catalog.components)
    files = [PurePosixPath(file) for file in changed]

    def touched(*paths: str) -> bool:
        return any(
            file.is_relative_to(path) for file in files for path in paths
        )

    if touched(*catalog.settings.global_paths):
        return list(catalog.components)
    return [
        path
        for path, component in catalog.components.items()
        if touched(path, *component.depends_on)
    ]


def lint_jobs(catalog: Catalog, paths: Iterable[str]) -> list[Job]:
    jobs = []
    for path in paths:
        component = catalog.component(path)
        if component.lint:
            targets = component.targets
            target = LINT_TARGET if LINT_TARGET in targets else targets[0]
            jobs.append(_job(catalog, path, target))
    return jobs


def test_jobs(catalog: Catalog, paths: Iterable[str]) -> list[Job]:
    return [
        _job(catalog, path, target)
        for path in paths
        for target in catalog.component(path).targets
    ]


def data_job(catalog: Catalog) -> Job:
    """Describes the job that rebuilds data/ before the tests run."""
    return _job(catalog, data.MAKER, LINT_TARGET)


def _job(catalog: Catalog, path: str, target: str) -> Job:
    component = catalog.component(path)
    return {
        "component": path,
        "target": target,
        "runner": catalog.settings.runners[component.host(target)],
        "toolchain": component.toolchain.name,
        "toolchain-version": component.toolchain.version,
    }
