import json
from pathlib import Path
from typing import Annotated

import typer

from ip2region_ci import cangjie, data, plan, xdb
from ip2region_ci.config import Catalog, Phase, load_catalog, repository_root
from ip2region_ci.errors import CiError
from ip2region_ci.run import current_target, run_component

app = typer.Typer(no_args_is_help=True, pretty_exceptions_enable=False)


def main() -> None:
    try:
        app()
    except CiError as error:
        typer.secho(f"error: {error}", err=True, fg=typer.colors.RED)
        raise SystemExit(1) from error


def _catalog() -> Catalog:
    return load_catalog(repository_root())


@app.command()
def validate() -> None:
    """Checks that every component directory has a valid ci.toml."""
    typer.echo(f"{len(_catalog().components)} components are valid")


@app.command("list")
def list_components() -> None:
    """Prints each component with its toolchain and targets."""
    for path, component in _catalog().components.items():
        targets = " ".join(component.targets)
        typer.echo(f"{path}\t{component.toolchain.name}\t{targets}")


@app.command("plan")
def plan_jobs(
    since: Annotated[
        str, typer.Option(help="Run only components changed after this commit.")
    ] = "",
    only: Annotated[
        list[str] | None, typer.Option(help="Run these components.")
    ] = None,
) -> None:
    """Prints the job matrices and the data job as GitHub output lines."""
    catalog = _catalog()
    paths = only or plan.affected(catalog, plan.changed_files(catalog, since))
    typer.echo(f"lint={json.dumps(plan.lint_jobs(catalog, paths))}")
    typer.echo(f"test={json.dumps(plan.test_jobs(catalog, paths))}")
    typer.echo(f"data={json.dumps(plan.data_job(catalog))}")


@app.command()
def run(
    component: Annotated[str, typer.Argument(help="Path such as binding/c.")],
    phases: Annotated[list[Phase], typer.Argument()],
    target: Annotated[
        str | None,
        typer.Option(help="Target such as macos-x64. Defaults to the host."),
    ] = None,
) -> None:
    """Runs the steps of the given phases for one component."""
    catalog = _catalog()
    run_component(
        catalog.component(component),
        catalog.root / component,
        phases,
        target,
    )


@app.command("sync-data")
def sync_data(
    destination: Annotated[Path, typer.Argument(resolve_path=True)],
) -> None:
    """Rebuilds data/*.xdb from data/*_source.txt into a directory."""
    data.sync(repository_root(), destination)


@app.command("xdb-diff")
def xdb_diff(expected: Path, actual: Path) -> None:
    """Fails when two xdb files differ beyond their creation time."""
    offset = xdb.first_difference(expected, actual)
    if offset is not None:
        raise CiError(f"{actual} differs from {expected} at byte {offset}")
    typer.echo(f"{actual} matches {expected}")


@app.command("check-runner")
def check_runner(
    component: Annotated[str, typer.Argument(help="Path such as binding/c.")],
    target: Annotated[str, typer.Argument(help="Target such as macos-x64.")],
) -> None:
    """Fails unless the runner is the one that hosts the target's jobs."""
    host = _catalog().component(component).host(target)
    actual = current_target()
    if actual != host:
        raise CiError(f"{target} jobs need a {host} runner, got {actual}")
    how = "natively" if host == target else "by emulation"
    typer.echo(f"{actual} runner runs {target} {how}")


@app.command("install-cangjie")
def install_cangjie(
    destination: Annotated[Path, typer.Argument(resolve_path=True)],
) -> None:
    """Installs the pinned Cangjie SDK and prints its envsetup.sh path."""
    typer.echo(cangjie.install(destination))
