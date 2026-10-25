from typer.testing import CliRunner

from ip2region_ci.cli import app
from ip2region_ci.config import TARGETS
from ip2region_ci.run import current_target

runner = CliRunner()


def test_validate() -> None:
    result = runner.invoke(app, ["validate"])

    assert result.exit_code == 0, result.output
    assert "components are valid" in result.stdout


def test_plan_only() -> None:
    result = runner.invoke(app, ["plan", "--only", "binding/golang"])

    assert result.exit_code == 0, result.output
    lint, test, data = result.stdout.splitlines()
    assert lint.startswith("lint=[")
    assert test.startswith("test=[")
    assert '"component": "binding/golang"' in test
    assert data.startswith('data={"component": "maker/golang"')


def test_check_runner() -> None:
    host = current_target()
    other = next(target for target in sorted(TARGETS) if target != host)

    native = runner.invoke(app, ["check-runner", "binding/c", host])
    foreign = runner.invoke(app, ["check-runner", "binding/c", other])

    assert native.exit_code == 0, native.output
    assert f"runs {host} natively" in native.stdout
    assert f"{other} jobs need a {other} runner" in str(foreign.exception)
