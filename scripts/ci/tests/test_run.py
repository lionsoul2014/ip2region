import sys
from pathlib import Path

import pytest

from ip2region_ci.config import Component, Phase, Step
from ip2region_ci.errors import CiError
from ip2region_ci.run import current_platform, run_component, run_step

PYTHON = sys.executable


def python(code: str, **fields: object) -> Step:
    return Step(name="python", run=(PYTHON, "-c", code), **fields)


def test_runs_steps_of_requested_phases_in_order(tmp_path: Path) -> None:
    def append(text: str) -> str:
        return f"open('log', 'a').write('{text}')"

    component = Component(
        toolchain={"name": "python"},
        platforms=frozenset({"linux", "macos", "windows"}),
        lint=(python(append("lint")),),
        test=(
            python(append("test")),
            python(append("never"), platforms={"linux"}),
        ),
        accept=(python(append("accept")),),
    )

    phases = (Phase.TEST, Phase.ACCEPT)
    run_component(component, tmp_path, phases, "windows-arm64")

    assert (tmp_path / "log").read_text() == "testaccept"


def test_target_environment(tmp_path: Path) -> None:
    component = Component(
        toolchain={"name": "go"},
        platforms=frozenset({"macos"}),
        target={
            "macos-x64": {"host": "macos-arm64", "env": {"GOARCH": "amd64"}}
        },
        test=(
            python("import os; open('arch', 'w').write(os.environ['GOARCH'])"),
        ),
    )

    run_component(component, tmp_path, (Phase.TEST,), "macos-x64")

    assert (tmp_path / "arch").read_text() == "amd64"


@pytest.mark.parametrize("target", ["macos-arm64", "linux-arm64"])
def test_rejects_unsupported_target(tmp_path: Path, target: str) -> None:
    component = Component(
        toolchain={"name": "python"},
        platforms=frozenset({"linux"}),
        exclude=frozenset({"linux-arm64"}),
        test=(python("pass"),),
    )

    with pytest.raises(CiError, match=f"not supported on {target}"):
        run_component(component, tmp_path, (Phase.TEST,), target)


def test_working_directory(tmp_path: Path) -> None:
    (tmp_path / "example").mkdir()
    step = python("open('out', 'w').write('ok')", working_directory="example")

    run_step(step, tmp_path)

    assert (tmp_path / "example" / "out").read_text() == "ok"


def test_step_environment(tmp_path: Path) -> None:
    step = python(
        "import os; assert os.environ['ANSWER'] == '42'", env={"ANSWER": "42"}
    )

    run_step(step, tmp_path)


def test_failing_step(tmp_path: Path) -> None:
    with pytest.raises(CiError, match="exited with 3"):
        run_step(python("raise SystemExit(3)"), tmp_path)


def test_timeout(tmp_path: Path) -> None:
    step = python("import time; time.sleep(5)", timeout=1)

    with pytest.raises(CiError, match="timed out after 1s"):
        run_step(step, tmp_path)


def test_fail_on_output(
    tmp_path: Path, capfd: pytest.CaptureFixture[str]
) -> None:
    run_step(python("", fail_on_output=True), tmp_path)

    with pytest.raises(CiError, match="reported findings"):
        run_step(python("print('a.go')", fail_on_output=True), tmp_path)
    assert "a.go" in capfd.readouterr().out


def test_resolves_relative_executable(tmp_path: Path) -> None:
    if current_platform() == "windows":
        pytest.skip("shebang scripts are POSIX only")
    script = tmp_path / "bin" / "hello"
    script.parent.mkdir()
    script.write_text(f"#!{PYTHON}\nopen('out', 'w').write('hi')\n")
    script.chmod(0o755)

    run_step(Step(name="hello", run=("./bin/hello",)), tmp_path)

    assert (tmp_path / "out").read_text() == "hi"


def test_missing_executable(tmp_path: Path) -> None:
    step = Step(name="missing", run=("no-such-program",))

    with pytest.raises(CiError, match="executable not found: no-such-program"):
        run_step(step, tmp_path)
