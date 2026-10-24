import re
import tomllib
from pathlib import Path
from typing import get_args

import pytest
from ruamel.yaml import YAML

from ip2region_ci.config import ToolchainName, repository_root

GITHUB = repository_root() / ".github"
WORKFLOWS = sorted((GITHUB / "workflows").glob("*.yml"))
ACTIONS = sorted((GITHUB / "actions").glob("*/action.yml"))
PINNED = re.compile(r"^[\w.-]+/[\w./-]+@[0-9a-f]{40} # \S+$")


def load(path: Path) -> dict[str, object]:
    return YAML(typ="safe").load(path)


def uses(node: object) -> list[str]:
    match node:
        case {"uses": str(action), **rest}:
            return [action, *uses(rest)]
        case dict():
            return [found for value in node.values() for found in uses(value)]
        case list():
            return [found for item in node for found in uses(item)]
        case _:
            return []


@pytest.mark.parametrize("path", WORKFLOWS, ids=lambda path: path.name)
def test_workflow_defaults(path: Path) -> None:
    workflow = load(path)

    assert workflow["permissions"] == {"contents": "read"}
    assert workflow["concurrency"]["cancel-in-progress"] is True
    assert all("timeout-minutes" in job for job in workflow["jobs"].values())


@pytest.mark.parametrize("path", WORKFLOWS + ACTIONS, ids=str)
def test_third_party_actions_are_pinned(path: Path) -> None:
    lines = path.read_text().splitlines()
    references = [
        line.split("uses:", 1)[1].strip() for line in lines if "uses:" in line
    ]
    remote = [ref for ref in references if not ref.startswith("./")]

    assert remote
    assert [ref for ref in remote if not PINNED.match(ref)] == []


def test_setup_handles_every_toolchain() -> None:
    steps = load(GITHUB / "actions" / "setup" / "action.yml")["runs"]["steps"]
    conditions = " ".join(str(step.get("if", "")) for step in steps)
    without_setup = {"docker", "native", "python"}

    for name in set(get_args(ToolchainName)) - without_setup:
        assert f"inputs.toolchain == '{name}'" in conditions


def test_lock_uses_only_pypi() -> None:
    lock_path = repository_root() / "scripts" / "ci" / "uv.lock"
    with lock_path.open("rb") as file:
        lock = tomllib.load(file)

    registries = {
        package["source"]["registry"]
        for package in lock["package"]
        if "registry" in package["source"]
    }
    assert registries == {"https://pypi.org/simple"}
