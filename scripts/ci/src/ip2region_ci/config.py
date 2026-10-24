import tomllib
from collections.abc import Iterator, Mapping
from dataclasses import dataclass
from enum import StrEnum
from pathlib import Path, PurePosixPath
from typing import Literal, Self, cast, get_args

from pydantic import (
    BaseModel,
    ConfigDict,
    Field,
    ValidationError,
    model_validator,
)

from ip2region_ci.errors import CiError

Platform = Literal["linux", "macos", "windows"]
Architecture = Literal["x64", "arm64"]
Target = Literal[
    "linux-x64",
    "linux-arm64",
    "macos-x64",
    "macos-arm64",
    "windows-x64",
    "windows-arm64",
]
ToolchainName = Literal[
    "cangjie",
    "docker",
    "dotnet",
    "erlang",
    "go",
    "java",
    "lua",
    "native",
    "node",
    "php",
    "python",
    "rust",
]

CONFIG_NAME = "ci.toml"
TARGETS: frozenset[str] = frozenset(get_args(Target))

# Targets whose jobs may run on another runner of the same platform, which
# runs the binaries by emulation: Rosetta runs x64 code on Apple silicon.
EMULATORS: Mapping[str, str] = {"macos-x64": "macos-arm64"}


class Phase(StrEnum):
    LINT = "lint"
    TEST = "test"
    ACCEPT = "accept"


class _Model(BaseModel):
    model_config = ConfigDict(extra="forbid", frozen=True)


class Step(_Model):
    name: str = Field(min_length=1)
    run: tuple[str, ...] = Field(min_length=1)
    timeout: int = Field(default=600, ge=1, le=7200)
    env: Mapping[str, str] = Field(default_factory=dict)
    platforms: frozenset[Platform] | None = None
    working_directory: str | None = None
    fail_on_output: bool = False

    @model_validator(mode="after")
    def _working_directory_inside_component(self) -> Self:
        if self.working_directory is not None:
            path = PurePosixPath(self.working_directory)
            if path.is_absolute() or ".." in path.parts:
                msg = f"step {self.name!r} leaves the component directory"
                raise ValueError(msg)
        return self

    def applies_to(self, platform: Platform) -> bool:
        return self.platforms is None or platform in self.platforms


class TargetSettings(_Model):
    host: Target | None = None
    env: Mapping[str, str] = Field(default_factory=dict)


class Toolchain(_Model):
    name: ToolchainName
    version: str = ""


class Component(_Model):
    toolchain: Toolchain
    platforms: frozenset[Platform] = Field(min_length=1)
    architectures: frozenset[Architecture] = frozenset(get_args(Architecture))
    exclude: frozenset[Target] = frozenset()
    target: Mapping[Target, TargetSettings] = Field(default_factory=dict)
    depends_on: tuple[str, ...] = ()
    lint: tuple[Step, ...] = ()
    test: tuple[Step, ...] = Field(min_length=1)
    accept: tuple[Step, ...] = ()

    @model_validator(mode="after")
    def _consistent_targets(self) -> Self:
        if not self.exclude < self._all_targets():
            msg = "exclude must list some, not all, of the component's targets"
            raise ValueError(msg)
        for name, settings in self.target.items():
            if name not in self.targets:
                msg = f"target {name!r} is not one of the component's targets"
                raise ValueError(msg)
            if settings.host not in (None, EMULATORS.get(name)):
                msg = f"a {settings.host} runner cannot run {name} jobs"
                raise ValueError(msg)
        for phase in Phase:
            for step in self.steps(phase):
                if step.platforms and not step.platforms <= self.platforms:
                    msg = f"step {step.name!r} targets an unsupported platform"
                    raise ValueError(msg)
        return self

    def steps(self, phase: Phase) -> tuple[Step, ...]:
        return getattr(self, phase.value)

    @property
    def targets(self) -> tuple[str, ...]:
        return tuple(sorted(self._all_targets() - self.exclude))

    def host(self, target: str) -> str:
        """Returns the target whose runner runs the jobs of `target`."""
        return self._settings(target).host or target

    def env(self, target: str) -> Mapping[str, str]:
        """Returns the environment that every step of `target` gets."""
        return self._settings(target).env

    def _settings(self, target: str) -> TargetSettings:
        return self.target.get(cast(Target, target), TargetSettings())

    def _all_targets(self) -> frozenset[str]:
        return frozenset(
            f"{platform}-{architecture}"
            for platform in self.platforms
            for architecture in self.architectures
        )


class Settings(_Model):
    component_roots: tuple[str, ...] = Field(min_length=1)
    global_paths: tuple[str, ...] = ()
    runners: Mapping[Target, str]
    skip: Mapping[str, str] = Field(default_factory=dict)

    @model_validator(mode="after")
    def _runner_for_every_target(self) -> Self:
        if set(self.runners) != TARGETS:
            msg = f"runners must map exactly {sorted(TARGETS)}"
            raise ValueError(msg)
        return self


@dataclass(frozen=True)
class Catalog:
    root: Path
    settings: Settings
    components: Mapping[str, Component]

    def component(self, path: str) -> Component:
        try:
            return self.components[path]
        except KeyError:
            raise CiError(f"unknown component: {path}") from None


def repository_root() -> Path:
    return Path(__file__).resolve().parents[4]


def load_catalog(root: Path) -> Catalog:
    """Loads every component config and checks that none is missing."""
    errors: list[str] = []
    settings = _load(root / "scripts" / "ci" / CONFIG_NAME, Settings)

    components: dict[str, Component] = {}
    for directory in _component_directories(root, settings):
        path = directory.relative_to(root).as_posix()
        config = directory / CONFIG_NAME
        if path in settings.skip:
            if config.exists():
                errors.append(f"{path}: skipped component has a {CONFIG_NAME}")
        elif not config.is_file():
            errors.append(f"{path}: missing {CONFIG_NAME}")
        else:
            try:
                components[path] = _load(config, Component)
            except CiError as error:
                errors.append(str(error))

    errors.extend(
        f"{path}: skip entry is not a component directory"
        for path in settings.skip
        if not (root / path).is_dir()
    )
    errors.extend(
        f"{path}: depends_on path does not exist: {dependency}"
        for path, component in components.items()
        for dependency in component.depends_on
        if not (root / dependency).exists()
    )
    if errors:
        raise CiError("\n".join(errors))
    return Catalog(root, settings, dict(sorted(components.items())))


def _component_directories(root: Path, settings: Settings) -> Iterator[Path]:
    for component_root in settings.component_roots:
        children = (root / component_root).iterdir()
        yield from sorted(path for path in children if path.is_dir())


def _load[M: BaseModel](path: Path, model: type[M]) -> M:
    try:
        with path.open("rb") as file:
            return model.model_validate(tomllib.load(file))
    except (OSError, tomllib.TOMLDecodeError, ValidationError) as error:
        raise CiError(f"{path}: {error}") from error
