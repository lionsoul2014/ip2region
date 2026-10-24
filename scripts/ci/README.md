# ip2region CI

`ip2region-ci` drives the GitHub Actions workflow in `.github/workflows/ci.yml`
and runs the same jobs locally. It needs [uv](https://docs.astral.sh/uv/).

```bash
cd scripts/ci
uv run ip2region-ci list                         # components and their targets
uv run ip2region-ci run binding/golang lint test accept
uv run ip2region-ci validate                     # check every ci.toml
uv run ruff format --check && uv run ruff check && uv run pytest
```

## Components

Every directory under `binding/` and `maker/` is a component and has a
`ci.toml`, unless `scripts/ci/ci.toml` lists it under `[skip]`:

```toml
toolchain = { name = "go" }            # set up by .github/actions/setup
platforms = ["linux", "macos", "windows"]
architectures = ["x64", "arm64"]       # optional, both by default
exclude = ["windows-arm64"]            # optional, targets to leave out
depends_on = ["binding/c"]             # optional, other paths that trigger it

[target.macos-x64]                     # optional, settings of one target
host = "macos-arm64"                   # optional, run on the arm64 runner
env = { GOARCH = "amd64" }             # optional, environment of every step

[[lint]]
name = "Check formatting"
run = ["gofmt", "-l", "."]
fail_on_output = true                  # optional, any stdout fails the step

[[test]]
name = "Unit tests"
run = ["go", "test", "./..."]
timeout = 900                          # optional, seconds, 600 by default
env = { GOFLAGS = "-count=1" }         # optional
platforms = ["linux"]                  # optional, a subset of the component's
working_directory = "example"          # optional, inside the component
```

Steps run in the component directory without a shell, in this order:

- `lint`: formatting and static analysis, on Linux x64 only.
- `test`: unit tests, on every target.
- `accept`: checks against the canonical files in `data/`, on every target.
  Bindings query every range of the source files, and makers must rebuild the
  committed xdb files byte for byte. `ip2region-ci xdb-diff expected actual`
  compares two xdb files and ignores their creation time.

A target with a `host` runs on the runner of that target, which emulates it:
the macOS arm64 runner runs `macos-x64` binaries under Rosetta.

A pull request runs the components whose directory or `depends_on` paths it
changes. Changes under `global_paths` run all of them.

Before the tests, the "Sync real data" job rebuilds `data/*.xdb` from
`data/*_source.txt` with `maker/golang`, and every test job replaces the
committed xdb files with that rebuild. Benches and maker checks so always
compare against the current sources. A committed xdb file that differs from
its rebuild gets a warning. `ip2region-ci sync-data <directory>` runs the
same rebuild locally.
