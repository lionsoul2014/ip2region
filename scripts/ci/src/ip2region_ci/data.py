from pathlib import Path

from ip2region_ci import xdb
from ip2region_ci.config import Step
from ip2region_ci.errors import CiError
from ip2region_ci.run import run_step, warn

# The maker that rebuilds data/, with the --version value, source and xdb
# file of each dataset.
MAKER = "maker/golang"
DATASETS = (
    ("ipv4", "ipv4_source.txt", "ip2region_v4.xdb"),
    ("ipv6", "ipv6_source.txt", "ip2region_v6.xdb"),
)


def sync(root: Path, destination: Path) -> None:
    """Rebuilds every xdb file of data/ from its source into `destination`
    and warns about the committed files that differ from their rebuild."""
    destination.mkdir(parents=True, exist_ok=True)
    for version, source, name in DATASETS:
        rebuilt = destination / name
        rebuilt.unlink(missing_ok=True)
        step = Step(
            name=f"Rebuild {name} from {source}",
            run=(
                "go",
                "run",
                ".",
                "gen",
                f"--src={root / 'data' / source}",
                f"--dst={rebuilt}",
                f"--version={version}",
            ),
            timeout=1200,
        )
        run_step(step, root / MAKER)
        # The maker exits with 0 even when it fails to write the file.
        if not rebuilt.is_file():
            raise CiError(f"{MAKER} did not write {rebuilt}")
        if xdb.first_difference(root / "data" / name, rebuilt) is not None:
            warn(
                f"data/{name}",
                f"does not match data/{source}; the tests use a rebuilt copy",
            )
