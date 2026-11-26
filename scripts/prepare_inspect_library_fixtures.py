#!/usr/bin/env python3
"""Stage offline library-inspection fixtures and the pinned legacy core."""
import argparse
import hashlib
import http.client
from pathlib import Path
import shutil
import sys
import time
import urllib.error
import urllib.request


ROOT = Path(__file__).resolve().parents[1]
SOURCE_FIXTURES = ROOT / "test_workspace/inspect_library"
LEGACY_URI = (
    "https://github.com/johnynek/bosatsu/releases/download/v0.0.69/"
    "core_alpha-v9.0.1.bosatsu_lib"
)
LEGACY_NAME = "core_alpha-v9.0.1.bosatsu_lib"
LEGACY_SIZE = 937080
LEGACY_SHA256 = "b6e7d87fcb786084dd90da490fe9514678fee7f6920fc17e0abdf28210a11dff"


DOWNLOAD_ATTEMPTS = 4
RETRY_DELAYS_SECONDS = (5, 15, 30)


def download_legacy_core() -> bytes:
    """Fetch the pinned legacy core, retrying transient network failures.

    A GitHub release download sometimes stalls past the read timeout. That
    used to fail the whole CI job, so timeouts, dropped connections and
    server errors are retried; a client error such as 404 is not.
    """
    request = urllib.request.Request(LEGACY_URI, headers={"User-Agent": "bosatsu-fixture-preparer"})
    for attempt in range(1, DOWNLOAD_ATTEMPTS + 1):
        try:
            with urllib.request.urlopen(request, timeout=60) as response:
                return response.read()
        except (OSError, http.client.HTTPException) as err:
            client_error = isinstance(err, urllib.error.HTTPError) and err.code < 500
            if client_error or attempt == DOWNLOAD_ATTEMPTS:
                raise
            delay = RETRY_DELAYS_SECONDS[attempt - 1]
            print(
                f"downloading {LEGACY_NAME} failed on attempt {attempt} of "
                f"{DOWNLOAD_ATTEMPTS} ({err!r}); retrying in {delay}s",
                file=sys.stderr,
            )
            time.sleep(delay)
    raise AssertionError("unreachable")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out-dir", type=Path, required=True)
    args = parser.parse_args()
    out_dir = args.out_dir.resolve()
    if out_dir == SOURCE_FIXTURES or SOURCE_FIXTURES in out_dir.parents:
        parser.error("--out-dir must not be inside the checked-in fixture directory")

    out_dir.mkdir(parents=True, exist_ok=True)
    shutil.copytree(SOURCE_FIXTURES, out_dir, dirs_exist_ok=True)

    legacy_bytes = download_legacy_core()
    actual_sha256 = hashlib.sha256(legacy_bytes).hexdigest()
    if len(legacy_bytes) != LEGACY_SIZE or actual_sha256 != LEGACY_SHA256:
        raise SystemExit(
            "pinned legacy core did not match the fixture pin: "
            f"size={len(legacy_bytes)} (expected {LEGACY_SIZE}), "
            f"sha256={actual_sha256} (expected {LEGACY_SHA256})"
        )

    (out_dir / LEGACY_NAME).write_bytes(legacy_bytes)
    print(
        f"prepared {len(list(out_dir.glob('*.bosatsu_lib')))} library fixtures in {out_dir}; "
        f"legacy core sha256={LEGACY_SHA256}"
    )


if __name__ == "__main__":
    main()
