#!/usr/bin/env python3

import argparse
import os
import re
import subprocess
import sys
from typing import Optional

VERSION_RE = re.compile(r"^\d+\.\d+\.\d+(?:[+-].*)?$")
INFO_PREFIX = "[info]"


def extract_version(output: str) -> Optional[str]:
    lines = output.splitlines()

    for idx, line in enumerate(lines):
        if line.strip() == f"{INFO_PREFIX} ThisBuild / version":
            for next_line in lines[idx + 1 :]:
                candidate = next_line.strip()
                if not candidate:
                    continue
                if candidate.startswith(INFO_PREFIX):
                    candidate = candidate[len(INFO_PREFIX) :].strip()
                if VERSION_RE.match(candidate):
                    return candidate

    matches = []
    for line in lines:
        candidate = line.strip()
        if candidate.startswith(INFO_PREFIX):
            candidate = candidate[len(INFO_PREFIX) :].strip()
        if VERSION_RE.match(candidate):
            matches.append(candidate)

    if matches:
        return matches[-1]
    return None


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Verify that sbt-dynver matches the git tag."
    )
    parser.add_argument(
        "--tag",
        default=os.environ.get("TAG_NAME", ""),
        help="Tag name (defaults to TAG_NAME env var).",
    )
    parser.add_argument(
        "--sbt",
        default=os.environ.get("SBT_CMD", "sbt"),
        help="sbt command (defaults to sbt).",
    )
    parser.add_argument(
        "--print-version",
        action="store_true",
        help="Print the detected version and exit.",
    )
    args = parser.parse_args()

    tag = args.tag.strip()
    if not tag:
        sys.stderr.write("Missing tag name; pass --tag or set TAG_NAME.\n")
        return 1

    expected = tag[1:] if tag.startswith("v") else tag

    try:
        proc = subprocess.run(
            [args.sbt, "-batch", "-no-colors", "show ThisBuild / version"],
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        sys.stderr.write(f"sbt command not found: {args.sbt}\n")
        return 1
    output = (proc.stdout or "") + "\n" + (proc.stderr or "")

    if proc.returncode != 0:
        sys.stderr.write("sbt failed while reading version:\n")
        sys.stderr.write(output)
        return proc.returncode

    actual = extract_version(output)
    if not actual:
        sys.stderr.write("Failed to determine sbt version from output:\n")
        sys.stderr.write(output)
        return 1

    if args.print_version:
        print(actual)
        return 0

    actual_base = actual.split("+", 1)[0]
    if actual_base != expected:
        sys.stderr.write(
            f"Tag version mismatch: tag={expected}, sbt={actual}\n"
        )
        return 1

    print(f"sbt version matches tag: {actual}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
