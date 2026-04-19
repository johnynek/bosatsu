#!/usr/bin/env python3
import argparse
import json
import pathlib
import shlex


def export_line(name: str, value: str) -> str:
    return f"export {name}={shlex.quote(value)}"


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Emit shell exports for vendored c_runtime dependency flags."
    )
    parser.add_argument("--sha", required=True, help="Runtime install id / git sha.")
    parser.add_argument(
        "--repo-root",
        default=".",
        help="Repository root containing .bosatsuc (default: current directory).",
    )
    args = parser.parse_args()

    repo_root = pathlib.Path(args.repo_root).resolve()
    runtime_dir = repo_root / ".bosatsuc" / args.sha
    conf_path = runtime_dir / "cc_conf.json"
    conf = json.loads(conf_path.read_text())

    runtime_include = f"-I{runtime_dir / 'include'}"
    runtime_platform_lib = str(runtime_dir / "lib" / "bosatsu_platform.a")

    iflags = [flag for flag in conf["iflags"] if flag != runtime_include]
    libs = [
        flag
        for flag in conf["libs"]
        if flag != runtime_platform_lib and flag != "-lm"
    ]

    print(export_line("C_RUNTIME_CC", conf["cc_path"]))
    print(export_line("C_RUNTIME_CPPFLAGS", " ".join(iflags)))
    print(export_line("C_RUNTIME_LIBS", " ".join(libs)))
    print(export_line("C_RUNTIME_CC_CONF", str(conf_path)))


if __name__ == "__main__":
    main()
