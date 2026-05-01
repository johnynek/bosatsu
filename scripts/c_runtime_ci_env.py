#!/usr/bin/env python3
import argparse
import json
import pathlib
import shlex
import sys


def export_line(name: str, value: str) -> str:
    return f"export {name}={shlex.quote(value)}"


def fail(message: str) -> None:
    raise SystemExit(f"c_runtime_ci_env.py: {message}")


def contains_archive(flags: list[str], archive_name: str) -> bool:
    return any(pathlib.PurePath(flag).name == archive_name for flag in flags)


def has_system_link_flag(flags: list[str]) -> bool:
    return any(
        flag == "-pthread"
        or flag == "-framework"
        or flag.startswith("-l")
        or flag.startswith("-Wl,")
        for flag in flags
    )


def require_flag(flag_name: str, flag: str, flags: list[str]) -> None:
    if flag not in flags:
        fail(f"missing required {flag_name} flag {flag!r}: {flags!r}")


def validate_vendored_metadata(
    *,
    conf: dict,
    iflags: list[str],
    libs: list[str],
    required_cflags: list[str],
) -> None:
    conf_flags = conf["flags"]
    conf_libs = conf["libs"]

    require_flag("compile", "-DGC_THREADS", conf_flags)
    require_flag("generated/runtime compile", "-DGC_THREADS", iflags + conf_flags)

    for flag in required_cflags:
        require_flag("compile", flag, conf_flags)

    if not contains_archive(conf_libs, "bosatsu_platform.a"):
        fail(f"installed cc_conf libs do not contain bosatsu_platform.a: {conf_libs!r}")
    if not contains_archive(conf_libs, "libuv.a"):
        fail(f"installed cc_conf libs do not contain vendored libuv.a: {conf_libs!r}")
    if not contains_archive(conf_libs, "libgc.a"):
        fail(f"installed cc_conf libs do not contain vendored libgc.a: {conf_libs!r}")
    if not contains_archive(libs, "libuv.a"):
        fail(f"exported C_RUNTIME_LIBS lost vendored libuv.a: {libs!r}")
    if not contains_archive(libs, "libgc.a"):
        fail(f"exported C_RUNTIME_LIBS lost vendored libgc.a: {libs!r}")
    if contains_archive(libs, "bosatsu_platform.a"):
        fail(f"exported C_RUNTIME_LIBS must not contain bosatsu_platform.a: {libs!r}")
    if "-lm" in libs:
        fail(f"exported C_RUNTIME_LIBS must not contain the runtime self -lm: {libs!r}")
    if not has_system_link_flag(libs):
        fail(
            "exported C_RUNTIME_LIBS did not preserve any vendored transitive "
            f"system link flags: {libs!r}"
        )


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
    parser.add_argument(
        "--validate-vendored-libuv",
        action="store_true",
        help=(
            "Fail unless installed metadata contains vendored libuv/bdwgc archives, "
            "GC_THREADS, and preserved transitive system link flags."
        ),
    )
    parser.add_argument(
        "--require-cflag",
        action="append",
        default=[],
        help="Additional cc_conf compile flag that must be present.",
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
        if flag != runtime_platform_lib
        and pathlib.PurePath(flag).name != "bosatsu_platform.a"
        and flag != "-lm"
    ]

    if args.validate_vendored_libuv:
        validate_vendored_metadata(
            conf=conf,
            iflags=iflags,
            libs=libs,
            required_cflags=args.require_cflag,
        )

    print(export_line("C_RUNTIME_CC", conf["cc_path"]))
    print(export_line("C_RUNTIME_CPPFLAGS", " ".join(iflags)))
    print(export_line("C_RUNTIME_LIBS", " ".join(libs)))
    print(export_line("C_RUNTIME_CC_CONF", str(conf_path)))


if __name__ == "__main__":
    try:
        main()
    except KeyError as err:
        print(f"c_runtime_ci_env.py: missing cc_conf key {err}", file=sys.stderr)
        raise SystemExit(1)
