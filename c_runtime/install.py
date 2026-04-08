import json
import os
import shlex
import shutil
import subprocess
import sys
from argparse import ArgumentParser


def find_git_directory(start_path):
    current_path = start_path
    while current_path != os.path.dirname(current_path):
        if os.path.exists(os.path.join(current_path, ".git")):
            return current_path
        current_path = os.path.dirname(current_path)
    raise FileNotFoundError("No .git entry found in current or parent directories.")


def detect_os_type():
    uname = subprocess.check_output(["uname"], text=True).strip()
    if uname == "Darwin":
        return "macos"
    if uname == "Linux":
        return "linux"
    return "unknown"


def split_flags(value):
    if value is None:
        return []
    stripped = value.strip()
    if not stripped:
        return []
    return shlex.split(stripped)


def unique_flags(parts):
    out = []
    seen = set()
    for part in parts:
        if not part:
            continue
        if part in seen:
            continue
        seen.add(part)
        out.append(part)
    return out


def resolve_cc_path(cc):
    # Keep supporting absolute paths while allowing simple executable names.
    if os.path.isabs(cc):
        return cc
    resolved = shutil.which(cc)
    return resolved if resolved else cc


def split_cppflags(parts):
    flags = []
    iflags = []
    takes_value = {"-I", "-isystem", "-iquote", "-F"}

    idx = 0
    while idx < len(parts):
        part = parts[idx]
        if part in takes_value:
            iflags.append(part)
            if idx + 1 < len(parts):
                iflags.append(parts[idx + 1])
                idx += 2
                continue
        if (
            part.startswith("-I")
            or part.startswith("-isystem")
            or part.startswith("-iquote")
            or part.startswith("-F")
        ):
            iflags.append(part)
        else:
            flags.append(part)
        idx += 1

    return flags, iflags


def write_cc_conf(
    rootdir,
    version,
    cc,
    cflags,
    cppflags,
    ldflags,
    libs,
    profile,
):
    os_type = detect_os_type()
    bosatsu_dir = os.path.join(rootdir, ".bosatsuc", version)

    profile_defaults = {
        "release": ["-O2"],
        "debug": ["-O0", "-g3"],
    }

    cflags_parts = split_flags(cflags)
    cppflags_parts = split_flags(cppflags)
    ldflags_parts = split_flags(ldflags)
    libs_parts = split_flags(libs)
    cpp_define_flags, cpp_include_flags = split_cppflags(cppflags_parts)

    flags = unique_flags(
        (
            profile_defaults[profile] if not cflags_parts else []
        )
        + cpp_define_flags
        + cflags_parts
    )
    iflags = unique_flags(
        [f"-I{os.path.join(bosatsu_dir, 'include')}"] + cpp_include_flags
    )
    link_libs = unique_flags(
        [f"{os.path.join(bosatsu_dir, 'lib', 'bosatsu_platform.a')}"]
        + ldflags_parts
        + libs_parts
        + ["-lm"]
    )

    config = {
        "cc_path": resolve_cc_path(cc),
        "flags": flags,
        "iflags": iflags,
        "libs": link_libs,
        "os": os_type,
    }

    path = os.path.join(bosatsu_dir, "cc_conf.json")
    with open(path, "w") as f:
        json.dump(config, f, indent=4)


def main():
    parser = ArgumentParser(description="Create a directory structure and copy files.")
    parser.add_argument("--include", action="append", required=True, help="Header files to include.")
    parser.add_argument("--lib", action="append", required=True, help="Library files to include.")
    parser.add_argument("--version", required=True, help="Version directory name.")
    parser.add_argument(
        "--root",
        help="Path to the repo root where .bosatsuc should be installed; defaults to locating a .git entry.",
    )
    parser.add_argument(
        "--cc",
        default=os.environ.get("CC", "cc"),
        help="Compiler executable recorded in cc_conf.json (default: $CC or cc).",
    )
    parser.add_argument(
        "--cflags",
        default=os.environ.get("CFLAGS", ""),
        help="CFLAGS forwarded into cc_conf.json.",
    )
    parser.add_argument(
        "--cppflags",
        default=os.environ.get("CPPFLAGS", ""),
        help="CPPFLAGS forwarded into cc_conf.json.",
    )
    parser.add_argument(
        "--ldflags",
        default=os.environ.get("LDFLAGS", ""),
        help="LDFLAGS forwarded into cc_conf.json.",
    )
    parser.add_argument(
        "--libs",
        default=os.environ.get("LIBS", ""),
        help="Additional linker libs/flags forwarded into cc_conf.json.",
    )
    parser.add_argument(
        "--pkg-config",
        dest="pkg_config",
        default=os.environ.get("PKG_CONFIG", "pkg-config"),
        help="pkg-config executable (default: $PKG_CONFIG or pkg-config).",
    )
    parser.add_argument(
        "--profile",
        choices=["release", "debug"],
        default="release",
        help="Default optimization profile when CFLAGS is empty.",
    )

    args = parser.parse_args()

    if args.root is not None:
        git_root = args.root
    else:
        try:
            git_root = find_git_directory(os.getcwd())
        except FileNotFoundError as e:
            print(str(e), file=sys.stderr)
            sys.exit(1)

    bosatsuc_dir = os.path.join(git_root, ".bosatsuc")
    os.makedirs(bosatsuc_dir, exist_ok=True)

    version_dir = os.path.join(bosatsuc_dir, args.version)
    if os.path.exists(version_dir):
        shutil.rmtree(version_dir)

    include_dir = os.path.join(version_dir, "include")
    lib_dir = os.path.join(version_dir, "lib")
    os.makedirs(include_dir)
    os.makedirs(lib_dir)

    write_cc_conf(
        git_root,
        args.version,
        args.cc,
        args.cflags,
        args.cppflags,
        args.ldflags,
        args.libs,
        args.profile,
    )

    for include_file in args.include:
        if not os.path.isfile(include_file):
            print(f"Include file not found: {include_file}", file=sys.stderr)
            sys.exit(1)
        shutil.copy(include_file, include_dir)

    for lib_file in args.lib:
        if not os.path.isfile(lib_file):
            print(f"Library file not found: {lib_file}", file=sys.stderr)
            sys.exit(1)
        shutil.copy(lib_file, lib_dir)

    print(version_dir)


if __name__ == "__main__":
    main()
