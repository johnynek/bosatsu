import json
import os
import shutil
import subprocess
import sys
from argparse import ArgumentParser

def find_git_directory(start_path):
    current_path = start_path
    while current_path != os.path.dirname(current_path):  # Stop when reaching the root
        if os.path.isdir(os.path.join(current_path, '.git')):
            return current_path
        current_path = os.path.dirname(current_path)
    raise FileNotFoundError("No .git directory found in current or parent directories.")

def write_cc_conf(rootdir, version):
      # Detect operating system
      uname = subprocess.check_output(["uname"], text=True).strip()
      if uname == "Darwin":
          os_type = "macos"
      elif uname == "Linux":
          os_type = "linux"
      else:
          os_type = "unknown"

      # Determine Bosatsu directory
      bosatsu_dir = os.path.join(rootdir, ".bosatsuc", version)

      # Initialize flags, IFLAGS, and LIBS
      flags = ["-flto", "-O3"]  # Adjust flags as needed
      iflags = [f"-I{os.path.join(bosatsu_dir, 'include')}"]
      libs = [f"{os.path.join(bosatsu_dir, 'lib', 'bosatsu_platform.a')}"]

      # Add platform-specific settings
      if os_type == "macos":
          bdw_gc_dir = "/opt/homebrew/Cellar/bdw-gc"
          if os.path.isdir(bdw_gc_dir):
              # todo: better version sorting
              latest_version = sorted(os.listdir(bdw_gc_dir))[-1]
              boehm_gc = os.path.join(bdw_gc_dir, latest_version)
              iflags.append(f"-I{os.path.join(boehm_gc, 'include')}")
              libs.append(f"{os.path.join(boehm_gc, 'lib', 'libgc.a')}")
      elif os_type == "linux":
          libs.append("-lgc")
      libs.append("-lm")

      # Find GCC path
      cc_path = subprocess.check_output(["which", "gcc"], text=True).strip()

      # Create the JSON configuration
      config = {
          "cc_path": cc_path,
          "flags": flags,
          "iflags": iflags,
          "libs": libs,
          "os": os_type
      }

      # Write the configuration to the specified file
      path = os.path.join(bosatsu_dir, "cc_conf.json")
      with open(path, "w") as f:
          json.dump(config, f, indent=4)

def main():
    parser = ArgumentParser(description="Create a directory structure and copy files.")
    parser.add_argument('--include', action='append', required=True, help="Header files to include.")
    parser.add_argument('--lib', action='append', required=True, help="Library files to include.")
    parser.add_argument('--version', required=True, help="Version directory name.")
    parser.add_argument(
        '--root',
        help="Path to the repo root where .bosatsuc should be installed; defaults to locating .git."
    )

    args = parser.parse_args()

    # Find the .git directory
    if args.root is not None:
        git_root = args.root
    else:
        try:
            git_root = find_git_directory(os.getcwd())
        except FileNotFoundError as e:
            print(str(e), file=sys.stderr)
            sys.exit(1)

    # Create the .bosatsuc/ directory if it doesn't exist
    bosatsuc_dir = os.path.join(git_root, '.bosatsuc')
    os.makedirs(bosatsuc_dir, exist_ok=True)

    # Remove the existing version directory if it exists
    version_dir = os.path.join(bosatsuc_dir, args.version)
    if os.path.exists(version_dir):
        shutil.rmtree(version_dir)

    # Create the new version directory structure
    include_dir = os.path.join(version_dir, 'include')
    lib_dir = os.path.join(version_dir, 'lib')
    os.makedirs(include_dir)
    os.makedirs(lib_dir)

    write_cc_conf(git_root, args.version)
    # Copy the include files
    for include_file in args.include:
        if not os.path.isfile(include_file):
            print(f"Include file not found: {include_file}", file=sys.stderr)
            sys.exit(1)
        shutil.copy(include_file, include_dir)

    # Copy the library files
    for lib_file in args.lib:
        if not os.path.isfile(lib_file):
            print(f"Library file not found: {lib_file}", file=sys.stderr)
            sys.exit(1)
        shutil.copy(lib_file, lib_dir)

    # Print the path to the created directory
    print(version_dir)

if __name__ == '__main__':
    main()
