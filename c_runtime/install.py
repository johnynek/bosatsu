import os
import sys
import shutil
from argparse import ArgumentParser

def find_git_directory(start_path):
    current_path = start_path
    while current_path != os.path.dirname(current_path):  # Stop when reaching the root
        if os.path.isdir(os.path.join(current_path, '.git')):
            return current_path
        current_path = os.path.dirname(current_path)
    raise FileNotFoundError("No .git directory found in current or parent directories.")

def main():
    parser = ArgumentParser(description="Create a directory structure and copy files.")
    parser.add_argument('--include', action='append', required=True, help="Header files to include.")
    parser.add_argument('--lib', action='append', required=True, help="Library files to include.")
    parser.add_argument('--version', required=True, help="Version directory name.")

    args = parser.parse_args()

    # Find the .git directory
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
