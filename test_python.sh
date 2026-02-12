#!/bin/sh
set -eu

tmp_dir=$(mktemp -d -t bosatsupy-XXXXXXXXXX)
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

./bosatsuj tool transpile \
  --input test_workspace/Issue1633.bosatsu \
  --package_root test_workspace/ \
  python --outdir "$tmp_dir"

python3 -m unittest discover "$tmp_dir" -v --pattern "*.py"
