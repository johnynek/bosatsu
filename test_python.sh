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

./bosatsuj tool transpile \
  --input_dir test_workspace \
  --input test_workspace/Bosatsu/IO/Error.bosatsu \
  --input test_workspace/Bosatsu/IO/Core.bosatsu \
  --input test_workspace/Bosatsu/IO/Std.bosatsu \
  --package_root test_workspace/ \
  python --outdir "$tmp_dir" \
  --externals test_workspace/Prog.bosatsu_externals \
  --evaluators test_workspace/Prog.bosatsu_eval

PYTHONPATH="$tmp_dir:$PWD/test_workspace" python3 -c "import Bosatsu.LsExample as LsExample; import ProgExt; ProgExt.run(LsExample.main)" > /dev/null
