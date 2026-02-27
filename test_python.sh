#!/bin/sh
set -eu

tmp_dir=$(mktemp -d -t bosatsupy-XXXXXXXXXX)
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

check_ls_output() {
  output="$1"
  printf '%s\n' "$output" | grep -Eq '^listing \.$'
  printf '%s\n' "$output" | grep -Eq '^\[(file|dir|symlink|other|missing)\] '
  printf '%s\n' "$output" | grep -Eq 'test_workspace'
  summary_line=$(printf '%s\n' "$output" | sed -n 's/^listed \([0-9][0-9]*\) entries in \([0-9][0-9]*\) ns$/\1 \2/p')
  [ -n "$summary_line" ]
  reported_count=${summary_line%% *}
  listed_count=$(printf '%s\n' "$output" | grep -Ec '^\[(file|dir|symlink|other|missing)\] ')
  [ "$reported_count" -eq "$listed_count" ]
}

check_create_mode_output() {
  output="$1"
  printf '%s\n' "$output" | grep -Eq '^create_mode_test:ok$'
}

./bosatsuj tool transpile \
  --input test_workspace/Issue1633.bosatsu \
  --package_root test_workspace/ \
  python --outdir "$tmp_dir"

python3 -m unittest discover "$tmp_dir" -v --pattern "*.py"

./bosatsuj tool transpile \
  --input_dir test_workspace \
  --input test_workspace/Bosatsu/IO/Error.bosatsu \
  --input test_workspace/Bosatsu/Collection/Array.bosatsu \
  --input test_workspace/Bosatsu/IO/Core.bosatsu \
  --input test_workspace/Bosatsu/IO/Bytes.bosatsu \
  --input test_workspace/Bosatsu/IO/Std.bosatsu \
  --input test_workspace/Bosatsu/IO/CreateModeMain.bosatsu \
  --package_root test_workspace/ \
  python --outdir "$tmp_dir" \
  --externals test_workspace/Prog.bosatsu_externals \
  --evaluators test_workspace/Prog.bosatsu_eval

ls_output=$(PYTHONPATH="$tmp_dir:$PWD/test_workspace" python3 -c "import Bosatsu.LsExample as LsExample; import ProgExt; ProgExt.run(LsExample.main)")
check_ls_output "$ls_output"

create_mode_output=$(PYTHONPATH="$tmp_dir:$PWD/test_workspace" python3 -c "import Bosatsu.IO.CreateModeMain as CreateModeMain; import ProgExt; ProgExt.run(CreateModeMain.main)")
check_create_mode_output "$create_mode_output"
