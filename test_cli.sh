#!/bin/sh
set -eu

check_lib_eval_output() {
  output="$1"
  printf '%s\n' "$output" | grep -Eq '^Main \{ run: <fn arity=1> \}: Bosatsu/Prog::Main$'
}

sbt cli/assembly
time ./bosatsuj tool test \
  --input_dir test_workspace \
  --input test_workspace/Bosatsu/IO/Error.bosatsu \
  --input test_workspace/Bosatsu/Collection/Array.bosatsu \
  --input test_workspace/Bosatsu/IO/Core.bosatsu \
  --input test_workspace/Bosatsu/IO/Bytes.bosatsu \
  --input test_workspace/Bosatsu/IO/Std.bosatsu \
  --package_root test_workspace

time ./bosatsuj lib fetch \
  --repo_root . \
  --name core_alpha

ls_output=$(./bosatsuj lib eval \
  --repo_root . \
  --name core_alpha \
  --main Bosatsu/LsExample \
  --color none)
check_lib_eval_output "$ls_output"
