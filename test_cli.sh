#!/usr/bin/env bash
set -euo pipefail

check_lib_eval_output() {
  output="$1"
  printf '%s\n' "$output" | grep -Eq '^Main \{ run: <fn arity=1> \}: Bosatsu/Prog::Main$'
}

sbt cli/assembly

jar_path="$(
  ls -1t "$(pwd)"/cli/target/scala-*/bosatsu-cli-assembly-*.jar 2>/dev/null | head -n 1 || true
)"
if [ -z "$jar_path" ]; then
  echo "test_cli.sh: no assembly jar found after cli/assembly" >&2
  exit 1
fi
python3 scripts/test_bazel_worker.py --jar "$jar_path"
./scripts/test_launcher_passthrough.sh
time ./bosatsuj tool test \
  --input_dir test_workspace \
  --input test_workspace/Bosatsu/IO/Error.bosatsu \
  --input test_workspace/Bosatsu/Collection/Array.bosatsu \
  --input test_workspace/Bosatsu/IO/Core.bosatsu \
  --input test_workspace/Bosatsu/IO/Bytes.bosatsu \
  --input test_workspace/Bosatsu/IO/Std.bosatsu

time ./bosatsuj fetch \
  --repo_root . \
  --name core_alpha

ls_output=$(./bosatsuj eval \
  --repo_root . \
  --name core_alpha \
  --main Bosatsu/LsExample \
  --color none)
check_lib_eval_output "$ls_output"
