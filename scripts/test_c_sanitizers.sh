#!/usr/bin/env bash
set -euo pipefail

export ASAN_OPTIONS='detect_leaks=0:halt_on_error=1:abort_on_error=1:check_initialization_order=1'
export UBSAN_OPTIONS='halt_on_error=1:print_stacktrace=1'

SANITIZER_CFLAGS='-O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined'
SANITIZER_LDFLAGS='-fsanitize=address,undefined'
SHA=$(./bosatsuj version -g)
RUNTIME_ARCHIVE="${RUNNER_TEMP:-/tmp}/bosatsu-c-runtime-${SHA}.tar.gz"

CC_FLAGS=(
  --cc_flag=-O1
  --cc_flag=-g
  --cc_flag=-fno-omit-frame-pointer
  --cc_flag=-fsanitize=address,undefined
  --cc_lib=-fsanitize=address,undefined
  --cc_lib=-lm
)

rm -f "$RUNTIME_ARCHIVE"
tar -czf "$RUNTIME_ARCHIVE" c_runtime
CFLAGS="$SANITIZER_CFLAGS" \
LDFLAGS="$SANITIZER_LDFLAGS" \
CPPFLAGS='-DBSTS_CI=1' \
./bosatsuj c-runtime install --repo_root . --archive "$RUNTIME_ARCHIVE" --git_sha "$SHA" --profile release
eval "$(python3 scripts/c_runtime_ci_env.py --sha "$SHA")"

cd c_runtime
rm -f test_exe
CC="$C_RUNTIME_CC" make PROFILE=debug VENDORED_DEPS=1 CPPFLAGS="$C_RUNTIME_CPPFLAGS" LIBS="$C_RUNTIME_LIBS" CFLAGS="$SANITIZER_CFLAGS" LDFLAGS="$SANITIZER_LDFLAGS" && git diff --quiet || { git diff; false; }
CC="$C_RUNTIME_CC" make boehm_example PROFILE=release VENDORED_DEPS=1 CPPFLAGS="$C_RUNTIME_CPPFLAGS" LIBS="$C_RUNTIME_LIBS" CFLAGS="$SANITIZER_CFLAGS" LDFLAGS="$SANITIZER_LDFLAGS"
./test_exe
./boehm_example
CC="$C_RUNTIME_CC" make bench_exe PROFILE=release VENDORED_DEPS=1 CPPFLAGS="$C_RUNTIME_CPPFLAGS" LIBS="$C_RUNTIME_LIBS" CFLAGS="$SANITIZER_CFLAGS" LDFLAGS="$SANITIZER_LDFLAGS"
./bench_exe 200000 | tee bench_ci_sanitizers.txt
cd ..

python3 - <<'PY'
import json
import os
import pathlib

sha = os.environ["SHA"]
conf_path = pathlib.Path(".bosatsuc") / sha / "cc_conf.json"
conf = json.loads(conf_path.read_text())
assert "-fsanitize=address,undefined" in conf["flags"], conf
assert "-DBSTS_CI=1" in conf["flags"] or "-DBSTS_CI=1" in conf["iflags"], conf
print(f"validated {conf_path}")
PY

rm -rf c_out
./bosatsuj tool transpile --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu c --outdir c_out --test --exe_out test_exe "${CC_FLAGS[@]}"
./c_out/test_exe

rm -rf issue1642_repro
mkdir issue1642_repro
cat > issue1642_repro/Foo.bosatsu <<'EOS'
package Foo
test = Assertion(True, "foo")
EOS
./bosatsuj tool transpile --input issue1642_repro/Foo.bosatsu c --outdir issue1642_repro/out --test --exe_out test_exe "${CC_FLAGS[@]}"
issue1642_repro/out/test_exe | tee issue1642_repro/output.txt
grep -Eq '^Foo: [0-9]+\.[0-9]{3}s$' issue1642_repro/output.txt
grep -Eq '^    passed: .*1' issue1642_repro/output.txt

./bosatsuj fetch
rm -rf c_out_lib
mkdir c_out_lib
./bosatsuj test --outdir c_out_lib "${CC_FLAGS[@]}"
./c_out_lib/test

echo 'now test without a given outdir'
rm -f ./test ./test_exe
./bosatsuj test "${CC_FLAGS[@]}"
if [ -x ./test ]; then
  ./test
elif [ -x ./test_exe ]; then
  ./test_exe
else
  echo 'no standalone binary produced for default-outdir test'
fi

rm -rf c_out_build
mkdir c_out_build
./bosatsuj build --outdir c_out_build --main_pack Bosatsu/FibBench --exe_out fib_bench "${CC_FLAGS[@]}"
./c_out_build/fib_bench 10
rm -f ./fib_bench_no_outdir
./bosatsuj build --main_pack Bosatsu/FibBench --exe_out fib_bench_no_outdir "${CC_FLAGS[@]}"
./fib_bench_no_outdir 10

rm -rf c_out_prog_assoc
./bosatsuj tool transpile --input test_workspace/ProgAssoc.bosatsu --input test_workspace/Loops.bosatsu --input test_workspace/Prog.bosatsu c --outdir c_out_prog_assoc --test --filter ProgAssoc --exe_out test_exe "${CC_FLAGS[@]}"
./c_out_prog_assoc/test_exe
