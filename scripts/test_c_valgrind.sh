#!/usr/bin/env bash
set -euo pipefail

CFLAGS_VAL='-O1 -g -fno-omit-frame-pointer'
VALGRIND_CC_FLAGS=(
  --cc_flag=-O1
  --cc_flag=-g
  --cc_flag=-fno-omit-frame-pointer
  --cc_lib=-lm
)
VALGRIND_OPTS=(
  --tool=memcheck
  --error-exitcode=101
  --undef-value-errors=no
  --leak-check=full
  --show-leak-kinds=definite,indirect
  --errors-for-leak-kinds=definite,indirect
  --num-callers=30
)

run_memcheck() {
  local label="$1"
  shift
  echo "::group::valgrind ${label}"
  valgrind "${VALGRIND_OPTS[@]}" "$@"
  echo '::endgroup::'
}

cd c_runtime
rm -f test_exe
make PROFILE=debug CFLAGS="$CFLAGS_VAL" && git diff --quiet || { git diff; false; }
make boehm_example PROFILE=release CFLAGS="$CFLAGS_VAL"
make install PROFILE=release CFLAGS="$CFLAGS_VAL" CPPFLAGS='-DBSTS_CI=1'
make bench_exe PROFILE=release CFLAGS="$CFLAGS_VAL"
run_memcheck 'c_runtime test_exe' ./test_exe
run_memcheck 'c_runtime boehm_example' ./boehm_example
run_memcheck 'c_runtime bench_exe' ./bench_exe 50000 | tee bench_ci_valgrind.txt
cd ..

export SHA
SHA=$(./bosatsuj version -g)
python3 - <<'PY'
import json
import os
import pathlib

sha = os.environ["SHA"]
conf_path = pathlib.Path(".bosatsuc") / sha / "cc_conf.json"
conf = json.loads(conf_path.read_text())
assert "-O1" in conf["flags"], conf
assert "-DBSTS_CI=1" in conf["flags"] or "-DBSTS_CI=1" in conf["iflags"], conf
print(f"validated {conf_path}")
PY

rm -rf c_out
./bosatsuj tool transpile --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu --package_root test_workspace/ c --outdir c_out --test --exe_out test_exe "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'generated c tests' ./c_out/test_exe

rm -rf issue1642_repro
mkdir issue1642_repro
cat > issue1642_repro/Foo.bosatsu <<'EOS'
package Foo
test = Assertion(True, "foo")
EOS
./bosatsuj tool transpile --input issue1642_repro/Foo.bosatsu --package_root issue1642_repro c --outdir issue1642_repro/out --test --exe_out test_exe "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'single assertion test' issue1642_repro/out/test_exe | tee issue1642_repro/output.txt
grep -Eq '^Foo: [0-9]+\.[0-9]{3}s$' issue1642_repro/output.txt
grep -Eq '^    passed: .*1' issue1642_repro/output.txt

./bosatsuj lib fetch
rm -rf c_out_lib
mkdir c_out_lib
./bosatsuj lib test --outdir c_out_lib "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'lib test outdir' ./c_out_lib/test

echo 'now test without a given outdir'
rm -f ./test ./test_exe
./bosatsuj lib test "${VALGRIND_CC_FLAGS[@]}"
if [ -x ./test ]; then
  run_memcheck 'lib test default outdir' ./test
elif [ -x ./test_exe ]; then
  run_memcheck 'lib test default outdir' ./test_exe
else
  echo 'no standalone binary produced for default-outdir lib test'
fi

rm -rf c_out_build
mkdir c_out_build
./bosatsuj lib build --outdir c_out_build --main_pack Bosatsu/FibBench --exe_out fib_bench "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'lib build fib_bench' ./c_out_build/fib_bench 10
rm -f ./fib_bench_no_outdir
./bosatsuj lib build --main_pack Bosatsu/FibBench --exe_out fib_bench_no_outdir "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'lib build fib_bench no outdir' ./fib_bench_no_outdir 10

rm -rf c_out_prog_assoc
./bosatsuj tool transpile --input test_workspace/ProgAssoc.bosatsu --input test_workspace/Loops.bosatsu --input test_workspace/Prog.bosatsu --package_root test_workspace/ c --outdir c_out_prog_assoc --test --filter ProgAssoc --exe_out test_exe "${VALGRIND_CC_FLAGS[@]}"
run_memcheck 'prog loop assoc test' ./c_out_prog_assoc/test_exe
