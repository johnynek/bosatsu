# Issue #2474 acceptance validation status

This report preserves the validation evidence from the Linux execution so a later validator can finish the acceptance checks without repeating successful work. The implementation under test was at `87bf26dde4bedc80279501604cfdcb19082331f6`; the working tree also contained the two test expectation fixes and the temporary tracker status change listed at the end.

## Successful checks

### Coverage and JVM/CLI tests

```text
sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"
```

Passed: core 2,193 passed, 0 failed, 2 ignored; CLI 167 passed, 0 failed, 1 skipped. Changed paths were fully covered: `EmitCCommand.renderMain` 14/14 statements and 100% branches; `ClangGen.renderMain` 6/6 statements and 100% branches; its shared main renderer 17/17 statements; `LocalRuntime` 165/168 statements and 96.55% branches; `LocalRuntimeManifest` 90/91 statements and 100% branches. Overall core coverage was 85.16% statements and 81.09% branches.

### Assembled CLI and native C emission

Built the CLI assembly with `sbt "cli/assembly"`, then ran the assembled-jar fixture with an explicit jar and compiler configuration:

```text
python3 scripts/test_emit_c.py --jar cli/target/scala-3.8.2/bosatsu-cli-assembly-0.0.69+27-87bf26dd+20261112-0558-SNAPSHOT.jar --cc-conf .bosatsuc/a8bbfdc70a64b35564646d25e6663a1c8bd75386/cc_conf.json
```

Passed. Existing test-emission cases remained green. Both program-emission fixtures emitted from compiled bundles alone in clean directories with an empty `PATH`; the C was linked separately and each program exited with status 7.

`./test_cli.sh` could not run under its `/bin/sh` interpreter because this host's Dash does not provide the `time` command. Running the same script body with Bash succeeded:

```text
bash ./test_cli.sh
```

Passed: 6,118 tests.

### C runtime and Valgrind

```text
./scripts/test_c_valgrind.sh
```

Passed with zero Valgrind errors/leaks across the C runtime, generated C suites, Issue1642 fixture, FibBench (10 -> 89), and ProgAssoc fixture. Before this run, sanitized objects and the archive were removed from both the source runtime directory and the installed runtime cache; without that cleanup, the Valgrind link initially picked up the prior sanitizer archive.

### Python and Node codegen checks

Python generated-code tests passed: 78 tests.

```text
./bosatsuj tool transpile --input_dir test_workspace/ --input test_workspace/Bosatsu/IO/Error.bosatsu --input test_workspace/Bosatsu/Collection/Array.bosatsu --input test_workspace/Bosatsu/IO/Core.bosatsu --input test_workspace/Bosatsu/IO/Bytes.bosatsu --input test_workspace/Bosatsu/IO/Std.bosatsu python --outdir pyout --externals test_workspace/Prog.bosatsu_externals --evaluators test_workspace/Prog.bosatsu_eval
PYTHONPATH=$PYTHONPATH:$(pwd)/test_workspace:$(pwd)/pyout python3 -m unittest discover pyout -v --pattern "*.py"
```

```text
sbt "cliJSJS/fullOptJS"
./bosatsu_node tool test ...
./bosatsu_node fetch
./bosatsu_node test --outdir node_out_lib
./bosatsu_node test
./bosatsu_node build --main_pack Bosatsu/FibBench --exe_out fib_bench_node --cc_flag=-O0 --cc_lib=-lm
./fib_bench_node 10
```

Passed. Node tool tests reported 6,118 passing; generated-library and ordinary test runs each reported 6,159 passing; FibBench printed `fib(10) = 89`. Library/tool evaluation printed `create_mode_test:ok`.

### Scala.js codegen checks

```text
sbt "coreJS/testOnly * -- --log=failure; jsapiJS/testOnly * -- --log=failure"
sbt "jsuiJS/testOnly * -- --log=failure"
NODE_OPTIONS=--openssl-legacy-provider sbt "jsuiJS/fullOptJS::webpack"
```

All commands exited 0. The coreJS/jsapiJS command passed 2 tests; jsuiJS passed 8 tests. The webpack task completed in 5:04; sbt printed a stats-parser error for webpack warnings lacking `moduleName`, but returned success.

## Remaining required evidence

The sanitizer run did not complete successfully on this host:

```text
./scripts/test_c_sanitizers.sh
```

It reached the generated `issue1642_repro` executable, then repeatedly printed `AddressSanitizer:DEADLYSIGNAL` before producing test output. A GDB launch backtrace for the same executable showed the fault before `main`, during dynamic-loader resolution of `pthread_getspecific` from `__asan::AsanInitInternal` / allocator initialization. The host uses GCC 12.2's `libasan8` and glibc 2.36. This is evidence of an AddressSanitizer runtime/startup failure in this Linux environment, not a passing test; rerun the unchanged sanitizer script on a compatible host.

No macOS result was obtained: the execution host is Linux x86_64. The acceptance step requires matching-platform macOS native evidence before completion, so this report does not certify that check.

## Working-tree changes made during validation

- `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala`: narrowed the runner-call ordering assertion so the newly emitted ABI prototype does not match the call-site search.
- `cli/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala`: updated the generated-C hash for the intentional runner-prototype output.
- `docs/design/hermetic-bazel-rules.md`: kept tracker item 0i incomplete while required acceptance evidence remains outstanding.

After the remaining checks pass and their results are transferred to the code-plan completion notes, this temporary handoff report should be removed before publication.
