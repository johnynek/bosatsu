# Code Plan #2349

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2349` Introduce a libuv-owned Prog runtime loop skeleton
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `1`
- Completed steps: `2`
- Total steps: `3`

## Summary

Introduce a private libuv-owned execution skeleton for the C `Prog` runtime. `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` now create, drive, drain, and close an owned `uv_loop_t`, while preserving the existing public runner entry points and current synchronous behavior for `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and existing `Effect` functions. No Bosatsu language, library, CLI, or generated C public surface changes in this slice.

## Current State

The repository has the direct inputs for this slice: the libuv integration contract in `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md` and implemented vendored libuv dependency support. The non-vendored `c_runtime/Makefile` discovers libuv compile/link flags with `pkg-config` and existing-style Darwin/Linux fallbacks, while preserving Boehm GC and `-lm` behavior. The C `Prog` runtime in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` now includes a private `BSTS_Prog_Runtime` context with an embedded, default-independent `uv_loop_t`, start handle, current Prog argument, continuation stack, final result, completion flag, and runtime status. `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` retain their public signatures and execute the private stepper through `uv_run(loop, UV_RUN_DEFAULT)`. Existing `Effect(arg, f)` handling remains synchronous by calling `f(arg)` immediately and assigning the returned Prog back into the runtime context. Generated C in `ClangGen.scala` still calls the existing public runner functions after GC/static initialization.

## Problem

The runtime could not safely migrate IO effects to libuv until the `Prog` interpreter had a stable loop-owner abstraction and a resumable state shape. That skeleton now exists for the direct C runtime path, but generated main and ProgTest coverage still needs to be exercised explicitly before the final branch is shippable. The required gate also needs a clean rerun because this round's `scripts/test_basic.sh` invocation failed in an unrelated ScalaCheck property in `dev.bosatsu.rankn.TypeTest`, not in the changed C runtime code.

## Steps

1. [x] `step-1-link-libuv-runtime-surface` Wire libuv Into The C Runtime Build Surface

Updated the C runtime build surfaces needed by `bosatsu_ext_Bosatsu_l_Prog.c` to include and link libuv. The non-vendored `c_runtime/Makefile` now discovers libuv compile/link flags through `pkg-config`, with conservative Darwin Homebrew/MacPorts and Linux `-luv` fallbacks matching the existing Boehm GC fallback style. `bosatsu_ext_Bosatsu_l_Prog.c` includes `<uv.h>`, which makes the dependency visible at compile time without changing runtime behavior yet. Generated C entry point shape and public runner signatures remain unchanged; tests now assert that generated Prog test runners keep GC/static initialization before delegating through `bsts_test_run_prog`.

#### Invariants

- Generated C continues to call the existing public runner entry points; there is no new Bosatsu-visible runtime API, CLI flag, or codegen option.
- Vendored generated-program link flags continue to place concrete static archives before system link flags, so libuv is available through the dependency path from issue #2344.
- Direct `make -C c_runtime` builds compile `bosatsu_ext_Bosatsu_l_Prog.c` with libuv headers and link runtime tests with libuv libraries without dropping existing Boehm GC or `-lm` behavior.
- `GC_init()`, `init_statics()`, and `atexit(free_statics)` remain before the generated runner call.

#### Property Tests

- The existing vendored dependency property coverage from issue #2344 continues to cover deterministic static archive/system flag ordering for generated-program link flags; this round did not add a new property because the selected slice only exposed libuv to the direct Makefile and unchanged generated entry point surface.

#### Assertion Tests

- Extended `ClangGenLibraryDepsTest` so generated Prog test runners assert `GC_init()`, `init_statics()`, and `atexit(free_statics)` appear before the `bsts_test_run_prog` delegation.
- Ran `make -C c_runtime PROFILE=debug test_out`; it compiled `bosatsu_ext_Bosatsu_l_Prog.c` with the libuv include path and linked the direct runtime test binary with libuv, Boehm GC, and `-lm`.
- Started `sbt "coreJVM/testOnly dev.bosatsu.codegen.clang.ClangGenLibraryDepsTest"`, but the worker tool session did not return a result after an extended wait, so this focused Scala test is not counted as passed in that round.

#### Completion Notes

Changed `c_runtime/Makefile`, `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`, and `core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala`. The direct C runtime debug test target passed with libuv on the non-vendored path. The full required gate `scripts/test_basic.sh` was not run in that round and remains a final PR gate.

2. [x] `step-2-introduce-owned-loop-runtime-state` Refactor Prog Execution Around Owned Loop State

Refactored `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` around a private `BSTS_Prog_Runtime` context that owns one embedded `uv_loop_t` per public main or Prog test invocation. The context carries the current `Prog` argument, continuation stack, final `BSTS_Prog_Test_Result`, completion flag, runtime status, and a runtime-owned idle handle used to enter the stepper from libuv. The previous interpreter loop is now `bsts_prog_runtime_step`, which mutates the context and remains resumable for later async effect migration. Existing effects remain synchronous: `Effect(arg, f)` calls `call_fn1(f, arg)` immediately and stores the returned Prog back into the context. Runtime execution starts from `uv_idle_start`, is driven with `uv_run(loop, UV_RUN_DEFAULT)`, closes the runtime-owned start handle, drains/walks busy handles if needed, and closes the loop before returning to the unchanged public runners.

#### Invariants

- Each `run_main` and `run_test` invocation owns a fresh default-independent `uv_loop_t`; no invocation uses libuv's global default loop.
- The private runtime context is only defined in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`; `bosatsu_ext_Bosatsu_l_Prog.h` and generated C public runner entry points remain unchanged.
- For synchronous programs, `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and `Effect` preserve the previous final success/error result and exit-code behavior.
- `FlatMap` continuations run in program order, `Recover` skips successful values and handles raised errors as before, and raised errors skip pending flat-map frames until a recover frame or top-level completion.
- Repeated Prog tests in one process allocate, run, drain, and close independent loop state.
- Loop close failures are not ignored: `UV_EBUSY` triggers `uv_walk`, handle closure, and another `uv_run`; non-busy close failures are reported as runtime faults.

#### Property Tests

- No broad generated Prog property was added in this C-only step. The runtime keeps the existing association tests and adds a focused table of public-runner behavioral assertions for the continuation algebra most relevant to the loop-backed stepper: pure success, uncaught raise, flat-map after pure, flat-map after raise, recover after raise, and recover after pure.

#### Assertion Tests

- Added C runtime assertions that `bsts_Bosatsu_Prog_run_test` returns success for a pure Prog through the libuv path.
- Added C runtime assertions that uncaught raises are still reported as `is_error=true` for Prog tests and as exit code `1` for main runs.
- Added C runtime assertions that flat-map after pure, flat-map after raise, recover after raise, and recover after pure preserve current behavior through the new context stepper.
- Added a repeated-run C assertion that invokes the same Prog test twice in one process, proving each run completes through independently closed runtime-owned loop state.
- Ran `make -C c_runtime PROFILE=debug test_out`; it passed with the new loop-backed runner tests.
- Ran `scripts/test_basic.sh`; it failed after 2115 passing tests due to an unrelated ScalaCheck failure in `dev.bosatsu.rankn.TypeTest.we can substitute to get an instantiation` with seed `LZPuJ5sNVQlfhUDOnSTZgcPFXkW9_LoedTSYCz30HWG=`.

#### Completion Notes

Changed `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` and `c_runtime/test.c`. The C Prog runner now enters interpretation through an owned libuv loop while keeping synchronous effect semantics and public runner signatures unchanged. Focused direct C runtime coverage passes via `make -C c_runtime PROFILE=debug test_out`. The configured required gate was attempted but did not pass because of an unrelated ScalaCheck failure in `dev.bosatsu.rankn.TypeTest`; rerun or repair that gate before PR submission.

3. [ ] `step-3-verify-generated-prog-main-and-test-regressions` Exercise Generated Main And ProgTest Paths

Add or run focused generated C regression coverage proving existing Bosatsu `main` and `ProgTest` programs still compile, run, exit, and report results correctly after the runtime moves under libuv. Prefer existing test-workspace programs and C backend test harnesses where they already cover Prog association and generated test execution. Keep the slice behavior-neutral: do not migrate file IO, sleep, process, or other IO/Core effects to asynchronous libuv APIs in this issue. This remaining step should also rerun the configured required gate because the latest `scripts/test_basic.sh` attempt failed in unrelated ScalaCheck coverage, not in the C runtime path changed by step 2.

#### Invariants

- Generated main programs preserve their current observable exit-code behavior for successful integer results and uncaught top-level errors.
- Generated Prog tests preserve pass/fail reporting, including the current uncaught Prog error path in `bsts_test_run_prog`.
- Current synchronous IO/Core effect functions continue to be called exactly as synchronous `Effect` callbacks; their error variants and context strings are not changed in this slice.
- The final branch is shippable only after `scripts/test_basic.sh` passes within the configured 2400 second timeout, with focused C runtime/generated C checks run during development.

#### Property Tests

- Where the existing clang/codegen suite already property-checks generated output or execution, extend it with a small generated family of ProgTest compositions whose expected pass/fail result is independent of whether evaluation crosses a future suspension boundary.

#### Assertion Tests

- Run or add a generated C test for a successful `main` returning an integer exit code through `bsts_Bosatsu_Prog_run_main`.
- Run or add a generated C ProgTest success case and a ProgTest uncaught-error case through `bsts_test_run_prog`.
- Run `make -C c_runtime PROFILE=debug` or the repo's equivalent C runtime command with the required libuv flags available, so the direct runtime tests cover the new loop skeleton.
- Rerun `scripts/test_basic.sh` as the required merge gate; the most recent run failed in `dev.bosatsu.rankn.TypeTest.we can substitute to get an instantiation` with ScalaCheck seed `LZPuJ5sNVQlfhUDOnSTZgcPFXkW9_LoedTSYCz30HWG=`.
