# Code Plan #2349

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2349` Introduce a libuv-owned Prog runtime loop skeleton
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Introduce a private libuv-owned execution skeleton for the C `Prog` runtime. The final change should make `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` create, drive, drain, and close an owned `uv_loop_t`, while preserving the existing public runner entry points and current synchronous behavior for `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and all existing `Effect` functions. No Bosatsu language, library, CLI, or generated C public surface should change in this slice.

## Current State

The repository already has the direct inputs for this slice: the libuv integration contract in `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md` and implemented vendored libuv dependency support. The current C `Prog` runtime in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` is still a synchronous while-loop interpreter. `Effect(arg, f)` calls `f(arg)` immediately, and the private runner returns `BSTS_Prog_Test_Result` directly. `bsts_Bosatsu_Prog_run_main` builds the argv list and maps uncaught top-level errors to exit code `1`; `bsts_Bosatsu_Prog_run_test` builds an empty argument list and returns the private runner result. Generated C in `ClangGen.scala` already calls `GC_init()`, `init_statics()`, `atexit(free_statics)`, and then the existing runner function supplied by the codegen path. The non-vendored `c_runtime/Makefile` currently discovers Boehm GC flags but does not yet discover libuv flags for direct runtime builds.

## Problem

The runtime cannot safely migrate IO effects to libuv until the `Prog` interpreter has a stable loop-owner abstraction and a resumable state shape. Keeping the synchronous interpreter as a single stack-local loop would force later async work to layer suspension, GC rooting, completion state, and loop cleanup onto code that assumes immediate effect returns. This issue should establish the minimal root-cause-complete skeleton now: private runtime state, owned loop lifecycle, deterministic close/drain behavior, and regression tests proving existing generated mains and Prog tests still behave exactly as before.

## Steps

1. [ ] `step-1-link-libuv-runtime-surface` Wire libuv Into The C Runtime Build Surface

Update the C runtime build surfaces needed by `bosatsu_ext_Bosatsu_l_Prog.c` to include and link libuv. This should use the vendored dependency metadata path already implemented for generated C/backend builds, and add the smallest necessary non-vendored `c_runtime/Makefile` support for local direct builds by discovering libuv compile/link flags with `pkg-config` plus conservative platform fallbacks only if existing Makefile patterns justify them. Keep `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` signatures unchanged, and avoid changing generated C entry point shape except for tests that lock in the unchanged surface.

#### Invariants

- Generated C continues to call the existing public runner entry points; there is no new Bosatsu-visible runtime API, CLI flag, or codegen option.
- Vendored generated-program link flags continue to place concrete static archives before system link flags, so libuv is available through the dependency path from issue #2344.
- Direct `make -C c_runtime` builds compile `bosatsu_ext_Bosatsu_l_Prog.c` with libuv headers and link runtime tests with libuv libraries without dropping existing Boehm GC or `-lm` behavior.
- `GC_init()`, `init_statics()`, and `atexit(free_statics)` remain before the generated runner call.

#### Property Tests

- Where generated C tests already use randomized/package-shaped coverage, add or extend a property-style check that rendering main/test entry points for generated packages still delegates through the configured runner identifier without introducing a new public call convention.

#### Assertion Tests

- Add focused Scala assertions in the clang/codegen test area that generated main and test runners still contain the same initialization order and call the existing runner identifiers.
- Add or update C runtime build/test coverage so the direct runtime test binary links successfully with libuv in the non-vendored path when system libuv is available.
- Run the configured required gate `scripts/test_basic.sh` before the PR is considered submittable.

2. [ ] `step-2-introduce-owned-loop-runtime-state` Refactor Prog Execution Around Owned Loop State

Refactor `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` around a private runtime context that owns one `uv_loop_t` per main or Prog test invocation. The context should carry the current `Prog` argument, the continuation stack, final `BSTS_Prog_Test_Result`, a completion flag, and the loop pointer or embedded loop storage. Move the existing interpreter switch into a resumable step function that mutates this context. For this slice, all existing effects should remain synchronous by assigning the result of `call_fn1(effect_fn, effect_arg)` back into the context and continuing; no IO/Core effect should be migrated yet. Start execution from the owned loop using a minimal runtime-owned libuv handle or callback path, drive it with `uv_run(loop, UV_RUN_DEFAULT)`, close runtime-owned handles, and close the loop before returning to the public runner.

#### Invariants

- Each `run_main` and `run_test` invocation owns a fresh default-independent `uv_loop_t`; no invocation uses libuv's global default loop.
- The private context remains an implementation detail in the C runtime and is not exposed through `bosatsu_ext_Bosatsu_l_Prog.h` except for any narrowly justified test-only helper guarded away from public generated code use.
- For synchronous programs, `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and `Effect` produce the same final success/error result and exit code as the current interpreter.
- `FlatMap` continuations run in program order, `Recover` skips successful values and handles raised errors as before, and raised errors skip pending flat-map frames until a recover frame or top-level completion.
- Repeated Prog tests in one process do not share loop state, pending callbacks, or handles.
- Loop close failures are not ignored: if `uv_loop_close` reports busy handles, runtime-owned handles are walked/closed and the loop is run again until close callbacks complete or a real runtime fault is reported.

#### Property Tests

- Add a C-level or generated-C-backed property-style regression for a representative family of small `Prog` trees containing `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix` where practical, and synchronous test effects, asserting the loop-backed runner matches the old synchronous semantics encoded by expected results.
- If broad generated C execution properties are too heavy for this PR, keep the property narrow to continuation algebra: generated left-associated flat-map/recover chains have the same result/error under the loop-backed runner as their right-associated expected form.

#### Assertion Tests

- Add C runtime assertions that `bsts_Bosatsu_Prog_run_test` returns success for a pure Prog through the libuv path.
- Add C runtime assertions that uncaught raises are still reported as `is_error=true` for Prog tests and as exit code `1` for main runs.
- Add C runtime assertions that flat-map after pure, flat-map after raise, recover after raise, and recover after pure preserve the current behavior through the new context stepper.
- Add a repeated-run C test that invokes two or more Prog tests in one process and verifies each run completes with an independently closed loop and no leaked runtime-owned handle state.

3. [ ] `step-3-verify-generated-prog-main-and-test-regressions` Exercise Generated Main And ProgTest Paths

Add focused generated C regression coverage proving existing Bosatsu `main` and `ProgTest` programs still compile, run, exit, and report results correctly after the runtime moves under libuv. Prefer existing test-workspace programs and C backend test harnesses where they already cover Prog association and generated test execution. Keep the slice behavior-neutral: do not migrate file IO, sleep, process, or other IO/Core effects to asynchronous libuv APIs in this issue.

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
- Run `scripts/test_basic.sh` as the required merge gate before PR submission.
