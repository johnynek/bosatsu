# Code Plan #2362

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2362` Harden C backend libuv coverage and CI validation
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `3`
- Completed steps: `1`
- Total steps: `4`

## Summary

Harden validation for the completed libuv-backed C runtime by making the sanitizer and valgrind scripts exercise the vendored dependency pipeline, generated C binaries, `GC_THREADS`, libuv link flags, Main/Test compatibility, async continuation behavior, file IO, process wait, and practical GC/thread safety assumptions. The final branch should remain gated by `scripts/test_basic.sh` and should also document the targeted C validation commands expected before PR submission.

## Current State

The repository already contains the direct dependency work for this roadmap node: `c_runtime/deps.json` pins vendored libuv, `VendoredDeps` and `CDeps` have libuv recipe/link metadata tests, the C runtime `Makefile` supports libuv for vendored and non-vendored builds, and `c_runtime/test.c` already exercises the libuv Prog loop, async suspend/resume helpers, IO/Core libuv effects, and process-related paths. This round tightened `scripts/c_runtime_ci_env.py` with opt-in vendored libuv validation, updated the sanitizer and valgrind scripts to require vendored libuv/bdwgc archives, `GC_THREADS`, script-specific compile flags, and preserved transitive system link flags, and scoped their generated-runtime cleanliness check to `c_runtime` so intentional PR script edits do not make the check fail. `scripts/test_basic.sh` passed, and `scripts/test_c_sanitizers.sh` passed after building the CLI assembly. Valgrind is not installed in this worker environment.

## Problem

The libuv integration spans several risk families that are easy to regress independently: vendored link metadata can omit required system flags or `GC_THREADS`; generated C programs can accidentally stop linking against the same dependency set as the runtime; Main and Test runners can diverge in initialization or loop ownership; async success/error continuations can break after IO completion; file/process IO can leak handles or lose error recovery behavior; and GC-managed `BValue`s must stay reachable across libuv callbacks. The existing tests cover many pieces, but issue #2362 asks for an explicit hardening pass that ties those pieces into CI-oriented scripts and fills any narrow coverage gaps before the roadmap closes.

## Steps

1. [x] `step-1` Tighten C CI Flag Validation

Audit `scripts/test_c_sanitizers.sh`, `scripts/test_c_valgrind.sh`, and `scripts/c_runtime_ci_env.py` so the scripts prove they are compiling and linking against the installed vendored runtime metadata, not accidentally relying on host defaults. Keep changes limited to shell/Python validation and adjacent assertions needed to expose libuv archive flags, transitive system link flags, sanitizer/valgrind compile flags, and `GC_THREADS` requirements for generated C and runtime C.

#### Invariants

- Vendored C runtime installs expose concrete static archive paths before system link flags.
- `GC_THREADS` remains present for bdwgc-backed runtime and generated C compilation paths.
- The CI helper strips only the runtime self include/archive paths that should be supplied by the local `c_runtime` build and preserves vendored dependency flags needed by generated binaries.
- Sanitizer and valgrind scripts build `c_runtime` with `VENDORED_DEPS=1` and exercise the same installed metadata used by generated C invocations.

#### Property Tests

- If practical in Scala/Python-adjacent tests, generated `cc_conf.json`-like flag lists preserve non-runtime dependency flags while removing only the runtime self include/archive and `-lm` entries.
- Existing property coverage for pkg-config parsing continues to prove self-library filtering preserves arbitrary distinct system flags.

#### Assertion Tests

- Added script-level assertions that installed `cc_conf.json` and exported helper variables contain vendored libuv and bdwgc static archives, preserved transitive system link flags, and `-DGC_THREADS`.
- Added script-level assertions that sanitizer flags, valgrind compile flags, and `-DBSTS_CI=1` are present in the installed generated C configuration.
- The helper now fails with clear `c_runtime_ci_env.py:` messages when required vendored libuv/GC flags are absent.

#### Completion Notes

Implemented opt-in `--validate-vendored-libuv` and repeatable `--require-cflag` checks in `scripts/c_runtime_ci_env.py`. Both C validation scripts now invoke the helper with validation enabled and retain local Python assertions over `cc_conf.json` and `C_RUNTIME_LIBS`. The helper continues to export the filtered compile/link environment, strips `bosatsu_platform.a` and `-lm`, and validates that libuv/bdwgc archives plus at least one transitive system link flag remain. Also scoped the scripts' generated-runtime `git diff` checks to `c_runtime` so validation can run on a dirty PR containing script edits. Verified with `python3 -m py_compile scripts/c_runtime_ci_env.py`, `bash -n scripts/test_c_sanitizers.sh`, `bash -n scripts/test_c_valgrind.sh`, synthetic helper success/failure checks, `sbt -batch cli/assembly`, `scripts/test_c_sanitizers.sh`, and `scripts/test_basic.sh`. `command -v valgrind` failed, so valgrind execution remains pending for an environment with valgrind installed.

2. [ ] `step-2` Extend Scala Metadata And Codegen Coverage

Add focused Scala tests only where current coverage is missing for this validation job. Prefer extending `VendoredDepsTest`, `CDepsTest`, `CDepsJvmTest`, or `ClangGenLibraryDepsTest` rather than creating new suites. Cover the dependency/link contract and generated Main/Test initialization contract without refactoring the dependency pipeline.

#### Invariants

- `c_runtime/deps.json` keeps bdwgc and libuv pins deterministic and parseable.
- Vendored link flags are emitted in the order needed for static archives and their system libraries.
- Libuv runtime requirements remain empty while bdwgc contributes `-DGC_THREADS` to runtime and generated C flags.
- Generated C Main and Test runners initialize GC/statics before entering Prog runners and use the expected Prog runtime entry points.

#### Property Tests

- Maintain or extend property coverage showing vendored link flags are stable for generated dependency orders and place archives before system flags.
- Maintain or extend property coverage showing pkg-config parsing preserves arbitrary system flags while filtering libuv/bdwgc self-library spellings.
- Use property-style tests for build-key or ordering behavior only if a newly discovered gap can be expressed as a deterministic invariant.

#### Assertion Tests

- Assert the checked-in manifest pins libuv 1.52.1 with the expected URI, hash, source subdir, and `libuv-cmake-static` recipe.
- Assert `BuildInputs.linkFlags` contains the libuv archive and required representative system flags in the expected relative order.
- Assert generated ProgTest and Main code still emits `GC_init()`, `init_statics()`, `atexit(free_statics)`, and the relevant Prog runner calls in order.

3. [ ] `step-3` Fill C Runtime Regression Gaps

Extend `c_runtime/test.c` narrowly to cover the runtime behaviors named by the issue that are not already asserted strongly enough. Keep the tests local and deterministic: prefer direct C runtime helpers for async continuation, recovered async errors, file IO, process wait, repeated Test isolation, and loop/handle cleanup rather than adding broad Bosatsu source fixtures unless generated C behavior specifically needs coverage.

#### Invariants

- Main and Test execution both run through the libuv-owned Prog loop with unchanged success and uncaught-error semantics.
- Suspended effects resume exactly once and continue captured `FlatMap`/`Recover` stacks after asynchronous completion.
- Recovered async errors behave the same as synchronous raised errors at the same Prog boundary.
- Runtime-owned file and process handles are closed or left in an intentionally owned state with no pending libuv handles leaking between Prog runs.
- Request records that carry `BValue`s across suspension stay reachable until the completion callback consumes them; worker-thread code does not allocate or dereference GC-managed Bosatsu objects off the loop thread.

#### Property Tests

- Do not add a large C property harness for this small job unless an existing helper makes it cheap; the useful property-level contracts are already represented in Scala metadata tests and repeated C loop-isolation cases.
- Where feasible, use small repeated-run loops in C as an invariant check that async requests and Test loops do not retain pending state across invocations.

#### Assertion Tests

- Add or strengthen C assertions for Main success and uncaught raise through the libuv loop.
- Add or strengthen C assertions for async success continuation after completion, async error recovery, start failure, double completion rejection, and unfinished/unreferenced work rejection.
- Add file IO assertions for representative read/write/flush/read-all/copy/error behavior through uv-backed handles.
- Add process wait assertions for successful wait, recovered spawn/wait failure paths, and process handle lifetime on failed spawn.
- Add a local GC/thread safety stress assertion that runs allocations/collections around in-flight or repeated libuv-backed operations without requiring non-deterministic timing.

4. [ ] `step-4` Run Required And Practical Verification

Use the repo-required gate as the shippability condition and document the practical libuv validation commands in the PR. Keep valgrind optional in the implementation notes because the issue explicitly allows reporting unavailable valgrind while still updating the script.

#### Invariants

- A PR cannot be submitted until `scripts/test_basic.sh` passes within the configured 2400 second timeout.
- Focused Scala suites for `dev.bosatsu.cruntime` and `dev.bosatsu.codegen.clang` pass after the metadata/codegen assertions are added.
- `scripts/test_c_sanitizers.sh` passes and exercises vendored libuv, bdwgc, generated C tests, generated Main builds, and Prog association tests.
- `scripts/test_c_valgrind.sh` is runnable when valgrind is installed and covers the same high-risk generated/runtime paths under memcheck.

#### Property Tests

- Run the ScalaCheck-backed suites touched by this plan, especially `CDepsTest` and `VendoredDepsTest`, so property invariants execute in the focused verification set.

#### Assertion Tests

- Run `sbt -batch "coreJVM/testOnly dev.bosatsu.cruntime.* dev.bosatsu.codegen.clang.ClangGenLibraryDepsTest -- --log=failure"` or the closest accepted focused command for these suites.
- Run `scripts/test_c_sanitizers.sh`.
- Run `scripts/test_c_valgrind.sh` when valgrind is installed; if unavailable, record that limitation in the PR while preserving the script updates.
- Run `scripts/test_basic.sh` as the final required gate.
