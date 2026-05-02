# Code Plan #2367

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2367` Prepare process state for idempotent wait and status queries
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `0`
- Completed steps: `3`
- Total steps: `3`

## Summary

Prepare the existing process runtime state so `wait(p)` is stable and idempotent across the JVM evaluator, Python external runtime, and C/libuv runtime, without adding any new public Bosatsu APIs. The final change makes each process object own one recorded final normalized exit code, allows repeated waits after exit to return the same value, preserves existing spawn/stdio behavior, and passes the repository gate `scripts/test_basic.sh` within the configured 2400 second timeout.

## Current State

The reviewed dependency artifact is `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, which defines stable process-status invariants for the later stop/status API work. Today `test_workspace/Bosatsu/IO/Core.bosatsu` exposes only `spawn` and `wait` for process handles. In `core/src/main/scala/dev/bosatsu/Predef.scala`, `ProcessValue` is private to the JVM evaluator and owns a synchronized final-exit-code slot through `waitForExitCode()`, so repeated JVM `wait` calls return the recorded value. In `test_workspace/ProgExt.py`, `_CoreProcess` remains the Python runtime handle, and `wait_process` routes through helpers that normalize and record exactly one final integer status, including Python negative return-code normalization to `128 + abs(code)`. In `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, `BSTS_Core_Process` now records libuv exit fields once and no longer carries or sets `wait_consumed`; post-exit waits return the cached normalized status repeatedly, while an already-suspended wait is still resumed exactly once from the exit callback. Focused repeated-wait coverage exists through `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, JVM/Python tests, and `c_runtime/test.c` cases for repeated zero and nonzero C/libuv waits. This verification round fixed `ProcessWaitMain.main` to wrap `run_test` with `show_error(run_test, 1)`, matching the existing IO test program pattern and giving `Main` the required `forall err. Prog[err, Int]` shape.

## Problem

Downstream `poll`, `wait_timeout`, `terminate`, and `kill` work needs a process object whose final status can be observed repeatedly without consuming it. The former C/libuv single-consumption wait model conflicted with that stable-status contract; this branch removes that root state issue for existing `wait` behavior. The verification round found and repaired a narrow test-program typing issue in `ProcessWaitMain.bosatsu`; the repository-required gate now passes, and the slice still avoids opportunistic stop, poll, or timeout additions.

## Steps

1. [x] `step-1` Factor Stable Exit State In JVM And Python

Update the JVM and Python process wrappers so final exit-code recording is an explicit shared operation rather than inline per-call mutation. In Scala, keep `ProcessValue` private to the evaluator, guard the cached final status consistently, and have `wait` return the already-recorded value without touching the underlying process. In Python, keep `_CoreProcess` as the runtime handle but route `wait_process` through a helper that normalizes and records exactly one final integer status. Do not change `Bosatsu/IO/Core.bosatsu`, `Prog.bosatsu_externals`, or add `terminate`, `kill`, `poll`, or `wait_timeout` in this slice.

#### Invariants

- Each JVM/Python process wrapper has one final exit-code slot that is empty before exit observation and immutable after it is filled.
- Repeated `wait(p)` calls after a child exits return the same integer code and do not re-wait or re-normalize mutable backend state.
- Ordinary zero and nonzero child exit codes are preserved.
- Invalid non-process values still map to the existing error style for that backend.
- Spawn argv semantics, inherited environment/cwd, and returned stdio handles are unchanged.

#### Property Tests

- Added a table-style Bosatsu process program over representative exit codes `0` and `7`: spawn a direct `python3` child, call `wait` twice on the same process handle, and require every observed code to equal the expected final code.
- For Python-backed transpile/runtime coverage, wired the same invariant through `test_python.sh` using `test_workspace/ProgExt.py` externals rather than adding backend-specific public APIs.

#### Assertion Tests

- Added focused Scala JVM evaluation regression coverage in `EvaluationTest.scala` for repeated `wait` after zero and nonzero exits via `ProcessWaitMain`.
- Added focused Python external/runtime coverage in `test_python.sh` for repeated `wait` after zero and nonzero exits via `ProcessWaitMain`.
- The invalid-process wait branch was left structurally unchanged: JVM still rejects non-`ProcessValue` values through `asProcessValue`, and Python still rejects non-`_CoreProcess` values before waiting.

#### Completion Notes

Implemented the JVM state refactor in `core/src/main/scala/dev/bosatsu/Predef.scala` by replacing the mutable public-ish case-class field with a private evaluator class and synchronized `waitForExitCode()` helper. Implemented the Python state refactor in `test_workspace/ProgExt.py` with `_normalize_process_exit_code`, `_record_process_exit_code`, and `_wait_core_process`. Added `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` and wired it into JVM/Python-focused tests without changing shared `Bosatsu/IO/Core.bosatsu`, `Prog.bosatsu_externals`, or any public stop/status API.

2. [x] `step-2` Make C Libuv Wait Non-Consuming

Replace the C/libuv wait-consumption model with stable cached status observation. The libuv exit callback records the final status fields, resumes any currently suspended waiter with the normalized status, and leaves the process object able to answer later waits from the recorded value. `wait_consumed` has been removed as a source of user-visible failure for repeated waits. The `uv_process_t` lifecycle remains handle-based: the implementation does not close or invalidate it before libuv delivers the process exit callback, and cleanup still happens after exit observation or spawn setup failure as before.

#### Invariants

- `BSTS_Core_Process` records the final normalized status once from `(exit_status, term_signal)` and never changes it afterward.
- A wait issued after `exited` is true returns the cached normalized status every time.
- A wait already suspended before exit is resumed exactly once when the libuv exit callback runs.
- The process handle remains valid until libuv observes process exit; the implementation does not introduce pid-cached control paths.
- Existing spawn failure cleanup, active-process rooting, stdio pipe ownership, and invalid-process error behavior are preserved.

#### Property Tests

- Added C runtime repeated-wait coverage as a small table over exit codes `0` and `7`: for each code, spawn a child, wait on the same process twice, and assert both observed waits equal the expected code.
- The property is intentionally narrow to match the current C test harness while covering the stable-status invariant for representative zero and nonzero exits.

#### Assertion Tests

- Added `c_runtime/test.c` regression coverage for repeated wait after a zero exit.
- Added `c_runtime/test.c` regression coverage for repeated wait after a nonzero exit.
- Re-ran the existing C libuv runtime assertions through `make -C c_runtime test_out`, including single wait, process rooting after dropped `Process` values, wait after GC, spawn failure recovery, piped stdout/stderr/stdin, incompatible stdio handles, and invalid process wait.

#### Completion Notes

Implemented the C/libuv state change in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` by removing `wait_consumed`, stopping the exit callback and post-exit wait path from consuming status, and keeping only an active `wait_suspended` guard for a currently pending wait. Added a defensive exited check in `bsts_core_wait_start` so a wait starting after exit observation returns the cached status immediately. Added repeated wait tests in `c_runtime/test.c` for `/bin/sh -c 'exit 0'` and `/bin/sh -c 'exit 7'`. Focused checks passed: `make -C c_runtime test_out`, `rg -n "wait_consumed" c_runtime` returned no matches, and `git diff --check` passed.

3. [x] `step-3` Verify Public Behavior And Required Gate

Run focused backend tests while iterating, then run the configured repository-required gate before the PR is considered ready. Because this slice touches Scala evaluator code, Python externals, and C/libuv runtime code, include focused checks for those surfaces in addition to `scripts/test_basic.sh`. Keep the final diff limited to process state/wait behavior and regression tests.

#### Invariants

- No new public Bosatsu API is introduced in this issue.
- Existing `spawn`, stdio, and single `wait` behavior remains compatible for current callers.
- The implementation is shippable only after `scripts/test_basic.sh` passes under the configured 2400 second timeout; this verification round reached that state.
- Coverage is focused on stable repeated wait behavior and does not drift into stop/status API implementation.

#### Property Tests

- Used the repeated-wait stable-status tests from the JVM/Python and C steps as the behavioral contract: for each backend and representative exit code, all waits on the same process handle after final status observation return the same code.

#### Assertion Tests

- Ran `python3 -m py_compile test_workspace/ProgExt.py`.
- Ran `make -B -C c_runtime test_out` after noticing a stale zero-byte `c_runtime/test_out` made a plain make invocation a no-op.
- Built the local CLI assembly with `sbt -batch cli/assembly` so the Python transpile/runtime script could run through `./bosatsuj`.
- Ran `./test_python.sh`, including the `ProcessWaitMain` Python external/runtime path.
- Ran focused Scala invocation `sbt -batch "coreJVM/testOnly dev.bosatsu.EvaluationTest -- *process wait is stable*"`; sbt completed successfully, though the filter reported zero matching tests, so the required gate is the stronger Scala signal for this round.
- Ran the required gate `scripts/test_basic.sh` successfully after the `ProcessWaitMain.main` typing fix.
- Ran `git diff --check` successfully.

#### Completion Notes

The first `scripts/test_basic.sh` run failed in `cli / Test / testOnly` because `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` used `main = Main(_ -> run_test)`, which left the `Main` branch at `Prog[IOError, Int]` instead of the required `forall err. Prog[err, Int]`. Fixed the test program by importing `show_error` from `Bosatsu/IO/Std` and using `main = Main(_ -> show_error(run_test, 1))`, matching adjacent IO examples such as `CreateModeMain.bosatsu`. After that fix, `scripts/test_basic.sh` passed. Additional focused checks passed: `python3 -m py_compile test_workspace/ProgExt.py`, `make -B -C c_runtime test_out`, `sbt -batch cli/assembly`, `./test_python.sh`, focused Scala invocation, and `git diff --check`. A stale earlier focused sbt tool session produced no further output and could not be inspected or killed due sandbox process-list restrictions, but it was superseded by the successful focused rerun and required gate.
