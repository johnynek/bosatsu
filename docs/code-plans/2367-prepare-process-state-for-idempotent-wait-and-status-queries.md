# Code Plan #2367

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2367` Prepare process state for idempotent wait and status queries
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `2`
- Completed steps: `1`
- Total steps: `3`

## Summary

Prepare the existing process runtime state so `wait(p)` is stable and idempotent across the JVM evaluator, Python external runtime, and C/libuv runtime, without adding any new public Bosatsu APIs. The final change should make each process object own one recorded final normalized exit code, allow repeated waits after exit to return the same value, preserve existing spawn/stdio behavior, and pass the repository gate `scripts/test_basic.sh` within the configured 2400 second timeout.

## Current State

The reviewed dependency artifact is `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, which defines stable process-status invariants for the later stop/status API work. Today `test_workspace/Bosatsu/IO/Core.bosatsu` exposes only `spawn` and `wait` for process handles. In `core/src/main/scala/dev/bosatsu/Predef.scala`, `ProcessValue` is now private to the JVM evaluator and owns a synchronized final-exit-code slot through `waitForExitCode()`, so repeated JVM `wait` calls return the recorded value. In `test_workspace/ProgExt.py`, `_CoreProcess` remains the Python runtime handle, and `wait_process` now routes through helpers that normalize and record exactly one final integer status, including Python negative return-code normalization to `128 + abs(code)`. Focused repeated-wait coverage was added through `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, a JVM evaluation test, and `test_python.sh`. In `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, `BSTS_Core_Process` still records libuv exit fields but also has `wait_consumed`, causing the current C wait path to reject a second wait as `BadFileDescriptor`; this directly conflicts with the stable-status contract and remains the next implementation step.

## Problem

Downstream `poll`, `wait_timeout`, `terminate`, and `kill` work needs a process object whose final status can be observed repeatedly without consuming it. The current C/libuv implementation is still single-consumption, while the JVM/Python state model has now been factored into explicit final-status recording helpers. The remaining root state issue is C/libuv: later public status APIs would inherit inconsistent wait behavior until that backend stops treating wait as consumed. This slice should continue to keep the public API unchanged and avoid opportunistic stop, poll, or timeout additions.

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

2. [ ] `step-2` Make C Libuv Wait Non-Consuming

Replace the C/libuv wait-consumption model with stable cached status observation. The libuv exit callback should record the normalized final status once, resume any currently suspended waiter with that status, and leave the process object able to answer later waits from the recorded value. Remove or bypass `wait_consumed` as a source of user-visible failure for repeated waits. Keep the `uv_process_t` lifecycle handle-based: do not close or invalidate it before libuv delivers the process exit callback, and continue closing/root cleanup only after exit has been observed or spawn setup fails as today.

#### Invariants

- `BSTS_Core_Process` records the final normalized status once from `(exit_status, term_signal)` and never changes it afterward.
- A wait issued after `exited` is true returns the cached normalized status every time.
- A wait already suspended before exit is resumed exactly once when the libuv exit callback runs.
- The process handle remains valid until libuv observes process exit; the implementation does not introduce pid-cached control paths.
- Existing spawn failure cleanup, active-process rooting, stdio pipe ownership, and invalid-process error behavior are preserved.

#### Property Tests

- For C tests, express the stable-status invariant as a small table over exit codes `0` and `7`: for each code, spawn a child, wait on the same process more than once after exit, and assert all observed waits equal the expected code.
- If the current C test harness makes arbitrary repeat counts awkward, keep the property narrow but explicit by checking at least two sequential waits for each representative code.

#### Assertion Tests

- Add `c_runtime/test.c` regressions for repeated wait after a zero exit.
- Add `c_runtime/test.c` regressions for repeated wait after a nonzero exit.
- Retain and re-run existing C assertions for single wait, process rooting after dropped `Process` values, wait after GC, spawn failure recovery, piped stdout/stderr/stdin, incompatible stdio handles, and invalid process wait.

3. [ ] `step-3` Verify Public Behavior And Required Gate

Run focused backend tests while iterating, then run the configured repository-required gate before the PR is considered ready. Because this slice touches Scala evaluator code, Python externals, and C/libuv runtime code, include focused checks for those surfaces in addition to `scripts/test_basic.sh`. Keep the final diff limited to process state/wait behavior and regression tests.

#### Invariants

- No new public Bosatsu API is introduced in this issue.
- Existing `spawn`, stdio, and single `wait` behavior remains compatible for current callers.
- The implementation remains shippable only after `scripts/test_basic.sh` passes under the configured 2400 second timeout.
- Coverage is focused on stable repeated wait behavior and does not drift into stop/status API implementation.

#### Property Tests

- Use the repeated-wait stable-status tests from the JVM/Python and C steps as the behavioral contract: for each backend and representative exit code, all waits on the same process handle after final status observation return the same code.

#### Assertion Tests

- Run the relevant focused Scala test target for the process evaluation/tool tests touched by the change.
- Run the relevant Python-backed test flow that exercises `test_workspace/ProgExt.py` externals, if separate from the Scala test invocation in this repo version.
- Run `make -C c_runtime test_out` for the C/libuv runtime tests.
- Run the required gate `scripts/test_basic.sh` before PR submission.

#### Completion Notes

Round-local checks completed: `python3 -m py_compile test_workspace/ProgExt.py`, direct Python helper assertions for repeated recording and negative-code normalization, and `git diff --check` passed. `./bosatsuj` could not run focused transpile checks because this checkout has no assembly jar. `sbt "coreJVM/testOnly dev.bosatsu.EvaluationTest -- *process wait is stable*"` and `sbt "coreJVM/test:compile"` did not return after initial project compilation; a subprocess-wrapped `sbt "coreJVM / Test / compile"` timed out after 600 seconds. The configured required gate `scripts/test_basic.sh` was not run in this round.
