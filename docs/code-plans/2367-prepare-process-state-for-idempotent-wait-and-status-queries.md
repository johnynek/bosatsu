# Code Plan #2367

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2367` Prepare process state for idempotent wait and status queries
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Prepare the existing process runtime state so `wait(p)` is stable and idempotent across the JVM evaluator, Python external runtime, and C/libuv runtime, without adding any new public Bosatsu APIs. The final change should make each process object own one recorded final normalized exit code, allow repeated waits after exit to return the same value, preserve existing spawn/stdio behavior, and pass the repository gate `scripts/test_basic.sh` within the configured 2400 second timeout.

## Current State

The reviewed dependency artifact is `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, which defines stable process-status invariants for the later stop/status API work. Today `test_workspace/Bosatsu/IO/Core.bosatsu` exposes only `spawn` and `wait` for process handles. In `core/src/main/scala/dev/bosatsu/Predef.scala`, `ProcessValue` already has a mutable `cachedExitCode: Option[Int]`, and `prog_core_wait` fills it after `java.lang.Process.waitFor()`, but the shared state is not explicitly guarded for future status operations. In `test_workspace/ProgExt.py`, `_CoreProcess` already stores `exit_code`, and `wait_process` fills it after `subprocess.Popen.wait()`, but the final-code recording should be made explicit and normalized through one helper. In `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, `BSTS_Core_Process` records libuv exit fields but also has `wait_consumed`, causing the current C wait path to reject a second wait as `BadFileDescriptor`; this directly conflicts with the stable-status contract. Existing C process regression tests live in `c_runtime/test.c`, and Scala/Python process behavior is exercised through the existing evaluation/tool-test surfaces that load `test_workspace` externals.

## Problem

Downstream `poll`, `wait_timeout`, `terminate`, and `kill` work needs a process object whose final status can be observed repeatedly without consuming it. The current C/libuv implementation is still single-consumption, and the JVM/Python implementations rely on minimal mutable fields rather than a clearly factored final-status slot. If this state is not prepared first, later public status APIs would either duplicate backend-specific state handling or inherit inconsistent wait behavior. This slice should fix the root state model while keeping the public API unchanged and avoiding any opportunistic stop, poll, or timeout additions.

## Steps

1. [ ] `step-1` Factor Stable Exit State In JVM And Python

Update the JVM and Python process wrappers so final exit-code recording is an explicit shared operation rather than inline per-call mutation. In Scala, keep `ProcessValue` private to the evaluator, guard the cached final status consistently, and have `wait` return the already-recorded value without touching the underlying process. In Python, keep `_CoreProcess` as the runtime handle but route `wait_process` through a helper that normalizes and records exactly one final integer status. Do not change `Bosatsu/IO/Core.bosatsu`, `Prog.bosatsu_externals`, or add `terminate`, `kill`, `poll`, or `wait_timeout` in this slice.

#### Invariants

- Each JVM/Python process wrapper has one final exit-code slot that is empty before exit observation and immutable after it is filled.
- Repeated `wait(p)` calls after a child exits return the same integer code and do not re-wait or re-normalize mutable backend state.
- Ordinary zero and nonzero child exit codes are preserved.
- Invalid non-process values still map to the existing error style for that backend.
- Spawn argv semantics, inherited environment/cwd, and returned stdio handles are unchanged.

#### Property Tests

- Where the existing Scala/Python evaluation harness can express it cleanly, add a table/property-style test over representative exit codes such as `0` and `7` and repeated wait counts greater than one: spawn a direct child, call `wait` repeatedly on the same handle, and assert every observed code equals the expected final code.
- For Python-backed transpile/runtime coverage, use the same invariant through existing `test_workspace/ProgExt.py` externals rather than adding backend-specific public APIs.

#### Assertion Tests

- Add focused Scala evaluation/tool regression coverage for repeated `wait` after a zero exit and repeated `wait` after a nonzero exit.
- Add focused Python external/runtime coverage for repeated `wait` after a zero exit and repeated `wait` after a nonzero exit, using bounded direct children and no shell-specific behavior when the existing harness allows it.
- Confirm existing invalid-process wait coverage still reports the same error category.

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
