# Code Plan #2369

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2369` Implement portable terminate, kill, poll, and wait_timeout APIs
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `2`
- Completed steps: `2`
- Total steps: `4`

## Summary

Add the low-level portable process stop/status API from the reviewed contract: public `StopResult`, `terminate`, `kill`, `poll`, and `wait_timeout` in `Bosatsu/IO/Core`, implemented consistently across the JVM evaluator, Python test runtime, and C/libuv runtime. The branch now includes JVM and Python semantics plus shared JVM/Python Bosatsu-level coverage; the remaining runtime work is the C/libuv implementation and final cross-backend verification.

## Current State

`test_workspace/Bosatsu/IO/Core.bosatsu` exposes `StopResult`, `terminate`, `kill`, `poll`, and `wait_timeout` beside the existing `Process`, `spawn`, and `wait` API, and now also exposes `duration_from_nanos` so callers and tests can construct timeout `Duration` values without changing the existing `Duration` constructor export compatibility. JVM `ProcessValue` in `core/src/main/scala/dev/bosatsu/Predef.scala` now records one stable final exit code across `wait`, `poll`, and `wait_timeout`, maps `terminate` to `Process.destroy()`, and maps `kill` to `Process.destroyForcibly()`. Python `_CoreProcess` in `test_workspace/ProgExt.py` now has matching stable-status helpers, maps `terminate` to `Popen.terminate()`, maps `kill` to `Popen.kill()`, maps `poll` to `Popen.poll()`, and maps `wait_timeout` to `Popen.wait(timeout=...)` with `TimeoutExpired` returning `None`. Python still normalizes negative return codes as `128 + abs(code)`. C/libuv runtime entries remain placeholders for the pending C step. Shared process coverage in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` now exercises repeated wait, poll before and after exit, timeout non-consumption, terminate followed by wait, kill followed by wait, and already-exited stop behavior through the JVM evaluator and Python runtime.

## Problem

Callers can now build portable process timeout and escalation flows on JVM and Python, but the public API is not fully portable until the C/libuv backend replaces its remaining placeholder implementations. The directly coupled JVM/Python stable-status contract is implemented and covered; the remaining insufficiency is the C/libuv stop, poll, and bounded-wait semantics plus final cross-backend verification after that backend lands.

## Steps

1. [x] `step-1-public-api-and-external-wiring` Add Public API Surface

Updated `test_workspace/Bosatsu/IO/Core.bosatsu` to define `enum StopResult: StopSent | AlreadyExited`, add external definitions for `terminate`, `kill`, `poll`, and `wait_timeout`, and export the new type and functions beside `Process`, `spawn`, and `wait`. Wired matching external names through `test_workspace/Prog.bosatsu_externals`, JVM and Scala.js evaluator external registries, Python placeholder functions, and C runtime declarations/definitions so all current backend entry points expose the same shape before backend logic is filled in.

#### Invariants

- `StopResult` is a portable semantic result type; no raw signal names, signal numbers, process ids, process groups, or process-tree behavior are exposed.
- The new public functions use the existing `Process`, `Duration`, `Option[Int]`, and `Prog[IOError, ...]` types from `Bosatsu/IO/Core`.
- Existing `spawn` and `wait` exports remain source-compatible.
- Runtime placeholder functions return `Unsupported` until the planned backend semantics steps replace them; they do not claim stop/status behavior is implemented yet.

#### Property Tests

- Compile/typecheck coverage establishes that the new exports are available from `Bosatsu/IO/Core` without changing existing callers.
- Where Bosatsu-level tests generate operation sequences later, every sequence can refer to the same public API across backends.

#### Assertion Tests

- Updated `ProcessWaitMain.bosatsu` so `StopResult`, `StopSent`, `AlreadyExited`, `terminate`, `kill`, `poll`, and `wait_timeout` are imported from `Bosatsu/IO/Core` and typechecked through an unreachable branch that does not execute placeholder runtime behavior.
- Kept existing `ProcessWaitMain` repeated-wait behavior passing to prove the public API addition does not regress `wait`.
- Ran `scripts/test_basic.sh`; it passed with 74 tests.

#### Completion Notes

Completed the public API and external-name wiring slice. Added temporary `Unsupported` placeholders for newly wired runtime entries so package-level evaluation/transpilation does not fail on missing externals while later steps implement real behavior. Verification: `scripts/test_basic.sh` passed after fixing Bosatsu import/typecheck coverage.

2. [x] `step-2-jvm-and-python-runtime-semantics` Implement JVM And Python Backends

Replaced the JVM and Python `Unsupported` placeholders with real process semantics. Extended JVM `ProcessValue` with shared helpers for final status recording, non-blocking poll, bounded wait, and stop requests. Mapped JVM `terminate` to `Process.destroy()` and `kill` to `Process.destroyForcibly()`. Extended Python `_CoreProcess` with the same shared state helpers, mapping `terminate` to `Popen.terminate()`, `kill` to `Popen.kill()`, `poll` to `Popen.poll()`, and `wait_timeout` to `Popen.wait(timeout=...)` with `TimeoutExpired` returning `None`. Positive nanosecond durations are clamped before conversion where the backend has bounded timeout units, zero or negative durations are immediate polls, and Python negative return-code normalization remains `128 + abs(code)`. Added `duration_from_nanos` as a small public helper for constructing timeout durations while preserving the existing `Duration` export shape required by API compatibility checks.

#### Invariants

- Once a JVM or Python process records a final normalized code, that code never changes and is returned by later `wait`, `poll`, and `wait_timeout`.
- `wait_timeout` returning `None` never records a fake result and never prevents a later `wait` from observing the real exit code.
- `terminate` and `kill` return `AlreadyExited` when final status is already recorded, otherwise return `StopSent` after issuing the backend stop request.
- Stop/status operations do not close, drain, or flush returned stdin/stdout/stderr handles.
- Scala.js may continue returning `Unsupported` for process stop/status APIs unless Node process support is intentionally added in this issue.

#### Property Tests

- The shared JVM/Python process program now exercises operation sequences where timeout and poll observations are followed by stable `wait` and repeated final-status observations.
- The shared JVM/Python process program covers non-positive and tiny positive timeout durations against a running child, all returning `None` before later successful `wait`.
- The shared JVM/Python process program covers already-exited children where repeated stop operations return `AlreadyExited` and preserve the final code.

#### Assertion Tests

- JVM evaluator coverage in `EvaluationTest.scala`/`ProcessWaitMain.bosatsu` now covers poll before and after exit, timeout followed by successful wait, terminate followed by wait, kill of a long-running child, and stop on an already-exited child.
- Python runtime coverage through `test_python.sh` and `ProcessWaitMain.bosatsu` now covers the same concrete cases; Python stop return-code normalization is exercised through nonzero terminated/killed waits without depending on exact platform signal numbers.
- Focused timeout cases cover zero, negative, tiny positive, and oversized durations.

#### Completion Notes

Implemented the JVM and Python backend semantics and updated shared Bosatsu process coverage. Added `duration_from_nanos` after the required gate showed that changing the `Duration` constructor export shape would break API compatibility checks. Verification run this round: `sbt "coreJVM/clean; coreJVM/testOnly dev.bosatsu.EvaluationTest -- --log=failure"` passed with 88 tests; `sbt cli/assembly` succeeded; `./test_python.sh` passed; `git diff --check` passed; `scripts/test_basic.sh` passed with 2118 passed and 2 ignored.

3. [ ] `step-3-c-libuv-runtime-semantics` Implement C/libuv Backend

Replace the C runtime `Unsupported` placeholders added in step 1 with real libuv process semantics. Extend `BSTS_Core_Process` in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` so `wait`, `poll`, `wait_timeout`, `terminate`, and `kill` all observe or update one normalized final-status record. Implement stop through the live `uv_process_t` handle with `uv_process_kill` using the strongest portable mapping from the contract: normal termination via the backend normal stop signal where available and forceful kill via the strongest available forceful stop signal where available, while preserving Windows libuv behavior as a portable semantic mapping. Implement `wait_timeout` with a `uv_timer_t` deadline coordinated with the process exit callback; if the timer wins, resume with `None` while leaving exit observation live for later `wait`, `poll`, or `wait_timeout`.

#### Invariants

- The C implementation uses the live `uv_process_t` handle for stop requests and does not expose or cache pid-based public behavior.
- The `uv_process_t` remains valid until libuv has delivered the process exit callback and the final status is recorded.
- `poll` returns `Some(code)` only after the exit callback records final status and `None` otherwise.
- A timeout waiter and a later normal waiter cannot consume or overwrite each other's final status.
- Already-recorded exit is checked before attempting `uv_process_kill`, causing `AlreadyExited` instead of a backend error for completed children.
- Returned stdio handles remain owned and closable by the caller; stop/status operations do not implicitly close them.

#### Property Tests

- Model C process state transitions as operation sequences: timeout, poll, stop, exit callback, wait, and repeated observations must converge on one final normalized code.
- For repeated timeout observations before exit, any number of `None` results followed by exit must still allow stable `wait` and `poll` results.
- For stop after recorded exit, repeated terminate/kill observations should always return `AlreadyExited` and preserve the cached code.

#### Assertion Tests

- Add C runtime tests in `c_runtime/test.c` for poll before/after exit, wait_timeout timeout followed by wait, terminate followed by wait, kill of a long-running child, and terminate/kill after an already-exited child has been recorded.
- Add C tests for zero or non-positive timeout as immediate poll and small positive timeout as a real bounded wait that does not collapse to an accidental zero-duration conversion.
- Keep existing repeated wait, dropped process rooting, and piped stdio tests passing.

4. [ ] `step-4-cross-backend-tests-and-required-gate` Verify Cross-Backend Contract

Consolidate the process tests so the same public contract is exercised through the existing JVM, Python, and C runtime entry points without depending on shell-specific behavior beyond existing guarded helpers. JVM and Python now share Bosatsu-level coverage in `ProcessWaitMain.bosatsu`; after the C backend lands, add or align C runtime assertions for the same contract and rerun the configured repository gate `scripts/test_basic.sh` with the 2400 second timeout expectation. Also run focused process tests during implementation, including the existing C runtime test target when touching C/libuv.

#### Invariants

- All backends agree on the public contract for `StopSent`, `AlreadyExited`, `None`, and `Some(exit_code)` results.
- Timeouts are observations, not final statuses.
- Stop requests affect only the direct spawned child and do not promise process-tree cleanup.
- The final change remains reviewable as a small job: no high-level managed cleanup helper, structured `ExitStatus`, public signal API, pid API, cwd/env spawn redesign, or process-tree semantics are added.

#### Property Tests

- Cross-backend stable-status invariant: once any status API observes a code, all later status APIs observe the same code.
- Cross-backend timeout non-consumption invariant: one or more timeout `None` results can be followed by stop or natural exit and then stable final observation.
- Cross-backend stop idempotence invariant: after recorded exit, arbitrary stop/status observations preserve `AlreadyExited` or the same final status as appropriate.

#### Assertion Tests

- Concrete tests for idempotent stop on an already-exited child, stop followed by wait, kill escalation for a long-running child, poll before and after exit, and wait_timeout timeout followed by successful wait.
- Concrete tests that stop/status calls do not automatically close returned stdin/stdout/stderr handles where existing pipe helpers make that practical.
- Run `scripts/test_basic.sh`; if C runtime changes are not covered by that gate on the local platform, also run the focused C runtime test command used by the repo before PR submission.
