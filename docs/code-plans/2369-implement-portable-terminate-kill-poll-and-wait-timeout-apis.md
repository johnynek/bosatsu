# Code Plan #2369

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2369` Implement portable terminate, kill, poll, and wait_timeout APIs
- Pending steps: `4`
- Completed steps: `0`
- Total steps: `4`

## Summary

Add the low-level portable process stop/status API from the reviewed contract: public `StopResult`, `terminate`, `kill`, `poll`, and `wait_timeout` in `Bosatsu/IO/Core`, implemented consistently across the JVM evaluator, Python test runtime, and C/libuv runtime. The final branch should keep `wait`, `poll`, and `wait_timeout` on one stable recorded exit code, make stop operations report `StopSent` versus `AlreadyExited`, and pass `scripts/test_basic.sh` within the configured 2400 second gate.

## Current State

`test_workspace/Bosatsu/IO/Core.bosatsu` currently exposes `Process`, `spawn`, and `wait`, but no public stop, poll, or timeout operations. The prior wait-state work is present: JVM `ProcessValue` in `core/src/main/scala/dev/bosatsu/Predef.scala`, Python `_CoreProcess` in `test_workspace/ProgExt.py`, and C `BSTS_Core_Process` in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` each cache final process status so repeated `wait` calls return the same code. Existing process coverage lives in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`, `test_python.sh`, and `c_runtime/test.c`. The reviewed contract is in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`; coding guidance in `coding_style.md` emphasizes typed invariants, immutable public APIs, focused tests while iterating, and the repository required gate.

## Problem

Callers cannot currently build portable process timeout and escalation flows. There is no way to request normal termination, forceful termination, non-blocking status observation, or bounded waiting without consuming the eventual status. Implementing these APIs separately per backend would risk divergent semantics, especially around already-exited children, timeout non-consumption, POSIX-style signal normalization, and returned stdio handle ownership. The implementation needs to extend the existing stable process-state slots rather than add one-off paths that race with or bypass `wait`.

## Steps

1. [ ] `step-1-public-api-and-external-wiring` Add Public API Surface

Update `test_workspace/Bosatsu/IO/Core.bosatsu` to define `enum StopResult: StopSent | AlreadyExited`, add external definitions for `terminate`, `kill`, `poll`, and `wait_timeout`, and export the new type and functions beside `Process`, `spawn`, and `wait`. Wire matching external names through `test_workspace/Prog.bosatsu_externals` and C runtime declarations in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h` so all backends expose the same shape before backend logic is filled in.

#### Invariants

- `StopResult` is a portable semantic result type; no raw signal names, signal numbers, process ids, process groups, or process-tree behavior are exposed.
- The new public functions use the existing `Process`, `Duration`, `Option[Int]`, and `Prog[IOError, ...]` types from `Bosatsu/IO/Core`.
- Existing `spawn` and `wait` exports remain source-compatible.

#### Property Tests

- Compile/typecheck coverage should establish that the new exports are available from `Bosatsu/IO/Core` without changing existing callers.
- Where Bosatsu-level tests generate operation sequences later, every sequence should be able to refer to the same public API across backends.

#### Assertion Tests

- Add or update a small Bosatsu process test module/import path so `StopResult`, `terminate`, `kill`, `poll`, and `wait_timeout` are imported from `Bosatsu/IO/Core`.
- Keep existing `ProcessWaitMain` repeated-wait behavior passing to prove the public API addition does not regress `wait`.

2. [ ] `step-2-jvm-and-python-runtime-semantics` Implement JVM And Python Backends

Extend JVM `ProcessValue` with shared helpers for final status recording, non-blocking poll, bounded wait, and stop requests. Map JVM `terminate` to `Process.destroy()` and `kill` to `Process.destroyForcibly()`. Extend Python `_CoreProcess` with the same shared state helpers, mapping `terminate` to `Popen.terminate()`, `kill` to `Popen.kill()`, `poll` to `Popen.poll()`, and `wait_timeout` to `Popen.wait(timeout=...)` with `TimeoutExpired` returning `None`. Convert positive nanosecond durations to backend timeout units by rounding up, treat zero or negative durations as an immediate poll, clamp excessive positive durations rather than overflowing, and preserve Python negative return-code normalization as `128 + abs(code)`.

#### Invariants

- Once a JVM or Python process records a final normalized code, that code never changes and is returned by later `wait`, `poll`, and `wait_timeout`.
- `wait_timeout` returning `None` never records a fake result and never prevents a later `wait` from observing the real exit code.
- `terminate` and `kill` return `AlreadyExited` when final status is already recorded, otherwise return `StopSent` after issuing the backend stop request.
- Stop/status operations do not close, drain, or flush returned stdin/stdout/stderr handles.

#### Property Tests

- For generated or table-driven operation sequences after a child has exited, arbitrary repetitions of `poll`, `wait_timeout`, and `wait` should observe the same final code.
- For generated non-positive and small positive durations against a long-running child, `wait_timeout` should return `None` before stop or natural exit and should not consume later `wait`.
- For generated already-exited cases, repeated `terminate` and `kill` should return `AlreadyExited` and leave the final status stable.

#### Assertion Tests

- JVM evaluator tests in `EvaluationTest.scala`/`ProcessWaitMain.bosatsu` for poll before and after exit, timeout followed by successful wait, terminate followed by wait, kill escalation of a long-running child, and stop on an already-exited child.
- Python runtime coverage through `test_python.sh` and `ProcessWaitMain.bosatsu` for the same concrete cases, including negative return-code normalization where platform behavior exposes signal termination.
- Focused edge cases for zero, negative, very small positive, and oversized timeout durations.

3. [ ] `step-3-c-libuv-runtime-semantics` Implement C/libuv Backend

Extend `BSTS_Core_Process` in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` so `wait`, `poll`, `wait_timeout`, `terminate`, and `kill` all observe or update one normalized final-status record. Add public C entry points in the `.c` and `.h` files. Implement stop through the live `uv_process_t` handle with `uv_process_kill` using the strongest portable mapping from the contract: normal termination via the backend normal stop signal where available and forceful kill via the strongest available forceful stop signal where available, while preserving Windows libuv behavior as a portable semantic mapping. Implement `wait_timeout` with a `uv_timer_t` deadline coordinated with the process exit callback; if the timer wins, resume with `None` while leaving exit observation live for later `wait`, `poll`, or `wait_timeout`.

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

Consolidate the process tests so the same public contract is exercised through the existing JVM, Python, and C runtime entry points without depending on shell-specific behavior beyond existing guarded helpers. Prefer compact Bosatsu-level invariant tests for stable status and timeout non-consumption where the same program can run on JVM and Python, and keep backend-specific C assertions for libuv handle/timer behavior. Finish by running the configured repository gate `scripts/test_basic.sh` with the 2400 second timeout expectation; also run focused process tests during implementation, including the existing C runtime test target when touching C/libuv.

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
