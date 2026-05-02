---
issue: 2365
priority: 3
touch_paths:
  - docs/design/2365-specify-the-portable-process-stop-and-status-contract.md
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - test_workspace/ProgExt.py
  - test_workspace/Prog.bosatsu_externals
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h
  - c_runtime/test.c
  - docs/src/main/paradox/design-docs/prog_concurrency_design.md
  - docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md
  - scripts/test_basic.sh
depends_on: []
estimated_size: M
generated_at: 2026-05-01T22:51:58Z
---

# Specify portable process stop and status contract

_Issue: #2365 (https://github.com/johnynek/bosatsu/issues/2365)_

## Summary

Defines the reviewed public contract for direct-child process stop and status APIs, backend mappings for JVM/Python/C-libuv, exit-code normalization, idempotent status observation, managed cleanup expectations, non-goals, tests, risks, and rollout guidance for issue #2365.

## Context

`Bosatsu/IO/Core.spawn` already exposes a portable direct-child process handle and `wait` observes the child's final integer status. Issue #2361 extends that surface so callers can stop children that do not exit on their own and can build timeout/escalation workflows without consuming the eventual exit status.

This document is the reviewed source of truth for the low-level process stop/status contract. Downstream implementation workers should use it directly when changing `Bosatsu/IO/Core`, the JVM evaluator externals, the Python test runtime externals, and the C/libuv runtime externals.

The existing process model remains in force:

- `spawn(cmd, args, stdio)` uses argv semantics, not shell semantics.
- `spawn` inherits the host runtime's current working directory and environment until a future API adds explicit `cwd` or `env` fields.
- Non-zero child exit is not an `IOError`; it is reported through process status APIs.
- `SpawnResult.stdin`, `SpawnResult.stdout`, and `SpawnResult.stderr` are returned only for literal `Stdio.Pipe` entries.
- Low-level process APIs do not implicitly close, drain, or flush any returned stdio handles.

## Public API Contract

Add the following public API to `test_workspace/Bosatsu/IO/Core.bosatsu` and export it with the existing process surface:

```bosatsu
enum StopResult:
  StopSent
  AlreadyExited

external def terminate(p: Process) -> Prog[IOError, StopResult]
external def kill(p: Process) -> Prog[IOError, StopResult]
external def poll(p: Process) -> Prog[IOError, Option[Int]]
external def wait_timeout(p: Process, d: Duration) -> Prog[IOError, Option[Int]]
```

The existing `wait(p: Process) -> Prog[IOError, Int]` becomes part of the same stable status contract.

`terminate` asks the direct child to stop using the backend's best available normal termination mechanism. The operation is semantic, not signal-specific. It does not guarantee that the child exits, and it does not guarantee graceful application-level shutdown on every backend.

`kill` asks the direct child to stop using the backend's best available forceful termination mechanism. It is also semantic, not signal-specific.

`poll` is a non-blocking status query. It returns `Some(code)` only after the runtime has recorded the final normalized exit code. It returns `None` while the direct child is still running or while the runtime has not yet observed exit.

`wait_timeout` has these required semantics:

- It waits up to the supplied `Duration` and returns `Some(code)` if the runtime records exit before the timeout elapses.
- It returns `None` if the timeout expires first. A timeout is not a process result and must not consume or invalidate the later final status.
- A zero or negative `Duration` is valid and behaves as a non-blocking status check: return `Some(code)` if final status is already recorded, otherwise return `None`.
- `Duration.to_nanos` is interpreted as nanoseconds. Backends that require a coarser timeout unit must round positive durations up to the next representable unit so a positive timeout is not accidentally converted to an immediate poll.
- If a positive duration exceeds the backend timeout type's maximum representable value, clamp to that maximum finite wait rather than overflowing or wrapping.
- Timeout measurement must use a monotonic clock or the backend's monotonic wait primitive, not wall-clock time.

`wait` waits until the final normalized exit code is recorded and returns that code.

## Direct-Child Scope

All operations apply only to the direct process represented by the `Process` value returned from `spawn`.

The public contract intentionally excludes process-tree, process-group, session, job-object, and descendant cleanup semantics. Implementations must not promise that stopping a direct child also stops grandchildren or other related processes. If a child creates descendants that outlive it, those descendants are outside this API's contract.

The public API must not expose raw signal names, raw signal numbers, process ids, `send_signal`, POSIX-only signal APIs, Windows job-object controls, or platform-specific process-group switches in this issue. Those may be designed separately only if Bosatsu later needs a structured non-portable or process-tree API.

## Status Invariants

Each `Process` runtime object owns one final normalized exit code slot. Once that slot is filled, it never changes.

The following invariants must hold for every backend that supports processes:

- `wait`, `poll`, and `wait_timeout` observe the same final normalized code after exit.
- Repeated `wait(p)` calls after exit return the same code.
- Repeated `poll(p)` calls after exit return `Some(the_same_code)`.
- Repeated `wait_timeout(p, d)` calls after exit return `Some(the_same_code)` without waiting for `d`.
- `wait_timeout(p, d)` returning `None` does not consume the eventual exit result.
- `terminate(p)` and `kill(p)` return `AlreadyExited` when the runtime has already recorded final exit.
- `terminate(p)` and `kill(p)` return `StopSent` when they successfully issue a stop request to a process that has not yet been recorded as exited.
- A stop request never consumes the final exit status; a later `wait`, `poll`, or `wait_timeout` still observes the recorded code.
- Stop/status operations never close or drain `SpawnResult` stdio handles.
- Canceling or abandoning a Bosatsu wait operation must not kill the external process.

Backends may have races where the operating system has exited the child but the runtime has not yet processed the exit notification. In that window, `terminate` or `kill` may either observe the recorded exit and return `AlreadyExited`, or send a stop request and return `StopSent` if the backend still accepts the request. After the exit is recorded, the stable-status invariants above take over.

Concurrent waits should share the same completion state. Implementations should avoid issuing competing independent waits that can race or consume each other's result. The C/libuv implementation should support multiple waiters if the runtime scheduler supports concurrent fibers; if the immediate runtime has only one active waiter path, it must still preserve stable post-exit `wait` behavior and must not keep the old single-consumption state as the long-term contract.

## Exit-Code Normalization

Bosatsu's current process status surface returns an `Int`, so this contract intentionally collapses ordinary exit and signal termination into one normalized integer.

Normalization rules:

- Ordinary exit returns the process exit code as an `Int`.
- POSIX-style signal termination returns `128 + signal_number`.
- Python negative `Popen.returncode` values are normalized to `128 + abs(returncode)`.
- libuv `(exit_status, term_signal)` results are normalized as `exit_status` when `term_signal == 0`, otherwise `128 + term_signal`.
- JVM `Process.exitValue()` and `Process.waitFor()` return the backend's integer exit code directly; on Unix-like JVMs this already includes POSIX signal normalization such as `128 + signal_number`.

The implementation should store the normalized Bosatsu integer in the shared process state, not repeatedly re-normalize from mutable backend state. If Bosatsu later needs to distinguish normal exit from signal termination, that should be a new structured `ExitStatus` API rather than a silent behavior change to `wait`.

## Backend Mappings

### JVM

The current JVM evaluator process state lives in `core/src/main/scala/dev/bosatsu/Predef.scala` around `ProcessValue(process: java.lang.Process, cachedExitCode: Option[Int])` and uses `ProcessBuilder` plus `Process.waitFor()`.

Required mapping:

- `terminate` maps to `java.lang.Process.destroy()`.
- `kill` maps to `java.lang.Process.destroyForcibly()`.
- `poll` uses `Process.isAlive` or `Process.exitValue()` without blocking, while respecting the shared cached normalized exit code.
- `wait_timeout` uses a bounded wait path such as `Process.waitFor(timeout, unit)` or an equivalent runtime wait, then records and returns the same cached code on success.
- `wait` records one final code and returns it for all later calls.

Implementation notes:

- Guard shared process state if the evaluator can execute process operations concurrently.
- `destroy()` and `destroyForcibly()` should only be called when the cached exit code is empty and the process appears alive.
- If a bounded wait observes completion, it must fill the same cached exit-code slot used by `wait` and `poll`.

### Python

The current Python test runtime process state lives in `test_workspace/ProgExt.py` as `_CoreProcess(process, exit_code)` and maps spawn to `subprocess.Popen(..., shell=False, text=True, encoding="utf-8")`.

Required mapping:

- `terminate` maps to `subprocess.Popen.terminate()`.
- `kill` maps to `subprocess.Popen.kill()`.
- `poll` maps to `Popen.poll()` and stores the normalized code when non-`None`.
- `wait_timeout` maps to `Popen.wait(timeout=seconds)` or an equivalent shared waiter. Timeout returns `None`; it is not an error and does not cache a result.
- `wait` records one normalized final code and returns it for all later calls.

Implementation notes:

- Normalize negative POSIX return codes before caching.
- As concurrency support grows, `_CoreProcess` should carry a lock and shared completion state so all status operations coordinate through one final code slot.
- `TimeoutExpired` from `Popen.wait(timeout=...)` is the expected implementation mechanism for `wait_timeout` returning `None`.

### C/libuv

The current C runtime process state lives in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` as a `BSTS_Core_Process` containing a `uv_process_t`, exit fields, close state, and wait state. The desired contract is handle-based: operate on the live `uv_process_t`, not on a cached pid.

Required mapping:

- `terminate` sends the backend's normal stop request through the `uv_process_t` handle. On Unix-like libuv targets this may be implemented with `uv_process_kill(&process->process, SIGTERM)` behind the semantic API. On Windows, use libuv's portable process-kill behavior as the available normal stop mapping, while documenting that graceful console-control semantics are not promised.
- `kill` sends the strongest portable forceful stop request available through the `uv_process_t` handle. On Unix-like libuv targets this may be `uv_process_kill(&process->process, SIGKILL)` behind the semantic API. On Windows, use libuv's portable process-kill behavior.
- On Windows, libuv maps non-zero `uv_process_kill` signals to `TerminateProcess`, so the C/libuv `terminate` and `kill` operations are expected to behave identically there even though they remain distinct portable Bosatsu operations.
- `poll` returns the cached normalized code if the libuv exit callback has run, otherwise `None`.
- `wait_timeout` combines the process exit callback with a `uv_timer_t` deadline. If the timer wins, return `None` and leave the process waiter/status state live for later observation.
- `wait` waits for the libuv exit callback, records the normalized status once, and returns that same status for all later calls.

Implementation notes:

- Keep the `uv_process_t` valid until libuv has delivered the process exit callback.
- Do not close the `uv_process_t` before process exit; doing so risks losing exit observation and can create zombie-process hazards on Unix.
- Do not implement stop by storing a pid and calling an OS kill function later. Pid reuse makes that unsafe.
- The old single-consumption C fields such as `wait_consumed` are not the public contract. Downstream state work should replace or bypass them with stable cached status and waiter coordination.
- `uv_process_kill` or equivalent handle-based libuv APIs may return backend errors. Map real failures to `IOError`, but treat already-recorded exit as `AlreadyExited` before attempting a stop.

## Managed Cleanup Helper Intent

A higher-level Bosatsu helper should later provide deterministic cleanup around `spawn`, but that helper is not required to land in the same implementation slice as the low-level APIs.

The intended helper behavior is:

- Run user code with the `SpawnResult`.
- On cleanup, close any returned `stdin`, `stdout`, and `stderr` pipe handles owned by the `SpawnResult`.
- If the direct child is still running after user code exits early or with an error, call `terminate`, wait for a configured grace `Duration`, then call `kill` if the child is still running.
- Always wait/reap the direct child before cleanup returns.
- Preserve the low-level APIs for callers that need custom ordering, such as draining stdout/stderr before closing handles or before escalating from `terminate` to `kill`.

This helper must not change the low-level contract: `terminate` and `kill` themselves still do not close stdio handles and still do not operate on process trees.

## Non-Goals And Rejected API Shapes

This issue does not add:

- `send_signal(pid, int)`.
- `send_posix_signal` or public POSIX signal enums.
- Public process ids.
- Process-tree, process-group, session, or Windows job-object APIs.
- A guarantee that `terminate` is graceful on every backend.
- Implicit stdio closure from `terminate`, `kill`, `poll`, `wait_timeout`, or `wait`.
- A structured `ExitStatus` replacement for the existing integer status.
- Shell-based process semantics.

These exclusions are part of the portability contract, not omissions to fill opportunistically in downstream implementation work.

## Testing Guidance

Property-check style tests should cover the behavioral invariants where the result should hold across many interleavings, durations, and child exit timings:

- Stable status: once any operation observes `Some(code)` or `wait` returns `code`, all later status observations return that same code.
- Timeout non-consumption: for generated short timeout durations and bounded long-running children, any number of `wait_timeout(...)=None` observations can be followed by stop or natural exit and then stable final `wait`.
- Duration edge cases: generated non-positive and small positive `Duration.to_nanos` values preserve the immediate-poll or rounded-up-wait behavior without consuming final status.
- Stop idempotence: after a child has exited and status is recorded, arbitrary sequences of `terminate`, `kill`, `poll`, `wait_timeout`, and `wait` preserve `AlreadyExited` or the same final status as appropriate.
- Stop does not close stdio: after a low-level stop request, returned pipe handles remain ordinary handles until explicitly closed or until backend EOF/broken-pipe behavior occurs naturally.

Narrow case-based tests remain the right fit for backend mappings and platform-sensitive behavior:

- JVM `terminate` calls `destroy()` and `kill` calls `destroyForcibly()` behavior using real long-running children.
- Python `terminate()` and `kill()` behavior using `subprocess.Popen` children.
- C/libuv handle-based termination through `uv_process_t` and exit callback status recording.
- POSIX signal normalization to `128 + signal_number` where the platform exposes signal termination.
- Invalid process values map to the existing bad-file-descriptor or invalid-argument style used by that backend.
- `wait_timeout` with zero, negative, or very small positive durations returns `None` for a child that is known to remain alive.
- Positive sub-unit timeout conversion is rounded up rather than collapsed to a zero-duration poll on JVM, Python, and C/libuv backends.
- Oversized positive timeout conversion clamps to the backend maximum finite wait rather than overflowing, wrapping, or becoming a negative duration.
- `poll` returns `None` before exit and `Some(code)` after exit.

Use bounded children and avoid shell-specific behavior when possible. Where shell commands are necessary in existing C tests, keep them local to test helpers and guarded for platform support.

Downstream workers should run the relevant focused targets for the files they touch, plus the repository gate:

```sh
make -C c_runtime test_out
scripts/test_basic.sh
```

Backend-specific workers should also run the existing Scala/JVM and Python test flows that already exercise `Bosatsu/IO/Core` externals in this repository. Add new tests near the current process coverage:

- C runtime process tests in `c_runtime/test.c`.
- JVM evaluator tests under the existing Scala test locations that cover `core/src/main/scala/dev/bosatsu/Predef.scala` externals.
- Python external/runtime tests that use `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`.

## Acceptance Criteria

The reference contract is satisfied when downstream implementation can demonstrate all of the following:

- `StopResult`, `terminate`, `kill`, `poll`, and `wait_timeout` are public from `Bosatsu/IO/Core` alongside `spawn` and `wait`.
- `terminate` and `kill` operate only on the direct spawned child and return `StopSent` or `AlreadyExited` according to the recorded process state.
- `terminate` and `kill` do not consume the final status and do not close returned stdio handles.
- `poll`, `wait_timeout`, and `wait` share one stable normalized exit-code record.
- `wait_timeout` returning `None` leaves the eventual final status observable by later `wait`, `poll`, or `wait_timeout`.
- POSIX-style signal termination is normalized to `128 + signal_number`; ordinary exit codes are preserved.
- JVM maps normal and forceful stop to `Process.destroy()` and `Process.destroyForcibly()`.
- Python maps normal and forceful stop to `Popen.terminate()` and `Popen.kill()`.
- C/libuv stop operations use the live `uv_process_t` handle and keep it valid until libuv observes process exit.
- No raw public signal API, process id API, or process-tree/process-group semantics are introduced.
- Focused backend tests and the repository basic test gate cover stable wait, timeout non-consumption, non-positive, rounded-up, and oversized timeout handling, stop idempotence, direct-child behavior, and stdio ownership.

## Risks And Rollout Notes

The largest portability risk is over-promising graceful termination. `terminate` must remain a best-effort semantic request whose exact native behavior differs by backend and operating system.

The largest C/runtime risk is process lifetime management. Closing or invalidating `uv_process_t` before the exit callback can lose exit status and create zombie hazards. C implementation work should prioritize stable process state before adding public stop/status externals.

The largest API risk is accidentally exposing platform details. Downstream code should resist adding raw signal names or process ids to make tests easier. Tests should validate semantic outcomes instead.

The safest rollout order is the roadmap order: first stabilize process exit state and idempotent `wait`, then add low-level public stop/status operations, then specify and implement the managed helper, then broaden cross-backend coverage and user-facing documentation.

Existing callers of `spawn` and `wait` should continue to work. The intentional behavior change is that `wait` is stable/idempotent everywhere rather than single-consumption on any backend path.
