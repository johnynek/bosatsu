---
issue: 2371
priority: 3
touch_paths:
  - docs/design/2371-specify-the-managed-with-process-helper-contract.md
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu
  - test_workspace/Prog.bosatsu_externals
  - test_python.sh
  - c_runtime/test.c
  - docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md
  - scripts/test_basic.sh
depends_on: []
estimated_size: M
generated_at: 2026-05-02T23:15:34Z
---

# Reference doc for #2371: Specify the managed with_process helper contract

_Issue: #2371 (https://github.com/johnynek/bosatsu/issues/2371)_

## Summary

Defines the reviewed Bosatsu-level managed process helper contract, including the public error-polymorphic `with_process` shape in `Bosatsu/IO/Core`, cleanup ordering, error precedence, stdio ownership, process reap invariants, test guidance, risks, and rollout notes.

## Context

Issue #2361 added portable direct-child process control to Bosatsu. The reviewed low-level contract in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md` defines `StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`, and stable repeated `wait` semantics. The implemented low-level API is now available in `Bosatsu/IO/Core` across the JVM, Python, and C/libuv runtime paths.

This document specifies the higher-level managed helper that downstream workers should add on top of those low-level APIs. The helper is a Bosatsu library function, not a runtime external. It should make the common lifecycle safe by bracketing `spawn`, returned stdio pipe handles, direct-child stop escalation, and final reap.

The low-level contract remains unchanged:

- `terminate`, `kill`, `poll`, `wait_timeout`, and `wait` operate only on the direct child represented by `Process`.
- Low-level stop/status APIs do not implicitly close, drain, or flush `SpawnResult` stdio handles.
- Process-tree, process-group, session, descendant, pid-based, and raw signal behavior remain out of scope.
- Callers needing custom drain order, custom close order, process-specific protocols, or bespoke escalation should continue to use `spawn` and the low-level APIs directly.

## Goals

The managed helper must provide one standard lifecycle for common process use:

- Spawn a direct child using the existing argv-based `spawn(cmd, args, stdio)` API.
- Pass the resulting `SpawnResult` to user code.
- Run cleanup after user code succeeds or raises its own error type.
- Close all stdio pipe handles returned in `SpawnResult` during cleanup.
- If the direct child is still running, request normal termination, wait for a grace duration, then request forceful termination if needed.
- Always wait for the direct child before cleanup returns, so the child is reaped and its final status is recorded.
- Preserve the user result on success, or preserve the user's error when user code failed.
- Let callers choose their own error type while still providing one explicit path for helper-owned `IOError` values from spawn and cleanup.

## Public API Shape

Add the helper to `test_workspace/Bosatsu/IO/Core.bosatsu` and export it beside `spawn`, `wait`, `terminate`, `kill`, `poll`, and `wait_timeout`.

Accepted public shape:

    def with_process[e, a](cmd: String, args: List[String], stdio: StdioConfig, grace: Duration, on_error: IOError -> Prog[e, a], use: SpawnResult -> Prog[e, a]) -> Prog[e, a]

Accepted naming and placement:

- The helper name is `with_process`, matching existing snake_case Bosatsu library style.
- The helper lives in `Bosatsu/IO/Core` because it composes `spawn`, `SpawnResult`, `StdioConfig`, `Duration`, stdio `Handle` cleanup, and process stop/status APIs from that module.
- It is a normal Bosatsu definition, not an `external def`.
- It should be included in the module export list.

Accepted argument order:

- `cmd`, `args`, and `stdio` come first, in the same order as `spawn`.
- `grace` comes after `stdio`, because it configures cleanup rather than spawn itself.
- `on_error` comes before `use`, because it adapts helper-owned `IOError` values into the caller's chosen `Prog[e, a]` domain.
- `use` is last so call sites can pass the lifecycle body in the final position.

The `use` block has the exact type `SpawnResult -> Prog[e, a]`. The `on_error` handler has the exact type `IOError -> Prog[e, a]` and is used for helper-owned `IOError` values from `spawn`, stdio close, `poll`, `terminate`, `wait_timeout`, `kill`, and final `wait`. This keeps the helper error-polymorphic without hiding where process and stdio `IOError` values enter the caller's program.

Do not add a narrower `Prog[IOError, a]`-only overload in this issue. Callers that want the simple `IOError` shape can instantiate `e` as `IOError` and make `on_error` re-raise or otherwise handle the helper-owned error.

## Cleanup Ordering

After `spawn` succeeds, cleanup is mandatory whether `use` succeeds or fails. If `spawn` itself fails, no cleanup runs because no `SpawnResult` exists.

Required cleanup order:

1. Close returned `stdin` when `SpawnResult.stdin` is `Some(handle)`. Closing parent-owned stdin first gives the child an EOF/broken-pipe signal on its input side without forcing the output side closed while the child may still be writing.
2. Call `poll(proc)`.
3. If `poll(proc)` returns `Some(_)`, call `wait(proc)` before closing output handles. This should be immediate under the stable-status contract and makes the reap/final-status step explicit.
4. If `poll(proc)` returns `None`, call `terminate(proc)`.
5. Call `wait_timeout(proc, grace)`.
6. If `wait_timeout` returns `Some(_)`, call `wait(proc)` before closing output handles.
7. If `wait_timeout` returns `None`, call `kill(proc)` and then call `wait(proc)`.
8. Close returned `stdout` and `stderr` handles, in that order, when the corresponding `SpawnResult` field is `Some(handle)`.

The helper must not skip final `wait(proc)` just because `poll` or `wait_timeout` observed a final status. The low-level contract makes repeated `wait` idempotent, so this final call is the helper's durable reap boundary.

A zero or negative `grace` is allowed and delegates to the reviewed `wait_timeout` contract: it behaves as an immediate status check after `terminate`. If the process is still running, cleanup escalates to `kill` without an additional sleep.

The helper deliberately closes output handles after the direct child is reaped. Closing `stdout` or `stderr` while the child is still running can cause the child to observe `SIGPIPE`, `EPIPE`, or an equivalent backend write failure and can change the final status that the helper is trying to observe. The managed helper should prefer a clean process lifecycle over early output-handle release. Callers that need early output cancellation can still use the low-level APIs directly.

For C/libuv, this ordering means the implementation must keep the event loop making progress through process exit and handle-close callbacks rather than closing process or pipe handles prematurely. The helper itself should remain Bosatsu library code; the C/libuv runtime remains responsible for driving `uv_run(loop, UV_RUN_DEFAULT)` or the existing runtime-loop equivalent until pending process, timer, pipe, and close callbacks needed by `wait`, `wait_timeout`, `kill`, and `close` have completed. If a runtime-level shutdown path later closes remaining handles with `uv_walk`, that is a backend shutdown concern and not new public helper behavior.

## Stdio Ownership

`with_process` owns only the pipe handles returned in the `SpawnResult` fields. Those fields are `Some(handle)` only for literal `Stdio.Pipe` entries supplied to `spawn`.

Required stdio behavior:

- Close every returned pipe handle at most once during cleanup.
- Attempt `stdin` close before process stop/wait escalation.
- Attempt `stdout` and `stderr` closes after the direct child has been reaped or after final `wait` has produced an error.
- Attempt both output-handle closes even if the first output close fails.
- Do not close handles supplied through `Stdio.UseHandle`; those are not returned in `SpawnResult` and remain caller-owned.
- Do not attempt to drain `stdout` or `stderr` during cleanup.
- Do not flush `stdin` during cleanup; close is the cleanup boundary.

This makes `with_process` suitable for callers that do not need protocol-aware draining. Callers that need to read all output, coordinate stdin shutdown with child behavior, or close streams in a different order should use `spawn` and the low-level APIs directly.

## Error Composition

Bosatsu's current `IOError` type has no structured suppressed-error or multi-error variant. The helper therefore must use deterministic precedence rather than inventing a new error aggregation type in this issue.

Accepted precedence:

- If `spawn` fails, call `on_error(spawn_error)` and do not run cleanup.
- If `use` succeeds and cleanup succeeds, return the value produced by `use`.
- If `use` succeeds and cleanup observes one or more `IOError` values, call `on_error(first_cleanup_error)` using the cleanup phase order below.
- If `use` fails with the caller's error type `e`, run cleanup and preserve the original `use` failure, even if cleanup also observes `IOError` values.
- If `on_error` itself fails with `e`, that failure is the returned result for the spawn or cleanup error path that invoked it.

Cleanup phase order for choosing the first cleanup error:

1. `stdin` close error.
2. `poll` error.
3. `terminate` error.
4. `wait_timeout` error.
5. `kill` error.
6. final `wait` error.
7. `stdout` close error.
8. `stderr` close error.

Cleanup must continue after recoverable cleanup errors whenever continuing is meaningful. In particular, still attempt process stop/reap after a `stdin` close failure, and still attempt both output closes after final `wait` completes or fails. If a stop request fails but the final status is not yet recorded, cleanup should continue through the remaining escalation path and final `wait` rather than returning before the child is reaped.

The returned process exit code is not part of `with_process`'s result. Non-zero child exit remains a status observation, not an `IOError`. If a caller needs the final exit code as application data, the `use` block can call `wait`, `poll`, or `wait_timeout`, or the caller can use the low-level APIs directly.

The helper should not call `on_error` for non-zero child exit by itself. `on_error` is only for `IOError` values raised by helper-owned operations.

## Behavioral Invariants

The following properties must hold after the helper is implemented:

- `use` is invoked exactly once for each successful `spawn`.
- Cleanup is invoked exactly once after `use` completes, whether `use` succeeds or raises the caller's error type `e`.
- `on_error` is invoked for failed `spawn` and for cleanup `IOError` values after successful `use`; it is not invoked for ordinary non-zero child exit.
- A failed `use` has precedence over cleanup `IOError` values.
- Every returned `SpawnResult` pipe handle is closed at most once by the helper.
- Every returned `SpawnResult` pipe handle is attempted for close before the helper returns.
- Returned `stdin` is closed before stop/wait escalation.
- Returned `stdout` and `stderr` are closed only after the helper has attempted final `wait`.
- The helper never closes caller-owned `UseHandle` resources.
- If cleanup returns, the direct child has been observed by a final `wait` call or that final `wait` error is the cleanup error under the precedence rules.
- If the child is already recorded as exited before cleanup process-control begins, cleanup does not send `terminate` or `kill`; it still performs final `wait`.
- If the child is still running at cleanup time, cleanup attempts `terminate` before any `kill` attempt.
- `kill` is attempted only after `wait_timeout(proc, grace)` returns `None` or after cleanup must continue past a recoverable termination-stage error and the process still has no recorded final status.
- `wait_timeout(proc, grace)` returning `None` never changes the eventual final status observed by the helper's final `wait`.
- Low-level `terminate` and `kill` remain explicit direct-child operations and do not gain implicit stdio closure because this helper exists.
- The helper provides no process-tree guarantee. Descendants created by the direct child remain outside the contract.

## Testing Guidance

Property-check style tests should cover the lifecycle invariants that are independent of a single child command:

- Cleanup always runs after generated `use` outcomes: success, caller-domain failure, and early return-like branches.
- For generated caller error domains, `use: SpawnResult -> Prog[e, a]` failures are preserved while helper-owned `IOError` values are routed through `on_error`.
- For generated `on_error` behaviors, spawn and cleanup `IOError` paths return the value or failure produced by `on_error`.
- For generated `SpawnResult` stdio configurations, the helper attempts close exactly for returned `Some` handles and never for non-returned handles.
- For generated operation outcomes in a fake or test double model of `poll`, `terminate`, `wait_timeout`, `kill`, and `wait`, cleanup ordering is stable and error precedence is deterministic.
- For generated grace durations including negative, zero, small positive, and large positive values, cleanup delegates timeout behavior to `wait_timeout` and escalates only when the timeout result is `None`.
- For generated child timing states, the helper always converges on final `wait` before returning when the mocked low-level operations allow progress.
- For generated child/output-handle states, `stdout` and `stderr` close attempts occur after the final `wait` attempt, never before stop/wait escalation.

Narrow case-based tests remain the right fit for real process behavior and backend-sensitive edges:

- Normal `use` success with a naturally exiting child returns the `use` value and reaps the child.
- `use` failure returns the original caller-domain error while still closing returned handles and reaping/stopping the child.
- Spawn failure calls `on_error` and does not run cleanup.
- Cleanup `IOError` after successful `use` calls `on_error`.
- Already-exited children are not terminated or killed and still pass through final `wait`.
- A long-running child is terminated, then force-killed after a short grace duration if it remains running.
- Returned pipe handles are closed during cleanup; `stdin` is closed before stop/wait escalation, and `stdout`/`stderr` are closed after final wait. Subsequent reads/writes through those handles should fail with the existing closed-handle behavior where the test can observe it.
- `Stdio.UseHandle` inputs remain open after `with_process` returns.
- Cleanup close errors after successful `use` are routed through `on_error`; cleanup close errors after failed `use` do not replace the original `use` error.

Add Bosatsu-level coverage near the existing process tests in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` or a similarly focused process helper test program. Reuse the existing JVM/Python test flow and add C runtime coverage only where it verifies the low-level process behavior that the helper depends on. The helper itself should be tested as Bosatsu library code rather than by adding backend-specific runtime hooks.

Expected verification for implementation workers:

- Run `scripts/test_basic.sh` for the repository gate.
- Run `./test_python.sh` when changing Python-backed workspace process tests.
- Run the existing focused JVM/Scala process tests when changing shared process test programs.
- Run `make -C c_runtime test_out` if adding or adjusting C/libuv process regression coverage.

## Acceptance Criteria

- `Bosatsu/IO/Core` exports `with_process` with the accepted signature.
- The helper is implemented as Bosatsu library code using public `spawn`, `close`, `poll`, `terminate`, `wait_timeout`, `kill`, and `wait` APIs.
- The helper supports caller-chosen error type `e` through `on_error: IOError -> Prog[e, a]` and `use: SpawnResult -> Prog[e, a]`.
- The helper closes returned `stdin`, `stdout`, and `stderr` pipe handles during cleanup, with `stdin` before stop/wait escalation and `stdout`/`stderr` after final wait.
- The helper does not close `Stdio.UseHandle` resources or any handles not returned in `SpawnResult`.
- A still-running direct child is terminated, given the configured grace duration, force-killed if still running, and finally waited.
- Cleanup runs after both successful and failed `use` blocks.
- Error precedence and `on_error` routing are deterministic and match this document.
- The helper does not change the low-level stop/status contract or add process-tree behavior.
- Focused tests cover normal completion, user-code failure, already-exited children, stdio close behavior, escalation after grace timeout, and error precedence.
- Property-check style coverage or an equivalent modeled test covers cleanup ordering and invariants across generated lifecycle outcomes.

## Risks And Rollout Notes

The main behavioral risk is output backpressure. Because the helper does not drain `stdout` or `stderr`, a child that writes enough output to fill a pipe may fail to exit during the graceful wait and may require forceful cleanup. The helper still closes output handles after final wait, but callers that need complete output or protocol-aware draining must use low-level APIs and choose their own drain/close order.

Closing output handles before process exit was rejected for this helper. It can cause a child that writes during cleanup to observe `SIGPIPE`, `EPIPE`, or equivalent backend errors and can change the final status. The accepted contract closes `stdin` first, performs stop/wait escalation, and closes `stdout`/`stderr` only after final wait.

A second risk is indefinite cleanup if a backend cannot stop a child and final `wait` never completes. The helper's contract intentionally prioritizes not returning before reap. Tests should use bounded child programs and should not depend on process-tree cleanup to make progress.

Error suppression is another explicit tradeoff. Because the helper is polymorphic in the caller's error type and `IOError` has no multi-error shape, cleanup `IOError` values can be hidden when `use` already failed. This should be documented as part of the helper contract rather than solved by adding a new error type in this issue.

Roll out the helper after the low-level stop/status API is present on the target branch. Keep the initial implementation small and in `Bosatsu/IO/Core`; do not introduce new runtime externals, public process ids, process-tree controls, signal enums, or shell semantics. Downstream documentation can later describe `with_process` as the recommended default for simple process lifecycles, while preserving the low-level APIs for custom protocols.
