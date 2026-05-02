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

Defines the reviewed Bosatsu-level managed process helper contract, including the public `with_process` shape in `Bosatsu/IO/Core`, cleanup ordering, error precedence, stdio ownership, process reap invariants, test guidance, risks, and rollout notes.

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
- Run cleanup after user code succeeds or raises `IOError`.
- Close all stdio pipe handles returned in `SpawnResult` during cleanup.
- If the direct child is still running, request normal termination, wait for a grace duration, then request forceful termination if needed.
- Always wait for the direct child before cleanup returns, so the child is reaped and its final status is recorded.
- Preserve the user result on success, or preserve the user `IOError` when user code failed.

## Public API Shape

Add the helper to `test_workspace/Bosatsu/IO/Core.bosatsu` and export it beside `spawn`, `wait`, `terminate`, `kill`, `poll`, and `wait_timeout`.

Accepted public shape:

    def with_process[a](cmd: String, args: List[String], stdio: StdioConfig, grace: Duration, use: SpawnResult -> Prog[IOError, a]) -> Prog[IOError, a]

Accepted naming and placement:

- The helper name is `with_process`, matching existing snake_case Bosatsu library style.
- The helper lives in `Bosatsu/IO/Core` because it composes `spawn`, `SpawnResult`, `StdioConfig`, `Duration`, stdio `Handle` cleanup, and process stop/status APIs from that module.
- It is a normal Bosatsu definition, not an `external def`.
- It should be included in the module export list.

Accepted argument order:

- `cmd`, `args`, and `stdio` come first, in the same order as `spawn`.
- `grace` comes after `stdio`, because it configures cleanup rather than spawn itself.
- `use` is last so call sites can pass the lifecycle body in the final position.

The `use` block has the exact type `SpawnResult -> Prog[IOError, a]`. This helper is intentionally not error-polymorphic over an arbitrary user error type because cleanup itself uses `IOError`-typed operations: `close`, `terminate`, `kill`, `wait_timeout`, and `wait`.

## Cleanup Ordering

After `spawn` succeeds, cleanup is mandatory whether `use` succeeds or fails. If `spawn` itself fails, no cleanup runs because no `SpawnResult` exists.

Required cleanup order:

1. Close returned `stdin`, `stdout`, and `stderr` handles, in that order, when the corresponding `SpawnResult` field is `Some(handle)`.
2. Call `poll(proc)`.
3. If `poll(proc)` returns `Some(_)`, call `wait(proc)` before returning from cleanup. This should be immediate under the stable-status contract and makes the reap/final-status step explicit.
4. If `poll(proc)` returns `None`, call `terminate(proc)`.
5. Call `wait_timeout(proc, grace)`.
6. If `wait_timeout` returns `Some(_)`, call `wait(proc)` before returning from cleanup.
7. If `wait_timeout` returns `None`, call `kill(proc)` and then call `wait(proc)`.

The helper must not skip final `wait(proc)` just because `poll` or `wait_timeout` observed a final status. The low-level contract makes repeated `wait` idempotent, so this final call is the helper's durable reap boundary.

A zero or negative `grace` is allowed and delegates to the reviewed `wait_timeout` contract: it behaves as an immediate status check after `terminate`. If the process is still running, cleanup escalates to `kill` without an additional sleep.

## Stdio Ownership

`with_process` owns only the pipe handles returned in the `SpawnResult` fields. Those fields are `Some(handle)` only for literal `Stdio.Pipe` entries supplied to `spawn`.

Required stdio behavior:

- Close every returned pipe handle at most once during cleanup.
- Attempt all returned-handle closes even if an earlier close fails.
- Do not close handles supplied through `Stdio.UseHandle`; those are not returned in `SpawnResult` and remain caller-owned.
- Do not attempt to drain `stdout` or `stderr` during cleanup.
- Do not flush `stdin` during cleanup; close is the cleanup boundary.

This makes `with_process` suitable for callers that do not need protocol-aware draining. Callers that need to read all output, coordinate stdin shutdown with child behavior, or close streams in a different order should use `spawn` and the low-level APIs directly.

## Error Composition

Bosatsu's current `IOError` type has no structured suppressed-error or multi-error variant. The helper therefore must use deterministic precedence rather than inventing a new error aggregation type in this issue.

Accepted precedence:

- If `spawn` fails, return the `spawn` error and do not run cleanup.
- If `use` succeeds and cleanup succeeds, return the value produced by `use`.
- If `use` succeeds and cleanup observes one or more errors, return the first cleanup error by cleanup phase.
- If `use` fails, run cleanup and return the original `use` error, even if cleanup also observes errors.

Cleanup phase order for choosing the first cleanup error:

1. `stdin` close error.
2. `stdout` close error.
3. `stderr` close error.
4. `poll` error.
5. `terminate` error.
6. `wait_timeout` error.
7. `kill` error.
8. final `wait` error.

Cleanup must continue after recoverable cleanup errors whenever continuing is meaningful. In particular, close all returned handles even if an earlier close failed, and still attempt process stop/reap after close failures. If a stop request fails but the final status is not yet recorded, cleanup should continue through the remaining escalation path and final `wait` rather than returning before the child is reaped.

The returned process exit code is not part of `with_process`'s result. Non-zero child exit remains a status observation, not an `IOError`. If a caller needs the final exit code as application data, the `use` block can call `wait`, `poll`, or `wait_timeout`, or the caller can use the low-level APIs directly.

## Behavioral Invariants

The following properties must hold after the helper is implemented:

- `use` is invoked exactly once for each successful `spawn`.
- Cleanup is invoked exactly once after `use` completes, whether `use` succeeds or raises `IOError`.
- Every returned `SpawnResult` pipe handle is closed at most once by the helper.
- Every returned `SpawnResult` pipe handle is attempted for close before the helper returns.
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

- Cleanup always runs after generated `use` outcomes: success, raised `IOError`, and early return-like branches.
- For generated `SpawnResult` stdio configurations, the helper attempts close exactly for returned `Some` handles and never for non-returned handles.
- For generated operation outcomes in a fake or test double model of `poll`, `terminate`, `wait_timeout`, `kill`, and `wait`, cleanup ordering is stable and error precedence is deterministic.
- For generated grace durations including negative, zero, small positive, and large positive values, cleanup delegates timeout behavior to `wait_timeout` and escalates only when the timeout result is `None`.
- For generated child timing states, the helper always converges on final `wait` before returning when the mocked low-level operations allow progress.

Narrow case-based tests remain the right fit for real process behavior and backend-sensitive edges:

- Normal `use` success with a naturally exiting child returns the `use` value and reaps the child.
- `use` failure returns the original `IOError` while still closing returned handles and reaping/stopping the child.
- Already-exited children are not terminated or killed and still pass through final `wait`.
- A long-running child is terminated, then force-killed after a short grace duration if it remains running.
- Returned pipe handles are closed during cleanup; subsequent reads/writes through those handles should fail with the existing closed-handle behavior where the test can observe it.
- `Stdio.UseHandle` inputs remain open after `with_process` returns.
- Cleanup close errors after successful `use` become the returned error; cleanup close errors after failed `use` do not replace the original `use` error.

Add Bosatsu-level coverage near the existing process tests in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` or a similarly focused process helper test program. Reuse the existing JVM/Python test flow and add C runtime coverage only where it verifies the low-level process behavior that the helper depends on. The helper itself should be tested as Bosatsu library code rather than by adding backend-specific runtime hooks.

Expected verification for implementation workers:

- Run `scripts/test_basic.sh` for the repository gate.
- Run `./test_python.sh` when changing Python-backed workspace process tests.
- Run the existing focused JVM/Scala process tests when changing shared process test programs.
- Run `make -C c_runtime test_out` if adding or adjusting C/libuv process regression coverage.

## Acceptance Criteria

- `Bosatsu/IO/Core` exports `with_process` with the accepted signature.
- The helper is implemented as Bosatsu library code using public `spawn`, `close`, `poll`, `terminate`, `wait_timeout`, `kill`, and `wait` APIs.
- The helper closes returned `stdin`, `stdout`, and `stderr` pipe handles during cleanup.
- The helper does not close `Stdio.UseHandle` resources or any handles not returned in `SpawnResult`.
- A still-running direct child is terminated, given the configured grace duration, force-killed if still running, and finally waited.
- Cleanup runs after both successful and failed `use` blocks.
- Error precedence is deterministic and matches this document.
- The helper does not change the low-level stop/status contract or add process-tree behavior.
- Focused tests cover normal completion, user-code failure, already-exited children, stdio close behavior, escalation after grace timeout, and error precedence.
- Property-check style coverage or an equivalent modeled test covers cleanup ordering and invariants across generated lifecycle outcomes.

## Risks And Rollout Notes

The main behavioral risk is that closing stdout or stderr before waiting can cause a child that writes during cleanup to observe a broken pipe. That is acceptable for this helper because it is the managed cleanup path for callers that do not need output draining. Callers that need complete output must use low-level APIs and choose their own drain/close order.

A second risk is indefinite cleanup if a backend cannot stop a child and final `wait` never completes. The helper's contract intentionally prioritizes not returning before reap. Tests should use bounded child programs and should not depend on process-tree cleanup to make progress.

Error suppression is another explicit tradeoff. Because `IOError` has no multi-error shape, cleanup errors can be hidden when `use` already failed. This should be documented as part of the helper contract rather than solved by adding a new error type in this issue.

Roll out the helper after the low-level stop/status API is present on the target branch. Keep the initial implementation small and in `Bosatsu/IO/Core`; do not introduce new runtime externals, public process ids, process-tree controls, signal enums, or shell semantics. Downstream documentation can later describe `with_process` as the recommended default for simple process lifecycles, while preserving the low-level APIs for custom protocols.
