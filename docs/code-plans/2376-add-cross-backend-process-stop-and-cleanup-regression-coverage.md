# Code Plan #2376

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2376` Add cross-backend process stop and cleanup regression coverage
- Pending steps: `4`
- Completed steps: `0`
- Total steps: `4`

## Summary

Add durable regression coverage for the portable process stop/status contract and the managed `with_process` cleanup helper across the supported backend paths. The intended final change should strengthen existing process tests rather than introduce a new harness, then verify the focused C runtime target and the required repository gate `scripts/test_basic.sh`.

## Current State

The direct dependencies for issue #2376 are present on `main`: the process stop/status contract exists in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, the low-level `terminate`, `kill`, `poll`, and `wait_timeout` APIs are wired through the JVM, Python test runtime, and C/libuv runtime, and `Bosatsu/IO/Core.with_process` implements managed process cleanup in Bosatsu library code. Current process coverage already lives in the expected surfaces: shared Bosatsu process scenarios in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, Python externals in `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`, JVM execution coverage through `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`, and focused C runtime coverage in `c_runtime/test.c`. The repository guidance in `coding_style.md` favors existing test entry points, strict Scala compilation behavior, focused tests while iterating, and expanding coverage when C/Python/runtime paths are touched.

## Problem

Process lifecycle behavior can regress independently in each backend because process handles, timeout waits, final-status caching, stop semantics, and stdio ownership are implemented separately. Existing implementation-local tests cover many cases, but issue #2376 needs the acceptance behavior to be anchored as cross-backend regression coverage: direct termination and force kill, stable status after stop, timeout non-consumption, idempotent stop after exit, poll before and after exit, no implicit low-level stdio closure, and helper-managed stdio close/reap behavior. Without this coverage, a backend could accidentally consume status during `poll` or `wait_timeout`, close caller-owned pipes during `terminate`/`kill`, fail to reap a stopped child, or break `with_process` cleanup while still passing narrower tests.

## Steps

1. [ ] `step-1` Audit Existing Process Test Surfaces

Review the contract document, `coding_style.md`, and current process tests to identify the smallest set of existing files to extend. Keep shared behavior in the existing Bosatsu process program and evaluator test path, keep Python runtime assertions in the existing Python external flow, and keep C/libuv-specific handle/status checks in `c_runtime/test.c`.

#### Invariants

- The final PR reuses existing verified test surfaces instead of adding a parallel process test harness.
- Every planned child process is bounded, portable, and uses argv semantics rather than unguarded shell assumptions.
- Tests distinguish low-level process APIs, which do not own returned stdio handles, from `with_process`, which performs managed cleanup for helper-owned handles.
- The required final gate remains `scripts/test_basic.sh` with the configured 2400 second timeout.

#### Property Tests

- None recorded.

#### Assertion Tests

- Confirm the implementation touches only the existing process coverage files needed for this regression job.
- Record focused verification commands in completion notes, including `make -C c_runtime test_out` if `c_runtime/test.c` changes and `scripts/test_basic.sh` before PR readiness.

2. [ ] `step-2` Strengthen Shared Stop/Status Regressions

Extend the existing shared Bosatsu process scenarios so normal runtime execution exercises the portable stop/status contract. Cover direct termination, force kill, stop after already-recorded exit, stable final status after stop, `poll` before and after exit, and `wait_timeout` returning `None` without consuming a later final `wait`.

#### Invariants

- `wait`, `poll`, and `wait_timeout` all observe one stable normalized final status once it has been recorded.
- `wait_timeout` returning `None` never consumes or invalidates the eventual process status.
- `terminate` and `kill` on an already-recorded exited child return the documented already-exited result without changing final status.
- Stop requests never make a later `wait`, `poll`, or `wait_timeout` hang or lose the recorded status.
- Platform differences are tolerated only where the contract allows them; portable contract violations must fail the tests.

#### Property Tests

- Express status stability as observation sequences over a bounded child: after any operation records a final status, repeated `wait`, `poll`, and `wait_timeout` observations must report the same code.
- Express timeout non-consumption as a sequence of one or more short timeout observations followed by natural exit or stop and a final `wait` that still reports the real final status.
- Express stop idempotence with both orderings: `wait` then stop and stop then `wait`, preserving the recorded final status.

#### Assertion Tests

- A running child can be terminated, then waited or polled without losing final status.
- A running child can be force-killed where supported by the backend surface, then waited without hanging.
- `poll` returns no status before a known-running child exits and returns the expected status after exit is recorded.
- Zero or very short `wait_timeout` returns no status for a known-running child, and a later `wait` returns the expected final code.
- Stopping an already-exited child reports `AlreadyExited` or the documented equivalent in the existing Bosatsu API.

3. [ ] `step-3` Cover Stdio Ownership And Managed Cleanup

Add the backend-specific regression assertions needed for ownership and cleanup behavior. Use `c_runtime/test.c` for C/libuv low-level stdio ownership and status/timeout edges that are only directly observable there. Use the existing shared Bosatsu/Python/JVM process flow for `with_process` behavior: returned pipe handles are closed during cleanup, running children are stopped and reaped, and caller-domain failure precedence is preserved.

#### Invariants

- Low-level `terminate`, `kill`, `poll`, `wait_timeout`, and `wait` do not implicitly close or drain returned `SpawnResult` stdio handles.
- `with_process` closes helper-owned returned stdio pipe handles during cleanup.
- `with_process` checks process state, stops still-running direct children using the grace/escalation path, and makes a final wait/reap attempt before returning.
- Caller-domain failures from the user callback retain their documented precedence over cleanup details.
- Backend-specific assertions stay in the backend surface that can directly observe the behavior.

#### Property Tests

- Treat helper cleanup as a resource-lifetime invariant: for normal callback success, callback failure, already-exited child, and still-running child cleanup, helper-owned handles are closed and the direct child is reaped or observed as already exited before the helper returns.
- Treat low-level stdio ownership as an invariant over stop/status operation sequences: returned pipe handles remain caller-owned until explicit caller closure or ordinary OS EOF/broken-pipe behavior.

#### Assertion Tests

- C runtime: after a low-level stop request on a child with piped stdio, the test closes the returned handles explicitly, proving stop did not take ownership of them.
- C runtime: keep or strengthen focused assertions for poll before/after exit, timeout followed by wait, terminate/kill followed by wait, and already-exited stop behavior.
- Shared/Python/JVM flow: `with_process` closes returned pipe handles owned by `SpawnResult` during cleanup.
- Shared/Python/JVM flow: `with_process` reaps or observes completion for normal success, non-zero child exit, caller failure, already-exited child, and zero-grace stop escalation cases.

4. [ ] `step-4` Verify Focused Targets And Required Gate

Run focused tests for the touched backend surfaces, then run the repository-required gate before the branch is considered reviewable. Keep new process tests bounded so they fit within the configured `scripts/test_basic.sh` timeout of 2400 seconds.

#### Invariants

- The branch is not PR-ready until `scripts/test_basic.sh` passes.
- Focused backend tests should pass before relying on the full gate.
- Any platform guard or skipped process case has an explicit reason so the regression intent remains clear.
- No long-running child process can outlive the test flow without an explicit cleanup path.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run `make -C c_runtime test_out` after changing `c_runtime/test.c`.
- Run the relevant Scala/JVM focused test target if shared evaluator tests or `ProcessWaitMain` coverage changes.
- Run the existing Python generation/evaluation flow if Python externals or Python-observed process cases change.
- Run `scripts/test_basic.sh` with the configured 2400 second timeout before completion.
