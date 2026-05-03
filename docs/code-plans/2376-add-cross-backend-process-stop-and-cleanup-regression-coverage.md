# Code Plan #2376

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2376` Add cross-backend process stop and cleanup regression coverage
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `2`
- Completed steps: `2`
- Total steps: `4`

## Summary

Add durable regression coverage for the portable process stop/status contract and the managed `with_process` cleanup helper across the supported backend paths. This round completed the shared JVM/Python stop/status strengthening in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`; the remaining work is the focused C/libuv stdio ownership assertion and final verification gate.

## Current State

The direct dependencies for issue #2376 are present on `main`: the process stop/status contract exists in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, the low-level `terminate`, `kill`, `poll`, and `wait_timeout` APIs are wired through the JVM, Python test runtime, and C/libuv runtime, and `Bosatsu/IO/Core.with_process` implements managed process cleanup in Bosatsu library code. The audit for step-1 confirmed the smallest useful test surfaces: shared JVM/Python behavior flows through `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, JVM execution is anchored by `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`, Python execution is anchored by `test_python.sh` with externals in `test_workspace/ProgExt.py` and `test_workspace/Prog.bosatsu_externals`, and C/libuv-specific process assertions live in `c_runtime/test.c`. Step-2 replaced the inert `stable_wait_case` observation branches with real `wait`, `poll`, `wait_timeout`, `terminate`, and `kill` observations after a recorded final status, and strengthened `terminate_case` and `kill_case` so stopped children remain observable through `wait`, `poll`, and `wait_timeout`. `ProcessWaitMain.bosatsu` still also covers repeated wait, poll/timeout non-consumption, already-exited stop, and several `with_process` cleanup cases; `c_runtime/test.c` already covers focused C poll, timeout, terminate/kill, already-exited stop, and GC rooting cases.

## Problem

Process lifecycle behavior can regress independently in each backend because process handles, timeout waits, final-status caching, stop semantics, and stdio ownership are implemented separately. The shared JVM/Python status-stability gap from the inert `stable_wait_case` branches is now covered, including post-wait `poll` and `wait_timeout` observations and already-exited stop idempotence. Remaining coverage risk for this issue is concentrated in the C/libuv low-level stdio ownership assertion after stop requests, plus completing focused JVM/Python and required-gate verification so backend regressions do not slip through.

## Steps

1. [x] `step-1` Audit Existing Process Test Surfaces

Review the contract document, `coding_style.md`, and current process tests to identify the smallest set of existing files to extend. Keep shared behavior in the existing Bosatsu process program and evaluator test path, keep Python runtime assertions in the existing Python external flow, and keep C/libuv-specific handle/status checks in `c_runtime/test.c`.

#### Invariants

- The final PR reuses existing verified test surfaces instead of adding a parallel process test harness.
- Every planned child process is bounded, portable, and uses argv semantics rather than unguarded shell assumptions where the target surface supports it.
- Tests distinguish low-level process APIs, which do not own returned stdio handles, from `with_process`, which performs managed cleanup for helper-owned handles.
- The required final gate remains `scripts/test_basic.sh` with the configured 2400 second timeout.

#### Property Tests

- None recorded.

#### Assertion Tests

- Confirmed the implementation should touch only the existing process coverage files needed for this regression job.
- Recorded focused verification commands in completion notes, including `make -C c_runtime test_out` if `c_runtime/test.c` changes and `scripts/test_basic.sh` before PR readiness.

#### Completion Notes

Audited `coding_style.md`, the contract in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`, `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, `test_workspace/ProgExt.py`, `test_workspace/Prog.bosatsu_externals`, `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`, `test_python.sh`, and `c_runtime/test.c`. The next implementation should keep shared JVM/Python regressions in `ProcessWaitMain.bosatsu` and its existing evaluator/Python runners, and keep C/libuv-only status/handle ownership checks in `c_runtime/test.c`. No new harness is needed.

2. [x] `step-2` Strengthen Shared Stop/Status Regressions

Extend the existing shared Bosatsu process scenarios in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` so JVM and Python execution exercise the portable stop/status contract. Keep the existing `exited_stop_case`, `poll_timeout_case`, `terminate_case`, and `kill_case`, but replace or strengthen the inert observation branches in `stable_wait_case` so completed processes are actually observed through `wait`, `poll`, and `wait_timeout` sequences. Cover direct termination, force kill, stop after already-recorded exit, stable final status after stop, `poll` before and after exit, and `wait_timeout` returning `None` without consuming a later final `wait`.

#### Invariants

- `wait`, `poll`, and `wait_timeout` all observe one stable normalized final status once it has been recorded.
- `wait_timeout` returning `None` never consumes or invalidates the eventual process status.
- `terminate` and `kill` on an already-recorded exited child return the documented already-exited result without changing final status.
- Stop requests never make a later `wait`, `poll`, or `wait_timeout` hang or lose the recorded status.
- Platform differences are tolerated only where the contract allows them; portable contract violations must fail the tests.

#### Property Tests

- Express status stability as observation sequences over bounded children: after `wait`, `poll`, or `wait_timeout` records a final status, repeated observations must report the same code.
- Express timeout non-consumption as one or more short timeout observations followed by natural exit or stop and a final `wait` that still reports the real final status.
- Express stop idempotence with both orderings: `wait` then stop and stop then `wait`, preserving the recorded final status.

#### Assertion Tests

- A running child can be terminated, then waited or polled without losing final status.
- A running child can be force-killed where supported by the backend surface, then waited without hanging.
- `poll` returns no status before a known-running child exits and returns the expected status after exit is recorded.
- Zero or very short `wait_timeout` returns no status for a known-running child, and a later `wait` returns the expected final code.
- Stopping an already-exited child reports `AlreadyExited` or the documented equivalent in the existing Bosatsu API.

#### Completion Notes

Updated `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` only. Removed the dead `if False` observation scaffolding and unused helpers from `stable_wait_case`; the case now records final status with `wait`, then asserts `poll`, non-blocking `wait_timeout`, `terminate`, `kill`, and a second `wait` all preserve the same final status for both `true` and `false`. Strengthened `terminate_case` and `kill_case` so after a stop request and final `wait`, both `poll` and non-blocking `wait_timeout` observe the same non-zero stopped-child status. Verification attempted: `./test_python.sh` failed immediately because no `cli/target/.../bosatsu-cli-assembly-*.jar` exists in this checkout; a fallback attempt with `cli/bosatsu.jar` used the older CLI shape and failed parsing current workspace syntax, so it was not a valid regression result. The focused JVM `sbt 'coreJVM/testOnly dev.bosatsu.EvaluationTest -- "process wait is stable and idempotent in JVM evaluation"'` began initial checkout compilation but did not produce a completed test result during this worker round; step-4 remains pending for authoritative verification.

3. [ ] `step-3` Cover Stdio Ownership And Managed Cleanup

Add the backend-specific regression assertions needed for ownership and cleanup behavior. Use `c_runtime/test.c` for the missing C/libuv low-level stdio ownership assertion: spawn with piped stdio, issue a low-level stop request, then explicitly close returned handles to prove stop did not take ownership. Keep the existing shared Bosatsu/Python/JVM `with_process` cases in `ProcessWaitMain.bosatsu` for helper-owned returned handle closure, caller-owned `UseHandle` ownership, running-child escalation, already-exited cleanup, non-zero child exit, and caller failure precedence; strengthen them only if step-2 edits expose an obvious gap.

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

Run focused tests for the touched backend surfaces, then run the repository-required gate before the branch is considered reviewable. If `ProcessWaitMain.bosatsu` changes, run the JVM evaluator test that loads it and the Python flow that imports `Bosatsu.IO.ProcessWaitMain`; if `c_runtime/test.c` changes, run the focused C runtime target. Keep new process tests bounded so they fit within the configured `scripts/test_basic.sh` timeout of 2400 seconds. Before the Python flow can be used in this checkout, build the expected CLI assembly jar with `sbt cli/assembly` or otherwise restore `cli/target/scala-*/bosatsu-cli-assembly-*.jar` so `./bosatsuj` and `./test_python.sh` exercise the current compiler/runtime rather than the stale `cli/bosatsu.jar`.

#### Invariants

- The branch is not PR-ready until `scripts/test_basic.sh` passes.
- Focused backend tests should pass before relying on the full gate.
- Any platform guard or skipped process case has an explicit reason so the regression intent remains clear.
- No long-running child process can outlive the test flow without an explicit cleanup path.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run `make -C c_runtime test_out` after changing `c_runtime/test.c`.
- Run the focused Scala/JVM evaluator test, for example the `EvaluationTest` case that loads `Bosatsu/IO/ProcessWaitMain`, after changing shared process coverage.
- Run the existing Python process flow from `test_python.sh` after changing `ProcessWaitMain.bosatsu`, Python externals, or Python-observed process cases.
- Run `scripts/test_basic.sh` with the configured 2400 second timeout before completion.
