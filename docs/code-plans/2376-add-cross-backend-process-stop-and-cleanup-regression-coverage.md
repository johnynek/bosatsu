# Code Plan #2376

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2376` Add cross-backend process stop and cleanup regression coverage
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `1`
- Completed steps: `2`
- Total steps: `3`

## Summary

Add durable regression coverage for the portable process stop/status contract and the managed cleanup helper across the supported runtime paths. The branch now strengthens the shared JVM/Python Bosatsu process test program and mirrors low-level contract gaps in the C/libuv runtime tests; final verification with the repository gate `scripts/test_basic.sh` remains pending within the 2400 second timeout.

## Current State

The merged dependencies have already added the public low-level process APIs (`StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`) and the higher-level `with_process` helper in `test_workspace/Bosatsu/IO/Core.bosatsu`. Earlier work strengthened shared JVM/Python coverage in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`: post-recorded status checks now actively assert stable `wait`, `poll`, `wait_timeout`, and already-exited stop behavior; timeout-before-wait coverage includes negative/zero/tiny durations; terminate/kill cases assert stable post-stop observations; low-level pipe ownership is checked after status operations; and the caller-owned `UseHandle` helper case now actually passes a caller-owned handle. This round strengthened C/libuv coverage in `c_runtime/test.c` with reusable post-recorded status assertions and a focused low-level stdio ownership regression. Focused C verification passed with `make -C c_runtime test_out`; final repository verification remains pending.

## Problem

Issue #2361's acceptance criteria are broader than isolated backend implementation tests. The regression suite should make the cross-backend contract explicit and harder to regress: direct terminate and kill, idempotent stop after recorded exit, stable final status across wait/poll/wait_timeout after stop, timeout non-consumption, poll before and after exit, low-level stdio ownership, and `with_process` owned-handle close/reap behavior. Shared JVM/Python coverage and C/libuv coverage now cover these clauses more directly, but the configured required test gate still needs to pass before PR submission.

## Steps

1. [x] `1` Strengthen shared JVM/Python contract coverage

Update `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` in place so the shared process regression program explicitly covers the full low-level and helper contract for both JVM evaluation and Python generation/evaluation. Keep child commands bounded and argv-based; prefer `python3 -c ...` and existing portable commands already used by the test program. Add small local test helpers only when they reduce duplication in the invariant checks, and keep the resulting program readable enough for failures to map back to one contract clause.

#### Invariants

- After any operation records a final child status, every later `wait`, `poll`, and `wait_timeout` observation returns the same normalized code.
- `wait_timeout` returning `None` never consumes or invalidates the eventual final status.
- `terminate` and `kill` return `AlreadyExited` after status has been recorded, and a stop request sent to a running child is followed by a stable non-zero final status.
- Low-level `terminate`, `kill`, `poll`, `wait_timeout`, and `wait` do not implicitly close returned stdio pipe handles.
- `with_process` closes only the `SpawnResult`-owned pipe handles, stops/reaps a still-running direct child, and preserves caller-domain error precedence.

#### Property Tests

- Added a compact shared helper, `post_recorded_status_is_stable`, that is reused across natural zero exit, natural non-zero exit, timeout-before-wait, terminate-before-wait, and kill-before-wait cases so each case asserts convergence to one stable final status.
- Added `low_level_pipe_ownership_case`, which performs low-level status operations on a process with `Pipe` stdio and verifies the returned stdin pipe remains usable until the test explicitly closes it.

#### Assertion Tests

- Strengthened assertions for `poll` returning `None` before a bounded child exits and stable `Some(code)` after status is recorded.
- Strengthened assertions for negative, zero, tiny positive, and oversized `wait_timeout` durations without consuming the later `wait` result.
- Kept and corrected helper assertions for `with_process` success, non-zero child exit not invoking `on_error`, caller-domain failure precedence, already-exited cleanup, zero-grace kill escalation, returned pipe-handle closure, and caller-owned handle preservation.

#### Completion Notes

Edited `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` only. `git diff --check` passed. Attempted focused JVM verification with `sbt -batch "coreJVM/testOnly dev.bosatsu.EvaluationTest -- -z process wait"`; the command reached project load/compile output but did not return a final test result through the tool session. Attempted `./test_python.sh`; it failed immediately because this checkout has no CLI assembly jar (`bosatsuj: no assembly jar found; run sbt cli/assembly first`). Full verification remains in pending step 3.

2. [x] `2` Mirror low-level gaps in C/libuv tests

Extend `c_runtime/test.c` near the existing IO/Core process tests so the C/libuv backend has focused coverage for the same low-level contract clauses that are not covered by the shared JVM/Python test program. Keep platform-sensitive tests inside the existing non-Windows guard or equivalent guards, and avoid widening runtime implementation scope unless a test exposes a correctness bug that is small enough to fix in this PR under the 1000 LoC heuristic.

#### Invariants

- C/libuv process status remains write-once and stable across repeated `wait`, `poll`, and `wait_timeout` observations.
- C/libuv timeout requests that return `None` leave the process waitable and observable by later status operations.
- C/libuv stop operations use the live process handle semantics already implemented and do not close or drain returned stdio handles.
- Already-recorded process exit is reported as `AlreadyExited` for both `terminate` and `kill`.

#### Property Tests

- Added reusable C post-recorded status assertion helpers that check repeated `poll`, zero-duration `wait_timeout`, and repeated `wait` against one cached final status after different first observations.
- Reused the stable-status helper from natural exit, timeout-before-wait, terminate-before-wait, kill-before-wait, and already-exited stop paths so the invariant is asserted consistently rather than as one-off checks.

#### Assertion Tests

- Strengthened C assertions for direct `terminate` and direct `kill` followed by `wait` returning a non-zero status, then stable `poll`, `wait_timeout`, and repeated `wait` observations.
- Strengthened C assertions for `poll` before exit and after recorded exit, including repeated post-exit poll observations.
- Strengthened C assertions for `wait_timeout` timeout followed by final `wait`, including zero/non-positive timeout behavior and stable post-exit timeout observations.
- Added a focused POSIX-guarded C assertion that low-level `poll`, zero-duration `wait_timeout`, and `terminate` do not implicitly close or drain returned pipe handles by writing through a caller-owned stdin pipe after those operations.

#### Completion Notes

Edited `c_runtime/test.c` only. The first focused run exposed a test wiring bug in the new already-exited closure slot count, which was fixed in the same file. `git diff --check` passed. `make -C c_runtime test_out` passed.

3. [ ] `3` Run focused and required verification

Run the smallest useful verification loop while developing, then finish with both the focused C runtime target and the repository-required test gate. Because the branch changes cross-backend coverage, also run the existing Python flow that executes `ProcessWaitMain` after transpilation. This checkout may need `sbt cli/assembly` or the required gate to produce the `bosatsuj` assembly before `./test_python.sh` can run successfully.

#### Invariants

- The final branch remains shippable only if `scripts/test_basic.sh` passes within the configured 2400 second timeout.
- Focused backend tests should fail close to the changed coverage when a process contract regression is introduced.
- The test suite must not rely on unbounded sleeps, shell-only behavior in shared JVM/Python coverage, or platform assumptions outside guarded C tests.

#### Property Tests

- The property-style/table-driven coverage added in steps 1 and 2 must run as part of the normal shared JVM/Python and C test entry points, not as ad hoc manual checks.

#### Assertion Tests

- Run `make -C c_runtime test_out` as a final focused C check, even though it passed during step 2.
- Run `./test_python.sh` to exercise Python generation/evaluation of `ProcessWaitMain` after the CLI assembly exists.
- Run `scripts/test_basic.sh` as the required PR gate.
