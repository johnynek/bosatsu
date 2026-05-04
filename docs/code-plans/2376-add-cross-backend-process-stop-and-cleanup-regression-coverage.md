# Code Plan #2376

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2376` Add cross-backend process stop and cleanup regression coverage
- Source design doc: `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md`
- Pending steps: `0`
- Completed steps: `3`
- Total steps: `3`

## Summary

Add durable regression coverage for the portable process stop/status contract and the managed cleanup helper across the supported runtime paths. The branch strengthens the shared JVM/Python Bosatsu process test program, mirrors low-level contract gaps in the C/libuv runtime tests, and now passes the focused C target, Python generation/evaluation flow, and the required `scripts/test_basic.sh` gate.

## Current State

The merged dependencies have already added the public low-level process APIs (`StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`) and the higher-level `with_process` helper in `test_workspace/Bosatsu/IO/Core.bosatsu`. This branch strengthens shared JVM/Python coverage in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`: post-recorded status checks actively assert stable `wait`, `poll`, `wait_timeout`, and already-exited stop behavior; timeout-before-wait coverage includes negative/zero/tiny durations; terminate/kill cases assert stable post-stop observations; low-level pipe ownership is checked after status operations; and the caller-owned `UseHandle` helper case actually passes a caller-owned handle. This round fixed directly coupled `IOError` typing mistakes exposed by verification in the new failure-path assertions, rebuilt stale compile-time embedded test workspace content with `sbt clean`, and completed final verification. C/libuv coverage in `c_runtime/test.c` includes reusable post-recorded status assertions and a focused low-level stdio ownership regression. Verification now passes with `make -C c_runtime test_out`, `./test_python.sh`, and `scripts/test_basic.sh`.

## Problem

Issue #2361's acceptance criteria are broader than isolated backend implementation tests. The regression suite should make the cross-backend contract explicit and harder to regress: direct terminate and kill, idempotent stop after recorded exit, stable final status across wait/poll/wait_timeout after stop, timeout non-consumption, poll before and after exit, low-level stdio ownership, and `with_process` owned-handle close/reap behavior. Shared JVM/Python coverage and C/libuv coverage now cover these clauses more directly, and the configured required test gate passes on the final branch state.

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

Edited `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`. Verification in step 3 exposed that new failure-path assertions were using raw `String` errors inside a `Prog[IOError, Bool]`; this round corrected those branches to use `InvalidArgument`, including the low-level missing-stdin branch and the `with_process_use_failure_case` caller/cleanup error assertions.

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

3. [x] `3` Run focused and required verification

Run the smallest useful verification loop while developing, then finish with both the focused C runtime target and the repository-required test gate. Because the branch changes cross-backend coverage, also run the existing Python flow that executes `ProcessWaitMain` after transpilation. This checkout may need `sbt cli/assembly` or the required gate to produce the `bosatsuj` assembly before `./test_python.sh` can run successfully.

#### Invariants

- The final branch remains shippable only if `scripts/test_basic.sh` passes within the configured 2400 second timeout.
- Focused backend tests should fail close to the changed coverage when a process contract regression is introduced.
- The test suite must not rely on unbounded sleeps, shell-only behavior in shared JVM/Python coverage, or platform assumptions outside guarded C tests.

#### Property Tests

- The property-style/table-driven coverage added in steps 1 and 2 runs as part of the normal shared JVM/Python and C test entry points, not as ad hoc manual checks.

#### Assertion Tests

- Ran `make -C c_runtime test_out`: passed/up-to-date.
- Ran `sbt -batch cli/assembly` to create the local CLI assembly required by `./test_python.sh`.
- Ran `./test_python.sh`: passed, including Python generation/evaluation of `ProcessWaitMain`.
- Ran `scripts/test_basic.sh` after `sbt clean`: passed with CLI tests `74/74` and core JVM tests `2118/2118` with `2` ignored.
- Ran `git diff --check`: passed.

#### Completion Notes

Focused C verification passed with `make -C c_runtime test_out`. Initial `./test_python.sh` failed because the checkout had no CLI assembly jar. The first required-gate run exposed raw string `raise_error` calls in the new `ProcessWaitMain.bosatsu` coverage; this round fixed those directly coupled type errors by using `InvalidArgument` values. Because `EvaluationTest` embeds Bosatsu workspace files at Scala compile time via `Predef.loadFileInCompile`, stale test output still showed the old source until `sbt clean` was run. Final verification passed: `scripts/test_basic.sh` passed with CLI tests `74/74` and core JVM tests `2118/2118` with `2` ignored; `sbt -batch cli/assembly` passed; `./test_python.sh` passed; `make -C c_runtime test_out` passed/up-to-date; and `git diff --check` passed.
