# Code Plan #2376

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2376` Add cross-backend process stop and cleanup regression coverage
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Add durable regression coverage for the portable process stop/status contract and the managed cleanup helper across the supported runtime paths. The final change should strengthen the shared JVM/Python Bosatsu process test program, mirror low-level contract gaps in the C/libuv runtime tests, and verify the branch with the focused C target plus the configured repository gate `scripts/test_basic.sh` within the 2400 second timeout.

## Current State

The merged dependencies have already added the public low-level process APIs (`StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`) and the higher-level `with_process` helper in `test_workspace/Bosatsu/IO/Core.bosatsu`. Shared JVM/Python process coverage currently flows through `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, which is loaded by `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` for JVM evaluation and by `test_python.sh` for Python transpile/evaluation. The C/libuv runtime has focused process tests in `c_runtime/test.c`, with `make -C c_runtime test_out` as the focused target. The repository-required gate is `scripts/test_basic.sh`, which runs the Scala CLI and core JVM test suites.

## Problem

Issue #2361's acceptance criteria are broader than isolated backend implementation tests. The current process coverage exercises many individual behaviors, but the regression suite should make the cross-backend contract explicit and harder to regress: direct terminate and kill, idempotent stop after recorded exit, stable final status across wait/poll/wait_timeout after stop, timeout non-consumption, poll before and after exit, low-level stdio ownership, and `with_process` owned-handle close/reap behavior. Without a small, intentionally mirrored set of tests in the shared JVM/Python harness and C runtime harness, a future backend change could preserve one path while breaking another without failing the required gate.

## Steps

1. [ ] `1` Strengthen shared JVM/Python contract coverage

Update `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` in place so the shared process regression program explicitly covers the full low-level and helper contract for both JVM evaluation and Python generation/evaluation. Keep child commands bounded and argv-based; prefer `python3 -c ...` and existing portable commands already used by the test program. Add small local test helpers only when they reduce duplication in the invariant checks, and keep the resulting program readable enough for failures to map back to one contract clause.

#### Invariants

- After any operation records a final child status, every later `wait`, `poll`, and `wait_timeout` observation returns the same normalized code.
- `wait_timeout` returning `None` never consumes or invalidates the eventual final status.
- `terminate` and `kill` return `AlreadyExited` after status has been recorded, and a stop request sent to a running child is followed by a stable non-zero final status.
- Low-level `terminate`, `kill`, `poll`, `wait_timeout`, and `wait` do not implicitly close returned stdio pipe handles.
- `with_process` closes only the `SpawnResult`-owned pipe handles, stops/reaps a still-running direct child, and preserves caller-domain error precedence.

#### Property Tests

- Add a compact property-style status-sequence check over a table of child specs and operation sequences, covering natural zero exit, natural non-zero exit, timeout-before-wait, terminate-before-wait, and kill-before-wait. Each case should assert that all final observations converge to one stable status.
- Add a property-style low-level stdio ownership case that performs one or more stop/status operations on a process with `Pipe` stdio and verifies the returned handles remain usable until the test explicitly closes or observes backend-natural EOF/broken-pipe behavior.

#### Assertion Tests

- Keep or add concrete assertions for `poll` returning `None` before a bounded child exits and `Some(code)` after status is recorded.
- Keep or add concrete assertions for zero, negative, tiny positive, and oversized `wait_timeout` durations without consuming the later `wait` result.
- Keep or add concrete assertions for `with_process` success, non-zero child exit not invoking `on_error`, caller failure precedence, already-exited cleanup, zero-grace kill escalation, returned pipe-handle closure, and caller-owned handle preservation.

2. [ ] `2` Mirror low-level gaps in C/libuv tests

Extend `c_runtime/test.c` near the existing IO/Core process tests so the C/libuv backend has focused coverage for the same low-level contract clauses that are not covered by the shared JVM/Python test program. Keep platform-sensitive tests inside the existing non-Windows guard or equivalent guards, and avoid widening runtime implementation scope unless a test exposes a correctness bug that is small enough to fix in this PR under the 1000 LoC heuristic.

#### Invariants

- C/libuv process status remains write-once and stable across repeated `wait`, `poll`, and `wait_timeout` observations.
- C/libuv timeout requests that return `None` leave the process waitable and observable by later status operations.
- C/libuv stop operations use the live process handle semantics already implemented and do not close or drain returned stdio handles.
- Already-recorded process exit is reported as `AlreadyExited` for both `terminate` and `kill`.

#### Property Tests

- Add a small table-driven C test helper for status stability after different first observations (`wait`, post-exit `poll`, post-exit `wait_timeout`) so repeated assertions are generated from one invariant rather than copied as one-off cases.
- If practical with the existing C harness, add a table-driven timeout/stop sequence helper that runs bounded children through timeout-then-wait, terminate-then-wait, and kill-then-wait sequences and checks final status stability.

#### Assertion Tests

- Add or strengthen concrete C assertions for direct `terminate` and direct `kill` followed by `wait` returning a non-zero stable status.
- Add or strengthen concrete C assertions for `poll` before exit and after recorded exit, including repeated observations.
- Add or strengthen concrete C assertions for `wait_timeout` timeout followed by final `wait`, including zero/non-positive timeout behavior.
- Add a focused C assertion that low-level stop/status operations do not implicitly close returned pipe handles, using existing C runtime handle helpers where available.

3. [ ] `3` Run focused and required verification

Run the smallest useful verification loop while developing, then finish with both the focused C runtime target and the repository-required test gate. Because the branch changes cross-backend coverage, also run the existing Python flow that executes `ProcessWaitMain` after transpilation.

#### Invariants

- The final branch remains shippable only if `scripts/test_basic.sh` passes within the configured 2400 second timeout.
- Focused backend tests should fail close to the changed coverage when a process contract regression is introduced.
- The test suite must not rely on unbounded sleeps, shell-only behavior in shared JVM/Python coverage, or platform assumptions outside guarded C tests.

#### Property Tests

- The property-style/table-driven coverage added in steps 1 and 2 must run as part of the normal shared JVM/Python and C test entry points, not as ad hoc manual checks.

#### Assertion Tests

- Run `make -C c_runtime test_out`.
- Run `./test_python.sh` to exercise Python generation/evaluation of `ProcessWaitMain`.
- Run `scripts/test_basic.sh` as the required PR gate.
