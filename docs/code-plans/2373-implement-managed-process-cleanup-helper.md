# Code Plan #2373

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2373` Implement managed process cleanup helper
- Pending steps: `3`
- Completed steps: `0`
- Total steps: `3`

## Summary

Add the public `with_process` helper to `Bosatsu/IO/Core` as ordinary Bosatsu library code. The helper will wrap `spawn`, invoke caller code exactly once after a successful spawn, then run deterministic cleanup that closes returned stdio pipe handles, terminates or kills still-running direct children after the configured grace duration, and always reaches a final `wait` attempt before returning from cleanup. The implementation stays on top of the public low-level APIs from issue #2369 and preserves the error-polymorphic contract from the accepted helper design document.

## Current State

`test_workspace/Bosatsu/IO/Core.bosatsu` currently exports the low-level process surface: `spawn`, `wait`, `terminate`, `kill`, `poll`, and `wait_timeout`, plus `Handle` and `close`. `SpawnResult` carries optional returned pipe handles for `stdin`, `stdout`, and `stderr`. Existing process coverage lives in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, with JVM evaluation coverage in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` and Python coverage through `test_python.sh`. The accepted helper contract is recorded in `docs/design/2371-specify-the-managed-with-process-helper-contract.md`, but the helper itself is not yet present in the `Bosatsu/IO/Core` export list or implementation.

## Problem

Callers currently have to hand-roll the same lifecycle sequence around every spawned process. That leaves cleanup ordering, returned pipe ownership, termination escalation, final child reap, and error precedence easy to implement inconsistently. The missing helper should centralize this common lifecycle without adding backend-specific hooks, process-tree behavior, signal APIs, or a new error aggregation type.

## Steps

1. [ ] `step-1` Add the managed helper to Bosatsu/IO/Core

Implement `with_process[e, a](cmd: String, args: List[String], stdio: StdioConfig, grace: Duration, on_error: IOError -> Prog[e, a], use: SpawnResult -> Prog[e, a]) -> Prog[e, a]` in `test_workspace/Bosatsu/IO/Core.bosatsu` and export it beside the low-level process APIs. Keep it as Bosatsu library code. Structure the cleanup into small local helpers for optional handle close, first cleanup error tracking, stop escalation, and final output close so the ordering from the helper contract is readable and maintainable. Use `recover` to ensure cleanup runs after both successful and failed `use`, while preserving caller-domain `use` failures over cleanup `IOError` values.

#### Invariants

- `spawn` failure calls `on_error` and does not run cleanup because there is no `SpawnResult`.
- For each successful `spawn`, `use` is invoked exactly once and cleanup is invoked exactly once after `use` completes or fails.
- Returned `stdin` is closed before `poll`, `terminate`, `wait_timeout`, `kill`, or final `wait` escalation.
- Returned `stdout` and `stderr` are closed only after the helper has attempted final `wait`, and both output close attempts are made when both handles exist.
- Only handles returned in `SpawnResult` are closed; `Stdio.UseHandle` resources remain caller-owned.
- If `poll` reports an already-exited child, cleanup sends neither `terminate` nor `kill` and still calls `wait`.
- If the child is still running, cleanup attempts `terminate`, then `wait_timeout(proc, grace)`, then `kill` only when no final status is observed, and then final `wait`.
- A successful `use` returns its value when cleanup succeeds; a successful `use` followed by cleanup `IOError` calls `on_error` with the first cleanup error in contract phase order.
- A failed `use` keeps the caller-domain error even if cleanup also observes `IOError` values.

#### Property Tests

- Add or reuse a small modeled cleanup-policy test where generated operation outcomes validate phase ordering, first-error precedence, and escalation decisions independent of a real child process.
- Generate stdio return-shape cases in the model and assert close attempts occur exactly for `Some` handles, with `stdin` before process control and `stdout`/`stderr` after final wait.

#### Assertion Tests

- Add compile/type coverage that imports `with_process` from `Bosatsu/IO/Core` and exercises the accepted error-polymorphic signature with an `IOError -> Prog[e, a]` adapter.
- Add a direct assertion that a successful `use` value is returned for a naturally exiting child when cleanup has no helper-owned error.

2. [ ] `step-2` Cover lifecycle behavior with Bosatsu process tests

Extend the existing process test surface, likely `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` or a focused adjacent module loaded by the same harness, with real-process cases for the public helper. Keep child programs bounded and portable across the JVM and Python test paths already used by the repository. Avoid backend-only hooks; observable behavior should come from public Bosatsu IO/process APIs and normal closed-handle behavior.

#### Invariants

- Normal child completion returns the `use` result and leaves the direct child reaped before the helper returns.
- A caller-domain `use` failure still runs cleanup, stops or reaps the child, and returns the original caller error.
- An already-exited child is waited but not terminated or killed by helper cleanup.
- A still-running child is terminated first and force-killed after a short grace when it remains running.
- Returned pipe handles are closed during cleanup, with parent `stdin` closed before stop/wait escalation and output handles closed after final wait.
- `on_error` is not called for ordinary non-zero child exit by itself.
- Cleanup `IOError` after successful `use` is routed through `on_error`; cleanup `IOError` after failed `use` does not replace the caller error.

#### Property Tests

- Where practical, drive the modeled cleanup-policy tests with generated grace values including negative, zero, small positive, and large positive durations, asserting that escalation depends on `wait_timeout` returning `None`, not on inspecting duration in the helper.
- Generate caller outcomes in the model and assert `use` failure precedence over helper-owned cleanup errors.

#### Assertion Tests

- Add real-process cases for normal success, `use` failure while a sleeping child is still running, already-exited child cleanup, and terminate-to-kill escalation with a short grace duration.
- Add handle-closure assertions by retaining returned pipe handles from `use` and verifying subsequent operations fail with the existing closed-handle behavior after `with_process` returns.
- Add a `Stdio.UseHandle` case using a caller-owned handle and assert the helper does not close that handle.

3. [ ] `step-3` Run the configured verification gate

Keep the branch shippable by running the repository-required test gate after implementation and focused checks while iterating. The required gate for this repo version is `scripts/test_basic.sh` with the configured 2400 second timeout. Because this change touches the shared Bosatsu IO workspace and process tests, also run the Python process path when those tests are adjusted.

#### Invariants

- The final branch passes `scripts/test_basic.sh` before PR submission.
- Focused process tests pass in the JVM evaluation path covered by `coreJVM/testOnly * -- --log=failure` through the required gate.
- If `test_python.sh` inputs are changed or process tests are extended for Python generation, `./test_python.sh` passes as an additional targeted check.
- No backend-specific runtime behavior is changed unless tests expose a correctness issue required for the helper contract.

#### Property Tests

- Ensure any modeled property-style cleanup tests are part of a test target reached by `scripts/test_basic.sh` or by an explicitly documented focused command that is run before the required gate.

#### Assertion Tests

- Run `scripts/test_basic.sh` as the merge-blocking verification command.
- Run `./test_python.sh` after changing the generated Python workspace process tests.
- If C/libuv-specific regression coverage becomes necessary while implementing, run `make -C c_runtime test_out` and keep that coverage limited to low-level behavior the helper depends on.
