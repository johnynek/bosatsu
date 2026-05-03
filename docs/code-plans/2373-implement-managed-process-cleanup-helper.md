# Code Plan #2373

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2373` Implement managed process cleanup helper
- Source design doc: `docs/design/2371-specify-the-managed-with-process-helper-contract.md`
- Pending steps: `0`
- Completed steps: `3`
- Total steps: `3`

## Summary

Add the public `with_process` helper to `Bosatsu/IO/Core` as ordinary Bosatsu library code. The helper wraps `spawn`, invokes caller code after a successful spawn, then runs deterministic cleanup that closes returned stdio pipe handles, terminates or kills still-running direct children after the configured grace duration, and reaches a final `wait` attempt before returning from cleanup. The implementation stays on top of the public low-level APIs from issue #2369 and preserves the error-polymorphic contract from the accepted helper design document.

## Current State

`test_workspace/Bosatsu/IO/Core.bosatsu` exports and implements `with_process` beside the low-level process surface. `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` now covers the helper through real-process cases for successful use results, non-zero child exits that do not call `on_error`, caller-domain use failure precedence, already-exited child cleanup, zero-grace stop escalation for a sleeping child, returned pipe handle closure, and unrelated caller-owned handle ownership. Focused JVM and Python checks pass, and the configured `scripts/test_basic.sh` gate passes on the final workspace state.

## Problem

The original gap was that callers had the managed lifecycle helper but lacked focused Bosatsu-level lifecycle coverage for cleanup ordering, returned-pipe ownership, stop escalation, final reap, and error precedence. This branch now has case-based process coverage for the public helper and has passed the configured required test gate.

## Steps

1. [x] `step-1` Add the managed helper to Bosatsu/IO/Core

Implemented `with_process[e, a](cmd: String, args: List[String], stdio: StdioConfig, grace: Duration, on_error: IOError -> Prog[e, a], use: SpawnResult -> Prog[e, a]) -> Prog[e, a]` in `test_workspace/Bosatsu/IO/Core.bosatsu` and exported it beside the low-level process APIs. The implementation remains Bosatsu library code and uses public `spawn`, `close`, `poll`, `terminate`, `wait_timeout`, `kill`, and `wait`. Cleanup uses small helpers for optional handle close, first cleanup error tracking, status observation, stop escalation, and output close ordering.

#### Invariants

- `spawn` failure calls `on_error` and does not run cleanup because there is no `SpawnResult`.
- For each successful `spawn`, `use` is invoked exactly once and cleanup is invoked exactly once after `use` succeeds or fails.
- Returned `stdin` is closed before `poll`, `terminate`, `wait_timeout`, `kill`, or final `wait` escalation.
- Returned `stdout` and `stderr` are closed only after the helper has attempted final `wait`, and both output close attempts are made when both handles exist.
- Only handles returned in `SpawnResult` are closed; `Stdio.UseHandle` resources remain caller-owned.
- If `poll` reports an already-exited child, cleanup sends neither `terminate` nor `kill` and still calls `wait`.
- If the child is still running, cleanup attempts `terminate`, then `wait_timeout(proc, grace)`, then `kill` only when no final status is observed, and then final `wait`.
- A successful `use` returns its value when cleanup succeeds; a successful `use` followed by cleanup `IOError` calls `on_error` with the first cleanup error in contract phase order.
- A failed `use` keeps the caller-domain error even if cleanup also observes `IOError` values.

#### Property Tests

- None recorded.

#### Assertion Tests

- Compile/type coverage for the exported helper is covered by the clean `scripts/test_basic.sh` run, which exercises the full test workspace and embedded JVM evaluation sources.

#### Completion Notes

Added `with_process` and internal cleanup helpers to `test_workspace/Bosatsu/IO/Core.bosatsu`. A clean `scripts/test_basic.sh` run passed: CLI tests `74/74`, core JVM tests `2118/2118` with `2` ignored. During verification, stale compile-time embedded test workspace content required `sbt clean` before rerunning the required gate.

2. [x] `step-2` Cover lifecycle behavior with Bosatsu process tests

Extended `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` with real-process tests for the public `with_process` helper. The tests stay on public Bosatsu IO/process APIs and use bounded commands already used by the process harness: `true`, `false`, and short Python children.

#### Invariants

- Normal child completion returns the `use` result and leaves the direct child reaped before the helper returns.
- A caller-domain `use` failure still runs cleanup, stops or reaps the child, and returns the original caller error.
- An already-exited child is waited but not terminated or killed by helper cleanup.
- A still-running child is terminated first and force-killed after a short grace when it remains running.
- Returned pipe handles are closed during cleanup, with parent `stdin` closed before stop/wait escalation and output handles closed after final wait.
- `on_error` is not called for ordinary non-zero child exit by itself.
- Cleanup `IOError` after successful `use` is routed through `on_error`; cleanup `IOError` after failed `use` does not replace the caller error.

#### Property Tests

- No separate modeled property test was added in this slice; the coverage added here is case-based real-process coverage through the public helper API, which is the highest-value surface for the selected lifecycle test step.

#### Assertion Tests

- Added compile/type coverage by importing and calling `with_process` from `Bosatsu/IO/Core` with both `IOError` and caller-domain `String` error adapters.
- Added a direct assertion that successful `use` returns its value for a naturally exiting child.
- Added a real-process non-zero child case asserting ordinary non-zero exit does not call `on_error`.
- Added a caller-domain `use` failure case while a sleeping child is still running, asserting the original caller error is preserved while cleanup runs.
- Added an already-exited child case where `use` waits the process before helper cleanup.
- Added a zero-grace sleeping-child case that observes the child running in `use`, forcing helper cleanup through the terminate/wait-timeout/kill path when needed.
- Added returned-pipe handle closure assertions by retaining returned stdin/stdout/stderr handles and verifying subsequent read/write operations fail after `with_process` returns.
- Added a caller-owned readable temp-file handle case and asserted the helper leaves unrelated handles open for the caller to close afterward.

#### Completion Notes

Added focused `with_process_*_case` tests to `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu`, plus small closed-handle assertion helpers. Verification passed with `sbt "coreJVM/testOnly dev.bosatsu.EvaluationTest -- --log=failure"` and `./test_python.sh` after building the local CLI assembly needed by the Python script.

3. [x] `step-3` Run the configured verification gate

Ran the repository-required test gate after adding lifecycle tests. The required gate for this repo version is `scripts/test_basic.sh` with the configured 2400 second timeout. Because the shared process test program is also part of the Python transpile path, `./test_python.sh` was run as the targeted cross-runtime check.

#### Invariants

- The final branch passes `scripts/test_basic.sh` before PR submission.
- Focused process tests pass in the JVM evaluation path covered by `coreJVM/testOnly * -- --log=failure` through the required gate.
- If `test_python.sh` inputs are changed or process tests are extended for Python generation, `./test_python.sh` passes as an additional targeted check.
- No backend-specific runtime behavior is changed unless tests expose a correctness issue required for the helper contract.

#### Property Tests

- The selected step landed case-based lifecycle tests; no modeled property-style cleanup-policy test was added because the helper currently composes concrete public IO/process APIs directly and this round stayed scoped to the real-process Bosatsu harness.

#### Assertion Tests

- Ran `sbt "coreJVM/testOnly dev.bosatsu.EvaluationTest -- --log=failure"`: passed `88/88`.
- Ran `sbt -batch cli/assembly` to create the local `bosatsuj` jar required by `test_python.sh`.
- Ran `./test_python.sh`: passed after the local assembly was available.
- Ran `scripts/test_basic.sh`: CLI tests passed `74/74`; core JVM tests passed `2118/2118` with `2` ignored.

#### Completion Notes

Final verification on the exact final workspace state passed with `scripts/test_basic.sh`: CLI tests `74/74`; core JVM tests `2118/2118` with `2` ignored. Focused JVM EvaluationTest and `./test_python.sh` also passed.
