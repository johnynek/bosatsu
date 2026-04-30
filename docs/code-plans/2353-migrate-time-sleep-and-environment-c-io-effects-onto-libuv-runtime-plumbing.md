# Code Plan #2353

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2353` Migrate time, sleep, and environment C IO effects onto libuv runtime plumbing
- Pending steps: `4`
- Completed steps: `0`
- Total steps: `4`

## Summary

Move the C backend implementations of Bosatsu `IO/Core` time, sleep, and environment effects onto the libuv-backed Prog runtime where applicable, while preserving existing Bosatsu-level return shapes and error behavior. The final change should make `now_wall`, `now_mono`, `sleep`, and `get_env` exercise the owned runtime loop and suspend/resume path, with focused C backend coverage and `scripts/test_basic.sh` as the required merge gate.

## Current State

The `main` branch already has the libuv-owned C Prog runtime loop from issue #2349 and the private suspend/resume infrastructure for C Prog effects from issue #2351. The reference contract in `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md` defines the intended runtime ownership and callback model. The remaining low-risk `IO/Core` externals live in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`; today their time, sleep, and environment behavior is still implemented as direct synchronous C runtime work rather than consistently using the libuv runtime plumbing introduced by the dependencies.

## Problem

Blocking or runtime-bypassing implementations under `IO/Core` leave part of the C backend outside the new libuv execution contract. In particular, `sleep` needs to suspend the current Prog continuation and resume from the owned `uv_loop_t` instead of blocking with process-level sleep. Time and environment effects should use libuv APIs where they are the runtime equivalent, without changing the Bosatsu observable values or existing `IOError` mapping. Without targeted coverage, the async suspend/resume path may be present but unproven in normal Bosatsu IO programs that sequence a delayed effect with later `flat_map` work.

## Steps

1. [ ] `step-1` Inspect IO Core Externals And Runtime Contracts

Read `coding_style.md`, the libuv integration reference document, the current `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` externals, and the Prog runtime suspend/resume helpers from `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` and `c_runtime/bosatsu_ext_Bosatsu_l_Prog_internal.h`. Confirm the exact generated external signatures for `now_wall`, `now_mono`, `sleep`, and `get_env`, the existing `IOError` constructors/mapping, and how an external effect obtains the current runtime owner loop before changing behavior.

#### Invariants

- No public Bosatsu API, generated symbol name, or runner entry point changes are introduced.
- Existing synchronous `Prog` semantics for pure, raise, recover, and flat_map remain unchanged.
- The implementation uses the private libuv runtime and suspend/resume APIs introduced by the direct dependency work instead of adding a second event-loop ownership model.

#### Property Tests

- None recorded.

#### Assertion Tests

- Keep this step as inspection-only unless an existing test already exposes a mismatch; no code edits are part of the plan-authoring turn.

2. [ ] `step-2` Move Time And Environment Effects To Libuv-Compatible Calls

Update the C `IO/Core` implementations for `now_wall`, `now_mono`, and `get_env` to use libuv equivalents where they match the required semantics, while preserving the current Bosatsu value encodings and error behavior. `now_wall` should continue returning wall-clock time in the existing unit/shape, `now_mono` should remain monotonic relative time in the current unit/shape, and `get_env` should keep the existing option/error distinction and allocation ownership rules. Keep the change local to the runtime external implementation unless generated tests reveal a required helper exposure.

#### Invariants

- `now_wall` remains wall-clock based and returns values compatible with existing callers and tests.
- `now_mono` never intentionally moves backward within a single process run, modulo the existing numeric representation limits.
- `get_env` preserves the current Bosatsu result for missing variables, present variables, empty-string variables, and libuv/system errors.
- Runtime allocations and C strings are released or rooted according to the existing C runtime conventions.

#### Property Tests

- Add or keep a C backend property-style check that two sequential `now_mono` calls in one program satisfy `second >= first`.
- Where the Bosatsu test harness can control environment variables, check that `get_env(name)` round-trips several simple ASCII values without changing their bytes.

#### Assertion Tests

- Add concrete C backend coverage that `now_wall` and `now_mono` execute successfully through the C runner.
- Add concrete `get_env` cases for a known-present variable, a known-absent variable, and an empty-string variable if the harness can set one portably.

3. [ ] `step-3` Implement Libuv-Backed Sleep Suspension

Replace the blocking `sleep` implementation with a libuv timer request that suspends the active Prog continuation, starts a `uv_timer_t` on the owned runtime loop, and resumes the continuation from the timer callback. The timer request should own any callback state until completion, integrate with the existing pending suspended-request list so Bosatsu values remain rooted, report start/allocation failures through the existing `IOError` path, and close/free libuv handles only after the callback lifecycle is complete.

#### Invariants

- `sleep` does not block the OS thread while the runtime loop can drive other pending libuv work.
- Every successful `sleep` suspension resumes exactly once, and failed setup resumes or returns exactly one existing-style `IOError` result.
- Timer callback state and suspended Bosatsu values remain alive until resume and are released once no longer needed.
- Zero-duration and small positive sleeps complete through the same suspend/resume machinery without special synchronous continuation behavior unless libuv requires a setup-failure path.
- Runner shutdown still detects unfinished suspended work using the existing dependency-provided runtime checks.

#### Property Tests

- Add a sequencing-style invariant test that `sleep(d).flat_map(_ => next)` always observes the post-sleep continuation exactly once for representative durations including zero and a small positive delay.
- If the C harness supports repeated runs, check that several consecutive short sleeps in one process all complete and do not leave pending runtime state.

#### Assertion Tests

- Add a regression in `test_workspace/Bosatsu/IO/Core.bosatsu`, generated C backend tests, or `c_runtime/test.c` that a delayed `sleep` resumes and then runs subsequent `flat_map` work.
- Add a narrow test that a zero-duration sleep still returns success and continues the program.
- Add a C runtime-level failure or lifecycle assertion only if it can be tested deterministically without non-portable timer races.

4. [ ] `step-4` Verify C Backend Coverage And Required Gate

Run the focused C runtime or generated C backend tests that cover the changed externals, then run the repository-required gate `scripts/test_basic.sh` with the configured 2400 second timeout before the branch is considered PR-ready. Use any failures to tighten the implementation or tests within the same scoped IO/Core surface rather than broadening into unrelated runtime work.

#### Invariants

- The required test gate passes before PR submission.
- New tests are deterministic enough for CI and avoid relying on exact wall-clock timing beyond a small lower-bound or sequencing assertion.
- The branch remains scoped to low-risk non-file `IO/Core` effects and does not start file/network IO migration work.

#### Property Tests

- Treat the monotonic ordering and sleep continuation sequencing checks as the primary behavioral contracts for this slice.

#### Assertion Tests

- Run `scripts/test_basic.sh` and record the result in the PR summary.
- Run any narrower C runtime test target used during development before the full gate, if available in the repo.
