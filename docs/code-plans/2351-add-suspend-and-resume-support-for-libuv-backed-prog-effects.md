# Code Plan #2351

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2351` Add suspend-and-resume support for libuv-backed Prog effects
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `3`
- Completed steps: `1`
- Total steps: `4`

## Summary

Implement a tightly scoped internal suspend/resume mechanism for the libuv-backed C `Prog` runtime. The final change should let selected private/test effects pause interpretation with their continuation stack, keep all GC-managed state reachable while a libuv request or handle is outstanding, and resume through the existing owned `uv_loop_t` on completion without changing public Bosatsu language, library, CLI, generated runner signatures, or IO/Core behavior.

## Current State

The repository already has vendored libuv support and a C `Prog` runner skeleton that owns and drains a default-independent `uv_loop_t` in `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test`. The current private runtime state in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` is `BSTS_Prog_Runtime`: it is stack-allocated per run, embeds `uv_loop_t loop` and `uv_idle_t start_handle`, carries the current `arg`, continuation `stack`, final `BSTS_Prog_Test_Result`, `completed`, and `runtime_status`, and starts interpretation from a one-shot idle callback. `bsts_prog_runtime_step` preserves synchronous behavior for `Pure`, `Raise`, `FlatMap`, `Recover`, `ApplyFix`, and `Effect`; current `Effect(arg, f)` handling still calls `call_fn1(f, arg)` immediately and stores the returned `Prog` in `runtime->arg`. Loop shutdown already uses `uv_loop_close`, `uv_walk`, handle closure, and `uv_run(UV_RUN_DEFAULT)` to drain busy handles. The existing direct C harness in `c_runtime/test.c` has focused Prog runner coverage for pure, raise, flat_map, recover, main exit code, and repeated test invocation through `make -C c_runtime PROFILE=debug test_out`, which passed during this inspection round. Coding guidelines and the required merge gate are `coding_style.md` and `scripts/test_basic.sh` with a 2400 second timeout.

## Problem

The current loop-backed runner still treats effects as synchronously complete. There is no internal representation for a suspended interpreter frame, no ownership contract for moving the current continuation stack into a libuv callback, no GC-scanned root collection for request records that hold `BValue`s while libuv owns native request memory, and no tests proving that async completion can re-enter the interpreter through success, failure/recovery, and `flat_map` continuation paths. Without this mechanism, later libuv-backed IO migrations would either block the runner or add one-off callback plumbing that risks losing continuations, error handlers, or GC reachability.

## Steps

1. [x] `step-1` Inspect Runtime Contract and Existing Loop State

Read `coding_style.md`, `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`, the current `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`, `c_runtime/test.c`, and the prior loop-core plan/tests. Confirm the exact private runtime state shape, effect dispatch path, loop-drain behavior, and C test harness conventions before implementation. Keep the implementation surface private to the C runtime unless an existing generated test hook is the smallest reviewable way to exercise the mechanism.

#### Invariants

- Runner entry points remain responsible for creating, driving, draining, closing, and disposing their owned `uv_loop_t`.
- Existing synchronous `Prog` behavior remains byte-for-byte compatible at the public entry points: pure values, raises, recovery, flat_map sequencing, apply-fix behavior, and synchronous effects still complete during interpreter execution.
- The async mechanism is private runtime infrastructure; no Bosatsu source language, library API, CLI, or generated public runner contract changes are introduced in this slice.

#### Property Tests

- None recorded.

#### Assertion Tests

- Reviewed `coding_style.md`, `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`, `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c`, `c_runtime/test.c`, and the prior loop-core plan.
- Ran the focused existing C runtime baseline with `make -C c_runtime PROFILE=debug test_out`; it passed.

#### Completion Notes

Inspection found a private, stack-allocated `BSTS_Prog_Runtime` with embedded loop/start idle handle and a synchronous `bsts_prog_runtime_step` interpreter. The smallest implementation surface is `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` plus focused direct C tests in `c_runtime/test.c`. The next step should not add public Bosatsu APIs or generated runner signatures. It should add private runtime suspension state, a GC-scanned pending request/root list, and a deterministic test-only async effect hook implemented inside the C runtime/test harness.

2. [ ] `step-2` Introduce Suspended Prog Runtime State

Extend the private `BSTS_Prog_Runtime` state and stepper to support explicit running, suspended/pending, resumed-success, resumed-error, and finished states. Add a private suspended-continuation/request representation that captures the current `arg`, continuation/error stack, owning runtime/loop pointer, completion result, and exactly-one-resume status. Because libuv-owned request/handle memory is not a reliable Boehm root for `BValue` fields, add a runtime-owned GC-scanned pending list or equivalent root-registration mechanism that keeps suspended continuation state, effect arguments, callback payloads, and result/error values reachable until completion is consumed. Refactor `Effect` dispatch just enough to support synchronous value, synchronous raise, and suspended pending outcomes while preserving the existing synchronous `call_fn1` path.

#### Invariants

- A suspended effect owns exactly one outstanding resume path; it cannot resume twice, be dropped while pending, or continue interpreting before libuv completion marks it ready.
- All Bosatsu values reachable from the suspended continuation stack, effect arguments, callback payload, success result, and error result remain reachable for the full async lifetime.
- Synchronous effects continue through the same continuation and recovery machinery as before; the new representation does not force all effects through libuv.
- Interpreter state transitions are explicit: running, suspended/pending, resumed with value, resumed with error, and finished.
- The existing public runner signatures and generated C contracts remain unchanged.

#### Property Tests

- If the C harness has reusable generated/property-style support, add a small generated `Prog` sequencing property over pure/synchronous effects around the refactor: inserting the suspend-capable runtime path must not change the result of equivalent pure `flat_map`/`recover` programs that never suspend.

#### Assertion Tests

- Add C-level assertions proving a synthetic suspend request leaves the interpreter pending until a libuv callback marks it complete.
- Add regression assertions that existing synchronous `Effect` programs still return their success values and still route raised errors through `recover`.
- Extend the existing repeated-run C test coverage to prove pending/root state from one runner invocation is not reused by the next invocation.

3. [ ] `step-3` Resume Interpreter from Libuv Completion

Wire the runner loop so a libuv completion callback can publish a success or error into suspended runtime state and resume interpretation on the owning loop. Prefer a deterministic test-only private effect based on a libuv handle/request shape such as `uv_timer_t`, `uv_async_t`, or `uv_work_t` after-callback; callbacks should be small and should only translate completion into a `Prog` value or error and re-enter the existing stepper. The runner should continue driving the owned loop with `uv_run(UV_RUN_DEFAULT)` until no active async work or interpreter work remains, then close handles as the loop-core skeleton already expects.

#### Invariants

- Resume always happens on the owning runner/loop context and never on a stale or unrelated loop.
- Success completion re-enters `flat_map` continuations in order, exactly as if the effect had returned synchronously with that value.
- Error completion re-enters the normal raise/recover path, including recovery handlers already on the captured stack.
- The runner cannot return before all outstanding suspended effects have either completed or been converted into a runtime error state handled by existing error semantics.
- Loop shutdown remains clean: no leaked active handles, no use-after-close callbacks, and repeated `run_main`/`run_test` invocations remain independent.

#### Property Tests

- Where feasible, add a generated or table-driven invariant that async completion is observationally equivalent to a synchronous effect returning the same success/error for combinations of `flat_map` and `recover`.

#### Assertion Tests

- Add a synthetic libuv-backed async success effect in `c_runtime/test.c` that completes after the initial interpreter pass and then runs at least one captured `flat_map` continuation.
- Add a synthetic async error effect that resumes through an existing `recover` handler and verifies the recovered value.
- Add a repeated-run test to ensure suspended state from one runner invocation is not reused by the next invocation.

4. [ ] `step-4` Verify Coverage and Required Gate

Run focused C runtime tests while iterating, then run the configured required gate `scripts/test_basic.sh` within the 2400 second repo timeout before the branch is considered shippable. Review the final diff for scope: the change should be private runtime machinery plus targeted tests, with no opportunistic IO/Core migrations or unrelated cleanup.

#### Invariants

- `scripts/test_basic.sh` passes before PR submission.
- Tests cover async success, async error/recover, captured `flat_map` continuation, synchronous effect compatibility, and repeated runner isolation.
- The implementation remains below the small-job scope: any discovered design flaw larger than about 1000 lines is recorded as technical debt rather than folded into this PR, unless it is correctness-critical for suspend/resume.

#### Property Tests

- Keep any property-style or table-driven equivalence tests in the final suite if they are stable under the required gate and directly express sync-vs-async continuation/recovery equivalence.

#### Assertion Tests

- Record the exact focused test commands used during development and the final `scripts/test_basic.sh` result in the PR summary or completion notes.
- If a platform-specific libuv behavior prevents deterministic async tests, replace the flaky case with a deterministic `uv_async_t`, `uv_timer_t`, or `uv_work_t` test hook and document the choice in code comments only where it clarifies ownership.
