# Code Plan #2351

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2351` Add suspend-and-resume support for libuv-backed Prog effects
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `1`
- Completed steps: `4`
- Total steps: `5`

## Summary

Implement a tightly scoped internal suspend/resume mechanism for the libuv-backed C `Prog` runtime. The branch has private suspended runtime state, a runtime-owned GC-scanned pending list, an internal callback-facing suspend API, and focused C harness coverage for synchronous compatibility plus synthetic libuv-backed resume paths. Pre-PR review found one remaining blocker: the runner must explicitly reject any unfinished suspended continuation after `uv_run` returns, including suspend starts that leave no referenced libuv work alive. The remaining plan is to close that runner-completion invariant with focused fault coverage and rerun the required gate. No public Bosatsu language, library, CLI, generated runner signatures, or IO/Core behavior should change in this slice.

## Current State

The repository has vendored libuv support and a C `Prog` runner skeleton that owns and drains a default-independent `uv_loop_t` in `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test`. `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` has explicit private runtime states for running, suspended, resumed-success, resumed-error, and finished. A GC-allocated `BSTS_Prog_Suspended` record captures the owning runtime, effect argument, continuation/recovery stack, private request payload, completion result, error flag, and exactly-once state. Active suspended records are linked from `BSTS_Prog_Runtime.pending_head`, keeping their `BValue` fields reachable while libuv owns native handle state. `Effect(arg, f)` still calls `f(arg)` synchronously for ordinary effects, but recognizes private tag-6 suspend values built by `bsts_Bosatsu_Prog_suspend`. That private helper carries an opaque request plus a start callback; libuv callbacks resume through `bsts_Bosatsu_Prog_suspended_success` or `bsts_Bosatsu_Prog_suspended_error`, and can recover the owner loop and request payload through internal accessors. The deterministic C harness timer-backed async effect stores its timer in its own request payload instead of in the generic suspended record. The direct C harness in `c_runtime/test.c` covers synchronous Effect success, synchronous Effect raise/recover, suspended success through a captured `flat_map`, suspended error through recover, owner-loop callback access, repeated suspended invocations, and abort-on-second-completion behavior. Pre-PR review of the authoritative candidate patch found that the runner tail still treats `uv_run(..., UV_RUN_DEFAULT)` returning with `runtime_status == 0` as enough to return `runtime.result`; it does not separately require `runtime.state == BSTS_PROG_RUNTIME_FINISHED`, `runtime.pending_head == NULL`, and `runtime.resumed == NULL`. If a suspend start callback returns success but leaves no referenced libuv handle/request alive, or leaves only unreferenced work, `uv_run` can return while suspended state is still pending, after which the runner closes the loop and returns the initial error result instead of enforcing the async runtime contract.

## Problem

The previous loop-backed runner still treated effects as synchronously complete. The current branch adds suspended interpreter frames, a private callback-facing resume surface, and GC-scanned pending roots, but the post-`uv_run` completion boundary is still too weak. The design contract requires the runner not to return before outstanding suspended effects complete or are converted into an explicit runtime fault/error. Without a post-run invariant check, a malformed or unreferenced libuv-backed effect can silently drop a pending continuation and return the default initial error result, masking a broken async effect and risking closed-loop state while `BValue` continuations are still pending.

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

2. [x] `step-2` Introduce Suspended Prog Runtime State

Extend the private `BSTS_Prog_Runtime` state and stepper to support explicit running, suspended/pending, resumed-success, resumed-error, and finished states. Add a private suspended-continuation/request representation that captures the current `arg`, continuation/error stack, owning runtime/loop pointer, completion result, and exactly-one-resume status. Because libuv-owned request/handle memory is not a reliable Boehm root for `BValue` fields, add a runtime-owned GC-scanned pending list or equivalent root-registration mechanism that keeps suspended continuation state, effect arguments, callback payloads, and result/error values reachable until completion is consumed. Refactor `Effect` dispatch just enough to support synchronous value, synchronous raise, and suspended pending outcomes while preserving the existing synchronous `call_fn1` path.

#### Invariants

- A suspended effect owns exactly one outstanding resume path; it cannot resume twice, be dropped while pending, or continue interpreting before libuv completion marks it ready.
- All Bosatsu values reachable from the suspended continuation stack, effect arguments, callback payload, success result, and error result remain reachable for the full async lifetime.
- Synchronous effects continue through the same continuation and recovery machinery as before; the new representation does not force all effects through libuv.
- Interpreter state transitions are explicit: running, suspended/pending, resumed with value, resumed with error, and finished.
- The existing public runner signatures and generated C contracts remain unchanged.

#### Property Tests

- No property-style test was added in this slice; the direct C harness does not currently have reusable generated/property support for private runtime state.

#### Assertion Tests

- Added C harness assertions for a private synthetic suspended Effect that parks the interpreter until a zero-delay libuv timer callback publishes a success completion.
- Added C harness assertions that existing synchronous Effect programs still return success values and still route raised errors through `recover`.
- Added repeated-run suspended Effect assertions proving each runner invocation creates and consumes its own pending state.
- Ran `make -C c_runtime PROFILE=debug test_out`; it passed.
- Ran `git diff --check`; it passed.

#### Completion Notes

Implemented private `BSTS_Prog_Runtime_State` and `BSTS_Prog_Suspend_State` enums, a GC-allocated `BSTS_Prog_Suspended` record, and a runtime-owned pending list rooted from `BSTS_Prog_Runtime`. `Effect` dispatch now preserves the existing synchronous `call_fn1` path and recognizes a private tag-6 suspend result used by the direct C harness. Suspended records capture the effect argument, continuation/recovery stack, request payload, result/error value, owning runtime, and libuv timer handle; completion is consumed exactly once and removes the record from the pending list before interpretation resumes. Focused tests cover synchronous Effect compatibility, suspended success through `flat_map`, suspended error through `recover`, and repeated suspended-run isolation.

3. [x] `step-3` Generalize Resume Interpreter from Libuv Completion

Refine the private suspend/resume path into the reusable completion surface needed by later libuv-backed IO effects. The current slice proves the core state machine with a deterministic private timer-backed suspend tag; the next slice should make the callback-facing helper shape explicit enough for real runtime effects to allocate request records, publish success or error from a libuv completion callback, and resume on the owning loop without relying on test-only construction details. Keep callbacks small: they should translate completion into a `Prog` value or error and re-enter the existing stepper through the private runtime helper. Do not migrate IO/Core functions in this step unless a tiny private hook is necessary to prove the helper contract.

#### Invariants

- Resume always happens on the owning runner/loop context and never on a stale or unrelated loop.
- Success completion re-enters `flat_map` continuations in order, exactly as if the effect had returned synchronously with that value.
- Error completion re-enters the normal raise/recover path, including recovery handlers already on the captured stack.
- The runner cannot return before all outstanding suspended effects have either completed or been converted into a runtime error state handled by existing error semantics.
- Loop shutdown remains clean: no leaked active handles, no use-after-close callbacks, and repeated `run_main`/`run_test` invocations remain independent.

#### Property Tests

- No property-style test was added in this slice; the direct C harness remains case-based for private runtime callback plumbing.

#### Assertion Tests

- Kept the synthetic libuv-backed async success effect and moved its timer handle into a request payload that starts through the new internal suspend callback helper; it still completes after the initial interpreter pass and runs a captured `flat_map` continuation.
- Kept the synthetic async error effect on the new helper surface and verified it resumes through an existing `recover` handler.
- Added focused C harness coverage that the completion callback sees the suspended owner loop and that a second completion attempt aborts instead of re-entering the interpreter twice.
- Ran `make -C c_runtime PROFILE=debug test_out`; it passed.
- Ran `git diff --check`; it passed.

#### Completion Notes

Added `c_runtime/bosatsu_ext_Bosatsu_l_Prog_internal.h` with the private callback-facing surface: opaque `BSTS_Prog_Suspended`, `bsts_Bosatsu_Prog_suspend`, owner-loop/request accessors, and success/error completion helpers. The generic suspended record no longer embeds a test timer; it stores the GC-rooted request payload and invokes the request start callback after linking the suspended continuation into the runtime pending list. The timer-backed C test effect now allocates its own request record with the timer handle and result/error payload, starts that request on the suspended owner loop, and resumes through the reusable success/error helpers. Added an abort-on-double-completion assertion in the C harness to lock down exactly-once completion behavior.

4. [x] `step-4` Verify Coverage and Required Gate

Run focused C runtime tests while iterating, then run the configured required gate `scripts/test_basic.sh` within the 2400 second repo timeout before the branch is considered shippable. Review the final diff for scope: the change should be private runtime machinery plus targeted tests, with no opportunistic IO/Core migrations or unrelated cleanup.

#### Invariants

- `scripts/test_basic.sh` passes before PR submission.
- Tests cover async success, async error/recover, captured `flat_map` continuation, synchronous effect compatibility, and repeated runner isolation.
- The implementation remains below the small-job scope: any discovered design flaw larger than about 1000 lines is recorded as technical debt rather than folded into this PR, unless it is correctness-critical for suspend/resume.

#### Property Tests

- Keep any property-style or table-driven equivalence tests in the final suite if they are stable under the required gate and directly express sync-vs-async continuation/recovery equivalence.

#### Assertion Tests

- Ran the focused C runtime harness with `make -B -C c_runtime PROFILE=debug test_out`; it rebuilt the debug C runtime and passed.
- Ran the configured required gate `scripts/test_basic.sh`; it passed within the 2400 second timeout, reporting 74 tests passed in the first sbt invocation and 2116 passed / 2 ignored in the second sbt invocation.
- Ran `git diff --check`; it passed.
- Reviewed the final diff/scope and found this round only updates the canonical code plan JSON; the implementation scope remains private C Prog runtime machinery plus targeted C harness tests, with no IO/Core migration or unrelated cleanup.

#### Completion Notes

Final verification completed before pre-PR review. `make -B -C c_runtime PROFILE=debug test_out` rebuilt and ran the focused direct C runtime harness successfully. The configured required merge gate `scripts/test_basic.sh` passed within the 2400 second timeout: the first sbt test invocation reported 74 passed, and the second reported 2116 passed with 2 ignored. `git diff --check` also passed. Pre-PR review subsequently identified FINDING-1, so a focused pending closure step and fresh verification are still required before submission.

5. [ ] `step-5` Reject Unfinished Suspensions at Runner Boundary

Close FINDING-1 by making the runner tail explicitly validate the suspend/resume invariant after `uv_run` returns and before accepting `runtime.result` as final. `uv_run` returning must not be treated as proof that suspended `Prog` work completed. Add a small private post-run check in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` that accepts return only when the runtime is finished and both `pending_head` and `resumed` are clear. If `runtime.state` is still suspended/resumed or pending records remain, convert that condition into an explicit runtime fault or existing error result before loop shutdown/return so an unfinished continuation cannot be silently dropped. Cover the helper surface in `c_runtime/test.c` with a synthetic suspend start that returns `0` but never calls success/error and leaves no referenced libuv work alive; if the current helper shape can express it without broadening scope, also cover a start that leaves only unreferenced libuv work. Keep the fix private to the C runtime and avoid IO/Core migrations.

#### Invariants

- Corresponds to FINDING-1: `bsts_Bosatsu_Prog_run_main` and `bsts_Bosatsu_Prog_run_test` must not return a normal result while `BSTS_Prog_Runtime.pending_head` is non-null, `runtime.resumed` is non-null, or `runtime.state` is not `BSTS_PROG_RUNTIME_FINISHED`.
- `uv_run(..., UV_RUN_DEFAULT)` returning with status 0 is only a loop-drain signal; the `Prog` runner must separately prove that interpreter state is finished.
- A suspend start callback that returns success but never completes must produce an explicit runtime fault/error, not the initial default error result and not a silently closed pending continuation.
- Referenced async success/error behavior, recover behavior, `flat_map` continuation order, repeated-run isolation, and double-completion rejection continue to pass unchanged.
- The additional guard remains below the 1000 LoC refactor threshold and does not require technical-debt deferral.

#### Property Tests

- Post-run completion contract: for every runner return path after a successful `uv_run`, returning the stored `runtime.result` is valid only when `state == FINISHED`, `pending_head == NULL`, and `resumed == NULL`; all other state combinations are faults or explicit errors.
- Async liveness contract: a private suspend start that returns success must either leave referenced libuv work that eventually resumes the suspended record or be rejected before the runner returns to its caller.

#### Assertion Tests

- Add a focused C harness regression for a no-completion suspend start returning `0` with no active referenced handle/request; assert via the existing child-abort or error-result pattern that the runner rejects the unfinished suspended run before returning.
- If feasible without new public API or broad test scaffolding, add a second C harness regression for a suspend start that initializes libuv work/handle and then unreferences it so `uv_run` can drain while `pending_head` remains non-null; assert the same rejection path.
- Rerun `make -B -C c_runtime PROFILE=debug test_out` after the guard and new regressions.
- Rerun `scripts/test_basic.sh` within the configured 2400 second timeout after the focused C runtime test passes.
- Rerun `git diff --check`.
