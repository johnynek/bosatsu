# Code Plan #2431

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2431` Document portable process stop APIs and cleanup helper
- Pending steps: `0`
- Completed steps: `1`
- Total steps: `1`

## Summary

Updated the existing user/developer documentation to accurately present the shipped `Bosatsu/IO/Core` direct-child lifecycle APIs and managed `with_process` helper, including stable status observation, stop-operation limits, explicit stdio ownership, and a type-correct grace-`Duration` example. The current supervised gate did not reach repository tests: `.mergexo/context/required-tests/4e68fe38e80ccdd40e04e75b7081bbb7de759692/01a09352a9d678e1bba67d99512809e7/report.json` records exit 127 at tested HEAD `4e68fe38e80ccdd40e04e75b7081bbb7de759692` because `scripts/test_basic.sh` could not find `sbt`; stdout is empty and stderr contains only that missing-command error. The candidate changes only documentation and its code-plan artifacts and does not modify the required-test script or build tooling. This worker-provisioning failure therefore demonstrates no repository defect, warrants no additional pending step for this documentation-only issue, and should be handled by MergeXO's automatic gate rerun in an environment with `sbt` available.

Plan trajectory: initial steps 1; review revisions 0; review-origin steps 0.

## Current State

`main` already contains the complete process surface in `test_workspace/Bosatsu/IO/Core.bosatsu`: `StopResult`, `terminate`, `kill`, `poll`, `wait_timeout`, stable `wait`, and the error-polymorphic `with_process` helper. The durable contracts in `docs/design/2365-specify-the-portable-process-stop-and-status-contract.md` and `docs/design/2371-specify-the-managed-with-process-helper-contract.md` describe their semantics, and existing coverage in `test_workspace/Bosatsu/IO/ProcessWaitMain.bosatsu` and `c_runtime/test.c` exercises stable observations, timeout non-consumption, stop behavior, low-level pipe ownership, and helper cleanup.

The promised user-facing artifact, `docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md`, still shows only `spawn` and `wait` in its process API shape and does not document the managed helper. Its process-termination section discusses the intentionally omitted self-`exit` operation without distinguishing that choice from the now-supported ability to stop a spawned child. The related shared process contract in `docs/src/main/paradox/design-docs/prog_concurrency_design.md` likewise lists only `spawn` and `wait` and still says process termination would be a future separate API.

## Problem

Users consulting the repository documentation cannot discover or correctly apply the shipped portable process-control and cleanup facilities. The documentation must match the implemented signatures and tested contract: lifecycle operations control only the direct child represented by `Process`; `wait`, `poll`, and `wait_timeout` share a stable final status; timeout observations do not consume that status; `terminate` and `kill` return `StopResult`; no raw-signal or process-tree behavior is promised; low-level returned pipe handles remain caller-managed; and `with_process` provides the existing managed cleanup path with a configurable grace `Duration`.

## Technical Debt Notes

Follow-up proposal: comprehensively reconcile the remainder of `docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md` with the current non-process `Bosatsu/IO/Core` file, byte, temporary-resource, and mode APIs, and update obsolete proposal/implementation-plan tense. Those broader discrepancies predate #2431 and are not required to document the process surface; correcting them is estimated at roughly 150–300 lines of documentation churn.

## Steps

1. [x] `step-1` Document the shipped process lifecycle surface and cleanup helper

Update `docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md` using the declarations in `test_workspace/Bosatsu/IO/Core.bosatsu` as the authority for exact public names, argument order, type parameters, and result types. Extend the process-related export list and API example with `StopResult()`, `terminate`, `kill`, `poll`, `wait_timeout`, and `with_process`; include `duration_from_nanos` where needed so the grace-duration example uses a publicly constructible `Duration`. Show `StopResult` with `StopSent` and `AlreadyExited`, the four exact low-level signatures, and the exact error-polymorphic helper shape `with_process[e, a](cmd, args, stdio, grace, on_error, use) -> Prog[e, a]`. Do not describe `with_process` as an external function because it is the existing Bosatsu-level composition of `spawn`, `close`, and the low-level lifecycle operations.

Add concise process semantics beside that API shape. Explain that `terminate` requests the backend's best available normal stop and `kill` requests its best available forceful stop; neither promises graceful application shutdown. State that both operate only on the spawned direct child, return `AlreadyExited` after final status has been recorded or `StopSent` when a request is issued, and neither exposes raw signals nor controls descendants or process trees. Describe `poll` as nonblocking, `wait_timeout` as returning `None` without consuming the eventual result, and `wait` as returning the same recorded normalized integer status on repeated observations. Make the ownership boundary explicit: low-level `spawn` callers must close handles returned for `Stdio.Pipe`; lifecycle operations do not close or drain them; and handles supplied through `Stdio.UseHandle` remain caller-owned.

Add one concise, type-correct `with_process` example that passes a grace value built with `duration_from_nanos`, routes helper-owned `IOError` through `on_error`, and consumes the supplied `SpawnResult` in `use`. Explain that after `use` succeeds or fails, the helper closes returned stdin first, observes or stops the direct child, waits for the grace duration before escalating to `kill` when necessary, attempts the final `wait`, then closes returned stdout/stderr. Clarify that it closes only pipe handles returned in `SpawnResult`, never caller-supplied `UseHandle` resources, and does not drain output; callers needing custom drain or close order should retain the low-level API.

Retitle or revise the existing process-termination tradeoff so it clearly concerns the absence of a self-`exit` primitive in `Bosatsu/IO/Core`, not an absence of spawned-child stop support. Preserve the recommendation to return the current program's status through `Main`.

Also make the bounded consistency repair in `docs/src/main/paradox/design-docs/prog_concurrency_design.md`: extend its shared process-contract snippet and nearby semantics to recognize the shipped low-level stop/status operations, and replace the stale statement that process termination is future work with the precise rule that canceling a fiber waiting on a process does not invoke the separate direct-child `terminate` or `kill` APIs. Do not duplicate the full helper guide there.

Keep this change documentation-only. Do not edit runtime/library code, tests, generated code-plan markdown, configuration, or historical code-plan artifacts, and do not add operator knobs, platform-specific public branches, raw-signal APIs, or process-tree guarantees. The step is complete when both active Paradox design pages agree with the shipped process declarations and no active documentation claims that spawned-process termination is unavailable or future work.

#### Invariants

- Every added API declaration and example matches the current `Bosatsu/IO/Core` public surface, including the `StopResult` constructors and the argument order and error polymorphism of `with_process`.
- All stop/status documentation is limited to the direct `Process` returned by `spawn` and makes no raw-signal, descendant, process-group, job-object, or process-tree guarantee.
- Once final status is recorded, `wait`, `poll`, and `wait_timeout` are documented as observing the same stable value; a timeout returning `None` does not consume or invalidate the eventual status.
- Low-level lifecycle operations never implicitly close or drain returned stdio handles; returned `Stdio.Pipe` handles remain caller-managed unless passed through `with_process`, and `Stdio.UseHandle` resources remain caller-owned.
- The managed helper is documented according to its implemented lifecycle: cleanup follows both successful and failed `use`, normal termination precedes grace waiting and any forceful kill, and final wait is attempted before returned output pipes are closed.
- The absence of a self-`exit` primitive remains documented without contradicting the availability of direct-child `terminate` and `kill`.
- The patch changes documentation only and introduces no executable behavior, public platform branch, or operator-facing setting.

#### Property Tests

- None recorded.

#### Assertion Tests

- None recorded.

#### Completion Notes

Updated both active Paradox process design pages to document the shipped direct-child stop/status APIs, explicit stdio ownership, and the Bosatsu-level `with_process` cleanup contract, including a type-correct grace-duration example. Verified the documented declarations against `test_workspace/Bosatsu/IO/Core.bosatsu`, checked the documentation diff and Markdown fences, and ran `git diff --check`; this documentation-only change requires no runnable tests.
