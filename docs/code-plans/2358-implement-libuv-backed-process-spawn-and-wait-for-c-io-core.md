# Code Plan #2358

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2358` Implement libuv-backed process spawn and wait for C IO/Core
- Source design doc: `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`
- Pending steps: `1`
- Completed steps: `4`
- Total steps: `5`

## Summary

Implement the existing Bosatsu `IO/Core.spawn` and `IO/Core.wait` externals in the C runtime using libuv process APIs, preserving the current Bosatsu API shapes for `SpawnResult`, `Stdio`, and `StdioConfig`. The branch has process lifecycle and stdio wiring in place, but pre-PR review found one approval-blocking lifecycle safety gap: active libuv process handles are backed by GC-owned state that can become unreachable before libuv invokes the exit callback. The remaining work is a small ownership refactor so active process state is native-owned or runtime-rooted until `uv_close` completes, plus focused regression coverage for ignored or dropped process values.

## Current State

The repository already has the libuv integration contract in `docs/design/2342-document-the-libuv-c-runtime-integration-contract.md`, suspend/resume support for C Prog effects in `c_runtime/bosatsu_ext_Bosatsu_l_Prog.c` and `c_runtime/bosatsu_ext_Bosatsu_l_Prog_internal.h`, and libuv-backed file and directory IO/Core operations in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`. `test_workspace/Bosatsu/IO/Core.bosatsu` defines the public Bosatsu-side `SpawnResult`, `Stdio`, and `StdioConfig` values that the C externals must continue to return and consume. The process audit found that `Stdio` enum tags are `0 = Inherit`, `1 = Pipe`, `2 = Null`, and `3 = UseHandle(handle)`; `StdioConfig` is a struct3 ordered as stdin, stdout, stderr; `SpawnResult` is a struct4 ordered as proc, stdin, stdout, stderr with stdio handles wrapped in `Option`; and `Process` is an external struct. The branch now replaces the unsupported `spawn` and `wait` C externals with a private boxed process state backed by `uv_process_t`, supports inherited and null stdio, creates fd-backed pipe handles for piped stdin/stdout/stderr, validates existing-handle stdio directions before passing descriptors to libuv, and closes created pipe descriptors on failure paths. Pre-PR review found that the private `BSTS_Core_Process` is allocated with `GC_malloc` and can become unreachable after successful spawn if user code ignores or drops the returned process value, while libuv can still call the exit callback through `uv_process_t.data`.

## Problem

The C backend previously could not execute external processes through the existing IO/Core API, so any Bosatsu program using process spawning or waiting failed through unsupported runtime behavior despite adjacent file, directory, and Prog async infrastructure being present. The unsupported process and stdio gaps are now mostly closed, but the current process ownership model is not submit-ready: an active libuv process handle must not point at GC-reclaimable callback state. This violates the libuv/GC contract and can become a use-after-free when the process value is not retained until child exit. The next revision must make active process state lifetime independent of Bosatsu value reachability, while preserving the existing wait rooting and single-resume behavior.

## Steps

1. [x] `step-1-audit-shapes-and-runtime-contracts` Audit process API shapes and handle contracts

Read the existing unsupported `spawn` and `wait` externals in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`, the Bosatsu-side constructors in `test_workspace/Bosatsu/IO/Core.bosatsu`, the libuv runtime contract document, and the recent suspend/resume and file IO implementations. Pin down the exact C constructor tags, argument order, handle representation, ownership rules, and IOError mapping helpers before changing implementation code. This step produced the smallest implementation shape that fits the existing API rather than introducing new Bosatsu-visible types.

#### Invariants

- The implementation must preserve the public Bosatsu `SpawnResult`, `Stdio`, and `StdioConfig` shapes exactly as defined today.
- Process support must reuse the existing C runtime loop, handle ownership, error mapping, and suspend/resume helpers instead of adding a second event-loop or continuation mechanism.
- Any `BValue` retained across libuv callbacks must be rooted through the runtime suspension/pending machinery or otherwise kept reachable according to the patterns introduced by #2351 and #2356.

#### Property Tests

- None recorded.

#### Assertion Tests

- No runtime assertion tests were added in this audit-only slice; the audited tag and field-order facts are recorded in completion notes and should be encoded in focused C harness coverage while implementing process lifecycle and stdio decoding.
- The unsupported-gap target remains concrete: `bsts_core_spawn_effect` and `bsts_core_wait_effect` are the only process externals still raising `Unsupported` in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`.

#### Completion Notes

Completed the contract audit. `spawn(cmd, args, stdio)` enters C as `bsts_prog_effect3(cmd, args, stdio, bsts_core_spawn_effect)`, so `bsts_core_spawn_effect` receives struct3 fields `0 = cmd`, `1 = args`, `2 = stdio`. `wait(process)` enters through `bsts_prog_effect1(process, bsts_core_wait_effect)`. Bosatsu-side process shapes are fixed by `test_workspace/Bosatsu/IO/Core.bosatsu`: `Process` is an external struct; `Stdio` tags are `0 = Inherit`, `1 = Pipe`, `2 = Null`, `3 = UseHandle(handle)`; `StdioConfig` is struct3 ordered stdin/stdout/stderr; `SpawnResult` is struct4 ordered proc/stdin/stdout/stderr where the stdio fields are `Option[Handle]`. Existing `Option` encoding is `0 = None`, `1 = Some(value)`. Runtime handles are currently boxed `BSTS_Core_Handle *` values with kind/file/stdio/readable/writable/close_on_close/closed fields; kinds are stdin/stdout/stderr/file only, so process support needs either a separate boxed process state for `Process` or an intentionally extended private handle family without changing Bosatsu-visible types. Existing IOError helpers to reuse are `bsts_ioerror_from_uv`, `bsts_ioerror_invalid_argument`, `bsts_ioerror_bad_fd`, and `bsts_ioerror_unsupported`. Any async process wait must use `bsts_Bosatsu_Prog_suspend`, keep retained Bosatsu values reachable through the Prog pending list, and resume with `bsts_Bosatsu_Prog_suspended_success/error` exactly once.

2. [x] `step-2-implement-libuv-spawn-and-wait-lifecycle` Implement process lifecycle

Replace the unsupported C `spawn` and `wait` externals with a libuv-backed lifecycle built around `uv_spawn`, `uv_process_t`, and the existing Prog suspension/resumption API. Decode `spawn`'s effect argument as struct3 fields `cmd`, `args`, and `stdio`; construct a private boxed `Process` state for the returned external struct; and return `SpawnResult` as `alloc_struct4(process, stdin_option, stdout_option, stderr_option)`. The process state should record the `uv_process_t`, completion status, exit code/term signal, any wait continuation, whether wait has consumed the exit result, and any owned stdio pipe handles. `wait` should either return an already-known exit code or suspend until the libuv exit callback resumes it. Cleanup must close process and pipe handles on all paths and reject double wait/completion states coherently.

#### Invariants

- A successfully spawned process has exactly one process state and its libuv handle must remain valid until process exit closes the handle; pre-PR review found the current GC-owned implementation does not yet satisfy this lifetime invariant when the Bosatsu process value is dropped.
- `wait` is single-consumption for this C representation: invalid, already-consumed, or currently-waited process values map to `BadFileDescriptor` instead of use-after-free or double resume.
- Each suspended wait continuation is resumed exactly once from the libuv process exit callback with the child exit value.
- Spawn argument buffers and stdio containers are cleaned up after `uv_spawn` returns; failed spawn resumes through the existing IOError conventions.
- Missing-command, invalid-argument, and libuv negative-status failures flow through the same IO/Core error conventions used by the file and directory migration.

#### Property Tests

- Repeated spawn/wait cycles of `/bin/sh -c 'exit N'` are covered by focused case tests in the C runtime harness for zero and non-zero exits; broader generated command properties remain better suited for the final stdio-enabled slice.

#### Assertion Tests

- Spawn `/bin/sh -c 'exit 0'` with null stdio and assert `wait` returns `0`.
- Spawn `/bin/sh -c 'exit 7'` with null stdio and assert `wait` returns `7`.
- Attempt to spawn a missing command and assert the result maps to the existing `NotFound` IOError variant rather than crashing or returning the old unsupported marker.
- Attempt to wait on a non-process value and assert the existing invalid-handle-style `BadFileDescriptor` error mapping.

#### Completion Notes

Implemented the lifecycle in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` using a private `BSTS_Core_Process` box with a magic value, embedded `uv_process_t`, exit status/signal fields, single wait continuation, wait-consumed state, and process-handle close tracking. `spawn` now suspends briefly to access the Prog-owned libuv loop, builds `argv` from the Bosatsu command and argument list, decodes `StdioConfig` enough to support `Inherit` and `Null`, invokes `uv_spawn`, and resumes with `SpawnResult(process, None, None, None)` on success or a mapped IOError on failure. `wait` returns immediately for already-exited unconsumed processes, otherwise suspends and is resumed exactly once by the libuv exit callback. Piped and existing-handle stdio were intentionally left to the next planned step in that round and were completed by `step-3-wire-stdio-configurations`. Added C harness tests in `c_runtime/test.c` for zero exit, non-zero exit, missing command error mapping, and invalid wait input. Verification run for that lifecycle round: `make -C c_runtime test_out PROFILE=debug` passed; `scripts/test_basic.sh` passed with 2116 tests passed and 2 ignored. Pre-PR review later identified that active process state ownership still needs a pending correction, captured in `step-5-root-active-process-state-until-close`.

3. [x] `step-3-wire-stdio-configurations` Wire stdio configurations

Implement the remaining translation from Bosatsu `StdioConfig` values into `uv_stdio_container_t` entries for stdin, stdout, and stderr. The lifecycle slice already decodes `StdioConfig` and supports `0 = Inherit` via `UV_INHERIT_FD` and `2 = Null` via `UV_IGNORE`; this step should replace the temporary `Unsupported` paths for `1 = Pipe` and `3 = UseHandle(handle)`. For piped stdio, create libuv pipe handles with correct direction, return `Some(handle)` in the matching `SpawnResult` field, and make those handles compose with the existing IO/Core read/write/close operations. For existing-handle stdio, validate handle kind/direction before passing it to libuv. Keep platform-specific command and descriptor assumptions guarded so tests remain portable across the repo's sanitizer and valgrind targets.

#### Invariants

- The order and meaning of stdin, stdout, and stderr in the C translation must match `test_workspace/Bosatsu/IO/Core.bosatsu`.
- Inherited and null stdio must continue not to create Bosatsu-visible handles or leak libuv handles.
- Piped stdio must create handles with correct read/write direction and ownership, and those handles must remain usable by existing IO/Core read/write/close operations until closed or process cleanup makes them invalid.
- Existing-handle stdio must validate handle kind/direction before passing it to libuv, and invalid handles must produce mapped IOError results without corrupting the original handle.
- All stdio resources created during a failed spawn attempt must be closed or freed on the failure path.

#### Property Tests

- The bounded stdin-to-stdout pipe round-trip invariant is covered by a deterministic C harness case using `/bin/sh -c cat`: bytes written to the returned stdin handle are read back exactly from the returned stdout handle after closing stdin.
- The repeated pipe isolation property remains better suited for a broader follow-up if the C harness grows generated process fixtures; this small-job slice added deterministic stdout, stderr, and stdin pipe cases instead.

#### Assertion Tests

- Spawn a command with piped stdout and assert the expected stdout bytes can be read through the returned Bosatsu handle.
- Spawn a command with piped stderr and assert expected stderr bytes can be read when the platform fixture supports it.
- Spawn a command with piped stdin, write bytes through the returned handle, close stdin, and assert the child receives the bytes where a portable fixture is available.
- Spawn with inherited and null stdio variants and assert the process can still be waited successfully without returned pipe handles.
- Pass an incompatible existing handle as stdio and assert the mapped invalid-handle IOError path.
- Assert the temporary `Unsupported` results for `Pipe` and `UseHandle` are removed once this step is complete.

#### Completion Notes

Completed stdio configuration wiring in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`. `Stdio.Pipe` now creates an OS pipe descriptor pair with the parent end wrapped in the existing fd-backed `BSTS_Core_Handle` representation and returned as `Some(handle)` in the matching `SpawnResult` field. Child ends are passed to `uv_spawn` with `UV_INHERIT_FD`, then closed in the parent after spawn; parent pipe ends are closed and their handle state is marked closed if spawn fails after pipe creation. Pipe directions are stdin parent-writable/child-readable and stdout/stderr parent-readable/child-writable, so the returned handles compose with existing `read_all_bytes`, `write_bytes`, and `close`. `Stdio.UseHandle(handle)` now validates closed state and direction before passing the existing descriptor to libuv, returning `BadFileDescriptor` for incompatible handles. Added C harness coverage for piped stdout, piped stderr, stdin-to-stdout round trip, and invalid existing-handle stdio. Verification run this round: `make -C c_runtime test_out PROFILE=debug` passed; `scripts/test_basic.sh` passed with 2116 tests passed and 2 ignored.

4. [x] `step-4-verify-required-gate-and-memory-safety` Verify required gate

Run the repository-required gate `scripts/test_basic.sh` with the configured 2400 second timeout after the remaining stdio implementation lands. This lifecycle round already ran the gate successfully, but final verification should be repeated after pipe and existing-handle stdio are implemented. Also run the most focused C runtime test target directly during development when available so process failures are faster to diagnose. Review sanitizer/valgrind-sensitive paths for libuv request cleanup, process and pipe close callbacks, GC reachability of retained `BValue`s, and raw `GC_malloc`/`GC_malloc_atomic` uses as called out in the dependency handoff review notes.

#### Invariants

- `scripts/test_basic.sh` must pass before the branch is considered submit-ready.
- No test may depend on a platform-only shell path without a guard or fallback appropriate for the current CI targets.
- The final implementation must not introduce leaked libuv requests/handles, double closes, double resumes, or unrooted suspended Bosatsu values.
- Failure-path cleanup must be covered at least by missing-command and invalid-handle tests.

#### Property Tests

- Keep any bounded repeated spawn/wait or pipe round-trip checks in the normal C runtime test suite only if they are deterministic and fast enough for the required gate.

#### Assertion Tests

- Record that `scripts/test_basic.sh` was run successfully in completion notes for the implementation PR.
- If focused sanitizer or valgrind scripts are available and practical within the repo conventions, run the relevant one and record results; otherwise explicitly note the required gate coverage and any residual manual-review risk.

#### Completion Notes

Verification completed after stdio wiring landed. `make -C c_runtime test_out PROFILE=debug` passed, covering the focused C runtime harness and new process stdio cases. `scripts/test_basic.sh` passed with 2116 tests passed and 2 ignored in 87 seconds on the final branch state. `git diff --check` passed. This round did not run separate sanitizer or valgrind scripts; the required gate and focused C runtime target were used for this small-job checkpoint. Pre-PR review later identified an active process ownership gap that was not caught by these gates, so final verification must be repeated after `step-5-root-active-process-state-until-close`.

5. [ ] `step-5-root-active-process-state-until-close` Root active process state until close

Address approval-blocking review finding F1 in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c`: `BSTS_Core_Process` currently embeds an active `uv_process_t` but is allocated with `GC_malloc` and is only returned through the `SpawnResult` process value. After successful spawn, user code may ignore or drop that process value before child exit, leaving libuv with `uv_process_t.data` pointing at GC-reclaimable memory. Refactor the process state lifetime so active libuv callback state remains valid independently of Bosatsu value reachability. The smallest acceptable designs are either native-owning `BSTS_Core_Process` with explicit free from the process close callback, or adding active processes to a GC-scanned/rooted runtime list until `uv_close` completes. Keep the existing public `Process` representation, wait single-consumption semantics, stdio behavior, and error mapping. Verify the adjacent pending wait path still keeps the process and suspended continuation reachable while wait is outstanding, and that process state is removed/freed only after the libuv close callback has run.

#### Invariants

- Corresponds to review finding F1: every successfully spawned active `uv_process_t` must have callback state whose lifetime extends until the `uv_close` callback completes, even when no Bosatsu `Process` value remains reachable.
- `uv_process_t.data` must never point at GC-reclaimable memory unless that memory is also held by an explicit runtime root for the entire active-handle lifetime.
- A pending `wait` must continue to root both the suspended continuation and the associated process state until it is resumed exactly once or rejected before suspension.
- The process close callback must be the single terminal ownership point for native-owned process state or the point where rooted process state is unlinked; no exit path may double-free, double-unlink, double-close, or leave an active process permanently rooted.
- The fix should be local to the C runtime process implementation and expected to stay well under the 1000 LoC refactor threshold; do the refactor in this PR rather than deferring it as technical debt.

#### Property Tests

- For any spawned child whose `Process` result is ignored or dropped before exit, forcing a GC cycle before the child exits and then draining the libuv loop must not crash, dereference freed process state, leak an active process root, or skip process close cleanup.
- For any spawned child with a pending `wait`, the wait continuation must be resumed exactly once with the same exit value regardless of whether other references to the returned `Process` value are retained.
- Across repeated spawn/drop and spawn/wait cycles, the number of active native-owned or runtime-rooted process states should return to its baseline after children exit and close callbacks run.

#### Assertion Tests

- Add focused C harness coverage that spawns a short-lived or delayed command, deliberately discards the returned process value, forces Boehm collection before child exit where practical, drains the Prog/libuv loop, and asserts the run completes without crash or unfinished suspension faults.
- Add a focused wait regression that starts `wait`, drops other process references, forces collection before child exit where practical, and asserts wait still returns the expected exit code exactly once.
- Retain existing spawn/wait, missing-command, pipe stdout/stderr/stdin, invalid existing-handle, and invalid wait tests; rerun `make -C c_runtime test_out PROFILE=debug`, `scripts/test_basic.sh`, and `git diff --check` after the ownership refactor.
