---
issue: 2287
priority: 3
touch_paths:
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/.jvm/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - core/.js/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - test_workspace/ProgExt.py
  - test_workspace/Prog.bosatsu_externals
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c
  - c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-30T19:24:31Z
---

# Issue #2287 Design: mode-aware mkdir for IO/Core

_Issue: #2287 (https://github.com/johnynek/bosatsu/issues/2287)_

## Summary

Add an additive POSIX-mode surface to `Bosatsu/IO/Core` so Bosatsu code can request exact leaf directory permissions, inspect mode bits on supported hosts, and align JVM eval, Python, C, and Node behavior without breaking existing `mkdir` or `FileStat` callers.

## Context

`Bosatsu/IO/Core.mkdir(path, recursive)` currently only controls whether parents are created. It cannot request a final directory mode, and the public API does not expose POSIX permission bits at all. That blocks an exact `mkdir(1)` implementation for `-m`, and it also hides backend drift we already have.

Repository-specific gaps today:

1. JVM eval uses `Files.createDirectory` and `Files.createDirectories`, so recursive create already behaves close to `mkdir -p`.
2. The C runtime recursive helper also treats an existing directory as success.
3. The Python runtime currently calls `os.makedirs(path)` with `exist_ok=False`, so `recursive=True` still fails when the leaf directory already exists.
4. `FileStat` exposes only kind, size, and mtime, so tests and future utilities cannot verify permission results.

The runtime contract needed by `mkdir(1)` is narrower than a full permission-management API:

1. Default create should stay platform-default.
2. `-m` needs an explicit leaf mode.
3. `-p` needs parent creation semantics that keep newly created parents traversable.
4. Unsupported hosts must fail explicitly instead of silently ignoring requested modes.

`mkdir(1)` symbolic modes do not require a public `umask` API. For `mkdir -m`, symbolic `+` and `-` operations are interpreted from an initial `a=rwx`, so Bosatsu-side parsing can stay pure code layered above a mode-aware runtime API.

## Goals

1. Add a non-breaking way to request an explicit POSIX mode when creating a directory.
2. Preserve existing `mkdir(path, recursive)` call sites.
3. Expose POSIX mode bits without changing the public `FileStat` shape.
4. Normalize recursive existing-directory behavior across JVM, Python, C, and Node-backed eval/test flows.
5. Make non-POSIX behavior explicit through typed errors instead of best-effort silent success.

## Non-goals

1. Do not add a public `umask` API in this issue.
2. Do not add a general public `chmod` API in this issue.
3. Do not attempt to model Windows ACLs as if they were full POSIX modes.
4. Do not implement the full Bosatsu `mkdir` utility in this issue; this issue only provides the runtime surface that unblocks it.

## Proposed Architecture

### 1. Additive public API in `Bosatsu/IO/Core`

Add a new opaque mode value and two new externals:

1. `PosixMode` representing only the low permission bits, including special bits, not file type bits.
2. `posix_mode(bits: Int) -> Option[PosixMode]` and `posix_mode_to_Int(mode: PosixMode) -> Int` as pure helpers.
3. `mkdir_with_mode(path: Path, recursive: Bool, mode: PosixMode) -> Prog[IOError, Unit]`.
4. `stat_posix_mode(path: Path) -> Prog[IOError, Option[PosixMode]]`.

Keep the existing `mkdir(path, recursive)` API unchanged.

This is intentionally additive:

1. Existing callers keep compiling.
2. Existing `FileStat` pattern matches do not break.
3. Utility code that only needs `-p` can keep using `mkdir`.
4. Utility code that needs `-m` can opt into the new API.

### 2. Runtime semantics

The new API should follow these rules:

1. `mkdir_with_mode(..., recursive=False, mode)` creates exactly one directory and applies `mode` to the newly created leaf.
2. `mkdir_with_mode(..., recursive=True, mode)` creates missing parents using `mkdir -p` semantics and applies `mode` only to the newly created leaf.
3. If `recursive=True` and the leaf already exists as a directory, the call succeeds and does not change the existing directory mode.
4. If `recursive=False` and the leaf already exists, the call returns `AlreadyExists`.
5. `stat_posix_mode` follows the same no-follow-symlink behavior as current `stat`.
6. `stat_posix_mode` returns `None` only for a missing path.
7. If the host or filesystem cannot faithfully expose or apply POSIX mode bits, `mkdir_with_mode` and `stat_posix_mode` return `IOError.Unsupported`.

This design deliberately avoids a public `chmod` or `umask` surface. Backends may still use host chmod/stat APIs internally to achieve the required semantics.

### 3. Why not extend `FileStat`

Adding permissions directly to `FileStat` looks attractive, but it creates a public shape change in a widely exposed struct. A separate `stat_posix_mode` call is lower risk:

1. It avoids breaking existing destructuring of `FileStat`.
2. It lets unsupported permission views fail explicitly without overloading `FileStat` with host-specific option semantics.
3. It keeps the permission API clearly scoped to POSIX-style mode bits.

### 4. Backend implementation plan

#### JVM eval

Implement new evaluator externals in `Predef.scala` and register them through `core/.jvm/.../PredefIoCorePlatform.scala`.

Implementation outline:

1. Add `PosixMode` extraction and validation helpers in the evaluator.
2. Implement `prog_core_stat_posix_mode` using NIO unix or posix attribute views when available, with `NOFOLLOW_LINKS`.
3. Implement `prog_core_mkdir_with_mode` with a manual segment walk for `recursive=True` so the runtime can distinguish newly created parents from an existing leaf.
4. For newly created parents, ensure owner write and execute bits are present if required by `mkdir -p` semantics.
5. For a newly created leaf, apply the requested mode after creation so the final result is not masked by process umask.
6. On filesystems without a usable unix or posix permission view, return `IOError.Unsupported` for the new APIs.

#### Python runtime

Update `test_workspace/ProgExt.py` and the externals mapping.

Implementation outline:

1. Replace the current `os.makedirs(path)` fast path with a component-by-component walk so Python matches JVM and C behavior when the leaf already exists.
2. Implement `stat_posix_mode` as `os.lstat(path).st_mode & 0o7777` on POSIX hosts.
3. Implement `mkdir_with_mode` via `os.mkdir` plus `os.chmod` for newly created directories.
4. For recursive parent creation, add owner write and execute bits after creation if the host masked them out.
5. On Windows, reject explicit mode requests up front with `IOError.Unsupported`, because `os.mkdir(..., mode)` is ignored there.

#### C backend

Extend `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and the header.

Implementation outline:

1. Add a new exported entry point for `mkdir_with_mode` and one for `stat_posix_mode`.
2. Reuse the current recursive walker, but teach it to distinguish parents from the final leaf.
3. Use `lstat` to read mode bits and `chmod` to repair parent `u+wx` semantics and to apply the final requested leaf mode.
4. Keep the existing `mkdir` entry point as-is for current callers.
5. For future `_WIN32` builds of the C runtime, compile the new entry points but return `IOError.Unsupported` until the broader Windows C runtime can honor exact mode semantics.

#### Scala.js and Node parity

The issue names JVM eval, Python, and the current C backend, but the public `Bosatsu/IO/Core` surface still needs to stay coherent for Node-backed test and eval flows.

Implementation outline:

1. Add the same externals in `core/.js/.../PredefIoCorePlatform.scala`.
2. On POSIX-like Node hosts, use `fs.lstatSync().mode`, `fs.mkdirSync`, and `fs.chmodSync`.
3. On `win32`, mirror the Python behavior and return `IOError.Unsupported` for explicit mode requests.

## Detailed Implementation Plan

1. Update `test_workspace/Bosatsu/IO/Core.bosatsu` to export `PosixMode`, the helper constructors, `mkdir_with_mode`, and `stat_posix_mode`.
2. Keep `mkdir` unchanged and document that it remains the portable default for callers that do not care about explicit modes.
3. Add JVM evaluator support in `core/src/main/scala/dev/bosatsu/Predef.scala` and register it in `core/.jvm/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala`.
4. Add Node parity in `core/.js/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala`.
5. Add Python implementations in `test_workspace/ProgExt.py` and wire them through `test_workspace/Prog.bosatsu_externals`.
6. Add C runtime implementations in `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.c` and `c_runtime/bosatsu_ext_Bosatsu_l_IO_l_Core.h`.
7. Add package-level tests to `test_workspace/Bosatsu/IO/Core.bosatsu` so the same cases run through existing JVM, Node, Python, and C test flows.
8. Add focused JVM eval regressions in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` to prove the new externals are registered and callable under evaluator execution.

## Testing Strategy

1. Add a `Bosatsu/IO/Core` test that `mkdir(path, True)` succeeds when the leaf directory already exists. This closes the current Python drift.
2. Add a `Bosatsu/IO/Core` test that `mkdir_with_mode` plus `stat_posix_mode` round-trips a common mode such as `0750` on supported hosts.
3. Add a recursive create test that verifies the leaf gets the requested mode and that created parents remain traversable.
4. Add a test that unsupported hosts return `IOError.Unsupported` instead of silently succeeding with the wrong mode.
5. Keep the tests written in Bosatsu where possible so existing `tool test`, Python transpile tests, C transpile tests, and Node test flows all exercise the same contract.
6. Add a JVM-specific evaluator regression in `EvaluationTest.scala` because the issue explicitly includes JVM eval.

## Acceptance Criteria

1. `Bosatsu/IO/Core` exposes additive `PosixMode`, `mkdir_with_mode`, and `stat_posix_mode` APIs.
2. Existing callers of `mkdir(path, recursive)` continue to compile unchanged.
3. Recursive create succeeds when the target directory already exists on JVM, Python, C, and Node-backed runs.
4. On POSIX-capable hosts, `mkdir_with_mode(path, False, mode)` creates a leaf directory whose final low permission bits equal `mode`.
5. On POSIX-capable hosts, `mkdir_with_mode(path, True, mode)` applies `mode` only to the newly created leaf and keeps newly created parents traversable.
6. If `recursive=True` and the leaf already exists, `mkdir_with_mode` succeeds without mutating that existing directory.
7. `stat_posix_mode` returns `None` for a missing path and otherwise reports the same mode bits used for a newly created leaf on supported hosts.
8. On hosts or filesystems that cannot faithfully honor POSIX modes, the new APIs return `IOError.Unsupported` instead of silently ignoring the request.
9. Existing Python, C, Node, and JVM eval test flows cover the new behavior.

## Risks and Mitigations

1. Risk: changing `FileStat` would create avoidable source churn.
Mitigation: add `stat_posix_mode` as a separate API instead of extending `FileStat`.

2. Risk: Python backend continues to drift from JVM and C for recursive create semantics.
Mitigation: replace `os.makedirs` with explicit segment walking and cover the existing-directory case in shared Bosatsu tests.

3. Risk: hosts without real POSIX permission support silently ignore requested modes.
Mitigation: detect unsupported hosts or attribute views and return `IOError.Unsupported` before pretending the request succeeded.

4. Risk: JVM permission APIs differ between unix and non-unix file attribute views.
Mitigation: use unix or posix attribute views only when they are actually present and scope exact mode assertions to supported hosts.

5. Risk: partial recursive creation leaves some directories behind if a later chmod or create step fails.
Mitigation: preserve current partial-create behavior, keep operations ordered and idempotent, and avoid rollback logic in this issue.

## Rollout Notes

1. Land this as an additive runtime-surface change, not a replacement of the existing `mkdir` API.
2. Prefer package-level Bosatsu tests in `Bosatsu/IO/Core` so the same assertions cover JVM, Node, Python, and C with the repository's existing commands.
3. Treat Windows explicit-mode support as typed `Unsupported` initially; widen support later only when a backend can prove exact semantics.
4. A follow-up utility PR can implement `mkdir -m` string parsing and verbose output on top of the new mode-aware API.

## Alternatives Considered

1. Extend `FileStat` with permissions.
Rejected because it is a broader public API shape change than this issue needs.

2. Add public `chmod` and `umask` APIs.
Rejected because `umask` is awkward to expose safely, and `mkdir_with_mode` can hide the required backend-specific chmod work.

3. Replace the existing `mkdir` signature instead of adding a new function.
Rejected because it creates source churn for callers that only need the current portable behavior.

4. Accept raw mode strings directly in the runtime API.
Rejected because it bakes CLI syntax into `IO/Core` and would duplicate symbolic-mode parsing logic across every backend instead of keeping parsing as pure Bosatsu code.
