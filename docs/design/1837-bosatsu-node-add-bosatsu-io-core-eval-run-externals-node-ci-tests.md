---
issue: 1837
priority: 3
touch_paths:
  - docs/design/1837-bosatsu-node-add-bosatsu-io-core-eval-run-externals-node-ci-tests.md
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/.jvm/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - core/.js/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - core/.js/src/test/scala/dev/bosatsu/PredefIoCoreNodeTest.scala
  - .github/workflows/ci.yml
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Ci.bosatsu
depends_on: []
estimated_size: M
generated_at: 2026-02-28T02:59:55Z
---

# Issue #1837 Design: bosatsu_node IO/Core eval --run externals + Node CI tests

_Issue: #1837 (https://github.com/johnynek/bosatsu/issues/1837)_

## Summary

Design doc content proposing runtime-specific eval externals, a Scala.js Node IO/Core external backend, and explicit `bosatsu_node` CI coverage for `tool eval --run` and `lib eval --run`.

---
issue: 1837
title: bosatsu_node: add Bosatsu/IO/Core eval --run externals + node CI tests
status: proposed
base_branch: main
touch_paths:
  - docs/design/1837-bosatsu-node-add-bosatsu-io-core-eval-run-externals-node-ci-tests.md
  - core/src/main/scala/dev/bosatsu/Predef.scala
  - core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/.jvm/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - core/.js/src/main/scala/dev/bosatsu/PredefIoCorePlatform.scala
  - core/.js/src/test/scala/dev/bosatsu/PredefIoCoreNodeTest.scala
  - .github/workflows/ci.yml
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Ci.bosatsu
depends_on: []
estimated_size: L
generated_at: 2026-02-28
---

# Issue #1837 Design: bosatsu_node IO/Core eval --run externals and Node CI tests

Issue: #1837 (https://github.com/johnynek/bosatsu/issues/1837)
Base branch: `main`

## Summary

`bosatsu_node` currently fails to evaluate programs that depend on `Bosatsu/IO/Core` because eval paths build `LibraryEvaluation` with `Predef.jvmExternals`, while Scala.js does not register IO/Core externals. This design introduces runtime-specific eval externals, adds a Node-backed Scala.js IO/Core external implementation, and adds explicit Node CI checks for `lib eval --run` and `tool eval --run`.

## Problem statement

Current behavior:

1. `tool eval` and `lib eval` use `Predef.jvmExternals` directly.
2. `Predef.addIoCoreExternals` currently gates registration on `Platform.isScalaJvm`.
3. On Scala.js, no `Bosatsu/IO/Core` externals are registered, so evaluation fails with missing external errors such as `path_sep`.

Impact:

1. `bosatsu_node` can run pure `Bosatsu/Prog::Main` values.
2. `bosatsu_node` cannot run IO mains that import/use `Bosatsu/IO/Core`.
3. Node CI does not currently include explicit eval-run coverage for this path.

## Goals

1. Make eval runtime externals backend-aware instead of hard-wired to JVM.
2. Provide Scala.js Node implementations for `Bosatsu/IO/Core` externals used by eval-run programs.
3. Preserve rich `IOError` context and tag mapping parity with JVM behavior.
4. Verify `OpenMode.CreateNew` and temp file/dir behavior on Node.
5. Add explicit `bosatsu_node` CI checks for `tool eval --run` and `lib eval --run`.
6. Preserve existing JVM behavior and existing pure `Prog::Main` forwarding checks.

## Non-goals

1. No change to `Bosatsu/IO/Core` public Bosatsu API shape.
2. No change to C runtime or Python runtime behavior.
3. No behavior change to JVM `Stdio.UseHandle` support (remains unsupported).
4. No broad refactor of all command modes in this issue beyond eval-run critical paths.

## Proposed architecture

### 1) Runtime-specific eval externals

Add a `Predef` entrypoint for evaluation externals (for example `Predef.evalExternals`) that resolves by runtime target.

1. JVM: returns current behavior (IO/Core + Bytes + predef externals).
2. Scala.js Node: returns the same external surface, but IO/Core bindings come from a Node-specific implementation.

This decouples evaluation from `jvmExternals` and prevents platform drift at call sites.

### 2) Platform split for IO/Core external bindings

Introduce a platform shim for IO/Core externals with same exported names and arities on JVM and JS.

1. `core/.jvm/.../PredefIoCorePlatform.scala` delegates to current JVM behavior.
2. `core/.js/.../PredefIoCorePlatform.scala` binds Node-backed implementations.
3. `Predef.scala` builds externals through this platform shim.

This keeps registration logic centralized while allowing backend-specific implementations.

### 3) Scala.js IO/Core implementation contract

Scala.js runtime behavior should be explicit for both Node and browser environments.

Node backend should support the full externally visible symbol set used by `Bosatsu/IO/Core` evaluation, including:

1. `path_sep`, std handles (`stdin`, `stdout`, `stderr`).
2. UTF-8 and bytes read/write operations.
3. File open/close/flush, list/stat/mkdir/remove/rename.
4. Temp file/dir creation.
5. Env/time/sleep helpers.
6. Process APIs (`spawn`, `wait`) with explicit unsupported paths reported as `IOError.Unsupported` when parity is not feasible in sync evaluator constraints.

Behavior requirements:

1. `OpenMode.CreateNew` must map to atomic create semantics and return `AlreadyExists` on collision.
2. Temp file/dir behavior must match JVM validation and context richness.
3. Returned path values should keep existing normalized path string behavior.
4. `runProgMain` output capture semantics must remain intact for stdout/stderr.

Browser (non-Node Scala.js) behavior requirements:

1. `Bosatsu/IO/Core` externals must not fail by missing external registration.
2. IO/Core operations that require OS/process/file capabilities must return `IOError.Unsupported` through the `Prog[IOError, _]` API (not throw raw JS/runtime exceptions).
3. Unsupported contexts should include actionable text indicating the API requires a Node/runtime host (for example, browser runtime has no filesystem/process handle support).
4. This unsupported-path behavior should be covered by Scala.js tests to prevent regressions where browser mode crashes instead of returning typed `IOError`.

### 4) IOError mapping parity

Node errors must map to existing `IOError` tags with context-rich messages, not generic failures.

1. Map common OS error codes (`ENOENT`, `EACCES`, `EEXIST`, `ENOTDIR`, `ENOTEMPTY`, `EBADF`, `EPIPE`, `EINTR`, etc.) to existing known tags.
2. Unknown codes map to `IOError.Other(context, code, message)`.
3. Keep context strings actionable and aligned with JVM style (`open_file(...)`, `create_temp_file(...)`, etc.).

### 5) Command wiring changes

Update eval call sites to use runtime-specific externals:

1. `tool_command/EvalCommand.scala`.
2. `library/Command.scala` eval subcommand path.

This removes the current `jvmExternals` hard-wire from the two failing entry points.

### 6) Node CI coverage additions

Add explicit Node eval-run checks in `.github/workflows/ci.yml` under `testWithNode`.

1. `lib eval --run`: `./bosatsu_node lib eval --repo_root . --name core_alpha --main Bosatsu/IO/CreateModeMain --run` and assert output includes `create_mode_test:ok`.
2. `tool eval --run`: `./bosatsu_node tool eval --run --main Bosatsu/IO/CreateModeMain --search --package_root test_workspace --input test_workspace/Bosatsu/IO/CreateModeMain.bosatsu` and assert output includes `create_mode_test:ok`.
3. Keep existing pure `Prog::Main` forwarding checks in JVM unit tests unchanged.

Because workflow source is modeled in `test_workspace/Bosatsu/Example/Json/Github/Workflows`, update those files with the same command additions to avoid source/generated drift.

## Detailed implementation plan

1. Add runtime eval externals API in `Predef.scala` and route eval commands to it.
2. Introduce platform IO/Core shim files in `.jvm` and `.js` source roots.
3. Keep JVM implementation behavior identical by delegating to current logic.
4. Implement Node IO/Core externals in `.js` using Node APIs with the existing Bosatsu value contracts.
5. Implement/port IOError code mapping and context builders for Node path.
6. Ensure CreateNew and temp file/dir semantics match JVM contract.
7. Add Scala.js-side tests for Node IO/Core semantics (`CreateNew`, temp path creation, argument validation).
8. Update `EvalCommand.scala` and `library/Command.scala` to use runtime eval externals.
9. Add Node CI eval-run checks in `.github/workflows/ci.yml` `testWithNode` job.
10. Mirror workflow changes in `test_workspace` workflow source files.
11. Run `coreJVM/test`, `coreJS/test`, and Node CI command sequence locally where possible.
12. Validate no regression in existing CLI behavior and output formatting.

## Testing strategy

### Runtime tests

1. Node-specific test covers `open_file(..., CreateNew)` succeeds once then yields `AlreadyExists`.
2. Node-specific test covers `create_temp_file` and `create_temp_dir` in default and explicit dirs.
3. Node-specific test covers invalid temp args and context-rich `InvalidArgument`.

### Command-level Node checks

1. `bosatsu_node lib eval --run` on `Bosatsu/IO/CreateModeMain` exits `0` and prints `create_mode_test:ok`.
2. `bosatsu_node tool eval --run` equivalent exits `0` and prints `create_mode_test:ok`.
3. Existing pure `Prog::Main` forwarding tests stay green in current JVM suite.

## Acceptance criteria

1. `bosatsu_node` no longer fails with missing `Bosatsu/IO/Core` externals during eval-run.
2. `tool eval --run` works for IO mains that depend on `Bosatsu/IO/Core`.
3. `lib eval --run` works for IO mains that depend on `Bosatsu/IO/Core`.
4. `OpenMode.CreateNew` lock-file behavior is verified on Node backend.
5. Temp file and temp dir creation behavior is verified on Node backend.
6. Node implementation returns structured `IOError` values with useful context.
7. Scala.js browser runtime returns `IOError.Unsupported` for unsupported IO/Core operations instead of missing externals or untyped runtime failures.
8. Existing JVM behavior remains unchanged for the same commands.
9. Existing pure `Prog::Main` argument pass-through checks remain passing.
10. `testWithNode` CI job includes explicit eval-run IO checks.
11. Workflow model files and `.github/workflows/ci.yml` stay in sync.

## Risks and mitigations

1. Risk: Node/JVM semantic drift in file and error behavior.
Mitigation: parity-focused tests for CreateNew/temp/error-context and explicit error-code mapping table.

2. Risk: Resource leaks from Node file descriptors/handles.
Mitigation: explicit close semantics in handle wrappers and tests that exercise repeated open/close paths.

3. Risk: Process API edge cases under synchronous evaluator constraints.
Mitigation: return `IOError.Unsupported` with context for unsupported combinations instead of silent misbehavior.

4. Risk: CI/workflow source drift.
Mitigation: update both `.github/workflows/ci.yml` and the `test_workspace` workflow source files in the same PR.

## Rollout notes

1. Roll out in one PR with runtime externals change, Node IO/Core implementation, and CI checks together.
2. Validate locally with `coreJS/test` and manual `bosatsu_node` eval-run commands before merge.
3. Monitor `testWithNode` as the primary regression signal.
4. If rollback is required, revert eval call sites to prior behavior and remove the added Node eval-run CI assertions while keeping JVM behavior untouched.
