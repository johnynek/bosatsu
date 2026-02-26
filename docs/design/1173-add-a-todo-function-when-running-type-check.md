---
issue: 1173
priority: 3
touch_paths:
  - docs/design/1173-add-a-todo-function-when-running-type-check.md
  - core/src/main/scala/dev/bosatsu/CompileOptions.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-25T17:30:12Z
---

# Design for Issue #1173: Type-check-only `todo` in Predef

_Issue: #1173 (https://github.com/johnynek/bosatsu/issues/1173)_

## Summary

Introduce a compile-mode-gated internal Predef variant that exports `todo` only for type-check-only flows, wire that mode through check commands, and keep emit/runtime paths unchanged and safe.

---
issue: 1173
priority: 2
touch_paths:
  - docs/design/1173-add-a-todo-function-when-running-type-check.md
  - core/src/main/scala/dev/bosatsu/CompileOptions.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-25T00:00:00Z
---

# Design add a todo function when running type check

_Issue: #1173 (https://github.com/johnynek/bosatsu/issues/1173)_

Status: proposed  
Date: 2026-02-25  
Base branch: `main`

## Problem statement
Developers sometimes need a temporary escape hatch during refactors: consume values so they look used, then return any type. The proposed shape is `external def todo(ignore: x) -> forall a. a`.

If this function is present in normal runtime compilation, it is unsafe:
1. It has no sound runtime implementation.
2. Codegen and evaluation paths could fail or become unsound if it leaks into emitted artifacts.

The issue requests making `todo` available only in type-check-only mode.

## Goals
1. Make `todo` available in internal Predef only for type-check-only compilation.
2. Keep emit/runtime compilation behavior unchanged.
3. Avoid adding any runtime external implementation for `todo`.
4. Keep the solution compatible with future resident compile caches where mode is part of the cache key.

## Non-goals
1. Do not expose `todo` in regular Predef source or runtime docs.
2. Do not add evaluator/codegen support for `todo`.
3. Do not redesign external def semantics.

## Current behavior
1. Internal Predef is single-flavor and built from `Package.predefPackage`.
2. `PackageMap.typeCheckParsed` injects that one internal Predef when no explicit Predef interface is provided.
3. `CompileOptions` only tracks optimization, not compile mode.
4. `tool check` and `lib check` currently differ mostly by optimization and command path, not by a typed compile mode contract.

## Proposed design

### 1. Add explicit compile mode to `CompileOptions`
Introduce an enum-like mode in compile options:
1. `Emit` (default): normal compile intended for runtime/codegen outputs.
2. `TypeCheckOnly`: compile intended only for type checking and diagnostics.

Keep convenience constructors:
1. `CompileOptions.Default` remains emit mode with optimization enabled.
2. `CompileOptions.NoOptimize` remains emit mode with optimization disabled.
3. Add `CompileOptions.TypeCheckOnly` convenience value (non-emit mode, no optimization by default).

This gives a stable bit that can participate in future cache keys.

### 2. Add internal Predef variants in `Package.scala`
Keep runtime Predef unchanged and introduce a type-check-only variant:
1. Runtime variant: existing behavior.
2. Type-check-only variant: runtime variant plus exported `todo` external.

`todo` signature in type-check-only variant: `external def todo(ignore: x) -> forall a. a`.

Important constraints:
1. Do not modify `core/src/main/resources/bosatsu/predef.bosatsu` to include `todo` globally.
2. Do not add `todo` to `Predef.jvmExternals` or any transpiler intrinsic list.

### 3. Make internal Predef selection mode-aware in `PackageMap.scala`
When internal Predef must be injected, select variant by compile mode:
1. `Emit` uses runtime Predef variant.
2. `TypeCheckOnly` uses type-check-only Predef variant.

Also make predef import expansion mode-aware:
1. Predef implicit import list must be derived from the compiled variant for that mode.
2. This keeps constructor-export expansion behavior and includes `todo` only in type-check mode.

Provide lazy cached compiled predefs per mode to avoid recompiling on every call.

### 4. Propagate mode through type-check entry points
1. `CompilerApi.typeCheck0` empty-input behavior should return the internal predef compiled for the selected mode.
2. `tool check` should use `CompileOptions.TypeCheckOnly`.
3. `lib check` should use `CompileOptions.TypeCheckOnly`.
4. Emit-oriented flows like transpile/build/test/show/doc remain in emit mode.

### 5. Behavior with explicit user-supplied Predef interface
No change:
1. If caller supplies a Predef interface explicitly, internal Predef injection is skipped.
2. In that case, `todo` availability depends on the supplied Predef, not internal defaults.

### 6. Cache-key contract
Document that compile mode must be part of any future resident cache key because the internal Predef interface differs between modes.

## Implementation plan
1. Extend `CompileOptions` with compile mode and add `TypeCheckOnly` convenience constructor.
2. Refactor `Package.predefPackage` creation logic so runtime and type-check-only variants can be built from shared code.
3. Add type-check-only Predef augmentation that appends exported `todo` external definition.
4. Add mode-aware internal predef selection helpers in `PackageMap`.
5. Add mode-aware internal predef import construction in `PackageMap` based on compiled exports of the selected variant.
6. Update `PackageMap.typeCheckParsed` to use mode-aware predef package/import helpers.
7. Update `CompilerApi.typeCheck0` no-input branch to return mode-appropriate internal predef compiled package.
8. Update `tool check` and `lib check` command wiring to pass `CompileOptions.TypeCheckOnly`.
9. Add unit tests for mode-gated `todo` availability and mode isolation.
10. Add command-level integration tests validating positive behavior in check mode and negative behavior in emit mode.

## Testing plan
1. `PackageTest`:
   1. Source using `todo` type-checks under `CompileOptions.TypeCheckOnly`.
   2. Same source fails under emit-mode options due to unresolved `todo`.
2. `ToolAndLibCommandTest`:
   1. `tool check` accepts a package using `todo`.
   2. `lib check` accepts a package using `todo`.
   3. Emit-oriented command path (for example transpile or build) rejects the same source.
3. Internal predef regression:
   1. Runtime internal predef exports do not include `todo`.
   2. Type-check-only internal predef exports include `todo`.

## Acceptance criteria
1. A compile mode concept exists in `CompileOptions` with explicit type-check-only mode.
2. Internal Predef has two variants, and only type-check-only variant exports `todo`.
3. `todo` is not present in runtime Predef exports.
4. No runtime evaluator/codegen external mapping is added for `todo`.
5. `PackageMap.typeCheckParsed` injects internal Predef variant based on compile mode.
6. Predef implicit imports in type-check-only mode include `todo`; emit mode does not.
7. `tool check` and `lib check` use type-check-only mode.
8. Emit-oriented flows remain unchanged and do not accept implicit `todo` from internal Predef.
9. Tests cover both positive (type-check mode) and negative (emit mode) behavior.
10. Design notes document compile mode as part of future cache key identity.

## Risks and mitigations
1. Risk: `todo` accidentally leaks into emit mode.
Mitigation: strict mode gate in `PackageMap` plus negative tests in emit command paths.

2. Risk: mode propagation is incomplete across check entry points.
Mitigation: wire both `tool check` and `lib check`, and test both.

3. Risk: predef import list mismatch if imports are built from wrong predef variant.
Mitigation: build imports from compiled exports of the selected mode-specific predef.

4. Risk: confusion with user-supplied custom Predef interfaces.
Mitigation: keep existing explicit-Predef precedence and document behavior clearly.

5. Risk: future cache poisoning across modes.
Mitigation: document and enforce compile mode in cache key construction when cache is introduced.

## Rollout notes
1. Land compile mode and mode-specific internal Predef machinery first.
2. Wire check commands to type-check-only mode in the same change.
3. Add regression tests before enabling any downstream tooling assumptions.
4. Follow-up for language server integration can reuse the same `CompileOptions.TypeCheckOnly` mode without additional semantics.

## Decision
Proceed with a mode-gated internal Predef augmentation: `todo` exists only in type-check-only mode, with no runtime implementation and no emit-path visibility.
