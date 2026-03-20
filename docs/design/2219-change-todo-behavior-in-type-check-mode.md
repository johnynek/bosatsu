---
issue: 2219
priority: 3
touch_paths:
  - docs/design/2219-change-todo-behavior-in-type-check-mode.md
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - docs/src/main/paradox/writing_bosatsu_5_minutes.md
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-20T20:08:18Z
---

# Change `todo` behavior across strict, warn, and lax check modes

_Issue: #2219 (https://github.com/johnynek/bosatsu/issues/2219)_

## Summary

Use emit-mode semantics for strict checks, keep `todo` only in relaxed `lib check` paths, and add a warn-only `todo` usage lint that stays stable on cache hits while remaining absent in `--lax` and all execution paths.

## Context

`todo` currently comes from the internal type-check-only predef path added in issue `#1173`. In the current codebase, `Package.scala` builds two internal predef variants, `PackageMap.scala` injects the variant selected by `CompileOptions.mode`, and both `tool check` and `lib check` currently choose `CompileOptions.TypeCheckOnly`. As a result, strict check mode resolves `Bosatsu/Predef::todo` before lint mode is even considered.

Issue `#2192` added `LintMode.Strict`, `LintMode.Warn`, and `LintMode.Lax`, but that mode is applied later in `CompilerApi`. The compiler can now demote a fixed set of diagnostics to warnings or suppress them, yet `todo` availability is still decided upstream by compile mode alone. That leaves an inconsistent user model: the default check path is named strict, but it still allows an unsound placeholder that build and test reject.

The desired behavior is:

1. Strict check rejects built-in `todo`.
2. `lib check --warn` still accepts built-in `todo`, but reports it as a warning.
3. `lib check --lax` still accepts built-in `todo`, and does not run the new `todo` warning pass.
4. Any command that emits or executes code continues to reject built-in `todo`.

## Goals

1. Make strict `tool check` and strict `lib check` reject built-in `todo`.
2. Preserve a relaxed edit loop in `lib check --warn` and `lib check --lax`.
3. Add a dedicated warning for built-in `todo` usage in `lib check --warn`.
4. Keep `lib check --lax` permissive without paying the cost of the new `todo` warning pass.
5. Keep `lib test`, `tool show`, `tool eval`, `lib build`, and similar emit or run flows unchanged.
6. Preserve warm-cache and cold-cache behavior parity.
7. Update user-facing guidance so the new behavior is discoverable from both docs and error hints.

## Non-Goals

1. No new `--warn` or `--lax` flags for `tool check` in this issue.
2. No change to the signature or internal definition of `todo`.
3. No change to the existing non-`todo` postponable lint set from issue `#2192`.
4. No attempt to lint arbitrary user-defined values named `todo`; this change is only about resolved `Bosatsu/Predef::todo`.
5. No change to runtime or codegen support, since `todo` remains intentionally unavailable on execution paths.

## Current Architecture

1. `core/src/main/scala/dev/bosatsu/Package.scala` defines `predefEmitPackage` and `predefTypeCheckPackage`; only the type-check-only variant exports `todo`.
2. `core/src/main/scala/dev/bosatsu/PackageMap.scala` injects the selected predef variant and implicit predef import list based on `CompileOptions.mode`.
3. `core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala` currently calls `CompilerApi.typeCheck` with `CompileOptions.TypeCheckOnly`.
4. `core/src/main/scala/dev/bosatsu/library/Command.scala` currently routes `lib check` through `CompilerApi.typeCheckWithLintMode`, but it also passes `CompileOptions.TypeCheckOnly` regardless of whether lint mode is strict, warn, or lax.
5. `core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala` partitions postponable diagnostics after compilation and can replay source-derived lints on cache hits, but it does not control whether `todo` exists in scope.
6. `lib test` already compiles with emit-mode options, so it rejects `todo` in strict, warn, and lax modes today.

The key architectural point is that `todo` is currently controlled by compile mode, while warn and lax behavior is controlled by lint mode. Issue `#2219` needs those two decisions to line up differently for strict vs relaxed `lib check`.

## Proposed Design

### 1. Make strict check use emit-mode semantics without enabling optimization

Strict check should stop using `CompileOptions.TypeCheckOnly`.

1. `tool check` should switch from `CompileOptions.TypeCheckOnly` to `CompileOptions.NoOptimize`.
2. Strict `lib check` should also switch from `CompileOptions.TypeCheckOnly` to `CompileOptions.NoOptimize`.
3. `lib check --warn` and `lib check --lax` should continue to use `CompileOptions.TypeCheckOnly`.
4. `lib test`, `lib build`, `tool show`, `tool eval`, and other emit or run commands should remain unchanged.

Using `CompileOptions.NoOptimize` instead of `CompileOptions.Default` is important. The issue is about predef mode and `todo` visibility, not about silently turning `check` into an optimized build. `NoOptimize` keeps current check performance and artifact shape while switching back to the emit-mode predef that does not export `todo`.

### 2. Keep `todo` tied to the existing type-check-only predef

The internal predef mechanism introduced in issue `#1173` does not need to be redesigned.

1. `Package.predefTypeCheckPackage` should continue to be the only internal predef that exports `todo`.
2. `PackageMap.predefCompiledForMode` and related predef caches can remain mode-based.
3. The behavior change should come from command wiring, not from changing the definition of `todo` itself.

This is the lowest-risk path because compile cache identity already includes both `compileOptions.mode` and `compileOptions.optimize`.

### 3. Add a dedicated postponable diagnostic for built-in `todo`

Introduce a new warning-oriented diagnostic, likely `PackageError.TodoUsage`, that records the package name and one or more source regions where resolved built-in `todo` is used.

The new diagnostic should:

1. Be classified as postponable via `PackageError.isPostponable`.
2. Render in `CompilerApi` as its own warning category, for example `todo usage` or `todo placeholder`, instead of being summarized as a type error.
3. Include actionable wording that `todo` is a temporary placeholder and should be removed before returning to strict check or any emit or run flow.

This keeps the new behavior aligned with the existing warn and lax architecture instead of special-casing `todo` as a hard error in one path and invisible in another.

### 4. Detect `todo` from resolved source, not raw text

The warning must only fire for resolved `Bosatsu/Predef::todo` references.

That means the implementation should not do a textual search for the identifier `todo`, because that would produce false positives for:

1. A local binding named `todo`.
2. A renamed import from some other package.
3. Any user-defined helper named `todo`.

Instead, the warning pass should reuse the same effective predef expansion and source-to-program resolution that normal compilation uses, then walk the resulting expressions looking for actual global references to `Bosatsu/Predef::todo`.

A practical split is:

1. Add a small source-level traversal helper in `core/src/main/scala/dev/bosatsu/PackageCustoms.scala` that collects resolved built-in `todo` usages.
2. Expose or refactor the effective-predef source preparation in `core/src/main/scala/dev/bosatsu/PackageMap.scala` so the warning pass uses the same implicit predef injection rules as `typeCheckSources`, including the current explicit-`Predef` override behavior.

### 5. Run the new warning pass only for `LintMode.Warn`

The new `todo` warning should not be wired into the core artifact-producing compile path. It should be added in the diagnostics layer.

Specifically:

1. `CompilerApi.typeCheckWithLintMode` should run the `todo` source scan only when `lintMode == LintMode.Warn` and the selected compile options use `Mode.TypeCheckOnly`.
2. `LintMode.Lax` should keep using `CompileOptions.TypeCheckOnly`, so `todo` remains available, but it should skip the new scan entirely.
3. Strict check should never see the warning because strict check no longer uses the `todo`-exporting predef.

Putting the new pass in `CompilerApi` instead of `Package.inferBodyUnopt` keeps the existing warn and lax cache-sharing model intact. The compiled package is the same for warn and lax relaxed checks; the difference is only whether the warn-only source scan runs and whether warnings are rendered.

### 6. Preserve cache-hit behavior

Warn output must be stable across cache misses and cache hits.

The new `todo` warning should therefore be derived from parsed source on every warn run, not only from cold-path inference diagnostics. That gives the correct behavior for:

1. A first `lib check --warn` run on a cold cache.
2. A repeated `lib check --warn` run that reuses compiled artifacts.
3. A later strict `lib check` run after warn or lax populated the cache.

No compile-cache schema bump should be required, because the cache key already separates strict emit-mode checks from relaxed type-check-only checks, and the warn-only `todo` scan does not affect the compiled artifact.

### 7. Update user-facing guidance and hints

The unknown-name hint in `core/src/main/scala/dev/bosatsu/PackageError.scala` currently says built-in `todo` is available in `tool check` and `lib check`. That becomes incorrect.

Update the hint and docs so they say:

1. Strict `tool check` rejects built-in `todo`.
2. Strict `lib check` rejects built-in `todo`.
3. `lib check --warn` accepts built-in `todo` and warns.
4. `lib check --lax` accepts built-in `todo` without running the new warning pass.
5. `lib test` and all other emit or run commands still reject built-in `todo`.

## Implementation Plan

1. Change `core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala` to compile with `CompileOptions.NoOptimize`.
2. Change `core/src/main/scala/dev/bosatsu/library/Command.scala` so `lib check` selects `CompileOptions.NoOptimize` in `LintMode.Strict` and `CompileOptions.TypeCheckOnly` in `LintMode.Warn` and `LintMode.Lax`.
3. Add `PackageError.TodoUsage` and its message formatting in `core/src/main/scala/dev/bosatsu/PackageError.scala`.
4. Extend `PackageError.isPostponable` and `CompilerApi` warning classification so the new diagnostic is treated as lint and rendered with its own summary label.
5. Add a resolved-source `todo` detector in `core/src/main/scala/dev/bosatsu/PackageCustoms.scala`.
6. Refactor or expose the effective-predef source preparation in `core/src/main/scala/dev/bosatsu/PackageMap.scala` so the detector uses the same implicit predef rules as real compilation.
7. Wire the warn-only `todo` scan into `core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala` and merge its output with the existing lint diagnostics before deduping and rendering.
8. Update command-level tests in `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`.
9. Update any low-level predef or lint regression coverage in `core/src/test/scala/dev/bosatsu/PackageTest.scala` as needed.
10. Update the docs in `docs/src/main/paradox/writing_bosatsu_5_minutes.md` and `docs/src/main/paradox/language_guide.md`.

## Acceptance Criteria

1. `tool check` rejects source that uses built-in `todo`.
2. Strict `lib check` rejects source that uses built-in `todo`.
3. `lib check --warn` succeeds when built-in `todo` is the only relaxed diagnostic and prints a warning block that mentions `todo` usage.
4. `lib check --lax` succeeds on the same source and does not print the `todo` warning.
5. `lib test`, `lib test --warn`, and `lib test --lax` all continue to reject built-in `todo`.
6. Emit or run commands such as `tool show`, `tool eval`, and `lib build` continue to reject built-in `todo`.
7. The new warning only fires for resolved `Bosatsu/Predef::todo`, not for unrelated values named `todo`.
8. Warn output is stable on both cold and warm cache runs.
9. A strict check after a warn or lax cache warmup still rejects built-in `todo`.
10. Existing predef-mode behavior remains intact: emit mode still excludes `todo`, and type-check-only mode still includes it.
11. User-facing docs and CLI hint text describe the new strict, warn, and lax behavior accurately.

## Risks And Mitigations

1. Risk: strict `tool check` and strict `lib check` drift apart again in a later refactor.
   Mitigation: update both call sites in the same change and add command-level regression tests for both.

2. Risk: the `todo` warning scan disagrees with actual compiler resolution and flags the wrong identifier.
   Mitigation: derive the scan input from the same effective-predef source preparation used by compilation instead of matching raw source text.

3. Risk: warn cache hits lose the `todo` warning.
   Mitigation: compute the warning from parsed source each warn run rather than relying on cold compile diagnostics alone.

4. Risk: implementing the warning in the core compile path forces warn and lax to diverge in cache identity or work done.
   Mitigation: keep the new pass in `CompilerApi` and run it only for `LintMode.Warn`.

5. Risk: users who learned the old workflow keep trying `tool check` or strict `lib check` with `todo` and think behavior regressed unexpectedly.
   Mitigation: update the unknown-name hint and the iteration docs in the same PR so the new relaxed path is obvious.

## Rollout Notes

1. Land the command wiring change and the new warn-only `todo` diagnostic in the same PR so strict rejection is paired with a documented relaxed alternative.
2. No migration is needed for emit or test workflows; those paths already reject `todo`.
3. Developers who currently rely on plain `tool check` or plain `lib check` with `todo` will need to switch to `lib check --warn` or `lib check --lax`.
4. No compile-cache schema change is expected, because existing cache identity already includes mode and optimize flags, and the new warning is report-time only.
5. A likely follow-up, but not part of this issue, is deciding whether `tool check` should eventually gain `--warn` and `--lax` for parity with `lib check`.
