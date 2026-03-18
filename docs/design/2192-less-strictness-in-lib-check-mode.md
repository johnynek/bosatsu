---
issue: 2192
priority: 3
touch_paths:
  - docs/design/2192-less-strictness-in-lib-check-mode.md
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/tool/LintMode.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/UnusedLetCheck.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-18T02:13:50Z
---

# Add `--warn` and `--lax` lint modes to `lib check` and `lib test`

_Issue: #2192 (https://github.com/johnynek/bosatsu/issues/2192)_

## Summary

Add non-strict lint handling for `lib check` and `lib test` by demoting an explicit set of lint-only diagnostics, replaying typed lints after successful compilation so cache hits still surface them, and keeping all soundness-critical checks fatal.

## Context

Issue #2192 asks for a less strict edit/test loop in `lib check`, with the same behavior likely useful in `lib test`.

On `main`, several diagnostics that are better described as lints are still fatal because they are produced inside the normal compile pipeline. The important ones are:

1. `PackageError.UnusedImport`
2. `PackageError.UnusedLetError`
3. `PackageError.UnusedLets`
4. `PackageError.ShadowedBindingTypeError`
5. `PackageError.TotalityCheckError` when the underlying typed-totality error is `UnreachableBranches`
6. `PackageError.TotalityCheckError` when the underlying error is structural-pattern lint (`InvalidStrPat` or `MultipleSplicesInPattern`)

At the same time, issue #2192 explicitly does not want to relax soundness-critical checks such as recursion safety or non-total matches. The design therefore needs a precise split between hard errors and lint-only diagnostics.

A second constraint is caching. `lib check` and `lib test` both go through cached typed-package compilation. If warnings only exist on fresh compiles and disappear on cache hits, the new modes will be unreliable. Strict mode also cannot be bypassed just because a previous `--warn` or `--lax` run populated cache artifacts.

## Goals

1. Add `--warn` and `--lax` to `lib check`.
2. Add the same options to `lib test`.
3. Keep strict mode as the default, with current behavior unchanged when no new flag is passed.
4. Demote only a small, explicit set of lint diagnostics.
5. Keep all soundness-critical diagnostics fatal in every mode.
6. Preserve mixed-diagnostic reporting when hard errors and lint diagnostics coexist.
7. Keep warning behavior stable across cache misses and cache hits.

## Non-goals

1. No new non-strict flags for `lib build`, `publish`, or other commands in this issue.
2. No relaxation of recursion checking.
3. No relaxation of `NonTotalMatch` totality failures.
4. No machine-readable warnings format.
5. No attempt to reclassify every existing `PackageError`; only clearly lint-like cases are in scope.

## Postponable Checks

Within the current architecture, the maximal safe v1 set of postponable checks is:

1. `UnusedImport`
2. `UnusedLetError`
3. `UnusedLets`
4. `ShadowedBindingTypeError`
5. `TotalityCheckError(UnreachableBranches)`
6. `TotalityCheckError(InvalidPattern(InvalidStrPat | MultipleSplicesInPattern))`

These are postponable because they do not affect type soundness, recursion soundness, or totality of successful matches.

The following remain hard in all modes:

1. Source conversion and parse-derived semantic errors.
2. Unknown or invalid imports/exports.
3. Duplicate package resolution failures.
4. Kind, type, variance, and private-type-escape errors.
5. Recursion errors.
6. `TotalityCheckError(NonTotalMatch)`.
7. Any invariant-style totality failure such as `UnknownConstructor` or `ArityMismatch` if it ever escapes the typed pipeline.
8. Library config validation and build/runtime readiness failures.

`DuplicatedImport` is intentionally left hard in this issue even though it is stylistic, because it is produced during import unification. Demoting it cleanly would require a separate decision about canonical import precedence during resolution.

## Proposed CLI Surface

1. `lib check --warn` and `lib test --warn` print lint diagnostics as warnings and continue unless a hard error exists.
2. `lib check --lax` and `lib test --lax` suppress the same lint diagnostics entirely and continue unless a hard error exists.
3. `--warn` and `--lax` are mutually exclusive.
4. With no new flag, behavior remains strict.
5. `lib build` and the rest of the CLI stay strict in this issue.

## Architecture

### 1. Make lint diagnostics non-blocking inside compilation

Today, several lint-producing checks return `Validated` or left-only `Ior` values, so they block package compilation even when the underlying program is otherwise well typed.

Refactor the lint-producing sites so they split diagnostics into hard and lint subsets before joining the overall result:

1. In `Package.inferBodyUnopt`, `checkExprDagBindables` should stop treating unused imports as blocking. Unknown exports remain hard.
2. `UnusedLetCheck` should become non-blocking for successful compilation.
3. `ShadowedBindingTypeCheck` should become non-blocking for successful compilation.
4. Typed totality should be split into:
   1. hard: `NonTotalMatch`
   2. lint: `UnreachableBranches`, `InvalidStrPat`, `MultipleSplicesInPattern`
5. If a phase produces both hard and lint diagnostics, keep both in the accumulated diagnostic set so the caller can still render both sections on failure.

The target behavior is:

1. lint-only packages can still produce a typed package result,
2. hard errors still prevent a typed package result,
3. lint diagnostics are not lost when hard errors also exist.

`PackageCustoms.assemble` already has the right general shape for successful packages because it can return `Ior.Both` with a package and additional diagnostics.

### 2. Replay typed lints after successful compilation

Demoting lint diagnostics during compilation is not enough, because cache hits currently only return the inferred package bytes, not the original lint list.

To keep cache behavior correct, successful compilation must be followed by a canonical typed lint replay over the resulting `PackageMap.Inferred`.

This replay should be the authoritative success-path lint source for all modes, on both cache misses and cache hits.

The replay should use:

1. a typed or shared traversal in `UnusedLetCheck` so local unused-value diagnostics can be recomputed from `TypedExpr[Declaration]`,
2. exposed lint-only helpers in `PackageCustoms` for unused imports and unused top-level values,
3. `ShadowedBindingTypeCheck` over typed lets,
4. `TotalityCheck`, filtered down to its lint-only variants.

This replay is what guarantees:

1. `--warn` still prints warnings on repeated cached runs,
2. strict mode still fails after a previous `--warn` or `--lax` run populated cache,
3. warning behavior does not depend on whether the compiler got a cache hit.

### 3. Make `CompilerApi` lint-mode aware

Add a diagnostics-aware entry point in `CompilerApi`, for example `typeCheckWithLintMode`, and keep the existing strict `typeCheck` as a wrapper around `Strict` mode.

`CompilerApi` should own three responsibilities:

1. partition accumulated diagnostics into hard errors and lint warnings,
2. run typed lint replay after any successful compile result,
3. render warnings and errors according to the selected lint mode.

Behavior by mode:

1. `Strict`
   1. if hard errors exist, fail as today,
   2. if only lint diagnostics exist, fail as today,
   3. keep existing strict commands working without CLI-surface changes.
2. `Warn`
   1. if hard errors exist, render warnings first, then hard errors, and exit non-zero,
   2. if only lint diagnostics exist, return success and emit warnings on stderr.
3. `Lax`
   1. if hard errors exist, render only hard errors and exit non-zero,
   2. if only lint diagnostics exist, return success and emit no warning block.

For render stability, deduplicate by `PackageError` equality before building the warning/error docs, so miss-time diagnostics and replay diagnostics do not double-render.

### 4. Wire lint modes only into `lib check` and `lib test`

`library/Command.scala` should add a small shared parser for `--warn` and `--lax` and thread the resulting mode only through the two affected commands.

Required call-path changes:

1. `lib check` should pass lint mode into `CheckState.packageMap` and `check`.
2. `lib test` should pass lint mode into the `build` path used for test transpilation/execution.
3. `lib build`, `doc`, `eval`, `json`, and the `tool` subcommands should continue to use the strict wrapper.

This keeps the user-facing surface narrowly scoped while still making the compiler core ready for a later follow-up if `tool check` or `tool test` should gain the same flags.

### 5. Warning rendering

Reuse existing `PackageError.message` bodies. Only the container and summary should change.

Requirements:

1. warnings are clearly labeled as warnings, not as type errors,
2. warnings go to stderr,
3. `lib check` still produces no normal stdout payload,
4. `lib test --warn` emits warnings before transpile/test output begins,
5. the existing strict multi-diagnostic error formatting remains the model for the strict path.

## Implementation Plan

1. Add a `LintMode` enum plus a small CLI parser helper, likely in a new shared file under `core/src/main/scala/dev/bosatsu/tool/`.
2. Refactor `Package.inferBodyUnopt` so lint-only diagnostics no longer block typed-package production.
3. Split or expose lint-only helpers in `PackageCustoms` for unused imports and unused top-level values.
4. Extend `UnusedLetCheck` with a typed replay path, or refactor it so untyped and typed traversals share one implementation strategy.
5. Add `CompilerApi.typeCheckWithLintMode` and keep the existing strict `typeCheck` as a wrapper.
6. Update `library/Command.scala` to parse and pass lint mode in `lib check` and `lib test` only.
7. Update docs to mention `--warn` and `--lax` on the relevant library commands.
8. Add command-level tests for strict, warn, lax, mixed diagnostics, and cache-hit parity.

## Acceptance Criteria

1. `lib check` accepts `--warn` and `--lax`.
2. `lib test` accepts `--warn` and `--lax`.
3. `--warn` and `--lax` are mutually exclusive.
4. Default strict behavior is unchanged when neither flag is present.
5. The demoted lint set is exactly the explicit list in this design, and no soundness-critical diagnostics are demoted.
6. `RecursionError` and `NonTotalMatch` remain fatal in all modes.
7. `--warn` exits successfully when the only diagnostics are lint diagnostics and emits them as warnings.
8. `--lax` exits successfully when the only diagnostics are lint diagnostics and suppresses them.
9. If hard errors and lint diagnostics coexist, `--warn` still exits non-zero because of the hard errors and still shows the warning block.
10. `lib test --warn` and `lib test --lax` still execute tests when the only compile-time diagnostics are lint diagnostics.
11. Warning behavior is stable across cache hits and cache misses.
12. A strict run after a prior `--warn` or `--lax` run still fails on the same lint diagnostics.
13. `lib build` remains strict and gains no new flags in this issue.

## Risks And Mitigations

1. Risk: cache hits silently lose warnings.
   Mitigation: typed lint replay after every successful compile result.

2. Risk: miss-time lint collection and replay-time lint collection drift apart.
   Mitigation: use the same `PackageError` variants in both paths and add parity tests that run the same source twice, once cold and once warm.

3. Risk: duplicate warning rendering from precheck plus replay.
   Mitigation: deduplicate `PackageError` values before rendering.

4. Risk: typed unused-let replay does not exactly match the existing untyped lint behavior.
   Mitigation: add focused tests covering lets, lambda args, pattern bindings, and top-level defs on successfully compiled programs.

5. Risk: users find warning-plus-error output noisy on `--warn` failure cases.
   Mitigation: keep warnings in a clearly separated block and preserve the current strict error block format underneath.

6. Risk: the added replay pass slows successful `lib check` and `lib test` runs.
   Mitigation: keep replay limited to the explicit lint set, and reuse already-typed package data instead of reparsing sources.

## Rollout Notes

1. Land the compiler plumbing first so strict mode still behaves exactly as today through the existing `CompilerApi.typeCheck` wrapper.
2. Then expose `--warn` and `--lax` on `lib check` and `lib test`.
3. Update user docs in the same PR so the new modes are discoverable.
4. No migration is required because strict mode remains the default.
5. If the lint set proves too broad or too narrow, adjust the central classifier rather than adding more ad hoc command-specific conditionals.
6. A later follow-up can expose the same lint modes on `tool check` and `tool test` without reworking the compiler internals again.
