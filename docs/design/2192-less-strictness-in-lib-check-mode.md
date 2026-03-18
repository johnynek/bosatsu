---
issue: 2192
priority: 3
touch_paths:
  - docs/design/2192-less-strictness-in-lib-check-mode.md
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/tool/LintMode.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - docs/src/main/paradox/writing_bosatsu_5_minutes.md
depends_on: []
estimated_size: M
generated_at: 2026-03-18T03:27:54Z
---

# Add `--warn` and `--lax` lint modes to `lib check` and `lib test`

_Issue: #2192 (https://github.com/johnynek/bosatsu/issues/2192)_

## Summary

Introduce library-only lint modes that demote a fixed set of non-soundness diagnostics to warnings or silence them, while keeping recursion, non-total matches, type/import/export failures, and library validation strict. The design relies on non-blocking `Ior` diagnostics in the compiler, success-path lint replay so cache hits behave the same as cache misses, and `CompilerApi` plus `library.Command` plumbing to expose `--warn` and `--lax` without changing default strict behavior.

## Context

Issue #2192 asks for a less strict edit loop in `lib check`, with the same behavior likely useful in `lib test`. The immediate pain is source-level strictness checks such as unused values that are currently fatal even when the program is otherwise well-typed and the author intends to use the code shortly.

Two existing architectural facts matter here:

1. `lib check` already uses `CompileOptions.TypeCheckOnly`, while `lib test` still uses `CompileOptions.Default` because it emits and runs code. Lint mode must therefore be separate from compile mode.
2. `PackageCustoms.assemble` already has the right shape for non-blocking diagnostics because it can return `Ior.Both` with a compiled package and package-level customs errors. The current strictness comes mostly from `CompilerApi.typeCheck`, which collapses any `Ior.Both` into failure via `strictToValidated`, and from a few checks in `Package.inferBodyUnopt` that still produce hard `Validated` failures.

The design needs a precise split between lint-like diagnostics and soundness-critical diagnostics, and it needs that split to behave the same on cache hits and cache misses.

## Goals

1. Add `--warn` and `--lax` to `lib check`.
2. Add the same options to `lib test`.
3. Keep strict mode as the default, with no behavior change when neither flag is passed.
4. Demote only an explicit, small set of lint-like diagnostics.
5. Keep soundness-critical diagnostics fatal in every mode.
6. Preserve mixed reporting when hard errors and lint diagnostics coexist.
7. Make warning behavior stable across cache misses and cache hits.

## Non-goals

1. No new non-strict flags for `lib build`, `lib publish`, `tool check`, or other commands in this issue.
2. No relaxation of recursion checking.
3. No relaxation of `TotalityCheck.NonTotalMatch`.
4. No change to `todo` availability rules: `lib test` remains an emit/run command, so `todo` stays invalid there.
5. No relaxation of `LibConfig` versioning, dependency-closure, or runtime-readiness validation in this issue.

## Current Architecture

1. `library.Command.check` already calls `CompilerApi.typeCheck` with `CompileOptions.TypeCheckOnly`.
2. `library.Command.test` reaches compilation through the normal build path, which still uses `CompileOptions.Default` so codegen and runtime checks can happen.
3. `Package.inferBodyUnopt` currently treats `UnusedLetError`, `ShadowedBindingTypeError`, and all typed totality failures as blocking.
4. `PackageCustoms.checkExprDagBindables` and `PackageCustoms.assemble` already produce some diagnostics that are naturally lint-like, especially `UnusedImport` and `UnusedLets`.
5. `CompilerApi.typeCheck` converts the final `Ior` result to `Validated`, which means any `Ior.Both` still fails the command.
6. Cached inferred packages do not store warning metadata, so lint diagnostics produced on a cold compile disappear on a warm compile unless they are replayed from the typed package.

## Postponable Checks

The maximal safe v1 set of postponable checks is:

1. `PackageError.UnusedImport`
2. `PackageError.UnusedLetError`
3. `PackageError.UnusedLets`
4. `PackageError.ShadowedBindingTypeError`
5. `PackageError.TotalityCheckError` when the underlying error is `TotalityCheck.UnreachableBranches`

These are postponable because they do not affect type soundness, recursion soundness, or the requirement that successful matches be total.

Invalid-pattern totality errors such as `InvalidStrPat` and `MultipleSplicesInPattern` stay hard. Even though they are style-adjacent, downstream code generation currently assumes validated patterns and can `sys.error` or throw if those forms are allowed through.

The following remain hard in all modes:

1. Parse and source-conversion failures.
2. Unknown or invalid imports and exports.
3. Duplicate package and import-resolution failures.
4. Kind, type, variance, and private-type-escape failures.
5. Recursion errors.
6. `TotalityCheck.NonTotalMatch`.
7. `TotalityCheck.InvalidPattern` failures such as `UnknownConstructor`, `ArityMismatch`, `InvalidStrPat`, and `MultipleSplicesInPattern`.
8. `LibConfig` validation failures and runtime-readiness preflight failures.

`DuplicatedImport` is intentionally left hard in v1 even though it is style-adjacent. It is produced during import unification, and demoting it would require a separate decision about canonical import precedence.

## Proposed Design

### 1. Add an explicit lint mode

Introduce a small `LintMode` enum, likely in `core/src/main/scala/dev/bosatsu/tool/LintMode.scala`, with three values:

1. `Strict`
2. `Warn`
3. `Lax`

`library.Command.scala` should parse `--warn` and `--lax` as mutually exclusive flags and default to `Strict` when neither is present.

This mode is orthogonal to `CompileOptions`:

1. `lib check` continues to use `CompileOptions.TypeCheckOnly`.
2. `lib test` continues to use `CompileOptions.Default`.
3. Lint mode controls only how eligible diagnostics are handled after or during compilation.

### 2. Centralize postponable classification

Add a single shared classifier, likely on `PackageError`, that answers whether a given `PackageError` is postponable in `Warn`/`Lax` modes.

For v1, that classifier should return `true` only for:

1. `UnusedImport`
2. `UnusedLetError`
3. `UnusedLets`
4. `ShadowedBindingTypeError`
5. `TotalityCheckError(UnreachableBranches)`

All compile-time partitioning, success-path replay filtering, and warn/lax rendering should use this one classifier rather than open-coding separate lists in `Package`, `CompilerApi`, and tests.

### 3. Make lint-only diagnostics non-blocking in the compiler pipeline

The compiler should keep collecting lint diagnostics on cold compiles even when later hard failures also exist, but those lint diagnostics should no longer prevent a typed package from existing when the rest of compilation succeeds.

The main refactor point is `Package.inferBodyUnopt`:

1. Keep recursion checking hard.
2. Change `UnusedLetCheck` from a blocking `Validated` contribution to a lint-only `Ior` contribution.
3. Change `ShadowedBindingTypeCheck` from a blocking `Validated` contribution to a lint-only `Ior` contribution.
4. Partition typed totality failures into:
   1. hard: `NonTotalMatch` and all `InvalidPattern(...)` cases
   2. lint: `UnreachableBranches`
5. Combine hard and lint results with `Ior` so lint-only packages can still produce a typed program, while mixed hard-plus-lint packages still preserve both sets of diagnostics.

`PackageCustoms.checkExprDagBindables` should continue to emit `UnusedImport` on cold compiles so mixed failures still report it, but `UnknownExport` remains hard.

`PackageCustoms.assemble` already returns `Ior.Both` for package-level customs. That is the behavior to preserve and reuse rather than replace.

### 4. Replay lint diagnostics from successful typed packages

Making lint diagnostics non-blocking on a cold compile is not enough because cache hits currently return only compiled package artifacts. The success path therefore needs a canonical replay pass that recomputes the lint set from `PackageMap.Inferred`.

This replay should be the authoritative source of warnings for successful compilations, regardless of whether the packages came from cache.

The replay should cover:

1. package-level lint from `PackageCustoms`, exposed through a new lint-only helper rather than rerunning hard customs checks
2. local unused bindings via `UnusedLetCheck.check` on typed lets
3. shadowed-binding lint via `ShadowedBindingTypeCheck.checkLets`
4. typed totality lint, which in v1 is only `UnreachableBranches`

Compiler-side diagnostics from the cold path and replayed diagnostics from the success path should be deduplicated by `PackageError` equality before rendering.

This gives the required behavior:

1. `--warn` still prints warnings on repeated cached runs.
2. `--lax` still suppresses the same warnings on repeated cached runs.
3. A later strict run still fails on the same lint diagnostics even if a previous `--warn` or `--lax` run populated the infer cache.
4. Lint mode does not need to participate in infer-cache keys because it does not change the compiled package artifact.

### 5. Add a diagnostics-aware `CompilerApi` entry point

`CompilerApi` should gain a diagnostics-aware typecheck entry point that returns:

1. compiled packages
2. source-path metadata
3. partitioned hard diagnostics
4. partitioned lint diagnostics

The existing `typeCheck` method stays as the strict wrapper used by existing callers.

Behavior by lint mode:

1. `Strict`
   1. if hard errors exist, fail
   2. if only lint diagnostics exist, fail
2. `Warn`
   1. if only lint diagnostics exist, emit a warning block on stderr and return success
   2. if both hard and lint diagnostics exist, emit warnings first, then fail with the hard-error block
3. `Lax`
   1. if only lint diagnostics exist, return success and emit nothing
   2. if hard diagnostics exist, fail and do not render the suppressed lint block

Warning rendering should reuse existing `PackageError.message` bodies, but the summary labels should be warning-oriented rather than reusing error labels blindly. In particular, `ShadowedBindingTypeError` should not be summarized as a `type error` inside a warning block.

### 6. Wire lint mode into `lib check` and `lib test` only

`library.Command.scala` should thread `LintMode` only through the two affected library commands.

Required call-path changes:

1. `lib check` passes `LintMode` into `CheckState.packageMap` and `check`.
2. `lib test` passes `LintMode` into the build path used before transpilation and test execution.
3. Other library commands continue to use strict behavior.
4. `tool` commands continue to use the existing strict wrapper.

This keeps the CLI surface narrow while still putting the reusable logic in `CompilerApi` for a future follow-up if `tool check` or `tool test` should gain the same flags.

## Implementation Plan

1. Add `LintMode` and shared `--warn` / `--lax` parsing in `library.Command.scala`.
2. Add a shared `PackageError` postponability classifier for the explicit v1 lint set.
3. Refactor `Package.inferBodyUnopt` so the postponable lint set contributes via `Ior.Both` rather than blocking `Validated` failures.
4. Expose a lint-only replay helper from `PackageCustoms` for package-level lints.
5. Add a diagnostics-aware `CompilerApi` path that partitions hard and lint diagnostics and performs replay on successful package maps.
6. Keep `CompilerApi.typeCheck` as the strict compatibility wrapper.
7. Thread lint mode through `lib check` and `lib test` only.
8. Update docs that describe the `lib check` / `lib test` iteration loop.
9. Add command-level tests for strict, warn, lax, mixed diagnostics, and cache-hit parity.

## Acceptance Criteria

1. `lib check` accepts `--warn` and `--lax`.
2. `lib test` accepts `--warn` and `--lax`.
3. `--warn` and `--lax` are mutually exclusive.
4. Default strict behavior is unchanged when neither flag is present.
5. The demoted lint set is exactly the explicit list in this design, with `UnreachableBranches` as the only postponable totality diagnostic.
6. `RecursionError` and `TotalityCheck.NonTotalMatch` remain fatal in all modes.
7. Invalid-pattern totality errors such as `InvalidStrPat` and `MultipleSplicesInPattern` remain fatal in all modes.
8. A single shared classifier determines postponability, and both compile-time partitioning and replay use that classifier.
9. `lib check --warn` succeeds when the only diagnostics are postponable lint diagnostics and prints them as warnings.
10. `lib check --lax` succeeds when the only diagnostics are postponable lint diagnostics and suppresses them.
11. `lib test --warn` and `lib test --lax` still run tests when the only compile-time diagnostics are postponable lint diagnostics.
12. Mixed hard-plus-lint compilations still fail, and `--warn` renders both blocks in a stable order.
13. Warning behavior is identical on cache misses and cache hits.
14. A strict run after a prior `--warn` or `--lax` run still fails on the same lint diagnostics.
15. `lib build`, `tool check`, and other unaffected commands remain strict and gain no new flags.
16. `todo` remains unavailable in `lib test` regardless of lint mode.

## Risks And Mitigations

1. Risk: cache hits silently lose warnings.
   Mitigation: replay the lint set from `PackageMap.Inferred` on every successful compile.

2. Risk: cold-path lint collection and replay-path lint collection drift apart.
   Mitigation: keep the postponable set centralized in one classifier and add warm-vs-cold parity tests.

3. Risk: warning summaries are confusing because some existing diagnostics are currently categorized as errors.
   Mitigation: add warning-specific summary labels in `CompilerApi` instead of reusing the current error summary labels verbatim.

4. Risk: replay adds noticeable latency to successful `lib check` and `lib test` runs.
   Mitigation: replay only the explicit lint subset and reuse typed packages rather than reparsing sources.

5. Risk: users expect library-config lint such as unused deps to be covered too.
   Mitigation: document that v1 is scoped to compiler/package diagnostics only, and treat `LibConfig` lint demotion as a separate follow-up.

## Rollout Notes

1. Land the compiler refactor and strict-wrapper preservation in the same PR so default behavior does not regress.
2. Expose `--warn` and `--lax` on `lib check` and `lib test` in that same change.
3. Update the iteration docs in the same PR so the new modes are discoverable.
4. No migration is required because strict mode remains the default.
5. No infer-cache key change is required because lint mode does not alter compiled artifacts; replay makes the user-visible behavior stable.
6. Likely follow-ups are `tool check` parity and a separate decision on whether any `LibConfig` errors should become warnable.
