---
issue: 2227
priority: 3
touch_paths:
  - docs/design/2227-add-a-lint-for-using-recur-instead-of-loop.md
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/RecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/Program.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/cache/CompileCache.scala
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/tool/CompileCacheTest.scala
  - docs/src/main/paradox/recursion.md
  - docs/src/main/paradox/language_guide.md
  - docs/src/main/paradox/writing_bosatsu_5_minutes.md
depends_on: []
estimated_size: M
generated_at: 2026-03-21T00:26:50Z
---

# Add postponable recursion-form lints for `recur` and `loop`

_Issue: #2227 (https://github.com/johnynek/bosatsu/issues/2227)_

## Summary

Turn non-recursive `recur`/`loop` and tail-recursive `recur` usage into replayable lint diagnostics, while keeping hard recursion soundness unchanged and cache-stable.

## Context
Bosatsu already has a source-level distinction between `recur` and `loop`: `recur` means terminating recursion that may still be non-tail, while `loop` adds a tail-position guarantee. `TypedExprRecursionCheck` already enforces the tail requirement for `loop`, but it also treats `recur` or `loop` with no effective recursion as a hard recursion error. At the same time, a tail-recursive `recur` is accepted silently even though the checked program could have been written as `loop`.

Issue `#2227` wants the source text to reflect the checked semantics. Reviewers should be able to read `recur` as meaning non-tail recursion, `loop` as meaning definite tail recursion, and non-recursive branching as `match`.

This change also has to fit the existing lint contract from issue `#2192`: strict mode still fails, `--warn` prints warnings, and `--lax` suppresses them.

One architectural constraint matters: cached `Package.Compiled` values keep only `Region` tags, and optimized paths can rewrite or erase the source recursion shape via `TypedExprLoopRecurLowering` and `TypedExprNormalization`. These new diagnostics therefore cannot be replayed from cached packages unless the finalized lint result itself is preserved.

## Goals
1. Introduce postponable diagnostics for `recur` or `loop` blocks that contain no effective recursion.
2. Introduce a postponable diagnostic when a `recur` block is fully tail-recursive and should be written as `loop`.
3. Reuse the same recursion-check facts the compiler already trusts, so continuation-style recursion, trusted delayed projections, and tuple targets do not drift between hard checking and linting.
4. Preserve all current hard recursion guarantees: invalid recursion, invalid recur targets, non-decreasing recursion, and non-tail `loop` remain fatal.
5. Keep warning behavior stable on cache misses and cache hits.
6. Avoid parser or CLI surface changes.

## Non-goals
1. Do not relax `loop` tail-position enforcement.
2. Do not add `--warn` or `--lax` to `tool check` in this issue.
3. Do not change recursion lowering or backend code generation rules.
4. Do not add broader recursion style checks beyond the two cases in the issue.

## Current Architecture
1. `TypedExprRecursionCheck` is the authoritative recursion checker. It already tracks whether a recursive region was entered, how many valid recursive uses were seen, and whether the enclosing function is tail-recursive via `SelfCallKind`.
2. `Package.inferBodyUnopt` currently maps every recursion-check failure to `PackageError.RecursionError`, so recursion-related diagnostics are always hard.
3. `CompilerApi` only treats diagnostics as postponable when `PackageError.isPostponable` says they are. Current lint replay works either from source (`UnusedLet`, `UnusedImport`, `todo`) or from cached typed trees that still contain enough information (`ShadowedBindingTypeError`, unreachable-branch totality).
4. The new recursion-form diagnostics do not fit that replay model:
   1. `Package.toCompiled` erases `Declaration` tags down to `Region`.
   2. `TypedExprLoopRecurLowering` can rewrite tail recursion to `Loop` / `Recur`.
   3. `TypedExprNormalization` can rewrite recursive lets with no self-call back to `NonRecursive`.
   Because of those steps, a warm cache hit cannot reliably reconstruct whether the source used `recur`, `loop`, or `match`.

## Proposed Design

### 1. Split recursion soundness from recursion-form linting
Add a small recursion-style lint model, separate from hard recursion soundness errors.

Proposed cases:
1. `NoRecursiveCall(fnName, matchKind, region, likelyRenamedCall)`
2. `TailRecursiveRecur(fnName, region)`

`matchKind` keeps the source distinction between `recur` and `loop`, and `likelyRenamedCall` preserves the current helpful rename hint for the no-recursion case.

Hard recursion checking stays in `RecursionCheck.Error`. The new lint model is only for source-form mismatches that do not threaten soundness.

### 2. Produce lint results from the existing typed recursion analysis
Do not add a second source-only walker.

Instead, extend `TypedExprRecursionCheck` so each checked recursive definition yields:
1. hard recursion errors, if any
2. zero or more recursion-form lints

The lint decision should reuse the existing end-of-definition state:
1. If a `recur` or `loop` header was seen and the checker recorded zero valid recursive uses, emit `NoRecursiveCall`.
2. If the header kind is `recur`, hard recursion checking succeeded for that definition, and `SelfCallKind(fnName, body) == TailCall`, emit `TailRecursiveRecur`.
3. If the header kind is `loop` and `SelfCallKind` is `NonTailCall`, keep emitting the existing hard error.
4. If the definition already has a hard recursion error, suppress recursion-form lints for that same definition to avoid cascaded noise.

Using the existing typed checker is important. It keeps the lint aligned with current rules for:
1. tuple lexicographic recursion
2. trusted delayed projections
3. reachable continuation patterns such as passing the recursive function value
4. branch-local proofs used by the recursion checker

### 3. Convert recursion-form lints into postponable package diagnostics
Add a package-level wrapper such as `PackageError.RecursionLint`, plus shared postponability classification.

Expected behavior:
1. strict `tool check` and strict `lib check` still fail on these lints
2. `lib check --warn` and `lib test --warn` print them as warnings
3. `lib check --lax` and `lib test --lax` suppress them

`CompilerApi` needs a dedicated warning classification for these diagnostics so the warning summary does not lump them into generic package errors. A single lint category such as `recursion form` is sufficient.

### 4. Persist finalized recursion-form lint metadata through the compiled artifact
Because cache replay cannot reconstruct these lints from `Package.Compiled`, the compiled package needs a small sidecar with finalized recursion-form lint metadata.

The implementation should:
1. attach recursion-form lint metadata before `Package.toCompiled` erases source tags
2. serialize that metadata through `TypedAst.proto` and `ProtoConverter`
3. bump the compile-cache schema version so stale cache entries are not mixed with the new payload
4. replay these lints in `CompilerApi.replayTypedLintDiagnostics` directly from the compiled package metadata

This keeps warm-cache behavior identical to cold compiles without re-running type inference just to rediscover style warnings.

### 5. Keep phase ordering unchanged
No pipeline reordering is needed.

The analysis should still happen:
1. after type inference
2. before tail-recursion lowering
3. before normalization

That preserves the current trust boundary and ensures the lint reflects the same checked state that decides whether `loop` is legal.

### 6. User-facing diagnostic shape
The diagnostics should steer code toward the intended source forms:

1. For non-recursive `recur`:
   `recur` but no recursive call to `<fn>`. Use `match` for non-recursive branching.
2. For non-recursive `loop`:
   `loop` but no recursive call to `<fn>`. Use `match` if this code is not recursive.
3. For tail-recursive `recur`:
   recursive calls to `<fn>` are all tail-position; use `loop` to make the stack-safety guarantee explicit.

The no-recursion message should keep the existing likely-renamed-call hint when present.

### 7. Why metadata is better than replay-by-reanalysis
Two plausible alternatives are weaker:

1. Replay from parsed source only.
   This risks diverging from the typed recursion checker on continuation-style recursion and proof-sensitive cases.
2. Re-run typed recursion analysis on every successful warn/lax cache hit.
   This largely defeats the current infer-cache design and makes warning replay much more expensive.

Persisting the finalized lint result is the lowest-risk way to keep behavior correct and fast.

## Implementation Plan
1. Introduce a new recursion-form lint ADT and package-error wrapper.
2. Extend `TypedExprRecursionCheck` to return hard recursion diagnostics plus recursion-form lints from the same per-definition analysis.
3. Remove the no-recursion case from the hard-failure path and emit it as lint output instead.
4. Emit the new tail-recursive `recur` lint when `SelfCallKind` proves every valid recursive call is tail-position.
5. Update `Package.inferBodyUnopt` to merge recursion-form lints into the existing postponable-diagnostics flow.
6. Extend `PackageError.isPostponable` and `CompilerApi` warning rendering to classify the new lint cleanly in strict, warn, and lax modes.
7. Persist recursion-form lint metadata through compiled-package serialization and bump the compile-cache schema version.
8. Replay the stored recursion-form lints from cached compiled packages in `CompilerApi.replayTypedLintDiagnostics`.
9. Add regression tests for strict, warn, lax, cache-hit replay, and serialization.
10. Update recursion and edit-loop docs so the recommended forms are explicit.

## Acceptance Criteria
1. A `recur` or `loop` block with no effective recursion is no longer a hard recursion soundness error; it is a postponable lint.
2. A `recur` block whose valid recursive calls are all tail-position produces a postponable lint suggesting `loop`.
3. Strict `tool check` and strict `lib check` still fail when either lint is present.
4. `lib check --warn` and `lib test --warn` succeed when these are the only diagnostics and print warning output.
5. `lib check --lax` and `lib test --lax` succeed on the same inputs and suppress the warning output.
6. `loop` with any non-tail recursive call remains a hard recursion error in every mode.
7. Invalid recur targets, invalid recursion through guards or shadowing, and non-decreasing recursion remain hard errors in every mode.
8. A valid non-tail `recur` produces no new lint.
9. The no-recursion diagnostic preserves the existing likely-renamed-call hint.
10. Warning behavior is identical on cache misses and cache hits.
11. A later strict run after a prior warn or lax cache hit still fails on the same recursion-form lint.
12. User docs describe `match` for non-recursive branching, `loop` for tail recursion, and `recur` only when non-tail recursion is required.

## Risks And Mitigations
1. Risk: recursion-form lints drift from real recursion legality.
   Mitigation: derive them from the existing typed recursion checker state instead of a second heuristic pass.

2. Risk: warm-cache runs lose or misclassify the new warnings.
   Mitigation: persist finalized lint metadata in the compiled artifact and bump the cache schema.

3. Risk: the compiler emits both a hard recursion error and a style lint for the same broken definition.
   Mitigation: only emit recursion-form lints for definitions that finished hard recursion checking successfully.

4. Risk: warning text overpromises backend details.
   Mitigation: phrase the new lint in terms of the checked `loop` guarantee and reuse the same tail-position classifier that `loop` already depends on.

5. Risk: the new sidecar complicates package serialization.
   Mitigation: keep the metadata small, source-anchored, and limited to finalized lint results rather than raw checker state.

## Rollout Notes
1. This change is source-breaking in strict modes for code that currently uses non-recursive `recur` / `loop` or tail-recursive `recur`. The immediate migration path is to rewrite to `match` or `loop`, or temporarily use `lib check --warn` / `--lax`.
2. The compile-cache schema should be bumped in the same PR so old cache entries cannot hide or drop the new lint metadata.
3. `tool check` remains strict because it still has no relaxed lint mode; that is unchanged by this issue.
4. Docs and command-level tests should land with the implementation so the relaxed workflow is discoverable the same day the strict behavior changes.
