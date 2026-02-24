---
issue: 1769
priority: 3
touch_paths:
  - docs/design/1769-design-refactoring-totalitychecker-to-run-on-typedexpr-declaration.md
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - core/src/test/scala/dev/bosatsu/TotalityTest.scala
  - core/src/test/scala/dev/bosatsu/WellTypedGen.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-24T00:38:35Z
---

# Issue #1769 Design: Refactor TotalityCheck to run on TypedExpr[Declaration]

_Issue: #1769 (https://github.com/johnynek/bosatsu/issues/1769)_

## Summary

Move totality checking from untyped Expr to TypedExpr so each match has scrutinee type information; use Inhabitedness (#1729) to conservatively prune impossible branches from missing-branch requirements and report definitely impossible branches as unreachable; update pipeline ordering and add typed generation-based tests via WellTypedGen.

---
issue: 1769
title: Design refactoring TotalityChecker to run on TypedExpr[Declaration]
status: proposed
depends_on:
  - 1729
---

# Design: Refactor TotalityCheck to run on TypedExpr[Declaration]

## Context

`TotalityCheck` currently traverses `Expr[Declaration]` and is invoked in `Package.inferBodyUnopt` before rank-n inference. At that point we have patterns, but we do not have the final inferred scrutinee type per `match`.

Issue #1769 asks us to use typed information, especially uninhabitedness data from #1729, so that:

1. Totality checks can reason from the scrutinee type.
2. Definitely unmatchable branches can be treated as unreachable.
3. Missing branches that are themselves unmatchable can be excluded from required coverage.
4. Testing can move from random pattern generation to well-typed generation.

## Problem Statement

Current behavior has three limitations:

1. `TotalityCheck` cannot use `TypedExpr.Match.arg.getType`, because it runs on untyped `Expr`.
2. Missing/unreachable logic is purely syntactic set algebra, so it cannot use `Inhabitedness.check` and `Inhabitedness.checkMatch` from #1729.
3. Property tests in `TotalityTest` generate many ill-typed pattern combinations; that is fine for set-ops laws but insufficient for typed totality behavior.

## Goals

1. Run totality checking on `TypedExpr[Declaration]`.
2. Use scrutinee type (`arg.getType`) for every match analysis.
3. Use `Inhabitedness.checkMatch` to conservatively identify impossible branches.
4. Allow missing branches to be omitted when they are provably unmatchable.
5. Keep conservative behavior for `Unknown` inhabitedness results.
6. Preserve guard semantics (guarded branches do not count toward coverage).
7. Keep error regions and user-facing diagnostics stable.
8. Add deterministic and property-based tests using well-typed generators.

## Non-goals

1. Rewriting `patternSetOps` algebra in this issue.
2. Eliminating all `Unknown` outcomes from inhabitedness.
3. Redesigning parser/source syntax for patterns.
4. Solving per-pattern region granularity (tracked separately in issue #132).

## Answers to Issue Questions

### Can TotalityCheck be simplified once patterns are fully typed?

Yes, substantially.

1. Constructor existence and constructor arity are already typechecker responsibilities for typed programs.
2. Totality can focus on coverage and reachability over well-typed patterns.
3. Keep only structural pattern lint that is not guaranteed by typing today (for example adjacent string wildcards and list splice constraints).

### Does #1729 have enough information to identify unmatchable branches so we can allow missing but unmatchable branches?

Yes, for definitive cases.

1. `Inhabitedness.check(scrutineeType, env)` identifies globally uninhabited scrutinee domains.
2. `Inhabitedness.checkMatch(scrutineeType, pattern, env)` identifies branch patterns that are definitely `Uninhabited`.
3. `Unknown` and validation failures must remain conservative and be treated as potentially matchable.

## Proposed Architecture

### 1) TotalityCheck API migrates to typed expressions

Refactor `TotalityCheck.checkExpr` to traverse `TypedExpr[A]` instead of `Expr[A]`.

1. `ExprError.matchExpr` should reference `TypedExpr.Match[A]`.
2. `NonTotalMatch`, `UnreachableBranches`, and `InvalidPattern` remain the same conceptual diagnostics.
3. `tag: Declaration` still provides region data for `PackageError.TotalityCheckError`.

### 2) Match analysis uses scrutinee type and inhabitedness

For each `TypedExpr.Match(arg, branches, tag)`:

1. Compute `scrutineeType = arg.getType`.
2. Compute `scrutineeState = Inhabitedness.check(scrutineeType, env)`.
3. Compute per-branch state with `Inhabitedness.checkMatch(scrutineeType, branch.pattern, env)`.
4. Treat branch as definitely impossible only when result is definitively `Uninhabited`.
5. Treat `Unknown` or any validation failure conservatively as potentially reachable.

### 3) Missing-branch computation with impossible-pattern pruning

1. Build coverage from unguarded branches that are not definitely impossible.
2. If scrutinee is definitively uninhabited, missing set is empty (vacuous totality).
3. Otherwise compute missing with existing `patternSetOps.missingBranches`.
4. Post-filter missing patterns: remove any missing pattern `p` where `checkMatch(scrutineeType, p, env)` is definitively `Uninhabited`.

This directly addresses issue #1769’s requirement to allow omitted branches that cannot match.

### 4) Unreachable-branch computation includes impossible branches

A branch is unreachable if either condition holds:

1. Its pattern is definitely impossible for the scrutinee type.
2. It is fully shadowed by prior unguarded, potentially matchable coverage.

Only unguarded, potentially matchable branches should extend the running covered set.

### 5) Validation split

Keep in totality:

1. Structural string/list pattern lint currently modeled as `InvalidStrPat` and `MultipleSplicesInPattern`.

De-emphasize in typed path:

1. Constructor arity and unknown constructor checks, since these are already typechecker invariants for typed trees.

### 6) Package pipeline changes

In `Package.inferBodyUnopt`:

1. Keep pre-inference checks (`DefRecursionCheck`, `UnusedLetCheck`).
2. Run inference to produce typed lets (`TypedExpr[Declaration]`).
3. Run typed totality on inferred lets.
4. Map failures to `PackageError.TotalityCheckError`.

Behavioral implication: if inference fails, typed totality does not run. This is expected and documented.

## Detailed Implementation Plan

1. Update `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`.
2. Change traversal and error payloads from `Expr` to `TypedExpr`.
3. Add helper that converts `Inhabitedness.Result[State]` to conservative state (`Uninhabited` is trusted, everything else treated as potentially matchable).
4. Integrate per-branch inhabitedness classification into missing/unreachable logic.
5. Keep existing set-ops methods (`difference`, `intersection`, `missingBranches`) unchanged.
6. Update `core/src/main/scala/dev/bosatsu/Package.scala` to run totality after inference.
7. Update `core/src/main/scala/dev/bosatsu/PackageError.scala` typing and rendering paths to consume typed match payloads.
8. Add a new deterministic typed-totality suite in `core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala`.
9. Extend `core/src/test/scala/dev/bosatsu/WellTypedGen.scala` with helpers that generate totality-focused typed inputs: type env, scrutinee type, and well-typed pattern sets.
10. Add/adjust property tests in `core/src/test/scala/dev/bosatsu/TotalityTest.scala` to consume well-typed generated cases for the new behavior.
11. Update `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` to assert diagnostics remain clear and region-accurate.

## Testing Strategy

### Deterministic tests

1. `Option[Never]` style case where a constructor branch is impossible: missing branch should not be required.
2. Definitely impossible branch is reported as unreachable.
3. Guarded branches still do not count toward coverage.
4. If scrutinee type is definitively uninhabited, totality succeeds even with no matchable coverage.
5. Existing structural invalid-pattern diagnostics remain unchanged.

### Property tests with well-typed generation

Current random pattern generation is intentionally untyped and should remain for algebraic set-op laws. For typed totality behavior, add well-typed generators.

Proposed generator flow in `WellTypedGen`:

1. Generate a well-typed program and finalized `TypeEnv[Kind.Arg]`.
2. Select candidate scrutinee types from that environment.
3. Generate patterns constrained to that scrutinee type.
4. Optionally inject known-impossible branches using `Inhabitedness.checkMatch` classification.
5. Generate branch sets with guard/no-guard variations.

Properties to assert:

1. Generated typed patterns validate under `Inhabitedness.checkMatch` (no malformed constructor/type data).
2. Missing branches reported by typed totality are never definitively uninhabited.
3. Adding reported missing branches to unguarded reachable coverage yields total coverage.
4. `Unknown` inhabitedness states never cause pruning (no unsound false negatives).

## Acceptance Criteria

1. Totality pass consumes `TypedExpr[Declaration]` rather than `Expr[Declaration]`.
2. Package compilation runs totality after successful inference.
3. Every `match` check uses `arg.getType`.
4. Branches classified as definitively `Uninhabited` are treated as unreachable.
5. Missing branch requirements exclude patterns proved `Uninhabited`.
6. `Unknown` inhabitedness outcomes are handled conservatively.
7. Guard semantics are unchanged.
8. Structural invalid-pattern diagnostics remain present and stable.
9. New deterministic typed-totality tests pass.
10. New well-typed property tests pass.
11. Existing `TotalityTest`, `ErrorMessageTest`, and broader typecheck suites remain green.

## Risks and Mitigations

1. Risk: totality diagnostics disappear when inference fails because typed totality runs later.
Mitigation: keep this behavior explicit in docs/tests; retain pre-inference checks that are independent.

2. Risk: unsound pruning if `Unknown` is treated as impossible.
Mitigation: only prune when state is definitively `Uninhabited`.

3. Risk: performance overhead from repeated `checkMatch` calls.
Mitigation: memoize per-match classifications keyed by normalized/unbound pattern.

4. Risk: flaky property tests from complex typed generators.
Mitigation: cap depth/branch counts, keep deterministic regressions as primary safety net.

5. Risk: diagnostic regression from error payload type migration.
Mitigation: lock message snapshots in `ErrorMessageTest` and assert region stability.

## Rollout Notes

1. Land typed totality logic and tests first while preserving old helper internals where useful.
2. Switch `Package.inferBodyUnopt` to the typed pass in the same PR once tests are green.
3. Remove dead untyped traversal code after migration is complete.
4. Monitor CI runtime and flaky-test rate for the first merge window; tune generator sizes if needed.

## Out-of-Scope Follow-ups

1. Move all remaining structural pattern lint fully into inference.
2. Improve inhabitedness precision to reduce `Unknown` cases.
3. Add per-pattern source regions for finer diagnostics (issue #132).
