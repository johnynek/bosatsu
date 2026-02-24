---
issue: 1769
priority: 3
touch_paths:
  - docs/design/1769-design-refactoring-totalitychecker-to-run-on-typedexpr-declaration.md
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/Inhabitedness.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - core/src/test/scala/dev/bosatsu/TotalityTest.scala
  - core/src/test/scala/dev/bosatsu/WellTypedGen.scala
  - core/src/test/scala/dev/bosatsu/TestUtils.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-24T00:26:00Z
---

# Issue 1769 Design: Refactoring TotalityCheck to run on TypedExpr[Declaration]

_Issue: #1769 (https://github.com/johnynek/bosatsu/issues/1769)_

## Summary

Migrate totality checking from untyped Expr to TypedExpr[Declaration], use scrutinee-type-aware and inhabitedness-aware coverage, and add typed generator-based tests via WellTypedGen so missing impossible branches are allowed while preserving conservative behavior for unknown cases.

# Issue 1769: Refactoring TotalityCheck to run on TypedExpr[Declaration]

Status: proposed  
Date: 2026-02-24  
Issue: https://github.com/johnynek/bosatsu/issues/1769

## Problem statement

Totality checking currently runs in `Package` before rank-n inference, on `Expr[Declaration]`. That means the checker does not have the inferred scrutinee type at each match site.

This causes three practical limits:

1. Coverage logic is pattern-list based, not scrutinee-type based.
2. The checker cannot directly use inhabitedness analysis from #1729.
3. The checker keeps defensive pattern-validity work that inference already guarantees once patterns are typed.

The goal of this issue is to move totality checking to `TypedExpr[Declaration]` so we can use fully typed match patterns plus scrutinee type information.

## Current state

1. `core/src/main/scala/dev/bosatsu/TotalityCheck.scala` checks `Expr[A]` matches, computes missing branches from `top = _`, and reports unreachable branches by coverage shadowing.
2. `core/src/main/scala/dev/bosatsu/rankn/Infer.scala` already produces typed patterns in `TypedExpr.Match` via `typeCheckPattern`.
3. `core/src/main/scala/dev/bosatsu/Inhabitedness.scala` (from #1729) can classify both types and matchability as `Inhabited`, `Uninhabited`, or `Unknown`.
4. `core/src/main/scala/dev/bosatsu/Package.scala` currently runs totality pre-inference.

## Design goals

1. Run totality checking on `TypedExpr[Declaration]`.
2. Use scrutinee type at each match site.
3. Use #1729 inhabitedness results to allow missing branches that are provably unmatchable.
4. Mark provably unmatchable branches as unreachable.
5. Keep conservative behavior when inhabitedness result is `Unknown`.
6. Keep diagnostics and guard semantics stable.
7. Add typed-pattern generator coverage for tests.

## Non-goals

1. Rewriting pattern set algebra internals.
2. Solving all `Unknown` inhabitedness cases.
3. Adding per-pattern region tracking (existing #132 follow-up).
4. Changing guard semantics.

## Answers to issue questions

### Can TotalityCheck be simplified with typed patterns and types?

Yes, incrementally.

1. Constructor existence and arity checks become inference responsibilities once checker input is typed.
2. Typed input removes the need to reason about random mixed-type pattern sets in totality-path tests.
3. Structural ambiguity checks that inference does not currently enforce (multi-splice list patterns and adjacent string globs) can remain initially to preserve user-facing diagnostics, then be moved later to inference if desired.

### Does #1729 have enough information to identify unmatchable branches?

Yes for definitive cases.

1. `Inhabitedness.checkMatch(scrutineeType, pattern, env)` gives `Uninhabited` for branches that cannot match.
2. `Inhabitedness.check(scrutineeType, env)` can make the whole match domain empty.
3. `Unknown` must remain conservative and should not be treated as unmatchable.

## Proposed architecture

### 1. Typed totality entrypoint

1. Refactor totality traversal to operate on `TypedExpr[A]`.
2. Keep current pattern set-ops machinery for `intersection`, `difference`, `missingBranches`.
3. Update totality error payloads to reference typed match data (or equivalent metadata containing tag, branches, and guard presence).

### 2. Matchability classification per branch

For each `TypedExpr.Match(arg, branches, tag)`:

1. Let `scrutineeType = arg.getType`.
2. For each branch pattern, call `Inhabitedness.checkMatch(scrutineeType, pattern, env)`.
3. Map result to checker behavior:
4. `Uninhabited`: branch is definitely unmatchable.
5. `Inhabited`: branch is matchable.
6. `Unknown` or validation failure: treat as conservatively matchable.

### 3. Domain-aware missing-branch computation

At each typed match:

1. Build initial domain from scrutinee type.
2. If `Inhabitedness.check(scrutineeType, env) == Uninhabited`, domain is empty and missing list is empty.
3. Otherwise, for scrutinee root constructors, build wildcard constructor patterns and classify each with `checkMatch`.
4. Subtract only definitely unmatchable constructor patterns from the domain.
5. Compute missing branches against unguarded branches that are not definitely unmatchable.

This is the key behavior change that allows missing impossible branches.

### 4. Unreachable branch computation

A branch is unreachable when either condition holds:

1. It is definitely unmatchable from inhabitedness.
2. It is shadowed by prior unguarded, matchable coverage.

This preserves existing ordered-coverage semantics and adds typed impossibility reachability.

### 5. Validation split

1. Inference validates typing and constructor arity.
2. Totality keeps only structural ambiguity checks that inference currently does not reject.
3. Optional follow-up: move those structural checks into `Infer.typeCheckPattern` and remove them from totality.

### 6. Pipeline move in Package

1. Remove untyped totality from pre-inference checks.
2. Run inference first to produce typed lets.
3. Run totality on typed lets (`TypedExpr[Declaration]`) before normalization.
4. Map errors to existing `PackageError.TotalityCheckError`.
5. Def-recursion and unused-let checks remain as current pre-inference checks.

## Implementation plan

1. Refactor checker API and traversal in `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`.
2. Wire post-inference typed totality execution in `core/src/main/scala/dev/bosatsu/Package.scala`.
3. Update error rendering plumbing in `core/src/main/scala/dev/bosatsu/PackageError.scala`.
4. Add helper(s) in `core/src/main/scala/dev/bosatsu/Inhabitedness.scala` if constructor-level reachability extraction is factored there.
5. If structural pattern checks move, update `core/src/main/scala/dev/bosatsu/rankn/Infer.scala`.
6. Add dedicated typed totality regressions in `core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala`.
7. Keep `core/src/test/scala/dev/bosatsu/TotalityTest.scala` focused on set-algebra laws and typed-compatible properties.
8. Update error-construction tests in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`.
9. Add package-level regressions in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`.

## Testing plan

### Deterministic regressions

1. Missing impossible constructor branch is accepted.
2. Explicit impossible constructor branch is reported unreachable.
3. Guarded branches still do not count toward totality.
4. Uninhabited scrutinee type yields vacuous totality behavior.
5. Existing invalid list/string pattern diagnostics are preserved.

### Property tests with well-typed patterns

Current random pattern generation in `TotalityTest` uses untyped random patterns. For this issue, add typed generators.

Extend `core/src/test/scala/dev/bosatsu/WellTypedGen.scala` with typed pattern-set sampling:

1. Generate a `TypeEnv[Kind.Arg]` from generated statements using existing finalize path.
2. Generate scrutinee types from that environment.
3. Generate pattern sets constrained by the scrutinee type.
4. Include both reachable and intentionally unmatchable patterns.
5. Optionally infer synthetic matches and extract compiler-produced typed patterns to guarantee well-typedness.

Suggested sample payload for tests:

`PatternSetSample(typeEnv, scrutineeType, branches)`

Core properties:

1. Adding checker-reported missing patterns yields total coverage.
2. Branches proven unmatchable by inhabitedness do not contribute to missing requirements.
3. `Unknown` inhabitedness never triggers pruning.

## Acceptance criteria

1. `Package` runs totality on `TypedExpr[Declaration]` only.
2. Totality logic uses `arg.getType` at every `TypedExpr.Match`.
3. Missing branches exclude definitely unmatchable cases proven by inhabitedness.
4. Definitely unmatchable branches are reported as unreachable.
5. `Unknown` inhabitedness remains conservative.
6. Guard behavior is unchanged.
7. Existing list/string structural-pattern diagnostics are unchanged.
8. New deterministic typed-totality regressions pass.
9. New well-typed generator-based properties pass.
10. Existing totality, evaluation, and error-message tests continue to pass.

## Risks and mitigations

1. Risk: totality diagnostics now depend on inference success.
2. Mitigation: keep this as an explicit pipeline behavior change and preserve other pre-inference checks.
3. Risk: performance regression from repeated inhabitedness checks.
4. Mitigation: memoize per-match `check` and `checkMatch` computations.
5. Risk: incorrect pruning in uncertain cases.
6. Mitigation: prune only on definitive `Uninhabited`, never on `Unknown`.
7. Risk: generator complexity or flakiness.
8. Mitigation: bound depth/branch counts and keep deterministic regression tests as primary safety net.

## Rollout notes

1. Phase 1: implement typed checker path and tests while keeping old helper APIs where needed.
2. Phase 2: switch `Package` to typed totality path.
3. Phase 3: remove dead untyped totality invocation and simplify compatibility code.
4. Phase 4: optional follow-up to move remaining structural pattern checks fully into inference.

## Out of scope for this issue

1. Per-pattern precise regions.
2. Major rewrite of `SetOps` pattern algebra.
3. Completeness improvements for all inhabitedness `Unknown` cases.
