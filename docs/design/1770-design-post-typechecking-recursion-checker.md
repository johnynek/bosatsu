---
issue: 1770
priority: 3
touch_paths:
  - docs/design/1770-design-post-typechecking-recursion-checker.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/RecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/DefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionParityTest.scala
  - core/src/test/scala/dev/bosatsu/WellTypedTests.scala
  - core/src/test/scala/dev/bosatsu/DefRecursionCheckTest.scala
  - docs/src/main/paradox/recursion.md
depends_on: []
estimated_size: M
generated_at: 2026-02-24T00:29:33Z
---

# Design post-typechecking Recursion checker

_Issue: #1770 (https://github.com/johnynek/bosatsu/issues/1770)_

## Summary

Design to move recursion checking from syntax-level `Declaration` to post-typechecking `TypedExpr`, with a typed-strategy architecture that preserves current structural/lexicographic behavior, enables cleaner `Int` recursion support for #1760, and migrates the legacy checker into test-only differential validation against WellTypedGen.

# Issue 1770: Post-Typechecking Recursion Checker

Status: proposed  
Date: 2026-02-24  
Issue: https://github.com/johnynek/bosatsu/issues/1770  
Base branch: `main`

## Problem
Today recursion legality is checked by `DefRecursionCheck` over `Declaration` syntax before type inference. This has three core drawbacks:

1. No type information at the recursion-check point, so strategy selection cannot use the scrutinee type.
2. `Int` recursion support (issue #1760) must encode extra syntax/metadata early, instead of using resolved types.
3. The checker is bound to pre-desugared syntax rather than the typed IR (`TypedExpr`) that later phases actually consume.

Issue #1770 asks to move recursion checking post-typechecking on `TypedExpr`, and to analyze safety/benefits of that move.

## Goals

1. Make post-typechecking recursion validation authoritative.
2. Preserve current acceptance/rejection behavior for existing structural and tuple-lexicographic recursion.
3. Keep diagnostics source-anchored and comparable to current messages.
4. Introduce a typed strategy-selection point so `Int` targets can use an `int_decrease` strategy naturally (for #1760).
5. Enable future expansion of “smaller” (including enum-constructor ordering) without reworking parser-level logic.
6. Move the current checker to test-only oracle use and prove agreement on `WellTypedGen` programs.

## Non-goals

1. No immediate change to parser surface syntax in this issue.
2. No full SMT/solver integration in this issue.
3. No change to runtime/codegen from this issue alone.
4. No broad redesign of `TypedExpr` normalization or loop lowering.

## Current Pipeline
In `Package.inferBodyUnopt` today:

1. Source conversion (`Statement` -> `Expr[Declaration]` lets).
2. `DefRecursionCheck` on syntax statements.
3. Source `TotalityCheck`.
4. Type inference (`Infer.typeCheckLets`) to `TypedExpr[Declaration]`.
5. Normalization/lowering in later stages.

Recursion checking therefore happens before types are known.

## Proposed Pipeline

1. Keep source conversion, source totality, and inference ordering.
2. Remove `DefRecursionCheck` from production compile path.
3. After successful inference, run `TypedExprRecursionCheck` on typed lets.
4. Map typed recursion errors through `PackageError.RecursionError`.
5. Continue with existing normalization/lowering pipeline unchanged.

Concretely this is a post-inference check in `Package.scala`, before the existing return of unoptimized typed program.

## Architecture

### 1. New checker module
Add `TypedExprRecursionCheck` in main code. Input:

1. Package name.
2. Full type environment (`TypeEnv[Kind.Arg]`) for constructor-family metadata.
3. Typed lets: `List[(Bindable, RecursionKind, TypedExpr[Declaration])]`.

Output:

1. `ValidatedNel[RecursionCheck.Error, Unit]`.

### 2. Shared recursion error algebra
Introduce a checker-neutral error ADT (for example `RecursionCheck.Error`) in main. Keep message text and region behavior aligned with current `DefRecursionCheck.RecursionError` semantics so diagnostics remain stable.

`PackageError.RecursionError` should depend on this neutral ADT, not directly on `DefRecursionCheck`.

### 3. Typed strategy abstraction
Add strategy classification per recur target component:

1. `Structural` (current behavior).
2. `IntDecrease` (typed hook for #1760).
3. `EnumOrdinal` (future extension, off by default initially).

Strategy is selected from target type information after typechecking.

## Typed Checker Algorithm
The checker keeps the same high-level invariants as the current checker, but evaluated on `TypedExpr`.

### A. Identify recursion context
For each recursive top-level let binding:

1. Recover function parameter groups from the typed lambda shape.
2. Traverse the function body with a state machine (`TopLevel`, `InDef`, `InDefRecurred`, `InRecurBranch`) analogous to current logic.

### B. Detect `recur` matches in typed tree
`TypedExpr.Match` does not store recursion kind directly, but tags are `Declaration`. A match is a recur-site when its tag is `Declaration.Match(RecursionKind.Recursive, target, ...)`.

This allows migration without changing core typed AST shape.

### C. Resolve recur target and strategy
At recur-site:

1. Resolve target names to function parameter positions.
2. Validate target shape (name or tuple of names, no duplicates, must refer to parameters).
3. Select strategy per target component from resolved type:
4. If normalized type is `Int`, choose `IntDecrease`.
5. Otherwise choose `Structural` for MVP parity.

### D. Branch-local decrease evidence
For each branch:

1. Compute allowed structural names via typed pattern `substructures` (same rule as current checker).
2. For tuple targets, compute evidence component-wise from tuple pattern branches.
3. Track reachable names for lambda-argument propagation exactly as current checker does in single-target mode.

### E. Validate recursive calls
For each self-call in recur-branch:

1. Extract call arguments at target positions.
2. Classify each target component as `Equal`, `Smaller`, or `Other` using its selected strategy.
3. Apply lexicographic acceptance: some component must be `Smaller`, all prior components must be `Equal`.
4. Reject otherwise.
5. Recurse into call arguments to validate nested recursive calls (same as current behavior).

## #1760 Improvement Path (`Int`-driven strategy)
Moving to typed checking enables cleaner syntax because strategy can be inferred from type:

1. If recur target type is `Int`, use `IntDecrease` automatically.
2. This removes the need for syntax-level strategy annotations in many cases.
3. The checker can still stay conservative until full obligations are implemented.

This provides the architectural slot needed by #1760 while keeping parser churn minimal.

## Future Extension: Enum Constructor Ordering as “Smaller”
With typed env metadata, we can safely discuss broader decrease relations.

Proposed optional extension:

1. For enum type `E` with constructors in declaration order `C0, C1, ..., Cn`.
2. In branch matching `Ci(...)`, allow recursive calls on values proven to be constructor `Cj` where `j < i` as a decrease witness.
3. Keep existing substructure decrease as primary rule.

Example in issue body: for `Nat = Zero | Succ(prev)`, in `Succ(p)` branch, `Zero` could be considered smaller by constructor rank.

Safety rationale: finite constructor rank is a well-founded order. This must remain strict and type-aligned to preserve totality.

This is explicitly future-facing and should be feature-gated initially.

## Moving Legacy Checker to `test/` and Differential Validation

### Plan

1. Move (or copy then remove) current `DefRecursionCheck` implementation into test scope as `LegacyDefRecursionCheck`.
2. Add a parity test harness that runs both checkers on the same programs.
3. Keep legacy checker as oracle for migration confidence only.

### Differential suite

1. `WellTypedGen` parity property:
2. Generate well-typed programs via phase1..phase4 configs.
3. Run legacy syntax checker on statements.
4. Run typed checker after inference.
5. Assert agreement on pass/fail.

And add curated regression parity:

1. Existing recursion-focused samples from `DefRecursionCheckTest` and error-message snapshots.
2. Compare acceptance and key diagnostic text.

Note: `WellTypedGen` currently has limited recursion shape coverage, so curated recursion corpus remains necessary.

## Acceptance Criteria

1. Production pipeline no longer invokes syntax-phase `DefRecursionCheck` from `Package.scala`.
2. `TypedExprRecursionCheck` runs post-inference and blocks invalid recursion with `PackageError.RecursionError`.
3. Existing structural and tuple-lexicographic recursion behavior matches current checker on regression corpus.
4. Differential tests show legacy/new checker agreement for all `WellTypedGen` generated programs executed in CI.
5. Legacy checker lives in test scope only (or main copy removed after parity stabilization).
6. New strategy-selection tests prove `Int` targets are recognized as `IntDecrease` candidates.
7. Documentation notes recursion validation now occurs post-typechecking.

## Risks and Mitigations

1. Risk: recursion diagnostics disappear when type inference fails first.
Mitigation: document ordering change; keep error text high quality for inferred programs; preserve curated syntax tests where useful.

2. Risk: relying on `Declaration` tags in `TypedExpr.Match` is brittle.
Mitigation: run checker immediately post-inference before rewriting; add fallback plan to add explicit recur marker in typed AST if needed later.

3. Risk: behavior drift from legacy checker.
Mitigation: differential tests (WellTypedGen + curated recursion suite) before switching default path.

4. Risk: performance overhead from another typed-tree traversal.
Mitigation: single linear traversal per recursive let; cache target-position resolution and type normalization.

5. Risk: enum-order extension could be over-permissive if done naïvely.
Mitigation: keep extension disabled by default and require strict typed proof of constructor rank decrease.

## Rollout Notes

1. Stage 1: add shared recursion error ADT and implement `TypedExprRecursionCheck` in shadow mode (tests only).
2. Stage 2: dual-run checker in tests; validate parity against legacy checker.
3. Stage 3: switch production pipeline to typed checker.
4. Stage 4: move legacy checker to test-only path and keep parity tests.
5. Stage 5: add #1760 behavior on top of typed strategy selection (`IntDecrease`).
6. Stage 6: evaluate optional enum-ordinal decrease extension behind a guarded flag.

## Likely Implementation Touch Points

1. `core/src/main/scala/dev/bosatsu/Package.scala`
2. `core/src/main/scala/dev/bosatsu/PackageError.scala`
3. `core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala` (new)
4. `core/src/main/scala/dev/bosatsu/RecursionCheck.scala` (new shared errors/strategy)
5. `core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala` (new/moved)
6. `core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala` (new)
7. `core/src/test/scala/dev/bosatsu/TypedExprRecursionParityTest.scala` (new)
8. `core/src/test/scala/dev/bosatsu/WellTypedTests.scala`
9. `core/src/test/scala/dev/bosatsu/DefRecursionCheckTest.scala`
10. `docs/src/main/paradox/recursion.md`

## Decision
Proceed with post-typechecking recursion validation as the authoritative checker, keep strict parity with current behavior first, and use typed strategy hooks to enable a cleaner #1760 implementation and future enum-order decrease exploration.
