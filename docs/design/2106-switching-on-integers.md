---
issue: 2106
priority: 2
touch_paths:
  - docs/design/2106-switching-on-integers.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessToValue.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessRegressionTest.scala
  - core/src/test/scala/dev/bosatsu/Issue1633Test.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-10T00:00:00Z
---

# Issue #2106 Design: Switching on literals (Int and Char)

_Issue: #2106 (https://github.com/johnynek/bosatsu/issues/2106)_

Status: proposed  
Base branch: `main`

## Summary

Lower large literal-match columns to a balanced decision tree in Matchless instead of a linear `If(EqualsLit(...))` chain.

This design introduces one new boolean IR node:

`LtEqLit(expr, lit)`

and uses it for ordered literal dispatch on `Int` and `Char` cases. Branch bodies remain compiled through the existing matrix specialization pipeline.

## Problem statement

Today, literal pattern matches in `matchExprMatrixCheap` flow through `LitSig` into ordered `compileCases`, which emits linear equality checks.

For code like:

- `case 1`, `case 2`, ... `case 9`, `case _`

this becomes a long if/else chain with O(n) comparisons in the worst case.

The issue asks for better code generation, ideally switch-like behavior on small integers, or at least balanced comparisons using a node like `LtEqLit`. The same shape is useful for parser-style character dispatch.

## Goals

1. Improve generated decision structure for literal pattern matches on `Int` and `Char`.
2. Keep language semantics unchanged, including guards and wildcard/default behavior.
3. Keep optimization in Matchless lowering (not backend-specific pattern recovery).
4. Require no runtime representation changes for this phase.
5. Preserve existing lowering for non-literal columns, mixed literal kinds, and small fanouts.

## Non-goals

1. No source syntax changes.
2. No generalized ordered lowering for `String` or `Float64` in this issue.
3. No C runtime API change to expose direct small-int switch tags.
4. No redesign of existing `SwitchVariant` logic.

## Design decision

Adopt matrix-time balanced literal dispatch with a new `LtEqLit` bool node.

Policy:

1. Trigger only when the selected matrix column signatures are all literals of one supported comparable kind (`Int` or `Char`).
2. Trigger only when literal case count is at least a threshold (`LiteralTreeMinCases = 4`).
3. Keep wildcard rows as the default branch. In total matches this is expected, since totality requires a fallback path when matching finite literal sets.
4. Otherwise keep current linear `compileCases` behavior.

Rationale:

1. Reduces worst-case compare depth from O(n) to O(log n).
2. Keeps correctness localized to existing matrix specialization machinery.
3. Avoids brittle backend-side reconstruction of switch opportunities.
4. Avoids runtime coupling required for true C `switch` over boxed integers.

## Proposed architecture

### 1) Extend BoolExpr with ordered literal comparison

Add in `Matchless.scala`:

`case class LtEqLit[A](expr: CheapExpr[A], lit: Lit) extends BoolExpr[A]`

Invariants:

1. `lit` is only emitted by lowering for supported ordered kinds (`Lit.Integer` and `Lit.Chr`) in this phase.
2. `expr` has the same literal runtime type as `lit` at the match site.

Semantics:

1. Evaluate `expr` once.
2. Return true iff `expr <= lit` using the literal-kind ordering for the match column.

Update all BoolExpr traversal and utility code for exhaustiveness:

1. Bool tag ordering and `Order[BoolExpr[A]]`.
2. `referencesBindable` and all recursive walkers.
3. Substitution/CSE/renaming/rewrite helpers.
4. `hasSideEffect` and expression weight accounting.

### 2) Add literal-tree lowering in matrix compiler

In `matchExprMatrixCheap`:

1. Detect literal-tree eligibility from selected `sigs`.
2. Require all `LitSig` in that split to have one comparable kind (`Int` or `Char`).
3. Reuse existing `minimizeSpecializedRows` and recursive `compileRows` to compile each literal case body.
4. Reuse existing default-row compilation for fallback.
5. Sort case literals by value within that kind.
6. Build a balanced decision tree:

- Internal node: `If(LtEqLit(occ, pivot), leftTree, rightTree)`
- Leaf node: `If(EqualsLit(occ, literal), caseExpr, fallbackExpr)`

This changes selector shape only; case bodies and default behavior still come from the current specialization logic.

### 3) Keep semantics for guards and fallback rows

Each literal case body is compiled from the same specialized rows as today, so guard behavior remains unchanged.

Reordering literal tests by value is semantically safe because distinct literal heads are mutually exclusive values.

### 4) Evaluator support

In `MatchlessToValue.scala` add `LtEqLit` evaluation in `boolExpr`:

1. Evaluate `expr`.
2. Dispatch comparison by literal kind:
   - `Int`: numeric compare.
   - `Char`: codepoint compare.
3. Return boolean.

### 5) Backend support

Clang (`ClangGen.boolToValue`):

1. `Int`: lower `LtEqLit(expr, lit)` to `bsts_integer_cmp(expr, lit_value) <= 0`.
2. `Char`: lower to `bsts_char_code_point_from_value(expr) <= lit_codepoint`.

Python (`PythonGen.boolExpr`):

1. Lower `LtEqLit(expr, lit)` to expression equivalent of `expr <= lit` using existing AST operators.
2. Works for both `int` and single-character string representation.

No C runtime changes are needed for this phase.

## Detailed implementation plan

1. Add `LtEqLit` node to `Matchless.BoolExpr` with `lit: Lit`.
2. Add exhaustive handling of `LtEqLit` across all BoolExpr helpers in `Matchless.scala`.
3. Add `LiteralTreeMinCases` threshold constant in `Matchless.scala`.
4. Add helper that identifies eligible `LitSig` sets (same supported literal kind, above threshold).
5. Add helper to compile literal case branches using current specialization/default-row machinery.
6. Add balanced tree builder that emits `LtEqLit` internal comparisons and `EqualsLit` leaves.
7. Integrate this path into `matchExprMatrixCheap` before legacy linear literal lowering.
8. Implement `LtEqLit` evaluation in `MatchlessToValue.scala` for `Int` and `Char`.
9. Implement Clang codegen support for `LtEqLit` (`Int` and `Char`).
10. Implement Python codegen support for `LtEqLit`.
11. Update exhaustive BoolExpr test walkers (`MatchlessTests`, `MatchlessRegressionTest`, `Issue1633Test`).
12. Add issue-focused tests for lowering shape and generated backend code.

## Testing plan

### Matchless + evaluator tests

1. Add regression for issue-style integer ladder (`1..9` + wildcard) and assert lowered IR includes `LtEqLit`.
2. Add regression for parser-style char dispatch and assert lowered IR includes `LtEqLit`.
3. Add threshold regression: below threshold remains linear and does not emit `LtEqLit`.
4. Add correctness checks for matched literal, unmatched literal, negative values, large integers, and character ordering.
5. Add guarded-literal test to ensure fallback semantics are preserved.

### Clang codegen tests

1. Add/extend test compiling a wide integer literal match and assert generated C uses `bsts_integer_cmp` with `<= 0`.
2. Add/extend test for char literal tree lowering and assert generated C uses codepoint ordered compare.
3. Keep existing literal equality tests passing.

### Python codegen tests

1. Add/extend test compiling wide integer literal match and assert generated Python contains ordered comparison shape (not equality-only ladder).
2. Add/extend test for char literal tree lowering with ordered comparisons.

### Compile hygiene tests

1. Update exhaustive BoolExpr pattern matches in:
   - `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`
   - `core/src/test/scala/dev/bosatsu/MatchlessRegressionTest.scala`
   - `core/src/test/scala/dev/bosatsu/Issue1633Test.scala`
2. Run core test suites covering match lowering and both codegen backends.

## Acceptance criteria

1. `Matchless.BoolExpr` has a new `LtEqLit(expr, lit: Lit)` node.
2. Matrix lowering emits balanced literal decision trees for eligible `Int` and `Char` fanout (>= threshold).
3. Non-eligible matches keep existing behavior.
4. Branch semantics for guards, wildcard/default, and bindings are unchanged.
5. `MatchlessToValue` evaluates `LtEqLit` correctly for `Int` and `Char`.
6. Clang codegen handles `LtEqLit` for `Int` and `Char`.
7. Python codegen handles `LtEqLit` for `Int` and `Char`.
8. Exhaustive BoolExpr tests compile and pass after updates.
9. New issue-focused tests pass.

## Risks and mitigations

1. Risk: semantic drift from reordered tests.
Mitigation: reorder only mutually exclusive literal heads; keep per-literal specialization and fallback compilation unchanged; add guard regressions.

2. Risk: accidental unsupported literal use of `LtEqLit`.
Mitigation: gate lowering to supported kinds (`Int`, `Char`) and keep explicit evaluator/codegen branches.

3. Risk: performance regression for tiny matches.
Mitigation: threshold gate and fallback to existing linear lowering for small fanout.

4. Risk: compile failures from new BoolExpr case.
Mitigation: update all exhaustive BoolExpr matches in main and tests.

5. Risk: C backend still not a native `switch` for boxed integers.
Mitigation: this phase delivers asymptotic depth improvement first; switch-on-small-int runtime-assisted path is a follow-up optimization.

## Rollout notes

1. Land as one PR with Matchless, evaluator, both backends, and tests.
2. Keep threshold conservative (`4`) for first rollout.
3. Validate generated output on integer and parser-char examples before merge.
4. If regressions appear, disable behavior quickly by raising threshold while keeping `LtEqLit` support.
5. Evaluate follow-up dense small-int C switch optimization only if profiling shows remaining hotspot.

## Alternatives considered

1. Backend-only recovery of linear literal `If` chains into switch/tree.  
Rejected due brittleness and loss of matrix semantics.

2. New `SwitchInt` or generalized `SwitchLit` expression in Matchless.  
Deferred because `LtEqLit` tree lowering solves the core issue with lower IR surface-area churn.

3. C runtime helper for direct small-int switch extraction.  
Deferred to a follow-up; higher risk and runtime API coupling.

## Decision

Proceed with matrix-time balanced literal dispatch (`Int` and `Char`) using `LtEqLit(expr, lit: Lit)`, with evaluator/backend support and targeted regression coverage.
