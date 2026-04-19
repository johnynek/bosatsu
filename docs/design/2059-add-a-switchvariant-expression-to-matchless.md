---
issue: 2059
priority: 2
touch_paths:
  - docs/design/2059-add-a-switchvariant-expression-to-matchless.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessToValue.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/Code.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessRegressionTest.scala
  - core/src/test/scala/dev/bosatsu/Issue1633Test.scala
  - core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/CodeTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-07T00:00:00Z
---

# Issue #2059 Design: Add a SwitchVariant expression to Matchless

_Issue: #2059 (https://github.com/johnynek/bosatsu/issues/2059)_

Status: proposed  
Base branch: `main`

## Summary

Add a new Matchless IR node, `SwitchVariant`, to represent enum-variant dispatch explicitly when a match lowers to a large orthogonal variant-only branch ladder.

Key decision in this revision: lower `SwitchVariant` directly from the matrix matcher, not from a post-pass over lowered `If` chains. This keeps constructor orthogonality and union-case structure available at the point where we decide to emit switch-form IR.

## Problem statement

Today, variant-only matches are compiled to nested `If(CheckVariant(...), ..., ...)` trees. For enums with many branches, especially in C output, this shape can be less optimal than direct switch dispatch and makes backend-specific lowering harder.

The issue asks three design questions:

1. Should Matchless always emit switch-like IR, or should it use heuristics?
2. How should C compile it?
3. How should Python compile it without a native switch statement?

## Goals

1. Introduce a first-class Matchless expression for variant dispatch.
2. Preserve behavior of existing match lowering.
3. Improve C backend opportunities for jump-table or switch-style code generation.
4. Keep Python backend behavior correct and deterministic.
5. Avoid error-prone reverse-engineering from generic lowered `If` trees.

## Non-goals

1. No source-language syntax changes.
2. No change to runtime enum representation.
3. No general switch over literals, Nat, or structs in this issue.
4. No typechecker or totality-checker redesign.

## Design decision

Use a hybrid policy:

1. Matchless emits `SwitchVariant` only when the selected matrix column is enum-dispatch and the branch fanout is large enough.
2. The threshold is `SwitchVariantMinCases = 4`.
3. Backends decide final emitted form from `SwitchVariant`.
4. C emits native `switch`.
5. Python lowers to cached-tag `if`/`elif` chain in this issue.

Rationale:

1. The threshold keeps IR/test churn small for tiny matches.
2. Direct matrix-time lowering preserves orthogonality context and union structure.
3. Backend-specific choices remain separate from match semantics.

## Proposed architecture

### 1) Add `SwitchVariant` to Matchless IR

Add a new case class in `Matchless.Expr`:

`SwitchVariant(on: CheapExpr[A], famArities: List[Int], cases: NonEmptyList[(Int, Expr[A])], default: Expr[A])`

Proposed invariants:

1. `cases` variant ids are distinct.
2. Each variant id is in `[0, famArities.length)`.
3. `cases.length >= 2`.
4. `default` is always present.

Semantics:

1. Evaluate `on` once.
2. Read enum variant tag.
3. Evaluate the matching case expression if present.
4. Otherwise evaluate `default`.

### 2) Emit `SwitchVariant` directly in matrix compilation

Integrate switch emission into `matchExprMatrixCheap` in `Matchless.scala`.

Eligibility at the selected column:

1. Head signatures are enum signatures only.
2. All enum signatures share the same enum family metadata (`famArities`).
3. Case count at the column is at least `SwitchVariantMinCases`.

Lowering shape:

1. Reuse existing specialization pipeline (`specializeRows`, `minimizeSpecializedRows`, recursive `compileRows`).
2. Build one case expression per variant from its specialized submatrix.
3. Build default from wildcard/default rows exactly as today.
4. Construct `SwitchVariant(occ, famArities, cases, default)` instead of nested `If(CheckVariant(...), ...)`.

### Why direct matrix-time lowering instead of an `If` post-pass

1. Orthogonality is explicit at matrix time; it is implicit after generic `If` lowering.
2. Union patterns are already represented in the matrix expansion/specialization data.
3. Cases like `B | C` do not require reconstructing intent from arbitrary boolean conditions.
4. We avoid brittle pattern recognition over potentially transformed `If` trees.
5. Correctness arguments stay local to one lowering algorithm instead of split across lowering plus recovery pass.

### 3) Matchless internal updates

Because Scala is compiled with strict pattern-match checks and `-Werror`, all exhaustive Matchless traversals must handle `SwitchVariant`.

Expected updates in `Matchless.scala` include:

1. `Expr.exprTag` and `Expr.Order` comparison.
2. `containsWhileExpr`, `readsMutable`, `referencesBindable`, `referencesLocalAnon`, `allNames`.
3. `hasSideEffect(expr)`.
4. `applyArgs` push-through for branch structure.
5. `topLevelFunctionArity` and `recoverTopLevelLambda` support.
6. Optimizer/pass recursion points (including constructor reuse and loop-hoist traversal).

### 4) Add evaluator support

In `MatchlessToValue.scala`, add evaluation for `SwitchVariant`:

1. Evaluate scrutinee once.
2. Read `asSum.variant`.
3. Dispatch to matching compiled branch function.
4. Fall back to `default`.

This keeps interpreter semantics aligned with backend compilation.

### 5) C backend lowering

Add explicit switch statement support:

1. Extend `core/src/main/scala/dev/bosatsu/codegen/clang/Code.scala` with `Code.Switch` statement and renderer.
2. In `ClangGen.innerToValue`, add `SwitchVariant` case.
3. Compute variant tag once.
4. Use `get_variant_value` when `famArities.forall(_ == 0)`.
5. Otherwise use `get_variant`.
6. Materialize one result variable.
7. Emit `switch(tag)` with one `case` per variant that assigns result and `break`.
8. Emit `default` that assigns result from default expression.

No runtime C API changes are required.

### 6) Python backend lowering

In `PythonGen.loop`, add `SwitchVariant` case:

1. Evaluate scrutinee once.
2. Compute cached variant tag once (`x` for int enums, `x[0]` otherwise).
3. Lower to `if`/`elif` chain comparing tag to case variant ids.
4. Use existing `Env.ifElse` utilities so branch values remain lazy in the same way as existing `If` lowering.

Note: a balanced binary decision tree is intentionally deferred. We keep the first implementation simple and measurable.

## Detailed implementation plan

1. Add `SwitchVariant` node and invariants in `Matchless.scala`.
2. Add matrix-time `SwitchVariant` emission path in `matchExprMatrixCheap` with `SwitchVariantMinCases` gating.
3. Keep existing non-eligible paths on current `If` lowering.
4. Update exhaustive Matchless traversals in `Matchless.scala` for the new node.
5. Add `SwitchVariant` evaluation in `MatchlessToValue.scala`.
6. Add `Code.Switch` AST + rendering in `codegen/clang/Code.scala`.
7. Add Clang lowering for `SwitchVariant` in `codegen/clang/ClangGen.scala`.
8. Add Python lowering for `SwitchVariant` in `codegen/python/PythonGen.scala`.
9. Update compile-sensitive traversal helpers in tests that pattern-match on `Matchless.Expr`.
10. Add targeted tests for matrix switch formation and backend output.

## Testing plan

### Matchless and evaluator tests

1. Add Matchless tests that compile a 4+ branch orthogonal enum match and assert `SwitchVariant` appears.
2. Add test that union case shapes (for example `B | C`) lower correctly when combined with other enum variants.
3. Add negative tests where switch lowering should not trigger.
4. Fewer than threshold branches.
5. Mixed signature columns (not enum-only).
6. Non-orthogonal fallback path still uses existing ordered lowering.
7. Add evaluator parity test for representative enum values and wildcard/default behavior.

### C codegen tests

1. Add `CodeTest` coverage for rendering `switch` statement syntax.
2. Add `ClangGenTest` case with 8+ enum variants and assert generated C includes `switch (` and `case` labels.
3. Keep existing small-branch regression tests green.

### Python codegen tests

1. Add a Python generation regression for multi-variant enum match.
2. Assert scrutinee tag is computed once and reused in comparisons.

### Compile hygiene tests

1. Update helper traversals in `MatchlessTests`, `MatchlessRegressionTest`, `Issue1633Test`, and `MatchlessApplyArgsTest` to include `SwitchVariant` recursion.
2. Run full `core` test suite.

## Acceptance criteria

1. `Matchless.Expr` includes `SwitchVariant` with explicit default branch.
2. `matchExprMatrixCheap` can emit `SwitchVariant` directly for eligible enum columns.
3. Switch lowering triggers only when enum-case fanout is at least 4.
4. Union-case semantics and wildcard/default behavior are preserved.
5. `MatchlessToValue` evaluates `SwitchVariant` correctly.
6. Clang codegen emits C `switch` for `SwitchVariant`.
7. Python codegen handles `SwitchVariant` correctly using cached-tag conditional lowering.
8. New and updated tests pass.
9. Existing behavior for non-eligible matches remains unchanged.

## Risks and mitigations

1. Risk: semantic drift in matrix specialization when adding switch emission.
Mitigation: use existing specialization/default machinery and add union + fallback regression tests.

2. Risk: C switch fall-through bugs.
Mitigation: always emit explicit `break` per case and add renderer/codegen assertions.

3. Risk: broad compile failures from missing new-case handling under `-Werror`.
Mitigation: audit and update all exhaustive Matchless expression traversals in main and tests.

4. Risk: Python performance or code-size concerns with long `if`/`elif` chain.
Mitigation: keep simple first and add benchmark-driven follow-up for balanced tree lowering if needed.

5. Risk: threshold mis-tuning.
Mitigation: keep threshold as a local constant (`SwitchVariantMinCases`) and tune with profiling data.

## Rollout notes

1. Land as one PR including IR, evaluator, and both backends, with tests.
2. Keep threshold conservative (`4`) for first rollout.
3. Validate generated C on a high-branch enum sample before merge.
4. If regressions appear, temporarily disable switch emission by raising threshold while retaining node/backends.
5. Follow-up optimization work can revisit Python balanced-tree lowering and threshold tuning.
