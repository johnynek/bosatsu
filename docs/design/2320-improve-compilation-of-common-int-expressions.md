---
issue: 2320
priority: 3
touch_paths:
  - docs/design/2320-improve-compilation-of-common-int-expressions.md
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessToValue.scala
  - core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangGen.scala
  - core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessRegressionTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-04-09T20:53:49Z
---

# Design: lower common Int and Int64 comparison idioms to direct Matchless predicates

_Issue: #2320 (https://github.com/johnynek/bosatsu/issues/2320)_

## Summary

Add Matchless-level comparison predicate lowering for common `Int` and `Int64` boolean observations so the evaluator, C backend, and Python backend emit direct relational checks instead of materializing `Comparison` values and immediately matching on them.

## Context
`Matchless` already has comparison-specific boolean nodes such as `EqualsLit` and `LtEqLit`, and the backends already know how to lower those nodes to direct platform comparisons. What is still missing is a path from source-level builtin comparison functions such as `eq_Int`, `cmp_Int`, `eq_Int64`, and `cmp_Int64` into that IR.

Today, code like `cmp_Int(x, 0) matches LT | EQ` or `cmp_Int(a, b) matches GT | EQ` is lowered as a normal function call that produces a `Comparison` enum, followed by enum-tag inspection. Likewise, `eq_Int(x, 0)` stays as a normal function call that returns a boxed `Bool`. That is semantically correct, but it hides the fact that the surrounding code only needs a relational test.

This issue is specifically about those common boolean observations of `Int` and `Int64` comparisons. The goal is not a new source feature. The goal is to preserve the existing semantics while exposing direct comparison structure early enough that Matchless cleanup, the evaluator, and the backends can all use it.

## Problem
The current lowering shape forces unnecessary intermediate values into hot paths.

- `cmp_Int(... ) matches ...` materializes a `Comparison` and then immediately inspects its tag.
- `eq_Int(... )` materializes a `Bool` value even when the surrounding code only needs the truth value as a predicate.
- The C and Python backends cannot see that these shapes are just `==`, `!=`, `<`, `<=`, `>`, or `>=` over known numeric domains.
- Zero comparisons are especially common, but the current lowering does not expose them to existing helpers such as `bsts_integer_cmp_zero`.

The issue body suggests adding `Int64` to `Lit` and driving this through literal comparison nodes. In the current repo, that is broader than necessary. There is no source `Int64` literal syntax, and one of the motivating examples is `cmp_Int(a, b) matches GT | EQ`, where neither side is a literal. A literal-only design would leave an important part of the issue unsolved.

## Goals
- Lower common boolean observations of builtin `Int` and `Int64` comparisons into direct Matchless predicates.
- Preserve exact runtime semantics of `eq_Int`, `cmp_Int`, `eq_Int64`, and `cmp_Int64`.
- Preserve exact-once operand evaluation and source evaluation order.
- Let the evaluator, C backend, and Python backend share the same optimized IR meaning.
- Keep the design extensible so additional builtin comparison families can be added later through an explicit registry.

## Non-goals
- No source-language syntax changes.
- No new `Int64` literal syntax.
- No general rewrite of every materialized `Comparison` value in the program.
- No required runtime ABI change in the first cut.
- No attempt to optimize unrelated builtin families until their semantics and backend support are explicit.

## Proposed Architecture
### 1. Add direct comparison predicates to `Matchless.BoolExpr`
Add a small relation enum, for example `CompareRel`, with `Eq`, `Ne`, `Lt`, `Lte`, `Gt`, and `Gte`.

Add new Matchless boolean nodes for the two domains in scope:

- `CompareInt(left: CheapExpr[A], rel: CompareRel, right: CheapExpr[A])`
- `CompareInt64(left: CheapExpr[A], rel: CompareRel, right: CheapExpr[A])`

These nodes represent the predicate directly. They do not materialize `Comparison`. They do not materialize `Bool`. They only answer the boolean question the surrounding code actually asked.

`EqualsLit` and `LtEqLit` should stay. They are already useful for wide literal dispatch and should remain the preferred canonical form when a new comparison can be reduced to an existing literal-specific node without losing meaning. The new nodes cover the missing cases: dynamic-vs-dynamic comparisons, non-zero relations, and `Int64` comparisons.

Important invariant: evaluating a compare node evaluates `left` and `right` exactly once, left before right, and has no side effects beyond evaluating those operands.

### 2. Recognize builtin comparison observations during Matchless lowering
Add a small builtin-comparison registry in Matchless lowering keyed by package and function name. The initial entries should be only:

- `Bosatsu/Predef::eq_Int`
- `Bosatsu/Predef::cmp_Int`
- `Bosatsu/Num/Int64::eq_Int64`
- `Bosatsu/Num/Int64::cmp_Int64`

Use that registry in two places.

First, direct apps of `eq_Int` and `eq_Int64` should lower to a boolean-result expression built from the new predicate instead of to a generic `App`. The existing `If(cond, TrueExpr, FalseExpr)` result shape is sufficient here because both backends already collapse that shape efficiently.

Second, boolean observations of `cmp_Int` and `cmp_Int64` should lower to relations instead of to `Comparison` materialization plus variant inspection. The key mapping is from the observed `Comparison` constructors to a relation:

- `LT` -> `Lt`
- `LT | EQ` -> `Lte`
- `EQ` -> `Eq`
- `GT | EQ` -> `Gte`
- `GT` -> `Gt`
- `LT | GT` -> `Ne`
- all three constructors -> constant true
- no constructors -> constant false

This recognizer should work both on source-shaped typed matches and on the transient `let tmp = cmp_* ...` shapes the compiler currently creates while lowering selector expressions. That keeps the optimization focused on observable boolean use rather than on arbitrary dataflow of `Comparison` values.

Important invariant: unsupported or ambiguous shapes fall back to the existing generic lowering unchanged.

### 3. Do not extend `Lit` for `Int64` in this issue
`Int64` does not need to become a general Matchless literal to solve this issue.

The compiler only needs enough constant extraction to recognize obvious Int64 constants when they appear in exact constructor forms such as `int_low_bits_to_Int64(<int literal>)` and successful `int_to_Int64(<int literal>)`. That is enough to cover common zero and small-constant cases without introducing a new cross-IR literal category.

This keeps scope smaller and avoids pushing an `Int64` literal model through unrelated parser, pretty-printer, and normalization code.

### 4. Reuse the existing Bool-as-enum expression shape
This issue does not need a new Matchless expression node for boxed booleans.

The repo already has the right backend simplifications for `If(cond, TrueExpr, FalseExpr)`:

- C codegen can collapse enum0 true/false branches into a direct `alloc_enum0(...)` expression.
- Python codegen can collapse the same shape into a native ternary.

That means direct `eq_*` lowering can remain `If(comparePredicate, TrueExpr, FalseExpr)`, while guard lowering can unwrap directly to the underlying `BoolExpr` without re-boxing.

A small supporting cleanup is still valuable: `guardToBoolExpr` should recognize `If(cond, TrueExpr, FalseExpr)` and equivalent `Let(..., If(...))` wrappers so a Bool-producing comparison app used in guard position does not get re-lowered back through variant inspection.

### 5. Backend and evaluator lowering
`MatchlessToValue.scala` should evaluate the new predicate nodes directly so IR-level tests can compare optimized and reference forms without backend-specific assumptions.

`ClangGen.scala` should lower:

- `CompareInt(Eq, ...)` and `CompareInt(Ne, ...)` to direct integer equality checks when that is the clearest lowering.
- ordered `CompareInt` relations to `bsts_integer_cmp(lhs, rhs) <op> 0`.
- `CompareInt` against literal zero to `bsts_integer_cmp_zero(lhs) <op> 0` when the rhs is statically zero.
- `CompareInt64` to direct signed comparisons over `bsts_int64_to_int64(...)` results.

`PythonGen.scala` should lower:

- `CompareInt` to native Python integer comparisons.
- `CompareInt64` to native comparisons on the unboxed `Int64` payload already used by the Python runtime bridge.

Important invariant: optimized backend output must not materialize a `Comparison` value and then inspect it with `get_variant` or its Python equivalent.

### 6. Cleanup and compiler hygiene
Adding new `BoolExpr` cases requires the usual Matchless audit.

At minimum, update:

- ordering and structural equality
- substitution and renaming
- reference collection and name collection
- CSE and constant-folding
- `hasSideEffect`
- IR rendering for `tool show --ir matchless`
- test helper traversals that pattern-match exhaustively on `BoolExpr`

Constant-folding should evaluate new compare nodes when both sides are known constants. Literal-friendly `CompareInt` cases may canonicalize back to `EqualsLit` or `LtEqLit` when that reduces backend duplication.

## Behavioral Properties
The change is correct when these properties hold.

- Any optimized form of `eq_Int`, `eq_Int64`, `cmp_Int(... ) matches ...`, or `cmp_Int64(... ) matches ...` is observationally equivalent to the current generic lowering.
- Operand evaluation count and evaluation order are preserved exactly once.
- The mapping from a set of `Comparison` constructors to a relation is total, deterministic, and backend-independent.
- When the optimized path fires, generated C and Python do not allocate or inspect an intermediate `Comparison` value before producing the final boolean result.
- Unsupported shapes continue to compile through the old path with unchanged semantics.
- Int64 optimized paths stay in Int64 representation end-to-end and do not round-trip through boxed arbitrary-precision `Int` unless the source program explicitly asked for that conversion.
- Literal-literal cases fold to the same result as the builtin evaluator semantics.

## Testing Strategy
Property-check style tests should carry the semantic invariants.

- In `MatchlessTests.scala`, add ScalaCheck properties that compare an optimized predicate node against a reference unoptimized Matchless expression that still calls the builtin and inspects the resulting `Comparison` or `Bool`. Cover random `Int` pairs, random `Long` pairs for Int64, and each relation.
- Use the same property suite to cover the `Comparison`-set-to-relation mapping so the optimizer cannot drift on `LT | EQ`, `GT | EQ`, or `LT | GT`.
- If constant extraction is added for `int_low_bits_to_Int64(<literal>)` or `int_to_Int64(<literal>)`, property coverage should verify that extraction only fires on exact known-safe forms.

Narrower case-based tests are still the right fit where code shape or carefully constructed evaluation order matters.

- In `ClangGenTest.scala`, add representative source programs for `cmp_Int(x, 0) matches LT | EQ`, `cmp_Int(a, b) matches GT | EQ`, `eq_Int(x, 0)`, and Int64 analogs, then assert the optimized output no longer contains `get_variant` inspection of a temporary compare result.
- In `PythonGenTest.scala`, assert the generated code uses direct comparison expressions rather than computing a `Comparison` value and branching on it.
- In `EvaluationTest.scala`, keep a compact set of end-to-end source regressions covering zero-literal, dynamic-vs-dynamic, Int64, and explicit fallback cases.
- In `MatchlessRegressionTest.scala`, add a regression for the transient `let tmp = cmp_* ...` cleanup path and for exact-once evaluation with non-cheap operands.

## Acceptance Criteria
- `Matchless.BoolExpr` has direct predicate nodes for `Int` and `Int64` relations.
- Direct `eq_Int` and `eq_Int64` apps lower to those predicates rather than to generic function calls when their shape is supported.
- Boolean observations of `cmp_Int` and `cmp_Int64` lower to direct relations without materializing `Comparison`.
- The evaluator, C backend, and Python backend all implement the new predicate nodes.
- Representative optimized C and Python output for the motivating examples no longer inspects a temporary `Comparison` enum.
- Unsupported uses of materialized `Comparison` values continue to work through the existing path.
- Property-check coverage exists for semantic equivalence and relation mapping, and narrower case-based coverage exists for code shape, exact-once evaluation, Int64, and fallback behavior.
- `tool show --ir matchless` can render the new nodes.

## Risks
- Risk: over-aggressive recovery could rewrite arbitrary `Comparison`-valued expressions unsafely.
  Mitigation: restrict v1 to boolean observations of explicitly registered builtins and fall back otherwise.

- Risk: operand evaluation could be duplicated if non-cheap operands are embedded directly in the new nodes.
  Mitigation: require `CheapExpr` operands in the predicate nodes and use `Let` or `LetBool` to memoize before construction.

- Risk: backend drift, especially for Int64 signed comparisons and zero-specialized Int comparisons.
  Mitigation: keep the relation mapping centralized in Matchless and back it with both property tests and backend code-shape tests.

- Risk: expanding the registry too early to String, Char, or other builtin families could create partially supported semantics.
  Mitigation: keep the initial registry to `Int` and `Int64` only.

- Risk: Matchless compile hygiene breaks because many helpers pattern-match exhaustively on `BoolExpr`.
  Mitigation: treat the Matchless audit as part of the implementation plan and keep dedicated regression tests for IR traversal helpers.

## Rollout Notes
- Merging this design doc is the `planned` milestone for this lane. Implementation can continue afterward from the same child issue.
- Land the Matchless IR change, evaluator support, and both backend lowerings together so no backend is left interpreting the new nodes indirectly.
- No runtime ABI change is required for the first cut. If later profiling shows a missing runtime helper is worthwhile, that should be a follow-up optimization, not a prerequisite for this issue.
- If one backend path proves unstable during rollout, disable the registry entry for that builtin family in that path and keep the generic lowering rather than backing out the whole design.
