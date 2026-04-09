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

Add Matchless-level comparison lowering for common `Int`, `Int64`, and `Float64` boolean observations, plus explicit literal-side compare nodes and `LitInt64`, so the evaluator, C backend, and Python backend can emit direct relational checks instead of materializing `Comparison` values and immediately matching on them.

## Context
`Matchless` already has comparison-specific boolean nodes, and the backends already know how to lower direct comparison shapes to platform operations. What is still missing is a path from source-level builtin comparison functions such as `eq_Int`, `cmp_Int`, `eq_Float64`, `cmp_Float64`, `eq_Int64`, and `cmp_Int64` into a unified comparison IR.

Today, code like `cmp_Int(x, 0) matches LT | EQ`, `cmp_Int(a, b) matches GT | EQ`, or the analogous `Float64` and `Int64` forms is lowered as a normal function call that produces a `Comparison` enum, followed by enum-tag inspection. Likewise, `eq_Int(x, 0)` and `eq_Float64(x, y)` stay as normal function calls that return a boxed `Bool`. That is semantically correct, but it hides the fact that the surrounding code only needs a relational test.

This issue is specifically about those common boolean observations of `Int`, `Int64`, and `Float64` comparisons. The goal is not a new source feature. The goal is to preserve the existing semantics while exposing direct comparison structure early enough that Matchless cleanup, the evaluator, and the backends can all use it.

## Problem
The current lowering shape forces unnecessary intermediate values into hot paths.

- `cmp_Int(... ) matches ...` materializes a `Comparison` and then immediately inspects its tag.
- `eq_Int(... )`, `eq_Int64(... )`, and `eq_Float64(... )` materialize a `Bool` value even when the surrounding code only needs the truth value as a predicate.
- The C and Python backends cannot see that these shapes are just `==`, `!=`, `<`, `<=`, `>`, or `>=` over known numeric domains.
- Zero comparisons are especially common, but the current lowering does not expose them to existing helpers such as `bsts_integer_cmp_zero`.

To cover the full issue cleanly, the IR should grow in two directions at once: a unified literal-side comparison node for ordinary `Lit` values, and a dedicated `LitInt64` cheap expression so `Literal(Int)` plus conversion-to-`Int64` can be folded inside Matchless instead of being left to backend-specific reconstruction.

## Goals
- Lower common boolean observations of builtin `Int`, `Int64`, and `Float64` comparisons into direct Matchless predicates.
- Preserve exact runtime semantics of `eq_Int`, `cmp_Int`, `eq_Int64`, `cmp_Int64`, `eq_Float64`, and `cmp_Float64`.
- Preserve exact-once operand evaluation and source evaluation order.
- Let the evaluator, C backend, and Python backend share the same optimized IR meaning.
- Make literal-side comparison handling explicit through `CompareLit`, and make literal `Int64` values explicit through `LitInt64`, so backends no longer need to reconstruct those cases from higher-level call shapes.

## Non-goals
- No source-language syntax changes.
- No new `Int64` literal syntax.
- No general rewrite of every materialized `Comparison` value in the program.
- No required runtime ABI change in the first cut.
- No attempt to optimize unrelated builtin families beyond `Int`, `Int64`, and `Float64` until their semantics and backend support are explicit.

## Proposed Architecture
### 1. Add a small comparison IR family
Add a small relation enum, for example `CompareRel`, with `Eq`, `Ne`, `Lt`, `Lte`, `Gt`, and `Gte`.

Add three domain-specific dynamic comparison nodes:

- `CompareInt(left: CheapExpr[A], rel: CompareRel, right: CheapExpr[A])`
- `CompareInt64(left: CheapExpr[A], rel: CompareRel, right: CheapExpr[A])`
- `CompareFloat64(left: CheapExpr[A], rel: CompareRel, right: CheapExpr[A])`

Add one literal-specialized node for ordinary `Lit` values:

- `CompareLit(expr: CheapExpr[A], rel: CompareRel, lit: Lit)`

If the implementation keeps `EqualsLit` and `LtEqLit` temporarily during the migration, cleanup should canonicalize toward `CompareLit` rather than continuing to grow the older split. The point is to have one literal-side comparison shape instead of accumulating more one-off nodes.

Important invariant: evaluating any compare node evaluates its operands exactly once, left before right, and has no side effects beyond evaluating those operands.

### 2. Add `LitInt64` as a `CheapExpr` and normalize literal conversion in Matchless
Add `LitInt64(value: Long)` as a `CheapExpr`.

During Matchless lowering, recognize exact literal-conversion forms such as:

- `int_low_bits_to_Int64(<literal Int>)`
- successful `int_to_Int64(<literal Int>)`

and lower them immediately to `LitInt64`. This avoids carrying `Literal(Int)` plus a conversion call all the way into backend codegen, and it forces the evaluator and every backend to agree on one direct representation for literal `Int64` values.

Once this exists, literal `Int64` comparisons become ordinary `CompareInt64(expr, rel, LitInt64(v))` cases instead of backend-specific reconstruction from call shapes.

### 3. Recognize builtin comparison observations during Matchless lowering
Add a small builtin-comparison registry in Matchless lowering keyed by package and function name. The initial entries should be:

- `Bosatsu/Predef::eq_Int`
- `Bosatsu/Predef::cmp_Int`
- `Bosatsu/Predef::eq_Float64`
- `Bosatsu/Predef::cmp_Float64`
- `Bosatsu/Num/Int64::eq_Int64`
- `Bosatsu/Num/Int64::cmp_Int64`

Use that registry in two places.

First, direct apps of `eq_Int`, `eq_Float64`, and `eq_Int64` should lower to a boolean-result expression built from the new comparison nodes instead of to generic `App` nodes. The existing `If(cond, TrueExpr, FalseExpr)` result shape is sufficient here because both backends already collapse that shape efficiently.

Second, boolean observations of `cmp_Int`, `cmp_Float64`, and `cmp_Int64` should lower to relations instead of to `Comparison` materialization plus variant inspection. This should not be limited to `matches` syntax. It should cover any total boolean match over a `Comparison` result by collecting which constructors lead to logical true. For example:

- `cmp_Int(x, y) matches LT | GT` -> `CompareInt(x, Ne, y)`
- `match cmp_Int(x, y): case LT: True; case GT: True; case _: False` -> `CompareInt(x, Ne, y)`

The key mapping is from the observed `Comparison` constructors to a relation:

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

### 4. Reuse the existing Bool-as-enum expression shape
This issue does not need a new Matchless expression node for boxed booleans.

The repo already has the right backend simplifications for `If(cond, TrueExpr, FalseExpr)`:

- C codegen can collapse enum0 true/false branches into a direct `alloc_enum0(...)` expression.
- Python codegen can collapse the same shape into a native ternary.

That means direct `eq_*` lowering can remain `If(comparePredicate, TrueExpr, FalseExpr)`, while guard lowering can unwrap directly to the underlying `BoolExpr` without re-boxing.

A small supporting cleanup is still valuable: `guardToBoolExpr` should recognize `If(cond, TrueExpr, FalseExpr)` and equivalent `Let(..., If(...))` wrappers so a Bool-producing comparison app used in guard position does not get re-lowered back through variant inspection.

### 5. Backend and evaluator lowering
`MatchlessToValue.scala` should evaluate the new comparison nodes and `LitInt64` directly so IR-level tests can compare optimized and reference forms without backend-specific assumptions.

`ClangGen.scala` should lower:

- `CompareInt(Eq, ...)` and `CompareInt(Ne, ...)` to direct integer equality checks when that is the clearest lowering.
- ordered `CompareInt` relations to `bsts_integer_cmp(lhs, rhs) <op> 0`.
- `CompareInt` against literal zero to `bsts_integer_cmp_zero(lhs) <op> 0` when the rhs is statically zero.
- `CompareFloat64` to `bsts_float64_equals` for `Eq` and `Ne`, and to `bsts_float64_cmp_total(lhs, rhs) <op> 0` for ordered relations so NaN and signed-zero behavior stay aligned with the existing builtin contract.
- `CompareLit` to direct comparisons against char, string, integer, and float literals where that preserves the existing runtime semantics.
- `CompareInt64` to direct signed comparisons over `bsts_int64_to_int64(...)` results.
- `LitInt64` to raw `bsts_int64_from_int64(...)` constants.

`PythonGen.scala` should lower:

- `CompareInt` to native Python integer comparisons.
- `CompareFloat64` to the same total-order and equality semantics currently used by `cmp_Float64` and `eq_Float64`, including `NaN` and signed-zero handling.
- `CompareLit` to direct literal comparisons where that preserves the current builtin semantics.
- `CompareInt64` to native comparisons on the unboxed `Int64` payload already used by the Python runtime bridge.
- `LitInt64` to direct Python integer constants in the Int64 domain.

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
- test helper traversals that pattern-match exhaustively on `BoolExpr` or `CheapExpr`

Constant-folding should evaluate new compare nodes when both sides are known constants. If the older `EqualsLit` and `LtEqLit` nodes remain temporarily during implementation, they should fold forward into `CompareLit` or the new domain-specific nodes rather than staying the long-term canonical form.

## Behavioral Properties
The change is correct when these properties hold.

- Any optimized form of `eq_Int`, `eq_Int64`, `eq_Float64`, `cmp_Int(... ) matches ...`, `cmp_Int64(... ) matches ...`, or `cmp_Float64(... ) matches ...` is observationally equivalent to the current generic lowering.
- Operand evaluation count and evaluation order are preserved exactly once.
- The mapping from a set of `Comparison` constructors to a relation is total, deterministic, and backend-independent, whether the source was written with `matches` or with an explicit boolean `match`.
- When the optimized path fires, generated C and Python do not allocate or inspect an intermediate `Comparison` value before producing the final boolean result.
- Unsupported shapes continue to compile through the old path with unchanged semantics.
- Int64 optimized paths stay in Int64 representation end-to-end, and `LitInt64` produces the same result as the current literal-Int plus conversion-helper sequence.
- Float64 optimized paths preserve the current total-order behavior for ordered relations and the current equality behavior for `Eq` and `Ne`, including `NaN` and signed-zero cases.
- Literal-literal cases fold to the same result as the builtin evaluator semantics.

## Testing Strategy
Property-check style tests should carry the semantic invariants.

- In `MatchlessTests.scala`, add ScalaCheck properties that compare an optimized predicate node against a reference unoptimized Matchless expression that still calls the builtin and inspects the resulting `Comparison` or `Bool`. Cover random `Int` pairs, random `Long` pairs for Int64, representative `Float64` values including `NaN`, infinities, and signed zero, and each relation.
- Use the same property suite to cover the `Comparison`-set-to-relation mapping so the optimizer cannot drift on `LT | EQ`, `GT | EQ`, or `LT | GT`, including explicit boolean `match` forms that are equivalent to `matches`.
- Add property coverage for `LitInt64` extraction so `int_low_bits_to_Int64(<literal>)` and successful `int_to_Int64(<literal>)` rewrite exactly when intended and nowhere else.

Narrower case-based tests are still the right fit where code shape or carefully constructed evaluation order matters.

- In `ClangGenTest.scala`, add representative source programs for `cmp_Int(x, 0) matches LT | EQ`, `cmp_Int(a, b) matches GT | EQ`, `match cmp_Int(x, y): case LT: True; case GT: True; case _: False`, `eq_Int(x, 0)`, `eq_Float64(x, y)`, `cmp_Float64(x, y)`, and Int64 analogs, then assert the optimized output no longer contains `get_variant` inspection of a temporary compare result.
- In `PythonGenTest.scala`, assert the generated code uses direct comparison expressions rather than computing a `Comparison` value and branching on it, and that `LitInt64` lowers without a conversion call.
- In `EvaluationTest.scala`, keep a compact set of end-to-end source regressions covering zero-literal, dynamic-vs-dynamic, Float64 corner cases, Int64 literal conversion, and explicit fallback cases.
- In `MatchlessRegressionTest.scala`, add a regression for the transient `let tmp = cmp_* ...` cleanup path and for exact-once evaluation with non-cheap operands.

## Acceptance Criteria
- `Matchless.BoolExpr` has direct predicate nodes for `Int`, `Int64`, and `Float64` relations, plus a literal-specialized `CompareLit` node.
- `Matchless` has `LitInt64` as a `CheapExpr`, and literal `Int` plus conversion-to-`Int64` forms are folded to it during lowering when they are statically known.
- Direct `eq_Int`, `eq_Int64`, and `eq_Float64` apps lower to those predicates rather than to generic function calls when their shape is supported.
- Boolean observations of `cmp_Int`, `cmp_Int64`, and `cmp_Float64` lower to direct relations without materializing `Comparison`, including equivalent explicit boolean `match` forms.
- The evaluator, C backend, and Python backend all implement the new predicate nodes.
- Representative optimized C and Python output for the motivating examples no longer inspects a temporary `Comparison` enum.
- Unsupported uses of materialized `Comparison` values continue to work through the existing path.
- Property-check coverage exists for semantic equivalence, relation mapping, Float64 corner semantics, and `LitInt64` extraction, and narrower case-based coverage exists for code shape, exact-once evaluation, Int64, and fallback behavior.
- `tool show --ir matchless` can render the new nodes.

## Risks
- Risk: over-aggressive recovery could rewrite arbitrary `Comparison`-valued expressions unsafely.
  Mitigation: restrict v1 to boolean observations of explicitly registered builtins and fall back otherwise.

- Risk: operand evaluation could be duplicated if non-cheap operands are embedded directly in the new nodes.
  Mitigation: require `CheapExpr` operands in the predicate nodes and use `Let` or `LetBool` to memoize before construction.

- Risk: backend drift, especially for Int64 signed comparisons, zero-specialized Int comparisons, and Float64 total-order behavior around `NaN` and signed zero.
  Mitigation: keep the relation mapping centralized in Matchless and back it with both property tests and backend code-shape tests.

- Risk: `CompareLit` and `LitInt64` increase the amount of canonicalization work in Matchless cleanup.
  Mitigation: make `CompareLit` the preferred literal-side form from the start and keep `LitInt64` lowering narrowly scoped to exact literal-conversion patterns.

- Risk: expanding the registry too early to String, Char, or other builtin families could create partially supported semantics.
  Mitigation: keep the initial registry to `Int`, `Int64`, and `Float64` only.

- Risk: Matchless compile hygiene breaks because many helpers pattern-match exhaustively on `BoolExpr`.
  Mitigation: treat the Matchless audit as part of the implementation plan and keep dedicated regression tests for IR traversal helpers.

## Rollout Notes
- Merging this design doc is the `planned` milestone for this lane. Implementation can continue afterward from child issue `#2320` (`https://github.com/johnynek/bosatsu/issues/2320`).
- Land the Matchless IR change, evaluator support, and both backend lowerings together so no backend is left interpreting the new nodes indirectly.
- No runtime ABI change is required for the first cut. If later profiling shows a missing runtime helper is worthwhile, that should be a follow-up optimization, not a prerequisite for this issue.
- If one backend path proves unstable during rollout, disable the registry entry for that builtin family in that path and keep the generic lowering rather than backing out the whole design.
