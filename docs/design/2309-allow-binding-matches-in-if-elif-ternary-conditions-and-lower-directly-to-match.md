---
issue: 2309
priority: 3
touch_paths:
  - docs/design/2309-allow-binding-matches-in-if-elif-ternary-conditions-and-lower-directly-to-match.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - docs/src/main/paradox/language_guide.md
  - core/src/test/scala/dev/bosatsu/DeclarationTest.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprTest.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-04-08T01:59:24Z
---

# Design: allow binding `matches` in `if`/`elif`/ternary conditions and lower directly to `match`

_Issue: #2309 (https://github.com/johnynek/bosatsu/issues/2309)_

## Summary

Use scoped conditional lowering in `Declaration` and `SourceConverter` so direct `matches` conditions bind names only in their true arm and lower to nested `Expr.Match` trees equivalent to explicit `match` desugarings, without adding new IR.

## Context

Bosatsu already has the surface pieces needed to represent this feature: `Declaration.Matches` already carries an optional guard, ternary expressions already lower through `IfElse`, and the rest of the compiler already knows how to typecheck, normalize, and codegen guarded `Expr.Match` branches.

The current gap is in source-level scope and lowering:

1. `SourceConverter` only lowers `if x matches p: ... else: ...` directly to `match` when `p.names.isEmpty`.
2. The general `IfElse` path flattens an `if`/`elif` chain into a boolean-oriented `Expr.Match`, which cannot put names from a later `matches` condition in scope for that arm body.
3. `Declaration.freeVars`, `Declaration.substitute`, and ternary-aware name walkers do not explicitly model names from a condition as being in scope for the corresponding true arm.

That is why guarded `matches` works today, but binding `matches` in `if`/`elif`/ternary conditions does not.

## Goals

1. Allow direct `cond matches pattern` conditions in `if`, `elif`, and ternary expressions to bind names for that arm's true body.
2. Keep those names in scope inside the `matches` guard itself, but nowhere else.
3. Lower conditional `matches` directly to real `Expr.Match` nodes instead of first booleanizing the condition.
4. Preserve left-to-right conditional semantics and single evaluation of each condition or scrutinee.
5. Keep standalone `matches` behavior unchanged outside these scoping conditional positions.

## Non-goals

1. No new `Expr`, `TypedExpr`, Matchless, or backend IR node.
2. No general change to standalone binding `matches`; `expr matches pat` outside a scoping conditional should keep today's restrictions.
3. No change to runtime semantics beyond making the new syntax equivalent to explicit nested `match`.
4. No parser redesign; any syntax work should stay within `Declaration.scala` and should be regression-level unless implementation finds a real parse hole.

## Architecture And Implementation Plan

### 1. Recognize direct conditional `matches`

Add a small helper, shared conceptually by `Declaration` bookkeeping and `SourceConverter`, that peels `Parens` and `Annotation` and recognizes a condition whose top-level shape is `Matches(scrutinee, pattern, guard)`.

This helper must stay intentionally narrow:

1. `if foo matches Some(a):` binds `a`.
2. `if (foo matches Some(a)):` binds `a`.
3. `if (foo matches Some(a) if pred(a)):` and the analogous ternary forms behave the same as the bare form.
4. Expressions where `matches` is not the top-level condition do not become binders.

The feature is about direct condition position, not arbitrary boolean composition.

### 2. Make condition-introduced binders explicit in `Declaration`

`Declaration.scala` needs to model that a direct conditional `matches` introduces names for only one arm.

For `IfElse`:

1. The condition is analyzed in the incoming scope.
2. If the condition is a direct `Matches`, its `pattern.names` are added only while analyzing that arm body.
3. Later `elif` conditions, later `elif` bodies, and the final `else` are analyzed in the original outer scope.

For `Ternary`:

1. The condition is analyzed in the incoming scope.
2. If the condition is a direct `Matches`, its `pattern.names` are added only while analyzing the true expression.
3. The false expression stays in the original outer scope.

This needs to be reflected in the binder-sensitive helpers, especially:

1. `freeVars`
2. `allNames`
3. `substitute`

`allNames` already picks up some `IfElse` cases incidentally because it traverses the condition before the body, but that is traversal-order luck, not an explicit scope rule, and ternary plus substitution are still wrong today. The implementation should make the scope relationship explicit rather than relying on visit order.

### 3. Replace narrow special-casing in `SourceConverter` with conditional-aware lowering

`SourceConverter.scala` should grow a helper that lowers an `if`/`elif` chain arm-by-arm.

The key rule is:

1. If a condition is a direct `Matches`, lower that arm as a real `Expr.Match` on the scrutinee.
2. Put the remainder of the conditional chain in the wildcard fallback branch.
3. Lower the arm body under `withBound(_, pattern.names)`.
4. Lower the `matches` guard under the same bound-name extension.
5. Keep the remaining chain outside that bound-name extension.

For example:

```bosatsu
if foo matches Some(a):
  fn(a)
elif bar matches Some(b):
  gn(b)
else:
  h
```

lowers as if it were:

```bosatsu
match foo:
  case Some(a): fn(a)
  case _:
    match bar:
      case Some(b): gn(b)
      case _: h
```

This same helper should also cover mixed chains such as:

```bosatsu
if c1:
  t1
elif foo matches Some(a):
  fn(a)
else:
  h
```

which should lower as if it were:

```bosatsu
match c1:
  case True: t1
  case False:
    match foo:
      case Some(a): fn(a)
      case _: h
```

That recursive fallback shape is required. A flat boolean `False if nextCond` branch cannot represent names introduced by a later `matches` condition.

Implementation details:

1. Keep the current flat boolean lowering for all-boolean chains to avoid unrelated churn.
2. When any arm needs the scoped `matches` path, lower the chain recursively from that point so later arms can still bind names correctly.
3. Guard canonicalization should stay aligned with existing behavior: if the converted `matches` guard is effectively `True`, erase the guard and emit an unguarded branch.
4. The standalone `Declaration.Matches` lowering path remains in place for non-conditional uses and continues to reject unscoped binding patterns.

The existing special case for `if x matches p` with `p.names.isEmpty` becomes a subset of this new lowering and can be removed or folded into the new helper.

### 4. Keep ternary aligned by reusing the same lowering path

`Ternary` already routes through `IfElse` lowering. That should stay true.

Once `Declaration` scope bookkeeping and `SourceConverter` conditional lowering are updated, both of these forms:

```bosatsu
f(a) if foo matches Some(a) else g
f(a) if foo matches Some(a) if pred(a) else g
```

should lower through the same machinery as block `if`, with `a` in scope only in the true expression and inside `pred(a)`.

No ternary-specific IR work should be needed.

### 5. Diagnostics and totality

The implementation should preserve the useful parts of current diagnostics without adding a second parallel lowering system.

1. Standalone `expr matches pat` outside a scoping conditional keeps the current `MatchesPatternBinding` rejection.
2. Nested guarded `matches` inside a `matches` guard keep the current parentheses requirement and error text.
3. Conditional `matches` arms should not trigger the standalone binding error, because their bindings are consumed by the arm body.
4. Irrefutable conditional patterns should still fail clearly after lowering. The acceptable target is explicit-`match` style diagnostics, even if the exact error class differs from standalone `MatchesAlwaysTrue`.

That tradeoff keeps the implementation localized to `Declaration` and `SourceConverter` instead of introducing special downstream handling just to preserve a source-specific error label.

## Behavioral Properties

The implementation should preserve these invariants:

1. Conditional evaluation stays left-to-right and short-circuiting. Later `elif` conditions are not evaluated when an earlier arm succeeds.
2. A direct conditional `matches` scrutinee is evaluated exactly once, and only if execution reaches that arm.
3. Pattern bindings from a condition are visible in exactly two places: the `matches` guard and the corresponding true body.
4. Those bindings are not visible in the false branch, later `elif` conditions or bodies, or the surrounding scope after the whole conditional expression.
5. Parenthesized and annotated direct conditions are semantically identical to the bare direct condition.
6. Lowered code is observationally equivalent to the explicit nested `match` desugaring, not merely to a booleanized approximation.
7. Standalone `matches` behavior remains unchanged.

## Testing Strategy

### Property-check style coverage

Use property-style tests where the contract is about scope bookkeeping or syntactic stability rather than one exact lowered tree.

1. `DeclarationTest.scala`: keep the existing `freeVars subset allNames` and substitution laws, and add targeted property or law coverage showing that condition-bound names are not free in the true arm but still do not leak outside the conditional.
2. Existing declaration round-trip coverage should continue exercising `Matches`, `IfElse`, and `Ternary`; if the current generators underproduce the new shapes, bias them toward direct-conditional `matches` rather than adding ad hoc parser-only logic.

Property-style tests are the right tool here because the regression risk is capture, leakage, or walker inconsistency across many tree shapes.

### Case-based coverage

Use targeted case tests where exact parse grouping, exact lowered shape, or exact diagnostics matter.

1. `ParserTest.scala`: parse and round-trip `if`/`elif`/ternary forms with binding `matches`, guarded `matches`, and parenthesized conditions.
2. `SourceConverterTest.scala`: assert desugaring equivalence to explicit nested `match` for simple, guarded, and mixed chains, including parenthesized conditions.
3. `ErrorMessageTest.scala`: prove the binding is rejected in the false branch and after the whole conditional, while standalone binding `matches` still reports the existing source-converter error.
4. `EvaluationTest.scala`: true-path, false-path, guarded-path, and mixed-chain runtime behavior.
5. `TypedExprTest.scala`: normalization of the new syntax matches the equivalent explicit `match` code shape and does not regress into extra booleanized matches.
6. `TypedTotalityTest.scala`: irrefutable conditional patterns still produce a clear diagnostic after lowering.

Case-based tests are better here because parse precedence, exact scope failure points, and lowered `Expr.Match` structure are not well described by a generic property.

## Acceptance Criteria

1. `if`, `elif`, and ternary conditions whose top-level shape is `matches` may bind names for the corresponding true arm.
2. Those names are in scope in the `matches` guard and the true arm only.
3. Mixed boolean and binding-`matches` chains lower as explicit nested matches would, with later arms placed in fallback branches.
4. Conditional `matches` arms lower directly to `Expr.Match` rather than through standalone boolean `matches` lowering.
5. Standalone binding `matches` outside scoping conditional positions still fails with the current source-converter diagnostic.
6. Parenthesized direct conditions behave the same as bare direct conditions.
7. Runtime behavior and normalized typed code match the equivalent explicit `match` desugaring.
8. Documentation and regression tests are updated.

## Risks And Rollout

The main implementation risks are scope drift and accidental reordering of evaluation.

1. If `Declaration` and `SourceConverter` do not use the same direct-condition rule, the parser may accept programs whose walker logic and lowering disagree about scope.
2. If mixed chains are flattened too aggressively, later `matches` arms will either lose their bindings or evaluate in the wrong order.
3. If the direct-condition helper strips too much syntax, bindings could leak into cases that were not intended to introduce scope.

Mitigations:

1. Keep one narrow notion of direct conditional `matches`: peel only `Parens` and `Annotation`.
2. Compare lowering against explicit nested `match` desugarings in unit tests.
3. Keep standalone `Matches` lowering unchanged so the new feature is isolated to conditional lowering.

Rollout notes:

1. This design-doc PR is the `planned` milestone for the lane.
2. The implementation can continue afterward under the same child issue.
3. No feature flag is needed. The change is source-compatible because it only accepts programs that are currently rejected.
4. `docs/src/main/paradox/language_guide.md` should be updated in the implementation PR so the new scoping rule is documented alongside existing `matches` guard examples.
