---
issue: 2309
priority: 3
touch_paths:
  - docs/design/2309-allow-binding-matches-in-if-elif-ternary-conditions-and-lower-directly-to-match.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/test/scala/dev/bosatsu/DeclarationTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-04-06T23:20:09Z
---

# Issue #2309 Design: Allow binding `matches` in `if`/`elif`/ternary conditions

_Issue: #2309 (https://github.com/johnynek/bosatsu/issues/2309)_

## Summary

Teach `SourceConverter` to lower conditional `matches` arms directly to real `match` branches, and update `Declaration` scope bookkeeping so bindings from `cond matches pattern` are visible only in that arm's guard and true body.

## Context
`Declaration.Matches` already has an optional guard, and `if`/`elif`/ternary conditions already parse as ordinary `NonBinding` expressions. The missing behavior is semantic, not grammatical.

Today:
1. Standalone `matches` lowering in `SourceConverter` rejects binding patterns unless the bindings are used only inside the `matches` guard.
2. `IfElse` has one narrow fast path for a leading binding-free `matches`, but the general lowering rewrites the chain as boolean tests.
3. `Ternary` routes through `IfElse`, so it inherits the same limitation.
4. `Declaration.freeVars`, `allNames`, and `substitute` do not treat bindings introduced by a conditional `matches` as visible in the corresponding true arm.

Because of that, code such as:

```bosatsu
if foo matches Some(a):
  fn(a)
elif bar matches Some(b):
  gn(b)
else:
  h
```

is parsed, but it cannot be source-converted with the intended scope.

Issue #2309 is therefore primarily a source-AST scoping and `SourceConverter` lowering change. No new runtime representation is required.

## Goals
1. Allow binding `matches` in `if` and `elif` conditions.
2. Allow binding `matches` in ternary conditions.
3. Put condition-introduced bindings in scope for the `matches` guard and the corresponding true arm only.
4. Lower these conditional forms directly to ordinary `match` structure rather than through a booleanized `matches` expression.
5. Preserve existing behavior for standalone `matches`, including rejection of non-scoped binding forms.
6. Preserve downstream architecture: no new `Expr`, `TypedExpr`, `Matchless`, or backend node.

## Non-goals
1. No change to the meaning of standalone `x matches p` outside conditional position.
2. No grammar expansion or tree-sitter change; the relevant surface syntax already parses today.
3. No general rewrite of all boolean `if` lowering beyond what is needed to support conditional `matches` scope.
4. No backend- or runtime-specific optimization work in this issue.

## Design decision
Treat `cond matches pattern` in conditional position as a scoped conditional match, not as a generic boolean expression.

In other words, when `matches` appears as the condition of `if`, `elif`, or ternary, lowering should use the existing `match` pipeline so that the arm body is converted under the pattern bindings and the remaining chain is placed in the wildcard fallback.

The intended meaning is the explicit `match` desugaring users would write by hand. For example:

```bosatsu
if foo matches Some(a):
  fn(a)
elif bar matches Some(b):
  gn(b)
else:
  h
```

becomes the equivalent of:

```bosatsu
match foo:
  case Some(a): fn(a)
  case _:
    match bar:
      case Some(b): gn(b)
      case _: h
```

and:

```bosatsu
f(a) if foo matches Some(a) else g
```

becomes the equivalent of:

```bosatsu
match foo:
  case Some(a): f(a)
  case _: g
```

This keeps the feature definition simple: conditional `matches` should behave exactly like explicit `match`, with no extra scope rule invented just for `if` or ternary.

## Proposed architecture
### 1. Centralize recognition of conditional `matches`
Add a small helper that strips transparent wrappers such as `Parens` and `Annotation` and classifies whether a condition is a `Matches` node, along with its pattern binders.

That helper should be reused in both `Declaration.scala` and `SourceConverter.scala` so the source-level scope model and the lowering model cannot drift apart.

### 2. Update source-level bookkeeping in `Declaration.scala`
For `IfElse` and `Ternary`, apply the same scope rule the lowering will use:
1. Traverse the condition in the outer scope.
2. If the condition is a binding `Matches`, traverse only the corresponding true arm in `outerScope ++ pattern.names`.
3. Traverse later `elif` arms, `else`, and ternary false branches in the original outer scope.

This affects at least:
1. `freeVars`
2. `allNames`
3. `substitute`

No AST shape change is required. `Declaration.Matches` already carries the needed information.

### 3. Replace the ad hoc conditional lowering in `SourceConverter.scala`
Factor the current `IfElse` lowering into a helper that can lower the chain segment by segment.

For a conditional arm whose condition is `Matches(scrutinee, pattern, guard)`:
1. Lower it as an ordinary `Expr.Match`, or via an equivalent synthetic `Declaration.Match` that then flows through the existing `Match` lowering path.
2. Lower the true arm under `withBound(_, pattern.names)`.
3. Lower the `matches` guard under the same bound names.
4. Lower the fallback chain without those names.
5. Put the remaining `elif`/`else` chain in the wildcard branch.

The key shape is:
1. First branch: `pattern`, optional `guard`, true-arm body.
2. Fallback branch: `_`, no guard, recursively lowered remainder.

For purely boolean segments, keep the current boolean-oriented lowering. If a later arm needs conditional-match lowering, that recursively lowered result becomes the final false fallback for the preceding boolean segment. This preserves the existing shape where it is already sufficient, while still allowing binding `matches` later in the chain.

The current narrow `p.names.isEmpty` special case should be removed or subsumed so there is only one conditional-`matches` lowering path.

### 4. Route ternary through the same helper
`Ternary` should continue to lower by delegating to the `IfElse` path, but the shared helper now needs to understand conditional `matches` scope.

That keeps these forms aligned:

```bosatsu
f(a) if foo matches Some(a) else g
f(a) if (foo matches Some(a)) else g
f(a) if foo matches Some(a) if pred(a) else g
```

The parser already produces a usable source AST for these forms. The change is in how the conditional is analyzed and lowered.

### 5. Keep standalone `matches` behavior intact
The existing standalone `Matches` branch in `SourceConverter` remains responsible for:
1. Ordinary boolean `matches` expressions.
2. Rejecting binding patterns outside a scoping conditional context.
3. Existing guarded-`matches` semantics.
4. Existing nested guarded-`matches` precedence diagnostics.

This keeps the new feature narrow: only conditional position grants body scope.

### 6. Diagnostics and downstream phases
No new downstream IR is required. After source conversion, typed inference, totality checking, normalization, Matchless lowering, and codegen should see the same shape as if the user had written the equivalent explicit `match`.

Diagnostic expectations:
1. Standalone irrefutable `matches` should keep the current always-true feedback.
2. Irrefutable conditional matches should still produce sensible explicit-match-equivalent diagnostics, even if the wording comes from unreachable-branch or totality machinery instead of `MatchesPatternAlwaysTrue`.
3. Region and tag selection should remain local enough that errors still point at the arm or condition that introduced the pattern.

## Behavioral properties and invariants
1. A conditional `matches` arm is alpha-equivalent to the explicit nested `match` desugaring for the same source program.
2. Names introduced by `cond matches pattern` are in scope inside that `matches` guard and in the corresponding true arm body.
3. Those names are not in scope in later `elif` conditions, later `elif` bodies, the `else` body, the ternary false branch, or after the whole conditional expression.
4. Pattern failure and guard failure both fall through exactly as if the user had written the wildcard branch of the explicit `match`.
5. The scrutinee for a conditional `matches` arm is evaluated once for that arm, and the lowering does not re-express the arm as `Bool` and then rematch on `True`/`False`.
6. Standalone binding `matches` remain invalid outside scoping conditional contexts.
7. Parenthesized conditional forms behave the same as the unparenthesized form.
8. Purely boolean conditional behavior is unchanged apart from being able to delegate to a later nested `match` fallback when the chain contains a conditional `matches` arm.
9. No new typed or backend-visible representation is introduced.

## Testing plan
### Property-check style tests
Property-style coverage belongs mainly in `DeclarationTest.scala`, because the new risk is lexical-scope bookkeeping.

Add targeted properties over constructed or generated `IfElse` and `Ternary` nodes whose condition is a binding `Matches`:
1. `freeVars` excludes names that are only introduced by the conditional `matches` and used in the corresponding true arm.
2. `allNames` still includes those binders.
3. `substitute` does not rewrite or capture conditional-match binders in the true arm, but still rewrites the condition and fallback branches when legal.
4. Existing broad structural laws such as `freeVars subset allNames` continue to hold for these shapes.

The existing parser/declaration round-trip properties should continue to run, but exact parser precedence and lowering shape are better enforced with focused examples than with random generation.

### Case-based tests
Case-based tests are the right fit for exact lowering structure, precedence, and diagnostics.

1. `ParserTest.scala`
Explicit regressions for:
`if foo matches Some(a): ...`
`elif bar matches Some(b): ...`
`f(a) if foo matches Some(a) else g`
Guarded forms and parenthesized forms that should classify the same way.

2. `SourceConverterTest.scala`
Check exact lowering shape for:
`if foo matches Some(a): body else: rest`
Multiple binding `elif` arms lowering to nested fallback matches.
Mixed boolean and binding chains in both orders.
Ternary conditional matches.
Guarded conditional matches.
Parenthesized conditions lowering the same way.

3. `ErrorMessageTest.scala`
Verify that:
A bound name is not visible in the false branch.
A bound name is not visible in later `elif` arms or after the conditional.
Standalone binding `matches` still give the existing source-converter-style error.
Nested guarded-`matches` precedence errors remain intact.

4. `EvaluationTest.scala`
End-to-end true and false cases for `if`, `elif`, and ternary.
Guard-pass and guard-fail cases.
Mixed chains whose semantics should match the explicit nested `match` desugaring.

5. `TypedTotalityTest.scala`
Irrefutable conditional patterns still produce sensible diagnostics and do not regress compared with writing the equivalent explicit `match`.

## Acceptance criteria
1. Bosatsu accepts binding `matches` in `if` and `elif` conditions, with bindings available in the corresponding body.
2. Bosatsu accepts binding `matches` in ternary conditions, with bindings available in the ternary true expression.
3. Condition-introduced bindings are in scope inside the `matches` guard and nowhere outside the corresponding true arm.
4. Standalone binding `matches` outside conditional position still fail with the existing source-level rule.
5. Conditional `matches` arms lower directly to ordinary `match` structure instead of first lowering to `Bool` and then branching on `True` and `False`.
6. Mixed boolean and binding conditional chains preserve the explicit nested-`match` semantics described in the issue.
7. Parenthesized conditional `matches` forms lower the same way as unparenthesized forms.
8. No new `Expr`, `TypedExpr`, `Matchless`, or backend node is introduced.
9. Structural scope tests cover `Declaration.freeVars`, `allNames`, and `substitute` for conditional-match binders.
10. Focused parser, source-converter, evaluation, error-message, and totality tests cover the new behavior.
11. User-facing docs mention that conditional `matches` binders are available only in the corresponding true arm.

## Risks and mitigations
1. Risk: scope leakage into later arms or false branches. Mitigation: centralize condition classification and reuse the same helper in both `Declaration` walkers and `SourceConverter` lowering.
2. Risk: losing the current optimized shape for ordinary boolean `if` chains. Mitigation: preserve the existing boolean lowering for pure boolean segments and only nest where a conditional `matches` arm requires it.
3. Risk: parenthesized forms behave differently from bare forms. Mitigation: classify conditions after stripping transparent wrappers and add explicit parenthesized regressions.
4. Risk: diagnostic wording or source regions become less precise after direct lowering. Mitigation: preserve arm-local tags and add focused error-message and totality tests.
5. Risk: implementation accidentally broadens standalone `matches` semantics. Mitigation: keep standalone `Matches` conversion and diagnostics as a separate path, and add explicit rejection tests.

## Rollout notes
1. Merging this design doc is the `planned` milestone for this lane. Implementation may continue from the same child issue afterward.
2. Ship the implementation as one PR covering `Declaration`, `SourceConverter`, tests, and docs. No feature flag or migration is needed.
3. Run focused suites first: `coreJVM/testOnly dev.bosatsu.DeclarationTest`, `coreJVM/testOnly dev.bosatsu.ParserTest`, `coreJVM/testOnly dev.bosatsu.SourceConverterTest`, `coreJVM/testOnly dev.bosatsu.ErrorMessageTest`, `coreJVM/testOnly dev.bosatsu.EvaluationTest`, and `coreJVM/testOnly dev.bosatsu.TypedTotalityTest`.
4. No tree-sitter or syntax-highlighting rollout work is expected, because the surface syntax already exists; the change is in scope and lowering behavior.
