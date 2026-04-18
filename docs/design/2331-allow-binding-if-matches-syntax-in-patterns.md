---
issue: 2331
priority: 3
touch_paths:
  - docs/design/2331-allow-binding-if-matches-syntax-in-patterns.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Expr.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/main/scala/dev/bosatsu/TypeValidator.scala
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/UnusedLetCheck.scala
  - core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala
  - core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala
  - core/src/main/scala/dev/bosatsu/TypedExprLoopRecurLowering.scala
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/SelfCallKind.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala
  - core/src/test/scala/dev/bosatsu/DeclarationTest.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - core/src/test/scala/dev/bosatsu/ShadowedBindingTypeCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/tool/ShowEdnRoundTripTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-04-18T21:36:48Z
---

# Issue #2331 Design: allow binding `matches` syntax in match guards

_Issue: #2331 (https://github.com/johnynek/bosatsu/issues/2331)_

## Summary

Add a scoped branch-guard model so `case ... if expr matches pattern:` can bind names into the same branch body while preserving guarded-branch totality semantics, postponable shadow diagnostics, and a Matchless-backend-neutral lowering.

## Context

Issue #2195 added guarded `matches` as Bool-returning sugar, and issue #2309 taught `if`/`elif`/ternary conditions to treat top-level `expr matches pattern` as a scoped conditional form. Match branch guards already parse any `NonBinding`, so code like:

```bosatsu
match foo:
  case (x, y) if as_even(x) matches Some(even_x):
    use(y, even_x)
```

already parses.

The missing behavior is semantic. Today `SourceConverter` converts a branch guard under the outer pattern bindings only, and the branch body is converted under the same outer bindings. Names introduced by a top-level `matches` inside the guard remain local to that `matches` guard and are not available in the branch body.

A direct replay of the issue #2309 strategy is not sufficient here. Rewriting the branch to an explicit nested `match` too early would either:

1. lose the fact that the outer branch is still guarded for totality and reachability, or
2. duplicate the remaining-branches continuation and downstream diagnostics.

This issue is therefore the first guard feature that needs the compiler to preserve guard-introduced binders past source conversion, while still keeping the outer branch visibly guarded.

## Goals

1. Allow a top-level `expr matches pattern` branch guard to bind names that are in scope for the same branch body.
2. Keep the existing operational order: outer pattern match first, then guard scrutinee evaluation, then inner guard pattern check, then optional inner guard predicate, then branch body.
3. Preserve the outer branch's guarded status for totality and reachability, except in the effectively-trivial case where the inner `matches` is provably always successful.
4. Evaluate the guard scrutinee exactly once per attempted branch.
5. Reuse existing postponable shadowing and unused-binding diagnostics where they already express the intended behavior.
6. Avoid a Matchless AST or backend schema change.

## Non-goals

1. No new surface syntax, parser precedence rule, or tree-sitter change.
2. No new body scope for arbitrary nested `matches` inside a larger boolean guard expression. Only the whole guard expression, after stripping transparent parens/annotations, can open the extra scope.
3. No attempt to make nontrivial guarded branches contribute to coverage.
4. No new hard source-converter error purely for outer-pattern/guard-pattern name collisions if the existing postponable diagnostics remain adequate.
5. No backend-specific optimization work beyond preserving single evaluation of the guard scrutinee.

## Proposed Model

### 1. Keep the source syntax, add an explicit guard model below `Declaration`

`Declaration.MatchBranch.guard` can stay as `Option[NonBinding]`; the parser already accepts the requested surface form.

The new structure is needed in `Expr.Branch` and `TypedExpr.Branch`, where the compiler must distinguish:

1. an ordinary boolean guard, which introduces no new names, and
2. a scoped guard match, which introduces pattern bindings for the inner guard and the branch body.

Introduce an explicit guard ADT in the untyped and typed IRs, conceptually:

- `BoolGuard(expr)`
- `MatchGuard(arg, pattern, guardOpt, outerAnnotations)`

`MatchGuard` reuses the same meaning as conditional `matches` in issue #2309: `arg` is tested against `pattern`, `guardOpt` runs in the inner pattern scope, and the successful branch body sees both the outer branch pattern bindings and the inner guard-pattern bindings.

`outerAnnotations` represents any annotation wrappers that appeared on the whole guard condition before `ConditionalMatch.unapply` classified it. That field, or an equivalent wrapper structure, should be carried through source conversion so type inference can still validate the overall guard as a `Bool`-producing condition and report annotation errors at the guard boundary instead of silently discarding them.

Use the existing `Declaration.ConditionalMatch.unapply` logic as the classifier for whole-guard conditional `matches`, including transparent `Parens` and `Annotation` wrappers. That keeps `if`/ternary and `match` branch scoping aligned.

### 2. Preserve outer guardedness, but recognize effectively-unguarded inner matches

A `MatchGuard` is still an outer branch guard unless it canonicalizes to the trivial case:

1. the inner pattern is definitely total, and
2. the inner guard is absent or canonicalizes to Predef `True`.

Only that trivial case should behave like an unguarded outer branch for coverage and reachability. Every other `MatchGuard` remains guarded, even if its inner pattern is irrefutable but its inner predicate can fail.

This mirrors the current guard-canonicalization behavior for ordinary guarded branches, but extends it to the new guard kind.

### 3. Right-most bindings win in the branch body

If the outer branch pattern and the inner guard pattern bind the same name, the branch body should resolve that name to the inner guard binding.

That is the same shadowing rule Bosatsu already uses elsewhere: lexical scope is still literal, and the right-most binding wins. The implementation should not special-case collisions into a new hard error at source-conversion time.

Instead, rely on the existing postponable diagnostics after typechecking:

1. `ShadowedBindingTypeCheck` should continue to report a type-changing shadow if the inner guard binding changes the type.
2. `UnusedLetCheck` should continue to report the earlier outer binding as unused when it is fully shadowed and no longer referenced.

If those existing diagnostics turn out to be too indirect in practice, a dedicated focused diagnostic can be added later without changing the core lowering model.

## Compiler Pipeline Impact

### `Declaration.scala`: source-level scope bookkeeping

`Declaration` walkers already have special handling for conditional `matches` in `if`/ternary. Match branches need the same treatment.

For a branch:

```bosatsu
case p_outer if guard_expr:
  body
```

where `guard_expr` classifies as a top-level conditional `matches` with inner pattern `p_inner`:

1. `guard_expr` itself is traversed in the outer pattern scope only, because the inner pattern bindings are still local to that guard expression.
2. `body` is traversed in `outerScope ++ p_outer.names ++ p_inner.names`.

This affects at least:

1. `freeVars`
2. `allNames`
3. `substitute`

The body-scope extension must happen only for top-level classified conditional `matches`, not for nested `matches` that appear inside a larger boolean expression.

### `SourceConverter.scala`: classify, canonicalize, and build the richer guard

`SourceConverter` should stop flattening every branch guard to `Option[Expr]`.

Instead:

1. ordinary boolean guards lower to `BoolGuard`.
2. top-level conditional `matches` guards lower to `MatchGuard`, including the preserved outer guard annotations from the source form.
3. inner guard canonicalization (`if True` => no inner guard) should be applied before deciding whether the whole guard is effectively unguarded.
4. those preserved outer annotations should live on the `MatchGuard` node itself, so annotated conditional-match guards still fail at the boolean-result level rather than silently disappearing after classification.

This keeps the scope-relevant information alive until typed inference and Matchless lowering, instead of trying to recover it later from synthetic nested matches.

### `Expr.scala` and `TypedExpr.scala`: branch traversal and helpers

Once branch guards are no longer `Option[Expr]`, every generic branch walker needs a shared helper instead of hand-written `branch.guard.foreach(...)` logic.

The important invariant is:

1. outer pattern bindings are in scope for the whole guard and the whole branch body.
2. inner `MatchGuard` bindings are in scope for the inner guard predicate and the branch body.
3. inner `MatchGuard` bindings are not in scope for later branches or outside the enclosing branch.

Provide one guard-aware traversal/mapping surface in `Expr` and `TypedExpr`, then update downstream passes to use it instead of duplicating scope logic ad hoc.

### `rankn/Infer.scala` and `TypeValidator.scala`: typecheck guard patterns before the body

`checkBranch` / `inferBranch` need to split branch processing into three stages:

1. typecheck the outer branch pattern against the match scrutinee, yielding `outerBindings`.
2. typecheck the branch guard:
   - `BoolGuard(expr)` runs under `outerBindings` and must produce `Bool`.
   - `MatchGuard(arg, pattern, guardOpt, outerAnnotations)` runs under `outerBindings`, infers `arg` as a `Sigma`, instantiates it to a `Rho` the same way the main match scrutinee is instantiated before pattern checking, typechecks `pattern` against that instantiated scrutinee type, yields `guardBindings`, and typechecks `guardOpt` as `Bool` under `outerBindings ++ guardBindings`.
   - after the inner match succeeds, the reconstructed whole-guard condition, including `outerAnnotations`, must still validate as a `Bool`-position expression so annotation errors continue to surface at the branch guard.
3. typecheck the branch body under `outerBindings ++ guardBindings`.

This ordering is what gives the body the requested names without forcing early desugaring into nested matches.

### `TotalityCheck.scala`: guardedness stays conservative, except for the trivial case

Totality and unreachable-branch logic currently ask only whether `branch.guard.isEmpty`.

That should become a helper such as `guardIsEffectivelyUnguarded(branch.guard)`:

1. `None` => unguarded
2. `BoolGuard(_)` => guarded
3. `MatchGuard(...)` => guarded unless the inner pattern is definitely total and the inner guard is effectively absent

That preserves the current conservative policy for guards while still avoiding bogus needs-fallback behavior for a branch like:

```bosatsu
case p if value matches x:
  ...
```

which is semantically equivalent to an unguarded branch plus an extra local binding.

### `UnusedLetCheck.scala` and `ShadowedBindingTypeCheck.scala`: rely on the existing postponable path

These passes already understand nested lexical binders; they need to be taught that `MatchGuard` adds a second binder layer within the same branch.

Required scope rule:

1. `outerBindings` are checked against the outer environment.
2. `guardBindings` are checked against the environment extended by `outerBindings`.
3. the inner guard expression and the branch body are traversed under `outerBindings ++ guardBindings`.

No new lint category is required for this issue. The existing shadowed-binding and unused-value paths are the intended enforcement mechanisms for collisions between the outer branch pattern and the guard pattern.

### `TypedExprNormalization.scala` and related typed visitors

Any pass that currently assumes `branch.guard: Option[TypedExpr]` needs the new guard walker.

The important behavior to preserve is:

1. rewrites that affect branch expressions must also affect `BoolGuard` expressions and both parts of `MatchGuard` (`arg` and optional inner guard).
2. free-variable and shadow calculations for branch transforms must include both outer and inner guard scopes.
3. guard simplification may collapse a `MatchGuard` into the effectively-unguarded case, but must not lose the inner body bindings when that happens.

Traversal-heavy files that are likely to need updates include `TypedExprNormalization.scala`, `TypedExprLoopRecurLowering.scala`, `TypedExprRecursionCheck.scala`, `TypeValidator.scala`, `ProtoConverter.scala`, and `ShowEdn.scala`.

### `Matchless.scala`: no AST schema change, but guard lowering must learn inner guard-pattern binds

The Matchless row format can stay as:

1. outer pattern
2. optional boolean guard
3. rhs
4. accumulated binds

The new work is in lowering a `TypedExpr.MatchGuard` into that existing row structure.

Conceptually:

1. lower the inner guard scrutinee once, after the outer branch binds are available.
2. run the existing pattern-match compilation helper on the inner guard pattern to obtain:
   - any additional binders introduced by the guard pattern
   - any boolean condition needed for failure and inner-guard rejection
3. append those additional binders after the outer pattern bindings in the row-bind sequence before lowering the rhs, so backend name resolution preserves the same right-most-wins shadowing rule as the typed branch body.
4. if the guard is effectively unguarded, emit no additional boolean test.
5. otherwise keep the outer branch guarded exactly once.

This reuses existing candidate-guard and bind-accumulation machinery and avoids a new Matchless node, while still guaranteeing that the guard scrutinee is not evaluated twice.

### Serialization and tooling

If `TypedExpr.Branch.guard` changes shape, typed-IR serialization and tooling must change too:

1. `proto/src/main/protobuf/bosatsu/TypedAst.proto`
2. `core/src/main/scala/dev/bosatsu/ProtoConverter.scala`
3. `core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala`

The encoding needs an explicit guard kind. A decoder must not silently collapse an unknown match-style guard into a plain boolean guard or drop it entirely.

Prefer a protobuf `oneof` for the branch guard encoding in `TypedAst.proto`, so `BoolGuard` and `MatchGuard` remain structurally distinct for decoders and round-tripping tools.

## Behavioral Properties And Invariants

1. `case p_outer if e matches p_inner: body` is operationally equivalent to: try `p_outer`; if it matches, evaluate `e` once; if `p_inner` (and its optional inner guard) succeeds, evaluate `body` in the combined scope; otherwise continue to the next outer branch.
2. Only a whole-guard conditional `matches` opens the extra body scope. A nested `matches` inside `lhs && rhs`, `not(...)`, or another larger boolean form does not.
3. Inner guard-pattern bindings are in scope for the inner guard predicate and the same branch body only.
4. Inner guard-pattern bindings are not in scope for later branches, later branch guards, or outside the enclosing `match`.
5. When outer and inner patterns bind the same name, the branch body resolves that name to the inner binding.
6. Type-changing outer/inner collisions continue to surface through the postponable shadowed-binding lint; same-type collisions continue to compile, and the shadowed outer binding can still be reported as unused.
7. A nontrivial branch guard never counts as coverage for totality. Only the effectively-trivial `MatchGuard` case participates as unguarded.
8. The guard scrutinee is evaluated exactly once per attempted branch.
9. No new Matchless AST or backend-visible node is introduced.

## Testing Strategy

### Property-check style tests

Property tests should focus on lexical-scope bookkeeping, because that is where the new risk is highest and the structure is stable enough to generate mechanically.

1. `core/src/test/scala/dev/bosatsu/DeclarationTest.scala`
   - Build targeted declarations whose branch guards are top-level conditional `matches`.
   - Check that `freeVars` excludes inner guard binders outside the guarded branch body but not inside it.
   - Check that `Declaration.substitute` treats both outer and inner guard-pattern binders as masking scopes for the branch body.
   - Keep the broad `freeVars subset allNames` property running over declarations that include this shape.
2. If the existing declaration generator does not produce branch-guard conditional `matches` often enough for those properties to be meaningful, add a small targeted helper rather than depending on accidental coverage.

These are better as property tests than example tests because they defend the general scope laws rather than a single spelling.

### Case-based tests

Case-based tests are the right fit for syntax classification, exact lowering shape, diagnostics, and semantics.

1. `core/src/test/scala/dev/bosatsu/ParserTest.scala`
   - Parse and round-trip `case (x, y) if as_even(x) matches Some(even_x): ...`
   - Cover annotated and parenthesized forms that should still classify as top-level conditional `matches`.
   - Add a negative classification example where `matches` is nested inside a larger boolean guard and therefore should not open body scope.
2. `core/src/test/scala/dev/bosatsu/SourceConverterTest.scala`
   - Check that a top-level conditional `matches` branch guard becomes the richer guard kind, not a plain boolean guard.
   - Check that an ordinary boolean guard stays on the old path.
   - Check the effectively-trivial case (`matches x`, or `matches p if True` after canonicalization) is recognized as unguarded where intended.
   - Check that outer annotations on the guard are preserved for later type errors.
3. `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`
   - A name bound by the guard pattern is available in the same branch body.
   - That same name is still rejected in later branches or after the whole `match`.
   - A nested or non-top-level `matches` inside a larger boolean guard does not open body scope.
   - A collision between outer and inner binders surfaces the existing postponable diagnostics rather than a new hard parse or source-converter failure.
4. `core/src/test/scala/dev/bosatsu/ShadowedBindingTypeCheckTest.scala`
   - Same-type outer and inner collisions pass.
   - Different-type outer and inner collisions produce the existing shadowed-binding lint.
   - A guard-pattern binder used only in the inner guard does not require a duplicate body-side binding.
5. `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`
   - True path and false path behavior against an equivalent explicit `match`.
   - Multi-branch fallthrough when the guard `matches` fails.
   - Inner-guard success and failure.
   - Single-branch refutable and irrefutable cases.
6. `core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala`
   - Nontrivial `MatchGuard` branches still do not count toward totality.
   - The effectively-trivial `MatchGuard` case participates like an unguarded branch.
   - The implementation does not create new synthetic unreachable-branch diagnostics.
7. `core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala` and `core/src/test/scala/dev/bosatsu/tool/ShowEdnRoundTripTest.scala`
   - Round-trip the new typed guard variant without collapsing it to the old boolean-only encoding.

## Acceptance Criteria

1. Bosatsu accepts `case ... if expr matches pattern:` with the guard-pattern binders available in the same branch body.
2. Guard-pattern binders are also available in the optional inner guard of that `matches`.
3. Those binders are not available in later branches, later branch guards, or after the enclosing `match`.
4. Only a whole-guard conditional `matches` opens the extra body scope; nested `matches` inside a larger boolean guard do not.
5. The outer branch still behaves as guarded for totality and reachability unless the inner `matches` is effectively trivial.
6. Effectively-trivial guard matches participate like unguarded branches for coverage.
7. The guard scrutinee is evaluated once per attempted branch.
8. Type-changing outer and inner binder collisions surface through the existing postponable shadowed-binding diagnostics; same-type collisions still compile and can still report unused shadowed binders.
9. No Matchless AST or backend schema change is required.
10. The typed AST, proto, and EDN/show representations can round-trip the new guard form.
11. Property tests cover the source-level scope laws, and case-based tests cover parsing, lowering, diagnostics, evaluation, and totality.
12. The language guide documents that top-level conditional `matches` in branch guards bind into the same branch body only.

## Risks And Rollout Notes

1. Risk: many typed visitors currently assume `Option[Expr]` guards and may drift apart.
   Mitigation: add one shared guard traversal API in `Expr` and `TypedExpr` and update every branch-walker callsite to use it.
2. Risk: the compiler accidentally treats every nested `matches` inside a boolean guard as body-scoping.
   Mitigation: reuse `ConditionalMatch.unapply` as the only classifier and add explicit negative tests for nested boolean forms.
3. Risk: totality or reachability changes in the trivial inner-match case.
   Mitigation: centralize `guardIsEffectivelyUnguarded` and use it in source conversion, typed totality, and Matchless lowering.
4. Risk: serialization and tooling silently lose the new guard kind.
   Mitigation: encode guard kind explicitly in proto and EDN and add round-trip coverage before landing implementation.
5. Risk: the existing postponable diagnostics for outer and inner binder collisions are too indirect.
   Mitigation: start with the existing shadowed-binding and unused-binding paths, but leave room to add a dedicated focused diagnostic later if the tests show confusing source anchoring.

Rollout notes:

1. Merging this design doc is the `planned` milestone for this lane; implementation may continue on the same child issue afterward.
2. Prefer one implementation PR that lands the guard ADT plumbing, SourceConverter changes, typed-lowering updates, serialization updates, tests, and docs together.
3. Start verification with focused suites: `DeclarationTest`, `ParserTest`, `SourceConverterTest`, `ShadowedBindingTypeCheckTest`, `ErrorMessageTest`, `EvaluationTest`, `TypedTotalityTest`, `ProtoConverterTest`, and `ShowEdnRoundTripTest`.
4. No tree-sitter or syntax-highlighting rollout is expected, because the syntax already parses today and the change is semantic.
