---
issue: 2195
priority: 3
touch_paths:
  - docs/design/2195-guards-on-matches.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - docs/src/main/paradox/language_guide.md
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/Gen.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-18T02:12:46Z
---

# Guards on `matches`

_Issue: #2195 (https://github.com/johnynek/bosatsu/issues/2195)_

## Summary

Extend surface-level `matches` syntax to accept optional `if` guards, reusing existing guarded `match` lowering so pattern bindings are scoped only to the predicate and no new typed or backend IR is required.

## Context
`matches` is currently surface syntax for a boolean match test:

```bosatsu
x matches p
```

lowers to:

```bosatsu
match x:
  case p: True
  case _: False
```

That lowering happens in `SourceConverter`, while the downstream compiler already supports ordinary branch guards on `match` and `recur` through `Expr.Branch.guard` and `TypedExpr.Branch.guard`.

The current limitation is at the source layer:
1. `Declaration.Matches` has no guard slot.
2. `SourceConverter` rejects `matches` patterns that introduce bindings.
3. `SourceConverter` also reports syntactically-total `matches` patterns as always `True`.

Issue #2195 asks for the concise form:

```bosatsu
def exists(list, pred): list matches [*_, x, *_] if pred(x)
```

with `x` available in the predicate but not outside that predicate.

## Goals
1. Support `expr matches pattern if predicate`.
2. Allow pattern-bound names inside the predicate scope for `matches`.
3. Preserve existing `match` guard semantics: scrutinee evaluated once, pattern checked first, guard checked only after a successful pattern match.
4. Keep `matches` as source sugar over the existing guarded-match pipeline.
5. Preserve current diagnostics for invalid no-guard binding forms and always-true forms where applicable.

## Non-goals
1. No new `Expr`, `TypedExpr`, or Matchless IR node.
2. No change to ordinary `match`/`recur` branch guard semantics.
3. No backend or runtime change.
4. No new lint requiring that every bound name is actually referenced in the guard.
5. No attempt to preserve every unparenthesized ternary edge case if it conflicts with the new `matches ... if ...` syntax.

## Design Decision
Add an optional guard to `Declaration.Matches`, and lower guarded `matches` directly to the existing guarded `Expr.Match` form.

This keeps the feature where it belongs:
1. Parser and source AST gain the syntax.
2. `SourceConverter` owns scope and diagnostics.
3. Type inference, normalization, totality-on-typed-expressions, Matchless lowering, and codegen continue to work through the already-supported guarded-branch path.

The feature remains defined by this desugaring:

```bosatsu
x matches p if g
```

becomes:

```bosatsu
match x:
  case p if g: True
  case _: False
```

This desugaring is what gives the bound names their intended scope: they are available while checking `g`, but the overall expression still returns only `Bool`.

## Architecture And Implementation Plan
### 1. Extend the source AST and parser
In `core/src/main/scala/dev/bosatsu/Declaration.scala`:
1. Change `Declaration.Matches` to carry `guard: Option[NonBinding]`.
2. Update the pretty-printer to emit `arg matches pattern if guard` when present.
3. Parse an optional guard after the pattern using the same rules as branch guards:
   1. require at least one space before `if`
   2. once `if` is seen, commit to parsing a guard expression
   3. parse the guard in the surrounding expression mode so constructs like `if x matches p if g:` still stop at the outer `:`
4. Keep the guard attached to `matches`, not to the pattern.

Precedence decision:
`if` after `matches` belongs to the `matches` expression. If users need a ternary around the whole `matches`, they should parenthesize it:

```bosatsu
(x matches p) if cond else other
```

### 2. Update source-level scope bookkeeping
Also in `Declaration.scala`, update helpers that currently assume `matches` has no guard:
1. free-variable traversal
2. all-name collection
3. substitution / renaming logic
4. region replacement / structural copying

The guard must be traversed in a scope extended by `pattern.names`. This is required so source-level rewrites do not accidentally capture or leak names.

### 3. Lower guarded `matches` in `SourceConverter`
In `core/src/main/scala/dev/bosatsu/SourceConverter.scala`:
1. Convert the `matches` pattern normally when the form is valid.
2. Convert the guard under `withBound(_, pattern.names)` so the predicate sees pattern-bound names.
3. Reuse the existing branch-guard canonicalization rule: if the guard resolves to Predef `True`, erase the guard and treat the form as unguarded.
4. Lower to a two-branch `Expr.Match` returning `True`/`False`.

Proposed lowering shape:
1. first branch: `Branch(pattern, guardOpt, True)`
2. fallback branch: `Branch(_, None, False)`

No change is needed in `Expr.scala`, `TypedExpr.scala`, `rankn/Infer.scala`, `TypedExprNormalization.scala`, or `Matchless.scala`, because they already support guarded branches once the source form has been lowered.

### 4. Preserve the current scope boundary for surrounding `if`
`SourceConverter` already has a shortcut that rewrites:

```bosatsu
if x matches p:
  a
else:
  b
```

into a direct source `match` when `p.names.isEmpty`.

That shortcut must stay narrow.

If it were widened to guarded or binding `matches`, code like:

```bosatsu
if xs matches [*_, x, *_] if pred(x):
  x
else:
  0
```

could accidentally put `x` in scope in the `if` body, which is explicitly not desired.

Plan:
1. Keep the existing shortcut only for unguarded, binding-free `matches`.
2. Let guarded or binding forms lower as ordinary boolean expressions first.
3. Rely on the outer `if` lowering to consume that boolean value without exposing pattern bindings.

### 5. Adjust diagnostics
In `SourceConverter.scala`, change the current diagnostics policy:
1. Binding patterns without a guard should remain an error.
2. The message should be updated to explain the new rule: bindings in `matches` are only allowed when they are scoped to a guard.
3. The source-converter “always true” diagnostic should only fire when there is no effective guard after canonicalization.

This means:
1. `42 matches x` remains invalid.
2. `42 matches x if pred(x)` becomes valid.
3. `42 matches x if True` canonicalizes back to the unguarded form and still reports as always `True`.

In `core/src/main/scala/dev/bosatsu/TotalityCheck.scala` and `core/src/main/scala/dev/bosatsu/PackageError.scala`:
1. update `Declaration.Matches` pattern matches for the new constructor shape
2. keep typed always-true diagnostics restricted to unguarded `matches`
3. keep the existing wording for typed irrefutable `matches` errors

### 6. Documentation update
In `docs/src/main/paradox/language_guide.md`:
1. replace the current caveat that `matches` cannot introduce bindings
2. document the new guarded form
3. explicitly state that names bound by the pattern are scoped only inside the guard
4. note the parenthesization rule for ternary expressions if needed

## Testing Strategy
1. Parser tests in `core/src/test/scala/dev/bosatsu/ParserTest.scala`:
   1. positive round-trip for `xs matches [*_, x, *_] if pred(x)`
   2. positive round-trip for binding-free guarded `matches`
   3. require at least one space before `if`
   4. commit after `if` so `xs matches p if` is a parse error at the guard site
2. Evaluation tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala`:
   1. the `exists(list, pred)` example evaluates correctly
   2. a guarded `matches` false path returns `False`
   3. a guarded `matches` true path returns `True`
3. Error-message tests in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala`:
   1. no-guard binding form still errors with the revised message
   2. guard-only scope is enforced, for example using a bound name outside the guard still fails
4. Totality tests in `core/src/test/scala/dev/bosatsu/TypedTotalityTest.scala`:
   1. `x matches y if pred(y)` does not report “always true”
   2. unguarded irrefutable `matches` still report “always true”
   3. `if True` guard canonicalization preserves the existing always-true behavior
5. Test infrastructure updates in `core/src/test/scala/dev/bosatsu/Gen.scala` and related constructor-based tests so generators, shrinkers, and round-trip helpers understand the new `Declaration.Matches` shape.

## Acceptance Criteria
1. Bosatsu accepts `expr matches pattern if predicate` syntax.
2. Pattern-bound names are in scope while typechecking and lowering the predicate.
3. Pattern-bound names are not in scope outside the predicate or in the body of an enclosing `if` that consumes the `matches` result.
4. `matches` with bindings and no guard still reports a source-level error.
5. The parser enforces the same whitespace and commit behavior as branch guards.
6. Guarded `matches` lower through ordinary guarded `Expr.Match` branches with no new typed or Matchless IR.
7. Existing guarded-branch typing and lowering continue to work unchanged downstream.
8. Unguarded irrefutable `matches` still produce always-true diagnostics.
9. Guarded irrefutable `matches` do not produce always-true diagnostics unless the guard canonicalizes to Predef `True`.
10. Parser, evaluation, error-message, and totality tests cover the new form.
11. Public docs describe the new syntax and scope rule.

## Risks And Mitigations
1. Risk: parser ambiguity around `if` after `matches`, especially with ternary syntax.
Mitigation: define the precedence clearly, parse `if` as part of `matches`, and document the parenthesized ternary form.

2. Risk: scope leakage through the existing `if x matches p` rewrite in `SourceConverter`.
Mitigation: keep that rewrite restricted to unguarded, binding-free `matches`.

3. Risk: source-level rewrites or substitutions accidentally treat guard names as free.
Mitigation: update all `Declaration.Matches` traversals so the guard runs under `pattern.names` scope.

4. Risk: diagnostic drift between source-converter and typed totality phases.
Mitigation: gate source-converter always-true reporting on “no effective guard after canonicalization,” and leave typed always-true checks unchanged for unguarded cases.

5. Risk: AST constructor changes ripple through property tests and helper code.
Mitigation: update generator/shrinker utilities and constructor-based tests in the same PR.

## Rollout Notes
1. Land this as a single parser/source-conversion/doc/test PR; no runtime or backend coordination is needed.
2. Mention the parenthesization rule in release notes or docs because `matches ... if ...` claims the `if` token.
3. Existing unguarded `matches` code continues to work unchanged.
4. The only intentional behavior change is that guarded `matches` with bound names become valid while those names remain guard-local.
