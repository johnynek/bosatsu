# Issue 1628: Pattern Value Reuse in `TypedExprNormalization`

Status: proposed  
Date: 2026-02-13  
Issue: <https://github.com/johnynek/bosatsu/issues/1628>

## Goal
Avoid redundant allocations in `match` branches when the branch result reconstructs a value that is already available from the matched pattern.

Example source shape:

```bosatsu
def if_one_two_none_zero(o: Option[Int]):
  match o:
    case Some(i) if i matches 1: Some(2)
    case Some(i): Some(i)
    case None: Some(0)
```

Desired optimized shape:

```bosatsu
def if_one_two_none_zero(o: Option[Int]):
  match o:
    case Some(i) if i matches 1: Some(2)
    case Some(_) as some: some
    case None: Some(0)
```

## Current state in `TypedExprNormalization`
`normalizeLetOpt` already normalizes `Match` branches by:
1. Normalizing guards and branch bodies in pattern scope.
2. Folding guards that normalize to `True`/`False`.
3. Dropping `False` branches.
4. Filtering unused pattern binders via `pattern.filterVars`.
5. Running `maybeEvalMatch` for partial constant evaluation.

There is no rewrite that recognizes "branch rebuilds matched value, so return that matched value directly."

`shareImmutableValues` is not sufficient here because the allocation may only occur once in a branch body.

## Non-goals
1. Adding new source syntax.
2. Proving arbitrary expression equivalence.
3. Changing match branch ordering or guard semantics.
4. Handling every pattern form in v1.

## Proposed rule (phase 1): whole-branch value reuse
For each normalized branch `(pattern, guard, expr)` of `Match(arg, ...)`, attempt:

1. Detect that `expr` structurally rebuilds the full value matched by `pattern`.
2. If true, rewrite branch body to a `Local` bound to the matched value:
   1. Reuse an existing top-level pattern binder if present.
   2. Otherwise wrap the pattern in `Pattern.Named(fresh, pattern)` and use `Local(fresh, expr.getType, expr.tag)`.
3. Continue with existing binder filtering (`pattern.filterVars`), which will drop no-longer-needed inner binders.

This directly models the source rewrite `case p: rebuild(p)  => case p as whole: whole`.

## Structural matching predicate
Define helper `rebuilds(pattern, expr): Boolean` (ignoring `Generic`/`Annotation` wrappers):

1. `Pattern.Var(x)` matches `Local(x, _, _)`.
2. `Pattern.Named(_, p)` matches if `rebuilds(p, expr)` (and optionally also if `expr` is the named binder local).
3. `Pattern.Annotation(p, _)` delegates to `p`.
4. `Pattern.PositionalStruct((pack, cons), pats)` matches:
   1. `App(Global(pack, cons, _, _), args, _, _)`
   2. same arity
   3. all children pairwise satisfy `rebuilds`.
5. `Pattern.Literal(lit)` matches `Literal(lit, _, _)`.
6. Return `false` for `Union`, `ListPat`, `StrPat`, and `WildCard` in v1.

This is conservative and still captures the issue-1628 example.

## Fresh-name and capture safety
When introducing `Pattern.Named(fresh, pattern)`, choose `fresh` not present in:
1. `pattern.names`
2. free vars of guard and branch expr
3. all vars in match arg/branch expressions already in scope for this rewrite

Using `Expr.nameIterator` plus an immutable used-name set threaded through rewrites is sufficient.

This prevents accidental shadowing of outer locals referenced by guards or branch bodies.

## Integration point
Implement helper:

```scala
private def reuseMatchedPatternValue[A](
  matchArg: TypedExpr[A],
  branch: TypedExpr.Branch[A],
  used: Set[Identifier.Bindable]
): (TypedExpr.Branch[A], Int, Set[Identifier.Bindable])
```

Use it inside `case Match(arg, branches, tag)` in `normalizeLetOpt`, after guard/body normalization and before `freeT1`/`filterVars`.

Return `(updatedBranch, changedFlag, used1)` so branch rewrites can be folded left in source order while threading the updated immutable used-name set.

## Why semantics are preserved
1. Constructor/literal rebuilding is pure in Bosatsu; replacing rebuild with alias does not change effects.
2. Pattern match success/failure and guard checks are unchanged.
3. Type is preserved because rewritten `Local` uses the matched value type (`expr.getType` for the branch result).
4. Existing branch pruning/eval passes still run as before.

## Test plan
Add tests in `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`:

1. Issue-1628 branch shape:
   1. `Some(i) if i matches 1 -> Some(2)`
   2. `Some(i) -> Some(i)`
   3. verify second branch normalizes to local reuse (pattern gains `Named` if needed).
2. Guarded sibling does not block rewrite.
3. Negative cases:
   1. `Some(i + 1)` does not rewrite.
   2. constructor/arity mismatch does not rewrite.
4. Capture safety:
   1. outer scope already has candidate name; introduced binder remains fresh.
5. Idempotence:
   1. second normalization pass does not change output.

## Phase 2 design: nested subpattern reuse
Phase 1 only rewrites when the branch expression rebuilds the entire matched pattern.
Phase 2 extends this to reuse nested matched values.

Example:

```bosatsu
match value:
  case Foo(_, Bar(x)):
    Bar(x)
```

can become (conceptually):

```bosatsu
match value:
  case Foo(_, (Bar(x) as bx)):
    bx
```

### Core idea (nested subpattern reuse)
Given `(pattern, guard, expr)`, find a subpattern `p_sub` inside `pattern` such that `expr` structurally rebuilds `p_sub`.

Then:
1. If `p_sub` already binds a full-value name (`Var` or `Named` top binder), reuse that name.
2. Otherwise insert `Pattern.Named(fresh, p_sub)` at that subpattern location.
3. Rewrite branch body to `Local(chosenName, expr.getType, expr.tag)`.

This preserves semantics and removes reconstruction allocation.

### Candidate discovery
Define:

```scala
private case class SubpatternCandidate(
  path: List[Int], // constructor-field path through PositionalStruct
  existingTopName: Option[Bindable],
  score: Int // larger = better reuse candidate
)
```

Algorithm:
1. Traverse pattern and enumerate eligible subpatterns with deterministic order (left-to-right DFS).
2. For each candidate subpattern, test `rebuilds(candidatePattern, expr)`.
3. Keep matches and choose best by:
   1. highest `score` (prefer larger reused structure),
   2. then deepest path,
   3. then first in traversal order.

`score` can be pattern node count, so reusing `Some(Bar(x))` wins over reusing only `x`.

### Structural matcher for phase 2
Reuse phase-1 `rebuilds` rules, but apply them to any subpattern:
1. `Var(x)` <-> `Local(x, _, _)`
2. `Named(_, p)` <-> `rebuilds(p, expr)` (or direct name local)
3. `Annotation(p, _)` <-> `rebuilds(p, expr)`
4. `PositionalStruct((pack, cons), pats)` <-> constructor app with same `(pack, cons)` and arity, children pairwise matching
5. `Literal(l)` <-> `Literal(l, _, _)`

Still excluded in phase 2:
1. `Union`
2. `ListPat`
3. `StrPat`
4. `WildCard` as a reuse target

### Pattern rewrite mechanics
Implement:

```scala
private def bindAtPath(
  p: Pattern[(PackageName, Constructor), Type],
  path: List[Int],
  name: Bindable
): Pattern[(PackageName, Constructor), Type]
```

Rules:
1. `path == Nil`: wrap this node in `Pattern.Named(name, p)` unless already top-bound by `Var`/`Named`.
2. For `Annotation(inner, t)`: recurse into `inner` and rebuild annotation.
3. For `Named(n, inner)`: recurse into `inner` (preserve `n`).
4. For `PositionalStruct(name, params)`: recurse into indexed child from `path.head`, rebuild node.
5. Any mismatch is defensive fallback: no rewrite.

### Name hygiene and shadowing
Use one `used: Set[Bindable]` per match rewrite pass, seeded from:
1. all vars in `arg`, all branch guards, all branch bodies
2. all names bound by all branch patterns

Fresh binder generation must avoid:
1. current branch pattern names,
2. free vars of branch guard and expr,
3. global `used` set for the whole match.

This keeps rewrites capture-safe and stable across branches.

Represent fresh-name allocation as a pure helper:

```scala
private def freshName(
  used: Set[Bindable],
  avoid: Set[Bindable]
): (Bindable, Set[Bindable])
```

`freshName` picks the first candidate from `Expr.nameIterator` not present in `used union avoid`, and returns both the chosen name and `used + name`.

### Integration in normalization flow
Inside `normalizeLetOpt` `case Match(arg, branches, tag)`:
1. Keep current guard/body normalization and `True`/`False` folding.
2. Before `freeT1` and `filterVars`, run:
   1. phase-1 whole-pattern reuse attempt,
   2. if no change, phase-2 nested-subpattern reuse attempt.
3. Continue existing `filterVars`, trailing wildcard rewrite, and `maybeEvalMatch`.

Running before `filterVars` is important so newly introduced names are preserved when used by rewritten `expr`.

### Safety and semantics
1. Matching behavior is unchanged: only binder introduction plus body alias.
2. Guard behavior is unchanged: guard still evaluated in the same branch/pattern scope.
3. Purity assumption holds: all constructor/literal rebuilds are immutable and side-effect free.
4. Type preservation holds by replacing with `Local(name, expr.getType, expr.tag)`.

### Phase-2 tests
Add focused tests in `TypedExprTest`:
1. Nested constructor reuse:
   1. `case Foo(_, Bar(x)): Bar(x)` rewrites to alias of nested bound value.
2. Existing nested binder reused:
   1. `case Foo(_, (Bar(x) as bx)): Bar(x)` rewrites to `bx` (no extra `Named`).
3. Deterministic choice:
   1. when multiple subpatterns match, chosen target is stable by score/depth/order.
4. Guard interaction:
   1. guard referencing inner names still typechecks and preserves behavior.
5. Negative coverage:
   1. no rewrite for `Union`/`ListPat`/`StrPat`.
   2. no rewrite for non-structural expressions (`Bar(x + 1)`).

### Optional phase-2b
If useful later:
1. add `Union` support when the matched branch has already selected a concrete side.
2. add `ListPat`/`StrPat` structural reuse by extending matcher with list/string constructors.
3. add profitability guard for very large pattern scans.

## Phase 3 design: branch subexpression constructor-collision reuse
Phase 1 and phase 2 only rewrite when the branch body itself is the matched value (or a nested matched value).
This phase also rewrites matching constructor subexpressions inside larger branch expressions.

Primary motivating example:

```bosatsu
match o:
  case Some(x):
    f(Some(x))
```

rewritten as:

```bosatsu
match o:
  case Some(_) as s:
    f(s)
```

### Core idea (subexpression constructor-collision reuse)
For each branch:
1. Collect constructor-shaped subpatterns from the branch pattern.
2. Convert each to a reusable candidate with:
   1. constructor key `(package, constructor, arity)`,
   2. structural matcher (same conservative `rebuilds` logic),
   3. reusable binder (existing if available, else synthesize `Pattern.Named(fresh, ...)` at that path).
3. Traverse branch expression (and optionally guard) and replace matching subexpressions with candidate binders.

This targets obvious allocation duplication without requiring full expression equivalence.

### Candidate extraction
Define:

```scala
private case class ReuseCandidate(
  path: List[Int],
  key: (PackageName, Constructor, Int),
  binder: Bindable,
  score: Int
)
```

Extraction rules:
1. Include `PositionalStruct` subpatterns (possibly under `Named`/`Annotation`).
2. Skip `Union`, `ListPat`, `StrPat` for the first version.
3. Prefer candidates with larger `score` (node count) and deeper `path`.

This is the "find constructors in patterns" part of the heuristic.

### Replacing subexpressions
Implement a scope-aware expression traversal:
1. Track blocked term names (`Set[Bindable]`) due to lambda args, let binders, loop binders, and branch pattern binders.
2. At each node, try candidates in stable priority order.
3. A candidate matches only when:
   1. constructor key matches,
   2. full structural matcher succeeds,
   3. candidate binder is not blocked at this node.
4. On match, replace the node with `Local(candidate.binder, node.getType, node.tag)`.
5. Otherwise recurse.

This prevents capture/shadowing bugs when inner scopes redefine names.

### Pattern updates required for replacement
If a selected candidate has no binder yet:
1. introduce one with `bindAtPath` (from phase 2),
2. thread immutable `used: Set[Bindable]` through the rewrite.

If a candidate already has a binder (`as name` / `Var`), reuse it directly.

### Integration order
Inside `normalizeLetOpt` `Match` branch normalization:
1. existing guard/body normalization and guard folding,
2. phase-1 whole-pattern reuse,
3. phase-2 nested whole-branch reuse,
4. **phase-3 subexpression reuse inside branch expr/guard**,
5. existing `filterVars`, trailing wildcard canonicalization, `maybeEvalMatch`.

Running phase 3 before `filterVars` ensures newly introduced binders are retained if used by rewritten subexpressions.

### Scope and profitability guardrails
1. Only rewrite immutable constructor/literal expressions (no `Let`, `Loop`, `Recur`, `Match` as candidate roots).
2. Optionally cap rewrites per branch to avoid large compile-time growth.
3. Keep deterministic traversal and candidate ordering for stable output.

### Phase-3 tests
Add tests in `TypedExprTest`:
1. `case Some(x): f(Some(x))` rewrites to reuse branch value binder.
2. Multiple occurrences: `g(Some(x), Some(x))` rewrites both.
3. Shadowing safety:
   1. do not rewrite inside `\x -> Some(x)` using outer branch binder.
   2. do not rewrite through inner `let x = ...`.
4. Existing binder preference:
   1. `case Some(x) as s: f(Some(x))` should prefer `s` over creating a second binder.
5. Negative case:
   1. `f(Some(x + 1))` does not rewrite.
