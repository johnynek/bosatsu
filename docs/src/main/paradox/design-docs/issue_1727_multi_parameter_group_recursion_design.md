# Issue 1727: Tail-Loop Rewrite for Multi-Parameter-Group Functions

Status: proposed  
Date: 2026-02-20  
Issue: <https://github.com/johnynek/bosatsu/issues/1727>

## Problem statement

Issue [#1727](https://github.com/johnynek/bosatsu/issues/1727) reports that recursive defs with multiple parameter groups often miss tail-rec loop lowering.

Example:

```bosatsu
def eq_List(fn: (a, a) -> Bool)(a: List[a], b: List[a]) -> Bool:
  recur (a, b):
    case ([ah, *at], [bh, *bt]) if fn(ah, bh): eq_List(fn)(at, bt)
    case ([], []): True
    case _: False
```

This should compile into a loop-shaped core, but currently the recursive call is seen as nested application (`eq_List(fn)` then `(at, bt)`) and does not match existing direct self-call rewrite logic.

## Current behavior summary

1. `SourceConverter.toLambdaExpr` lowers parameter groups into nested lambdas.
2. The recursive call in the innermost body is represented as a chained `App(App(Local(self), prefixArgs), recurArgs)`.
3. `SelfCallKind` and `rewriteTailCalls` only recognize direct `App(Local(self), args)` calls.
4. `rewriteTailRecToLoop` therefore misses this shape and no `Loop/Recur` is introduced for issue-1727 patterns.

## Where this rewrite should live

This rewrite should be implemented in `TypedExprNormalization`, specifically in the recursive `Let` normalization path in `normalizeLetOpt`.

Rationale:

1. At this stage we have typed `TypedExpr`, so call-shape checks are reliable and alpha-safe rewrites are already centralized here.
2. It composes naturally with existing normalizer passes (`rewriteTailRecToLoop` and `rewriteNonEscapingClosureBinding`).
3. It avoids backend-specific duplication in C/Python/JS codegen.
4. It keeps `SourceConverter` simple and syntax-oriented rather than optimization-oriented.

## Proposed rewrite

### High-level transformation

For recursive definitions with two or more lambda groups, rewrite innermost tail self-calls into a fresh local recursive helper whose parameter list includes outer-group arguments plus the innermost group arguments.

Conceptual source-level form:

```bosatsu
def f(g1)(x1, x2):
  ... f(g1)(y1, y2) ...
```

becomes:

```bosatsu
def f(g1)(x1, x2):
  def f_loop(g1_loop, x1_loop, x2_loop):
    ... f_loop(g1_loop, y1, y2) ...
  f_loop(g1, x1, x2)
```

This preserves partial-application semantics for `f(g1)` while exposing a single-group recursive function that existing loop lowering can optimize.

### Detection constraints (conservative)

Apply rewrite only when all of the following hold:

1. Recursive RHS is nested lambdas with at least two groups.
2. Every self-reference in the innermost group body is a full self-application in tail position.
3. Prefix groups in recursive calls are invariant passthroughs of the current outer lambda binders.
4. No escaping self-reference appears (for example, storing `self` in data or returning it).

If any condition fails, do not rewrite.

### Integration point

In `TypedExprNormalization.normalizeLetOpt`, recursive `Let` case:

1. normalize RHS (`ex1`) as today.
2. run new helper, for example `rewriteMultiGroupTailRec(arg, ex1, tag)`.
3. run existing `rewriteTailRecToLoop` on the rewritten result.
4. keep existing recursive/non-recursive recomputation and closure-rewrite logic unchanged.

This keeps the new rewrite as a shape-exposing pre-pass, while existing passes remain responsible for loop materialization and capture tuning.

## Sketch of helper API

```scala
private def rewriteMultiGroupTailRec[A](
  fnName: Bindable,
  expr: TypedExpr[A],
  tag: A
): Option[TypedExpr[A]]
```

Supporting helpers:

1. lambda-group collection/unwrapping helper.
2. self-call decomposition helper for chained `App`.
3. tail-position rewriter that substitutes matched self-calls with local helper calls.
4. fresh-name allocator using existing `Expr.nameIterator` plus `TypedExpr.allVarsSet`.

## Correctness notes

1. External function type and calling convention are unchanged.
2. The rewrite only changes internal recursion target and argument threading.
3. Guard/match ordering and evaluation order are preserved.
4. Rewrite is alpha-safe via existing unshadow/substitution utilities and fresh-name discipline.

## Tests to add

In `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`:

1. Issue-1727 regression: normalized `eq_List` contains loop form for inner recursion.
2. Partial-application regression: `eq_List(eq_Int)` behavior unchanged.
3. Negative case: transformed prefix argument (not passthrough) does not rewrite.
4. Negative case: escaping self-reference does not rewrite.
5. Idempotence: second normalization pass makes no further structural change.

## Non-goals

1. No syntax change for defs or recur.
2. No general uncurrying of all multi-group functions.
3. No backend-specific rewrite in codegen.
4. No changes to recursion validity rules in `DefRecursionCheck`.

