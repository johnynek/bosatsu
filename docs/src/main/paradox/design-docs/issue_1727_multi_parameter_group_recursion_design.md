# Issue 1727: Tail-Loop Rewrite for Multi-Parameter-Group Functions

Status: proposed  
Date: 2026-02-20  
Issue: <https://github.com/johnynek/bosatsu/issues/1727>

## Problem statement

Issue [#1727](https://github.com/johnynek/bosatsu/issues/1727) reports that recursive defs with multiple parameter groups often miss tail-rec loop lowering.

Current source shape:

```bosatsu
def eq_List(fn: (a, a) -> Bool)(a: List[a], b: List[a]) -> Bool:
  recur (a, b):
    case ([ah, *at], [bh, *bt]) if fn(ah, bh): eq_List(fn)(at, bt)
    case ([], []): True
    case _: False
```

Target efficiency should match the explicit helper form:

```bosatsu
def eq_List(fn: (a, a) -> Bool)(a: List[a], b: List[a]) -> Bool:
  def loop(fn: (a, a) -> Bool, a: List[a], b: List[a]) -> Bool:
    recur (a, b):
      case ([ah, *at], [bh, *bt]) if fn(ah, bh): loop(fn, at, bt)
      case ([], []): True
      case _: False

  loop(fn, a, b)
```

## Current behavior summary

1. `SourceConverter.toLambdaExpr` lowers parameter groups into nested lambdas.
2. Recursive calls become chained application nodes like `App(App(Local(self), g1Args), g2Args)`.
3. Existing tail-call loop lowering is oriented around direct self-call shapes and may miss these chained grouped calls.

## Where this rewrite should live

This rewrite should live in `TypedExprNormalization` as a pre-pass immediately before tail-call-to-loop lowering.

1. It belongs in normalization (not parser/source conversion), because normalization already sees typed `TypedExpr` shapes and performs alpha-safe rewrites.
2. It should run right before whichever hook currently lowers self tail calls to `Loop`/`Recur`.
3. If loop-lowering internals moved (for example, after PR #1737), keep this as a normalization-stage pre-pass and wire it at the new call site.

## Proposed rewrite

### High-level transformation

For a recursive binding with grouped lambdas, create an internal helper that takes all group parameters in one argument list, rewrite recursive self-calls to that helper, then run existing loop lowering.

General shape:

```bosatsu
def f(g1)(g2)...(gn):
  body with recursive calls f(a1)(a2)...(an)
```

becomes (conceptually):

```bosatsu
def f(g1)(g2)...(gn):
  def loop(g1, g2, ..., gn):
    body with recursive calls loop(a1, a2, ..., an)
  loop(g1, g2, ..., gn)
```

This handles any number of parameter groups (`n >= 2`) in one pass.

### Explicit detection in `TypedExpr`

Detection is AST-driven and type-checked, not string-based.

1. Identify candidate recursive binding `Let(fnName, rhs, in, rec = Recursive, tag)`.
2. Collect lambda groups from `rhs` by unwrapping `Generic`/`Annotation`, walking nested `AnnotatedLambda`, and allowing non-recursive `Let` wrappers between groups when they do not shadow already-collected group names. Abort if fewer than two groups are recovered.
3. Traverse only the terminal group body and classify each occurrence of `fnName`: flatten chained `App` into `(head, argGroups)`; require `head` is `Local(fnName, _, _)` (after stripping wrappers); require group-count and per-group arity match; require full application to result type (no partial `f(g1)` values); require tail position eligibility.
4. If any recursive occurrence fails these checks, abort rewrite for this binding.

This is robust to shapes like:

```bosatsu
foo = fn -> (
  x = 2
  (x, y) -> (
    ... foo(fn)(x, y)
  )
)
```

because the detection allows a non-recursive `Let` wrapper between groups and still requires fully-applied grouped self-calls at the terminal body.

### Prefix arguments may change

We do not require prefix groups to be invariant. Recursive calls like `f(nextFn)(x1, x2)` are valid rewrite targets.

Reason:

1. The helper threads all group arguments as explicit loop state.
2. Recursive updates to any group become ordinary `Recur` state updates.
3. This is semantically direct and avoids imposing an unnecessary restriction.

### Why not recurse through an outer-function reference?

Keeping recursion as "call outer function from inside helper" is possible but weaker:

1. It reintroduces higher-order application in the recursive path.
2. It can block existing direct `Recur` lowering and may keep extra closure/application overhead.
3. Rewriting directly to helper self-calls aligns with the current loop-lowering pipeline.

## Integration point

In normalization, at the recursive-let rewrite site:

1. normalize RHS first.
2. run `rewriteMultiGroupTailRec`.
3. then run existing tail-call loop lowering.
4. then run existing closure/capture rewrites.

This preserves current pass ordering while exposing the loopable shape earlier.

## Sketch of helper API and invariants

```scala
private def rewriteMultiGroupTailRec[A](
  fnName: Bindable,
  expr: TypedExpr[A],
  tag: A
): Option[TypedExpr[A]]
```

Invariants:

1. `None` means "no safe rewrite".
2. `Some(expr1)` means type-preserving rewrite: `expr1.getType.sameAs(expr.getType)`.
3. `Some(expr1)` preserves semantics up to alpha-renaming and existing normalization equalities.

## Correctness notes

1. External function signature and partial-application behavior are unchanged.
2. Only internal recursive target shape changes.
3. Match/guard ordering and evaluation order are preserved.
4. Alpha-safety is maintained via existing fresh-name/unshadow discipline.

## Tests to add

In `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`:

1. Issue-1727 regression (`eq_List`) rewrites to helper-recursive shape and then loop form.
2. Partial-application behavior remains unchanged (`eq_List(eq_Int)` style).
3. Changed-prefix recursion rewrites correctly (`f(g1)(x)` recursing as `f(g1Next)(xNext)`).
4. `Let`-between-groups shape is supported.
5. Escaping self-reference still blocks rewrite.
6. Idempotence: second normalization pass is stable.

## Non-goals

1. No syntax changes.
2. No backend-specific rewrite.
3. No recursion-rule changes in `DefRecursionCheck`.
