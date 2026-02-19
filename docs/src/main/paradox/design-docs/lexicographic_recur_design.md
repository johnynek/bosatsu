# Lexicographic `recur` Tuple Design

Status: proposed  
Date: 2026-02-19
Tracking issue: [#1720](https://github.com/johnynek/bosatsu/issues/1720)

## Goal
Allow direct Ackermann-style recursion with tuple `recur` targets, while preserving totality:

```bosatsu
def ack(n: Nat, m: Nat) -> Nat:
  recur (n, m):
    case (Zero, _): Succ(m)
    case (Succ(n_prev), Zero): ack(n_prev, Succ(Zero))
    case (Succ(n_prev), Succ(m_prev)): ack(n_prev, ack(n, m_prev))
```

The acceptance rule is lexicographic decrease in the order written in `recur (...)`.

## Non-goals
1. General size-change termination analysis.
2. Arithmetic/semantic reasoning (for example, proving `x - 1 < x` over `Int`).
3. User-defined measures.

## Current Behavior (Relevant)
`DefRecursionCheck` currently:
1. Allows only one `recur` argument position (`getRecurIndex`).
2. Requires recursive calls to be structurally smaller only in that one position.
3. Does not enforce lexicographic decrease across multiple arguments.

This is why Ackermann currently needs higher-order encoding (`ack1` returning `Nat -> Nat`).

## Proposed Language Rule
`recur` target may be:
1. A single name (existing behavior).
2. A tuple of names, each name bound directly to a function parameter.

For `recur (r0, r1, ..., rk)` a recursive call with corresponding call arguments
`(a0, a1, ..., ak)` is legal iff:
1. There exists an index `i` such that `ai` is structurally smaller than `ri` in the current branch.
2. For every `j < i`, `aj` is exactly `rj` (syntactic equality to the original parameter name).
3. Arguments `j > i` are unrestricted for this specific call.

This is standard lexicographic descent on a well-founded structural order.

Why `case (Succ(n_prev), Zero): ack(n_prev, Succ(Zero))` is safe:
1. The first component decreases (`n_prev < n` structurally).
2. Lexicographic order permits any second component once an earlier component decreased.

## Checker Design

## Data model changes in `DefRecursionCheck`
Replace the single recursion position with a recursion target vector.

Current:
1. `InDefRecurred(..., group: Int, index: Int, ...)`
2. `InRecurBranch(..., allowedNames: Set[Bindable])`

Proposed:
1. `RecurTarget` = ordered non-empty vector of parameter positions:
   `NonEmptyList[(groupIndex, argIndex, paramName)]`
2. `InDefRecurred(..., target: RecurTarget, ...)`
3. `InRecurBranch(..., allowedPerComponent: NonEmptyList[Set[Bindable]])`

`allowedPerComponent(i)` means names proven structurally smaller than target component `i` in this branch.

## Target parsing (`getRecurTarget`)
Replace `getRecurIndex` with `getRecurTarget`:
1. If `recur` arg is `Var(v)`, resolve exactly as today, target length = 1.
2. If `recur` arg is `TupleCons(items)`, each item must be `Var(v)` and map to a distinct def parameter position.
3. Otherwise reject.

Conservative constraints:
1. No duplicates in tuple target.
2. Every tuple element must resolve to a top-level parameter binding (not a local, not an expression).

## Branch analysis
For each branch pattern:
1. Single target: keep existing behavior (`pat.substructures`).
2. Tuple target of arity `k`:
   1. If branch pattern is a tuple pattern with arity `k`, compute component-wise:
      `allowedPerComponent(i) = componentPattern(i).substructures.toSet`.
   2. Otherwise, set all components to empty sets (no provable decrease in this branch).

This is conservative and sound.

## Recursive call check
For a recursive call to function `f` in `InRecurBranch`:
1. Extract call argument at each target position (existing `argsOnDefName` + indexing by group/index).
2. For each component `i`, classify call arg as:
   1. `Equal` if arg is exactly `Var(paramName_i)`.
   2. `Decreases` if arg is `Var(name)` and `name ∈ allowedPerComponent(i)`.
   3. `Other` otherwise.
3. Accept iff some `i` satisfies:
   1. `class(i) == Decreases`, and
   2. all earlier classes are `Equal`.
4. Reject otherwise.

Important soundness detail:
After validating the outer recursive call, still recurse into all call arguments with `checkDecl` so nested recursive calls are checked too.

This is required for expressions like:
`ack(n_prev, ack(n, m_prev))`
where the inner recursive call must independently satisfy lexicographic descent.

## Error model
Existing errors stay, but tuple-lexicographic mode needs new diagnostics.

### New errors
1. `RecurTargetInvalid`
Message: `recur target must be a name or tuple of names bound to function parameters.`
Use when target contains non-vars, locals, or non-parameter names.

2. `RecurTargetDuplicate`
Message: `recur target contains duplicate parameter <name>; each target component must be distinct.`
Use when tuple target repeats a parameter.

3. `RecursionNotLexicographic`
Message:
`recursive call to <fn> is not lexicographically smaller on recur target (<t0, ...>).`
`Expected: some component decreases and all earlier components are unchanged.`
Use when call arguments fail the lexicographic predicate.

### Existing errors reused
1. `NotEnoughRecurArgs` still applies if call does not supply enough args to inspect target positions.
2. `InvalidRecursion`, `UnexpectedRecur`, `IllegalShadow`, `RecursiveDefNoRecur` unchanged.
3. `RecursionArgNotVar`/`RecursionNotSubstructural` become effectively superseded for tuple mode; they can remain for single-target compatibility or be folded into `RecursionNotLexicographic`.

## Examples

Accepted:
1. `ack(n_prev, Succ(Zero))` in branch `(Succ(n_prev), Zero)` (decrease in component 0).
2. `ack(n, m_prev)` in branch `(Succ(n_prev), Succ(m_prev))` (component 0 equal, component 1 decreases).
3. `ack(n_prev, ack(n, m_prev))` because:
   1. outer call decreases component 0;
   2. inner call decreases component 1 with component 0 equal.

Rejected:
1. `ack(n, Succ(m))` in branch `(Succ(n_prev), Succ(m_prev))` (no component proven to decrease).
2. Calls that decrease component 1 while component 0 is changed to a non-equal non-decreasing value.

## Test plan
Update `core/src/test/scala/dev/bosatsu/DefRecursionCheckTest.scala` with:
1. Positive: direct two-arg Ackermann form with `recur (n, m)`.
2. Positive: branch where earlier component decreases and later increases.
3. Positive: nested recursive call in later argument (`ack(n_prev, ack(n, m_prev))`).
4. Negative: classic non-terminating "alternate decreases" counterexample.
5. Negative: invalid `recur` tuple target elements (non-var/non-parameter/local).
6. Negative: duplicate tuple target names.
7. Negative: recursive call that is not lexicographically smaller.
8. Regression: existing single-argument recursion cases remain unchanged.

## Impact outside recursion checker
Required:
1. Tests in `DefRecursionCheckTest`.
2. User docs (`docs/src/main/paradox/recursion.md`, and possibly `language_guide.md`) to document tuple `recur` + lexicographic rule.

Not required:
1. Parser/AST shape changes: `recur` arg already accepts general expressions including tuples.
2. Type inference, totality checker, Matchless lowering, or codegen changes.
3. `PackageError` plumbing changes (new `RecursionError` cases flow through existing wrapper).

## Complexity and performance
1. Per recursive call check becomes `O(k)` for `k` target components (typically small).
2. Traversing all call arguments in recursive-call cases matches existing behavior for non-recursive calls and is linear in argument AST size.

## Open implementation choices
1. Keep `RecursionArgNotVar` + `RecursionNotSubstructural` for single-target path, or replace with unified `RecursionNotLexicographic`.
2. Whether to allow `recur (x)` as equivalent to `recur x` (recommended: yes, for consistency).
3. Diagnostic detail level: include per-component classification (`Equal/Decreases/Other`) in error text for debugging.
