# `recur` Type-Directed Decrease Design

Status: proposed  
Date: 2026-02-28

## Goal
Enable total recursion over `Int` without adding new `recur` syntax. Recursion strategy should be selected from the target type, not from a user-written `by ...` clause.

## Non-goals
1. Full automatic termination proving.
2. User-authored proof terms.
3. Implementing enum-based decrease rules in phase 1.
4. Changing runtime semantics of `recur`.

## Why this change
The previous draft required syntax like `recur i by int_decrease:`.
That is now the wrong abstraction boundary. The recursion checker should choose the strategy from type information:
1. Types can expose zero or more decrease strategies.
2. `recur` syntax remains unchanged.
3. Strategy application is an internal checker choice, not source-level syntax.

## Language Surface
No syntax change.

Valid source remains:

```bosatsu
def int_loop[a](i: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur i:
    ...
```

There is no `by int_decrease` form in this design.

## Type-Directed Strategy Model
At each recursive self-call, for each recur target, the checker:
1. Looks up the target type.
2. Finds applicable recursion strategies for that type.
3. Tries to discharge the obligations for at least one applicable strategy.
4. Reports an error if no strategy can be proven.

This keeps `recur` syntax stable while allowing strategy growth by type over time.

## Phase 1: `Int` Strategy
`Int` gets one strategy in phase 1:
1. measure is the next recursive `Int` argument,
2. recursive call must decrease strictly,
3. recursive argument must stay in the non-negative region.

For current value `i`, next recursive value `next_i`, and path condition `PC`, require:
1. `PC => next_i >= 0`
2. `PC => next_i < i`

This is the same well-founded argument as before, but now selected because the target type is `Int`, not because syntax requested `int_decrease`.

## Example
```bosatsu
def int_loop[a](i: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur i:
    case _ if i > 0:
      (next_i, next_state) = fn(i, state)
      if next_i > 0:
        if next_i < i:
          int_loop(next_i, next_state, fn)
        else:
          next_state
      else:
        next_state
    case _:
      state
```

On the recursive call path, guards provide facts to prove both `Int` obligations.

## Using Existing `dev.bosatsu.smt` Support
The repository already has useful SMT infrastructure in `core/src/main/scala/dev/bosatsu/smt`:
1. `SmtExpr` models integer/bool terms and comparisons (`Lt`, `Gte`, `And`, `Not`, etc.).
2. `SmtCommand`/`SmtScript` build scripts (`SetLogic`, `DeclareConst`, `Assert`, `CheckSat`, `GetModel`).
3. `SmtLibRender` renders SMT-LIB text.
4. `Z3Api` parses solver status/model output and returns structured results.

For an obligation `PC => goal`, the checker can emit:
1. declarations for free variables,
2. `(assert PC)`,
3. `(assert (not goal))`,
4. `(check-sat)` (and optional `(get-model)`).

Interpretation:
1. `unsat`: obligation proven,
2. `sat`: obligation failed (use model in diagnostics),
3. `unknown`/execution failure: fail conservatively.

## Compiler Placement
1. Keep existing declaration-phase recursion checks for structural shape.
2. Run typed recursion termination checks after inference, when argument types and call targets are resolved.
3. In typed checking, dispatch strategy by recur-target type (`Int` in phase 1).

No parser changes are required for this issue.

## Error Reporting
When an `Int` obligation fails, report:
1. recursive call location,
2. failed obligation (`next_i >= 0` or `next_i < i`),
3. simplified path condition used,
4. model values when solver returns `sat`.

## Future Direction: Enums (Not Phase 1)
We should keep moving toward one unified, type-directed notion of decrease.

A likely next step for enums:
1. `EnumOrdinal`: constructor declaration order defines base order.
2. Example: `enum Opt[a]: None, Some(v: a)` gives `None < Some(_)`.

Possible refinement after that:
1. For same constructor, compare payloads when payload types have applicable strategies.
2. Example direction: `Some(n) < Some(m)` when `n < m`.

These enum refinements are intentionally out of scope for phase 1, but this type-dispatch design leaves room for them without introducing new syntax.

## Test Plan
1. Parser regression: existing `recur` parsing unchanged; no `by int_decrease` syntax.
2. Positive typed recursion tests: accepted `Int` loops with provable decrease.
3. Negative typed recursion tests: reject non-decreasing or non-provable `Int` calls.
4. Structural recursion regression: existing ADT recursion behavior unchanged.
5. Solver failure policy: `unknown`/execution failure remains conservative error.
