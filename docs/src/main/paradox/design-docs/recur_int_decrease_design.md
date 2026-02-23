# `recur ... by int_decrease` Design

Status: proposed  
Date: 2026-02-21

## Goal
Allow practical total loops over `Int` (for example `int_loop`) to be written in Bosatsu source, with a minimal language change and a conservative termination proof strategy.

## Non-goals
1. General automatic termination proving.
2. User-authored proof terms or dependent typing.
3. Replacing structural recursion checks (`recur` on ADT substructure).
4. Making all type checking depend on an SMT solver.

## Motivation
Today `int_loop` is trusted runtime code:
1. `/Users/oscar/code/bosatsu2/core/src/main/resources/bosatsu/predef.bosatsu:266` declares `external def int_loop(...)`.
2. `/Users/oscar/code/bosatsu2/core/src/main/scala/dev/bosatsu/Predef.scala:1264` implements the loop in Scala.
3. `/Users/oscar/code/bosatsu2/docs/src/main/paradox/language_guide.md:967` explicitly notes Bosatsu cannot prove this style today.

This is a high-value gap: users frequently want bounded integer loops, but must currently rely on trusted externals.

## Proposed Language Change
Extend `recur` header syntax with an optional strategy clause:

```bosatsu
recur <target>:
recur i by int_decrease:
```

For this proposal, `int_decrease` is only valid in the single-name form.
In v1 we only support:
1. accepted: `recur i by int_decrease:`
2. rejected: `recur (i) by int_decrease:`
3. rejected: `recur (i, j) by int_decrease:`
4. rejected: any tuple target with `by int_decrease`.

Example target usage:

```bosatsu
def int_loop[a](i: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur i by int_decrease:
    ...
```

If `by ...` is absent, behavior is unchanged (existing structural/lexicographic recursion checks).

## Semantics of `int_decrease`
For a recursive self-call in a `recur i by int_decrease` body:

```text
f(..., next_i, ...)
```

accepted iff the checker can prove, under the current path condition `PC`:
1. `PC => next_i >= 0`
2. `PC => next_i < i`

`i` is the current loop index (the recur target in the current call frame).

This yields a well-founded measure over `Nat` via `next_i`, so recursive call chains are finite.

## Writing `int_loop` in Bosatsu
With this strategy, `int_loop` can be implemented directly:

```bosatsu
def int_loop[a](intValue: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur intValue by int_decrease:
    case _ if intValue > 0:
      (next_i, next_state) = fn(intValue, state)
      if next_i > 0:
        if next_i < intValue:
          int_loop(next_i, next_state, fn)
        else:
          next_state
      else:
        next_state
    case _:
      state
```

The recursive call path includes `next_i > 0` and `next_i < intValue`, so obligations are provable.

## Comparison with Intrinsic `int_loop`
Question: does `recur ... by int_decrease` add power beyond a trusted intrinsic
`int_loop`?

Short answer:
1. For the chosen design (prove each recursive call decreases), `by int_decrease` is strictly more expressive than intrinsic `int_loop`.
2. A hypothetical restricted subset could be made equivalent to `int_loop`, but that is not the chosen direction for this proposal.

### Baseline: intrinsic `int_loop` model
Today `int_loop` has a trusted external implementation:

```bosatsu
external def int_loop(intValue: Int, state: a, fn: (Int, a) -> (Int, a)) -> a
```

This gives one loop combinator:
1. single control counter (`Int`),
2. one state value (`a`),
3. one step function per iteration,
4. operationally linear iteration (one step at a time).

### Where power is equivalent
If we intentionally constrain `by int_decrease` definitions to:
1. one recur target `i: Int`,
2. at most one recursive self-call per control path,
3. recursive call in tail position,
4. no nested recursive calls in recursive arguments,

then a mechanical encoding to `int_loop` exists by packing all evolving locals
into `state`.

Conversely, `int_loop` itself can be written with `recur i by int_decrease`, so
under this tail-loop subset both formulations are mostly syntax/ergonomics
differences.

### Where `by int_decrease` is more expressive
The current proof rule checks each recursive self-call independently under path
conditions. That allows patterns not naturally representable by one `int_loop`
without extra encodings (manual stacks/continuations), for example:
1. non-tail recursion,
2. more than one recursive call in a branch,
3. nested recursive calls used as arguments.

Example shape:

```bosatsu
def f(i: Int) -> Int:
  recur i by int_decrease:
    case _ if i <= 0:
      0
    case _:
      f(i.sub(1)).add(f(i.sub(2)))
```

Each call can satisfy decrease obligations, but this is not a simple linear
`int_loop` step function.

### Safety/trust trade-off
Comparing approaches:
1. Intrinsic-only:
   1. trust one implementation (`int_loop`) globally,
   2. users must encode loops through that API shape.
2. `by int_decrease`:
   1. trust the checker + solver integration,
   2. each use site is locally justified by proof obligations,
   3. removes need for a special trusted runtime primitive for integer loops.

We choose the broader expressiveness direction while keeping simple local proof
obligations (`0 <= next_i < i` at each recursive call).

## Where to Check: Declaration vs Typed Trees
### Question
Could we run this purely on `Declaration` AST and assume type checker will fail later if needed?

### Decision
Use a two-phase approach:
1. **Early declaration-phase validation** for shape only.
2. **Authoritative typed-phase proof** for `int_decrease` obligations.

### Why
Declaration-only SMT checks are possible but brittle:
1. no typed certainty that arithmetic/compare terms are `Int`,
2. less robust normalization for aliases and inferred types,
3. worse diagnostics when proof failure and type failure interact.

Typed-phase checks keep termination proofs aligned with the same resolved/typed program that will run.

## Retaining Strategy Information Across Phases
Minimal retention plan:
1. Extend `Declaration.Match` to carry an optional recursion strategy tag.
2. Parse `by int_decrease` into that tag.
3. Keep existing `Expr`/`TypedExpr` shapes unchanged.
4. Read strategy from expression tags (`Declaration`) during typed recursion checking.
5. Do not persist proof results/metadata in package output for this feature.

Why this is minimal:
1. `Expr.Match` and `TypedExpr.Match` already preserve source tags.
2. No new runtime IR node or codegen representation is required.
3. Existing recursion-kind behavior on lets remains intact.

## Compiler Pipeline Changes
Current (`inferBodyUnopt`):
1. Source conversion
2. `DefRecursionCheck` on statements
3. source `TotalityCheck`
4. type inference

Proposed:
1. Source conversion
2. `DefRecursionCheck` (existing + strategy shape checks)
3. source `TotalityCheck`
4. type inference
5. **new `TypedRecursionTerminationCheck` for `int_decrease`**
6. normalization

Hook point: after inference succeeds, before normalization in `/Users/oscar/code/bosatsu2/core/src/main/scala/dev/bosatsu/Package.scala`.

## Declaration-Phase Checks (Cheap)
For `recur ... by int_decrease`:
1. target must be a single variable bound to a function parameter,
2. no duplicate/tuple target in v1,
3. recursive self-call must still be syntactically identifiable (existing checks),
4. preserve all current recursion errors for non-strategy recursion.

No SMT is run in this phase.

## Typed-Phase Obligation Generation
For each recursive call in a `by int_decrease` region:
1. identify current target term `i`,
2. identify recursive argument term `next_i`,
3. collect typed path condition `PC` on that control-flow path,
4. build two obligations:
   1. `PC => next_i >= 0`
   2. `PC => next_i < i`

If either fails, emit recursion error at call site.

## Follow-up: Lexicographic `int_decrease` for Tuple Targets
The v1 design intentionally starts with one target (`recur i by int_decrease`).
As a follow-up, we should extend this to match existing tuple-recursion
semantics (`recur (x, y, ...)`) using lexicographic decrease.

### Intended syntax (follow-up)
```bosatsu
recur (x, y) by int_decrease:
recur (x, y, z) by int_decrease:
```

### Intended proof rule
For target tuple `(x1, x2, ..., xn)` and recursive arguments
`(x1_next, x2_next, ..., xn_next)`, require:
1. non-negativity side conditions for integer measure components:
   1. `PC => x1_next >= 0`
   2. ...
   3. `PC => xn_next >= 0`
2. strict lexicographic decrease:
   1. `PC => (x1_next < x1) OR`
   2. `      (x1_next = x1 AND x2_next < x2) OR`
   3. `      ... OR`
   4. `      (x1_next = x1 AND ... AND x(n-1)_next = x(n-1) AND xn_next < xn)`

This is the direct integer analogue of current `recur (x, y, ...)` ordering.

### SMT shape
The backend can emit one implication per recursive call:
1. `PC => LexLess(nextTuple, currentTuple)`
2. optional split into smaller obligations (prefix-equality + strict-decrease
   cases) for better diagnostics.

This remains in standard linear integer arithmetic and is close in complexity to
the unary v1 obligations.

### Implementation impact (follow-up)
Expected work is moderate:
1. parser/AST shape checks: allow tuple targets with `by int_decrease`,
2. typed extraction: map tuple target indices to recursive call arguments,
3. obligation generation: build `LexLess` formula over `Int` tuples,
4. diagnostics: report which lexicographic branch failed, with model output.

### Planning note
This extension should be tracked as the next strategy enhancement after v1 so
`by int_decrease` aligns with the language model of existing tuple `recur`.

## Worked Example: `TypedExpr` -> SMT-LIB
This section shows how much machinery is needed if we only support
`by int_decrease` and linear integer constraints.

### Source shape (`int_loop` style)
```bosatsu
def int_loop[a](intValue: Int, state: a, fn: (Int, a) -> (Int, a)) -> a:
  recur intValue by int_decrease:
    case _ if intValue > 0:
      (next_i, next_state) = fn(intValue, state)
      if next_i > 0:
        if next_i < intValue:
          int_loop(next_i, next_state, fn)
        else:
          next_state
      else:
        next_state
    case _:
      state
```

### Representative typed AST fragment (real constructors)
Below is a reduced Scala fragment using actual constructors from
`TypedExpr.scala` to show the recursive call node we inspect:

```scala
import cats.data.NonEmptyList
import dev.bosatsu.{Identifier, PackageName, TypedExpr}
import dev.bosatsu.rankn.Type

val tag = () // real code uses Region-bearing tags (Declaration)
val intT = Type.IntType
val stateTpe: Type = ???    // inferred state type `a`
val fnTpe: Type = ???       // inferred type of `fn`
val intLoopFnTpe: Type = ??? // inferred type of `int_loop`

val intLoopName = Identifier.Name("int_loop")
val intValueName = Identifier.Name("intValue")
val nextIName = Identifier.Name("next_i")
val nextStateName = Identifier.Name("next_state")
val fnName = Identifier.Name("fn")

val recCall: TypedExpr[Unit] =
  TypedExpr.App(
    fn = TypedExpr.Local(intLoopName, intLoopFnTpe, tag),
    args = NonEmptyList.of(
      TypedExpr.Local(nextIName, intT, tag),
      TypedExpr.Local(nextStateName, stateTpe, tag),
      TypedExpr.Local(fnName, fnTpe, tag)
    ),
    result = stateTpe,
    tag = tag
  )
```

At the recursive call site above, path-condition extraction from surrounding
typed guards/matches yields:
1. `intValue > 0`
2. `next_i > 0`
3. `next_i < intValue`

### Constraint IR we need (minimal)
For v1 we only need:
1. integer variables/constants,
2. `+`, `-`,
3. comparisons `<`, `<=`, `=`, `>=`, `>`,
4. conjunction of atoms (`PC`).

Then for each recursive call we check:
1. `PC => next_i >= 0`
2. `PC => next_i < intValue`

by SAT checks on `PC /\ not(goal)`.

### SMT-LIB query A (prove `next_i >= 0`)
```smt2
(set-logic QF_LIA)
(declare-fun intValue () Int)
(declare-fun next_i () Int)

; path condition at recursive call
(assert (> intValue 0))
(assert (> next_i 0))
(assert (< next_i intValue))

; negate goal
(assert (not (>= next_i 0)))

(check-sat)
; expected: unsat
```

### SMT-LIB query B (prove `next_i < intValue`)
```smt2
(set-logic QF_LIA)
(declare-fun intValue () Int)
(declare-fun next_i () Int)

(assert (> intValue 0))
(assert (> next_i 0))
(assert (< next_i intValue))

(assert (not (< next_i intValue)))

(check-sat)
; expected: unsat
```

### Failing example (missing strict-decrease guard)
Suppose extracted `PC` is only:
1. `intValue > 0`
2. `next_i > 0`

Check `PC => next_i < intValue`:

```smt2
(set-logic QF_LIA)
(declare-fun intValue () Int)
(declare-fun next_i () Int)

(assert (> intValue 0))
(assert (> next_i 0))

; negate goal
(assert (not (< next_i intValue))) ; equivalent to next_i >= intValue

(check-sat)
(get-model)
; expected: sat, e.g. intValue = 1, next_i = 1
```

This demonstrates that the required SMT integration for `int_decrease` is small:
we only need QF_LIA obligations generated from typed guards and recursive-call
arguments, not full general-purpose theorem proving.

### Non-tail divide-and-conquer example (`n/2` and `n-n/2`)
This is a useful non-tail recursion pattern that is beyond the shape of a
single linear `int_loop` step.

Goal conditions for a split:
1. `n1 >= 2`
2. `n2 = n1 / 2` (integer division)
3. `n3 = n1 - n2`
4. prove:
   1. `1 <= n2 < n1`
   2. `1 <= n3 < n1`

SMT-LIB check:

```smt2
(set-logic ALL)
(declare-const n1 Int)
(declare-const n2 Int)
(declare-const n3 Int)

(assert (>= n1 2))
(assert (= n2 (div n1 2)))
(assert (= n3 (- n1 n2)))

(assert (not (and (<= 1 n2) (< n2 n1)
                  (<= 1 n3) (< n3 n1))))

(check-sat)
; expected: unsat
```

For `int_decrease`, we would typically run per-call obligations, one recursive
call at a time:
1. call on `n2`: prove `0 <= n2` and `n2 < n1`,
2. call on `n3`: prove `0 <= n3` and `n3 < n1`.

Bosatsu shape:

```bosatsu
def split_sum(n: Int) -> Int:
  recur n by int_decrease:
    case _ if n <= 1:
      n
    case _:
      n2 = n.div(2)
      n3 = n.sub(n2)
      split_sum(n2).add(split_sum(n3))
```

This demonstrates where SMT-backed `int_decrease` gives clear value:
1. non-tail recursion,
2. two recursive calls in one branch,
3. local arithmetic proofs for each recursive argument.

### Extraction sketch from typed trees
At each recursive self-call node:
1. recognize `TypedExpr.App(TypedExpr.Local(defName, _, _), args, _, _)` where
   `defName` is current recursive def,
2. select the recur-target argument position (`intValue` position in this
   example) to obtain `next_i`,
3. collect typed branch/guard facts on the path to the call (`PC`),
4. translate supported typed expressions to linear atoms,
5. run the two SMT-LIB checks above (`PC /\ not(goal)`).

Unsupported expressions in steps (3)-(4) become conservative failures:
"cannot prove decrease in supported arithmetic fragment."

## Path Condition Extraction (v1)
Support a conservative boolean/arithmetic fragment:

### Integer terms
1. integer literals,
2. loop variables/local int bindings,
3. addition/subtraction/unary negation,
4. optional multiplication by literal constant.

### Boolean constraints
1. comparisons derived from `cmp_Int(a, b)` patterns:
   1. `case LT` -> `a < b`
   2. `case EQ` -> `a == b`
   3. `case GT` -> `a > b`
2. boolean match on `True`/`False` from desugared `if`.
3. guard expressions that normalize into conjunctions of supported comparisons.

Unsupported terms/conditions in an `int_decrease` proof path cause a
conservative hard type-check error:
`cannot prove decrease in supported arithmetic fragment`.
This is never downgraded to a warning, because that would weaken totality.

## Z3 Backend Design (`scalawasiz3`)
Backend assumption for this proposal:
1. use Z3 via SMT-LIB queries,
2. invoke through `scalawasiz3` on both JVM and Scala.js,
3. run proofs in every compiler mode (JVM, Node.js, browser-hosted Scala.js),
   with no proof-check bypass mode.

Per obligation query shape:

```smt2
(set-logic QF_LIA)
(declare-fun i () Int)
(declare-fun next_i () Int)
; plus all free vars in PC/goal
(assert <PC>)
(assert (not <GOAL>))
(check-sat)
(get-model)
```

Interpretation:
1. `unsat` => obligation proven.
2. `sat` => produce counterexample model for diagnostics.
3. `unknown`/solver failure/timeout => hard type-check error (fail
   conservatively).

### `scalawasiz3` call sketch
```scala
import dev.bosatsu.scalawasiz3.{Z3Result, Z3Solver}

val solver = Z3Solver.default
val smt: String = makeObligationSmt2(pc, goal) // emits (check-sat) and (get-model)

solver.runSmt2(smt) match {
  case Z3Result.Success(stdout, stderr, _) =>
    parseFirstStatus(stdout) match {
      case "unsat"   => ObligationResult.Proved
      case "sat"     => ObligationResult.Refuted(parseModel(stdout))
      case "unknown" => ObligationResult.Unknown(stderr)
      case other     => ObligationResult.InvalidSolverOutput(other, stdout, stderr)
    }
  case Z3Result.Failure(msg, _, stdout, stderr, _) =>
    ObligationResult.SolverFailure(msg, stdout, stderr)
}
```

## Dependency Management
### Bosatsu constraints
Bosatsu compiler is Scala 3 (`/Users/oscar/code/bosatsu2/build.sbt:12`) and uses Scala.js/JVM cross projects.

### `scalawasiz3` fit
`scalawasiz3` is designed as a Scala 3 cross-platform library:
1. one shared API (`runSmt2: String => Z3Result`),
2. JVM backend runs embedded Z3 WASI via Chicory,
3. Scala.js backend runs embedded Z3 WASI via an internal MiniWASI host.

### Browser/runtime notes
From current `scalawasiz3` code path:
1. Scala.js backend uses `WebAssembly.Module`/`WebAssembly.Instance` directly,
2. no pthread worker model is used,
3. no `SharedArrayBuffer` requirement is currently introduced by this backend.

So COOP/COEP-style thread isolation is not a baseline requirement for this
design as currently implemented in `scalawasiz3`. If backend internals change
later (for example worker/thread-based execution), we should revisit this note.

### Cross-platform proof policy
Proof obligations are checked in all modes:
1. JVM compiler path checks all `int_decrease` obligations.
2. Scala.js compiler path checks all `int_decrease` obligations.
3. If solver execution is unavailable/fails in any mode, compilation fails.

## Error Reporting
New error family (typed recursion termination):
1. `IntDecreaseNotProvable(callRegion, obligation, maybeModel)`
2. `IntDecreaseUnsupportedExpr(callRegion, reason)`
3. `IntDecreaseTimeout(callRegion, timeoutMs)`

Diagnostic content:
1. obligation text (`next_i >= 0` or `next_i < i`),
2. simplified path condition used,
3. if `sat`, model values for involved symbols.
4. actionable hint text when possible (for example: rewrite with explicit `Nat`
   fuel or use a trusted loop combinator such as `int_loop` when appropriate).

## Soundness and Completeness
1. Soundness target: only accept recursive calls when obligations are proven.
2. Completeness is intentionally partial: unsupported arithmetic/boolean forms are rejected conservatively.
3. No change to behavior of existing structural recursion code.

## Performance
1. Obligation count: 2 per recursive call site.
2. Use query caching on normalized `(PC, goal)` pairs.
3. Use per-obligation timeout budget.
4. Reuse a solver instance/process for multiple obligations in one def when
   possible.

## Rollout Plan
1. Parser/AST support for `by int_decrease`.
2. Extend `DefRecursionCheck` with strategy shape checks only.
3. Implement typed obligation extractor.
4. Integrate Z3 obligation execution via `scalawasiz3` in compiler backends.
5. Ensure JVM and Scala.js compilation paths both run proof checks.
6. Add deterministic parsing of solver status/model output (`unsat`/`sat`/`unknown`).
7. Add `PackageError` plumbing and diagnostics.
8. Add `int_loop` as normal Bosatsu definition in predef, keep external behind temporary flag during migration.
9. Remove external once tests pass and perf is acceptable.
10. Follow-up: add tuple-target lexicographic `by int_decrease` to match
    `recur (x, y, ...)` semantics.

## Test Plan
1. Parser tests:
   1. parse/print `recur i by int_decrease:`.
   2. reject invalid strategy placement.
2. Positive recursion tests:
   1. direct `int_loop` in Bosatsu compiles.
   2. loops with `next_i = i - 2` under suitable guards compile.
3. Negative recursion tests:
   1. missing lower bound proof (`next_i < i` only) fails.
   2. non-decreasing call fails.
   3. unsupported nonlinear term in proof path fails conservatively.
4. Counterexample quality:
   1. failing proof reports model values.
5. Regression:
   1. existing structural/lexicographic recursion behavior unchanged.
6. Cross-platform policy:
   1. JVM and Scala.js both run proof obligations.
   2. solver unavailable/failure causes compile failure in both modes.

## Open Questions
1. For `unknown`, should the primary hint prefer `Nat` fuel or `int_loop` first?
2. Should we require `(set-option :produce-models true)` for all queries, or add
   it only when issuing `(get-model)`?
3. Do we normalize all obligations to one global logic (`QF_LIA`) or pick the
   minimal logic per obligation?

## References
1. `scalawasiz3` repository: <https://github.com/johnynek/scalawasiz3>
2. `scalawasiz3` README (architecture and platform notes): <https://github.com/johnynek/scalawasiz3/blob/main/README.md>
3. `scalawasiz3` Scala.js backend (`JsWasiZ3Solver`): <https://github.com/johnynek/scalawasiz3/blob/main/core/js/src/main/scala/dev/bosatsu/scalawasiz3/JsWasiZ3Solver.scala>
4. `scalawasiz3` JVM backend (`JvmWasiZ3Solver`): <https://github.com/johnynek/scalawasiz3/blob/main/core/jvm/src/main/scala/dev/bosatsu/scalawasiz3/JvmWasiZ3Solver.scala>
5. SMT-LIB standard: <https://smt-lib.org/language.shtml>
6. Z3 project: <https://github.com/Z3Prover/z3>
