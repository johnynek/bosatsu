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
recur <target> by int_decrease:
```

For this proposal, `int_decrease` is only valid when `<target>` is a single variable (not tuple).

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
1. Extend `Declaration.Match` to carry optional recursion strategy metadata.
2. Parse `by int_decrease` into that metadata.
3. Keep existing `Expr`/`TypedExpr` shapes unchanged.
4. Read strategy from expression tags (`Declaration`) during typed recursion checking.

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

Unsupported terms/conditions in an `int_decrease` proof path cause a conservative error: "cannot prove decrease in supported arithmetic fragment".

## Princess Backend Design
Two backend options are viable:

### Option A (recommended for Bosatsu now): CLI + SMT-LIB2
Reason: avoids Scala binary compatibility issues (see dependency section).

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
3. `unknown`/timeout => fail conservatively.

### Option B (future): in-process `SimpleAPI`
Princess `SimpleAPI` call sketch (from `ap.api.SimpleAPI`):

```scala
import ap.SimpleAPI
import ap.SimpleAPI.ProverStatus
import ap.parser.IExpression._

def proveImplication(
    pc: IFormula,
    goal: IFormula,
    timeoutMs: Long
): Either[Counterexample, Unit] =
  SimpleAPI.withProver { p =>
    import p._

    // add constants used by pc/goal if needed
    addConstantsRaw(SymbolCollector constantsSorted (pc &&& !goal))

    withTimeout(timeoutMs) {
      scope {
        !!(pc)
        !!(!goal)
        ??? match {
          case ProverStatus.Unsat => Right(())
          case ProverStatus.Sat | ProverStatus.Inconclusive =>
            Left(Counterexample.fromPartialModel(partialModel))
          case ProverStatus.Unknown =>
            Left(Counterexample.unknown)
          case ProverStatus.OutOfMemory =>
            Left(Counterexample.outOfMemory)
          case other =>
            Left(Counterexample.error(other.toString))
        }
      }
    }
  }
```

Implementation notes tied to Princess API:
1. `withTimeout` is supported by `SimpleAPI` (`SimpleAPI.scala:486`).
2. assertions via `!!`/`addAssertion` (`SimpleAPI.scala:1831`, `1837`).
3. satisfiability via `???`/`checkSat` (`SimpleAPI.scala:1930`, `1955`).
4. model extraction via `partialModel` (`SimpleAPI.scala:3111`) and `PartialModel.eval` (`PartialModel.scala:66`).
5. temporary scopes via `scope/push/pop` (`SimpleAPI.scala:3924`, `3965`, `4022`).

## Error Reporting
New error family (typed recursion termination):
1. `IntDecreaseNotProvable(callRegion, obligation, maybeModel)`
2. `IntDecreaseUnsupportedExpr(callRegion, reason)`
3. `IntDecreaseTimeout(callRegion, timeoutMs)`

Diagnostic content:
1. obligation text (`next_i >= 0` or `next_i < i`),
2. simplified path condition used,
3. if `sat`, model values for involved symbols.

## Dependency Management
### Bosatsu constraints
Bosatsu compiler is Scala 3 (`/Users/oscar/code/bosatsu2/build.sbt:12`) and uses Scala.js/JVM cross projects.

### Princess ecosystem facts
From Princess source and Maven Central (as of 2026-02-21):
1. Princess source repo build file currently configures Scala 2.11/2.12 (`https://github.com/uuverifiers/princess/blob/master/build.sbt`).
2. Maven Central publishes JVM artifacts for Scala 2.11/2.12/2.13 (`io.github.uuverifiers:princess_2.11/_2.12/_2.13`, latest `2025-06-25`).
3. No Scala 3 artifacts (`princess_3`) found.
4. No Scala.js artifacts (`princess_sjs1_*`) found.
5. Runtime deps include Scala library, parser modules, `scala-parser-combinators`, and `java-cup`.

### Practical consequence
1. **Direct in-process dependency from Bosatsu Scala 3 core is not currently feasible**.
2. **Scala.js integration is not currently feasible** (no published Scala.js artifacts).
3. Most realistic near-term integration is solver subprocess via SMT-LIB2 (CLI), JVM-only in compiler path.

### Recommendation
Phase 1: CLI backend (no Scala binary coupling).  
Phase 2: optionally add a small Scala 2.13 bridge module only if in-process API proves necessary.

## JVM + JS Support Model
Goal: keep Bosatsu usable on JVM, Node.js, and browser without making web tooling depend on JVM-only solver availability.

### Shared abstraction in `core`
Add a tiny solver abstraction in shared `core` (implemented in `.jvm` and `.js`):
1. input: normalized `int_decrease` obligations (`PC`, `next_i`, `i`),
2. output: `Proved`, `Refuted(model)`, `Unknown`, or `Unavailable`.

This lets typed recursion checking stay platform-agnostic while backends differ.

### Backend implementations
1. JVM:
   1. preferred backend: Princess (CLI or in-process where feasible),
   2. this is the authoritative proving path for CI/release.
2. JS:
   1. optional backend: `z3-solver` via Scala.js facades,
   2. must support both Node.js and browser,
   3. browser note: `z3-solver` requires threading support (`SharedArrayBuffer` + COOP/COEP); if unavailable, backend returns `Unavailable`.

### Authoritative-check policy
`int_decrease` should be treated as a **compile-time proof obligation**, not a runtime behavior.

Recommended policy:
1. authoritative proving runs on JVM builds/CI,
2. resulting artifacts (package/lib) are marked as proof-verified,
3. JS tools can consume those verified artifacts without rerunning SMT.

This matches the user model "same code checked on JVM, reused on JS."

### JS disable/skip mode
Add an explicit mode for JS compilers:
1. `require_proof` (strict): must prove locally; fail on `Unavailable`/`Unknown`.
2. `assume_jvm_verified` (recommended default for web UI + Node tooling): skip local SMT if input artifacts are marked proof-verified from JVM.
3. `unsafe_skip` (debug-only): skip all `int_decrease` checks even without verification metadata; emit clear warning.

Guardrails:
1. in `assume_jvm_verified`, raw source files without verification metadata must fail (or require `unsafe_skip`),
2. metadata must be invalidated by source hash/compiler-version mismatch,
3. if metadata is missing or stale, fall back to JVM check in CI pipeline.

### Why this preserves multi-platform support
1. JVM remains the trust anchor for theorem proving.
2. Browser/Node remain usable even when local SMT runtime is unavailable.
3. Bosatsu stays conceptually total: acceptance still depends on a proof, just not necessarily proved on every platform.

## Soundness and Completeness
1. Soundness target: only accept recursive calls when obligations are proven.
2. Completeness is intentionally partial: unsupported arithmetic/boolean forms are rejected conservatively.
3. No change to behavior of existing structural recursion code.

## Performance
1. Obligation count: 2 per recursive call site.
2. Use query caching on normalized `(PC, goal)` pairs.
3. Use per-obligation timeout budget.
4. Reuse solver process for multiple obligations in one def when using CLI backend.

## Rollout Plan
1. Parser/AST support for `by int_decrease`.
2. Extend `DefRecursionCheck` with strategy shape checks only.
3. Implement typed obligation extractor.
4. Implement JVM backend adapter (Princess first, start with CLI).
5. Define proof metadata format in compiled package/library output.
6. Implement JS solver adapter (`z3-solver`) and JS skip modes (`require_proof` / `assume_jvm_verified` / `unsafe_skip`).
7. Add `PackageError` plumbing and diagnostics.
8. Add `int_loop` as normal Bosatsu definition in predef, keep external behind temporary flag during migration.
9. Remove external once tests pass and perf is acceptable.

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
   1. JVM proof-verified artifact compiles in JS with `assume_jvm_verified`.
   2. JS strict mode fails when solver unavailable.
   3. JS unsafe mode emits warning and is disallowed in release CI.

## Open Questions
1. v1 syntax flexibility: allow only `recur i by int_decrease` or also `recur (i) by int_decrease`?
2. Should unsupported expressions be hard errors or a separate warning + failure code?
3. When CLI backend returns `unknown`, do we suggest a rewrite to `Nat` fuel automatically?
4. Where should proof metadata live for long-term stability: package binary only, interface file too, or both?
5. Should `assume_jvm_verified` be default for JS CLI/web UI, or require explicit opt-in?

## References
1. Princess repo: <https://github.com/uuverifiers/princess>
2. Princess `SimpleAPI` source: <https://github.com/uuverifiers/princess/blob/master/src/main/scala/ap/api/SimpleAPI.scala>
3. Princess `ITerm` operators: <https://github.com/uuverifiers/princess/blob/master/src/main/scala/ap/parser/ITerm.scala>
4. Maven Central search API: <https://search.maven.org/>
5. Maven query for Scala 3 artifact (`princess_3`): <https://search.maven.org/solrsearch/select?q=g:%22io.github.uuverifiers%22%20AND%20a:%22princess_3%22&rows=20&wt=json>
6. Maven query for Scala.js artifact (`princess_sjs1_2.13`): <https://search.maven.org/solrsearch/select?q=g:%22io.github.uuverifiers%22%20AND%20a:%22princess_sjs1_2.13%22&rows=20&wt=json>
7. `z3-solver` npm package/readme: <https://www.npmjs.com/package/z3-solver>
8. SharedArrayBuffer browser requirements (COOP/COEP): <https://web.dev/coop-coep/>
