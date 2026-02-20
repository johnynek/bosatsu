# Pattern Guards Design

Status: implemented  
Date: 2026-02-11

## Goal
Add match branch guards to Bosatsu:

```bosatsu
match value:
  case Foo(x, y) if fn(x, y):
    ...
```

while preserving Bosatsu's totality guarantees and keeping codegen predictable.

## Non-goals
1. Proving arbitrary guard predicates for exhaustiveness.
2. Introducing effectful guard semantics.
3. Replacing the existing pattern set algebra.

## Literature Review

### Semantics and syntax
1. OCaml supports branch guards (`when`) evaluated after the pattern matches, with top-down branch order.
Source: [OCaml Reference Manual 5.3 (patterns/matching)](https://caml.inria.fr/pub/distrib/ocaml-5.3/ocaml-5.3-refman.html)

2. Haskell supports guards and pattern guards; guards are checked after pattern matching in source order.
Source: [Haskell 2010 Report, Expressions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)

3. Scala 3 case clauses allow optional guards (`case Pattern [Guard] =>`), with a first-match style operational reading.
Source: [Scala 3 Language Specification, Pattern Matching](https://scala-lang.org/files/archive/spec/3.4/08-pattern-matching.html)

4. Rust supports `pattern if guard` and explicitly states guards run after the pattern matches.
Source: [Rust Reference, match expressions](https://doc.rust-lang.org/reference/expressions/match-expr.html)

### Exhaustiveness and guard complexity
1. Rust documents that match guards are not used for exhaustiveness checking.
Source: [The Rust Book, Match Guards](https://doc.rust-lang.org/book/ch19-03-pattern-syntax.html)

2. GHC's pattern-match checker models guards and can be expensive; docs describe an exponential blow-up driver and a model cap (`-fmax-pmcheck-models`).
Source: [GHC Users Guide, -fmax-pmcheck-models](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html#ghc-flag--fmax-pmcheck-models)

3. OCaml tooling still carries explicit guard-related warning categories (for example `All_clauses_guarded` in compiler warnings API), which signals practical caution around guarded coverage.
Source: [OCaml compiler-libs Warnings](https://ocaml.github.io/odoc/ocaml-base-compiler/compiler-libs.common/Warnings/index.html)

### Compilation literature
1. Maranget-style pattern matrices remain the baseline for decision-tree compilation.
2. GHC's guard-tree work is specifically about integrating guards into coverage and diagnostics.
Source: [Lower Your Guards (ICFP 2020)](https://www.microsoft.com/en-us/research/publication/lower-your-guards-a-compositional-pattern-match-coverage-checker/)

## Proposed Bosatsu Semantics

## Syntax
Extend match branch syntax to:

```text
case <pattern> [if <nonbinding-expr>] : <declaration>
```

## Dynamic semantics
For each branch, in order:

1. Try to match the pattern against the scrutinee.
2. If it fails, continue.
3. If it succeeds, evaluate the guard in the pattern-extended scope.
4. If guard is `True`, evaluate branch body.
5. If guard is `False`, continue to next branch.

No change to evaluation order of the scrutinee or branch ordering.

## Typing semantics
1. Guard expression is checked in the environment extended by pattern bindings.
2. Guard must typecheck as `Bool` (`Type.BoolType`).
3. Guard introduces no new bindings.
4. Branch result typing is unchanged.

## Core Design Decisions

### 1. Branch model becomes explicit
Current code uses `(Pattern, Expr)` pairs throughout parser, source IR, typed IR, normalization, proto, and codegen lowering. With guards, tuples become brittle.

Introduce explicit branch records in all relevant AST layers:

1. Parsed declaration branch: `pattern`, optional `guard`, `body`.
2. Untyped Expr branch: `pattern`, optional `guard`, `body`.
3. TypedExpr branch: `pattern`, optional typed `guard`, typed `body`.

This reduces accidental field-order bugs and makes normalization/lowering logic clearer.

### 2. Conservative totality policy
To preserve Bosatsu totality without theorem proving:

1. Only unguarded branches contribute to exhaustiveness coverage.
2. Guarded branches are checked for pattern validity and body totality, but do not reduce `missingBranches`.
3. Reachability uses only prior unguarded coverage for shadowing.

Consequence: a guarded catch-all (`case _ if cond`) does not make a match total; an unguarded fallback is still required.

This is intentionally conservative and aligned with practical behavior in Rust/GHC ecosystems.

### 3. Pattern set algebra stays unchanged
No changes to `TotalityCheck.patternSetOps` are required.  
Set algebra remains over `Pattern` only.

Changes are localized to branch selection in totality analysis:

1. Project branches to coverage patterns via `branch.guard.isEmpty` (or future `guardIsTriviallyTrue`).
2. Run existing `missingBranches`, `unreachableBranches`, `difference`, `intersection` unchanged.

## Compiler Pipeline Impact

### Parser and source AST
Files:

1. `core/src/main/scala/dev/bosatsu/Declaration.scala`
2. `core/src/main/scala/dev/bosatsu/SourceConverter.scala`

Changes:

1. Parse optional guard between pattern and `:`.
2. Pretty-printer emits `case <pat> if <guard>:` when present.
3. Source conversion converts guard under the same pattern-bound scope as branch body.
4. SourceConverter canonicalizes guards:
   1. If guard resolves to Predef `True`, rewrite branch as unguarded.
   2. If guard resolves to Predef `False`, keep it guarded (do not drop branch in SourceConverter).

### Type inference and typed AST
Files:

1. `core/src/main/scala/dev/bosatsu/Expr.scala`
2. `core/src/main/scala/dev/bosatsu/TypedExpr.scala`
3. `core/src/main/scala/dev/bosatsu/rankn/Infer.scala`

Changes:

1. `checkBranch`/`inferBranch` accept optional guard.
2. After `typeCheckPattern`, extend env with bindings, then typecheck guard as `Bool`.
3. Continue to branch body typing exactly as today.

### Totality and errors
Files:

1. `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`
2. `core/src/main/scala/dev/bosatsu/PackageError.scala`

Changes:

1. Exhaustiveness input patterns become "unguarded branch patterns".
2. Unreachable logic ignores prior guarded branches as covering evidence.
3. Error text adds explicit hint that guarded branches do not establish totality.
4. With SourceConverter canonicalization, `case p if True:` (where `True` resolves to Predef Bool constructor) participates as an unguarded branch.

### Recursion and lint-style analyses
Files:

1. `core/src/main/scala/dev/bosatsu/DefRecursionCheck.scala`
2. `core/src/main/scala/dev/bosatsu/UnusedLetCheck.scala`

Changes:

1. Guards must be traversed anywhere branch bodies are traversed today.
2. Pattern-bound names are in scope for guards.
3. In recursive contexts, a recursive call inside a guard is allowed iff that call would be allowed in that branch body.
4. `DefRecursionCheck` should validate guard expressions in the same branch state (`InRecurBranch`) used for the branch body, so recursive-call legality rules are identical.

### TypedExprNormalization (guard-aware)
Files:

1. `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
2. `core/src/main/scala/dev/bosatsu/Package.scala` (ordering note)

Guard-specific requirements:

1. Any substitution/inlining rule that can affect branch expressions must also apply to guard expressions.
2. When moving lets/lambdas across match branches, free-variable and shadowing checks must consider both guard and body.
3. Pattern-bound names still shadow outer names in guards exactly as they do in branch bodies.

Guard simplification rules:

1. Normalize/infer substitutions into the guard first.
2. If the normalized guard becomes `True`, remove the guard from that branch.
3. If the normalized guard becomes `False`, remove that branch as unreachable.
4. Run existing match simplifications after guard elimination (for example, simpler trailing wildcard rewrites).

Pattern-independent guard hoisting (optimization):

1. If a guard does not reference any names bound by its pattern, it can be hoisted into the scrutinee shape:
   1. `match x: case p if g: a; case p2: b`
   2. `=> let g0 = g in match (x, g0): case (p, True): a; case (p2, _): b`
2. This can unlock better matrix compilation by turning a guard check into ordinary pattern columns.
3. This rewrite should be heuristic-gated because it changes guard evaluation timing from conditional to eager.
4. Recommended gate:
   1. always allow when `g` normalizes to a cheap/simple expression,
   2. or when the same `g` is shared across multiple guarded branches (so hoisting avoids repeated evaluation).
5. Do not apply this rewrite when guard depends on pattern-bound names.

Why this matters:

1. It avoids regressions where a rewrite updates the branch body but leaves the guard stale.
2. It enables practical optimization from existing inlining/constant-folding work.
3. It gives us a path to compile some guards with pure matrix specialization rather than late guard checks.

Pipeline note:
`TypedExprNormalization` currently runs after type inference (`Package.inferBody`) and after source-level totality checking (`Package.inferBodyUnopt`).  
So guard elimination in normalization improves generated code quality and downstream optimization, but does not change current source totality diagnostics.

### Serialization
Files:

1. `proto/src/main/protobuf/bosatsu/TypedAst.proto`
2. `core/src/main/scala/dev/bosatsu/ProtoConverter.scala`

Changes:

1. Extend `Branch` with optional `guardExpr`.
2. Decode absent guard as `None`.
3. Encode guard when present.

Compatibility risk:
Old decoders could ignore unknown guard fields, so rollout should include a format/version gate to avoid silently dropping guard semantics.

## Matchless Design

Files:

1. `core/src/main/scala/dev/bosatsu/Matchless.scala`

### Data model
Extend matrix row:

1. `pats`
2. optional `guard`
3. `rhs`
4. `binds`

### Matrix compilation
Keep constructor/literal specialization unchanged.  
Guard logic is added only when a row becomes selected (all remaining pats are wildcards).

At row selection:

1. Build `rhsExpr = lets(binds, rhs)`.
2. If no guard: return `rhsExpr` (existing behavior).
3. If guarded:
   1. Build guard in bound scope: `gExpr = lets(binds, guard)`.
   2. Convert expression guard to `BoolExpr` via helper (`CheapExpr -> isTrueExpr`, otherwise `LetBool(..., isTrueExpr(...))`).
   3. On guard true: `rhsExpr`.
   4. On guard false: continue with remaining rows in original order.

This preserves left-to-right semantics and matrix sharing.

### Ordered compiler path
`matchExprOrderedCheap` must also incorporate guards, because it is used for non-orthogonal list/string patterns and small matches.

Effective branch test becomes:

1. pattern condition
2. then guard condition (short-circuited)

### `mustMatch` optimization impact
Current optimization can skip some final pattern checks.  
With guards:

1. Pattern-shape checks may still be skipped where valid.
2. Guard checks are never skipped unless proven trivially true.

## Trade-offs

## Performance
Pros:

1. Matrix shape-specialization logic stays mostly unchanged.
2. Set algebra complexity does not increase.
3. Guard simplification in `TypedExprNormalization` can remove runtime guard checks.

Costs:

1. Additional guard evaluation and branching in compiled Matchless.
2. Potential duplication of bound projection work between guard and rhs.
3. More normalization work because guard expressions participate in substitution and simplification.
4. Guard-hoisting can evaluate some guards earlier; without heuristics this can be a runtime regression.

## Error reporting
Pros:

1. Sound totality is preserved without predicate proving.
2. Diagnostics remain deterministic and simple.

Costs:

1. More conservative non-total errors ("add unguarded fallback") even for logically exhaustive guarded partitions.
2. Reachability may be less precise for guards that are always true/false but not syntactically obvious.
3. Special-casing Predef `True` in SourceConverter is slightly non-uniform, but it avoids surprising non-total errors for explicit `if True`.

## Implementation Plan
1. Add branch data structures and parser support.
2. Thread guards through `Declaration -> Expr -> TypedExpr`.
3. Typecheck guards as `Bool` in `Infer`.
4. Update totality projection and diagnostics.
5. Update Matchless matrix + ordered compilers.
6. Update recursion/lint traversals.
7. Add guard-aware `TypedExprNormalization` rules (substitution + `True`/`False` elimination + gated pattern-independent hoisting).
8. Update proto schema + converter.
9. Add targeted tests.

## Test Plan
1. Parser tests for `case p if g:`.
2. Scope tests: guard can use pattern names; later branches cannot.
3. Type tests: guard must be `Bool`.
4. SourceConverter canonicalization tests:
   1. Guard resolving to Predef `True` is rewritten to unguarded.
   2. Guard resolving to Predef `False` remains guarded.
   3. Locally-shadowed `True` is not canonicalized as Predef `True`.
5. Recursion-check tests:
   1. A recursive call legal in the branch body is also legal in that branch guard.
   2. A recursive call illegal in the branch body is also illegal in that branch guard.
6. Totality tests:
   1. Guard-only coverage is rejected.
   2. Unguarded fallback restores totality.
   3. Unreachable with prior unguarded patterns is still detected.
7. Matchless/evaluation tests:
   1. Guard false falls through correctly.
   2. Guard true selects branch.
   3. Works in matrix and ordered paths.
8. TypedExprNormalization tests:
   1. Let-substitution rewrites guard and body consistently.
   2. Guard folding removes `if True`.
   3. Guard folding removes `if False` branches.
   4. Shadowing-sensitive rewrites preserve semantics when guard references pattern names.
   5. Pattern-independent guard hoisting rewrites to tuple-scrutinee form when gate conditions hold.
   6. Pattern-dependent guards are not hoisted.
9. Proto round-trip tests for guarded branches.

## Open Questions
1. Should we add a dedicated error kind when all covering branches are guarded?
2. Do we want a later SMT-backed or rewrite-backed "guard simplifier" for better diagnostics?
