---
issue: 1883
priority: 3
touch_paths:
  - docs/design/1883-type-level-integers-or-nats.md
  - build.sbt
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/Kind.scala
  - core/src/main/scala/dev/bosatsu/TypeParser.scala
  - core/src/main/scala/dev/bosatsu/TypeRef.scala
  - core/src/main/scala/dev/bosatsu/TypeRefConverter.scala
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Expr.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/rankn/ConstructorFn.scala
  - core/src/main/scala/dev/bosatsu/rankn/DefinedType.scala
  - core/src/main/scala/dev/bosatsu/rankn/TypeEnv.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementAst.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementParser.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementCheck.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementToSmt.scala
  - core/src/test/scala/dev/bosatsu/TypeRefTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/refinement/RefinementParserTest.scala
  - core/src/test/scala/dev/bosatsu/refinement/RefinementCheckTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-01T07:48:14Z
---

# Issue #1883 Design: Type-level integers or Nats with SMT-backed where clauses

_Issue: #1883 (https://github.com/johnynek/bosatsu/issues/1883)_

## Summary

Proposes a Nat-first refinement typing architecture (Int-ready) that adds constructor invariants plus function contracts (`requires` input obligations, `ensures` output guarantees), introduces index-aware kinding (`Nat ~> * -> *` style constructor shape), checks obligations in a new post-inference refinement pass, and discharges proofs through existing SMT/Z3 infrastructure with clear acceptance criteria, risks, and rollout sequencing.

---
issue: 1883
title: Type level integers or Nats
status: proposed
base_branch: main
touch_paths:
  - docs/design/1883-type-level-integers-or-nats.md
  - build.sbt
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/Kind.scala
  - core/src/main/scala/dev/bosatsu/TypeParser.scala
  - core/src/main/scala/dev/bosatsu/TypeRef.scala
  - core/src/main/scala/dev/bosatsu/TypeRefConverter.scala
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Expr.scala
  - core/src/main/scala/dev/bosatsu/TypedExpr.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/rankn/ConstructorFn.scala
  - core/src/main/scala/dev/bosatsu/rankn/DefinedType.scala
  - core/src/main/scala/dev/bosatsu/rankn/TypeEnv.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementAst.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementParser.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementCheck.scala
  - core/src/main/scala/dev/bosatsu/refinement/RefinementToSmt.scala
  - core/src/test/scala/dev/bosatsu/TypeRefTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/refinement/RefinementParserTest.scala
  - core/src/test/scala/dev/bosatsu/refinement/RefinementCheckTest.scala
depends_on: []
estimated_size: L
generated_at: 2026-03-01
---

# Issue #1883 Design: type-level integers or Nats with SMT-backed where clauses

Issue: #1883 (https://github.com/johnynek/bosatsu/issues/1883)  
Base branch: `main`  
Status: proposed

## Summary

Add first-class index sorts (`Nat` and `Int`), constructor invariants, and split function refinements (`requires` + `ensures`), then discharge generated proof obligations with Z3 during typechecking. The initial scope is a tractable subset: explicit contracts plus quantifier-free linear integer arithmetic.

## Problem statement

Bosatsu already has:

1. Rank-n inference over `rankn.Type`.
2. Constructor-local existentials (`ConstructorFn.exists`).
3. A standalone SMT/Z3 layer in `core/src/main/scala/dev/bosatsu/smt`.

Bosatsu does not currently have:

1. A way to declare indexed ADTs such as `Vec[n, a]` where `n` is an arithmetic index.
2. Constructor invariants plus split function contracts (`requires` / `ensures`).
3. A pass that turns typing facts into SMT obligations.

As a result, examples like length-safe vectors cannot be checked beyond simple syntactic type equality.

## Goals

1. Support indexed ADTs with explicit index sorts (`Nat`, `Int`) and index-aware constructor kinding.
2. Support `where` predicates on enum/struct constructors.
3. Support split function contracts:
   - `requires` for caller obligations on inputs.
   - `ensures` (or return-type refinement) for callee guarantees on outputs.
4. Make pattern matching add constructor predicates as branch assumptions.
5. Make constructor application require proving constructor predicates.
6. Reuse existing SMT/Z3 infrastructure and keep obligations in QF_LIA.
7. Keep existing non-refinement code behavior unchanged.

## Non-goals

1. Full Liquid Haskell style predicate inference.
2. Non-linear arithmetic solving (`x * y` where both are variables).
3. Arbitrary theorem proving from runtime booleans and higher-order code.
4. Automatic contract synthesis for unannotated functions.
5. Termination checking via refinements.

## Proposed user model

### Indexed type definitions

```bosatsu
enum Vec[n: Nat, a]:
  EVec where n == 0
  CVec[m: Nat](head: a, tail: Vec[m, a]) where n == m + 1
```

### Function contracts: `requires` vs `ensures`

```bosatsu
def filter(ary: Vec[n, a], fn: a -> Bool) ->
  exists r: Nat. Vec[r, a]
  ensures r <= n:
  ...
```

```bosatsu
def get_Vec[n: Nat, a](v: Vec[n, a], i: Int) ->
  a
  requires 0 <= i and i < n:
  ...
```

`requires` and `ensures` play different checking roles:

1. At each call site, the caller must prove the callee `requires`.
2. Inside the callee body, `requires` is added as an assumption.
3. At function definition, the callee body must prove `ensures`.
4. At call sites, the callee `ensures` is available as a post-call fact.

Phase 1 allows at most one `requires` and one `ensures` clause per signature;
multiple constraints in the same role are written with conjunction (`and`).

A return type refinement `... -> exists r: Nat. T where P(r, ...)` is treated as
`ensures` sugar in phase 1.

### Kinding indexed parameters and literal Nat arguments

Phase 1 makes indexed-argument shape explicit in kinding metadata. For an
indexed type like `Vec[n: Nat, a]`, the constructor shape is:

1. one index argument of sort `Nat`,
2. one type argument of kind `*`,
3. result kind `*`.

Shorthand: `Vec : Nat ~> * -> *`.

This prevents `Vec[Long, Long]`: the first slot expects an index term of sort
`Nat`, not a type expression. `Vec[3, Long]` is valid; integer literals are
accepted in index positions and lowered as literal terms in SMT obligations.

### Additional useful examples

1. `append(xs: Vec[n, a], ys: Vec[m, a]) -> exists r: Nat. Vec[r, a] where r == n + m`
2. `zip(xs: Vec[n, a], ys: Vec[n, b]) -> Vec[n, (a, b)]`
3. `take(xs: Vec[n, a], k: Int) -> exists r: Nat. Vec[r, a] where r <= n`
4. `transpose(m: Mat[rows, cols, a]) -> Mat[cols, rows, a]`

### Concrete implementations and how each typechecks

The review concern is valid: utility depends on whether realistic programs are
both writable and checkable. Below are concrete sketches in proposed syntax.

#### Shared stack-safe helper

`reverse_vec` is used below to convert accumulator-in-reverse-order loops into
the expected output order without sacrificing stack safety. In
`reverse_append`, self-calls are in tail position under `loop`, so it is
constant-stack.

```bosatsu
def reverse_append[a](left: Vec[n, a], right: Vec[m, a]) ->
  exists r: Nat. Vec[r, a] where r == n + m:
  loop left:
    case EVec:
      right
    case CVec(x, tail):
      reverse_append(tail, CVec(x, right))

def reverse_vec[a](xs: Vec[n, a]) -> Vec[n, a]:
  tmp = reverse_append(xs, EVec)
  # tmp has length n + 0; SMT discharges n + 0 == n
  tmp
```

#### `append`

```bosatsu
enum Vec[n: Nat, a]:
  EVec where n == 0
  CVec[m: Nat](head: a, tail: Vec[m, a]) where n == m + 1

def append[a](xs: Vec[n, a], ys: Vec[m, a]) ->
  exists r: Nat. Vec[r, a] where r == n + m:
  rev_xs = reverse_vec(xs)
  reverse_append(rev_xs, ys)
```

Typechecking outline:

1. `reverse_vec(xs)` is stack-safe (internally uses `loop`) and preserves length `n`.
2. `reverse_append(rev_xs, ys)` yields `exists r. Vec[r, a] where r == n + m`.
3. No recursive call in `append` itself is non-tail; stack safety comes from the helper loops.

#### `zip`

```bosatsu
def zip_vec[a, b](xs: Vec[n, a], ys: Vec[n, b]) -> Vec[n, (a, b)]:
  def zip_acc(todo_x: Vec[t, a], todo_y: Vec[t, b], acc_rev: Vec[r, (a, b)]) ->
    exists out: Nat. Vec[out, (a, b)] where out == t + r:
    loop (todo_x, todo_y):
      case (EVec, EVec):
        acc_rev
      case (CVec(xh, xt), CVec(yh, yt)):
        zip_acc(xt, yt, CVec((xh, yh), acc_rev))

  reverse_vec(zip_acc(xs, ys, EVec))
```

Typechecking outline:

1. `zip_acc` has an explicit invariant `out == t + r`.
2. Base case returns `acc_rev`, proving `out == 0 + r`.
3. Step case peels one element from each input and prepends one pair to `acc_rev`; SMT proves invariant preservation.
4. Initial call `zip_acc(xs, ys, EVec)` gives length `n`, and `reverse_vec` preserves that length while restoring order.
5. The implementation is stack-safe because recursion is under `loop` and self-calls are tail-position.

#### `take`

```bosatsu
def take[a](xs: Vec[n, a], k: Int) ->
  exists r: Nat. Vec[r, a] where r <= n:
  def take_acc(todo: Vec[t, a], remain: Int, acc_rev: Vec[r, a]) ->
    exists out: Nat. Vec[out, a] where out <= t + r:
    loop todo:
      case EVec:
        acc_rev
      case CVec(head, tail):
        if cmp_Int(remain, 0) matches GT:
          take_acc(tail, sub_Int(remain, 1), CVec(head, acc_rev))
        else:
          acc_rev

  reverse_vec(take_acc(xs, k, EVec))
```

Typechecking outline:

1. `take_acc` carries a relaxed invariant `out <= t + r` where `t` is remaining input and `r` is accumulated output.
2. Base case (`todo == EVec`) returns `acc_rev` and satisfies `out == r <= 0 + r`.
3. Step-continue case consumes one input and adds one output; SMT proves invariant preservation.
4. Step-stop case returns `acc_rev` immediately; inequality remains true because `r <= t + r`.
5. Initial call gives `out <= n`, and `reverse_vec` preserves length; result order is restored with stack safety.

#### `transpose`

For a fully total version over `rows`, we pass a shape witness for `cols`.

```bosatsu
def empty_cols[a](shape: Vec[cols, Unit]) -> Vec[cols, Vec[0, a]]:
  recur shape:
    case EVec:
      EVec
    case CVec(_, tail):
      CVec(EVec, empty_cols(tail))

def cons_each[a](row: Vec[cols, a], cols_acc: Vec[cols, Vec[r, a]]) ->
  Vec[cols, Vec[r + 1, a]]:
  recur (row, cols_acc):
    case (EVec, EVec):
      EVec
    case (CVec(x, xs), CVec(col, rest)):
      CVec(CVec(x, col), cons_each(xs, rest))

def transpose_with_shape[a](
  shape: Vec[cols, Unit],
  m: Vec[rows, Vec[cols, a]]
) -> Vec[cols, Vec[rows, a]]:
  recur m:
    case EVec:
      empty_cols(shape)
    case CVec(row, tail):
      ttail = transpose_with_shape(shape, tail)
      cons_each(row, ttail)
```

Typechecking outline:

1. `empty_cols` preserves the shape index while setting inner vector length to `0`.
2. `cons_each` zips same-length vectors (`cols`) and increments inner length from `r` to `r + 1`.
3. In `transpose_with_shape`, `EVec` branch corresponds to `rows == 0`.
4. `CVec(row, tail)` branch has `rows == rt + 1`; recursive call gives `Vec[cols, Vec[rt, a]]`, then `cons_each` yields `Vec[cols, Vec[rt + 1, a]]`, equal to target.

#### Stack-safe `map` with `loop`

Using the shared `reverse_vec` helper above, here is a constant-stack
implementation.

```bosatsu
def map_vec_loop[a, b](xs: Vec[n, a], fn: a -> b) -> Vec[n, b]:
  def map_acc(todo: Vec[t, a], acc_rev: Vec[r, b]) ->
    exists out: Nat. Vec[out, b] where out == t + r:
    loop todo:
      case EVec:
        acc_rev
      case CVec(x, tail):
        map_acc(tail, CVec(fn(x), acc_rev))

  acc = map_acc(xs, EVec)
  reverse_vec(acc)
```

Typechecking outline:

1. `map_acc` loop invariant is explicit in the return contract: output length is `t + r`.
2. Base branch (`todo == EVec`) returns `acc_rev`, proving `out == 0 + r == r`.
3. Step branch updates `(t, r)` to `(t - 1, r + 1)` in Nat terms; SMT proves invariant preservation.
4. Calling `map_acc(xs, EVec)` yields `out == n + 0 == n`.
5. `reverse_vec` preserves length, so final result has index `n`.
6. Because recursion is under `loop` and self-calls are tail-position, this variant is stack safe.

## Core design decisions

1. Nat-first semantics, Int-ready syntax:
   - Both `Nat` and `Int` sorts are parsed.
   - SMT lowering maps both to SMT `Int`; `Nat` introduces implicit `>= 0` assumptions.
2. Restrict arithmetic to Presburger-friendly forms:
   - Terms: vars, literals, `+`, `-`, unary negation, constant multiplication.
   - Predicates: `==`, `<=`, `<`, `>=`, `>`, conjunction.
   - No user quantifiers in `where`.
3. Keep rank-n type inference as the primary type pass:
   - Refinement proof is a separate typed pass after inference.
   - This avoids destabilizing existing unification behavior.
4. Use index-aware kinding for indexed constructors:
   - `Kind` metadata tracks index-argument positions and sorts.
   - Example: `Vec : Nat ~> * -> *`.
   - Type application checks index slots and type slots separately.

## Architecture

### 1) Refinement AST and parser layer

Add a dedicated refinement model under `dev.bosatsu.refinement`:

1. `IndexSort`: `Nat | Int`.
2. `Term`: variable, integer literal, arithmetic nodes.
3. `Predicate`: comparisons and conjunction.
4. `Contract`: optional `requires` and `ensures`.
5. Parser for constructor `where`, function contracts, and index-sort annotations.

Parser integration points:

1. `Statement.scala`:
   - constructor branches gain optional `where`.
   - type parameter annotations accept `Nat` and `Int`.
2. `TypeParser.scala` and `TypeRef.scala`:
   - type quantifier binders (`forall`/`exists`) accept index sorts.
   - parse `requires`/`ensures` on function signatures.
   - keep postfix return-type `where` as `ensures` sugar.
3. `Declaration.scala`:
   - add `where`, `requires`, and `ensures` as keywords to prevent identifier ambiguity.

### 2) Type and environment representation

Extend type-definition/value metadata, not runtime values:

1. `Kind` (and related helpers) represent index-argument shape (for example, `Nat ~> * -> *`).
2. `rankn.ConstructorFn` gets `wherePredicate: Option[Predicate]`.
3. `DefinedType`/`TypeEnv` expose index-parameter and index-aware kind metadata.
4. Annotated value signatures carry optional `requiresPredicate` and `ensuresPredicate`.
5. Interface/package serialization carries new metadata (`TypedAst.proto`, `ProtoConverter.scala`).

Existing `rankn.Type` continues to represent structural type shape. Refinement checking consults contract metadata and index-aware kinding metadata.

### 3) Refinement checking pass

Add `RefinementCheck` as a new typed pass in `Package.inferBodyUnopt`, after `Infer.typeCheckLets` and recursion checks.

High-level flow per typed let:

1. Traverse `TypedExpr`.
2. Maintain a refinement context:
   - in-scope index variables and sorts,
   - accumulated assumptions.
3. Generate obligations:
   - constructor application must satisfy constructor `where`,
   - function application must satisfy callee `requires`,
   - expression checked against annotated signature must satisfy callee `ensures` (or return-type `where` sugar).
4. At `match`:
   - each constructor pattern introduces its instantiated constructor `where` predicate as an assumption in that branch.
5. On function entry, add declared `requires` as assumptions for checking body obligations.
6. On function call return, add instantiated callee `ensures` as an assumption for subsequent checks.
7. Send each obligation to SMT as entailment: prove `assumptions => goal`.

Result handling:

1. `unsat` on `assumptions and not goal` means proof success.
2. `sat` produces a refinement type error (optionally include model).
3. `unknown` is a deterministic compile error in phase 1.

### 4) SMT lowering and solving

Implement `RefinementToSmt`:

1. Lower each index variable to SMT `Int`.
2. Emit `x >= 0` for each `Nat` variable.
3. Lower terms and predicates into existing `SmtExpr` nodes.
4. Build scripts using `QF_LIA`, `(assert assumptions)`, `(assert (not goal))`, `(check-sat)`, optional `(get-model)`.
5. Run through `Z3Api` and cache normalized obligations per package compile to avoid duplicate solver calls.

If needed, add tiny `SmtExpr` helpers for normalization, but keep existing SMT API surface.

### 5) Error reporting

Add `PackageError.RefinementError` with:

1. source region,
2. rendered assumptions and failed goal,
3. optional counterexample model,
4. concise hint tied to constructor or signature clause.

Message style should align with current `TypeErrorIn` and `TotalityCheckError` formatting.

### 6) Serialization and import behavior

Refinement metadata must round-trip through interface/package artifacts so imported indexed types preserve contracts. Proto changes must be backward-compatible:

1. old artifacts without refinement metadata still decode,
2. missing metadata means no refinement obligations for that artifact,
3. new artifacts carry constructor invariants and function `requires`/`ensures` data.

## Tractability and expected capability

Tractable in phase 1:

1. Structural recursive programs over indexed ADTs.
2. Branch-local linear arithmetic obligations.
3. Common size and bound contracts (`<=`, `==`, `+`, `-`).

Not tractable in phase 1:

1. Non-linear constraints.
2. Deep quantified properties requiring lemma discovery.
3. Contracts that depend on arbitrary runtime booleans unless explicitly reflected as refinement assumptions.
4. Singleton parameter typing like `i: m` where `m` is a Nat index binder.

## Relation to Liquid Haskell

This design is a strict subset of Liquid Haskell-style refinement typing:

1. Explicit user-written predicates only.
2. No qualifier inference or Horn-clause solving in phase 1.
3. Refinement checks are attached to declared contracts and constructor invariants, not inferred globally.

## Detailed implementation plan

1. Introduce refinement AST/parser (`RefinementAst.scala`, `RefinementParser.scala`).
2. Extend syntax and AST plumbing in `TypeParser.scala`, `TypeRef.scala`, `Statement.scala`, and `Declaration.scala` for constructor `where` plus function `requires`/`ensures`.
3. Extend kinding metadata in `Kind.scala` and definition metadata (`ConstructorFn.scala`, `DefinedType.scala`, `TypeEnv.scala`) for index-aware constructor shapes.
4. Extend source conversion to produce contract metadata (`SourceConverter.scala`).
5. Add proto schema fields and converter support (`TypedAst.proto`, `ProtoConverter.scala`).
6. Implement SMT lowering (`RefinementToSmt.scala`) and solver wrapper reuse.
7. Implement `RefinementCheck.scala` and integrate it into `Package.scala`.
8. Add `PackageError` rendering for refinement failures.
9. Add parser/source-converter/infer/refinement/error-message tests.
10. Keep refinement pass no-op when no constructor invariants or function contracts are present.

## Testing strategy

Unit tests:

1. Parse and pretty-print round-trip for index sorts and `where` clauses.
2. Parse and pretty-print round-trip for `requires` and `ensures`.
3. SMT lowering golden tests from predicate AST to SMT-LIB snippets.
4. Refinement checker tests for constructor obligations, function call preconditions, and postcondition propagation.

Integration tests:

1. Positive: `Vec` + `filter` example typechecks.
2. Positive: `append`/`zip` style constraints typecheck.
3. Negative: intentionally false constructor invariant, `requires`, or `ensures` fails with readable error.
4. Negative: invalid type application such as `Vec[Long, Long]` is rejected before refinement solving.
5. Unknown/timeout path surfaces deterministic error text.
6. Interface round-trip preserves refinement metadata.
7. Existing non-refinement suites remain green.

## Acceptance criteria

1. Parser accepts index-sort annotations (`Nat`, `Int`) in relevant type binders.
2. Parser accepts constructor-level `where` clauses.
3. Parser accepts function-level `requires` and `ensures` clauses.
4. Duplicate `requires` or duplicate `ensures` clauses are rejected; conjunction is written explicitly with `and`.
5. Return-type `where` refinements are accepted as `ensures` sugar.
6. Indexed constructors carry index-aware kind shape (for example, `Vec : Nat ~> * -> *`).
7. Invalid application of a type expression into an index slot (for example, `Vec[Long, Long]`) fails with a kind/index error.
8. Constructors carry `where` predicates in typed metadata.
9. Annotated function signatures carry `requires` and `ensures` metadata.
10. Imported interfaces preserve refinement and index-aware kind metadata.
11. A new refinement pass runs during typechecking of typed lets.
12. Pattern matching with constructors introduces constructor `where` assumptions.
13. Constructor application fails unless constructor `where` is provable.
14. Function calls fail unless callee `requires` is provable at the call site.
15. Function bodies are checked under declared `requires` assumptions and must prove declared `ensures`.
16. Obligations are lowered to SMT `QF_LIA` and solved through `Z3Api`.
17. `sat` and `unknown` solver outcomes are reported as user-facing compiler errors.
18. `Vec` + `filter` style program from the issue typechecks under this feature.
19. At least one negative vector-size example fails with a refinement-specific message.
20. No-refinement programs preserve existing behavior and test outcomes.
21. Existing `TypeErrorIn`/`TotalityCheck` diagnostics remain unaffected for unrelated failures.
22. Integer literals in index positions participate in refinement obligations (for
    example, proving bounds in `get_Vec(v, 3)`-style checks).

## Risks and mitigations

1. Solver performance regressions:
   - Mitigation: normalize and cache obligations; keep phase 1 logic QF_LIA only.
2. Solver availability/platform issues:
   - Mitigation: fail with explicit actionable error when refinement contracts are used but solver execution is unavailable.
3. Diagnostic complexity:
   - Mitigation: include minimal assumption set plus focused failed goal and source region.
4. Metadata compatibility risk:
   - Mitigation: additive proto fields and backward-compatible decoding.
5. Scope creep into full refinement inference:
   - Mitigation: keep explicit-contract-only boundary for phase 1.
6. Kinding migration complexity for indexed constructors:
   - Mitigation: keep backward-compatible defaults for existing `* -> ... -> *` constructors and only require index-aware shape when index binders are present.

## Rollout notes

1. Land syntax + metadata plumbing first, with refinement checking disabled by default path (no clauses means no solver use).
2. Land refinement checker and solver integration in one PR behind syntax gating.
3. Enable CI coverage with new refinement test suites.
4. Measure compile-time impact on representative packages before broad adoption.
5. Follow-up issues can add richer arithmetic, better model-based diagnostics, and optional inference features.
