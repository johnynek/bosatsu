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

Proposes a Nat-first refinement typing architecture (Int-ready) that adds `where` predicates on constructors and signatures, checks obligations in a new post-inference refinement pass, and discharges proofs through existing SMT/Z3 infrastructure with clear acceptance criteria, risks, and rollout sequencing.

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

Add first-class index sorts (`Nat` and `Int`) and `where` predicates to type definitions and value signatures, then discharge generated proof obligations with Z3 during typechecking. The initial scope is a tractable subset: explicit contracts plus quantifier-free linear integer arithmetic.

## Problem statement

Bosatsu already has:

1. Rank-n inference over `rankn.Type`.
2. Constructor-local existentials (`ConstructorFn.exists`).
3. A standalone SMT/Z3 layer in `core/src/main/scala/dev/bosatsu/smt`.

Bosatsu does not currently have:

1. A way to declare indexed ADTs such as `Vec[n, a]` where `n` is an arithmetic index.
2. `where` clauses on constructors or value signatures.
3. A pass that turns typing facts into SMT obligations.

As a result, examples like length-safe vectors cannot be checked beyond simple syntactic type equality.

## Goals

1. Support indexed ADTs with explicit index sorts (`Nat`, `Int`).
2. Support `where` predicates on enum/struct constructors.
3. Support `where` predicates on value type annotations (including `def` return signatures).
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

### Function contracts

```bosatsu
def filter(ary: Vec[n, a], fn: a -> Bool) ->
  exists r: Nat. Vec[r, a] where r <= n:
  ...
```

### Additional useful examples

1. `append(xs: Vec[n, a], ys: Vec[m, a]) -> exists r: Nat. Vec[r, a] where r == n + m`
2. `zip(xs: Vec[n, a], ys: Vec[n, b]) -> Vec[n, (a, b)]`
3. `take(xs: Vec[n, a], k: Int) -> exists r: Nat. Vec[r, a] where r <= n`
4. `transpose(m: Mat[rows, cols, a]) -> Mat[cols, rows, a]`

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

## Architecture

### 1) Refinement AST and parser layer

Add a dedicated refinement model under `dev.bosatsu.refinement`:

1. `IndexSort`: `Nat | Int`.
2. `Term`: variable, integer literal, arithmetic nodes.
3. `Predicate`: comparisons and conjunction.
4. Parser for `where` clauses and index-sort annotations.

Parser integration points:

1. `Statement.scala`:
   - constructor branches gain optional `where`.
   - type parameter annotations accept `Nat` and `Int`.
2. `TypeParser.scala` and `TypeRef.scala`:
   - type quantifier binders (`forall`/`exists`) accept index sorts.
   - postfix `where` on type annotations.
3. `Declaration.scala`:
   - add `where` as keyword to prevent identifier ambiguity.

### 2) Type and environment representation

Extend type-definition/value metadata, not runtime values:

1. `rankn.ConstructorFn` gets `wherePredicate: Option[Predicate]`.
2. `DefinedType`/`TypeEnv` expose index-parameter metadata.
3. Annotated value signatures carry optional `wherePredicate`.
4. Interface/package serialization carries new metadata (`TypedAst.proto`, `ProtoConverter.scala`).

Existing `rankn.Type` continues to represent structural type shape. Index reasoning is attached as refinement metadata plus index-sorted binders.

### 3) Refinement checking pass

Add `RefinementCheck` as a new typed pass in `Package.inferBodyUnopt`, after `Infer.typeCheckLets` and recursion checks.

High-level flow per typed let:

1. Traverse `TypedExpr`.
2. Maintain a refinement context:
   - in-scope index variables and sorts,
   - accumulated assumptions.
3. Generate obligations:
   - constructor application must satisfy constructor `where`,
   - expression checked against annotated signature must satisfy signature `where`.
4. At `match`:
   - each constructor pattern introduces its instantiated constructor `where` predicate as an assumption in that branch.
5. Send each obligation to SMT as entailment: prove `assumptions => goal`.

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
3. new artifacts carry constructor/value `where` data.

## Tractability and expected capability

Tractable in phase 1:

1. Structural recursive programs over indexed ADTs.
2. Branch-local linear arithmetic obligations.
3. Common size and bound contracts (`<=`, `==`, `+`, `-`).

Not tractable in phase 1:

1. Non-linear constraints.
2. Deep quantified properties requiring lemma discovery.
3. Contracts that depend on arbitrary runtime booleans unless explicitly reflected as refinement assumptions.

## Relation to Liquid Haskell

This design is a strict subset of Liquid Haskell-style refinement typing:

1. Explicit user-written predicates only.
2. No qualifier inference or Horn-clause solving in phase 1.
3. Refinement checks are attached to declared contracts and constructor invariants, not inferred globally.

## Detailed implementation plan

1. Introduce refinement AST/parser (`RefinementAst.scala`, `RefinementParser.scala`).
2. Extend syntax and AST plumbing in `TypeParser.scala`, `TypeRef.scala`, `Statement.scala`, and `Declaration.scala`.
3. Extend source conversion to produce refinement metadata (`SourceConverter.scala`).
4. Extend rank-n definition metadata (`ConstructorFn.scala`, `DefinedType.scala`, `TypeEnv.scala`).
5. Add proto schema fields and converter support (`TypedAst.proto`, `ProtoConverter.scala`).
6. Implement SMT lowering (`RefinementToSmt.scala`) and solver wrapper reuse.
7. Implement `RefinementCheck.scala` and integrate it into `Package.scala`.
8. Add `PackageError` rendering for refinement failures.
9. Add parser/source-converter/infer/refinement/error-message tests.
10. Keep refinement pass no-op when no `where` clauses or index sorts are present.

## Testing strategy

Unit tests:

1. Parse and pretty-print round-trip for index sorts and `where` clauses.
2. SMT lowering golden tests from predicate AST to SMT-LIB snippets.
3. Refinement checker tests for constructor obligations and match assumptions.

Integration tests:

1. Positive: `Vec` + `filter` example typechecks.
2. Positive: `append`/`zip` style constraints typecheck.
3. Negative: intentionally false constructor or function `where` fails with readable error.
4. Unknown/timeout path surfaces deterministic error text.
5. Interface round-trip preserves refinement metadata.
6. Existing non-refinement suites remain green.

## Acceptance criteria

1. Parser accepts index-sort annotations (`Nat`, `Int`) in relevant type binders.
2. Parser accepts constructor-level and signature-level `where` clauses.
3. Constructors carry `where` predicates in typed metadata.
4. Imported interfaces preserve refinement metadata.
5. A new refinement pass runs during typechecking of typed lets.
6. Pattern matching with constructors introduces constructor `where` assumptions.
7. Constructor application fails unless constructor `where` is provable.
8. Signature `where` clauses on annotated values are checked against bodies.
9. Obligations are lowered to SMT `QF_LIA` and solved through `Z3Api`.
10. `sat` and `unknown` solver outcomes are reported as user-facing compiler errors.
11. `Vec` + `filter` style program from the issue typechecks under this feature.
12. At least one negative vector-size example fails with a refinement-specific message.
13. No-refinement programs preserve existing behavior and test outcomes.
14. Existing `TypeErrorIn`/`TotalityCheck` diagnostics remain unaffected for unrelated failures.

## Risks and mitigations

1. Solver performance regressions:
   - Mitigation: normalize and cache obligations; keep phase 1 logic QF_LIA only.
2. Solver availability/platform issues:
   - Mitigation: fail with explicit actionable error when `where` is used but solver execution is unavailable.
3. Diagnostic complexity:
   - Mitigation: include minimal assumption set plus focused failed goal and source region.
4. Metadata compatibility risk:
   - Mitigation: additive proto fields and backward-compatible decoding.
5. Scope creep into full refinement inference:
   - Mitigation: keep explicit-contract-only boundary for phase 1.

## Rollout notes

1. Land syntax + metadata plumbing first, with refinement checking disabled by default path (no clauses means no solver use).
2. Land refinement checker and solver integration in one PR behind syntax gating.
3. Enable CI coverage with new refinement test suites.
4. Measure compile-time impact on representative packages before broad adoption.
5. Follow-up issues can add richer arithmetic, better model-based diagnostics, and optional inference features.
