# Quantifier Evidence on `TypedExpr.Annotation`

## Status
This document records the current converged direction after exploring multiple alternatives (new `Instantiate` node, DB-index-only approaches, stricter quantifier ordering).

### Final direction in this document
1. Keep `TypedExpr.Annotation` as the existing coercion node.
2. Add an **optional quantifier-evidence field** to `Annotation`.
3. Use that evidence to persist solved quantifier substitutions/hidings when inference has already proven them.
4. Preserve current fallback behavior for true widening/coercion cases where no quantifier evidence exists.

This is an incremental change with lower churn than splitting the AST immediately.

## Problem statement
`Annotation` currently conflates two cases:

1. **Quantifier solving** (very common):
   - instantiate `forall` variables, and/or
   - hide concrete types behind `exists` variables.
2. **General widening** (rare):
   - subtype relation that is not just outer quantifier solving.

Recent scan numbers from `test_workspace`:

- command: `./bosatsuj lib show --name core_alpha --color none`
- total `(ann ...)` nodes: `2011`
- `2009 / 2011` (`99.90%`) had annotated term types with top-level quantification (`forall`/`exists`) and are quantifier-solving candidates
- `2 / 2011` (`0.10%`) were non-quantifier cases (both in `TypeConstraint` proof terms), i.e. widening-like outliers

So, in the observed corpus, quantifier-solving is overwhelmingly dominant (>99%).

### Core loss of information
Inference often proves quantifier solving is valid, then stores only:

- `Annotation(term, coerce)`

which erases:

- exact substitutions,
- which quantifiers were solved,
- partial-solve details,
- witness types used to hide behind existentials.

## Evidence from code
- `Annotation` currently only carries `(term, coerce)`:
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala:571`
- Inference already solves instantiate relations but emits `Annotation`:
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/rankn/Infer.scala:1412`
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/rankn/Infer.scala:1426`
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/rankn/Infer.scala:1436`
- Normalization later re-derives this by pattern matching and calling `instantiateTo`:
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala:1043`
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala:1668`

## Key difficulty: metavariables and hidden dependencies
### What we know
Metavariables can be assigned to `Var.Bound` in current inference:

- `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/rankn/Infer.scala:2899`

So unresolved metas can hide future dependencies on bound variables.

### Consequence
Any optimization that drops quantifier binders from `freeBoundTyVars` alone is unsafe while metas remain unresolved.

Pruning is only safe in meta-free situations (or after zonk).

## Requirements and invariants
1. **Semantic correctness**:
   - `Annotation` must remain a coercion proof; evidence must never change typing behavior.
2. **Alpha-robustness**:
   - Evidence must survive binder renaming/normalization.
3. **Partial solve support**:
   - Evidence can solve only subset of quantifiers.
4. **Meta safety**:
   - If unresolved metas are present, evidence is optional and may be absent.
5. **Replay-checkable**:
   - If evidence is present, we can validate/replay it against current types (up to `sameAs`).
6. **Backwards compatibility**:
   - Existing serialized trees without evidence must still decode.

## Proposed model
Add optional quantifier-evidence to `Annotation`.

### Scala sketch
```scala
object TypedExpr {
  final case class QuantifierEvidence(
    sourceAtSolve: Type, // should be sameAs(term.getType)
    targetAtSolve: Type, // should be sameAs(annotation.coerce)

    // substitutions for universally-quantified vars solved on the source side
    forallSolved: SortedMap[Type.Var.Bound, (Kind, Type)],

    // witness types hidden under existentials on the target side
    existsHidden: SortedMap[Type.Var.Bound, (Kind, Type)]
  )
}

case class Annotation[T](
  term: TypedExpr[T],
  coerce: Type,
  quantifierEvidence: Option[TypedExpr.QuantifierEvidence]
) extends TypedExpr[T]
```

Callers must pass `None` or `Some(evidence)` explicitly (no default argument).

### Why include `sourceAtSolve` / `targetAtSolve`
This addresses binder-renaming concerns:

- Evidence is interpreted in snapshot-space where it was proven.
- At use-sites we check `sameAs` with current `term.getType` / `coerce`.
- If snapshots do not align, ignore evidence and fallback.

This avoids coupling evidence to current binder names.

## Where Replay/validation runs
This is not intended as a standalone public API. It is a private helper used at the exact points where we currently try to re-solve from plain `Annotation` shape.

Primary call sites to wire:

1. Annotation normalization path:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala:1034`
   - currently calls `instantiateTo` at:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala:1043`
2. Lambda-resolution normalization path:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala:1655`
   - currently calls `instantiateTo` at:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala:1663`
3. Coercion fallback in `TypedExpr.coerceRho` for `Generic`:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala:1886`
4. Coercion fallback in `TypedExpr.coerceFn1` for `Generic`:
   - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala:2404`

At each site, the flow is: try evidence replay first, fallback to existing solver when evidence is absent/invalid.

## Replay/validation sketch
```scala
def normalizeAnnotation(ann: Annotation[A]): TypedExpr[A] = ann.quantifierEvidence match {
  case None => fallback(ann)
  case Some(ev) =>
    val srcNow = ann.term.getType
    val dstNow = ann.coerce

    if (!srcNow.sameAs(ev.sourceAtSolve) || !dstNow.sameAs(ev.targetAtSolve))
      fallback(ann)
    else {
      // alpha-align snapshots to current forms if needed
      val aligned = alignEvidenceToCurrent(ev, srcNow, dstNow)

      aligned match {
        case None => fallback(ann)
        case Some(aev) =>
          // reconstruct/check coercion path from quantifier evidence
          if (replayQuantifierEvidence(aev, srcNow).exists(_.sameAs(dstNow)))
            useReplayedResult(...)
          else
            fallback(ann)
      }
    }
}

// call site sketch in normalization:
// case Annotation(term, tpe, ev) => normalizeAnnotation(Annotation(term, tpe, ev))
```

## Expected behavior by case
1. **Instantiation-like coercion** (common):
   - `quantifierEvidence = Some(...)`
2. **General widening** (rare):
   - `quantifierEvidence = None`
3. **Meta-heavy/unstable stage**:
   - may choose `None` until after zonk

## Code changes needed
## 1) TypedExpr AST and helpers
- File: `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala`
- Changes:
  - add `QuantifierEvidence` model,
  - extend `Annotation` with optional evidence,
  - update `Eq[TypedExpr]`, traversals, and constructors/utils.

## 2) Inference emission
- File: `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/rankn/Infer.scala`
- Changes:
  - in `subsInstantiate`, when `Type.instantiate` succeeds, attach evidence instead of dropping it,
  - include both source/target snapshots and solved maps.

Pseudo:
```scala
Type.instantiate(...).map { case (frees, subs) =>
  val ev = QuantifierEvidence(
    sourceAtSolve = inferred,
    targetAtSolve = declared,
    forallSolved = ...,
    existsHidden = ...
  )
  dom.Annotation(te, declared, Some(ev))
}
```

## 3) Normalization/coercion consumption
- File: `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- Changes:
  - in annotation-normalization path, prefer evidence-driven replay,
  - fallback to existing `instantiateTo` behavior if evidence missing/invalid.

## 4) Domain interface updates
- File: `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/TypedExpr.scala`
- `TypedExpr.Domain.Annotation` currently takes `(term, coerce)`.
- Update to include optional evidence, or provide overload/helper.

## 5) Proto schema
- File: `/Users/oscar/code/bosatsu/proto/src/main/protobuf/bosatsu/TypedAst.proto`
- Current: `AnnotationExpr { int32 expr; int32 typeOf; }`
- Add evidence message and optional field.

Proto sketch:
```proto
message QuantAssignment {
  int32 varName = 1;
  int32 kind = 2;
  int32 typeOf = 3;
}

message QuantifierEvidence {
  int32 sourceType = 1;
  int32 targetType = 2;
  repeated QuantAssignment forallSolved = 3;
  repeated QuantAssignment existsHidden = 4;
}

message AnnotationExpr {
  int32 expr = 1;
  int32 typeOf = 2;
  QuantifierEvidence quantifierEvidence = 3; // optional presence
}
```

## 6) Proto converter
- File: `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/ProtoConverter.scala`
- Decode path around:
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/ProtoConverter.scala:527`
- Encode path around:
  - `/Users/oscar/code/bosatsu/core/src/main/scala/dev/bosatsu/ProtoConverter.scala:944`
- Changes:
  - map proto evidence <-> Scala `QuantifierEvidence`,
  - keep backward compatibility when field absent.

## 7) Test-only invariant gate in `TestUtils`
- File: `/Users/oscar/code/bosatsu/core/src/test/scala/dev/bosatsu/TestUtils.scala`
- Goal:
  - enforce post-inference `Annotation` invariants in tests without adding production runtime cost.

### What to change
1. Add a test helper that walks `TypedExpr` and validates every `Annotation`.
2. Wire that helper into existing validation entry points:
   - `assertValid(te)` (central hook),
   - codepaths that already call `assertValid` (`checkLast`, `checkPackageMap`),
   - `compileFile` to validate all compiled let bodies from typed packages.
3. Return simple stats (`total`, `quantifierSolving`, `widening`) so tests can assert distribution if desired.

### Properties we can enforce now
1. **No redundant annotation**:
   - fail if `annotation.term.getType.sameAs(annotation.coerce)`.
2. **Canonical annotation type**:
   - fail if `annotation.coerce != Type.normalize(annotation.coerce)`.
3. **Classification is explicit**:
   - mark annotation as quantifier-solving if either:
   - source is `ForAll(...)` that can instantiate to target, or
   - target is `Exists(...)` that can instantiate from source.
   - otherwise classify as widening.
4. **No escaping metas/skolems**:
   - already enforced by existing `assertValid`; keep as a prerequisite for annotation checks.

### Properties to enforce after evidence lands
1. If quantifier-solving is possible in a finalized/meta-free tree, require `quantifierEvidence = Some(...)`.
2. If `quantifierEvidence` is present:
   - `sourceAtSolve` is `sameAs` current source type,
   - `targetAtSolve` is `sameAs` current target type,
   - replay/validation succeeds and yields current `coerce` up to `sameAs`.
3. Allow `None` for true widening cases.

## Test plan
1. Unit tests for encode/decode round-trip of evidence.
2. Property tests:
   - replay evidence yields `coerce` (up to `sameAs`),
   - alpha-renaming of binders does not break replay,
   - evidence absent still follows legacy path.
3. Regression tests for rare widening cases where evidence is `None`.

## Migration plan
1. Add field and plumbing with explicit `Option` arguments at all call-sites.
2. Emit evidence in `subsInstantiate` only.
3. Consume evidence in normalization with fallback.
4. Expand to more inference sites if beneficial.
5. Optionally later split into dedicated `Instantiate` node if experience shows value.

## Open questions
1. Should evidence store binder names, indices, or both?
2. Should we require evidence to be fully replayable in all phases, or best-effort with fallback?
3. Should we disallow non-existential metas from resolving to `Var.Bound` via stronger typing (`TauNoBound`), and keep a separate explicit existential-realization API?
4. At what phase do we guarantee enough zonking to safely perform binder-pruning/canonicalization aggressively?

## Non-goal (for this change)
This proposal does not require immediate replacement of `Annotation` with a new AST node. It aims to preserve solved information first, with minimal structural disruption.
