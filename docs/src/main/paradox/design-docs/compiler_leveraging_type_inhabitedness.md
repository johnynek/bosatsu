# Compiler Leveraging Type Inhabitedness

Status: proposed  
Date: 2026-02-19
Issue: <https://github.com/johnynek/bosatsu/issues/1724>

## Goal
Use type inhabitedness in the compiler to:

1. identify uninhabited types with explicit, complete rules (in a supported fragment),
2. improve match totality by excluding impossible constructors,
3. let users omit uninhabited branch cases,
4. prune unreachable branches before codegen.

## Motivation
We already rely on totality and strong typing, but we do not currently leverage full type inhabitedness information during match checking.

Examples we want to classify statically:

1. `forall a. a` is uninhabited.
2. `struct Nothing(never: Nothing)` is uninhabited.
3. A struct with at least one uninhabited field is uninhabited.
4. An enum where every constructor is uninhabited is uninhabited.
5. A function type returning an uninhabited type is uninhabited when all its arguments are inhabited.

## Current State
`Package.inferBodyUnopt` runs `TotalityCheck` on untyped `Expr` before rank-n inference.  
This cannot use final instantiated scrutinee types (for example `Option[NothingLike]`) to remove impossible constructors during coverage.

## Scope and Completeness
Full inhabitation for unrestricted higher-rank/higher-kinded polymorphism is not a practical target here.  
This design defines a **complete decision procedure for a supported fragment** and a conservative fallback outside it.

Supported complete fragment:

1. kind-`Type` (value) types,
2. closed or locally-quantified types built from:
   1. ADTs from `TypeEnv`,
   2. function types,
   3. `forall` / `exists` over kind-`Type` variables.

Outside the fragment (for example higher-kinded quantifier-heavy cases), analysis returns `Unknown` and totality behavior remains conservative (no new pruning).

## Formal Rules
Let `I(t)` mean “type `t` is inhabited”, `U(t)` mean “type `t` is uninhabited”.

### Base and constructors
1. Base literal types (`Int`, `String`, `Char`, `Float64`, `Bool`, `Unit`) are inhabited.
2. For constructor `C : τ1, ..., τn -> T`:
   1. `I(C)` iff `I(τ1) && ... && I(τn)`.
3. For struct type `T` (single constructor):
   1. `U(T)` iff at least one field type is uninhabited.
4. For enum type `T`:
   1. `U(T)` iff every constructor is uninhabited.

### Functions
For `Fn(τ1, ..., τn) -> ρ`:

1. `U(Fn(...)->ρ)` iff `U(ρ)` and all arguments are inhabited.
2. Equivalent inhabited form:
   1. `I(Fn(...)->ρ)` iff `I(ρ)` or at least one argument is uninhabited.

This captures the requested rule:

1. returning uninhabited type + all args inhabited => uninhabited,
2. returning uninhabited type + some arg uninhabited => inhabited (vacuous function).

### Quantifiers
For kind-`Type` quantifiers:

1. `I(forall a. t)` iff `for all assignments of a, I(t)`.
2. `U(forall a. t)` iff `there exists an assignment of a with U(t)`.
3. `I(exists a. t)` iff `there exists an assignment of a with I(t)`.
4. `U(exists a. t)` iff `for all assignments of a, U(t)`.

This yields:

1. `U(forall a. a)` (pick an uninhabited assignment),
2. `I(forall a. a -> a)` (identity),
3. `I(forall a. List[a])` (empty list constructor),
4. `U(forall a. (a, a))` if tuple construction requires `I(a)`.

### Recursive ADTs
For mutually recursive type constants, define equations:

`I(T(args)) = OR over constructors c of T: (exists c-existentials. AND over c-fields fi: I(fi))`

Solve as least fixed point over the finite equation graph for the query.  
Least fixed point gives expected inductive behavior:

1. `struct Nothing(never: Nothing)` => uninhabited,
2. recursion only through impossible constructor paths stays uninhabited.

## Algorithm Design
New module: `core/src/main/scala/dev/bosatsu/Inhabitedness.scala`

Core API:

1. `verdict(t: Type, env: TypeEnv[Kind.Arg]): Inhabitedness.Verdict`
2. `constructorReachable(scrutinee: Type, cons: (PackageName, Constructor), env): Reachability`

Proposed result types:

1. `Inhabited`
2. `Uninhabited`
3. `Unknown` (outside supported complete fragment)

Implementation strategy:

1. Normalize type (`Type.normalize`), split quantifiers, and classify by shape.
2. Build/evaluate equations with memoization by `(Type, quantifier context)`.
3. Use finite quantifier elimination for kind-`Type` vars (boolean inhabited/uninhabited assignments).
4. Solve recursive SCCs with fixed-point iteration.
5. If unsupported kind-level features are encountered, return `Unknown`.

## Using Inhabitedness in Matches
Introduce typed totality checking that uses scrutinee type + constructor reachability.

### Coverage domain
For match scrutinee type `S`, define:

1. `reachableConstructors(S)` = constructors of `S` whose argument bundle is inhabited under current type instantiation.
2. Coverage requires only these constructors.
3. Constructors proven uninhabited are removed from missing-branch requirements.

### Effects
1. Users may omit branches for uninhabited constructors.
2. Branches that explicitly match uninhabited constructors are reported as unreachable.
3. Pattern-set operations still work, but domain expansion (`topFor`) filters out unreachable constructors.

## Pipeline Changes
Current:

1. `SourceConverter` -> `TotalityCheck` on `Expr` -> `Infer` -> `TypedExprNormalization`

Proposed:

1. `SourceConverter` -> `Infer` -> `TypedTotalityCheck` (inhabitedness-aware) -> `TypedExprNormalization`

Details:

1. Keep cheap source-level pattern-shape validation where helpful.
2. Move exhaustiveness/unreachable match checks to typed AST.
3. `Package.inferBodyUnopt` should run typed totality after successful inference.
4. Diagnostics stay at source regions via typed tags already attached to `TypedExpr`.

## Match Pruning and Codegen
Branch pruning should run after typed totality (or as part of normalization):

1. Remove branches that are impossible from inhabitedness facts.
2. Keep source order of remaining branches.
3. Preserve guard semantics (guarded branches still do not count toward totality unless unguarded fallback exists).

Codegen benefits:

1. smaller match matrices,
2. fewer runtime constructor checks,
3. fewer dead branches emitted in Matchless IR.

## Edge Cases
1. Polymorphic matches with free kind-`Type` vars:
   1. prune only when constructor is unreachable for all assignments.
2. Outside supported fragment:
   1. `Unknown` means no pruning and existing conservative totality behavior.
3. If a scrutinee type itself is uninhabited:
   1. every branch is unreachable,
   2. totality is vacuously satisfied.
   3. Optional future extension: allow empty `match` syntax explicitly.

## Test Plan
Add tests across `TotalityCheck` replacement + inference + normalization:

1. `forall a. a` classified uninhabited.
2. Recursive self-field struct classified uninhabited.
3. Struct with one uninhabited field classified uninhabited.
4. Enum with one inhabited and one uninhabited constructor:
   1. missing inhabited constructor still errors,
   2. missing uninhabited constructor does not error.
5. Explicit branch on uninhabited constructor is unreachable.
6. Function rule:
   1. `Unit -> Nothing` uninhabited,
   2. `Nothing -> Nothing` inhabited.
7. Guard interactions:
   1. guarded branch still excluded from totality coverage as today.
8. Regression tests for existing match behavior when no inhabitedness facts apply.

## Rollout Plan
1. Phase 1: add `Inhabitedness` module + unit tests.
2. Phase 2: add typed totality checker and wire into `Package.inferBodyUnopt`.
3. Phase 3: apply branch pruning in normalization/Matchless lowering.
4. Phase 4 (optional): syntax/AST support for empty matches on provably uninhabited scrutinees.

## Trade-offs
Pros:

1. Better exhaustiveness precision.
2. Cleaner user code (no mandatory impossible cases).
3. Better generated code via dead-branch pruning.

Costs:

1. More compiler complexity (new analysis + typed totality phase).
2. Some cases remain conservative (`Unknown`) by design.
3. Totality diagnostics now depend on successful type inference.
