# Code Plan #2331

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `implementation`
- Issue: `#2331` Allow binding if matches syntax in patterns.
- Source design doc: `docs/design/2331-allow-binding-if-matches-syntax-in-patterns.md`
- Pending steps: `0`
- Completed steps: `5`
- Total steps: `5`

## Summary

Implement scoped match-branch guards so `case p_outer if expr matches p_inner:` behaves like a branch-local conditional match: match the outer pattern first, evaluate the guard scrutinee once, check the inner guard pattern and optional inner guard, then run the branch body in the combined scope. Land that without a Matchless/backend node change, keep totality conservative except for effectively-trivial guards, preserve the existing shadowed/unused-binding diagnostics, and round-trip the new guard shape through typed IR tooling.

## Current State

`TypedExpr.MatchGuard` now lowers end to end through `Matchless.scala`, `ShowEdn.scala`, and the already-landed proto encoding without backend schema growth, with focused regression coverage across evaluation, lowering, and round-trip tooling. `docs/src/main/paradox/language_guide.md` now documents the shipped branch-guard `matches` semantics: only a whole-guard conditional `matches` extends scope into the optional inner guard and same branch body, nested boolean uses do not, and totality remains conservative except for trivially successful cases. Verification passed with `scripts/test_basic.sh`, `sbt "doc; paradox"`, and `git diff --check`.

## Problem

Issue #2331 required compiler behavior, diagnostics, lowering, tooling, tests, and user-facing docs to agree on scoped branch-guard `matches`. That gap is now closed on this branch, so no further planned work remains for the issue.

## Steps

1. [x] `refactor-branch-guard-adt` Refactor Branch Guards Into Explicit IR Nodes

Introduce explicit branch-guard ADTs in `Expr` and `TypedExpr`, with `BoolGuard` as the no-behavior-change starting point, and add shared guard-aware map/fold/traverse helpers that all branch walkers must use. This is the explicit refactor step for the issue: replace the current ad hoc `branch.guard.foreach/map` logic across `Expr.scala`, `TypedExpr.scala`, `TypedExprNormalization.scala`, `TypedExprLoopRecurLowering.scala`, `TypedExprRecursionCheck.scala`, `SelfCallKind.scala`, validator checks, and the plain bool-guard serialization surfaces before adding new semantics.

#### Invariants

- Existing boolean-guard programs keep the same free-variable accounting, rewrite behavior, and evaluation order after the structural refactor.
- Guard-aware helpers rewrite and traverse guard payloads uniformly, but do not yet open any new binder scope beyond the existing outer branch pattern.
- Plain guarded branches remain representable and round-trippable through typed IR tooling via an explicit `BoolGuard` kind rather than an implicit `Option[Expr]` convention.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `SourceConverterTest` regression that ordinary boolean branch guards and `if`/`elif` false-branch guards lower as `BoolGuard`, while guardless branches remain unwrapped.
- Add a `ProtoConverterTest` round-trip for a typed match branch carrying a plain `BoolGuard` after the IR shape change.
- Add a `ShowEdnRoundTripTest` regression that the explicit bool-guard encoding decodes back to the same typed branch shape.

#### Completion Notes

`Expr.Branch` and `TypedExpr.Branch` now store explicit `BranchGuard` nodes, `BoolGuard` is the live boolean variant, the shared guard-expression helpers drive the updated branch walkers and serialization surfaces, and verification passed with `coreJVM/test:compile`, focused `SourceConverterTest`/`ProtoConverterTest`/`ShowEdnRoundTripTest`, plus `scripts/test_basic.sh`.

2. [x] `extend-declaration-and-source-conversion` Extend Source-Level Scope and Guard Classification

Keep `Declaration.MatchBranch.guard` as source syntax, but teach `Declaration.freeVars`, `allNames`, and `substitute` to recognize a whole-guard `ConditionalMatch` on match branches and extend only that branch body scope with the inner guard-pattern names. In `SourceConverter`, classify branch guards with `Declaration.ConditionalMatch.unapply`, preserve whole-guard annotation validation via a stored check expression, canonicalize effectively-trivial inner guards, and emit `Expr.MatchGuard` instead of flattening every branch guard to a boolean expression.

#### Invariants

- Only a whole branch guard classified by `ConditionalMatch.unapply` opens the extra body scope; nested `matches` inside `&&`, `not`, or other larger boolean forms do not.
- Inner guard-pattern bindings are in scope for the optional inner guard predicate and the same branch body only; they do not leak to later branches, later guards, or outside the enclosing `match`.
- Outer annotation wrappers on the whole guard survive classification so later typechecking still validates the branch guard as a `Bool` position at the original guard boundary.
- The effectively-trivial case is canonicalized once and carried forward, so later totality and lowering code can reuse that classification instead of re-deriving it ad hoc.

#### Property Tests

- Add targeted `DeclarationTest` properties for branch guards whose whole guard is a conditional `matches`, checking that `freeVars` excludes inner guard binders outside the guarded branch body but not inside it.
- Add a `DeclarationTest` property that `Declaration.substitute` treats both outer branch-pattern names and inner guard-pattern names as masking scopes for the inner guard predicate and same-branch body.
- Keep the existing `freeVars subset allNames` law running over declarations that include top-level conditional-match branch guards, using a targeted generator helper if the general declaration generator under-produces this shape.

#### Assertion Tests

- Add `ParserTest` round-trips for `case (x, y) if as_even(x) matches Some(even_x): ...`, plus parenthesized and annotated whole-guard forms.
- Add a `ParserTest` negative classification case where `matches` is nested inside a larger boolean guard and therefore must not open branch-body scope.
- Add `SourceConverterTest` coverage that a top-level conditional-match branch guard lowers to `MatchGuard`, an ordinary boolean guard stays `BoolGuard`, the effectively-trivial case is recognized, and guard-level annotations are preserved for later type errors.

#### Completion Notes

`Declaration.freeVars`, `allNames`, and `substitute` now treat top-level conditional-match branch guards like the existing scoped conditional-match source forms: the guard itself is still traversed under the outer branch-pattern names only, while the same branch body gains the inner guard-pattern binders. `SourceConverter` now classifies those whole-guard branch forms into `Expr.MatchGuard`, stores the later Bool-position annotation check on the guard node, canonicalizes inner `if True` to `None`, and keeps ordinary branch guards on `BoolGuard`. Added focused `DeclarationTest`, `ParserTest`, and `SourceConverterTest` coverage, then reran `coreJVM/test:compile`, the focused suites, and `scripts/test_basic.sh`.

3. [x] `typecheck-matchguard-and-reuse-diagnostics` Typecheck Scoped Match Guards and Reuse Existing Diagnostics

Update `rankn/Infer.scala` so branch checking runs in three stages: outer pattern, guard, then body. `BoolGuard` still checks as `Bool` under the outer bindings; `Expr.MatchGuard` must infer the guard scrutinee, instantiate it the same way match scrutinees are instantiated for pattern checking, typecheck the guard pattern, extend the environment with guard bindings for the optional inner guard and branch body, and then revalidate the stored whole-guard Bool-position check expression when present. In the same slice, update `TypeValidator.scala`, `UnusedLetCheck.scala`, `ShadowedBindingTypeCheck.scala`, and `TotalityCheck.scala` to understand the two-layer binder scope and effectively-unguarded detection.

#### Invariants

- The operational order stays `outer pattern -> guard scrutinee/pattern -> optional inner guard -> branch body`, and the branch body typechecks under `outerBindings ++ guardBindings`.
- Right-most bindings win in the branch body: same-type outer/inner collisions continue to compile, type-changing collisions surface through the existing postponable shadowed-binding path, and a fully shadowed outer binder can still surface through the existing unused-binding path.
- A nontrivial `MatchGuard` never counts as coverage for totality or reachability; only the effectively-trivial case participates as unguarded.
- No new hard source-converter error is introduced for outer-pattern/guard-pattern collisions if the existing postponable diagnostics express the intended behavior.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add an `ErrorMessageTest` regression that a guard-pattern binder is accepted in the same branch body and in the optional inner guard, but rejected in later branches, later guards, and after the enclosing `match`.
- Add `ShadowedBindingTypeCheckTest` cases for same-type outer/inner collisions (allowed) and type-changing collisions (existing shadowed-binding lint).
- Add an `ErrorMessageTest` or equivalent compile-diagnostic regression that a fully shadowed outer branch binder can still surface through the existing unused-binding diagnostic path rather than a new source-converter failure.
- Add `TypedTotalityTest` coverage that nontrivial `MatchGuard` branches stay guarded, effectively-trivial `MatchGuard` branches participate like unguarded ones, and no new synthetic unreachable-branch behavior appears.

#### Completion Notes

`Infer.scala` now checks match branches in outer-pattern -> guard -> body order, with `Expr.MatchGuard` inferring the guard scrutinee under outer bindings, typechecking the inner pattern against the instantiated scrutinee type, extending scope with guard-pattern bindings for the optional inner guard and same-branch body, and revalidating the stored whole-guard Bool-position expression. `NameCheck`, `TypeValidator`, `UnusedLetCheck`, `ShadowedBindingTypeCheck`, and `TotalityCheck` now understand the two-layer binder scope and effectively-unguarded case, and directly coupled updates in `TypedExpr.scala`, `TypedExprNormalization.scala`, `TypedExprRecursionCheck.scala`, `TypedAst.proto`, and `ProtoConverter.scala` were absorbed so the typed pipeline and required gate stayed coherent. Added focused `ErrorMessageTest`, `ShadowedBindingTypeCheckTest`, and `TypedTotalityTest` regressions, reran `coreJVM/test:compile`, `coreJVM/testOnly dev.bosatsu.ErrorMessageTest dev.bosatsu.ShadowedBindingTypeCheckTest dev.bosatsu.TypedTotalityTest dev.bosatsu.TypedExprRecursionCheckTest`, and `scripts/test_basic.sh`, all of which passed.

4. [x] `lower-matchguard-through-backend-and-tooling` Lower MatchGuard Through Matchless and Remaining Tooling

Teach the remaining backend/tooling pipeline to carry `TypedExpr.MatchGuard` end to end. `Matchless.scala` should lower it into the existing row structure by evaluating the guard scrutinee once, compiling the inner guard pattern with the existing pattern-matrix helpers, appending guard binders after outer binders before lowering the RHS, and emitting only the necessary boolean test. Keep the already-landed proto encoding and add focused round-trip coverage, then extend `ShowEdn.scala` so the richer guard shape still round-trips across the remaining typed IR tooling without backend schema growth.

#### Invariants

- No new Matchless AST or backend-visible node is introduced; the existing row shape of pattern, optional boolean guard, RHS, and accumulated binds remains sufficient.
- The guard scrutinee is evaluated exactly once per attempted branch, even when the inner guard pattern fails or the branch falls through.
- Backend name resolution preserves the same right-most-wins shadowing order as the typed branch body by appending guard binders after outer branch binders.
- Proto and EDN decoders preserve guard kind explicitly and must not silently collapse a match-style guard into a plain boolean guard or drop it.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add `EvaluationTest` cases comparing `case ... if expr matches pattern:` against an equivalent explicit nested `match` for success, guard-pattern failure, branch fallthrough, and inner-guard success/failure.
- Add an `EvaluationTest` single-evaluation regression using a counter/ref-style helper so the guard scrutinee is observed to run once per attempted branch.
- Add a `ProtoConverterTest` round-trip for a typed branch carrying `MatchGuard`, proving the proto encoding preserves the variant.
- Add a `ShowEdnRoundTripTest` regression that the EDN/show encoding round-trips the new guard variant without collapsing it to `BoolGuard`.

#### Completion Notes

`Matchless.scala` now carries `TypedExpr.MatchGuard` through lowering by preserving a scoped guard shape until Matchless compilation, evaluating the guard scrutinee once, compiling the inner guard pattern with the existing `doesMatch` helper, appending guard binders after outer binders before lowering the RHS, and emitting only the needed boolean test. `ShowEdn.scala` now round-trips `match-guard`, focused `EvaluationTest`/`MatchlessTest`/`ProtoConverterTest`/`ShowEdnRoundTripTest` regressions were added, and a directly coupled `TypedExpr.Branch.mapGuardNodeExprScoped` identity fix stopped `MatchGuard` inner-guard normalization from looping. Verification passed with `coreJVM/testOnly dev.bosatsu.EvaluationTest dev.bosatsu.MatchlessTest dev.bosatsu.ProtoConverterTest dev.bosatsu.tool.ShowEdnRoundTripTest` and `scripts/test_basic.sh`.

5. [x] `document-and-clear-required-gate` Document the Scope Rule and Reconfirm the Gate

Update `docs/src/main/paradox/language_guide.md` so the user-facing semantics match the shipped implementation: top-level conditional `matches` in branch guards bind into the same branch body, nested boolean uses do not, and guarded totality remains conservative except for effectively-trivial guards. If this branch changes again while documenting, rerun `scripts/test_basic.sh` before PR handoff.

#### Invariants

- The language guide states the exact scoping boundary: only a whole-guard conditional `matches` extends scope, and only for that branch body plus the optional inner guard.
- The language guide notes that guarded totality remains conservative except for effectively-trivial `MatchGuard`s.
- The branch is not ready for PR handoff unless `scripts/test_basic.sh` is green on the final branch state.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run the repo-required gate: `scripts/test_basic.sh`.

#### Completion Notes

`docs/src/main/paradox/language_guide.md` now documents that a whole-guard conditional `matches` on a match or `recur` branch opens scope for the optional inner `matches` guard and the same branch body only, nested boolean uses do not extend branch-body scope, and totality stays conservative except for trivially successful guard matches. Because the docs changed in this round, verification reran `scripts/test_basic.sh`; `sbt "doc; paradox"` and `git diff --check` also passed.
