# Code Plan #2331

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `implementation`
- Issue: `#2331` Allow binding if matches syntax in patterns.
- Source design doc: `docs/design/2331-allow-binding-if-matches-syntax-in-patterns.md`
- Pending steps: `1`
- Completed steps: `7`
- Total steps: `8`

## Summary

Finish scoped match-branch guards so every compiler phase and typed IR tool, including `TypedExprNormalization`, honors the same binder model for `case p_outer if expr matches p_inner:`: match the outer pattern first, evaluate the guard scrutinee once, check the inner guard pattern and optional inner guard, then run the branch body in `outerBindings ++ guardBindings`. Preserve right-most-wins shadowing through normalization, typed substitution, recursion analysis, loop/recur lowering, Matchless lowering, and proto/EDN tooling, keep totality conservative except for effectively-trivial guards, and fail closed on invalid guard encodings without backend schema growth.

## Current State

The branch already lands the explicit branch-guard ADT, source/typechecker support, Matchless lowering, docs, recursive-consumer follow-through, and fail-fast proto decoding for `MatchGuard`. However, the current plan has no pending step for the remaining pre-PR blocker: review of the authoritative candidate patch found that `TypedExprNormalization` still has approval-blocking scope holes where some rewrites and hoist/rename helpers use the synthetic `branch.guard` view or only `branch.pattern.names`, so normalization can still drop or capture guard-local binders. The prior focused suites and `scripts/test_basic.sh` were green on the post-review revision, but they did not pin these normalization-specific binder cases.

## Problem

Blocking review finding `F1` shows that normalization is still not semantics-preserving for scoped branch guards. In `TypedExprNormalization.scala`, `rewriteLeadingWildcardGuard`, `rewriteTrailingGuardPair`, the constructor/literal `chooseBranch` fast paths, `shareImmutableValues`, and `unshadowInlineBranch` can move, select, hoist, or rename `branch.expr` after looking only at the synthetic boolean guard or the outer branch-pattern binders. A branch like `case _ if opt matches Some(x): x` can therefore normalize into an expression where `x` is free, captured, or otherwise no longer scoped only to that branch's inner guard/body path. Until normalization becomes `guardNode`/`guardBindings` aware and gains regressions that exercise inner-guard/body uses of guard-local binders, the branch is not ready for another review round.

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

`Infer.scala` now checks match branches in outer-pattern -> guard -> body order, with `Expr.MatchGuard` inferring the guard scrutinee under outer bindings, typechecking the inner pattern against the instantiated scrutinee type, extending scope with guard-pattern bindings for the optional inner guard and same-branch body, and revalidating the stored whole-guard Bool-position expression. `NameCheck`, `TypeValidator`, `UnusedLetCheck`, `ShadowedBindingTypeCheck`, and `TotalityCheck` now understand the two-layer binder scope and effectively-unguarded case. Initial follow-through changes in `TypedExpr.scala`, `TypedExprNormalization.scala`, `TypedExprRecursionCheck.scala`, `TypedAst.proto`, and `ProtoConverter.scala` also landed in this slice, but pre-PR review found remaining binder-scope and decoder-contract gaps in that follow-through work; the pending steps below track that cleanup. Added focused `ErrorMessageTest`, `ShadowedBindingTypeCheckTest`, and `TypedTotalityTest` regressions, reran `coreJVM/test:compile`, `coreJVM/testOnly dev.bosatsu.ErrorMessageTest dev.bosatsu.ShadowedBindingTypeCheckTest dev.bosatsu.TypedTotalityTest dev.bosatsu.TypedExprRecursionCheckTest`, and `scripts/test_basic.sh`, all of which passed on the pre-review revision.

4. [x] `lower-matchguard-through-backend-and-tooling` Lower MatchGuard Through Matchless and Remaining Tooling

Teach the remaining backend/tooling pipeline to carry `TypedExpr.MatchGuard` end to end on valid typed IR. `Matchless.scala` should lower it into the existing row structure by evaluating the guard scrutinee once, compiling the inner guard pattern with the existing pattern-matrix helpers, appending guard binders after outer binders before lowering the RHS, and emitting only the necessary boolean test. Keep the already-landed proto encoding and add focused valid-path round-trip coverage, then extend `ShowEdn.scala` so the richer guard shape still round-trips across the remaining typed IR tooling without backend schema growth.

#### Invariants

- No new Matchless AST or backend-visible node is introduced; the existing row shape of pattern, optional boolean guard, RHS, and accumulated binds remains sufficient.
- The guard scrutinee is evaluated exactly once per attempted branch, even when the inner guard pattern fails or the branch falls through.
- Backend name resolution preserves the same right-most-wins shadowing order as the typed branch body by appending guard binders after outer branch binders.
- Valid proto and EDN round-trips preserve guard kind explicitly; fail-fast handling for malformed proto guard payloads is tracked in a follow-up step.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add `EvaluationTest` cases comparing `case ... if expr matches pattern:` against an equivalent explicit nested `match` for success, guard-pattern failure, branch fallthrough, and inner-guard success/failure.
- Add an `EvaluationTest` single-evaluation regression using a counter/ref-style helper so the guard scrutinee is observed to run once per attempted branch.
- Add a `ProtoConverterTest` round-trip for a typed branch carrying `MatchGuard`, proving the proto encoding preserves the variant.
- Add a `ShowEdnRoundTripTest` regression that the EDN/show encoding round-trips the new guard variant without collapsing it to `BoolGuard`.

#### Completion Notes

`Matchless.scala` now carries `TypedExpr.MatchGuard` through lowering by preserving a scoped guard shape until Matchless compilation, evaluating the guard scrutinee once, compiling the inner guard pattern with the existing `doesMatch` helper, appending guard binders after outer binders before lowering the RHS, and emitting only the needed boolean test. `ShowEdn.scala` now round-trips `match-guard`, focused `EvaluationTest`/`MatchlessTest`/`ProtoConverterTest`/`ShowEdnRoundTripTest` regressions were added, and a directly coupled `TypedExpr.Branch.mapGuardNodeExprScoped` identity fix stopped `MatchGuard` inner-guard normalization from looping. Pre-PR review later found two remaining follow-up gaps outside Matchless lowering itself: some typed recursive/self-call consumers still shadow only outer pattern binders, and malformed proto guard payloads still decode as `None`; those are tracked below. Verification passed with `coreJVM/testOnly dev.bosatsu.EvaluationTest dev.bosatsu.MatchlessTest dev.bosatsu.ProtoConverterTest dev.bosatsu.tool.ShowEdnRoundTripTest` and `scripts/test_basic.sh` on the pre-review revision.

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

6. [x] `close-typed-matchguard-binder-followthrough` Finish Binder-Aware Typed Transforms and Recursive Consumers

Carry `MatchGuard` binder scope through typed substitution, type replacement, loop/recur lowering, self-call classification, and recursion checking. In `TypedExpr.scala`, thread `branch.guardBindings` and `branch.allBindings` through `substituteAll`, `replaceVarType`, and `unshadowBranch` so typed substitution and alpha-renaming treat `MatchGuard` binders as real binders and preserve the right-most-wins contract. In `TypedExprLoopRecurLowering.scala`, `SelfCallKind.scala`, and `TypedExprRecursionCheck.scala`, use the same branch binder split for self-call classification, loop/recur legality, and grouped rewrites, and rebuild guard nodes with the scoped guard-node helpers so rewrites keep `MatchGuard` structure instead of flattening it to `BoolGuard`.

#### Invariants

- The guard scrutinee is transformed under outer branch-pattern binders only; the optional inner guard and same-branch body are transformed under `outerBindings ++ guardBindings`.
- Typed substitution, alpha-renaming, and recursive/self-call classification treat guard-pattern binders as real binders, so a guard-local shadow of the recursive function name is never misclassified as a self call.
- Any transform that changes a guarded branch preserves the original guard kind; rewriting a `MatchGuard` may change its subexpressions but must not collapse it to `BoolGuard` or drop its binder scope.

#### Property Tests

- Add a targeted `TypedExprTest` property that `TypedExpr.substituteAll` is identity when the substituted name is masked by either the outer branch pattern or the `MatchGuard` pattern, including the optional inner guard and same-branch body.

#### Assertion Tests

- Add a `TypedExprTest` regression that substituting across `case _ if opt matches Some(x): ...x...` leaves the guard-bound `x` untouched, and alpha-renames the branch when the replacement expression would otherwise capture `x`.
- Add a `TypedExprTest` regression that `coerceFn`/`replaceVarType` leaves `MatchGuard`-bound locals untouched while outer guard-scrutinee references still track the rewritten outer binder type.
- Add a `TypedExprTest` regression that grouped loop/recur lowering rewrites a branch with `MatchGuard` without rebuilding it as `BoolGuard`.
- Add a `SelfCallKindTest` plus a `TypedExprRecursionCheckTest` regression that a guard-bound `f` is treated as a local shadow for self-call classification and recursion legality rather than as a recursive self reference.

#### Completion Notes

`TypedExpr.substituteAll` now filters substitutions separately for outer-pattern scope versus `MatchGuard` body scope, and `unshadowBranch` alpha-renames outer and guard-pattern binders independently so capture avoidance preserves right-most-wins semantics. `replaceVarType` now rewrites the guard scrutinee under outer bindings only, leaving guard-bound locals untouched in the optional inner guard and branch body. `TypedExprLoopRecurLowering` now keeps `MatchGuard` nodes intact during grouped rewrites and tail-call lowering while honoring the guard binder split, `SelfCallKind` no longer counts guard-shadowed branch bodies or inner guards as recursive self calls, and `TypedExprRecursionCheck` now checks the guard scrutinee before introducing guard-pattern binders for the inner guard/body path. Added focused `TypedExprTest`, `SelfCallKindTest`, and `TypedExprRecursionCheckTest` regressions, then reran `coreJVM/test:compile`, `coreJVM/testOnly dev.bosatsu.TypedExprTest dev.bosatsu.SelfCallKindTest dev.bosatsu.TypedExprRecursionCheckTest`, and `scripts/test_basic.sh`. A later pre-PR review still found normalization-specific scope bugs in `TypedExprNormalization`; that remaining work is tracked below.

7. [x] `harden-branch-guard-proto-decoding` Fail Fast on Invalid Typed BranchGuard Payloads

Close review finding `F3` by tightening `ProtoConverter.decodeGuard` so a present `BranchGuard` message whose `oneof` is unset or otherwise unexpected is rejected instead of silently decoding to `None`. Keep valid `BoolGuard` and `MatchGuard` round-trips unchanged, but make malformed or forward-incompatible payloads fail closed so guarded branches cannot be decoded as unguarded ones. After the fix, rerun the focused typed/proto suites and the repo-required gate on the final post-review branch state.

#### Invariants

- A missing branch-guard field still means `None`, but a present `BranchGuard` with no recognized `oneof` value is a decode failure, not an unguarded branch.
- Valid `BoolGuard` and `MatchGuard` encodings remain structurally distinct through proto round-trips.
- The branch is not ready for PR handoff until the focused regressions and `scripts/test_basic.sh` pass on the post-review revision.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `ProtoConverterTest` negative decode regression for a branch with a present-but-empty or otherwise unrecognized `BranchGuard`, asserting decode failure instead of `Success(None)`.
- Run `sbt "coreJVM/test:compile"`, focused `ProtoConverterTest`, `TypedExprTest`, and `TypedExprRecursionCheckTest` coverage, then rerun `scripts/test_basic.sh`.

#### Completion Notes

`ProtoConverter.decodeGuard` now treats a present `BranchGuard` whose `oneof` is unset as invalid typed-AST input and returns failure instead of `None`, so malformed or forward-incompatible payloads cannot erase guarded branches during decode. Added a `ProtoConverterTest` regression that mutates an encoded match branch to carry `Some(proto.BranchGuard())` and asserts decode failure, then reran `sbt "coreJVM/test:compile" "coreJVM/testOnly dev.bosatsu.ProtoConverterTest dev.bosatsu.TypedExprTest dev.bosatsu.TypedExprRecursionCheckTest"` and `scripts/test_basic.sh`, all of which passed.

8. [ ] `fix-typedexprnormalization-matchguard-scope` Make TypedExprNormalization Preserve MatchGuard Scope

Close blocking review finding `F1` by making `TypedExprNormalization.scala` `guardNode`/`guardBindings` aware end to end. `rewriteLeadingWildcardGuard`, `rewriteTrailingGuardPair`, and the constructor/literal `chooseBranch` fast paths must either preserve `MatchGuard` binders when they rebuild or select branch bodies, or explicitly skip the rewrite for `MatchGuard` instead of routing through the synthetic `branch.guard` view. In the same slice, update `shareImmutableValues` and `unshadowInlineBranch` to use outer-pattern binders for the guard-scrutinee scope and `branch.allBindings` for the optional inner guard plus branch body, so hoisting and alpha-renaming cannot move, duplicate, or capture guard-local names.

#### Invariants

- After normalization, `MatchGuard` binders remain in scope for the optional inner guard and same branch body only; no rewrite may materialize a body that refers to those names outside that combined scope.
- Guard-scrutinee transforms run under outer branch-pattern binders only, while hoisting, alpha-renaming, and branch-body rewrites treat `branch.allBindings` as the live blocker set for the optional inner guard and branch body.
- Bool-only normalization fast paths remain available for `BoolGuard` branches, but `MatchGuard` branches may only take those paths when the transformed result explicitly preserves the same binder scope and single-evaluation behavior.
- When a scope-preserving rewrite is not local or obvious, the normalizer may leave a `MatchGuard` branch unchanged rather than collapsing it through the synthetic boolean view.

#### Property Tests

- Add a targeted `TypedExprTest` property that normalizing a `MatchGuard` branch with distinct outer and inner binder names keeps those binders non-free in the normalized root when the optional inner guard and branch body both reference them.

#### Assertion Tests

- Add a `TypedExprTest` regression for the leading-wildcard rewrite on `case _ if opt matches Some(x): x`, asserting normalization either preserves the `MatchGuard` or rebuilds an equivalent nested match whose successful arm still binds `x`.
- Add a `TypedExprTest` regression for the trailing-guard-pair rewrite where the guarded branch body uses the inner `MatchGuard` binder, asserting the rewrite does not drop the binder or evaluate the guard scrutinee twice.
- Add `TypedExprTest` regressions for the constructor and literal `chooseBranch` fast paths, proving constant-folded branch selection keeps `MatchGuard` binders in scope for the selected body.
- Add a `TypedExprTest` regression that `shareImmutableValues` and `unshadowInlineBinders` do not hoist or capture expressions across a `MatchGuard` inner binder used in the inner guard and branch body.
- Run `sbt "coreJVM/test:compile"`, focused `coreJVM/testOnly dev.bosatsu.TypedExprTest dev.bosatsu.EvaluationTest`, then rerun `scripts/test_basic.sh` on the post-fix branch state.
