# Code Plan #2331

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `implementation`
- Issue: `#2331` Allow binding if matches syntax in patterns.
- Source design doc: `docs/design/2331-allow-binding-if-matches-syntax-in-patterns.md`
- Pending steps: `0`
- Completed steps: `11`
- Total steps: `11`

## Summary

Complete issue `#2331` by making recursive guard recovery provenance-safe: genuine whole-guard `matches` still reopen branch-body scope in recursive branches, while explicit boolean `match ... True/False` guards stay on the plain `BoolGuard` path.

## Current State

The branch now keeps recursive guard recovery provenance-safe. `TypedExprLoopRecurLowering` only restores `TypedExpr.MatchGuard` for recursive branches whose original source guard classified as a whole-guard conditional `matches`, so explicit boolean `match ... True/False` guards remain `BoolGuard`s and keep their inner binders local to the guard expression. Focused `TypedExprTest`, `EvaluationTest`, and `TypedExprRecursionCheckTest` coverage passed, and the required gate `scripts/test_basic.sh` is green on the current branch state.

## Problem

The last blocking review issue was that recursive `loop`/`recur` lowering could infer scoped-guard provenance from typed `BoolGuard` shape alone. That was insufficient because a user-written boolean guard spelled as `match ... { case p [if g]: True; case _: False }` is intentionally just a boolean expression; reconstructing it as `MatchGuard` leaks guard-local binders into the branch body and changes recursive shadowing semantics for code that never used whole-guard `matches` syntax.

## Steps

1. [x] `refactor-branch-guard-adt` Refactor Branch Guards Into Explicit IR Nodes

Introduce explicit branch-guard ADTs in `Expr` and `TypedExpr`, starting with `BoolGuard`, and route branch walkers through shared guard-aware helpers instead of ad hoc `Option[Expr]` logic.

#### Invariants

- Existing boolean-guard programs keep the same free-variable accounting, rewrite behavior, and evaluation order after the structural refactor.
- Guard-aware helpers rewrite and traverse guard payloads uniformly, but do not yet open any new binder scope beyond the existing outer branch pattern.
- Plain guarded branches remain representable and round-trippable through typed IR tooling via an explicit `BoolGuard` kind rather than an implicit `Option[Expr]` convention.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `SourceConverterTest` regression that ordinary boolean branch guards and `if` or `elif` false-branch guards lower as `BoolGuard`, while guardless branches remain unwrapped.
- Add a `ProtoConverterTest` round-trip for a typed match branch carrying a plain `BoolGuard` after the IR shape change.
- Add a `ShowEdnRoundTripTest` regression that the explicit bool-guard encoding decodes back to the same typed branch shape.

#### Completion Notes

`Expr.Branch` and `TypedExpr.Branch` now store explicit `BranchGuard` nodes, shared guard-aware helpers drive traversal and serialization, and focused source, proto, and show coverage plus `scripts/test_basic.sh` passed.

2. [x] `extend-declaration-and-source-conversion` Extend Source-Level Scope and Guard Classification

Teach `Declaration` scope bookkeeping and `SourceConverter` to recognize a whole-guard conditional `matches`, extend only the guarded branch body scope with the inner guard-pattern names, preserve guard-boundary annotations, canonicalize trivial inner guards, and emit `Expr.MatchGuard` instead of flattening everything to a boolean expression.

#### Invariants

- Only a whole branch guard classified by `ConditionalMatch.unapply` opens the extra body scope; nested `matches` inside larger boolean forms do not.
- Inner guard-pattern bindings are in scope for the optional inner guard predicate and the same branch body only; they do not leak to later branches, later guards, or outside the enclosing `match`.
- Outer annotation wrappers on the whole guard survive classification so later typechecking still validates the branch guard as a `Bool` position at the original guard boundary.
- The effectively-trivial case is canonicalized once and carried forward so later totality and lowering code can reuse that classification instead of re-deriving it ad hoc.

#### Property Tests

- Add targeted `DeclarationTest` properties for top-level conditional-match branch guards, checking that `freeVars` excludes inner guard binders outside the guarded branch body but not inside it.
- Add a `DeclarationTest` property that `Declaration.substitute` treats both outer branch-pattern names and inner guard-pattern names as masking scopes for the inner guard predicate and same-branch body.
- Keep the existing `freeVars subset allNames` law running over declarations that include top-level conditional-match branch guards, using a targeted generator helper if needed.

#### Assertion Tests

- Add `ParserTest` round-trips for `case (x, y) if as_even(x) matches Some(even_x): ...`, plus parenthesized and annotated whole-guard forms.
- Add a `ParserTest` negative classification case where `matches` is nested inside a larger boolean guard and therefore must not open branch-body scope.
- Add `SourceConverterTest` coverage that a top-level conditional-match branch guard lowers to `MatchGuard`, an ordinary boolean guard stays `BoolGuard`, the effectively-trivial case is recognized, and guard-level annotations are preserved for later type errors.

#### Completion Notes

`Declaration.freeVars`, `allNames`, and `substitute` now respect whole-guard conditional `matches` on match branches, and `SourceConverter` classifies those guards into `Expr.MatchGuard` while preserving annotations and trivial-guard canonicalization. Focused declaration, parser, and source-converter coverage passed along with `scripts/test_basic.sh`.

3. [x] `typecheck-matchguard-and-reuse-diagnostics` Typecheck Scoped Match Guards and Reuse Existing Diagnostics

Update inference and validation so branch checking runs in outer pattern, guard, then body order. `BoolGuard` still checks as `Bool` under outer bindings; `MatchGuard` infers the guard scrutinee under outer bindings, checks the guard pattern against that type, extends scope with guard bindings for the optional inner guard and branch body, and revalidates the stored whole-guard bool-position expression.

#### Invariants

- The operational order stays `outer pattern -> guard scrutinee or pattern -> optional inner guard -> branch body`, and the branch body typechecks under `outerBindings ++ guardBindings`.
- Right-most bindings win in the branch body: same-type outer and inner collisions continue to compile, type-changing collisions surface through the existing postponable shadowed-binding path, and a fully shadowed outer binder can still surface through the existing unused-binding path.
- A nontrivial `MatchGuard` never counts as coverage for totality or reachability; only the effectively-trivial case participates as unguarded.
- No new hard source-converter error is introduced for outer-pattern and guard-pattern collisions if the existing postponable diagnostics express the intended behavior.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add an `ErrorMessageTest` regression that a guard-pattern binder is accepted in the same branch body and in the optional inner guard, but rejected in later branches, later guards, and after the enclosing `match`.
- Add `ShadowedBindingTypeCheckTest` cases for same-type outer and inner collisions and for type-changing collisions.
- Add an `ErrorMessageTest` or equivalent diagnostic regression that a fully shadowed outer branch binder can still surface through the existing unused-binding path rather than a new source-converter failure.
- Add `TypedTotalityTest` coverage that nontrivial `MatchGuard` branches stay guarded, effectively-trivial `MatchGuard` branches participate like unguarded ones, and no new synthetic unreachable-branch behavior appears.

#### Completion Notes

`Infer`, `NameCheck`, `TypeValidator`, `UnusedLetCheck`, `ShadowedBindingTypeCheck`, and `TotalityCheck` now understand the two-layer branch scope and reuse the existing postponable diagnostics for outer and inner collisions. Focused error, shadowing, totality, and recursion coverage passed along with the required gate.

4. [x] `lower-matchguard-through-backend-and-tooling` Lower MatchGuard Through Matchless and Remaining Tooling

Carry `TypedExpr.MatchGuard` through Matchless lowering and the remaining typed tooling without introducing a backend-visible node. Lower the guard scrutinee exactly once, compile the guard pattern with the existing helpers, append guard binders after outer binders before lowering the branch body, and preserve the distinct guard kind through typed IR round-trips.

#### Invariants

- No new Matchless AST or backend-visible node is introduced; the existing row shape of pattern, optional boolean guard, rhs, and accumulated binds remains sufficient.
- The guard scrutinee is evaluated exactly once per attempted branch, even when the inner guard pattern fails or the branch falls through.
- Backend name resolution preserves the same right-most-wins shadowing order as the typed branch body by appending guard binders after outer branch binders.
- Valid proto and EDN round-trips preserve guard kind explicitly.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add `EvaluationTest` cases comparing `case ... if expr matches pattern:` against an equivalent explicit nested `match` for success, guard-pattern failure, branch fallthrough, and inner-guard success or failure.
- Add an `EvaluationTest` single-evaluation regression using a counter or ref-style helper so the guard scrutinee is observed to run once per attempted branch.
- Add a `ProtoConverterTest` round-trip for a typed branch carrying `MatchGuard`, proving the proto encoding preserves the variant.
- Add a `ShowEdnRoundTripTest` regression that the EDN or show encoding round-trips the new guard variant without collapsing it to `BoolGuard`.

#### Completion Notes

`Matchless` now preserves scoped guard lowering, evaluates the scrutinee once, appends guard binders after outer binders, and emits only the needed boolean test. `ShowEdn` round-trips `MatchGuard`, and focused evaluation, Matchless, proto, and show coverage passed.

5. [x] `document-and-clear-required-gate` Document the Scope Rule and Reconfirm the Gate

Update `docs/src/main/paradox/language_guide.md` so the user-facing semantics match the implementation: a top-level conditional `matches` in a branch guard binds into the same branch body, nested boolean uses do not, and guarded totality remains conservative except for effectively-trivial guards.

#### Invariants

- The language guide states the exact scoping boundary: only a whole-guard conditional `matches` extends scope, and only for that branch body plus the optional inner guard.
- The language guide notes that guarded totality remains conservative except for effectively-trivial `MatchGuard` cases.
- The branch is not ready for PR handoff unless `scripts/test_basic.sh` is green on the final branch state.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run the repo-required gate: `scripts/test_basic.sh`.

#### Completion Notes

The language guide now documents whole-guard `matches` scope for match and recur branches, notes the conservative totality behavior, and the documentation round reran `scripts/test_basic.sh`. `doc; paradox` and `git diff --check` also passed.

6. [x] `close-typed-matchguard-binder-followthrough` Finish Binder-Aware Typed Transforms and Recursive Consumers

Carry `MatchGuard` binder scope through typed substitution, type replacement, loop or recur lowering, self-call classification, and recursion checking. These transforms must treat guard-pattern binders as real binders and preserve the right-most-wins contract without collapsing `MatchGuard` back to `BoolGuard`.

#### Invariants

- The guard scrutinee is transformed under outer branch-pattern binders only; the optional inner guard and same-branch body are transformed under `outerBindings ++ guardBindings`.
- Typed substitution, alpha-renaming, and recursive or self-call classification treat guard-pattern binders as real binders, so a guard-local shadow of the recursive function name is never misclassified as a self call.
- Any transform that changes a guarded branch preserves the original guard kind.

#### Property Tests

- Add a targeted `TypedExprTest` property that `TypedExpr.substituteAll` is identity when the substituted name is masked by either the outer branch pattern or the `MatchGuard` pattern, including the optional inner guard and same-branch body.

#### Assertion Tests

- Add a `TypedExprTest` regression that substituting across `case _ if opt matches Some(x): ...x...` leaves the guard-bound `x` untouched and alpha-renames the branch when the replacement expression would otherwise capture `x`.
- Add a `TypedExprTest` regression that `coerceFn` or `replaceVarType` leaves `MatchGuard`-bound locals untouched while outer guard-scrutinee references still track the rewritten outer binder type.
- Add a `TypedExprTest` regression that grouped loop or recur lowering rewrites a branch with `MatchGuard` without rebuilding it as `BoolGuard`.
- Add `SelfCallKindTest` and `TypedExprRecursionCheckTest` regressions that a guard-bound function name is treated as a local shadow rather than as a recursive self reference.

#### Completion Notes

`TypedExpr` substitution and alpha-renaming, `replaceVarType`, grouped loop or recur lowering, `SelfCallKind`, and `TypedExprRecursionCheck` now all honor the guard binder split and preserve `MatchGuard` structure. Focused typed, self-call, and recursion coverage passed with the required gate.

7. [x] `harden-branch-guard-proto-decoding` Fail Fast on Invalid Typed BranchGuard Payloads

Tighten `ProtoConverter.decodeGuard` so a present `BranchGuard` message whose `oneof` is unset or otherwise unexpected is rejected instead of silently decoding to `None`. Valid `BoolGuard` and `MatchGuard` encodings should continue to round-trip unchanged.

#### Invariants

- A missing branch-guard field still means `None`, but a present `BranchGuard` with no recognized `oneof` value is a decode failure, not an unguarded branch.
- Valid `BoolGuard` and `MatchGuard` encodings remain structurally distinct through proto round-trips.
- The branch is not ready for PR handoff until the focused regressions and `scripts/test_basic.sh` pass on the post-review revision.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `ProtoConverterTest` negative decode regression for a branch with a present-but-empty or otherwise unrecognized `BranchGuard`, asserting decode failure instead of `Success(None)`.
- Run `coreJVM/test:compile`, focused `ProtoConverterTest`, `TypedExprTest`, and `TypedExprRecursionCheckTest`, then rerun `scripts/test_basic.sh`.

#### Completion Notes

`ProtoConverter.decodeGuard` now fails closed on malformed present guard payloads instead of erasing them to `None`. A negative decode regression landed, focused typed and proto coverage passed, and `scripts/test_basic.sh` stayed green.

8. [x] `fix-typedexprnormalization-matchguard-scope` Preserve MatchGuard Structure in Normalization Rewrites

Address the first wave of normalization follow-through for `MatchGuard` by keeping specialized rewrite and fast-path code in `TypedExprNormalization` from collapsing scoped guards through the synthetic boolean view, and by using `branch.allBindings` wherever code is moved across branch boundaries.

#### Invariants

- Scope-preserving rewrite and hoist paths keep `MatchGuard` binders in scope for the optional inner guard and same branch body only; no selected or rebuilt body may leak those names outside that scope.
- Bool-only normalization fast paths stay gated on `guardBindings.isEmpty` unless the transformed result explicitly preserves binder scope and single-evaluation behavior.
- Normalization rewrites that move lets or lambdas across branch boundaries treat `branch.allBindings` as capturing binders for the branch body and optional inner guard.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add `TypedExprTest` regressions that normalized source-level `MatchGuard` branches keep outer and inner binders non-free in the normalized root.
- Add `TypedExprTest` regressions for leading-wildcard and trailing-guard-pair rewrites where the branch body uses a guard-pattern binder, asserting the binder stays scoped and the guard scrutinee still appears once in the normalized result.
- Add `TypedExprTest` regressions for literal and constructor branch-selection fast paths with `Some(1) matches Some(x)` guards, asserting selected bodies do not leak `x`.
- Add a `TypedExprTest` regression that repeated `Some(x)` expressions inside a `MatchGuard` branch do not hoist `x` out of scope during normalization.
- Run `coreJVM/test:compile`, `TypedExprTest`, `EvaluationTest`, and `scripts/test_basic.sh`.

#### Completion Notes

`TypedExprNormalization` now preserves `MatchGuard` structure and scope through sharing, inline unshadowing, leading and trailing guard rewrites, constructor and literal branch selection, let sinking, and lambda pushdown. Focused typed and evaluation regressions passed with the required gate.

9. [x] `finish-binder-sensitive-normalization-match-walkers` Finish Binder-Sensitive Match Walkers in TypedExprNormalization

Close the remaining binder-sensitive normalization gap by teaching the match walkers in `TypedExprNormalization` to distinguish outer guard-scrutinee scope from inner guard and body scope. This covers shadowed recur-invariance checks and closure-call rewriting without adding another ad hoc branch walker.

#### Invariants

- Loop-invariant analysis only treats a local reference as the outer loop binder when that name is visible from the outer branch scope; a `MatchGuard`-bound shadow of the same name inside the inner guard or branch body must not count as the outer binder.
- Closure escape detection and call-prepending use outer scope for the guard scrutinee and `branch.allBindings` for the optional inner guard plus branch body, so a guard-local function name masks the captured closure name throughout that inner scope.
- These normalization fixes preserve existing `MatchGuard` guarantees: no duplicated guard-scrutinee evaluation, no collapse to `BoolGuard`, and the same right-most-wins name resolution as typed branch bodies.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `TypedExprTest` regression for the shadowed loop-invariant case, asserting normalization does not drop or reorder outer `recur` arguments when a `MatchGuard` binder reuses a loop name.
- Add a `TypedExprTest` regression for the shadowed closure-call case, asserting normalization does not prepend captured closure arguments through a `MatchGuard`-bound function name in the optional inner guard or branch body.
- Add an `EvaluationTest` end-to-end regression for a shadowed closure example, comparing compiled behavior with an equivalent explicit nested `match`.
- Run `coreJVM/test:compile`, focused `TypedExprTest` and `EvaluationTest`, and `scripts/test_basic.sh`.

#### Completion Notes

`TypedExprNormalization` now threads shadowed-name visibility through `outerRecurInvariantFlags`, `hasEscapingFnRef`, and `prependArgsToFnCalls`, preserving shadowed recur slots and closure-call rewriting for `MatchGuard`. Focused typed and evaluation regressions plus `scripts/test_basic.sh` passed.

10. [x] `fix-source-loop-recur-shadowing-followthrough` Fix Source-Level Loop/Recur Shadowing Follow-Through

Finish the adjacent explicit `loop` or `recur` surface uncovered while landing the normalization walker fixes. A source-level branch like `case (S(prev), _) if prev matches x: loop_guarded(prev, x)` had been lowering to valid typed `MatchGuard` semantics but still normalizing as if the guard binder never reached the recursive body, so the missing recursive follow-through had to keep the explicit loop slot alive end to end.

#### Invariants

- Explicit source-level `loop` and `recur` branches use the same binder model as ordinary match branches: the guard scrutinee sees outer pattern names only, and the optional inner guard plus branch body see `outerBindings ++ guardBindings`.
- A guard-bound shadow inside an explicit source-level `loop` or `recur` branch must not collapse a recur slot to the outer accumulator or erase the recursive state update during normalization.
- The fix preserves the earlier typed-normalization walker behavior and the closure-shadowing regression coverage from the previous step.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a `TypedExprTest` or equivalent source-level lowering regression that `loop (n, x): case (S(prev), _) if prev matches x: ...` still lowers and normalizes with both recur slots intact.
- Add an `EvaluationTest` end-to-end regression for a shadowed explicit `loop` or `recur` example compared with an equivalent explicit nested `match`.
- Run `coreJVM/test:compile`, focused `TypedExprTest`, `EvaluationTest`, and `TypedExprRecursionCheckTest`, plus `scripts/test_basic.sh`.

#### Completion Notes

`TypedExprLoopRecurLowering.scala` now keeps the positive source-level `loop_guarded` whole-guard `matches` case alive through lowering, normalization, and evaluation, and the focused `TypedExprTest` and `EvaluationTest` regressions for that case passed with `coreJVM/test:compile`, `TypedExprRecursionCheckTest`, and `scripts/test_basic.sh`. Pre-PR review later found that the current shape-based recursive guard restoration is over-broad for explicit boolean `match ... True/False` guards, so a provenance-safe follow-up remains before PR handoff.

11. [x] `make-recursive-matchguard-recovery-provenance-safe` Make Recursive MatchGuard Recovery Provenance-Safe

Address review finding `F1` by replacing the shape-only `BoolGuard` -> `MatchGuard` recovery in `TypedExprLoopRecurLowering.scala` with a small provenance refactor. Carry enough source-classification information from the whole-guard conditional-`matches` pipeline, or through the adjacent recursive lowering staging that consumes it, so recursive `loop`/`recur` lowering restores only genuine scoped guards before binder-sensitive rewrites and leaves user-written boolean `match ... True/False` guards on the plain `BoolGuard` path.

#### Invariants

- Recursive lowering never reconstructs `TypedExpr.MatchGuard` from typed AST shape alone; only guards proven to originate from source whole-guard conditional `matches` may reopen branch-body scope.
- User-written boolean guards shaped as `match ... { case p [if g]: True; case _: False }` remain `BoolGuard`s through `loop`/`recur` lowering, so guard-local binders stay local to the guard expression and cannot change recur-slot or closure-shadowing behavior in the branch body.
- The valid source `expr matches pattern` recursive case from the previous step still keeps single guard-scrutinee evaluation and right-most-wins shadowing for the genuine guard binder.

#### Property Tests

- None recorded.

#### Assertion Tests

- Extend the existing `TypedExprTest` source-level `loop_guarded` regression to assert the provenance-aware path still yields `MatchGuard` for genuine recursive whole-guard `matches` after the refactor.
- Add a `TypedExprTest` regression that a recursive `loop` branch with an explicit boolean `match keep_nat(prev): case Some(x): True; case _: False` guard still lowers as `BoolGuard`, and normalization keeps the outer `x` recur slot instead of the guard-local `x`.
- Add the analogous recursive `recur` regression, again using an inner `Some(x)` binder that shadows an outer `x`, to prove the binder remains guard-local and does not retarget recursive branch semantics.
- Add `EvaluationTest` comparisons for the explicit boolean `loop` and `recur` guard forms against equivalent explicit nested matches, pinning that the branch body sees the outer binder while genuine whole-guard `matches` keep the existing scoped behavior.
- Run `coreJVM/test:compile`, focused `TypedExprTest`, `EvaluationTest`, and `TypedExprRecursionCheckTest`, plus `scripts/test_basic.sh`.

#### Completion Notes

`TypedExprLoopRecurLowering` now consults recursive branch provenance from the original source `Declaration.Match` tag before restoring a scoped guard, so only genuine whole-guard conditional `matches` recover as `MatchGuard`. New typed and evaluation regressions cover explicit boolean `loop` and `recur` guards that shadow an outer binder, the positive source-level `loop_guarded` path still proves `MatchGuard` recovery, and `coreJVM/test:compile`, `TypedExprTest`, `EvaluationTest`, `TypedExprRecursionCheckTest`, and `scripts/test_basic.sh` all passed.
