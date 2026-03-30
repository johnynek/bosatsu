# Inlining Improvements Work Plan

## Context

We recently added Matchless global inlining and `show` support for inspecting generated `TypedExpr` and `Matchless` at different optimization points. Running those views against `../zafu` makes several optimizer gaps visible, especially in higher-order `Traverse` and `Applicative` code such as:

- `Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn`
- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

The current optimizer already has some of the building blocks we want, but they are narrow or disconnected:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala` already has a narrow let-to-match sinking rewrite around line 1368.
- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala` already has `rewriteNonEscapingClosureBinding` around line 1001, but it is only invoked for recursive lets around line 1358.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` already has `parameterDemandSummary` and `inlineApplyArgs`, which can keep some non-cheap arguments inside `If` and `SwitchVariant` branches after beta-reduction.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` already has `simplifyKnownConditions.knownValue`, which can fold `GetStructElement` and `GetEnumElement` when the underlying constructor value is syntactically known.
- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala` currently publishes summaries only for lambdas, not small pure values.
- TypedExpr inlining still uses hard size gates (`ResolveToLambda.MaxSize`, `MaxLocalSize`, `LambdaArgInlineBonus`), while Matchless uses a separate coarse score based on demand and body weight.
- `core/src/main/scala/dev/bosatsu/Matchless.scala` still runs a single `postLoweringCleanup` round (`hoistInvariantLoopLets`, `reuseConstructors`, `simplifyKnownConditions`) rather than a bounded fixpoint.

The goal of this plan is to turn those existing pieces into a more general optimizer pipeline that removes wrapper plumbing, avoids eager branch-only work, and converges on a stable final Matchless shape for semantically equivalent source programs.

## Goals

- Make semantically equivalent source variants compile to the same or nearly the same final Matchless shape.
- Evaluate expensive pure work only in the branches that actually need it.
- Inline small pure helper values, not just lambdas, so wrapper records and field-selection scaffolding disappear.
- Turn non-escaping local function values into direct-call helpers with explicit parameters for captures.
- Keep laziness and escaping-function behavior correct, especially around `Unit -> a` suspension boundaries.
- Avoid size growth in cases where inlining does not unlock any real simplification.

## How We Will Evaluate Progress

### Build and run against `../zafu`

Do not make future implementers rediscover the invocation path. For a checked-out repository such as `../zafu`, use the top-level `show` command, not `tool show`.

Verified baseline:

- Current working branch: `codex/inlining-improvements`
- Branch baseline when this plan was prepared: aligned with `origin/main`
- Verified CLI assembly output after item 1 work: `cli/target/scala-3.8.2/bosatsu-cli-assembly-0.0.61+2-e81d96d5+20260330-0940-SNAPSHOT.jar`

Build the current CLI once from the Bosatsu repo root:

```sh
sbt cli/assembly
```

Then use the checked-in wrapper, which always picks the newest assembly jar from `cli/target/scala-*/bosatsu-cli-assembly-*.jar`:

```sh
mkdir -p /tmp/zafu_show

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir typedexpr \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.typed.edn

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir matchless \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.matchless.edn

./bosatsuj show \
  --repo_root ../zafu \
  --name zafu \
  --value Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn \
  --ir matchless \
  --disable-matchless-pass global-inlining \
  --color none \
  --no_cache \
  --output /tmp/zafu_show/nonemptychain.matchless.no_global_inline.edn
```

Useful follow-up comparison:

```sh
diff -u \
  /tmp/zafu_show/nonemptychain.matchless.no_global_inline.edn \
  /tmp/zafu_show/nonemptychain.matchless.edn
```

Swap the `--value` selector to inspect the other standing probes:

- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

### IR inspection

Use `show --ir typedexpr` and `show --ir matchless` on targeted Zafu values, with typed and Matchless pass toggles when needed to isolate each rewrite. The primary probes should stay the higher-order sites that exposed the weaknesses in the first place:

- `Zafu/Collection/NonEmptyChain::traverse_NonEmptyChain_fn`
- `Zafu/Collection/Chain::traverse_Chain_fn`
- `Zafu/Collection/Vector::traverse_Vector_fn`
- `Zafu/Control/Result::traverse_Result_fn`
- `Zafu/Control/PartialResult::traverse_PartialResult_fn`

### Regression tests

Expand the existing test areas rather than inventing a separate harness:

- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`
- `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`

### Progress checks

- Final Matchless no longer contains obvious dead lets, projections, or wrapper calls that disappear if one more cleanup round is run manually.
- Re-running the bounded optimizer pipeline is a no-op on the resulting IR.
- `Matchless.exprWeight` improves or stabilizes across rounds and does not oscillate.
- Escaping functions remain closures when they must, and `Unit -> a` thunks still preserve laziness.

### Current status after item 1

- The first work item is implemented and covered by focused regressions in `TypedExprTest` and `MatchlessTest`.
- The standing Zafu probe files for `NonEmptyChain`, `Chain`, `Result`, and `PartialResult` were rerun after the change and are still textually unchanged.
- That is expected: item 1 installs the general branch-sinking machinery, while item 2 is what should expose more pure wrapper values and let the existing Zafu higher-order probes benefit more visibly.

## Items To Do

1. [x] General branch-sinking after inlining

Relevant code:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessApplyArgsTest.scala`

Plan:

- Generalize the current TypedExpr let-to-match rewrite into a reusable pass that can sink pure branch-only work under `Match` and equivalent branch structure after inlining exposes it.
- Add the Matchless analogue so pure lets and eager call arguments can be pushed under `If` and `SwitchVariant` after beta-reduction and global inlining, not only when `inlineApplyArgs` sees the opportunity directly at the call site.
- Preserve current behavior when both branches need the value, or when the condition or discriminant depends on the value.

Done when:

- Branch-only expensive expressions no longer appear in pre-branch lets or as eager call arguments.
- In `traverse_NonEmptyChain_fn`-style code, `fn(item)` is not evaluated before the `Option` or `Result` discrimination.
- Final Matchless branches first and evaluates `fn(item)` only in the branch that needs it.

2. [ ] Inline small pure values, not only lambdas

Relevant code:

- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Extend published Matchless summaries from “tiny lambdas only” to “tiny pure values that are safe to duplicate or substitute”.
- Reuse the existing known-value projection folding in `simplifyKnownConditions` so `GetStructElement` and `GetEnumElement` collapse once a global or local pure value is substituted.
- Target the higher-order wrapper cases in Zafu where concrete `Traverse` and `Applicative` records should disappear from the caller IR instead of surviving as wrapper construction plus field selection.

Done when:

- Tiny closed record-valued or constructor-valued globals and locals disappear from caller IR.
- `GetStructElement` and `GetEnumElement` on known values fold away.
- Final Matchless no longer carries wrapper-record plumbing around `Traverse` and `Applicative` values when the concrete helper or branch logic can be exposed instead.

3. [ ] Generalize closure conversion of non-escaping function values

Relevant code:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/test/scala/dev/bosatsu/TypedExprTest.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Generalize `rewriteNonEscapingClosureBinding` so it can run for non-recursive local function values, not only recursive bindings.
- Rewrite direct-call-only local functions to take captured values as explicit extra parameters, then update call sites to pass those captures directly.
- Keep the escape analysis conservative: escaping functions should remain closures, and `Unit -> a` suspension boundaries should continue to block eager sharing or forced evaluation.

Done when:

- In TypedExpr, direct-call-only local functions gain explicit extra parameters for captured values.
- In Matchless, there are fewer `Lambda(captures = ...)` nodes, fewer `ClosureSlot`s, and more `Lambda(captures = Nil, ...)` or fully beta-reduced code.
- Escaping functions and suspension boundaries keep their current semantics.

4. [ ] Replace size bonuses with a richer shared benefit model

Relevant code:

- `core/src/main/scala/dev/bosatsu/TypedExprNormalization.scala`
- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Replace the current “small enough” TypedExpr gates and the separate Matchless demand score with a shared notion of payoff.
- Count concrete optimizer wins, such as dead-argument elimination, branch sinking, beta-reduction, projection folding, wrapper removal, and closure removal, then compare those wins against the code-size cost.
- Keep no-payoff cases conservative so harmless aliases and annotations stop perturbing final IR shape, but helpers that unlock real simplification still inline even when they are slightly above the old thresholds.

Done when:

- Source variants that differ only by harmless aliases, annotations, or tiny wrappers compile to the same or nearly the same final Matchless.
- Helpers that unlock branch sinking, beta-reduction, or projection folding inline even when they slightly exceed the old thresholds.
- Helpers that unlock none of those wins stay as calls, and total IR size stops growing in no-payoff cases.

5. [ ] Run the new passes to a small fixpoint

Relevant code:

- `core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala`
- `core/src/main/scala/dev/bosatsu/Matchless.scala`
- `core/src/test/scala/dev/bosatsu/MatchlessTests.scala`

Plan:

- Add a bounded stabilization loop around the new Matchless rewrite stack so the common sequence can complete: inline wrapper, expose pure value, fold projection, sink branch-only work, then simplify.
- Compare IR after each round and stop on structural equality or a small hard cap. The cap should be low enough to be predictable and high enough to let the new rewrites compose.
- Reject oscillation: if a round makes the IR worse or starts cycling, stop and fix the pass interaction before broadening the fixpoint.

Done when:

- Re-running the same optimizer pipeline is a no-op on the IR.
- Final Matchless does not retain dead lets, field projections, or wrapper calls that only disappear after one more cleanup round.
- Expression weight monotonically improves or stabilizes across rounds.

## Completed Items

- Item 1 completed.
  TypedExpr let-to-match sinking now recurses through `Annotation` and `Generic` wrappers, and Matchless `postLoweringCleanup` now sinks pure branch-only lets into `If` and `SwitchVariant` branches after inlining.
