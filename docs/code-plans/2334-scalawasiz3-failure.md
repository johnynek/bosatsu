# Code Plan #2334

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2334` scalawasiz3 failure
- Pending steps: `0`
- Completed steps: `2`
- Total steps: `2`

## Summary

Consume `scalawasiz3` `0.0.13` in Bosatsu, pin the reported `pathImplies` soundness counterexample with a deterministic regression, and keep the branch shippable by rerunning the focused SMT suites plus `scripts/test_basic.sh`.

## Current State

On this branch, `project/Dependencies.scala` now pins `dev.bosatsu` `%%%` `scalawasiz3` at `0.0.13`, so the shared solver dependency used by `core` and downstream modules resolves the published upstream trap fix referenced from the issue comments. `core/src/test/scala/dev/bosatsu/smt/SmtExprNormalizeAndPathImpliesTest.scala` now contains a deterministic regression for `goal = Lte(IntConst(1), Var(z))` with `facts = List(Lt(Var(v), Add(Vector(Var(z), Var(v)))))` that exercises the existing live `z3Implies` helper instead of depending on a ScalaCheck seed override. Verification completed on 2026-04-21: `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.SmtExprNormalizeAndPathImpliesTest -- --log=failure"`, `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.Z3ApiTest -- --log=failure"`, and `scripts/test_basic.sh` all passed.

## Problem

This issue existed because `main` resolved the broken `0.0.12` solver bundle, so `coreJVM` tests could fail nondeterministically when the existing `pathImplies` soundness property hit the known z3.wasm trap. Relying on the recorded ScalaCheck seed was not sufficient for a reviewable fix: Bosatsu needed to consume the published upstream `0.0.13` release, pin the exact Bosatsu-level regression, and clear the required gate `scripts/test_basic.sh` so the dependency upgrade stayed shippable.

## Steps

1. [x] `upgrade-scalawasiz3-and-pin-the-trap-case` Adopt the Fixed scalawasiz3 Release and Add a Deterministic Regression

Update `project/Dependencies.scala` so the shared `scalawasiz3` setting resolves `0.0.13`, which is the published upstream fix referenced from the issue. In the same slice, add a focused regression to `core/src/test/scala/dev/bosatsu/smt/SmtExprNormalizeAndPathImpliesTest.scala` for the exact issue input, using the existing live-solver helper so Bosatsu still exercises its own SMT encoding rather than reaching into `scalawasiz3` internals. If a small test-only helper extraction makes the case clearer, keep it local to the suite and leave production SMT logic unchanged.

#### Invariants

- Bosatsu keeps the same `pathImplies` contract: it may conservatively return `false`, but any `true` result must remain accepted by the live solver for the same normalized goal and facts.
- Every `core` target continues to resolve `scalawasiz3` from the single version pin in `project/Dependencies.scala`; there is no split JVM/JS version drift.
- The new regression is deterministic and directly covers the reported `Lte(1, z)` / `Lt(v, z + v)` case without relying on a ScalaCheck seed override.

#### Property Tests

- None recorded.

#### Assertion Tests

- Add a case-based regression in `SmtExprNormalizeAndPathImpliesTest` for `goal = Lte(IntConst(1), Var(z))` and `facts = List(Lt(Var(v), Add(Vector(Var(z), Var(v)))))`, asserting that the live `z3Implies` path no longer fails and that `pathImplies` remains sound for that exact input.

#### Completion Notes

Bumped the shared `scalawasiz3` version pin in `project/Dependencies.scala` from `0.0.12` to `0.0.13`, so every `core` target consumes the published upstream trap fix through the existing single dependency setting. Added the deterministic regression `pathImplies soundness handles the reported scalawasiz3 trap case` to `SmtExprNormalizeAndPathImpliesTest`, covering `goal = Lte(IntConst(1), Var(z))` with `facts = List(Lt(Var(v), Add(Vector(Var(z), Var(v)))))` via the existing live `z3Implies` helper. The regression asserts that the live solver now proves the implication and that `pathImplies` remains within its soundness contract, without changing production SMT logic.

2. [x] `reverify-smt-solver-integration` Re-run Focused SMT Coverage and the Required Gate

After the version bump and regression land, re-run the touched SMT suites first to confirm the issue is fixed at the Bosatsu integration boundary, then run the repo-required gate `scripts/test_basic.sh`. This keeps the branch shippable and catches any fallout from the shared dependency upgrade in `core`, including the existing `TypedExprRecursionCheck` and CLI-transitive test surfaces that rely on the same `core` artifact.

#### Invariants

- The dependency upgrade does not require Bosatsu production-code behavior changes beyond consuming the fixed upstream solver package.
- The exact regression case passes without special seeding, and the broader live-solver suites continue to parse and execute SMT scripts normally.
- The branch is not reviewable until `scripts/test_basic.sh` passes.

#### Property Tests

- None recorded.

#### Assertion Tests

- Run `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.SmtExprNormalizeAndPathImpliesTest -- --log=failure"`.
- Run `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.Z3ApiTest -- --log=failure"`.
- Run `scripts/test_basic.sh`.

#### Completion Notes

Completed on 2026-04-21. `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.SmtExprNormalizeAndPathImpliesTest -- --log=failure"` passed with 17 tests after the version bump and regression addition. `sbt -batch "coreJVM/testOnly dev.bosatsu.smt.Z3ApiTest -- --log=failure"` also passed with 6 tests, covering the directly adjacent live-solver integration surface that shares the upgraded dependency. The repo-required gate `scripts/test_basic.sh` then passed end-to-end.
