---
issue: 2274
priority: 3
touch_paths:
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ShowCommand.scala
  - core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala
  - core/src/main/scala/dev/bosatsu/CompileOptions.scala
  - core/src/main/scala/dev/bosatsu/cache/InferPhases.scala
  - core/src/main/scala/dev/bosatsu/cache/CompileCache.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/tool/ShowEdnRoundTripTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - docs/src/main/paradox/debugging_with_show.md
depends_on: []
estimated_size: M
generated_at: 2026-03-29T00:26:56Z
---

# Design Matchless Support and Pass Controls for `show`

_Issue: #2274 (https://github.com/johnynek/bosatsu/issues/2274)_

## Summary

Add a self-describing `--ir matchless` mode to both `show` entry points and refactor typed and Matchless lowering into named pass profiles so `show` can disable individual optimization stages for debugging while preserving the current EDN/JSON package wrapper.

## Context

`show` currently has two entry points: the root library-mode command in `core/src/main/scala/dev/bosatsu/library/Command.scala` and `tool show` in `core/src/main/scala/dev/bosatsu/tool_command/ShowCommand.scala`. Both select compiled `Package.Typed[Any]` values through `ShowSelection` and serialize them through `ShowEdn` to EDN or to the JSON projection of that EDN. Only the root library command exposes `--no-opt`, and that flag is coarse: it flips `CompileOptions.optimize` and therefore either runs or skips the whole typed post-typecheck pipeline.

Matchless lowering already exists and is used by evaluation and code generation. `Matchless.fromLet` lowers one typed let to `Matchless.Expr`, and `MatchlessFromTypedExpr.compile` lowers whole packages. The lowering path already has post-lowering cleanup in `Matchless.scala`, but it is not selectable from `show`: the current implementation always runs raw lowering followed by `hoistInvariantLoopLets` and `reuseConstructors`.

The typed pipeline is similarly coarse. `InferPhases.default.finishPackage` currently treats optimization as one boolean and executes three ordered stages when enabled:

1. `TypedExprLoopRecurLowering.lowerProgram`
2. `TypedExprNormalization.normalizeProgram`
3. `Package.discardUnused`

That is enough for the existing `--no-opt`, but not enough to inspect intermediate compiler stages.

## Problem

1. `show` can only emit typed-expression packages, so users cannot inspect the Matchless AST that evaluator and code generators actually consume.
2. The existing debug control is too coarse. `--no-opt` disables all typed post-typecheck work in one step, and there is no equivalent control over Matchless cleanup passes.
3. The output is not self-describing with respect to IR stage. Once `show` can emit either typedexpr or matchless, EDN and JSON consumers need a stable way to know which payload they received.
4. The two `show` entry points do not share the same IR and debug feature set today, which will become more confusing if only one grows Matchless support.

## Goals

1. Add `--ir typedexpr|matchless` to both `show` entry points, with `typedexpr` as the default.
2. Keep EDN as the default format and continue to support `--json` by projecting the same EDN structure to JSON.
3. Preserve the current package-selection model (`--package`, `--type`, `--value`, `--externals`) regardless of IR target.
4. Let users disable stable, top-level typed and matchless passes individually so they can inspect intermediate lowering states.
5. Keep default behavior unchanged for users who do not opt into new flags.

## Non-goals

1. This issue does not expose every micro-rewrite inside `TypedExprNormalization` as a separate CLI flag. The granularity for this issue is the stable top-level pipeline stages that already exist around typed compilation.
2. This issue does not change evaluator or codegen defaults. Matchless pass selection is added so `show` can render alternate views; normal runtime and codegen paths should continue to use the default optimized pipeline.
3. This issue does not require a general `EdnCodec[Matchless.Expr]` round-trip API. Renderable, parseable EDN and JSON are sufficient for `show`.
4. This issue does not try to eliminate every existing difference between library `show` and `tool show`; it only aligns them on IR selection and optimization-debug controls.

## Proposed Architecture

### 1. Add a shared show request model

Introduce a shared show configuration model that both CLI surfaces use before rendering:

1. `IrTarget`: `typedexpr` or `matchless`
2. `TypedPass`: `loop-recur-lowering`, `normalize`, `discard-unused`
3. `MatchlessPass`: `hoist-invariant-loop-lets`, `reuse-constructors`

CLI surface:

1. Add `--ir typedexpr|matchless`, defaulting to `typedexpr`.
2. Add repeatable `--disable-typed-pass <name>`.
3. Add repeatable `--disable-matchless-pass <name>`.
4. Keep the existing root-library `--no-opt` behavior as a compatibility alias for disabling all typed passes. New work should use the explicit pass flags.

Pass names should be validated by Decline so errors list the valid stable names.

### 2. Make typed optimization selectable by named stages

Replace the current boolean-only view of typed optimization with a small profile or set carried by `CompileOptions`, while keeping the existing helpers:

1. `CompileOptions.Default` keeps all typed passes enabled.
2. `CompileOptions.NoOptimize` becomes the profile with no typed passes.
3. `CompileOptions.TypeCheckOnly` remains the type-check-only mode and also carries no typed passes.

`InferPhases.default.finishPackage` should then execute typed passes in the existing fixed order, but only when each pass is enabled:

1. `TypedExprLoopRecurLowering.lowerProgram`
2. `TypedExprNormalization.normalizeProgram`
3. `Package.discardUnused`

This keeps semantics close to the current implementation while giving `show` stable pipeline boundaries to expose.

Because compiled artifacts are cached, the compile-cache key and phase identity must include the typed pass profile. Otherwise `show` could accidentally reuse artifacts built with a different debug configuration.

### 3. Make Matchless post-lowering cleanup selectable

Refactor `Matchless.fromLet` into explicit stages:

1. raw typedexpr-to-Matchless lowering
2. ordered post-lowering pass application

The current explicit post-lowering passes are already good first-class debug boundaries:

1. `hoistInvariantLoopLets`
2. `reuseConstructors`

Add a small `Matchless` lowering-options model so callers can request either the default pipeline or a subset. `MatchlessFromTypedExpr.compile` should accept that options model and continue to return the same compiled shape.

This refactor should not change default runtime behavior. Existing callers that do not pass options should continue to receive the current optimized Matchless output.

### 4. Keep selection on typed packages, then lower only selected defs

`ShowSelection` should stay typed-package based. The flow should remain:

1. compile or load typed packages with the requested typed pass profile
2. apply `ShowSelection` to those typed packages
3. if `--ir typedexpr`, render the selected typed packages directly
4. if `--ir matchless`, lower only the selected typed packages to Matchless and render those defs

This choice keeps `--package`, `--type`, `--value`, and `--externals` semantics stable even when later Matchless passes would change or inline expression bodies.

For matchless lowering, build a temporary `PackageMap` from the selected typed packages and lower with `Unit` scope tags for show purposes. Imported interface information is already preserved in selected packages, and `MatchlessFromTypedExpr.compile` already knows how to fall back to imported interfaces when constructor `DataRepr` is not available from local implementations. That makes on-demand lowering of the selected package set viable without needing a separate runtime namespace model.

### 5. Extend `ShowEdn` rather than inventing a second serializer

Keep one show serializer entry point and make the output self-describing.

Top-level `show` output should add:

1. `:ir` with value `typedexpr` or `matchless`
2. `:typed-passes` with the enabled typed pass names
3. `:matchless-passes` with the enabled matchless pass names
4. the existing `:interfaces` and `:packages`

Package forms should stay structurally familiar. The important rule is:

1. package metadata such as imports, exports, defined types, and externals remains the same
2. only `:defs` changes when `:ir matchless` is selected

Top-level defs should keep the current wrapper shape, for example `def` versus `defrec`, but the expression payload should be a Matchless encoding instead of a typed-expression encoding.

For EDN readability, Matchless rendering should stay as close as practical to the existing `ShowEdn` style:

1. prefer s-expression style over Scala case-class dumps
2. omit typed-expression-only type annotations from matchless expression nodes
3. render anonymous locals and mutable locals with stable synthetic names derived from their ids so the output is readable and diffable
4. only use keyword-heavy forms where a purely positional form would become ambiguous

JSON should continue to be generated by projecting the same EDN through `ShowEdn.ednToJson`, so EDN and JSON remain two views of one schema rather than two separately maintained serializers.

## Detailed Implementation Plan

1. Extend `CompileOptions` so typed post-typecheck work is represented as named passes instead of a single boolean, while preserving the existing convenience constructors.
2. Update `InferPhases.default` to run the typed passes conditionally in order, and update `CompileCache` key generation and phase identity to include the typed pass profile.
3. Refactor `Matchless.fromLet` so raw lowering and post-lowering cleanup are explicit stages, then thread a `Matchless` pass profile through `MatchlessFromTypedExpr.compile`.
4. Extend `ShowEdn` with Matchless expression encoders and a top-level `show` metadata block that records the IR target and enabled pass sets.
5. Update `library/Command.scala` to parse `--ir`, `--disable-typed-pass`, and `--disable-matchless-pass`, keep `--no-opt` as a compatibility alias, and pass the resulting profiles into typed compilation plus show rendering.
6. Update `tool_command/ShowCommand.scala` to parse the same new IR and debug flags and wire them into its compile-and-render path.
7. Keep `ShowSelection` in front of Matchless lowering so existing selection behavior remains stable.
8. Update the show documentation to describe the new IR target and pass-debug flags.

## Testing Strategy

1. Extend `ToolAndLibCommandTest.scala` with root `show --ir matchless` and `tool show --ir matchless` cases for both EDN and JSON.
2. Assert that Matchless JSON and EDN include self-describing IR metadata and still use the existing top-level `show` wrapper.
3. Add a typed-pass regression that proves disabling `discard-unused` keeps a helper value visible under `show`, while the default profile still removes it.
4. Add a typed-pass regression that proves disabling `loop-recur-lowering` or `normalize` changes the visible typedexpr shape on a small recursion or normalization fixture.
5. Add a Matchless-pass regression for `hoistInvariantLoopLets` using a loop example whose invariant let visibly moves in or out of the `WhileExpr` body.
6. Add a Matchless-pass regression for `reuseConstructors` using a fixture with repeated constructor creation so the shape difference is visible in `show --ir matchless`.
7. Extend `ShowEdnRoundTripTest.scala` so both typed and Matchless `show` output remain parseable as EDN and as JSON.
8. Add compile-options tests that prove cache and profile plumbing distinguishes different typed pass sets.

## Acceptance Criteria

1. Both `show` and `tool show` accept `--ir typedexpr|matchless`, and `typedexpr` remains the default.
2. `--json` continues to work for both IR targets, and EDN remains the default output format.
3. Top-level `show` output identifies the selected IR and the enabled typed and Matchless pass sets.
4. Matchless output preserves the existing package wrapper and selection behavior; only the expression bodies in `:defs` switch from typedexpr to Matchless.
5. The typed pipeline exposed to `show` supports at least `loop-recur-lowering`, `normalize`, and `discard-unused` as individually disableable stages.
6. The Matchless pipeline exposed to `show` supports at least `hoist-invariant-loop-lets` and `reuse-constructors` as individually disableable stages.
7. The default typed and Matchless pipelines continue to produce the same output shapes they produce today when no new flags are passed.
8. Compile-cache identity distinguishes typed pass profiles so debug builds cannot reuse incompatible cached artifacts.
9. New tests cover both CLI surfaces, both serialization formats, and at least one positive and negative example for typed-pass and Matchless-pass toggling.

## Risks And Mitigations

1. Risk: adding per-pass typed profiles changes compile-cache behavior or causes stale-cache mismatches.
Mitigation: include the pass profile in both the cache key payload and the phase identity, and add regression coverage for distinct profiles.
2. Risk: `show` JSON consumers may assume the old top-level field set.
Mitigation: preserve all existing fields and shapes, only add explicit metadata fields, and document the new keys in `debugging_with_show.md`.
3. Risk: Matchless rendering becomes hard to read if it mirrors raw case-class structure too literally.
Mitigation: keep the current `ShowEdn` style as the model, omit typed-only noise, and give anonymous and mutable locals stable readable names.
4. Risk: pass toggles grow stale as new Matchless cleanup passes are added later.
Mitigation: centralize pass registration in small enums or registries instead of sprinkling booleans through CLI code.
5. Risk: the scope expands into exposing every internal `TypedExprNormalization` rewrite.
Mitigation: keep this issue at stable stage boundaries and treat finer-grained toggles as a separate follow-up if they are still needed after this debug surface lands.

## Rollout Notes

1. Land the refactor with defaults that preserve current behavior for both typed `show` and existing runtime and codegen Matchless consumers.
2. Document the new `--ir` and pass-disable flags in `docs/src/main/paradox/debugging_with_show.md`, including one example for inspecting raw Matchless output.
3. Keep the existing root `--no-opt` flag working during the transition, but describe the explicit pass flags as the preferred debug interface.
4. If any regression appears, rollback is straightforward because the new plumbing can fall back to the current default pass profiles without removing the serializer or CLI surface.
