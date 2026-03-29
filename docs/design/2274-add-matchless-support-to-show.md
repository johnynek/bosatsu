---
issue: 2274
priority: 3
touch_paths:
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ShowCommand.scala
  - core/src/main/scala/dev/bosatsu/tool/Output.scala
  - core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala
  - core/src/main/scala/dev/bosatsu/CompileOptions.scala
  - core/src/main/scala/dev/bosatsu/cache/InferPhases.scala
  - core/src/main/scala/dev/bosatsu/cache/CompileCache.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/tool/ShowEdnRoundTripTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - docs/src/main/paradox/debugging_with_show.md
depends_on: []
estimated_size: M
generated_at: 2026-03-29T02:32:10Z
---

# Design matchless support and pass controls for `show`

_Issue: #2274 (https://github.com/johnynek/bosatsu/issues/2274)_

## Summary

Add `--ir matchless` plus explicit typedexpr and Matchless pass toggles to both `show` entry points while preserving today’s typedexpr defaults and extending the `show` schema so Matchless output stays readable in EDN and JSON.

## Context

`show` currently renders selected `Package.Typed[Any]` values through `ShowEdn`. Library `show` exposes only a coarse `--no-opt` switch, `tool show` has no equivalent, and both commands can only emit typedexpr output. Matchless lowering already exists in `Matchless.fromLet` and `MatchlessFromTypedExpr.compile`, and the lowering path already applies two cleanup passes by default: `hoistInvariantLoopLets` and `reuseConstructors`.

Typed post-typecheck work is similarly coarse. `InferPhases.default.finishPackage` currently treats optimization as one boolean and, when enabled, runs three ordered stages:

1. `TypedExprLoopRecurLowering.lowerProgram`
2. `TypedExprNormalization.normalizeProgram`
3. `Package.discardUnused`

That is enough for the current `--no-opt`, but it is not enough to inspect intermediate compiler states or to compare typedexpr output against Matchless output at different optimization layers.

## Problem

1. `show` cannot render the Matchless AST that evaluator and codegen paths actually consume.
2. The current debug surface is too coarse. `--no-opt` disables all typed post-typecheck work at once, and there is no equivalent control for Matchless cleanup passes.
3. Once `show` can emit multiple IRs, the output needs to be self-describing so EDN and JSON consumers can tell which payload they received.
4. The two `show` entry points should expose the same IR-selection and pass-control model to avoid diverging debugging workflows.

## Goals

1. Add `--ir typedexpr|matchless` to both `show` entry points, defaulting to `typedexpr`.
2. Keep EDN as the default format and keep `--json` as a projection of the same logical `show` data.
3. Preserve existing selection semantics for `--package`, `--type`, `--value`, and `--externals`.
4. Let users disable stable typedexpr and Matchless passes individually so intermediate compiler states are inspectable.
5. Preserve current behavior when no new flags are passed.

## Non-goals

1. This issue does not expose every internal rewrite inside `TypedExprNormalization` as a separate CLI flag. The scope is the existing stable top-level stages around typed compilation.
2. This issue does not change evaluator or codegen defaults. Those paths should continue to use the current default Matchless pipeline.
3. This issue does not require a general `EdnCodec[Matchless.Expr]` round-trip API; it only needs readable, parseable `show` output in EDN and JSON.
4. This issue does not change package selection rules or export semantics.

## Proposed Architecture

### 1. Add a shared show request and render model

Both CLI surfaces should build the same internal show request:

1. IR target: `typedexpr` or `matchless`
2. Enabled typed passes
3. Enabled Matchless passes
4. Package/type/value/external selectors
5. Output format and any existing wrapper options

A small render payload should then carry the selected packages, interfaces, IR metadata, and whichever def representation is being shown. `Output.ShowOutput` should carry that payload rather than only raw `Package.Typed[Any]` lists so `Output.report`, `ShowEdn.showDoc`, and `ShowEdn.showJson` all operate on the same schema.

CLI surface details:

1. Add `--ir typedexpr|matchless`.
2. Add repeatable `--disable-typed-pass <name>`.
3. Add repeatable `--disable-matchless-pass <name>`.
4. Keep library `show --no-opt` as a compatibility alias for disabling all typed passes.
5. Reject ambiguous combinations such as `--no-opt` plus explicit typed-pass disables, and reject Matchless-pass flags unless `--ir matchless` is selected.

### 2. Make typed post-typecheck passes explicit in `CompileOptions`

Replace the current boolean-only typed optimization view with an explicit set or profile of enabled typed passes while preserving the existing convenience constructors:

1. `CompileOptions.Default` keeps all typed passes enabled.
2. `CompileOptions.NoOptimize` keeps none of them enabled.
3. `CompileOptions.TypeCheckOnly` remains typecheck-only and also carries no typed passes.

`InferPhases.default.finishPackage` should continue to run passes in the current fixed order, but only when each pass is enabled:

1. `TypedExprLoopRecurLowering.lowerProgram`
2. `TypedExprNormalization.normalizeProgram`
3. `Package.discardUnused`

Because compiled artifacts are cached, cache identity must distinguish typed-pass profiles. `CompileCache.keyHashValue` cannot continue to hash only `optimize` once the pass model expands.

### 3. Make Matchless lowering and cleanup selectable

Refactor Matchless lowering into explicit stages:

1. raw typedexpr-to-Matchless lowering
2. ordered Matchless cleanup passes

The current cleanup passes are already good debug boundaries:

1. `hoistInvariantLoopLets`
2. `reuseConstructors`

Add a Matchless lowering-options model so `show` can request the default pipeline or a subset. `Matchless.fromLet` and `MatchlessFromTypedExpr.compile` should preserve current behavior when callers do not pass explicit options, which keeps evaluator and codegen behavior unchanged.

### 4. Keep selection typed-package based, then lower only what `show` needs

`ShowSelection` should remain typed-package based. The flow should be:

1. compile or load typed packages with the requested typed-pass profile
2. apply `ShowSelection` to those typed packages
3. if `--ir typedexpr`, render the selected typed packages directly
4. if `--ir matchless`, build a temporary `PackageMap` from the selected typed packages, lower only those selected defs with the requested Matchless-pass profile, and render that lowered form

This keeps `--package`, `--type`, `--value`, and `--externals` semantics stable even when later Matchless passes inline or restructure expression bodies. The existing imported-interface fallback in `MatchlessFromTypedExpr.compile` is sufficient for selected-package lowering, so `show` does not need a separate runtime-only namespace model.

### 5. Extend the `show` schema rather than inventing a second serializer

Keep one logical `show` schema and make it self-describing. The top-level wrapper should continue to be `show`, but add IR metadata:

1. `:ir` with value `typedexpr` or `matchless`
2. `:typed-passes` with the enabled typed pass names
3. `:matchless-passes` with the enabled Matchless pass names
4. the existing `:interfaces` and `:packages`

Package-level metadata such as imports, exports, defined types, and externals should stay structurally familiar. The main IR-dependent field is `:defs`.

For Matchless EDN rendering:

1. prefer the same s-expression style already used by `ShowEdn`
2. omit typedexpr-only type annotations
3. render anonymous and mutable locals with stable synthetic names so output is readable and diffable
4. keep forms close to the existing lowered typedexpr style where possible, rather than exposing raw Scala case-class structure

JSON should continue to be generated by projecting the same EDN structure through `ShowEdn.ednToJson` so EDN and JSON remain two views of one schema.

## Detailed Implementation Plan

1. Introduce the shared show request/payload model and update `Output.ShowOutput` so both CLI surfaces feed one renderer contract.
2. Update `library.Command.scala` to parse `--ir`, `--disable-typed-pass`, and `--disable-matchless-pass`, while keeping `--no-opt` as a compatibility alias.
3. Update `tool_command.ShowCommand.scala` to parse the same new IR and pass flags and pass them through the compile-and-render flow.
4. Extend `CompileOptions` so typed post-typecheck work is represented as named passes instead of a single boolean.
5. Update `InferPhases.default.finishPackage` to run typed passes conditionally in order and update `CompileCache` key hashing so distinct typed-pass profiles do not collide.
6. Refactor `Matchless.fromLet` so raw lowering and post-lowering cleanup are explicit stages, then thread Matchless pass options through `MatchlessFromTypedExpr.compile`.
7. Keep `ShowSelection` ahead of Matchless lowering so selectors continue to behave against typed compiled packages.
8. Extend `ShowEdn` with Matchless expression encoders and top-level IR metadata, and keep JSON as the EDN projection.
9. Update `docs/src/main/paradox/debugging_with_show.md` with examples for `--ir matchless` and the new pass-control flags.

## Testing Strategy

1. Extend `ToolAndLibCommandTest.scala` with library `show --ir matchless` and `tool show --ir matchless` cases for both EDN and JSON.
2. Add coverage that the default path still emits typedexpr output and that existing typedexpr behavior remains unchanged when no new flags are passed.
3. Add typed-pass regressions proving `discard-unused` can be disabled to keep a helper visible under `show`.
4. Add typed-pass regressions proving disabling `loop-recur-lowering` or `normalize` changes the visible typedexpr shape on a focused fixture.
5. Add Matchless-pass regressions for `hoistInvariantLoopLets` and `reuseConstructors` so the structural differences are visible in `show --ir matchless`.
6. Extend `ShowEdnRoundTripTest.scala` so both typedexpr and Matchless `show` output remain parseable as EDN and as JSON.
7. Add Matchless-level tests that exercise the new lowering-options plumbing without changing the default pipeline.
8. Add cache regressions proving distinct typed-pass profiles produce distinct cache identities.

## Acceptance Criteria

1. Both `show` entry points accept `--ir typedexpr|matchless`, and `typedexpr` remains the default.
2. `--json` continues to work for both IR targets, and EDN remains the default output format.
3. Top-level `show` output identifies the selected IR and the enabled typed and Matchless pass sets.
4. Matchless output preserves the existing package wrapper and selection behavior; the IR-specific change is in `:defs`.
5. The typed pipeline exposed to `show` supports at least `loop-recur-lowering`, `normalize`, and `discard-unused` as individually disableable stages.
6. The Matchless pipeline exposed to `show` supports at least `hoist-invariant-loop-lets` and `reuse-constructors` as individually disableable stages.
7. The default typed and Matchless pipelines produce the same shapes they produce today when no new flags are passed.
8. Compile-cache identity distinguishes typed-pass profiles so debug builds cannot reuse incompatible cached artifacts.
9. New tests cover both CLI surfaces, both serialization formats, and positive pass-toggle cases at both the typedexpr and Matchless layers.

## Risks And Mitigations

1. Risk: per-pass typed profiles could cause cache collisions or stale artifact reuse.
Mitigation: make the enabled typed-pass set part of `CompileOptions` and `CompileCache` key hashing, and add regression coverage for profile separation.
2. Risk: Matchless rendering becomes hard to read if it mirrors raw case-class structure too literally.
Mitigation: treat the current `ShowEdn` style as the readability baseline, omit typed-only noise, and use stable synthetic names for anonymous locals.
3. Risk: output consumers may assume the old top-level field set.
Mitigation: preserve the existing `show` wrapper and existing package fields, and add only explicit metadata fields rather than replacing the schema.
4. Risk: pass controls grow stale as new Matchless cleanup passes are added later.
Mitigation: centralize pass registration in small enums or registries rather than scattering booleans through CLI code.
5. Risk: the issue scope expands into toggling every normalization rewrite individually.
Mitigation: keep this PR at stable stage boundaries and treat finer-grained normalization controls as a separate follow-up if they are still needed.

## Rollout Notes

1. Land the refactor with defaults that preserve current typed `show`, evaluator, and codegen behavior.
2. Document `--ir matchless` and the pass-disable flags in `debugging_with_show.md`, including at least one example of inspecting raw Matchless output.
3. Keep library `show --no-opt` working during the transition, but document the explicit pass flags as the preferred long-term debug interface.
4. If regressions appear, rollback is straightforward because the new plumbing can fall back to the current default typed and Matchless pass profiles without changing the public `show` wrapper.
