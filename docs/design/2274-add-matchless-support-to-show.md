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
  - core/src/main/scala/dev/bosatsu/MatchlessGlobalInlining.scala
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

The Matchless roadmap now also includes a namespace-level global inlining phase. If `show` is meant to debug Matchless optimizations rather than only raw lowering, it needs to be able to show packages before and after that inliner as well.

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
4. Let users disable stable typedexpr and Matchless passes individually so intermediate compiler states are inspectable, including the Matchless global inliner.
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

### 3. Make Matchless lowering, local cleanup, and global inlining selectable

Refactor Matchless lowering into explicit stages:

1. raw typedexpr-to-Matchless lowering
2. ordered local Matchless cleanup passes
3. optional namespace-level Matchless global inlining

The current cleanup passes are already good debug boundaries:

1. `hoistInvariantLoopLets`
2. `reuseConstructors`

Add a Matchless pass-options model so `show` can request the default pipeline or a subset. The local pass profile should cover `hoistInvariantLoopLets` and `reuseConstructors`, and the namespace-level profile should also be able to enable or disable `MatchlessGlobalInlining`.

`Matchless.fromLet` should stay responsible for raw lowering plus local cleanup. Global inlining should remain a separate coordinator over the compiled Matchless namespace, matching the design in issue #2185 rather than folding cross-package inlining back into raw lowering. Existing non-`show` callers should preserve current behavior when they do not pass explicit options, which keeps evaluator and codegen behavior unchanged.

### 4. Keep selection typed-package based, then lower only what `show` needs

`ShowSelection` should remain typed-package based. The flow should be:

1. compile or load typed packages with the requested typed-pass profile
2. apply `ShowSelection` to those typed packages
3. if `--ir typedexpr`, render the selected typed packages directly
4. if `--ir matchless` without global inlining, build a temporary `PackageMap` from the selected typed packages, lower only those selected defs with the requested local Matchless-pass profile, and render that lowered form
5. if `--ir matchless` with global inlining enabled, build a temporary compilation namespace rooted at the selected defs, include any reachable implementation packages the inliner may consult, run the Matchless global inliner on that namespace, and then render only the originally requested packages and defs

This keeps `--package`, `--type`, `--value`, and `--externals` semantics stable even when later Matchless passes inline or restructure expression bodies. The existing imported-interface fallback in `MatchlessFromTypedExpr.compile` is sufficient for raw selected-package lowering. When global inlining is enabled, `show` needs implementation bodies in the temporary namespace for any packages that may be inlined; interface-only dependencies remain visible as uninlined `Global(...)` references.

### 5. Extend the `show` schema rather than inventing a second serializer

Keep one logical `show` schema and make it self-describing. The top-level wrapper should continue to be `show`, but the fields differ slightly by IR:

1. `:ir` with value `typedexpr` or `matchless`
2. `:typed-passes` with the enabled typed pass names
3. `:matchless-passes` only when `:ir matchless`
4. `:interfaces` only when `:ir typedexpr`
5. `:packages` for both IRs

The typedexpr view can stay close to today’s schema. The Matchless view should be explicitly treated as a value-only, untyped view. It should not claim to have typed interfaces or per-package type definitions, because Matchless does not carry that information. A Matchless package wrapper can still keep contextual metadata such as package name and selected import/export names, but typed sections such as `:interfaces` and `:types` should be omitted. Top-level defs sometimes still reveal function shape when they lower to lambdas, but that is structural information, not a type or interface.

For Matchless EDN rendering:

1. prefer the same s-expression style already used by `ShowEdn`
2. omit typedexpr-only type annotations
3. render `LocalAnon(n)` as `anon$n` and `LocalAnonMut(n)` as `mut$n`, reusing the shared allocator id directly instead of inventing a second renumbering scheme
4. keep forms close to the existing lowered typedexpr style where possible, rather than exposing raw Scala case-class structure

JSON should continue to be generated by projecting the same EDN structure through `ShowEdn.ednToJson` so EDN and JSON remain two views of one schema.

## Detailed Implementation Plan

1. Introduce the shared show request/payload model and update `Output.ShowOutput` so both CLI surfaces feed one renderer contract.
2. Update `library.Command.scala` to parse `--ir`, `--disable-typed-pass`, and `--disable-matchless-pass`, while keeping `--no-opt` as a compatibility alias.
3. Update `tool_command.ShowCommand.scala` to parse the same new IR and pass flags and pass them through the compile-and-render flow.
4. Extend `CompileOptions` so typed post-typecheck work is represented as named passes instead of a single boolean.
5. Update `InferPhases.default.finishPackage` to run typed passes conditionally in order and update `CompileCache` key hashing so distinct typed-pass profiles do not collide.
6. Refactor `Matchless.fromLet` so raw lowering and local post-lowering cleanup are explicit stages, then thread local Matchless pass options through `MatchlessFromTypedExpr.compile`.
7. Add a show-facing path for optional `MatchlessGlobalInlining` that runs on a temporary namespace rooted at the selected defs and trims rendered output back to the original typed selection after rewriting.
8. Keep `ShowSelection` ahead of Matchless lowering so selectors continue to behave against typed compiled packages even when Matchless global inlining is enabled later in the pipeline.
9. Extend `ShowEdn` with separate typedexpr and Matchless renderers, with `:matchless-passes` only in Matchless output and with typed interfaces/types omitted from the Matchless view.
10. Update `docs/src/main/paradox/debugging_with_show.md` with examples for `--ir matchless`, the new pass-control flags, and at least one before/after global-inline inspection example.

## Testing Strategy

1. Extend `ToolAndLibCommandTest.scala` with library `show --ir matchless` and `tool show --ir matchless` cases for both EDN and JSON.
2. Add coverage that the default path still emits typedexpr output and that existing typedexpr behavior remains unchanged when no new flags are passed.
3. Add typed-pass regressions proving `discard-unused` can be disabled to keep a helper visible under `show`.
4. Add typed-pass regressions proving disabling `loop-recur-lowering` or `normalize` changes the visible typedexpr shape on a focused fixture.
5. Add Matchless-pass regressions for `hoistInvariantLoopLets` and `reuseConstructors` so the structural differences are visible in `show --ir matchless`.
6. Add a Matchless global-inlining regression that uses a cross-package helper and proves `show --ir matchless` can display the pre-inline and post-inline shapes.
7. Extend `ShowEdnRoundTripTest.scala` so both typedexpr and Matchless `show` output remain parseable as EDN and as JSON, with Matchless output omitting typed-only wrapper sections.
8. Add Matchless-level tests that exercise the new lowering-options plumbing without changing the default pipeline.
9. Add cache regressions proving distinct typed-pass profiles produce distinct cache identities.

## Acceptance Criteria

1. Both `show` entry points accept `--ir typedexpr|matchless`, and `typedexpr` remains the default.
2. `--json` continues to work for both IR targets, and EDN remains the default output format.
3. Top-level `show` output identifies the selected IR, always reports the enabled typed-pass set, and reports `:matchless-passes` only when the emitted IR is Matchless.
4. Matchless output preserves the original package and def selection behavior, but it is explicitly a value-only view: typed interfaces and type-definition sections are omitted.
5. The typed pipeline exposed to `show` supports at least `loop-recur-lowering`, `normalize`, and `discard-unused` as individually disableable stages.
6. The Matchless pipeline exposed to `show` supports at least `hoist-invariant-loop-lets`, `reuse-constructors`, and `global-inlining` as individually disableable stages.
7. Matchless local names render with a stable scheme derived directly from the shared allocator ids, so `LocalAnon` and `LocalAnonMut` are distinguishable without extra renumbering.
8. The default typed and Matchless pipelines produce the same shapes they produce today when no new flags are passed.
9. Compile-cache identity distinguishes typed-pass profiles so debug builds cannot reuse incompatible cached artifacts.
10. New tests cover both CLI surfaces, both serialization formats, and positive pass-toggle cases at both the typedexpr and Matchless layers, including the Matchless global inliner.

## Risks And Mitigations

1. Risk: per-pass typed profiles could cause cache collisions or stale artifact reuse.
Mitigation: make the enabled typed-pass set part of `CompileOptions` and `CompileCache` key hashing, and add regression coverage for profile separation.
2. Risk: Matchless rendering becomes hard to read if it mirrors raw case-class structure too literally.
Mitigation: treat the current `ShowEdn` style as the readability baseline, omit typed-only noise, and use stable synthetic names for anonymous locals.
3. Risk: output consumers may assume the old top-level field set.
Mitigation: preserve the existing `show` wrapper and existing package fields, and add only explicit metadata fields rather than replacing the schema.
4. Risk: Matchless global inlining needs implementation bodies outside the immediate user selection, which can blur what `show` is actually inspecting.
Mitigation: root the temporary Matchless namespace at the original typed selection, expand only to the reachable implementation bodies the inliner needs, and trim the rendered output back to the original requested packages and defs.
5. Risk: the issue scope expands into toggling every normalization rewrite individually.
Mitigation: keep this PR at stable stage boundaries and treat finer-grained normalization controls as a separate follow-up if they are still needed.

## Rollout Notes

1. Land the refactor with defaults that preserve current typed `show`, evaluator, and codegen behavior.
2. Document `--ir matchless` and the pass-disable flags in `debugging_with_show.md`, including examples of inspecting both raw Matchless output and post-global-inlining output.
3. Keep library `show --no-opt` working during the transition, but document the explicit pass flags as the preferred long-term debug interface.
4. If regressions appear, rollback is straightforward because the new plumbing can fall back to the current default typed and Matchless pass profiles without changing the public `show` wrapper.
