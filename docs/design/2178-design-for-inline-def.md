---
issue: 2178
priority: 3
touch_paths:
  - docs/design/2178-design-for-inline-def.md
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/DefStatement.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/Program.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/ExportedName.scala
  - core/src/main/scala/dev/bosatsu/Referant.scala
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/tool/ShowEdn.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/main/scala/dev/bosatsu/codegen/CompilationSource.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibraryWithDeps.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessInterfaceTest.scala
  - core/src/test/scala/dev/bosatsu/tool/CompileCacheTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-16T17:23:05Z
---

# Top-level inline def lowered through Matchless

_Issue: #2178 (https://github.com/johnynek/bosatsu/issues/2178)_

## Summary

Add opt-in top-level `inline def` syntax, persist inline metadata through interfaces and package serialization, and inline fully saturated calls during topo-ordered Matchless lowering so cross-package control-flow helpers avoid eager argument evaluation.

## Context

Bosatsu already gets some package-local inlining from `TypedExprNormalization`, but that behavior is heuristic and stops at package boundaries. Issue #2178 asks for an explicit source-level way to mark control-flow helpers so downstream lowering can avoid eagerly evaluating arguments that are only needed in some branches.

The current Matchless pipeline lowers each package independently and preserves cross-package calls as `Global(from, pack, name)`. That means imported helpers can never be substituted once lowering starts, even when the implementation is available through decoded library packages.

## Goals

1. Add explicit `inline def` syntax for Bosatsu top-level definitions.
2. Make fully saturated direct calls to inline defs lower without eagerly evaluating every actual argument ahead of control flow.
3. Support inlining across package and library boundaries when dependency implementations are available.
4. Preserve current behavior for ordinary `def`.
5. Keep serialized artifacts backward compatible.

## Non-goals

1. General cross-package inlining for ordinary defs. That remains the broader work in #1315.
2. Nested or expression-local `inline def` in this PR.
3. Inlining partial applications or arbitrary higher-order uses of inline defs.
4. Serializing full Matchless bodies into interfaces.
5. Solving source-provenance/debug mapping for moved code. That stays aligned with the source-info work in #1778.

## Syntax and surface contract

V1 should support `inline def` only at the top-level statement layer:

```bosatsu
inline def if_else(cond, on_true, on_false):
  if cond:
    on_true
  else:
    on_false
```

Contract:

1. `inline def` is accepted only as a top-level statement.
2. It type-checks exactly like an ordinary `def` and exports as an ordinary value.
3. Fully saturated direct calls are candidates for Matchless-time substitution.
4. Unsaturated uses, passing the function as a value, or unresolved dependency bodies keep ordinary `Global` and `App` lowering.
5. Non-tail self-recursive inline defs are rejected. Tail-recursive inline defs are allowed once loop lowering has converted them to loop form.

Restricting V1 to top-level defs avoids having to tag every local `Let` shape throughout the typed AST while still covering the motivating cross-package control-flow use case.

## Data model

### 1. Parsed and typed package metadata

Introduce a top-level-only inline marker in the source AST, then preserve the resulting bindables in typed programs.

Proposed in-memory shape:

1. Parse `inline def` as a distinct top-level statement form or a top-level-only modifier on `Statement.Def`.
2. Extend `Program` with `inlineDefs: SortedSet[Bindable]`.
3. Preserve `inlineDefs` through normalization, tree shaking, filtering, voiding, and round-trip helpers.
4. No general SCC validation is needed. Bosatsu name graphs are DAGs by construction apart from self recursion, so the inline-specific validation only needs to reject remaining non-tail self recursion after loop lowering.

### 2. Public interface metadata

Inline-ness of exported values must be visible in interfaces so downstream recompilation and cache keys see the change even when the value type stays identical.

Proposed interface model:

1. Extend `Referant.Value` to carry an `isInline` flag.
2. Update `ExportedName.buildExports` so exported bindables found in `Program.inlineDefs` become inline-valued referants.
3. Keep `TypeEnv` behavior unchanged. Inline-ness is lowering metadata, not a type-system property.

This keeps the public API explicit without serializing implementation bodies into interfaces.

### 3. Serialized package metadata

Implementation packages need the full inline-def set so decoded libraries can inline private and public inline defs inside their own package graphs.

Proposed protobuf changes:

1. Add `repeated int32 inline_defs` to `proto.Package`, storing top-level bindable ids.
2. Add a backward-compatible representation for inline exported values in `proto.Referant`, for example a new `ValueReferant` message carrying `type` plus `is_inline`, while continuing to accept the legacy scalar value arm during decode.
3. Decode old artifacts as non-inline by default.
4. Keep interface bodies absent; only the exported flag is serialized there.

## Matchless architecture

### 1. Two-phase pipeline

Keep the initial Matchless conversion fully parallel, then add a second phase that rewrites inline calls after raw Matchless bodies already exist.

Proposed shape:

1. Keep `MatchlessFromTypedExpr.compile` as the raw conversion step so every let is lowered to Matchless in full parallelism.
2. Add an `InlineEnv[K]` keyed by `(scopeKey, packageName, bindable)` that returns an already-rewritten inline Matchless body plus recovered arity.
3. Add a second pass over compiled Matchless output that runs in topo order of packages and rewrites calls to inline defs using `InlineEnv`.
4. Within each package, rewrite inline defs first so later lets in the same package can inline them too.

This solves two problems at once:

1. Forward references to later inline defs in the same package still inline.
2. Imported inline defs can inline once their dependency packages have already gone through the post-lowering rewrite phase.

### 2. Inline application semantics

Direct substitution needs different behavior from the current `Matchless.applyArgs` helper.

`applyArgs` deliberately introduces outer `Let`s for every argument. That preserves sharing, but it also makes all actual arguments eager before any branch executes, which is exactly what `inline def` is trying to avoid.

Add a separate `Matchless.inlineApplyArgs` for inline calls:

1. Resolve the callee to `recoverTopLevelLambda(compiledInlineExpr)`.
2. Require a fully saturated direct call before using the inline path.
3. Alpha-rename lambda binders to avoid capture.
4. Substitute actual arguments directly into the body instead of wrapping them in eager outer `Let`s.
5. Reuse existing branch-push and closure-slot rewriting machinery where possible.

This keeps branch arguments inside the `If`, `SwitchVariant`, or similar control-flow node where they are actually needed.

Trade-off:

1. If an inline parameter is referenced multiple times, the argument IR can be duplicated.
2. That is acceptable in V1 because inline is explicit and opt-in.
3. Any future heuristic for sharing should be a follow-up and must not reintroduce eager evaluation ahead of control flow.

### 3. Post-lowering inlining sketch

The clearer proposal is:

1. Phase A runs exactly as the compiler does today: lower all lets to raw Matchless in full parallelism, with inline defs still called through `Global`.
2. Build the inline candidate table from typed-program metadata (`Program.inlineDefs`) plus interface metadata for exported inline defs.
3. Phase B walks packages in topo order and rewrites raw Matchless bodies:
   - Process packages by topo layer so independent packages still rewrite in parallel.
   - For one package, collect the lets whose bindable is in `Program.inlineDefs`.
   - Order those local inline defs by same-package `Global` references, ignoring any self edge. This is a straight topo order because Bosatsu's name graph is already acyclic apart from self recursion.
   - Rewrite each local inline body using already-processed dependency inline defs plus already-rewritten local inline defs.
   - Publish the rewritten local inline defs into `InlineEnv`.
   - Rewrite all lets in the package using the same environment.
4. `Matchless.inlineApplyArgs` is used only in Phase B when the callee resolves to an inline body and the call is fully saturated.
5. Tail-recursive inline defs work because `TypedExprLoopRecurLowering` has already rewritten them into loop form before Matchless lowering, so the post-pass just inlines a `WhileExpr`-shaped body.
6. If an inline def body still contains a direct self call after loop lowering, Phase B reports it as an unsupported non-tail recursive inline def instead of recursing forever.

### 4. Dependency ordering and remaining parallelism

The point of the two-phase approach is that only the inlining rewrite needs ordering. Raw Matchless lowering can remain fully parallel.

The remaining ordering work is therefore limited to Phase B:

1. `CompilationSource.packageMapSrc.compiled` should iterate `PackageMap.topoSort.layers`.
2. `DecodedLibraryWithDeps.compiled` should iterate `CompilationNamespace.topoSort.layers` so cross-library package dependencies are honored.
3. Packages inside the same topo layer still rewrite in parallel.
4. Only packages on actual dependency chains lose parallelism during the post-pass.

This keeps the ordering change localized to the namespace builders rather than the raw Matchless conversion itself or every backend.

### 5. Cross-library lookup

Decoded libraries already carry implementation packages in addition to interfaces. That is enough to inline without putting Matchless into interfaces.

Resolution rule:

1. Interfaces tell the compiler whether an imported exported value is inline.
2. Implementation packages provide the typed body that can be lowered and cached into `InlineEnv`.
3. If a context only has interfaces and no implementation package, lowering keeps the ordinary `Global` form.

## Implementation plan

1. Add top-level `inline def` parsing and rendering in the statement layer.
2. Extend `Program` with `inlineDefs` and thread it through package helpers, normalization, tree shaking, and typed-package utilities.
3. Add a typed-package validation that rejects only non-tail self recursion that survives loop lowering.
4. Extend exported-value metadata so interfaces record whether an exported value is inline.
5. Update `TypedAst.proto` and `ProtoConverter` to round-trip both package-level inline defs and interface-level inline export flags with backward-compatible decode.
6. Keep the current parallel Matchless conversion and add a second inlining pass over compiled Matchless bodies.
7. Add `Matchless.inlineApplyArgs` and use it during the post-lowering rewrite of fully saturated direct applications to inline globals.
8. Update `CompilationSource` and `DecodedLibraryWithDeps` so the post-lowering inlining pass runs by topo layer over already-lowered packages.
9. Keep evaluator and backend codegen unchanged except for consuming the new Matchless output they already receive.

## Testing plan

1. Parser tests for top-level `inline def` round-tripping.
2. Parser or package tests confirming nested `inline def` is rejected in V1.
3. Package tests confirming non-tail self-recursive inline defs are rejected and tail-recursive inline defs remain allowed.
4. Source-converter or package-shape tests confirming `Program.inlineDefs` is populated correctly.
5. Proto round-trip tests for `inline_defs` on packages and inline export flags on interfaces.
6. Backward-compat decode tests for old artifacts with no inline metadata.
7. Interface-hash tests showing that changing a public `def` to `inline def` changes serialized interface bytes and cache identity.
8. Matchless structural tests using an inline control-flow helper such as `if_else` and asserting actual branch arguments stay under the branch node instead of being hoisted into eager lets.
9. Cross-package tests confirming imported inline defs inline even when defined later in source order.
10. Cross-library codegen tests confirming decoded dependency libraries inline exported inline defs in downstream compilation.
11. Regression tests confirming ordinary defs and interface-only constructor import behavior remain unchanged.

## Acceptance criteria

1. Top-level `inline def` parses and round-trips, non-tail self-recursive inline defs are rejected, and tail-recursive inline defs remain allowed.
2. Typed programs preserve inline-def metadata, and package filtering, normalization, and tree shaking do not drop it accidentally.
3. Exported inline defs are marked in interfaces, so interface bytes and dependency hashes change when a public def switches between ordinary and inline.
4. Package and library serialization round-trip inline metadata, and older artifacts decode as non-inline.
5. Fully saturated direct calls to inline defs are substituted during Matchless lowering without eagerly evaluating all actual arguments ahead of control-flow branches.
6. Imported inline defs inline across package boundaries inside one compilation and across decoded library dependencies.
7. Unsaturated uses and ordinary defs continue to use the current `Global` and `App` lowering paths.
8. Raw Matchless lowering remains fully parallel, and only the post-lowering inline rewrite is ordered by topo layers.

## Risks and mitigations

1. Risk: inline substitution can duplicate work or increase code size when a parameter is used multiple times.
Mitigation: keep the feature explicit and opt-in, and accept duplication in V1 rather than weakening the laziness guarantee.

2. Risk: self-recursive inline defs can make the post-pass recurse forever if recursion survives loop lowering.
Mitigation: allow tail-recursive defs after loop lowering and reject only bodies that still contain a direct self call.

3. Risk: if inline-ness is omitted from interfaces, caches can go stale because downstream generated code changes while the exported type does not.
Mitigation: make exported inline-ness part of interface identity.

4. Risk: less parallelism can slow large builds with long dependency chains.
Mitigation: preserve parallelism inside each topo layer and only serialize along real import edges.

5. Risk: source attribution for inlined code remains coarse until source-info work lands.
Mitigation: keep this design compatible with the provenance model proposed in #1778 and treat that as follow-up work.

## Rollout notes

1. Ship this as an additive feature with ordinary `def` unchanged.
2. Keep V1 scoped to top-level defs; do not silently support only some nested cases.
3. Decode older `.bosatsu_package`, `.bosatsu_interface`, and `.bosatsu_lib` artifacts as non-inline so mixed-version dependency graphs continue to work.
4. Follow up after merge with user-facing examples of inline control-flow helpers and, separately, provenance improvements for inlined code.
