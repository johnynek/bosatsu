---
issue: 2216
priority: 3
touch_paths:
  - docs/design/2216-remove-unsound-cast-in-compilecache.md
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/Region.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/cache/InferCache.scala
  - core/src/main/scala/dev/bosatsu/cache/CompileCache.scala
  - core/src/main/scala/dev/bosatsu/cache/InferPhases.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/ShadowedBindingTypeCheck.scala
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/tool/CompileCacheTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-19T19:39:34Z
---

# Issue #2216 Design: Replace CompileCache’s Unsound Cast With Truthful Compiled Artifacts

_Issue: #2216 (https://github.com/johnynek/bosatsu/issues/2216)_

## Summary

Remove the `CompileCache` cast by making the cache boundary return a truthful compiled-package type instead of pretending deserialized `Unit` tags are `Declaration`. The recommended design is to keep `Package.Typed[Declaration]` only on the cold compile path, serialize real regions into cached packages, return `Package.Typed[Region]` from cache and compiler APIs, and split source-sensitive diagnostics from region-only replay so warm-cache behavior stays safe and useful.

## Context
`CompileCache.get` currently deserializes `proto.Package` values as `Package.Typed[Unit]` and then casts the result to `Package.Inferred`:

- the cache format does not retain `Declaration`
- the cache format does not retain typed-expression regions
- some downstream code still assumes those tags exist and can be read

That makes the current cast unsound. It is not just an imprecise type; it can crash at runtime when cached packages are used by code that touches source tags.

This is already visible in the work around issue [#2206](https://github.com/johnynek/bosatsu/pull/2206), where cache-hit lint replay has to defensively avoid paths that read `Declaration` tags on cached packages.

A second constraint matters here: cached packages also do not retain the source-side `program.from` / `List[Statement]` payload. Even if regions are restored, cached artifacts still cannot honestly claim to be full `Package.Typed[Declaration]` values unless the cache starts serializing the source AST, which this issue should avoid.

## Problem
We need a cache artifact type that is:

1. Truthful about what the cache actually stores.
2. Strong enough to support real downstream use cases, especially region-based consumers.
3. Small enough that we do not serialize the full source AST.
4. Compatible with warm-cache behavior needed by issue [#2206](https://github.com/johnynek/bosatsu/pull/2206) and later region-reading work.

## Goals
1. Remove the unsound cast from `CompileCache`.
2. Make the cache boundary and compile APIs advertise a truthful result type.
3. Preserve warm-cache usability for code generation, evaluation, docs, and other typed-package consumers.
4. Preserve enough location information for region-based consumers and cache-hit lint replay that only needs regions.
5. Keep source-sensitive diagnostics correct by running them either before caching or from parsed-source replay.

## Non-goals
1. Serializing the full `Declaration` tree or `program.from` into cached package artifacts.
2. Preserving compatibility with existing on-disk cache entries.
3. Reworking interface hashing beyond what is needed to keep interface artifacts stable.
4. Making every diagnostic replayable from cache alone.

## Alternatives Considered
### Return `Package.Typed[Any]`
This is the smallest mechanical fix: it removes the lie without changing the cache format.

It is not the recommended design because:

- it does not give blocked consumers any region data
- it keeps cache-hit lint replay in a weak state, since even region-only passes cannot run directly
- it pushes ambiguity outward to every caller instead of defining a precise artifact contract

`Typed[Any]` is a safe stopgap, but not the best repository-level contract.

### Serialize full `Declaration` / source AST
This would preserve the current `Package.Inferred` contract.

It is rejected because it expands the cache format far beyond the issue’s scope, duplicates large source structures, and defeats the current design choice to cache compiled typed artifacts rather than full parsed source.

## Decision
Adopt a two-type model:

- keep `Package.Typed[Declaration]` as the internal cold-compile representation
- introduce a truthful compiled artifact type, `Package.Typed[Region]`, for the cache boundary and post-compile outputs

This gives the cache a precise, useful contract:

- cached packages do not claim to carry source declarations
- cached packages do carry real regions
- callers that need more than regions must stay on the cold path or use parsed-source replay

This is the smallest type that is both truthful and still useful.

## Architecture And Implementation Plan
### 1. Split source-tagged and compiled artifact package types
In `Package.scala` and `PackageMap.scala`:

- keep `Package.Inferred = Package.Typed[Declaration]` for the internal inference/custom-check pipeline
- add `Package.Compiled = Package.Typed[Region]`
- add `PackageMap.Compiled = PackageMap.Typed[Region]`
- add a helper such as `Package.toCompiled(inferred: Package.Inferred): Package.Compiled` that maps each typed-expression tag from `Declaration` to `Declaration.region`

In `Region.scala`:

- add `HasRegion[Region]` as the identity instance

This makes the main invariant explicit: source-tagged packages are an internal compiler representation, while cached packages are compiled artifacts with regions only.

### 2. Move the cache boundary to `Package.Compiled`
In `InferCache.scala`, `CompileCache.scala`, `InferPhases.scala`, and `PackageMap.scala`:

- change `InferCache.get` / `put` to use `Package.Compiled`
- change `CompileCache.outputHashValue`, `get`, and `put` to serialize and deserialize `Package.Compiled`
- remove the cast entirely from `CompileCache.get`
- after cold compilation finishes, convert `Package.Inferred` to `Package.Compiled` before writing to cache and before returning the final package map
- widen `InferPhases.dependencyInterface` so it can accept the compiled artifact type, since interface derivation only needs typed/package structure, not `Declaration`

Cold-path flow becomes:

1. parse and resolve source
2. infer and run source-sensitive checks on `Package.Inferred`
3. run finish phases
4. convert to `Package.Compiled`
5. store and return `Package.Compiled`

Warm-path flow becomes:

1. deserialize `Package.Compiled`
2. use it directly, with no cast and no hidden tag assumptions

### 3. Serialize real regions in package artifacts
The current protobuf format erases tags, so `Package.Typed[Region]` requires a schema change.

In `TypedAst.proto` and `ProtoConverter.scala`:

- add a protobuf `Region` message or equivalent inline fields
- store a region for each `TypedExpr`
- store branch pattern regions as well, since typed match diagnostics depend on them
- update package encode/decode so compiled package round-trips preserve regions exactly
- leave interface protobufs region-free, so interface hashing and interface artifacts do not grow unnecessarily

This should be treated as a cache-format change and paired with a `CompileCache.schemaVersion` bump.

### 4. Separate source-sensitive diagnostics from region-only diagnostics
This issue should not try to make cached artifacts pretend they still have the source AST.

Instead, split consumers into two categories.

Source-sensitive consumers:

- require `Declaration` or `program.from`
- must run on the cold path before `Package.toCompiled`, or replay from parsed source
- examples: unused-let replay based on source lets, source-form-specific checks, any diagnostic that needs the original `Declaration.Matches` form or parsed-pattern reconstruction

Region-only consumers:

- only need typed structure plus regions
- should be widened or split to accept `HasRegion` / `Region`
- examples: `ShadowedBindingTypeCheck`, and the replayable subset of totality diagnostics

Concrete implications:

- `ShadowedBindingTypeCheck` can be generalized from `TypedExpr[Declaration]` to `TypedExpr[A]` with `HasRegion[A]`
- `TotalityCheck` should expose a replay-oriented entrypoint for postponable diagnostics that works on region-tagged typed expressions and does not depend on `Declaration`-specific source-form inspection
- `PackageError` and `CompilerApi` should stop assuming replayed diagnostics always carry `Declaration`

This is also the clean path for issue [#2206](https://github.com/johnynek/bosatsu/pull/2206): source-based lint replay remains source-based where necessary, while region-only replay can run safely on warm-cache compiled artifacts.

### 5. Update outward-facing compile APIs
`CompilerApi` and any command/library plumbing that currently returns `PackageMap.Inferred` should return `PackageMap.Compiled` instead.

This keeps the boundary honest:

- internal helpers that truly test pre-cache compiler behavior can still use `Package.Inferred`
- command/runtime/codegen paths receive the same truthful type regardless of cold or warm compile

Because the package type is covariant, most consumers that only need `Package.Typed[Any]` should require little or no behavioral change.

### 6. Testing Strategy
Add targeted tests for the new contract.

`ProtoConverterTest`:

- compiled package round-trip preserves typed-expression regions
- compiled package round-trip preserves branch pattern regions

`CompileCacheTest`:

- warm hits return region-tagged compiled packages
- reading `HasRegion.region` from cached expressions does not crash
- cache hits still decode correctly for packages with imports and for optimized outputs
- old schema entries are ignored after the schema bump

`ToolAndLibCommandTest`:

- cache-hit replay paths used by issue `#2206` behave the same on JVM and Scala.js without any best-effort cast workaround
- postponed lint that only needs regions can still replay on warm hits
- source-sensitive replay still comes from parsed source, not from fabricated cached declarations

## Acceptance Criteria
1. `CompileCache` no longer contains the `asInstanceOf[Package.Inferred]` cast, and cache-hit decoding does not rely on any equivalent unsound cast.
2. Cached compiler outputs have a truthful public type, `Package.Typed[Region]` or an equivalent `Package.Compiled` alias, and `CompilerApi` / `PackageMap` expose that type at the compile boundary.
3. Regions survive protobuf round-trip for compiled packages, including match-branch pattern regions needed by downstream diagnostics.
4. Warm-cache consumers that only need typed structure plus regions can read cached artifacts without runtime crashes on either JVM or Scala.js.
5. Diagnostics that truly require source declarations remain on the cold path or replay from parsed source rather than assuming the cache has `Declaration` tags.
6. The compile-cache schema version is bumped so existing regionless entries are treated as misses instead of being decoded under the new contract.

## Risks
1. The protobuf change is easy to under-specify. Missing even one region-bearing structure would produce a truthful top-level type with incomplete data and hard-to-debug downstream failures.
2. `Package.Typed[Region]` still does not carry `program.from` or the original `Declaration`, so any overlooked source-sensitive consumer will need refactoring rather than a simple type rename.
3. `TotalityCheck` is partly source-sensitive today. If its replay path is not explicitly split, callers may keep reaching for `Declaration` where only postponable, region-based replay was intended.
4. The API rename from `Inferred` to a compiled-artifact type may touch a broad set of signatures, even if most consumers only need covariance-compatible `Typed[Any]` behavior.

## Rollout Notes
1. Land the cache-format and type-boundary change together. The schema/version bump should make all existing cache entries cold-miss automatically; no migration is required.
2. Keep any temporary cache-hit workaround in issue `#2206` only until the region-backed compiled artifact and region-only replay helpers are available, then remove that workaround immediately.
3. Do not change interface serialization unless strictly necessary. Keeping interfaces region-free avoids churning interface hashes and unnecessary cache invalidation outside the compiled-package artifact path.
4. Stale `keys/` and `cas/` entries from the old schema can remain on disk safely; optional cleanup can remove them later, but the new key version should make them unreachable.
