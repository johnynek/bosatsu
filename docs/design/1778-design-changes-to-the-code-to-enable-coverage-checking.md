---
issue: 1778
priority: 3
touch_paths:
  - docs/design/1778-design-changes-to-the-code-to-enable-coverage-checking.md
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/main/scala/dev/bosatsu/PlatformIO.scala
  - cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala
  - cliJS/src/main/scala/dev/bosatsu/Fs2PlatformIO.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessInterfaceTest.scala
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
  - cli/src/test/scala/dev/bosatsu/IOPlatformIOTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-24T17:22:41Z
---

# Issue #1778 Design: Source Hash and Region Metadata for Coverage Checking

_Issue: #1778 (https://github.com/johnynek/bosatsu/issues/1778)_

## Summary

Persist implementation source provenance (package hash + expression regions) into proto and Matchless while keeping interfaces metadata-free and deterministic for cache-key use.

---
issue: 1778
priority: 2
touch_paths:
  - docs/design/1778-design-changes-to-the-code-to-enable-coverage-checking.md
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/main/scala/dev/bosatsu/PlatformIO.scala
  - cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala
  - cliJS/src/main/scala/dev/bosatsu/Fs2PlatformIO.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/MatchlessInterfaceTest.scala
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
  - cli/src/test/scala/dev/bosatsu/IOPlatformIOTest.scala
depends_on:
  - 1749
  - 1315
estimated_size: L
generated_at: 2026-02-24T08:00:00Z
---

# Issue #1778 Design: Source Hash and Region Metadata for Coverage Checking

Issue: #1778 (https://github.com/johnynek/bosatsu/issues/1778)
Base branch: `main`
Status: proposed

## Summary

Add source provenance to compiled implementation artifacts while keeping interface artifacts minimal and deterministic.

1. Add optional `Region` on serialized `TypedExpr`.
2. Add one package-level source hash field on serialized `Package`, encoded as the full hash identifier string (`blake3:...`).
3. Carry `(packageHash, region)` as source info on Matchless expression nodes.
4. Keep interfaces and interface-only libraries free of source hash and region metadata so interface bytes remain stable cache keys.

## Problem Statement

Current compiled implementations discard source provenance at serialization time.

1. `TypedExpr` tags are dropped in `ProtoConverter`, so we lose expression regions after writing `.bosatsu_package` / `.bosatsu_lib`.
2. `TypedAst.proto` `Package` has no source hash field, so we cannot reliably identify which source file a compiled implementation came from.
3. `Matchless.Expr` has no source-region payload on nodes, which blocks downstream coverage mapping and will be insufficient once cross-package inlining moves expressions across package boundaries.

At the same time, interfaces are used as cache keys and must remain normalized and byte-identical when semantically unchanged.

## Goals

1. Persist expression-level source regions for implementations.
2. Persist one package-level source hash for implementations.
3. Make Matchless nodes carry `(packageHash, region)` source info so inlined code still points to the original package and region.
4. Preserve backward compatibility for old serialized artifacts with no new fields.
5. Preserve interface determinism and cache-key suitability.

## Non-goals

1. Implement Matchless protobuf serialization (#629).
2. Implement full coverage reporting UI or policy in this PR.
3. Add source metadata to `Interface` protobuf schema.
4. Change type-erased Matchless semantics by reintroducing type payloads (#1749 context).

## Design

### 1. Protobuf Schema Changes

Modify `proto/src/main/protobuf/bosatsu/TypedAst.proto`.

1. Add a protobuf `Region` message with `start` and `end` integer offsets.
2. Extend `TypedExpr` with optional `region` field at a new field number outside the existing `oneof`.
3. Add a small wrapper message for package hash, for example `PackageSourceHash { string ident = 1; }`.
4. Extend `Package` with optional `sourceHash` field at a new field number.
5. Keep `Interface` unchanged.

Rationale for hash wrapper message instead of plain string:

1. Field presence is explicit in proto3 without relying on `optional` scalar semantics.
2. Absent field cleanly represents old artifacts and interface contexts.

### 2. In-memory Package Source Metadata

Implementation packages need to retain source hash after decode/encode cycles.

1. Introduce a typed package-source metadata model in Scala, owned by `Package` (or typed program provenance), with at least `sourceHashIdent: Option[String]`.
2. For freshly compiled packages, compute source hash from source text bytes and attach to that metadata before serialization.
3. For decoded packages, read hash from proto and attach to same metadata model.
4. For interfaces, metadata hash is always unset.

This avoids losing hash when reading a `.bosatsu_package` and later reassembling libraries.

### 3. Region Serialization Contract for `TypedExpr`

`ProtoConverter` becomes region-aware for implementation paths.

1. Encode: if a typed-expression tag yields a region, write `TypedExpr.region`; otherwise leave it unset.
2. Decode: read optional `TypedExpr.region`; old artifacts decode with `None`.
3. Interface serialization never writes expression tables, so no region metadata appears in `.bosatsu_interface` / exported iface sections.
4. Decode/encode must remain stable for legacy inputs that do not contain new fields.

### 4. Package Hash Serialization Contract

`Package.sourceHash` is written once per package proto.

1. Value format is full hash ident string via existing hash API (`blake3:<hex>`).
2. Hash is optional on decode for backward compatibility.
3. Hash is absent for interface-only serialization paths.
4. Invalid hash identifiers fail decode with a clear data-corruption error.

### 5. Matchless Source Info Model

Add source metadata to Matchless expression nodes.

1. Introduce `Matchless.SourceInfo(packageHashIdent: String, region: Region)`.
2. Extend Matchless expression/boolean nodes to carry required `sourceInfo` (no `Option` in the AST).
3. During lowering from `TypedExpr`, attach source info from current package hash + current expression region.
4. If provenance is unavailable (for example legacy decoded artifacts), use a sentinel `SourceInfo(emptyHashIdent, Region(0, 0))`, where `emptyHashIdent` is the hash ident for empty source bytes.
5. For synthetic helper nodes created during lowering/optimization, inherit the nearest dominating source info before falling back to the sentinel.
6. When inlining across packages (#1315), retain original source info on moved nodes.

Important constraint:

1. Keep Matchless type-erased with respect to language types.
2. Source info is provenance metadata only and must not affect optimization legality.

### 6. Interface Determinism and Cache-Key Invariant

Interfaces must stay minimal and deterministic.

1. New fields are only in `Package` and `TypedExpr`; `Interface` schema is unchanged.
2. `interfaceToProto` must continue to serialize only type/export surface.
3. Tests must assert semantically equivalent interfaces remain byte-identical even when implementation source hash/regions differ.
4. Add a ScalaCheck property asserting that for any fully compiled package, permuting any sequence-typed implementation data (for example bindings and type-environment entry order) does not change the serialized interface bytes or their hash.
5. Interface-only libraries (`internal_packages = []`) must contain no source hash/region metadata.

## Implementation Plan

1. Update `TypedAst.proto` with `Region` and package hash wrapper fields.
2. Regenerate protobuf classes and update any compile-time references.
3. Add package source metadata representation in Scala (`Package`/typed-program provenance).
4. Plumb source hash calculation in `CompilerApi` from parsed source text to compiled package metadata.
5. Update `ProtoConverter.typedExprToProto` and decode path to map optional regions.
6. Update `ProtoConverter.packageToProto` and decode path to map optional package hash.
7. Update `PlatformIO` read/write signatures and implementations where needed so decoded package metadata is preserved (JVM + JS platforms).
8. Add Matchless `SourceInfo` as required provenance on expression/boolean nodes and propagate it through lowering in `Matchless.fromLet` and through package compilation in `MatchlessFromTypedExpr.compile`.
9. Add sentinel provenance defaults (`emptyHashIdent`, `Region(0, 0)`) for legacy or otherwise missing origins.
10. Ensure transformations in `Matchless` preserve `sourceInfo` when rewriting nodes.
11. Keep interface code paths explicitly metadata-free.
12. Add tests for compatibility, determinism, and Matchless source-info propagation.

## Testing Strategy

### Proto and Converter Tests

1. Round-trip `TypedExpr` with present regions preserves regions.
2. Decoding legacy `TypedExpr` blobs with missing `region` still succeeds.
3. Round-trip package with `sourceHash` preserves full ident string.
4. Decoding legacy packages with missing `sourceHash` still succeeds.
5. Re-encoding decoded packages preserves hash/region presence semantics.

### Interface Determinism Tests

1. Interface serialization does not include source hash/region fields.
2. Two compilations with identical public API but changed implementation body produce byte-identical interface protobuf bytes.
3. ScalaCheck property: for fully compiled packages, any permutation of sequence-typed implementation data (binding names, type-environment items, and similar order-insensitive sequences) preserves serialized interface bytes and hash.
4. Interface-only library assembly contains no implementation metadata.

### Matchless Tests

1. Lowering attaches source info on expression nodes when package hash and region are available and uses sentinel values when they are not.
2. Matchless expression/boolean AST constructors and rewrites always produce nodes with `sourceInfo` (no missingness).
3. Cross-package inlining preserves original package hash on inlined nodes.
4. Existing Matchless optimization/regression suites remain green with metadata-carrying nodes.

### Platform Read/Write Tests

1. `.bosatsu_package` file round-trip via IO platform preserves package hash and regions.
2. Same invariants hold for both JVM and JS platform implementations where applicable.

## Acceptance Criteria

1. `TypedAst.proto` includes optional typed-expression region and optional package source hash fields with new field numbers.
2. `ProtoConverter` writes `TypedExpr.region` for implementation expressions when available.
3. `ProtoConverter` writes `Package.sourceHash` exactly once per implementation package when available.
4. Legacy compiled artifacts without new fields decode successfully.
5. Interfaces remain schema-minimal and do not carry source hash or region metadata.
6. Interface protobuf output remains deterministic and suitable for cache keys, including ScalaCheck permutation-invariance checks over sequence-typed implementation data.
7. Matchless expression/boolean nodes always carry `(packageHash, region)` source info (required field, no `Option`).
8. Matchless lowering populates source info from typed-expression provenance and uses sentinel values for missing legacy provenance.
9. Inlined Matchless nodes retain original package hash provenance.
10. `lib assemble` / package read-write flows preserve source hash metadata rather than dropping it.
11. Tests explicitly cover “interface-only libraries contain no source metadata”.
12. Existing compilation/evaluation/codegen test suites continue to pass.

## Risks and Mitigations

1. Risk: Matchless API churn is large and touches many pattern matches.
Mitigation: perform refactor mechanically in one pass; add compatibility helpers where possible; run full Matchless regression suites.

2. Risk: Memory overhead from per-node source info in Matchless.
Mitigation: do not copy hash payloads per node; store references to already-built package-hash values, rely on small region payload size, and keep only a small number of hashes in scope at once.

3. Risk: Interface cache-key regressions if source metadata leaks.
Mitigation: keep `Interface` schema unchanged; add byte-level determinism tests.

4. Risk: Mixed-version ecosystems with old package/library blobs.
Mitigation: decode missing fields as absent metadata; treat invalid present hash strings as explicit decode errors.

5. Risk: Source-hash computation differences across platforms.
Mitigation: define hashing input as UTF-8 bytes of source text and assert cross-platform round-trip tests.

## Rollout Notes

1. Land schema + converter compatibility first (read old, write new).
2. Land package source-hash plumbing and file IO preservation next.
3. Land Matchless source-info propagation after package metadata is available.
4. Keep coverage checker consumer work separate; this issue provides enabling metadata.
5. Monitor CI for interface determinism regressions and Matchless performance regressions after merge.

## Follow-up Work (Out of Scope Here)

1. Implement Matchless protobuf serialization (#629) with string-table deduplication for source hash values.
2. Build coverage-checking execution and reporting on top of the new metadata (#1749).
3. Consider optional stripping of Matchless source info in production-only codegen flows if footprint becomes a concern.
