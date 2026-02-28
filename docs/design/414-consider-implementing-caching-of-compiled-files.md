---
issue: 414
priority: 2
touch_paths:
  - docs/design/414-consider-implementing-caching-of-compiled-files.md
  - core/src/main/scala/dev/bosatsu/tool/CommonOpts.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/scala/dev/bosatsu/tool/CompileCache.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/DocCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ShowCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala
  - core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/TestCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/TranspileCommand.scala
  - core/src/test/scala/dev/bosatsu/tool/CompileCacheTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: L
generated_at: 2026-02-27T00:00:00Z
---

# Issue #414 Design: Optional Caching of Compiled Files

Issue: #414 (https://github.com/johnynek/bosatsu/issues/414)
Base branch: `main`
Status: proposed

## Summary

Add an opt-in compile cache for `tool` commands that compile source files. The cache key is a hash of a normalized source package (line/region-insensitive) plus hashes of dependency interfaces and compile options. The key maps to a content-addressed compiled package artifact, matching the issue’s key->output-link plus CAS model.

This revision makes the implementation concrete at the `PackageMap.inferAll` layer with a cache algebra and `IorT[F, ...]` pipeline, and explicitly keeps the no-cache path near-zero overhead via an `InferCache`-dependent key type.

## Context

Today, `CompilerApi.typeCheck` recompiles source packages on every invocation. This is visible in `tool check` where we currently filter out predef from outputs and have a TODO referencing #414. Existing predef lazy caches in `PackageMap` only help within a single process; they do not avoid cross-run recompilation of user packages.

## Problem Statement

We need a cache that:

1. Is optional and user-controlled via a cache directory.
2. Reuses compiled packages across process runs.
3. Invalidates correctly when source semantics, dependency interfaces, compile mode, or compiler identity changes.
4. Avoids false misses from source location noise (line/column offsets).
5. Integrates with the existing inference DAG in `PackageMap.inferAll` rather than re-implementing dependency logic in command code.

## Goals

1. Add `--cache_dir` support to `tool` compile flows (`check`, `doc`, `show`, `eval`, `test`, `transpile`).
2. Cache per-package compiled outputs, not only whole-invocation outputs.
3. Use a key composed of:
   1. normalized source hash without line/region sensitivity,
   2. dependency interface hashes,
   3. compile options and compiler identity.
4. Store compiled outputs in a content-addressed store and keep key->output indirection.
5. Preserve current behavior when cache is disabled.
6. Make cache integration concrete in `PackageMap.inferAll` with an effectful API.

## Non-goals

1. Enabling compile cache by default in this issue.
2. Wiring the same cache into `lib` command flows in this issue.
3. Remote/distributed cache protocol support.
4. Caching parse failures or error outputs.

## Proposed Architecture

### 1. CLI surface

Add `commonOpts.compileCacheDirOpt` in `CommonOpts`:

1. Flag: `--cache_dir <path>`.
2. Default: absent (`None`), meaning caching is disabled.
3. Wire through tool command paths that call `CompilerApi.typeCheck` directly or via `RuntimeCommandSupport`.

### 2. Cache data model (key link + CAS)

Introduce `core/src/main/scala/dev/bosatsu/tool/CompileCache.scala`.

Directory layout under `<cache_dir>`:

1. `keys/blake3/<k0>/<k1>`: link file keyed by compile key hash; value is output hash ident (for example `blake3:<hex>`).
2. `cas/blake3/<h0>/<h1>.bosatsu_package`: content-addressed compiled package artifact.

Flow:

1. Compute compile key hash.
2. Read key link file; if present, resolve CAS object.
3. If CAS object is readable and valid, treat as cache hit.
4. On miss, compile package, write CAS object first, then write link file.

This matches the issue proposal and bazel-style indirection.

### 3. Source hash without line-number sensitivity

For each `Package.Parsed`, compute `sourceExprHash` from normalized syntax:

1. Replace statement regions with a sentinel region via `Statement.replaceRegions(Region(0, 0))`.
2. Drop `PaddingStatement` and `Comment` from fingerprint input.
3. Preserve package name/import/export/program ordering.
4. Serialize normalized package deterministically and hash with blake3.

Result: edits that only shift line numbers (or padding/comments) do not change the key.

### 4. Dependency hash model

For each direct import of the package being compiled, include an interface hash in the key:

1. For source dependencies compiled in the same run: hash `Package.interfaceOf(compiledDep)`.
2. For explicit external interfaces from `.bosatsig`/library deps: hash those interface values.
3. For internal predef (when implicit predef is used): include the mode-specific internal predef interface hash.
4. Sort by dependency package name before hashing key payload.

Using interface hashes (not full implementation hashes) matches actual typecheck inputs and avoids unnecessary invalidations.

### 5. Concrete cache algebra (`Key` is cache-dependent) and `inferAll` API

Cache API sketch:

```scala
package dev.bosatsu.tool

import cats.data.Ior
import cats.data.NonEmptyList
import dev.bosatsu.{CompileOptions, Package, PackageError, PackageMap}

trait InferCache[F[_]] {
  type Key

  // Inputs are values inferAll already has; implementations decide key cost.
  def generateKey(
      pack: Package.Parsed,
      depResults: List[(dev.bosatsu.PackageName, Package.Inferred)],
      compileOptions: CompileOptions,
      compilerIdentity: String
  ): F[Key]

  def get(key: Key): F[Option[Package.Inferred]]
  def put(key: Key, value: Package.Inferred): F[Unit]
}

object InferCache {
  // No-op cache: no hashing/serialization work, constant key.
  def noop[F[_]: cats.Applicative]: InferCache[F] { type Key = Unit } = ???

  // Filesystem cache implementation uses a richer key payload.
  final case class FsKey(
      packageName: dev.bosatsu.PackageName,
      compileOptions: CompileOptions,
      compilerIdentity: String,
      sourceExprHash: String,
      depInterfaceHashes: List[(dev.bosatsu.PackageName, String)],
      schemaVersion: Int
  )
}
```

`PackageMap` API sketch (effectful path + compatibility shim):

```scala
object PackageMap {
  def inferAll[F[_]: cats.Monad: cats.Parallel](
      ps: Resolved,
      compileOptions: CompileOptions,
      cache: InferCache[F]
  )(implicit cpuEC: dev.bosatsu.Par.EC): F[Ior[NonEmptyList[PackageError], Inferred]]

  // existing call sites keep this behavior when no cache is configured
  def inferAll(
      ps: Resolved,
      compileOptions: CompileOptions
  )(implicit cpuEC: dev.bosatsu.Par.EC): Ior[NonEmptyList[PackageError], Inferred]
}
```

`CompilerApi.typeCheck` then chooses cache implementation:

1. `cacheDirOpt = None` -> no-op `InferCache` with `type Key = Unit`; `generateKey` is constant and does not compute hashes.
2. `cacheDirOpt = Some(path)` -> filesystem-backed CAS/key-link `InferCache[IO]` with `type Key = FsKey`.

### 6. Concrete `inferAll[F]` flow (with `IorT`)

The `inferAll` body remains DAG-driven; the new behavior is per-node cache lookup before local inference.

Sketch:

```scala
def inferAll[F[_]: Monad: Parallel](
    ps: Resolved,
    compileOptions: CompileOptions,
    cache: InferCache[F]
)(implicit cpuEC: Par.EC): F[Ior[NonEmptyList[PackageError], Inferred]] = {

  type InferRes = (TypeEnv[Kind.Arg], Package.Inferred)
  type ErrOr[A] = IorT[F, NonEmptyList[PackageError], A]

  val infer0: ResolvedU => F[Ior[NonEmptyList[PackageError], InferRes]] =
    Memoize.memoizeDagFuture[ResolvedU, Ior[NonEmptyList[PackageError], InferRes]] {
      case (pack, recurse) =>
        val depsF: ErrOr[List[(PackageName, Package.Inferred)]] = ??? // existing import resolution via recurse

        depsF.flatMap { depPacks =>
          IorT.liftF(
            cache.generateKey(pack, depPacks, compileOptions, compilerIdentity)
          ).flatMap { key =>
            IorT.liftF(cache.get(key)).flatMap {
              case Some(hit) =>
                val fte = ExportedName.typeEnvFromExports(hit.name, hit.exports)
                IorT.rightT[F, NonEmptyList[PackageError]]((fte, hit))

              case None =>
                val inferred: ErrOr[InferRes] = runExistingInferBody(pack, depPacks, compileOptions)
                inferred.semiflatTap { case (_, compiled) => cache.put(key, compiled) }
            }
          }
        }.value
    }

  ps.toMap.parTraverse(infer0.andThen(IorT(_))).map(_.map(PackageMap(_)).value)
}
```

Key points:

1. We keep existing error accumulation semantics (`Ior`, `NonEmptyList[PackageError]`).
2. `IorT[F, ...]` allows effectful cache operations while preserving partial-success diagnostics.
3. `Key` is path-dependent on the cache implementation, so no-op cache can use `Unit`.
4. For no-op cache, `generateKey` is constant and avoids hash/serialization work entirely.
5. Cache misses execute existing inference logic unchanged.
6. Cache hits return typed packages directly and still feed downstream dependency typing.

### 7. Behavior of `resolveThenInfer`

`resolveThenInfer` gets an effectful variant for cache-aware callers:

```scala
def resolveThenInfer[F[_]: Monad: Parallel, A: Show](
    ps: List[(A, Package.Parsed)],
    ifs: List[Package.Interface],
    compileOptions: CompileOptions,
    cache: InferCache[F]
)(implicit cpuEC: Par.EC): F[Ior[NonEmptyList[PackageError], Inferred]]
```

Existing pure signature remains and delegates to a no-op cache in `Par.F` to preserve call sites.

### 8. Corruption and race handling

Cache operations are best-effort:

1. Any parse/read/validation failure in cache files is treated as a miss.
2. Cache write failures do not fail compilation; they continue with fresh compile output.
3. CAS write happens before link write to avoid dangling links.
4. If `get` returns a package whose interface hash does not match key payload, treat as miss.

## Implementation Plan

1. Add `compileCacheDirOpt` to `CommonOpts` and pass it through all tool compile command constructors.
2. Add `InferCache[F]` abstraction with dependent `Key`, plus filesystem implementation in `tool/CompileCache.scala`.
3. Introduce effectful `PackageMap.inferAll[F]` with `IorT`-based cache integration.
4. Keep existing `PackageMap.inferAll` as compatibility wrapper using no-op cache.
5. Add effectful `PackageMap.resolveThenInfer[F]` overload and route cache-enabled `CompilerApi.typeCheck` to it.
6. Keep no-cache path behavior equivalent to current logic and near-zero overhead by using `InferCache.noop` (`Key = Unit`).
7. Add unit tests for key stability/invalidation and cache read/write fallbacks.
8. Add integration tests for `tool` commands with `--cache_dir`.

## Testing Strategy

### Unit tests (`CompileCacheTest`)

1. Same semantic package with different regions hashes to the same `sourceExprHash`.
2. Comment/padding-only deltas do not change `sourceExprHash`.
3. Changing a dependency interface hash changes compile key.
4. Changing compile mode or optimize flag changes compile key.
5. Corrupt link file or missing CAS object is handled as miss.

### Unit/property tests in `PackageMap` area

1. `inferAll[F]` with no-op cache matches current `inferAll` results exactly.
2. `inferAll[F]` with hit for leaf dependency skips local inference and still types dependents.
3. Mixed hit/miss DAG preserves existing `Ior` warning/error behavior.
4. No-op cache path does not compute source/dependency hashes during inference.

### Integration tests (`ToolAndLibCommandTest`)

1. `tool check --cache_dir <dir>` succeeds and writes cache artifacts.
2. Re-running with unchanged sources reuses cache-compatible artifacts and yields identical command outputs.
3. Editing only line positions/comments keeps same key path and does not create new cache entries.
4. Changing an imported dependency interface causes dependent package cache miss and recompilation.
5. Running without `--cache_dir` keeps existing behavior.

## Acceptance Criteria

1. `tool` commands that compile source accept `--cache_dir`.
2. With no `--cache_dir`, existing behavior remains unchanged.
3. `PackageMap.inferAll[F]` exists with cache parameter and returns `F[Ior[NonEmptyList[PackageError], Inferred]]`.
4. Existing `PackageMap.inferAll` API remains available as compatibility path.
5. `InferCache` defines a dependent `type Key` and `generateKey(...)`, allowing no-op key type `Unit`.
6. No-op cache path does not perform source/dependency hash computation.
7. Filesystem cache key includes normalized source hash (line/region-insensitive), dependency interface hashes, compile options, and compiler identity.
8. Key->output indirection is implemented via on-disk link files.
9. Compiled package artifacts are stored in a content-addressed store.
10. Cache hit for a package skips recompiling that package.
11. Cache miss compiles and then populates CAS + link entries.
12. Corrupt/missing cache entries are treated as misses, not hard failures.
13. Internal predef behavior remains mode-correct and preserved in final package map semantics.
14. New unit and integration tests pass.

## Risks and Mitigations

1. Risk: introducing `F[_]` into `PackageMap` increases complexity.
Mitigation: keep old API as wrapper; confine effectful logic to cache get/put boundaries and preserve existing infer body helpers.

2. Risk: incorrect key composition causes stale/incorrect hits.
Mitigation: include compile mode, optimize flag, compiler identity, source hash, and dependency interface hashes; add focused key-invalidation tests.

3. Risk: cold builds could be slower if incremental flow reduces current parallel inference.
Mitigation: keep feature opt-in; keep no-cache path untouched; benchmark after correctness, then tune cache lookup granularity.

4. Risk: cache corruption or concurrent writers produce broken entries.
Mitigation: CAS-first write order, tolerant reads (miss on parse/validation failure), and non-fatal cache write errors.

5. Risk: scope creep into library command path.
Mitigation: explicitly phase this issue to `tool` command compile flows; treat `lib` wiring as follow-up.

6. Risk: path-dependent `Key` type increases API complexity.
Mitigation: keep all call sites generic over `cache.Key`; provide `InferCache.noop` and filesystem constructors so callers do not manually construct keys.

## Rollout Notes

1. Land as opt-in only (`--cache_dir`) with no default behavior change.
2. Land `InferCache` + `inferAll[F]` + wrapper in one PR so migration is atomic.
3. Verify in CI with deterministic tests and memory-platform tests.
4. After stability, consider follow-up to:
   1. wire into `lib` command compile paths,
   2. add lightweight cache hit/miss stats output for diagnostics,
   3. evaluate making a repository-local default cache path under `.bosatsuc/`.
