---
issue: 414
priority: 3
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
estimated_size: M
generated_at: 2026-02-27T20:35:43Z
---

# Issue #414 Design: Optional Caching of Compiled Files

_Issue: #414 (https://github.com/johnynek/bosatsu/issues/414)_

## Summary

Opt-in cache_dir compile cache for tool commands, keyed by region-insensitive source hash plus dependency interface hashes, with key-to-CAS indirection and incremental per-package reuse.

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

## Context

Today, `CompilerApi.typeCheck` recompiles source packages on every invocation. This is visible in `tool check` where we currently filter out predef from outputs and have a TODO referencing #414. Existing predef lazy caches in `PackageMap` only help within a single process; they do not avoid cross-run recompilation of user packages.

## Problem Statement

We need a cache that:

1. Is optional and user-controlled via a cache directory.
2. Reuses compiled packages across process runs.
3. Invalidates correctly when source semantics, dependency interfaces, compile mode, or compiler identity changes.
4. Avoids false misses from source location noise (line/column offsets).

## Goals

1. Add `--cache_dir` support to `tool` compile flows (`check`, `doc`, `show`, `eval`, `test`, `transpile`).
2. Cache per-package compiled outputs, not only whole-invocation outputs.
3. Use a key composed of:
   1. normalized source hash without line/region sensitivity,
   2. dependency interface hashes,
   3. compile options and compiler identity.
4. Store compiled outputs in a content-addressed store and keep key->output indirection.
5. Preserve current behavior when cache is disabled.

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

Introduce a new helper in `core/src/main/scala/dev/bosatsu/tool/CompileCache.scala`.

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
4. Serialize normalized package deterministically (canonical doc/proto-like encoding) and hash with blake3.

Result: edits that only shift line numbers (or padding/comments) do not change the key.

### 4. Dependency hash model

For each direct import of the package being compiled, include an interface hash in the key:

1. For source dependencies compiled in the same run: hash `Package.interfaceOf(compiledDep)`.
2. For explicit external interfaces from `.bosatsig`/library deps: hash those interface values.
3. For internal predef (when implicit predef is used): include the mode-specific internal predef interface hash.
4. Sort by dependency package name before hashing key payload.

Using interface hashes (not full implementation hashes) matches actual typecheck inputs and avoids unnecessary invalidations.

### 5. Compile key payload

Hash this payload to produce `compileKey`:

1. schema version constant (for cache format migrations),
2. compiler identity (`BuildInfo.version` + git sha if available),
3. package name,
4. compile options (`optimize`, `mode`),
5. `sourceExprHash`,
6. sorted `(depPackageName, depInterfaceHash)` list.

### 6. Cache-aware compile flow in `CompilerApi`

Add cache-aware path in `CompilerApi.typeCheck` when `cacheDirOpt` is defined.

High-level algorithm:

1. Parse inputs with existing `PackageResolver.parseAllInputs` and preserve source map behavior.
2. Run existing graph/error checks (`resolveAll` semantics) before cache reads so duplicate/cycle/import errors stay consistent.
3. Build topological order of source packages.
4. Iterate in dependency order:
   1. compute key,
   2. attempt cache hit,
   3. on miss compile only that package against dependency interfaces,
   4. on success write CAS + key link,
   5. register compiled package and interface hash for downstream packages.
5. If internal predef is in use, include `PackageMap.predefCompiledForMode(...)` in final returned map as today.
6. Return `(PackageMap.Inferred, path->packageName mappings)` with existing shape.

When `cacheDirOpt` is absent, keep current `PackageMap.typeCheckParsed` flow unchanged.

### 7. Corruption and race handling

Cache operations are best-effort:

1. Any parse/read/validation failure in cache files is treated as a miss.
2. Cache write failures do not fail compilation; they log/debug and continue with fresh compile output.
3. CAS write happens before link write to avoid dangling links.

## Implementation Plan

1. Add `compileCacheDirOpt` to `CommonOpts` and pass it through all tool compile command constructors.
2. Extend `CompilerApi.typeCheck` (and `typeCheck0` path as needed) with `cacheDirOpt: Option[Path] = None`.
3. Implement `CompileCache` helper:
   1. key encoding/hash,
   2. source normalization hash,
   3. interface hash,
   4. CAS/link read/write.
4. Implement cache-aware package-at-a-time compilation in `CompilerApi` using topological dependency order.
5. Keep no-cache path byte-for-byte behavior equivalent to current logic.
6. Add unit tests for key stability/invalidation and cache read/write fallbacks.
7. Add integration tests for `tool` commands with `--cache_dir`.

## Testing Strategy

### Unit tests (`CompileCacheTest`)

1. Same semantic package with different regions hashes to the same `sourceExprHash`.
2. Comment/padding-only deltas do not change `sourceExprHash`.
3. Changing a dependency interface hash changes compile key.
4. Changing compile mode or optimize flag changes compile key.
5. Corrupt link file or missing CAS object is handled as miss.

### Integration tests (`ToolAndLibCommandTest`)

1. `tool check --cache_dir <dir>` succeeds and writes cache artifacts.
2. Re-running with unchanged sources reuses cache-compatible artifacts and yields identical command outputs.
3. Editing only line positions/comments keeps same key path and does not create new cache entries.
4. Changing an imported dependency interface causes dependent package cache miss and recompilation.
5. Running without `--cache_dir` keeps existing behavior.

## Acceptance Criteria

1. `tool` commands that compile source accept `--cache_dir`.
2. With no `--cache_dir`, existing behavior remains unchanged.
3. Cache key includes normalized source hash (line/region-insensitive), dependency interface hashes, compile options, and compiler identity.
4. Key->output indirection is implemented via on-disk link files.
5. Compiled package artifacts are stored in a content-addressed store.
6. Cache hit for a package skips recompiling that package.
7. Cache miss compiles and then populates CAS + link entries.
8. Corrupt/missing cache entries are treated as misses, not hard failures.
9. Internal predef behavior remains mode-correct and preserved in final package map semantics.
10. New unit and integration tests pass.

## Risks and Mitigations

1. Risk: incorrect key composition causes stale/incorrect hits.
Mitigation: include compile mode, optimize flag, compiler identity, source hash, and dependency interface hashes; add focused key-invalidation tests.

2. Risk: cold builds could be slower if incremental flow reduces current parallel inference.
Mitigation: keep feature opt-in; keep no-cache path untouched; optimize cache path iteratively after correctness lands.

3. Risk: cache corruption or concurrent writers produce broken entries.
Mitigation: CAS-first write order, tolerant reads (miss on parse/validation failure), and non-fatal cache write errors.

4. Risk: scope creep into library command path.
Mitigation: explicitly phase this issue to `tool` command compile flows; treat `lib` wiring as follow-up.

## Rollout Notes

1. Land as opt-in only (`--cache_dir`) with no default behavior change.
2. Verify in CI with deterministic tests and memory-platform tests.
3. After stability, consider follow-up to:
   1. wire into `lib` command compile paths,
   2. add lightweight cache hit/miss stats output for diagnostics,
   3. evaluate making a repository-local default cache path under `.bosatsuc/`.
