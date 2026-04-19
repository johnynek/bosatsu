---
issue: 2073
priority: 3
touch_paths:
  - docs/design/2073-add-doc-base-urls-to-libraries.md
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/library/LibConfig.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool/CommandSupport.scala
  - core/src/main/scala/dev/bosatsu/tool/MarkdownDoc.scala
  - core/src/main/scala/dev/bosatsu/tool_command/DocCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/AssembleCommand.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-08T17:02:00Z
---

# Issue #2073 Design: Add doc base URLs to libraries

_Issue: #2073 (https://github.com/johnynek/bosatsu/issues/2073)_

## Summary

Add optional docs base URL metadata to library artifacts, route markdown links by package (with external-site overrides for dependency packages), and make `lib doc` aware of exported vs private packages with labeling and an option to skip private docs.

---
issue: 2073
priority: 2
touch_paths:
  - docs/design/2073-add-doc-base-urls-to-libraries.md
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/main/scala/dev/bosatsu/library/LibConfig.scala
  - core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool/CommandSupport.scala
  - core/src/main/scala/dev/bosatsu/tool/MarkdownDoc.scala
  - core/src/main/scala/dev/bosatsu/tool_command/DocCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/AssembleCommand.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-08T00:00:00Z
---

# Issue #2073 Design: Add doc base URLs to libraries

_Issue: #2073 (https://github.com/johnynek/bosatsu/issues/2073)_

Status: proposed  
Base branch: `main`

## Summary

`doc` generation currently assumes every referenced package has docs at a local relative path. This is wrong for dependency packages hosted on separate doc sites. This design adds optional docs base URL metadata to libraries and package-aware link resolution in markdown docs. It also makes `lib doc` aware of exported package boundaries so non-exported packages are labeled `private package`, with a flag to omit private-package docs.

## Problem statement

Current behavior has two gaps:

1. Package/type links are always rendered as local relative links (for example `../Dep/Util.md#type-...`), even when the package comes from another library.
2. `lib doc` does not use `exported_packages` to annotate visibility, and cannot generate exported-only docs.

Consequences:

1. Dependency links in published docs are often broken.
2. Generated docs do not clearly distinguish public API packages from internal/private packages.

## Goals

1. Store an optional docs base URL in library metadata.
2. Resolve links per package, not with a single local-path strategy.
3. Preserve existing behavior when no docs base URL is available.
4. In `lib doc`, label non-exported local packages as `private package`.
5. Add a `lib doc` option to skip private-package doc generation.
6. Keep backward compatibility with existing configs and `.bosatsu_lib` files.

## Non-goals

1. Redesigning markdown layout beyond required labeling/link routing.
2. Requiring every dependency library to provide a docs base URL.
3. Changing library API validation semantics.
4. Implementing docs publishing/deployment workflows.

## Proposed architecture

### 1) Add docs base URL to library metadata

Add an optional docs-base field at config and binary levels:

1. `LibConfig` adds `docBaseUrl: Option[String]` (JSON key: `doc_base_url`).
2. `proto.Library` adds `doc_base_url` as a new optional string field (`13`).
3. `LibConfig.unvalidatedAssemble` writes the value into `proto.Library`.
4. `DecodedLibrary` exposes normalized accessor (`None` for empty, trailing-slash normalization).

Rationale:

1. Dependency metadata already flows through `DecodedLibrary` for doc generation commands.
2. Optional field keeps old artifacts valid and avoids migration churn.

### 2) Build package -> docs base URL map from dependencies

Introduce a helper that builds package-level routing data from decoded dependency libraries:

1. For each dependency library with `doc_base_url`, map each exported interface package to that base URL.
2. Keep mapping package-specific (library-level URL expanded across its exported packages).
3. Detect collisions: if the same package maps to different base URLs, fail early with a deterministic error.
4. If no base URL exists for a package, fall back to current relative-link behavior.

### 3) Extend MarkdownDoc link resolution

`MarkdownDoc.generate` should accept package metadata and route links through a resolver:

1. New parameter: `packageBaseUrls: Map[PackageName, String]`.
2. Existing local relative routing remains default.
3. Resolver behavior:
   - same package: anchor-only link (`#type-...`),
   - package with base URL override: `${base}/${pkg-path}.md` (+ anchor),
   - otherwise: relative local path as today.
4. Apply resolver consistently in:
   - type reference links,
   - package dependency links in the package header section.

### 4) Add package visibility metadata for lib-doc output

In `lib doc` path, derive visibility from `conf.exportedPackages`:

1. Local package is `Exported` if matched by `exportedPackages`.
2. Local package is `Private` if not exported.
3. Predef is treated separately (not labeled private).
4. `MarkdownDoc` receives visibility info and writes a `private package` marker on private package pages.

### 5) Add option to omit private-package docs

Add a new `lib doc` flag:

1. `--exclude_private_packages` (default: false).
2. Default behavior remains inclusive (private pages still generated, now labeled).
3. With flag enabled, only exported local packages are emitted (plus optional Predef when `--include_predef` is set).
4. Typechecking still runs on all source files, preserving current diagnostics behavior.

### 6) Apply routing to both doc entry points

1. `lib doc`: use dependency libraries from config/CAS to construct package base URL map.
2. `tool doc`: when dependency libraries are provided via `--pub_dep`/`--priv_dep`, pass the same map into `MarkdownDoc`.
3. Visibility labeling is only applied in `lib doc` because only it has library export context.

## Detailed implementation plan

1. Update protobuf schema:
   - Add `doc_base_url` to `Library` in `proto/src/main/protobuf/bosatsu/TypedAst.proto`.
2. Update library config model and JSON:
   - Add optional `docBaseUrl` field in `core/src/main/scala/dev/bosatsu/library/LibConfig.scala`.
   - Read/write `doc_base_url` optionally.
   - Include field when assembling `proto.Library`.
3. Add decode helper:
   - In `core/src/main/scala/dev/bosatsu/library/DecodedLibrary.scala`, add normalized accessor for `protoLib.docBaseUrl`.
4. Update command surfaces:
   - `core/src/main/scala/dev/bosatsu/library/Command.scala`:
     - `lib doc` adds `--exclude_private_packages`,
     - doc-generation flow computes visibility map and dependency package base URL map,
     - pass new metadata to `MarkdownDoc.generate`.
   - `core/src/main/scala/dev/bosatsu/tool_command/AssembleCommand.scala`:
     - optional `--doc_base_url` for direct library assembly parity.
5. Add shared mapping helper:
   - `core/src/main/scala/dev/bosatsu/tool/CommandSupport.scala` helper to derive package->base URL mapping from decoded dependencies with conflict checks.
6. Extend markdown generator:
   - `core/src/main/scala/dev/bosatsu/tool/MarkdownDoc.scala`:
     - add package-base-url aware resolver,
     - add private label rendering,
     - keep output stable when optional maps are empty.
7. Wire tool doc:
   - `core/src/main/scala/dev/bosatsu/tool_command/DocCommand.scala` passes dependency-based base URL map into `MarkdownDoc.generate`.
8. Add tests:
   - `core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala`:
     - optional `doc_base_url` JSON read/write and defaults.
   - `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`:
     - dependency link uses external base URL,
     - fallback relative link when missing base URL,
     - private package label in lib doc output,
     - `--exclude_private_packages` suppresses private docs.

## Testing plan

### Config and metadata tests

1. Legacy config without `doc_base_url` parses unchanged.
2. Config with `doc_base_url` round-trips through JSON reader/writer.
3. Assembled library contains `doc_base_url` when set.

### Link routing tests

1. Dependency package with docs base URL yields absolute external links in markdown.
2. Same setup without docs base URL yields existing relative links.
3. Type-reference anchors still resolve correctly with external base URLs.

### Lib-doc visibility tests

1. Default `lib doc` includes private package pages and each private page includes `private package`.
2. `lib doc --exclude_private_packages` omits private package files.
3. Exported package pages are not labeled private.

## Acceptance criteria

1. Library metadata supports optional `doc_base_url` in config and `.bosatsu_lib`.
2. `doc` generation can route links by package base URL when metadata is present.
3. Existing relative-link behavior is preserved when package base URL metadata is absent.
4. In `lib doc`, any non-exported local package page contains label text `private package`.
5. `lib doc --exclude_private_packages` emits only exported package docs (plus optional Predef).
6. Existing users who do not set docs base URLs see no behavior regressions beyond the new private label on private pages.
7. Added tests pass in `core` test suite.

## Risks and mitigations

1. Risk: malformed base URLs create broken links.
   Mitigation: normalize URLs in one helper and keep fallback behavior when unset.

2. Risk: package collisions across dependencies create ambiguous URL routing.
   Mitigation: fail fast on conflicting package mappings.

3. Risk: behavior differences between `tool doc` and `lib doc` implementations.
   Mitigation: centralize mapping/resolution logic and cover both commands in tests.

4. Risk: older dependencies lacking `doc_base_url` still produce local links.
   Mitigation: explicit fallback and rollout guidance for republishing dependencies with metadata.

## Rollout notes

1. Land as a single PR including schema, config, command wiring, markdown generation, and tests.
2. No migration required; all new fields/options are optional.
3. Upstream libraries can adopt `doc_base_url` incrementally; downstream links improve as dependencies republish.
4. If regressions occur, keep metadata plumbing but temporarily disable external override usage behind an empty-map default while fixing link resolution bugs.
