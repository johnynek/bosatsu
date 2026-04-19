---
issue: 2156
priority: 3
touch_paths:
  - docs/design/2156-remove-implicit-package-names.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala
  - core/src/main/scala/dev/bosatsu/tool/CommonOpts.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool/MarkdownDoc.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/PlatformIO.scala
  - core/src/main/scala/dev/bosatsu/MemoryMain.scala
  - cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala
  - core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala
  - jsapi/src/main/scala/dev/bosatsu/jsapi/JsApi.scala
  - jsui/src/main/scala/dev/bosatsu/jsui/Store.scala
  - jsui/src/test/scala/dev/bosatsu/jsui/StoreTest.scala
  - test_workspace/Foo.bosatsu
  - test_workspace/Bar.bosatsu
  - test_workspace/Quicksort.bosatsu
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - core/src/test/scala/dev/bosatsu/LibBuildImplicitPackageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/TestUtils.scala
  - docs/src/main/paradox/transpile_python.md
  - test_cli.sh
  - test_python.sh
  - testc
  - scripts/test_c_valgrind.sh
  - scripts/test_c_sanitizers.sh
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Ci.bosatsu
depends_on: []
estimated_size: M
generated_at: 2026-03-13T20:16:02Z
---

# remove implicit package names

_Issue: #2156 (https://github.com/johnynek/bosatsu/issues/2156)_

## Summary

Require explicit `package` declarations in all source parsing paths, remove path-derived package inference and related CLI flags (`--package_root`, `--search`), and migrate in-repo fixtures/tests/docs/tooling to explicit package naming.

## Context
Bosatsu currently supports implicit package naming when a package root is provided. The current flow derives a default package name from file paths and passes it into parsing, which allows files to omit `package ...` declarations.

That behavior conflicts with the language design goals for explicitness and single-source naming:
1. Package identity can come from source or filesystem position.
2. Package identity is not always visible in the file.
3. Composition depends on an implicit filesystem convention.

## Goals
1. Make package names explicit in source for all compiled files.
2. Remove compiler support for path-derived default package names.
3. Remove CLI options that only exist to support implicit package naming.
4. Keep command behavior otherwise stable when sources already have explicit package declarations.

## Non-Goals
1. Redesign package import semantics.
2. Add a replacement auto-discovery mechanism for missing imports.
3. Change package naming syntax.
4. Preserve backward compatibility for `--package_root` / `--search`.

## Current Behavior (Relevant Architecture)
1. `PackageResolver.LocalRoots` maps path -> package via `PlatformIO.pathPackage(...)`.
2. Parsers are invoked as `Package.parser(Some(defaultPackage))` or `Package.headerParser(Some(defaultPackage))`.
3. `--package_root` and `--search` expose this mode through tool commands.
4. Some tests, scripts, and JS wrappers rely on this implicit mode.

## Proposed Design
### 1. Parser Contract: explicit package declaration only
1. Remove default-package parsing mode from package/header parsing entry points.
2. All source parsing uses explicit package declarations from source text.
3. Any file missing a package declaration fails parse, independent of input path.

Primary implementation focus:
- `core/src/main/scala/dev/bosatsu/Package.scala`
- `core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala`
- `core/src/main/scala/dev/bosatsu/library/Command.scala`
- `core/src/main/scala/dev/bosatsu/tool/MarkdownDoc.scala`

### 2. Remove path-derived package inference from IO and resolver layers
1. Remove `PlatformIO.pathPackage` API and shared helper implementation.
2. Remove corresponding implementations in filesystem and memory platforms.
3. Simplify `PackageResolver` to explicit parsing behavior; remove local-root implicit naming mode.

Primary implementation focus:
- `core/src/main/scala/dev/bosatsu/PlatformIO.scala`
- `cli/src/main/scala/dev/bosatsu/IOPlatformIO.scala`
- `core/src/main/scala/dev/bosatsu/MemoryMain.scala`
- `core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala`

### 3. CLI surface cleanup
1. Remove `--package_root` and `--search` from tool command option parsers.
2. Keep existing `--input`, `--input_dir`, and `--input_all_subdir` as explicit input selection mechanisms.
3. Update user-facing error hints to avoid referencing removed flags.

Primary implementation focus:
- `core/src/main/scala/dev/bosatsu/tool/CommonOpts.scala`
- `core/src/main/scala/dev/bosatsu/tool_command/CheckCommand.scala`
- `core/src/main/scala/dev/bosatsu/PackageError.scala`

### 4. In-repo source/fixture migration
1. Add explicit package declarations to in-repo `.bosatsu` files that currently rely on implicit naming.
2. Update embedded source fixtures and command strings that pass removed flags.

Known migration targets:
- `test_workspace/Foo.bosatsu`
- `test_workspace/Bar.bosatsu`
- `test_workspace/Quicksort.bosatsu`
- `jsui/src/test/scala/dev/bosatsu/jsui/StoreTest.scala` (embedded source)
- Scripts/docs/wrappers that pass `--package_root` or `--search`

## Implementation Plan
1. Core parser/refactor phase.
- Make package declaration mandatory at parse boundaries.
- Remove default package plumbing in resolver and library-side parse paths.

2. Resolver/CLI phase.
- Remove local-root implicit package machinery.
- Remove CLI flags and update option wiring.
- Update diagnostics mentioning removed flags.

3. Migration phase.
- Update remaining implicit fixtures and embedded sample source.
- Remove obsolete flags from JS API/UI wrappers and shell scripts.
- Update command examples in docs.

4. Test stabilization phase.
- Replace implicit-package-specific tests with explicit-package tests.
- Update option-parsing and command-integration tests to no longer pass removed flags.

## Testing Strategy
1. Parser tests: source without `package` fails with clear parse errors.
2. Command tests: `tool` subcommands succeed with explicit packages and explicit inputs.
3. Option parsing tests: `--package_root` and `--search` are rejected.
4. Integration tests/scripts: updated commands run without removed flags.
5. Regression tests: unknown import errors still provide actionable guidance (minus package_root hint).

## Acceptance Criteria
1. Files cannot compile based on filesystem-derived package names; explicit `package` declaration is required.
2. No production path calls `Package.parser`/`headerParser` with a path-derived default package.
3. `--package_root` and `--search` are removed from tool CLI parsing.
4. `PlatformIO.pathPackage` and related implicit naming logic are removed from production code.
5. All in-repo `.bosatsu` files include explicit package declarations.
6. JS API/UI wrappers and CI/helper scripts no longer pass removed flags.
7. Documentation examples no longer mention `--package_root` for normal workflows.
8. Tests previously asserting implicit behavior are replaced with explicit-declaration behavior.

## Risks And Mitigations
1. Breaking existing user scripts that pass removed flags.
- Mitigation: clear release notes with migration examples; ensure parser errors clearly point to missing package declarations.

2. Users relying on `--search` for transitive import discovery may see missing import failures.
- Mitigation: document `--input_dir` / `--input_all_subdir` as the explicit replacement workflow.

3. Hidden fixture dependencies on implicit naming may break CI unexpectedly.
- Mitigation: repository-wide audit for missing `package` declarations and for `--package_root`/`--search` usage before merge.

4. Error-message regressions from option and hint removal.
- Mitigation: update targeted tests in `ToolAndLibCommandTest` and related suites.

## Rollout Notes
1. Ship as a single breaking change on `main` with explicit migration notes.
2. Migration guidance for users:
- Add `package <Name>` at the top of each `.bosatsu` file.
- Remove `--package_root` and `--search` from command invocations.
- Use explicit input selection (`--input`, `--input_dir`, `--input_all_subdir`) to include needed sources.
3. No runtime/state migration is required; this is parser and CLI surface cleanup plus fixture/doc updates.
