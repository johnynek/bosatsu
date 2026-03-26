---
issue: 2254
priority: 3
touch_paths:
  - docs/design/2254-design-required-exposes-declarations-for-exported-dependency-visibility.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala
  - core/src/main/scala/dev/bosatsu/tool_command/DepsCommand.scala
  - core/src/main/scala/dev/bosatsu/library/LibConfig.scala
  - core/src/main/resources/bosatsu/predef.bosatsu
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-26T05:01:11Z
---

# Design required `exposes` declarations for exported dependency visibility

_Issue: #2254 (https://github.com/johnynek/bosatsu/issues/2254)_

## Summary

Require a checked header-level `exposes` declaration for every exported package, define it as the normalized set of dependency packages visible from typed exports, and align source checking with existing library public/private dependency validation without changing runtime or interface serialization.

## Context

Bosatsu already computes the packages that escape through a package's exported API. `Package.visibleDepPackages` derives that set from typed exported referants, and `LibConfig.validatePacks` already uses it to reject exported APIs that mention private dependencies or private same-library packages. What is missing is a required source declaration that makes this fact visible in the header itself.

The design should make exported dependency visibility part of the source interface, not a derived fact hidden behind later tooling. The declaration therefore needs to live in the header, but the check must run after export assembly, when exported values, constructors, aliases, and inferred types have all been resolved.

## Goals

1. Require every package with any `export` declaration to declare the dependency packages visible from its exported API.
2. Make the declaration exact and checked: missing packages and extra packages are both hard errors.
3. Reuse current compiler visibility semantics wherever possible so library assembly and source checking stay aligned.
4. Keep the declaration package-level and header-local.
5. Keep migration mechanical for existing code.

## Non-goals

1. This is not an opt-in lint or documentation convention.
2. This does not change runtime behavior, code generation, or interface protobuf format.
3. This does not replace library assembly checks around public and private dependencies.
4. This does not add per-import visibility annotations.

## Syntax And Placement

### Chosen syntax

Add a new header declaration `exposes`.

Accepted surface forms:

- `exposes Foo/Bar, Baz/Qux`
- `exposes (...)` using the same parenthesized multiline list style Bosatsu already uses for `import` and `export`
- `exposes ()` for an explicitly empty set

This matches Bosatsu's existing header vocabulary: `package`, `from ... import ...`, `export`, and now `exposes`. It keeps the public surface recoverable from the top of the file without tying exposure to a specific import or export item.

### Placement rules

- `exposes` is a top-level header declaration, not a statement in the body.
- There is exactly one `exposes` declaration per package.
- It appears after the last `export` line and before the first body statement.
- If a package has at least one `export` declaration, it must have exactly one `exposes` declaration.
- If a package has no `export` declarations, `exposes` is rejected.
- Multiple `export` lines still mean one package-level `exposes` declaration whose meaning is the union of all exports.

### Why not the alternatives?

- A block form such as `exposes:` is workable, but Bosatsu headers are currently flat keyword declarations rather than indentation blocks. Reusing the existing comma or parenthesized list style keeps the feature visually consistent and parser-local.
- An import modifier such as `from Foo/Bar visibly import ...` is the wrong abstraction. Exposure is not an import property; it depends on the typed exported API, including inferred value types, transparent aliases, constructor fields, and opaque type exports.
- Attaching exposure annotations to individual `export` items duplicates a package-level fact and becomes awkward when multiple exports jointly expose the same dependency.

## Meaning Of Exposed

Define the checked set as:

`actualExposes(pack) = normalize(pack.visibleDepPackages)`

where `normalize`:

- removes the package's own name
- removes `Bosatsu/Predef`
- sorts and deduplicates package names for canonical comparison and diagnostics

This intentionally reuses the compiler's current exported-visibility computation and only strips entries that are not user-facing dependencies. `visibleDepPackages` includes the package itself and `Bosatsu/Predef` for internal validation reasons; `exposes` should describe dependency visibility, not self-reference or implicit predef availability.

The resulting rules are:

- Exported value types count, using the fully inferred exported type, not just explicit source annotations.
- Exported transparent type aliases count packages that appear in the alias right-hand side.
- Exporting only a local type name keeps that type opaque; foreign packages used only inside hidden constructor fields do not count.
- Exporting constructors, or `Foo()`, counts packages that appear in constructor field types.
- `external` values count through their types. `external struct` exported as a type name behaves like any other opaque exported type.
- Renamed imported identifiers do not affect the declaration; `exposes` always names canonical package paths.
- The package does not count as exposing itself.
- `Bosatsu/Predef` does not count.
- Other packages in the same library do count if they are distinct package names, because they are still part of the exported package dependency surface.

This definition is the source-visible counterpart of the existing library-level notion, but with a user-facing normalization step.

## Checking And Diagnostics

The compiler should check `exposes` after export assembly, when `ExportedName.buildExports` has produced typed exported referants and the package's actual exposed dependency set is known.

Rules:

1. No exports and no `exposes`: valid.
2. No exports and any `exposes`: error.
3. Exports present and no `exposes`: error.
4. Exports present and `exposes` present: compare declared and actual sets exactly.
5. Any missing package is an error.
6. Any extra package is an error.

Comparison should use canonical sorted distinct sets so diagnostics and suggested fixes are stable even if source order differs.

Diagnostics should be one hard error class with enough detail to fix the header quickly:

- show the declared set and the actual set
- show a canonical replacement line such as `exposes Foo/Bar, Baz/Qux` or `exposes ()`
- for each missing package, list the exported names that cause that package to escape

The last point matters for inferred cases. If `Dep/Api` appears only because an exported value's inferred type mentions it, the error should still say which exported binding exposed it.

The first implementation does not need whole-body blame tracking. The existing exported referants are enough to compute a useful `PackageName -> exported names` cause map.

## Interaction With Library Validation

`exposes` is the source-visible counterpart to the existing library assembly checks in `LibConfig.validatePacks`; it does not replace them.

The intended split is:

- Source checking answers whether the package header truthfully declares which other packages its exported API exposes.
- Library assembly answers whether those exposed packages are allowed by exported-package and public-dependency rules for the library.

That means:

- Declaring `exposes Dep/Private` does not make a private dependency valid. Assembly must still reject exported packages that expose private dependencies.
- Declaring `exposes My/Internal` does not make a non-exported same-library package public. Assembly must still reject that leak unless the package is itself exported.
- `LibConfig.validatePacks` should reuse the same normalized helper instead of open-coding raw `visibleDepPackages` handling. That keeps source checks, library validation, and human-facing docs aligned.

No interface protobuf or API-diff schema change is required for this feature. The declaration is redundant once the package has already been compiled successfully, because the actual exposed set is derivable from the exported typed API. Semver behavior remains driven by actual API changes and existing public-dependency rules, not by storing the header declaration in compiled artifacts.

## Implementation Plan

1. Extend header parsing and header data flow.
- Update `core/src/main/scala/dev/bosatsu/Package.scala` to parse one optional `exposes` declaration in the header, including `exposes ()`.
- Thread the parsed declaration through source-package loading in `core/src/main/scala/dev/bosatsu/PackageMap.scala` and `core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala`.
- Update header-only consumers such as `core/src/main/scala/dev/bosatsu/tool_command/DepsCommand.scala` for the richer header shape.
- Update in-repo source files with exported headers, especially `core/src/main/resources/bosatsu/predef.bosatsu`.

2. Add a shared normalized exposed-dependency helper.
- Add a helper near `Package.visibleDepPackages` that returns the user-facing exposed dependency set: sorted distinct packages, excluding self and `Bosatsu/Predef`.
- Use that helper for the new source check.
- Switch `core/src/main/scala/dev/bosatsu/library/LibConfig.scala` to the same helper so package-level source checking and library-level validation are definitionally aligned.

3. Add the post-typecheck `exposes` check.
- Extend `core/src/main/scala/dev/bosatsu/PackageCustoms.scala` so `assemble` receives the declared `exposes` set and compares it to the actual set after exports are built.
- Add new hard diagnostics in `core/src/main/scala/dev/bosatsu/PackageError.scala` for:
  - missing required `exposes`
  - `exposes` on a package with no exports
  - declared or actual mismatch
- Include canonical fix text and exported-name causes in the error message.

4. Add regression coverage.
- Parser coverage in `core/src/test/scala/dev/bosatsu/ParserTest.scala` for inline, multiline, and empty `exposes` syntax.
- Semantic coverage in `core/src/test/scala/dev/bosatsu/PackageTest.scala` for opaque exported types, exported constructors, inferred exported values, type aliases, `external` declarations, same-library packages, self, and `Bosatsu/Predef`.
- Error-message coverage in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` for missing and mismatched declarations.
- Library integration coverage in `core/src/test/scala/dev/bosatsu/library/LibConfigTest.scala` and `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala` to confirm the new source feature and existing public or private dependency validation stay aligned.

5. Document the new header rule.
- Update `docs/src/main/paradox/language_guide.md` to describe `exposes` as part of the package interface header and to document `exposes ()` for exported packages with no exposed dependencies.

## Acceptance Criteria

1. Bosatsu accepts a new header declaration `exposes` with inline, parenthesized multiline, and empty-set spellings.
2. Any package with at least one `export` declaration must also declare exactly one `exposes` declaration.
3. A package with no exports cannot declare `exposes`.
4. The checked set is the compiler's exported visible-package computation, normalized by removing self and `Bosatsu/Predef`.
5. Exported inferred value types, transparent type aliases, exported constructors, and `external` value types all participate in the check.
6. Exporting only a local type name does not force dependencies from hidden constructor fields to appear in `exposes`.
7. Missing declarations and extra declarations are hard errors.
8. Diagnostics show the canonical expected `exposes` line and name the exported bindings or types that caused a missing package to escape.
9. Different same-library packages count when they appear in exported APIs; the package itself does not.
10. Existing library assembly checks continue to reject exposure of private dependencies or private same-library packages, using the same normalized exposed-dependency helper.

## Risks And Mitigations

1. Risk: the feature touches parser or header plumbing, source-unit loading, and in-repo predef or test sources, so the implementation surface is wider than a single check.
Mitigation: keep the semantics centered on one shared helper and add parser plus end-to-end regression coverage.

2. Risk: users may be surprised when a package is exposed only through inference from an exported value.
Mitigation: diagnostics should name the escaping exported bindings and print the exact replacement `exposes` line.

3. Risk: opaque or exported-type rules can be confusing if the source feature does not match library validation exactly.
Mitigation: define `exposes` in terms of normalized `visibleDepPackages` and reuse that helper from `LibConfig.validatePacks`.

4. Risk: migration creates broad header churn across existing Bosatsu packages and downstream libraries.
Mitigation: treat the change as a major language break, make the required edit mechanical, and ensure the compiler prints a canonical fix.

5. Risk: authors may think declaring a private dependency in `exposes` makes it acceptable.
Mitigation: document clearly that `exposes` is descriptive, not permissive; library assembly remains authoritative for public or private dependency policy.

## Rollout Notes

- Land this as a breaking language change and call it out in the next major Bosatsu release notes.
- Update all in-repo exported packages in the same change, including predef and test fixtures, so the repository stays self-hosting.
- The migration path for existing code should be:
  1. run `check`
  2. add the exact `exposes ...` line the compiler reports
  3. if the reported package should not be public, change the exported API instead of the declaration
- A dedicated autofix command would be useful but is not required to land the feature if the diagnostic already prints the canonical replacement line.
