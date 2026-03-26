---
issue: 2253
priority: 3
touch_paths:
  - docs/design/2253-design-required-declarations-for-exported-dependency-visibility.md
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/Parser.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/PackageMap.scala
  - core/src/main/scala/dev/bosatsu/tool/PackageResolver.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/main/resources/bosatsu/predef.bosatsu
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-26T05:00:03Z
---

# Design required `exposes` declarations for exported dependency visibility

_Issue: #2253 (https://github.com/johnynek/bosatsu/issues/2253)_

## Summary

Add a required package-header `exposes` declaration for every exported package, define it as the typed exported visible-package set normalized by removing the package itself and `Bosatsu/Predef`, and validate it during source compilation with copy-pasteable mismatch diagnostics. Keep library assembly and binary interfaces unchanged, reusing the existing visibility computation as the semantic source of truth and treating rollout as a breaking source-language change.

## Context

Bosatsu source already makes several interface facts locally visible at the top of a file:

- the package name,
- the directly imported names and packages,
- the explicitly exported names.

After typechecking, the compiler also already knows which packages become visible through the exported API. That logic exists today in package export handling and is consumed later by library assembly, especially in `LibConfig.validatePacks`, which rejects exported APIs that mention private-dependency packages or private same-library packages.

The missing piece is a required source declaration for that visibility. Today a reader can inspect a header and still not know which package dependencies leak through the exported API without asking the compiler. That is the gap this design closes.

This design deliberately keeps the feature source-level and package-level. It does not invent a new notion of public API visibility; it makes the existing compiler-computed notion explicit in source in a normalized form that is practical for humans.

## Goals

1. Require every package with exports to declare which packages are exposed by its exported API.
2. Make the declaration compiler-checked, not documentary.
3. Run the check only after typechecking has enough information to know the real exposed set.
4. Error on missing declarations, extra declarations, and duplicate package names.
5. Keep the declaration in the package header so the public surface is recoverable from the top of the file.
6. Reuse the compiler's existing exported-API visibility computation so there is one semantic source of truth.
7. Preserve current library assembly checks as a later backstop.

## Non-goals

1. Changing how Bosatsu computes exported API visibility.
2. Replacing library-level public/private dependency validation.
3. Adding new metadata to package interfaces or `.bosatsu_lib` artifacts.
4. Making exposure tracking import-local or opt-in.
5. Solving broader API-diff or docs-rendering problems in this issue.

## Chosen Syntax

Add a new header declaration:

```bosatsu
exposes Foo/Bar, Baz/Qux
```

Like `export`, it also supports parenthesized multiline form:

```bosatsu
exposes (
  Foo/Bar,
  Baz/Qux,
)
```

The required spelling for the empty set is:

```bosatsu
exposes ()
```

### Why this syntax

This fits the current header style best.

- `package`, `from ... import ...`, and `export ...` are all header declarations rather than modifiers buried in later syntax.
- Exposure is a whole-package property of the exported API, not a property of any single import.
- The compiler only knows the real set after typechecking exported types, so import-time syntax such as `visibly import` would be the wrong abstraction.
- A flat declaration keeps the "truth at the top of the file" property stronger than a block form.

### Rejected alternatives

`from Foo/Bar visibly import ...`

- Rejected because visibility is not determined by import sites. A package can import many things privately and still expose none of them, or expose a package only through inference on exported values.

`exposes:` block syntax

- Rejected because the current package header is line-oriented and compact. A block adds more parser and formatting surface without improving the core semantics.

Tying `exposes` to each `export`

- Rejected because exposure is defined over the union of the whole exported API. The source of truth should be a single whole-package declaration.

## Placement And Header Rules

Bosatsu today parses a header as package declaration, zero or more imports, an optional export declaration, then the body. This design extends that shape with an optional `exposes` declaration that lives in the same header region.

The rules are:

1. `exposes` is a new top-level header declaration.
2. It appears after `export` and before the first body statement.
3. A package with one or more exported names must have exactly one `exposes` declaration.
4. A package with no `export` declaration must not have `exposes`.
5. `exposes` is a set declaration. Order is ignored by checking. Duplicate package names are errors.
6. The empty set must be written explicitly as `exposes ()`.

This keeps the header readable in a stable order:

```bosatsu
package My/Lib
from Dep/Util import PublicType, make
export Public(), build
exposes Dep/Util
```

## Exact Meaning Of "Exposed"

The declaration should reflect a normalized user-facing subset of the compiler's current visible-package computation.

Define the actual exposed set as:

```scala
rawVisible = typedExports.flatMap(_.tag.depPackages).distinct
actualExposes = rawVisible.filterNot(p => p == thisPackage || p == PackageName.PredefName).sorted
```

This is intentionally not a second semantic system. It is the current exported referant visibility computation with only two normalizations:

- remove the package itself,
- remove `Bosatsu/Predef`.

### Consequences of that definition

Exported value types

- Count. The value's fully inferred exported type is part of the public API, even when the foreign package only appears through inference.

Exported type definitions

- `export Foo` counts only the exported type name itself. For a local type, that normalizes away to nothing because the package does not list itself.
- `export Foo()` counts the type and its constructors. Foreign packages mentioned in constructor field types therefore count.

Opaque exported types

- Do not expose constructor-field dependency packages unless constructors are exported. This matches current `Referant.DefinedT` versus `Referant.Constructor` behavior.

Exported type aliases

- Count packages mentioned in the alias right-hand side.

`external` values and types

- Participate exactly the same way as non-`external` exports because the check runs on typed exported referants, not on surface syntax alone.

Renamed imports and aliases

- Do not change the declaration. `exposes` always names canonical package paths, never local aliases.

The package itself

- Never appears in `exposes`.

`Bosatsu/Predef`

- Never appears in `exposes`, even though it may appear in the raw visibility set for exported values involving `Int`, `String`, `Bool`, and similar predef types.

Same-library packages

- Do count if they are not the current package. This is a package-level public-surface declaration, not only an external-library declaration. If package `A` exports a type from sibling package `B`, readers should see that fact in `A`'s header.

## Compiler Architecture

### 1. Extend parsed header state

The parser must preserve three distinct states:

- no `exposes` declaration was written,
- `exposes ()` was written,
- `exposes ...` with one or more package names was written.

That means the parsed-source representation needs an optional declaration that preserves absence versus explicit empty. The exact encoding can be a new parsed-header wrapper or an equivalent field threaded alongside parsed packages, but it must remain source-only and does not need to survive into binary interfaces.

Primary implementation points:

- `Package.headerParser` and `Package.parser`
- package pretty-printing/document rendering in `Package.scala`
- header-only parsing in `PackageResolver.scala`
- parsed source plumbing in `PackageMap.scala`

### 2. Add a shared helper for exposed-package computation

Add one helper that, from typed exports, computes:

- the raw visible package set,
- the normalized `exposes` set,
- a witness map from package name to the exported names that cause that package to appear.

The important design constraint is that source-level checking and later library validation should share the same underlying referant-to-package logic so they cannot silently diverge.

### 3. Check in `PackageCustoms.assemble`

The new source-level check belongs after export referants are built, because only then does the compiler know the typed exported API.

The intended flow is:

1. Parse header, including optional `exposes`.
2. Typecheck the package as today.
3. Build typed exports with `ExportedName.buildExports`.
4. Compute the normalized actual exposed set from those typed exports.
5. Compare it to the declared set.
6. Emit hard package errors on mismatch.
7. Continue to run existing customs such as private-type-escape checks and unused-import checks.

`PackageCustoms.assemble` is the right location because it already sits at the boundary between typed body inference and final typed package assembly.

### 4. Keep binaries unchanged

No protobuf or interface-format change is required.

The declaration is source-facing and enforced before interface emission. Package interfaces and `.bosatsu_lib` artifacts already encode enough typed information for existing library visibility checks, so this feature does not need to serialize the source declaration.

That keeps the migration source-breaking but not binary-format-breaking.

## Checking Rules And Diagnostics

### Rules

1. If a package has exports and no `exposes` declaration, compilation fails.
2. If a package has no exports and does declare `exposes`, compilation fails.
3. If a declaration contains duplicate package names, compilation fails.
4. The declared set is compared to the normalized actual set as a set, not as an ordered list.
5. Missing packages are errors.
6. Extra packages are errors.
7. Declaring the current package or `Bosatsu/Predef` is therefore always an extra-package error.

### Diagnostic shape

Diagnostics should be easy to act on without additional tooling.

For a mismatch, report:

- the declared set,
- the actual set,
- missing packages,
- extra packages,
- a canonical replacement line, using sorted package names,
- witness exported names for each missing package.

Example shape:

```text
package My/Lib has incorrect exposes declaration.
declared: exposes ()
actual:   exposes Dep/Util
missing:  Dep/Util
witnesses for Dep/Util: build, PublicAlias
suggested replacement: exposes Dep/Util
```

### Diagnostic location strategy

In v1, diagnostics should point at the `exposes` line when present, or at the package header when the declaration is missing. The error text should name the exported witnesses that cause the package to escape.

This design does not require region-tracking every exported item immediately. If that later proves too coarse, a follow-up can add region-carrying parsed export nodes. The first implementation should prioritize correct semantics and actionable witness text.

## Interaction With Library Public/Private Dependencies

`exposes` is the source-visible counterpart of the existing library-level checks, but it does not replace them.

The relationship is:

- package-level `exposes` tells the truth about this package's exported API surface,
- library-level public/private dependency checks still decide whether that surface is allowed for a given library build.

Consequences:

- If a package exposes a package from a private dependency, the source declaration may still correctly list it, but library assembly must continue to reject the library build.
- If a package exposes a private same-library package, `exposes` makes that leak explicit in source, and `LibConfig.validatePacks` must still reject it unless that sibling package is exported.
- `public_deps` and `private_deps` semantics do not change.
- Semver validation does not need new rules because it already reasons from exported package interfaces and public dependencies.
- Docs generation does not require changes for correctness. Rendering `exposes` in generated docs can be a later enhancement, not part of this issue.

## Migration And Rollout

This is a backwards-incompatible source-language change.

### Migration strategy

1. Land parser, checker, docs, tests, and in-repo source updates together.
2. Update all exported packages in the repository to include `exposes`, including `exposes ()` where appropriate.
3. Update the bundled predef source in the same change so internal compiler bootstrapping still succeeds.
4. Make the error message print the exact canonical replacement line so most packages can be fixed with a direct copy-paste.

A dedicated auto-rewriter is not required for the first implementation. The compiler already has enough information to print the exact declaration users should add. If external migration pain turns out to be high, a follow-up command can generate or apply those fixes, but this feature should not wait on that extra tooling.

### Compatibility implications

- Recompiling existing source with the new compiler may fail until `exposes` is added.
- Existing compiled libraries remain readable because the binary formats do not change.
- This should ship in the next major compiler/language release, not as a warning-only lint mode.

## Acceptance Criteria

1. The source grammar accepts a new header declaration `exposes`, including multiline parenthesized form and the explicit empty spelling `exposes ()`.
2. Any package with exports must contain exactly one `exposes` declaration.
3. Any package without exports rejects `exposes` as an invalid extra header declaration.
4. The actual set compared against the declaration is computed from typed exported referants using the current visibility logic, normalized by removing the package itself and `Bosatsu/Predef`.
5. Exported value types, exported type aliases, exported constructors, inferred foreign types on exported values, and `external` declarations all participate according to the rules above.
6. `export Type` and `export Type()` behave differently in the `exposes` set in the same way they already behave differently in export visibility.
7. Missing packages, extra packages, and duplicate declared packages are all hard compile errors.
8. Mismatch diagnostics include the actual set, the declared set, a canonical suggested replacement line, and exported-name witnesses for missing packages.
9. Same-library packages other than the current package are included when they appear in the exported API.
10. `Bosatsu/Predef` and the package itself never appear in the required declaration.
11. Existing library assembly validation continues to reject leaked private dependencies and leaked private same-library packages.
12. No protobuf or package-interface schema changes are required.
13. Parser tests, package-visibility tests, error-message tests, and library-integration tests cover the new behavior.
14. The bundled predef source and the language guide are updated to reflect the new required header declaration.

## Risks And Mitigations

1. Risk: source-level and library-level visibility computations drift apart.
   Mitigation: implement the new check on top of the same export-to-package helper logic used by existing visibility code, not by duplicating a second algorithm.

2. Risk: users find the new breaking change noisy, especially for packages whose exposed set is empty.
   Mitigation: require the explicit but simple spelling `exposes ()` and make diagnostics print the exact replacement line.

3. Risk: mismatch diagnostics are frustrating if they do not show why a package escaped.
   Mitigation: include exported-name witnesses for each missing package in v1; add finer region tracking only if needed later.

4. Risk: users confuse package-level `exposes` with library-level public dependencies.
   Mitigation: document clearly that `exposes` names packages visible in this package's exported API, while library config still governs which dependencies are public or private.

5. Risk: parser and parsed-source plumbing changes touch several core code paths.
   Mitigation: keep the new declaration source-only, avoid binary-format changes, and cover header parsing plus in-repo predef compilation in tests.

## Rollout Notes

- Land this as one breaking-source PR that updates parser support, package checking, diagnostics, predef source, tests, and docs together.
- Keep package interfaces and library artifacts unchanged so already-published dependencies remain consumable.
- Update repository packages in the same change so `main` stays buildable immediately after merge.
- Call out the language break explicitly in release notes and recommend treating the compiler release as a major-version upgrade.
