---
issue: 2239
priority: 3
touch_paths:
  - docs/design/2239-type-aliases.md
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/Package.scala
  - core/src/main/scala/dev/bosatsu/Shape.scala
  - core/src/main/scala/dev/bosatsu/KindFormula.scala
  - core/src/main/scala/dev/bosatsu/ExportedName.scala
  - core/src/main/scala/dev/bosatsu/Referant.scala
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/rankn/ParsedTypeEnv.scala
  - core/src/main/scala/dev/bosatsu/rankn/TypeEnv.scala
  - core/src/main/scala/dev/bosatsu/rankn/Infer.scala
  - core/src/main/scala/dev/bosatsu/rankn/TypeAlias.scala
  - core/src/main/scala/dev/bosatsu/ProtoConverter.scala
  - core/src/main/scala/dev/bosatsu/library/ApiDiff.scala
  - proto/src/main/protobuf/bosatsu/TypedAst.proto
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala
  - core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala
  - core/src/test/scala/dev/bosatsu/library/ApiDiffTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-24T02:43:02Z
---

# Add Transparent Type Aliases

_Issue: #2239 (https://github.com/johnynek/bosatsu/issues/2239)_

## Summary

Introduce transparent top-level type aliases with alias-aware kind/variance solving, cross-package/interface metadata, and `Infer.scala` support for saturated expansion plus higher-kinded extensional comparison so aliases like `Baz[a] = Quux[a, Int]` work without requiring user-visible type lambdas.

## Context
Bosatsu currently has only three top-level type statements: `struct`, `enum`, and `external struct`. `Statement.scala` parses them, `SourceConverter.scala` lowers them into `ParsedTypeEnv`, and `Package.scala` runs `Shape.solveAll` plus `KindFormula.solveShapesAndKinds` over `ParsedTypeEnv.allDefinedTypes` before building a `TypeEnv` and invoking `Infer.runFully`.

Several current implementation details matter for aliases:
1. `SourceConverter` and `TypeEnv` only model `DefinedType` values plus external value signatures.
2. `external struct` is already represented as a `DefinedType` with no constructors, and `Package.exportedTypeEnv` treats constructorless exported types as opaque.
3. `Infer.Env` knows about constructor functions and a `Type.Const.Defined -> Kind` map, but it has no alias table and no way to compare higher-kinded synonyms extensionally.
4. Interface/library serialization and API diffing only know about `DefinedType` today.

That means aliases are not just a parser change. They affect name resolution, kind/variance solving, import/export surfaces, library metadata, and the way `Infer.scala` decides type equality.

## Goals
1. Support transparent aliases with syntax in the shape proposed by the issue, e.g. `Foo = Bar[Int]` and `Baz[a: +*] = List[a]`.
2. Allow aliases to be exported and imported across packages and libraries as first-class type names.
3. Reuse current type-parameter rules: explicit parameter lists control order and phantoms, while omitted ones can still be inferred from usage.
4. Support higher-kinded aliases such as `Baz[a] = Quux[a, Int]`.
5. Keep runtime representation, pattern matching, and codegen unchanged.
6. Avoid requiring user-visible type-lambda syntax in the same change if the compiler can handle alias equivalence internally.

## Non-goals
1. No opaque alias or newtype feature in this issue.
2. No value-level aliasing.
3. No new constructor or value namespace entry for an alias name.
4. No change to runtime data representation.
5. No requirement that every user-facing rendered type preserve alias spelling if internal canonicalization prefers expanded types.

## Proposed Design
### 1. Add a dedicated top-level alias statement
Add a new `Statement.TypeAlias(name, typeArgs, body)` form in `Statement.scala`.

Parsing rules:
1. Syntax is `Constructor [typeParams]? = TypeRef`.
2. The right-hand side is parsed with `TypeRef.parser`, not `Declaration.parser`.
3. The alias parser must run before the generic binding parser, because `Foo = ...` currently falls through to pattern binding.
4. This intentionally reserves top-level uppercase `Foo = ...` for aliases rather than zero-argument constructor-pattern bindings. That is an acceptable compatibility tradeoff because the pattern form has an unambiguous `match` rewrite.

Printing rules:
1. Aliases render back to the same surface form.
2. `export Foo` remains the way to export an alias.
3. `export Foo()` remains constructor export syntax and is invalid for aliases.

### 2. Model aliases separately from `DefinedType`
Do not encode aliases as constructorless `DefinedType`.

Instead, add a new `rankn.TypeAlias[A]` carrying:
1. `packageName`
2. `name`
3. `annotatedTypeParams`
4. `rhs: Type`

Then extend:
1. `ParsedTypeEnv` with `typeAliases`
2. `TypeEnv` with `typeAliases`
3. `TypeEnv.toKindMap` to include both data types and aliases

This separation is important because constructorless `DefinedType` already means “opaque/external data type” in the current compiler. Aliases need the opposite behavior: they are always transparent and must retain an expansion body.

### 3. Aliases are always transparent across package boundaries
For this issue, aliases should be transparent.

Concretely:
1. Importing an alias gives the consumer its kind and expansion body.
2. Exporting an alias never creates an opaque surface similar to exporting only a type name from a struct or enum.
3. Alias names live only in the type namespace. They do not introduce constructors or values.

Implementation implications:
1. `Referant` needs a new alias variant.
2. `ExportedName` and import resolution must treat aliases as exported type names.
3. `Package.exportedTypeEnv` must preserve current opacity rules for real data types while always including aliases transparently.
4. `PackageCustoms.checkValuesHaveExportedTypes` must treat exported aliases as satisfying public-type references in exported value types.

### 4. Reuse current type-parameter inference rules for aliases
Alias parameter handling should mirror current struct and enum behavior in `SourceConverter.scala`.

Rules:
1. If the alias has no explicit type-parameter list, infer free type variables from the right-hand side in left-to-right order.
2. If the alias has an explicit parameter list, it must be a superset of the discovered free variables so the declaration can control order and introduce phantom parameters.
3. Explicit kind and variance annotations are validated against the solved result.
4. Missing kind and variance annotations are inferred.

This keeps aliases consistent with the rest of the language and lets existing parameter-order and phantom-parameter rules carry over.

### 5. Generalize shape and kind solving from data declarations to all type statements
`Shape.scala` and `KindFormula.scala` currently solve only `DefinedType` declarations. Alias bodies need to participate in the same pass.

The cleanest approach is to generalize the local type-declaration solver so both data definitions and aliases contribute constraints:
1. Data definitions continue using constructor-field constraints.
2. Aliases contribute constraints directly from their `rhs` type expression.
3. Alias solving should reuse the existing arbitrary-type walkers already present in `Shape.shapeOfType` and `KindFormula.addTypeConstraints`-style logic, rather than pretending aliases are constructors.

Variance behavior:
1. Yes, variance can be inferred for aliases.
2. The same variance lattice already used by `KindFormula` is sufficient.
3. An alias parameter is covariant, contravariant, invariant, or phantom based on how it appears in the solved `rhs` kind formula.
4. Explicit variance annotations remain checked, not trusted blindly.

### 6. Reject recursive alias cycles explicitly
Pure alias cycles should be illegal.

At minimum, reject:
1. `Foo = Foo`
2. `Foo = Bar` and `Bar = Foo`
3. Cycles that pass through data constructors but return to an alias without adding new structure, e.g. `Foo = List[Foo]`

A practical implementation is:
1. After converting alias right-hand sides to `Type`, build a dependency graph over local alias names using `Type.constantsOf`.
2. Topologically sort it.
3. Emit a dedicated source-conversion or package error when a cycle is found.
4. Only build the final alias environment from the acyclic portion so unrelated lets can still typecheck.

### 7. Extend `Infer.scala` with alias-aware comparison rather than source-level type lambdas
The issue’s hardest case is `Baz[a] = Quux[a, Int]`.

A fully saturated use such as `Baz[String]` can be expanded by substituting `String` into the alias body. The difficult case is comparing the bare higher-kinded alias `Baz` itself against other higher-kinded types.

The recommended design is:
1. Add an alias table to `Infer.Env`.
2. Add a helper that expands any saturated alias spine:
   - collect `(head, args)` with `Type.unapplyAll`
   - if `head` is an alias and it has received enough arguments for its declared arity, substitute those arguments into the alias body
   - rebuild with any remaining arguments
   - repeat until the head is no longer an expandable alias
3. When `Infer` must compare two types whose kind is not `Type`, eta-expand both sides with fresh rigid arguments until the comparison reaches kind `Type`, then run the existing subsumption and unification logic on the instantiated result types.

This solves the `Baz[a] = Quux[a, Int]` problem without requiring user-visible type-lambda syntax or a new public `Type` AST node in the same PR. The compiler learns higher-kinded alias equivalence internally by comparing aliases extensionally.

### 8. Normalize saturated aliases before downstream runtime-oriented passes
Downstream passes such as totality, inhabitedness, and value or document rendering mostly care about concrete data-type heads, not alias spelling.

After inference succeeds:
1. Expand saturated aliases through typed expression types and the internal `TypeEnv`.
2. Keep alias declarations in the package and interface environment for import/export and library serialization.
3. Run totality and similar runtime-facing passes on the normalized view.

This keeps alias support localized to the front end, type environment, and interface metadata instead of forcing every downstream pass to understand transparent synonyms.

## Implementation Plan
1. Add `Statement.TypeAlias` plus parser and document support in `core/src/main/scala/dev/bosatsu/Statement.scala`.
2. Add a new `rankn.TypeAlias` model and extend `ParsedTypeEnv` and `TypeEnv` to carry aliases.
3. Update `SourceConverter.scala` to:
   - collect local alias names in type-name resolution
   - lower alias statements into `TypeAlias`
   - infer and validate explicit type parameters for aliases
   - detect duplicate alias and type-name collisions
   - detect alias cycles
4. Generalize `Shape.scala` and `KindFormula.scala` so alias bodies participate in shape, kind, and variance solving alongside structs, enums, and external structs.
5. Update `Package.scala`, `ExportedName.scala`, `Referant.scala`, `PackageCustoms.scala`, and `PackageError.scala` for alias-aware export, import, customs, and diagnostics handling.
6. Extend `Infer.scala` with:
   - alias environment input
   - saturated alias expansion
   - eta-expansion-based comparison for higher-kinded aliases
7. Add alias serialization and compatibility handling in `ProtoConverter.scala`, `proto/src/main/protobuf/bosatsu/TypedAst.proto`, and `core/src/main/scala/dev/bosatsu/library/ApiDiff.scala`.
8. Add parser, package, inference, proto-roundtrip, and API-diff regression tests.
9. Update the language guide after the compiler behavior is stable.

## Testing Strategy
1. Parser coverage in `core/src/test/scala/dev/bosatsu/ParserTest.scala`:
   - `Foo = Bar[Int]`
   - `Baz[a] = List[a]`
   - `Baz[a: +*] = List[a]`
   - rejection of malformed alias syntax
2. Package and type-environment coverage in `core/src/test/scala/dev/bosatsu/PackageTest.scala`:
   - exported alias imported from another package
   - alias and type-name collision rejection
   - alias cycle rejection
3. Inference coverage in `core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala`:
   - ground alias expansion
   - alias inside constructor fields and function annotations
   - higher-kinded alias `Baz[a] = Quux[a, Int]`
   - missing variance annotations inferred from alias bodies
   - explicit variance or kind mismatches rejected
4. Serialization coverage in `core/src/test/scala/dev/bosatsu/ProtoConverterTest.scala`:
   - package and interface round-trip retains exported aliases
5. API-compatibility coverage in `core/src/test/scala/dev/bosatsu/library/ApiDiffTest.scala`:
   - adding an alias is tracked as a public API change
   - changing an alias kind or right-hand side is treated as a breaking public API change

## Acceptance Criteria
1. Bosatsu accepts top-level type aliases with the surface form `Foo = Bar[Int]` and `Baz[a] = List[a]`.
2. Alias names occupy the type-name namespace only and do not introduce constructors or values.
3. Exported aliases can be imported from another package and remain transparent there.
4. Missing kind and variance annotations on alias parameters are inferred, and explicit annotations are validated.
5. Higher-kinded aliases such as `Baz[a] = Quux[a, Int]` typecheck in positions that require `* -> *`.
6. Alias cycles are rejected with a dedicated diagnostic.
7. Internal inference and subsumption treat alias-expanded and non-alias-expanded types as equivalent where the alias definition says they are.
8. Totality, codegen, and runtime representation require no new alias-specific runtime behavior.
9. Package and interface protobuf round-trips preserve exported aliases.
10. Public API diffing notices alias additions, removals, kind changes, and right-hand-side changes.

## Risks And Mitigations
1. Risk: parsing `Foo = ...` as an alias steals a rarely-used constructor-pattern binding form.
Mitigation: give aliases precedence, document the tradeoff, and add regression tests so the new grammar is intentional rather than accidental.

2. Risk: reusing constructorless `DefinedType` would silently turn exported aliases into opaque types.
Mitigation: keep aliases in a separate model with their own export and import path.

3. Risk: higher-kinded alias comparison in `Infer.scala` becomes incomplete or expensive.
Mitigation: compare higher-kinded aliases extensionally with fresh rigid arguments, memoize saturated alias expansion per spine, and reject alias cycles before inference.

4. Risk: old library or interface protobuf data becomes incompatible or loses alias metadata.
Mitigation: land the schema and `ProtoConverter` changes in the same PR and cover them with round-trip tests.

5. Risk: downstream passes mis-handle alias-headed saturated types.
Mitigation: normalize saturated aliases in the internal typed env and program before totality and other runtime-oriented checks.

## Rollout Notes
1. Land parser, source conversion, kind solving, infer, and interface or protobuf changes together; partial landing would create broken cross-package behavior.
2. Regenerate protobuf outputs and treat previously serialized compiled-library metadata as incompatible with alias-aware metadata.
3. No runtime or codegen migration is expected because aliases are transparent and normalize to existing type shapes.
4. Update the language guide once the compiler behavior and error messages are settled.
