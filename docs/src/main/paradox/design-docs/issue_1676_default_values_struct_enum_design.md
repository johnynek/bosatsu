# Issue 1676: Default Values in Struct and Enum Constructors

Status: implemented  
Date: 2026-02-16  
Implemented: 2026-02-17 (PR #1699)  
Issue: <https://github.com/johnynek/bosatsu/issues/1676>

Implementation status: all items in this design were implemented and merged in PR #1699.

## Goal
Add default values for constructor fields so record-style construction can omit fields that have defaults, while preserving current language behavior.

Example:

```bosatsu
struct Record(
  name: Option[String],
  uses: Option[String] = None,
  with: Option[Record] = None,
  run: Option[String] = None,
)

r = Record { name: Some("foo") }
```

This should typecheck as if missing fields were filled by compiler-provided default expressions.

## Explicit V1 Constraints
1. Defaults are applied only for record constructor syntax: `C { field: expr }`.
2. Positional constructor calls are unchanged: `C(expr1, expr2, ...)` gets no default filling.
3. Defaults cannot reference constructor parameters.

Rejected example:

```bosatsu
struct S(a: Int, b: Int = a) # rejected
```

Allowed default dependencies are only:
1. imported values,
2. values defined earlier in the same file,
3. earlier default helpers by source order.

If positional constructor defaults are added in the future, that should be designed together with function default arguments in a separate proposal; that is explicitly out of scope for this doc.

## Non-goals
1. Changing tuple/positional constructor calls (`Record(...)`).
2. Adding named arguments for general function calls.
3. Changing pattern matching semantics.
4. Making constructor shape changes non-breaking for semver.

## Current State
1. Type definitions (`Statement.Struct`, `Statement.EnumBranch`) only store `(fieldName, optionalType)`; no default expression slot.
2. Record construction (`Declaration.RecordConstructor` in `SourceConverter.fromDecl`) requires every constructor field; missing fields produce `SourceConverter.MissingArg`.
3. Constructor metadata in `rankn.ConstructorFn`/`TypedAst.proto` contains field names and types only.
4. API compatibility (`library/ApiDiff.scala` via `LibConfig.validNextVersion`) checks constructor shape/name/type/index changes, but has no default-value diff model.

## Proposed Semantics

### Definition-site syntax
Allow defaults on constructor parameters in both `struct` and `enum` constructors:

```bosatsu
struct S(a: Int, b: Int = 0)

enum E:
  C(x: Int, y: Int = 1)
```

### Construction semantics (record syntax only)
For `C { field1: e1, ... }`:
1. Unknown field names still error.
2. Duplicate/extra field behavior remains as today (no new behavior change in this feature).
3. For each constructor field in declared order:
   1. Use provided value if present.
   2. Else use the field default if one exists.
   3. Else emit existing `MissingArg` error.
4. The result still elaborates to ordinary constructor application with all arguments present.
5. `C {}` is valid syntax. It typechecks iff all required fields are defaulted; otherwise existing missing-field errors are reported.

### Constructor-function calls unchanged
`C(...)` and first-class constructor values keep current arity/type behavior. Missing positional arguments remain errors.

### Pattern matching unchanged
No default filling occurs in patterns. Pattern conversion/totality/match lowering behavior is unchanged.

## Elaboration Model

### 1. Extend constructor metadata
Replace raw `(Bindable, Type)` argument pairs with a param model carrying optional default metadata.

Suggested shape:

```scala
final case class ConstructorParam(
  name: Bindable,
  tpe: Type,
  defaultBinding: Option[Bindable]
)

final case class ConstructorFn[+A](
  name: Constructor,
  args: List[ConstructorParam],
  exists: List[(Type.Var.Bound, A)] = Nil
)
```

`defaultBinding` names a compiler-generated helper binding in the defining package.

### 2. Generation point and scope boundary
Defaults are generated during source-to-core conversion as a conceptual desugaring of the statement stream.

Semantics are:
1. Process top-level statements in source order.
2. When a `struct`/`enum` definition is seen, keep the type definition unchanged.
3. Immediately after that type definition, insert synthetic non-recursive helper bindings for its defaulted fields.

So yes: semantically these names exist immediately after the defining `struct`/`enum` statement.

Consequences:
1. Later top-level statements can see/use those helpers (through constructor metadata), earlier ones cannot.
2. Default expressions can reference earlier top-level values.
3. Defaults may reference earlier defaults from the same type definition under a deterministic order:
   1. `struct`: parameter order.
   2. `enum`: constructor declaration order, then parameter order within each constructor.
4. Self-recursive default helpers are not allowed in v1.

### 3. Generate one helper binding per defaulted field
For each defaulted field, generate a synthetic top-level binding in the defining package using the deterministic API-fingerprint naming rule below.

The helper expression is the parsed default expression and is typechecked against the field type.

This gives:
1. Single evaluation/sharing (not re-elaborated per construction site).
2. A concrete symbol that downstream packages can reference after import of constructor metadata.
3. Normal error reporting at the default expression source region.

### 4. Helper name allocation and uniqueness
Name allocation happens in `SourceConverter` (whole-file context available), before defaults are lowered into helper lets.

#### API-fingerprint naming rule
Each helper name is a deterministic hash of constructor parameter API identity, not of local file context.

Fingerprint input (`DefaultNameV1`):
1. package name,
2. type name,
3. constructor name,
4. parameter index,
5. canonical parameter type digest.

Canonical type digest should be computed from a normalized, alpha-stable encoding of the field type (for example de-Bruijn-style bound-variable numbering) so harmless binder renaming does not perturb helper names.

Suggested emitted bindable:
1. create `Identifier.synthetic("default$" + <digest>)`,
2. where `<digest>` is base32/hex `Blake3(DefaultNameV1 input)`.

Example shape:

```text
_default$8f3c1a7d9b42...
```

#### Why this satisfies semver-stable naming
Under current `ApiDiff` policy, changing any of (type/constructor identity, parameter position, parameter type) is already major-only.

So:
1. patch/minor changes that keep constructor parameter API identity unchanged keep helper names unchanged,
2. helper-name changes imply a major-allowed API change.

Default expression body changes are excluded from the fingerprint, so implementation-only default rewrites do not rename helpers.

#### Collision and shadowing policy
Assumption: `Identifier.synthetic(...)` names are not definable by users in Bosatsu source.

Under that assumption:
1. user-vs-helper collisions cannot occur,
2. user shadowing of helper names cannot occur.

For generated helpers:
1. we generate at most one helper per constructor parameter slot,
2. emitted name is a pure function of `DefaultNameV1` key for that slot,
3. assuming no Blake3 collisions, two different keys cannot produce the same emitted name.

So helper-helper collisions cannot occur under the no-Blake3-collision assumption.

To avoid non-deterministic behavior, we still do **not** fall back to `unusedNames`; any observed duplicate generated helper name should be treated as an internal invariant failure.

### 5. Scope rule for default expressions (v1)
To keep evaluation acyclic and predictable, a default expression may reference:
1. Imported names.
2. Top-level values defined earlier in source order.
3. Earlier generated default helpers.

It may not reference constructor parameters (no dependent defaults in v1).

Rejected example:

```bosatsu
struct S(a: Int, b: Int = a) # rejected
```

This matches a DAG-friendly ordering and avoids introducing implicit call-site scope.

### 6. Record-constructor conversion
In `SourceConverter` record-constructor handling:
1. Look up constructor params with default metadata.
2. Build full positional arg list by field name:
   1. explicit arg
   2. default helper global
   3. `MissingArg`.
3. Emit existing `Expr.buildApp(cons, fullArgs, ...)`.

Because this remains an ordinary application after elaboration, the rest of inference/normalization/codegen stays stable.

## Type Errors and Regions

### Why region capture needs parser/data-model changes
Current constructor args in `Statement.Struct`/`Statement.EnumBranch` are just `(Bindable, Option[TypeRef])`, so there is no per-default region.

To surface precise errors for defaults, we should change parsed arg shape to carry:
1. field name,
2. optional declared type,
3. optional default expression,
4. default-clause region (recommended: region covering `= <expr>`),
5. arg region.

The parser change is to parse default clauses with `.region` and keep that region on the arg node.

### How this plugs into the current typechecker pipeline
Current flow already has good region plumbing:
1. `SourceConverter.toProgram` builds `Expr[Declaration]`.
2. `Infer.typeCheckLets` uses `HasRegion[Declaration]` when creating `Infer.Error`.
3. `PackageError.TypeErrorIn` renders source snippets from those regions via `LocationMap`.

Defaults should use this same path by generating helper lets with region-aware tags.

### Default helper typing strategy
For each defaulted field, generate helper RHS as a typed annotation against the field type:

```bosatsu
__bosatsu_default_Foo_a = (<default-expr> : <field-type>)
```

Region policy:
1. The annotation tag region is the stored default-clause region (`= <expr>`).
2. The inner default expression keeps its own parsed subregions.

Effect with current `Infer` behavior:
1. `checkAnnotated` reports expected-type region from the annotation tag.
2. Found-type region comes from the inner expression.
3. `PackageError.TypeErrorIn` therefore highlights the original default source location, not an invented synthetic location.

Example:

```bosatsu
struct Foo(a: Int = "foo")
```

reports a normal type mismatch at the default clause (expected `Int`, found `String`).

### Non-type errors for defaults
Default-scope policy violations (for example, forbidden forward/default dependency shape) should be emitted as `SourceConverter` errors at the stored default-clause region.

This keeps:
1. parse/scope errors in `SourceConverterErrorsIn`,
2. type mismatches in `TypeErrorIn`,
with both pointing to the same user-visible source location.

## API Compatibility Rules

Compatibility checks live in `library/ApiDiff.scala` and are enforced from `LibConfig.validNextVersion`.

### Existing constructor-shape rules remain
These remain `major`-only:
1. Constructor add/remove.
2. Constructor index change.
3. Param add/remove.
4. Param rename.
5. Param type change.

Reason: positional constructor calls are intentionally unchanged and remain part of API surface.

### New default-specific diffs
Add constructor-param default diffs:
1. `ConstructorParamDefaultAdded` (`None -> Some`) : allowed in `minor` and `major`, disallowed in `patch`.
2. `ConstructorParamDefaultRemoved` (`Some -> None`) : `major` only.

If default exists in both versions, expression/body changes are treated like ordinary value-body changes (not type-shape diffs).
Adding a default stays minor-only even if no constructor type/arity changes are made, because patch requires exactly unchanged API.

## Protobuf and Serialization

### Schema change
Extend `FnParam` in `proto/src/main/protobuf/bosatsu/TypedAst.proto` with a proto3 `int32` pointer to the helper binding name.

Example:

```proto
message FnParam {
  int32 name = 1;
  int32 typeOf = 2;
  int32 defaultBindingName = 3; // 1-based string-table index; 0 means no default
}
```

### Backward compatibility
1. Old serialized artifacts have no field `3`; proto3 default is `0`, interpreted as `None`.
2. New decoder therefore treats all existing libraries/packages as constructors with no defaults.
3. Unknown-field behavior keeps forward-read tolerance for older decoders.

### Converter changes
In `ProtoConverter`:
1. Encode/decode `defaultBindingName` on constructor params.
2. Continue serializing generated helper lets as ordinary package lets.

No change is required to match/pattern protobuf encoding.

## Dependency Visibility
Default expressions may reference imported packages, but those references are contained in generated helper bindings in the defining package. Consumers of the constructor depend only on the defining package’s constructor metadata/helpers, not on direct visibility of the helper’s internal expression dependencies.

This avoids introducing named-argument-like implicit imports at constructor call sites.

## Imports, Exports, and Customs

### Export/import behavior
Bosatsu reminder:
1. `export T` exports only the type name.
2. `export T()` exports the type name plus all constructors.

Defaults follow constructor visibility:
1. Exporting `T` only does not expose constructor defaults.
2. Exporting `T()` includes constructor default metadata and the associated synthetic helper values needed to realize those defaults.
3. Importing constructor names via `T()` automatically links the default behavior for record construction in downstream packages.
4. Default helper names are not a user-facing import surface; they are synthetic implementation details attached to constructor export/linking.

### Customs (`PackageCustoms.allImportsAreUsed`)
No new user-visible import item is introduced for defaults.

Consequences:
1. Unused-import checks remain keyed to the explicit imported constructor/type/value names.
2. Using record construction that relies on defaults still counts as using the constructor import (same as any constructor use).
3. There should be no new false-positive unused-import errors caused solely by default helpers.

### Unused-value reachability (`PackageCustoms.noUselessBinds`)
Bosatsu normally reports unused top-level values unless they are reachable from roots (exports/main/tests/non-binding uses).

For defaults:
1. helper lets are synthetic (`Identifier.synthetic(...)`),
2. they are treated as synthetic exports reachable from exported constructors (`T()` path),
3. thus they are on the export-reachability path under the normal rule,
4. and because they are synthetic, they are excluded from user-facing unused-let diagnostics (`Identifier.isSynthetic`) so implementation details do not surface as lint noise.

## Implementation Plan
1. AST/parser:
   1. Extend `Statement.Struct`/`Statement.EnumBranch` arg model to carry optional default expression.
   2. Update parser/printer for `arg [: Type] [= expr]`.
   3. Allow empty record-construction braces (`C {}`) in parser; rely on existing missing-field checks in conversion.
2. rankn/type metadata:
   1. Extend `ConstructorFn` arg representation with optional `defaultBinding`.
   2. Update `TypeEnv` helpers (`getConstructorParams`, etc.) to expose default metadata.
3. Source conversion:
   1. Generate default helper bindings.
   2. Allocate helper names from `DefaultNameV1` API fingerprint (no freshness fallback).
   3. Enforce default-expression scope rule.
   4. Update record-constructor expansion to fill omitted defaulted fields.
4. Export/linking/customs:
   1. Mark default helpers as synthetic constructor-linked exports when constructors are exported (`T()`).
   2. Ensure reachability roots include these synthetic constructor-linked defaults.
5. Inference/typing:
   1. Typecheck helper bindings against declared field types.
   2. Preserve existing constructor-function typing.
6. API diff:
   1. Add default-added/default-removed diff cases.
   2. Wire semver validity as above.
7. Proto:
   1. Add `int32 defaultBindingName` to `FnParam` (`0` => absent).
   2. Update encode/decode.

## Test Plan
1. Parsing/printing:
   1. Struct and enum defaults roundtrip.
   2. Record-constructor syntax accepts `C {}`.
2. Typing/elaboration:
   1. Record construction succeeds with subset fields when defaults exist.
   2. `C {}` succeeds when all fields have defaults.
   3. Missing non-defaulted field still errors.
   4. Positional constructor call arity behavior unchanged.
3. Pattern matching:
   1. Existing pattern behavior unchanged.
4. Scope restrictions:
   1. Default references to constructor params rejected.
   2. Forward-reference defaults rejected (or reported) per ordering rule.
5. API diff/semver:
   1. Add default => minor-valid, patch-invalid.
   2. Remove default => major-only.
   3. Param add/remove still major-only even if default exists.
   4. Add golden test: unchanged constructor API identity => unchanged generated default-helper names.
6. Protobuf compatibility:
   1. Decode old proto (without field) as no defaults.
   2. Roundtrip new proto with defaults preserved.
7. Naming collisions:
   1. Assert generated helper names are unique for all defaulted params in a package.
   2. Document invariant: synthetic helper names are not user-definable in Bosatsu source.
8. Visibility and reachability:
   1. `export T` does not permit external default-backed construction.
   2. `export T()` does permit external default-backed construction.
   3. No user-visible unused-let errors are produced for synthetic default helpers.
