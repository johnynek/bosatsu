# Issue 356: Struct Update Syntax (`..old`)

Status: proposed  
Date: 2026-02-19  
Issue: <https://github.com/johnynek/bosatsu/issues/356>

## Goal
Add Rust-style struct update syntax:

```bosatsu
struct Foo(a: Int, b: Int, c: Int)
oldFoo = Foo(1, 2, 3)
newA = 12
foo1 = Foo { a: newA, ..oldFoo }
```

with source-level desugaring in `SourceConverter`, no new core runtime features, and clear rejection rules for non-struct-like constructors.

## Assessment of the Current Plan
The current assessment is mostly correct:

1. `Declaration`/parser changes are required first, because current syntax only supports record fields (`field` / `field: expr`) and has no `..base` form.
2. `SourceConverter` is the right place for desugaring, because it already has constructor/type metadata and already lowers record constructors.
3. The key eligibility check should be based on constructor-to-type lookup and confirming the type has exactly one constructor.

Two important caveats:

1. `SourceConverter` must use `getConstructor` (not only `getConstructorParams`) so it can inspect the owning `DefinedType` and its full constructor list.
2. For eligible updates, desugar to a single-branch `match` (no fallback `_` branch), otherwise totality checking may report the fallback as unreachable for single-constructor types.

## Non-goals
1. Supporting updates for enums/types with multiple constructors.
2. Changing general record-constructor semantics (`Foo { a: 1 }`) beyond update-specific behavior.
3. Changing pattern syntax (this is expression syntax only).
4. Introducing mutable update semantics; this remains pure reconstruction.

## Proposed Syntax and Semantics

### Syntax (V1)
Allow `..` update source inside record constructor braces:

```bosatsu
Foo { a: newA, ..oldFoo }
Foo { a, ..oldFoo }         # shorthand field
Foo { ..oldFoo }            # parses, but rejected during SourceConverter (Ior.Both)
```

Constraints:
1. At most one `..base` per constructor.
2. `..base` must appear last in the brace list.
3. `..` in expressions must always be followed by a source expression (`..baseExpr`); bare `..` is invalid.

Rejected examples:

```bosatsu
Foo { ..x, a: 1 }     # spread not last
Foo { ..x, ..y }      # multiple spreads
Foo { .. }            # missing source expression
```

### Relation to existing `...` pattern syntax
Bosatsu already uses `...` in pattern matching for partial constructor patterns, for example:

```bosatsu
match v:
  case Foo { a, ... }:
    ...
```

This proposal adds expression syntax with `..`:

```bosatsu
next = Foo { a: 1, ..oldFoo }
```

Disambiguation rules:
1. Pattern context uses `Pattern.matchParser`; there `...` keeps its existing meaning ("ignore remaining fields/args").
2. Expression context uses `Declaration.recordConstructorP`; there we parse `.. <nonbinding-expr>` as update source.
3. Token shape is distinct by design:
   1. `...` (three dots) is pattern-partial syntax.
   2. `..expr` (two dots plus expression) is expression-update syntax.
4. In expression record constructors, a literal `...` is rejected.

### Eligibility Rule (semantic)
`Foo { ..., ..base }` is valid only if constructor `Foo` belongs to a type with exactly one constructor.

Implementation check in `SourceConverter`:
1. Resolve `(package, constructor)` via existing `nameToCons`.
2. Lookup `env.getConstructor(package, constructor)` to get `(definedType, constructorFn)`.
3. Require `definedType.constructors.lengthCompare(1) == 0`.

If not single-constructor, emit a dedicated source-converter error.

### Update Rewrite
Given:

```bosatsu
Foo { <explicit fields>, ..baseExpr }
```

with constructor parameter order `p1, p2, ..., pn`, rewrite to:

```bosatsu
match baseExpr:
  case Foo(<pat1>, <pat2>, ..., <patn>):
    Foo(<arg1>, <arg2>, ..., <argn>)
```

where for each parameter `pi`:
1. If `pi` was explicitly set in the update:
   1. `pati = _`
   2. `argi = <explicit expression for pi>`
2. If `pi` was omitted:
   1. `pati = freshVar_i`
   2. `argi = freshVar_i`

No fallback branch is emitted.

### Required non-trivial update rules
To avoid no-op syntax and fully-explicit rebuilds, require both:
1. At least one field sourced from `baseExpr` (at least one omitted parameter).
2. At least one explicit field override.

If either condition fails, emit a SourceConverter error as `Ior.Both` and keep converting so additional errors can still be accumulated.

Example rejected:

```bosatsu
struct Foo(a: Int)
Foo { a: 3, ..oldFoo }   # no field actually taken from oldFoo
Foo { ..oldFoo }         # no explicit field override
```

### Duplicate explicit field policy
If the same field appears multiple times in update fields:
1. Keep the last written value for lowering (deterministic "last write wins").
2. Emit a SourceConverter error as `Ior.Both`.
3. Continue conversion to accumulate other diagnostics.

Example:

```bosatsu
Foo { a: 1, a: 2, ..x }
```

lowers as if `a: 2` was used, but still reports the duplicate-field error.

### Defaults Interaction
For update syntax with `..base`, omitted fields come from `base`, not constructor defaults.  
Defaults remain relevant only for existing non-update record construction (`Foo { ... }` without `..base`).

## AST and Parser Changes

### `Declaration.scala`
Update `RecordConstructor` shape to carry optional update source, e.g.:

```scala
case class RecordConstructor(
  cons: Constructor,
  args: List[RecordArg],
  updateFrom: Option[NonBinding]
)
```

and update all traversals/utilities:
1. `toDoc` printer (`Foo { fields..., ..expr }` formatting).
2. `freeVars` / `allNames`.
3. `replaceRegions`.
4. `substitute`.

### `RecordArg` parsing
Keep existing field arg kinds:
1. `Simple(field)`
2. `Pair(field, expr)`

and add parsing for optional trailing `..decl` in record-constructor braces.

Parser note:
1. The record-constructor parser should parse an optional trailing spread clause separately from field args (do not treat `..` as a `RecordArg`).
2. Within expression parsing, attempt `...` first as an explicit rejection path (or forbid it directly) so users get a clear error message pointing to pattern-only syntax.

### Pattern conversion safeguard
`Declaration.toPattern` must reject record constructors that contain `updateFrom` (`None` result), since update syntax is expression-only.

## SourceConverter Changes

### `fromDecl` record-constructor branch
Split behavior:

1. `updateFrom = None`: keep current behavior (including default filling).
2. `updateFrom = Some(baseExpr)`: apply update rewrite path.

### Update rewrite algorithm details
1. Convert explicit field args to a mapping exactly as today (`Simple` resolves as variable expression; `Pair` uses explicit expression).
2. Detect duplicate explicit fields:
   1. Add error (`Ior.Both`) when duplicates are present.
   2. Keep the last explicit value per field for the mapping.
3. Validate unexpected fields exactly as today.
4. Resolve constructor metadata with `env.getConstructor`.
5. Enforce single-constructor type.
6. Compute omitted parameters:
   1. `omitted = params.filterNot(p => mapping.contains(p.name))`
7. If `mapping.isEmpty`, emit dedicated error as `Ior.Both` ("update has no explicit field overrides").
8. If `omitted.isEmpty`, emit dedicated error as `Ior.Both` ("update uses no fields from base").
9. Generate fresh bindables for omitted params using existing synthetic-name strategy (`unusedNames`).
10. Build pattern args in declared parameter order:
   1. explicit -> `Pattern.WildCard`
   2. omitted -> `Pattern.Var(fresh)`
11. Build constructor body args in same order:
   1. explicit -> mapped explicit expression
   2. omitted -> `Expr.Local(fresh, tag)`
12. Convert `baseExpr` once via `loop(baseExpr)` and use it as match scrutinee.
13. Emit single-branch `Expr.Match(scrutinee, Branch(PositionalStruct(...), None, rebuiltCtor), tag)`.

This guarantees single evaluation of `baseExpr` and no accidental double evaluation.

## New SourceConverter Errors
Add explicit errors for update syntax failures:

1. `RecordUpdateRequiresSingleConstructor`
2. `RecordUpdateNoFieldsFromBase`
3. `RecordUpdateRequiresExplicitField`
4. `RecordUpdateDuplicateField`

with regions on the record constructor expression.

Message guidance:
1. For multi-constructor types, include constructor/type context and explain update syntax is limited to single-constructor types.
2. For zero-use-of-base, explain all fields were explicitly set and suggest removing `..base` or omitting at least one field.
3. For zero-explicit-fields, explain this is an identity update and suggest using the source expression directly.
4. For duplicates, list duplicated field(s) and explain last explicit value is used for continued checking.

`PackageError.SourceConverterErrorsIn` needs no structural changes; new errors flow through existing rendering.

## Why this design is safe
1. Pure source-level desugaring: no new IR/proto/runtime representation needed.
2. Constructor field order and names come from existing type env metadata (same source of truth as current record constructor conversion).
3. Totality remains valid because generated match is total under enforced single-constructor eligibility and wildcard/var parameter patterns.
4. Name capture is avoided with synthetic fresh variables.

## Implementation Plan
1. Extend `Declaration.RecordConstructor` with optional `updateFrom`.
2. Update `RecordConstructor` parser to accept optional trailing `..expr`.
3. Update declaration printer and traversal helpers (`freeVars`, `allNames`, `replaceRegions`, `substitute`, `toPattern` behavior).
4. Add/adjust parser round-trip tests for update syntax and invalid forms.
5. Add new `SourceConverter.Error` variants for update eligibility/usage failures.
6. Refactor current record-constructor conversion into shared helpers for:
   1. explicit field mapping
   2. unexpected-field checks
   3. constructor metadata lookup.
7. Implement update rewrite path in `SourceConverter.fromDecl`.
8. Keep non-update record constructor path unchanged (including defaults behavior).
9. Add `SourceConverterTest` cases for:
   1. successful update desugaring shape
   2. single-constructor eligibility failure
   3. no-fields-from-base failure
   4. no-explicit-fields failure (`Foo { ..x }`)
   5. duplicate explicit fields produce `Ior.Both` and last-write-wins lowering
   6. shorthand fields with update
   7. unknown/extra fields behavior parity.
10. Add `ErrorMessageTest` cases for new user-facing error messages.
11. Run focused test targets (`ParserTest`, `SourceConverterTest`, `ErrorMessageTest`), then full `core` test suite.

## Test Matrix
1. Parser accept:
   1. `Foo { a: 1, ..x }`
   2. `Foo { a, ..x }`
   3. `Foo { ..x }`
2. Parser reject:
   1. `Foo { ..x, a: 1 }`
   2. `Foo { ..x, ..y }`
3. Conversion success:
   1. Struct with omitted fields copied from base.
   2. Generic single-constructor type update.
4. Conversion failure:
   1. Multi-constructor enum update attempt.
   2. All-fields-explicit plus `..base`.
   3. No-explicit-fields (`Foo { ..x }`) with partial conversion (`Ior.Both`).
   4. Duplicate fields with partial conversion (`Ior.Both`).
5. Regression:
   1. Existing `Foo { ... }` constructor defaults behavior unchanged.
   2. Pattern parsing/`toPattern` behavior unchanged except update-form exclusion.

## Resolution Notes
1. Reject `Foo { ..oldFoo }` semantically via SourceConverter error (`Ior.Both`) while continuing conversion.
2. Treat duplicate explicit fields as SourceConverter errors (`Ior.Both`) with deterministic last-write-wins lowering.
