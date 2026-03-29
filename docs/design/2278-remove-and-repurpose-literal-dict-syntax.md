---
issue: 2278
priority: 3
touch_paths:
  - docs/design/2278-remove-and-repurpose-literal-dict-syntax.md
  - core/src/main/resources/bosatsu/predef.bosatsu
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ListLang.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/main/scala/dev/bosatsu/ValueToDoc.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/DeclarationTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ParserHintsTest.scala
  - core/src/test/scala/dev/bosatsu/ValueToDocTest.scala
  - docs/src/main/paradox/language_guide.md
  - docs/src/main/paradox/index.md
  - syntax/vscode/bosatsu/syntaxes/bosatsu.tmLanguage.json
depends_on: []
estimated_size: M
generated_at: 2026-03-29T04:47:42Z
---

# Design: Remove Literal Dict Syntax And Reuse Bare Braces For Inferred Records

_Issue: #2278 (https://github.com/johnynek/bosatsu/issues/2278)_

## Summary

Recommend a staged plan: remove dict literals and dict comprehensions, add `from_List_Dict` as the migration surface, and then repurpose non-empty bare braces for exact field-set constructor inference instead of introducing anonymous records.

## Context
Bosatsu currently uses `Declaration.DictDecl` plus `ListLang.KVPair` to parse `{...}` and `{... for ...}`. `SourceConverter` then lowers that syntax to `empty_Dict(string_Order)`, repeated `add_key`, or a `foldl_List` build. Literal dicts are therefore surface sugar over the existing `Dict` library type rather than a distinct runtime feature.

Bosatsu already has a stronger nominal record story than its dict-literal story. `Foo { field: value }` is parsed as `Declaration.RecordConstructor`, resolved in `SourceConverter` using `TypeEnv`, and validated against `rankn.ConstructorParam` metadata. That existing pipeline is the cheapest place to repurpose `{}`.

Anonymous records are much larger. `rankn.Type` and `TypeRef` currently model nominal type constructors, type application, and quantifiers. They do not have a structural record type, row variable, or field-projection concept. Adding anonymous records would therefore spill into parsing, type checking, interface serialization, pretty-printing, and likely pattern semantics.

There is also a migration concern. The repo already has old dict code that uses bindable keys such as `{k: v}`, not only string literals. A one-step change from “dict literal” to “inferred record” would reparse some existing programs into a different syntax class and produce confusing failures. The safest plan is staged: remove and migrate first, repurpose second.

## Goals
1. Remove literal dict and dict-comprehension surface syntax from Bosatsu source.
2. Keep `Dict` as a supported library type with no runtime or `ValueToJson` behavior change.
3. Reuse bare braces for a feature that aligns with Bosatsu's nominal typing model.
4. Provide an explicit migration surface and targeted diagnostics for old dict syntax.
5. Minimize changes to the type system, code generation, and serialization layers.

## Non-goals
1. Removing the `Dict` type or its existing APIs such as `empty_Dict`, `add_key`, `get_key`, and `items`.
2. Adding anonymous or structural records in this issue.
3. Adding type-directed constructor inference.
4. Adding bare-brace pattern syntax.
5. Supporting bare-brace record updates or default-driven omission in the first inferred-record version.

## Current Architecture
1. `core/src/main/scala/dev/bosatsu/Declaration.scala` owns both `DictDecl` and `RecordConstructor`, along with the parser, printer, substitution, free-variable, and region-rewrite logic that would be affected by any `{}` repurpose.
2. `core/src/main/scala/dev/bosatsu/ListLang.scala` contains a generic list sublanguage plus a dict-specific `KVPair` path that exists only to support `DictDecl`.
3. `core/src/main/scala/dev/bosatsu/SourceConverter.scala` already knows how to validate named constructor fields, fill defaults, and desugar record updates. It is the right layer for constructor-name inference because it already has access to the merged local-plus-imported `TypeEnv`.
4. `core/src/main/scala/dev/bosatsu/ValueToDoc.scala` and the Paradox docs still render or describe `Dict[String, a]` values with `{...}` syntax, so syntax removal is not only a parser change.

## Options Considered
### Anonymous records
Anonymous records would make `{ field: value }` feel like a direct replacement for dict literals, but they do not fit the current compiler boundary.

To make anonymous records useful, Bosatsu would need at least:
1. A new type representation for record shapes.
2. Equality and inference rules for those record types.
3. A story for projection, matching, printing, and interface serialization.
4. A decision on whether records are closed, open, or row-polymorphic.

That is substantially larger than this issue. It would also create a second record model beside today's nominal constructor records. This option should be treated as a future type-system design, not as the replacement for dict literal syntax.

### Inferred constructor name from field set
This option reuses what Bosatsu already has: named constructor fields and nominal data types.

Advantages:
1. It stays inside the parser plus `SourceConverter`; no new runtime representation is required.
2. It leverages the existing constructor metadata in `TypeEnv`.
3. It keeps explicit `Foo { ... }` as the fallback when inference is ambiguous.

Costs:
1. The compiler must define a stable, easy-to-explain ambiguity rule.
2. Some old dict code with bindable keys can look superficially similar, so migration must be staged.
3. It is intentionally less expressive than anonymous records.

This is the recommended direction.

## Recommendation
Repurpose bare braces for inferred record construction, but do it in two stages.

Stage 1 should remove dict literals and dict comprehensions, add a migration helper, and reserve bare-brace forms with targeted errors. Stage 2 should introduce inferred record construction only after the tree, docs, and pretty-printers no longer depend on the old dict meaning.

This staged plan is important because a one-step flip would turn some old `{k: v}` dict programs into a different parse class. The issue body is correct that dict literals are rare, but the repo is not at zero usage. Removing first and repurposing second keeps the language change legible.

## Proposed Design
### 1. Remove dict surface syntax but keep `Dict`
Delete `Declaration.DictDecl` and the `ListLang.KVPair`-based source syntax path. `Dict` remains a normal library type in `predef.bosatsu`; runtime values, evaluation, and JSON conversion continue to work exactly as they do today.

`Parser.dictLikeParser` in `core/src/main/scala/dev/bosatsu/Parser.scala` should remain. It is a general utility used outside the Bosatsu expression grammar and does not need to disappear just because source dict literals do.

### 2. Add a canonical migration helper
Add a small helper to `core/src/main/resources/bosatsu/predef.bosatsu`:

```bosatsu
def from_List_Dict(comp: Order[k], items: List[(k, v)]) -> Dict[k, v]
```

It should fold `items` over `empty_Dict(comp)` using `add_key`. This gives the compiler, docs, and users one canonical replacement surface:

```bosatsu
from_List_Dict(string_Order, [("a", 1), ("b", 2)])
from_List_Dict(string_Order, [(k, v) for (k, v) in pairs if keep(k)])
```

This helper is not a runtime change. It is a migration and pretty-printing aid.

### 3. Stop pretty-printers and docs from emitting removed syntax
Once `{...}` no longer means dict literal syntax, Bosatsu should not keep rendering dict values that way.

`ValueToDoc` should switch `Dict[String, a]` rendering from brace-literal form to the helper form, for example:

```bosatsu
from_List_Dict(string_Order, [("a", 1), ("b", 2)])
```

Paradox docs and parser hints should use the same replacement so users see one consistent migration target.

### 4. Reserve old dict syntax with targeted diagnostics
During stage 1, bare brace literals should remain invalid. The parser and/or `ParserHints` should detect the old dict shapes and report a direct migration error instead of a generic parse failure.

At minimum, the diagnostics should distinguish:
1. `{}`: old empty-dict literal removed.
2. `{ expr: expr, ... }`: old dict literal removed.
3. `{ expr: expr for pat in src if cond }`: old dict comprehension removed.

Those messages should point users to `from_List_Dict`, or to `empty_Dict` plus `add_key` for lower-level control.

### 5. Introduce a new inferred-record AST node
After the syntax has been removed and internal callers migrated, add a new expression-only node in `Declaration.scala`, for example `InferredRecordConstructor(args)`.

This node should reuse the existing `RecordArg` grammar:
1. `field`
2. `field: expr`

Deliberate exclusions in the first version:
1. No `..base` without an explicit constructor.
2. No empty `{}` form.
3. No pattern form; `Foo { ... }` remains the only record-pattern spelling.
4. No default-based omission during inference.

### 6. Resolve inferred records in `SourceConverter` by exact field-set match
Inference should happen in `SourceConverter`, where constructor metadata already exists.

Resolution algorithm:
1. Build a field-set index over visible constructors in `localTypeEnv`.
2. Use only constructors whose full declared field-name set exactly matches the explicit field-name set written in the literal.
3. If there is exactly one candidate, synthesize the equivalent explicit `RecordConstructor` and reuse the current lowering path.
4. If there are zero candidates, emit a new source-converter error that no visible constructor matches the field set.
5. If there are multiple candidates, emit a new ambiguity error and require the user to write the constructor name explicitly.

Why exact field-set match:
1. It keeps inference purely name-based, with no type-directed behavior.
2. It avoids hidden interaction with constructor defaults.
3. It keeps the feature easy to explain: bare braces mean “the unique visible constructor with exactly these fields”.

Consequences of this rule:
1. If a constructor relies on omitted defaults, users must still write `Foo { ... }`.
2. `TupleN` can participate naturally via `item1`, `item2`, and so on, because those fields already exist in constructor metadata.
3. Bare-brace inference is stable regardless of the types of the provided expressions.

### 7. Keep explicit constructor syntax as the escape hatch
Explicit constructor spelling remains the full-featured form:

```bosatsu
File { name: "readme.txt", size: 4096 }
File { size: 4096, ..base }
Rec {}
```

Bare-brace inference is intentionally narrower:

```bosatsu
{ name: "readme.txt", size: 4096 }   # only if exactly one visible constructor has fields {name, size}
```

Rejected in the first version:

```bosatsu
{}                        # still invalid
{ size: 4096, ..base }    # write File { size: 4096, ..base }
{ a: 1 }                  # error if zero or multiple constructors have exactly {a}
```

## Implementation Plan
1. Add `from_List_Dict` to `core/src/main/resources/bosatsu/predef.bosatsu`.
2. Update `core/src/main/scala/dev/bosatsu/ValueToDoc.scala` to stop rendering removed dict syntax.
3. Remove `DictDecl` from `core/src/main/scala/dev/bosatsu/Declaration.scala` and delete the dict-specific pieces of `core/src/main/scala/dev/bosatsu/ListLang.scala`.
4. Remove dict-literal lowering from `core/src/main/scala/dev/bosatsu/SourceConverter.scala` and the corresponding parsed-declaration cases in `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`.
5. Add targeted removed-dict diagnostics in `core/src/main/scala/dev/bosatsu/Declaration.scala` and/or `core/src/main/scala/dev/bosatsu/ParserHints.scala`.
6. Migrate internal tests and examples off dict literal syntax using `from_List_Dict`.
7. Add `InferredRecordConstructor` to `Declaration.scala` and parse non-empty bare-brace record fields.
8. Add exact-field-set constructor resolution plus new ambiguity/no-match errors in `SourceConverter.scala`.
9. Update syntax highlighting and user docs once bare-brace inference is live.

## Testing Strategy
1. Parser tests in `core/src/test/scala/dev/bosatsu/ParserTest.scala` should cover:
   - old dict literal forms failing with targeted messages,
   - old dict comprehensions failing with targeted messages,
   - successful parsing of inferred records,
   - rejection of `{}` and bare-brace `..base`.
2. Source-converter tests in `core/src/test/scala/dev/bosatsu/SourceConverterTest.scala` should cover:
   - a unique exact-field-set match lowering to the same core expression as the explicit constructor,
   - no-match errors,
   - ambiguity errors,
   - defaults still requiring explicit constructor names,
   - explicit update syntax remaining unchanged.
3. Error-message tests in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` and `core/src/test/scala/dev/bosatsu/ParserHintsTest.scala` should verify the migration guidance and ambiguity wording.
4. Evaluation tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` should cover `from_List_Dict` and any migrated internal examples that previously used dict literals.
5. `core/src/test/scala/dev/bosatsu/ValueToDocTest.scala` should verify that string-key dict rendering no longer emits removed brace-literal syntax.

## Acceptance Criteria
1. Bosatsu source no longer accepts literal dict or dict-comprehension syntax as a way to construct `Dict`.
2. `Dict` remains available through library APIs, and `from_List_Dict` exists as the documented migration surface.
3. The parser, docs, and `ValueToDoc` no longer advertise removed dict brace syntax.
4. Non-empty bare-brace record construction is accepted only when exactly one visible constructor has exactly that field set.
5. Bare-brace inference is expression-only, does not use defaults, and does not support `..base`.
6. Explicit `Foo { ... }` construction, defaults, and record update syntax continue to work unchanged.
7. Ambiguous or unmatched field sets produce dedicated compiler errors that instruct users to write the constructor name explicitly.
8. The design does not require a new type form, new runtime representation, or interface/protobuf changes.

## Risks And Mitigations
1. Risk: old `{k: v}` dict code is reparsed as inferred-record syntax and fails in confusing ways.
Mitigation: land the change in two stages and reserve removed dict syntax with targeted diagnostics before enabling inference.

2. Risk: constructor inference becomes hard to explain if defaults or types participate.
Mitigation: use exact field-set matching only, with explicit constructors as the escape hatch.

3. Risk: multiple visible constructors share the same field set.
Mitigation: treat that as a hard ambiguity and require explicit spelling rather than adding tie-breaking rules.

4. Risk: keeping `ValueToDoc` or docs on `{...}` after syntax removal makes the language appear internally inconsistent.
Mitigation: switch rendered dict examples to `from_List_Dict` in the same rollout as parser removal.

5. Risk: users ask for bare-brace patterns immediately after expression inference lands.
Mitigation: keep patterns explicit in this issue and treat typed pattern inference as a separate follow-up.

## Rollout Notes
1. Preferred landing order:
   - add `from_List_Dict`, update renderers/docs, and remove dict syntax;
   - migrate internal callers and tests;
   - then enable inferred bare-brace records.
2. If stage 2 slips, stage 1 is still a coherent improvement: Bosatsu loses low-value syntax while keeping `Dict` fully supported.
3. Release notes should explicitly call out that `Dict` still exists; only the literal surface is gone.
4. Anonymous records should remain a separate future design topic if Bosatsu later wants structural data rather than nominal constructor inference.
