---
issue: 2278
priority: 3
touch_paths:
  - docs/design/2278-remove-and-repurpose-literal-dict-syntax.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ListLang.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/main/scala/dev/bosatsu/TotalityCheck.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/DeclarationTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/ParserHintsTest.scala
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

Remove dict literals and dict comprehensions in one compiler change, and repurpose non-empty bare braces for constructor inference when the provided fields uniquely identify a constructor defined in the current source file.

## Context
Bosatsu currently uses `Declaration.DictDecl` plus `ListLang.KVPair` to parse `{...}` and `{... for ...}`. `SourceConverter` then lowers that syntax to `empty_Dict(string_Order)`, repeated `add_key`, or a `foldl_List` build. Literal dicts are therefore surface sugar over the existing `Dict` library type rather than a distinct runtime feature.

Bosatsu already has a stronger nominal record story than its dict-literal story. `Foo { field: value }` is parsed as `Declaration.RecordConstructor`, resolved in `SourceConverter` using `TypeEnv`, and validated against `rankn.ConstructorParam` metadata. That existing pipeline is the cheapest place to repurpose `{}`.

Anonymous records are much larger. `rankn.Type` and `TypeRef` currently model nominal type constructors, type application, and quantifiers. They do not have a structural record type, row variable, or field-projection concept. Adding anonymous records would therefore spill into parsing, type checking, interface serialization, pretty-printing, and likely pattern semantics.

There is also a compatibility concern. The repo already has old dict code that uses bindable keys such as `{k: v}`, not only string literals. That means the change should be treated as a direct source-language break, not as a parser-only cleanup. Given the small user base and the issue direction, that is acceptable, but the new inference rule needs to be simple enough that the resulting errors are easy to understand.

## Goals
1. Remove literal dict and dict-comprehension surface syntax from Bosatsu source.
2. Keep `Dict` as a supported library type with no runtime or `ValueToJson` behavior change.
3. Reuse bare braces for a feature that aligns with Bosatsu's nominal typing model.
4. Preserve Bosatsu's strong local-context property: readers should not need imported constructor metadata to understand why a bare record compiles.
5. Keep the new brace-inference rule simple and produce clear errors for ambiguous or impossible brace forms.
6. Minimize changes to the type system, code generation, and serialization layers.

## Non-goals
1. Removing the `Dict` type or its existing APIs such as `empty_Dict`, `add_key`, `get_key`, and `items`.
2. Adding anonymous or structural records in this issue.
3. Adding type-directed constructor inference.
4. Adding bare-brace pattern syntax.
5. Supporting bare-brace record updates such as `{ field: value, ..base }`.

## Current Architecture
1. `core/src/main/scala/dev/bosatsu/Declaration.scala` owns both `DictDecl` and `RecordConstructor`, along with the parser, printer, substitution, free-variable, and region-rewrite logic that would be affected by any `{}` repurpose.
2. `core/src/main/scala/dev/bosatsu/ListLang.scala` contains a generic list sublanguage plus a dict-specific `KVPair` path that exists only to support `DictDecl`.
3. `core/src/main/scala/dev/bosatsu/SourceConverter.scala` already knows how to validate named constructor fields, fill defaults, and desugar record updates. It is the right layer for constructor-name inference because it already has access to the merged local-plus-imported `TypeEnv`.
4. The Paradox docs and parser-hint surface still assume `{...}` can mean dict syntax, so the change is not only a parser edit.

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
2. Some old dict code with bindable keys can look superficially similar, so the uniqueness rule has to stay easy to explain.
3. It is intentionally less expressive than anonymous records.

This is the recommended direction, but the scope of inference matters.

### Scope of inference: local-only vs imported constructors
There are two plausible ways to scope constructor inference once Bosatsu accepts bare braces:

1. Consider every visible constructor, including imported ones.
2. Consider only constructors defined in the current source file.

Allowing imported constructors is more uniform, but it weakens one of Bosatsu's readability properties. Today, a reader can generally tell where a constructor came from because it is either named explicitly in the file or imported explicitly. If `{ name: "x", size: 1 }` can silently target an imported constructor, then the reason that expression compiles depends on non-local constructor metadata that is no longer obvious from reading the file.

Restricting inference to current-file constructors preserves that local-context property:
1. A reader can find the candidate constructors in the same file.
2. Imported record construction remains explicit, which matches the current design bias toward source readability.
3. The asymmetry is manageable because `Foo { ... }` already remains available everywhere as the universal escape hatch.

The cost is that inference becomes intentionally non-uniform: the same field set may infer locally but require an explicit constructor once the type is imported from another file. That is acceptable here because preserving local readability is more important than maximizing inference reach.

Recommendation: inference should consider only constructors defined in the current source file.

## Recommendation
Repurpose bare braces directly for inferred record construction in the same compiler change that removes dict literals and dict comprehensions, but limit inference to constructors defined in the current source file.

The key rule should be: bare braces are allowed when the provided field names identify exactly one constructor from the current source file. Once that constructor is chosen, Bosatsu should reuse the existing record-constructor path, including defaults for omitted fields and ordinary missing-field errors when required fields are absent.

This keeps the feature aligned with the nominal type system, avoids a large anonymous-record detour, preserves Bosatsu's local-context property, and gives users one clear fallback: if the compiler cannot infer the constructor, write it explicitly.

## Proposed Design
### 1. Remove dict surface syntax but keep `Dict`
Delete `Declaration.DictDecl` and the `ListLang.KVPair`-based source syntax path. `Dict` remains a normal library type in `predef.bosatsu`; runtime values, evaluation, and JSON conversion continue to work exactly as they do today.

`Parser.dictLikeParser` in `core/src/main/scala/dev/bosatsu/Parser.scala` should remain. It is a general utility used outside the Bosatsu expression grammar and does not need to disappear just because source dict literals do.

### 2. Keep removed-dict diagnostics simple
The implementation does not need a dedicated migration helper. Existing `Dict` APIs such as `empty_Dict`, `add_key`, `items`, and ordinary folds remain sufficient.

The parser and/or `ParserHints` should still detect the most obvious removed dict shapes and report a direct error instead of a generic parse failure.

At minimum, the diagnostics should distinguish:
1. `{}`: old empty-dict literal removed.
2. `{ expr: expr, ... }`: old dict literal removed.
3. `{ expr: expr for pat in src if cond }`: old dict comprehension removed.

Those messages can point users to the existing `Dict` APIs without introducing a new compatibility layer.

### 3. Introduce a new inferred-record AST node
Add a new expression-only node in `Declaration.scala`, for example `InferredRecordConstructor(args)`.

This node should reuse the existing `RecordArg` grammar:
1. `field`
2. `field: expr`

Deliberate exclusions in the first version:
1. No `..base` without an explicit constructor.
2. No empty `{}` form.
3. No pattern form; `Foo { ... }` remains the only record-pattern spelling.

### 4. Resolve inferred records in `SourceConverter` by unique compatible local constructor
Inference should happen in `SourceConverter`, where constructor metadata already exists.

Resolution algorithm:
1. Build an index over constructors defined in the current source file and their declared fields.
2. Keep candidates that define all of the provided field names.
3. If there is exactly one local candidate, synthesize the equivalent explicit `RecordConstructor` and reuse the current lowering path.
4. After the constructor is chosen, let the existing record-constructor validation decide whether omitted fields are filled from defaults or reported as missing.
5. If there are zero local candidates, emit a new source-converter error that no current-file constructor defines all of the provided fields. Imported constructors are intentionally not considered.
6. If there are multiple local candidates, emit a new ambiguity error and require the user to write the constructor name explicitly.

Why this rule:
1. It keeps inference name-based, with no type-directed behavior.
2. It allows defaults to keep working naturally once the constructor is known.
3. It keeps the feature easy to explain: bare braces mean “the unique current-file constructor compatible with these fields”.
4. It preserves the package-local readability rule for imported types.

Consequences of this rule:
1. Partial arguments remain valid when the inferred constructor can fill omitted fields from defaults.
2. `TupleN` can participate naturally via `item1`, `item2`, and so on, because those fields already exist in constructor metadata.
3. If a unique constructor is found but required fields are still missing, Bosatsu should report the same missing-field error it already reports for explicit record construction.
4. Imported constructors always require explicit spelling, even if their fields would be unambiguous.

### 5. Keep explicit constructor syntax as the escape hatch
Explicit constructor spelling remains the full-featured form:

```bosatsu
File { name: "readme.txt", size: 4096 }
File { size: 4096, ..base }
Rec {}
```

Bare-brace inference is intentionally narrower:

```bosatsu
{ name: "readme.txt", size: 4096 }   # only if exactly one current-file constructor accepts fields {name, size}
```

Rejected in the first version:

```bosatsu
{}                        # still invalid
{ size: 4096, ..base }    # write File { size: 4096, ..base }
{ a: 1 }                  # error if zero or multiple constructors are compatible with field a
```

## Implementation Plan
1. Remove `DictDecl` from `core/src/main/scala/dev/bosatsu/Declaration.scala` and delete the dict-specific pieces of `core/src/main/scala/dev/bosatsu/ListLang.scala`.
2. Remove dict-literal lowering from `core/src/main/scala/dev/bosatsu/SourceConverter.scala` and the corresponding parsed-declaration cases in `core/src/main/scala/dev/bosatsu/TotalityCheck.scala`.
3. Add targeted removed-dict diagnostics in `core/src/main/scala/dev/bosatsu/Declaration.scala` and/or `core/src/main/scala/dev/bosatsu/ParserHints.scala`.
4. Add `InferredRecordConstructor` to `Declaration.scala` and parse non-empty bare-brace record fields.
5. Add unique-compatible-constructor resolution plus new ambiguity/no-match errors in `SourceConverter.scala`, reusing the existing explicit-constructor validation for defaults and missing fields.
6. Ensure imported constructors are excluded from bare-brace inference and require explicit spelling.
7. Update syntax highlighting, docs, and tests once bare-brace inference is live.

## Testing Strategy
1. Parser tests in `core/src/test/scala/dev/bosatsu/ParserTest.scala` should cover:
   - old dict literal forms failing with targeted messages,
   - old dict comprehensions failing with targeted messages,
   - successful parsing of inferred records,
   - rejection of `{}` and bare-brace `..base`.
2. Source-converter tests in `core/src/test/scala/dev/bosatsu/SourceConverterTest.scala` should cover:
   - a unique compatible local-constructor match lowering to the same core expression as the explicit constructor,
   - no-match errors,
   - ambiguity errors,
   - omitted fields being filled from defaults after inference,
   - omitted required fields still producing the normal missing-field error after inference,
   - imported constructors remaining ineligible for bare-brace inference,
   - explicit update syntax remaining unchanged.
3. Error-message tests in `core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala` and `core/src/test/scala/dev/bosatsu/ParserHintsTest.scala` should verify the migration guidance and ambiguity wording.
4. Evaluation tests in `core/src/test/scala/dev/bosatsu/EvaluationTest.scala` should cover inferred record construction in successful and failing cases, plus any migrated internal examples that previously used dict literals.

## Acceptance Criteria
1. Bosatsu source no longer accepts literal dict or dict-comprehension syntax as a way to construct `Dict`.
2. `Dict` remains available through its existing library APIs.
3. Non-empty bare-brace record construction is accepted only when the provided fields identify exactly one constructor defined in the current source file.
4. Once the constructor is inferred, Bosatsu reuses the existing explicit-record behavior for defaults and missing fields.
5. Bare-brace inference is expression-only and does not support `..base`.
6. Imported constructors still require explicit `Foo { ... }` spelling.
7. Ambiguous or unmatched field sets produce dedicated compiler errors that instruct users to write the constructor name explicitly.
8. The design does not require a new type form, new runtime representation, or interface/protobuf changes.

## Risks And Mitigations
1. Risk: old `{k: v}` dict code is reparsed as inferred-record syntax and fails in confusing ways.
Mitigation: treat the change as a direct source break, keep removed-dict diagnostics clear for obvious old forms, and make ambiguity/no-match errors point users toward explicit constructors.

2. Risk: constructor inference becomes hard to explain if defaults or types participate.
Mitigation: use only the provided field names to choose the constructor, then reuse the existing explicit-constructor validation logic after that choice.

3. Risk: multiple current-file constructors are compatible with the same provided fields.
Mitigation: treat that as a hard ambiguity and require explicit spelling rather than adding tie-breaking rules.

4. Risk: local-only inference feels asymmetric because imported constructors do not participate.
Mitigation: keep the rule explicit in docs and diagnostics, and rely on ordinary `Foo { ... }` syntax as the universal form.

5. Risk: users ask for bare-brace patterns immediately after expression inference lands.
Mitigation: keep patterns explicit in this issue and treat typed pattern inference as a separate follow-up.

## Rollout Notes
1. Parser, lowering, docs, and tests should all move together in one landing so the language does not temporarily describe two different meanings for bare braces.
2. Release notes should explicitly call out that `Dict` still exists; only the literal surface is gone.
3. Existing `Dict` users migrate with the APIs already in `predef.bosatsu`.
4. Imported record construction should remain explicit so that source files retain Bosatsu's local-context readability property.
5. Anonymous records should remain a separate future design topic if Bosatsu later wants structural data rather than nominal constructor inference.
