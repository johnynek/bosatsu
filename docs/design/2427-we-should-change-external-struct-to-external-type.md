---
issue: 2427
priority: 3
touch_paths:
  - docs/design/2427-we-should-change-external-struct-to-external-type.md
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/resources/bosatsu/predef.bosatsu
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/EvaluationTest.scala
  - core/src/test/scala/dev/bosatsu/KindFormulaTest.scala
  - core/src/test/scala/dev/bosatsu/PackageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/clang/ClangGenLibraryDepsTest.scala
  - core/src/test/scala/dev/bosatsu/codegen/python/PythonGenTest.scala
  - core/src/test/scala/dev/bosatsu/protobuf/ProtoToBosatsuTest.scala
  - core/src/test/scala/dev/bosatsu/rankn/RankNInferTest.scala
  - test_workspace/Prog.bosatsu
  - test_workspace/Int64.bosatsu
  - test_workspace/Bosatsu/Collection/Array.bosatsu
  - test_workspace/Bosatsu/IO/Bytes.bosatsu
  - test_workspace/Bosatsu/IO/Core.bosatsu
  - test_workspace/Bosatsu/Lazy.bosatsu
  - docs/src/main/paradox/language_guide.md
  - docs/src/main/paradox/design-docs/minimal_prog_io_tools_design.md
  - docs/src/main/paradox/design-docs/prog_concurrency_design.md
  - docs/design/1796-design-a-bytes-type.md
  - docs/design/1925-lazy-type.md
  - docs/design/2182-add-prog-var.md
  - docs/design/2239-type-aliases.md
  - docs/design/2254-design-required-exposes-declarations-for-exported-dependency-visibility.md
  - docs/design/2304-add-int64-external-struct.md
  - docs/code-plans/2358-implement-libuv-backed-process-spawn-and-wait-for-c-io-core.md
  - docs/code-plans/2358-implement-libuv-backed-process-spawn-and-wait-for-c-io-core.json
depends_on: []
estimated_size: L
generated_at: 2026-09-11T18:11:51Z
---

# Canonicalize opaque external declarations as `external type`

_Issue: #2427 (https://github.com/johnynek/bosatsu/issues/2427)_

## Summary

Accept `external type` as the canonical spelling for opaque externally owned types, retain `external struct` as a source-compatible parser alias, and migrate repository-owned source, fixtures, and documentation without changing type-system or runtime semantics.

## Context

Bosatsu currently parses an opaque externally represented type through the `external` branch in `Statement.parser1` using the spelling `external struct`. It becomes a constructorless `rankn.DefinedType`, so neither its parser representation nor its downstream semantics establish that the external value is a product type. The spelling is a historical artifact from when the language exposed only `struct` and `enum` declarations.

Bosatsu now also has transparent aliases written as `type Foo = Bar`. The representation-neutral spelling for an opaque type owned by a runtime is therefore `external type Foo`. Despite the shared word `type`, an external type remains opaque and is not a transparent `Statement.TypeAlias`.

The issue-tree manifest contains only issue #2427 and no ancestor or dependency nodes, so there is no roadmap or dependency conflict. The existing external declaration parser and statement document renderer are the mechanisms to extend. The existing `Statement.ExternalStruct` node already carries the required opaque, constructorless semantics, so its downstream lowering does not need to change.

## Goals

1. Accept `external type Name` with the same type-parameter and kind syntax currently accepted by `external struct`.
2. Make `external type` the canonical spelling emitted by Bosatsu's statement document renderer and used in repository-owned Bosatsu sources and documentation.
3. Continue accepting `external struct` without warnings or semantic differences, preserving existing Bosatsu source compatibility.
4. Retain the existing constructorless `DefinedType` lowering and all current kind, variance, export, evaluation, and code-generation behavior.
5. Keep ordinary `struct`, transparent `type` aliases, external values, and external functions unchanged.

## Non-goals

1. No new transparent or opaque alias semantics.
2. No constructors, fields, destructuring, or runtime representation metadata for external types.
3. No deprecation warning or removal schedule for `external struct`.
4. No protobuf, compiled-library, runtime, evaluator, or backend representation change.
5. No broad rewrite of unrelated prose. Existing documents are changed only where they state or demonstrate the old external-type syntax; stable issue-derived filenames remain unchanged to avoid breaking links.
6. No editor-grammar redesign. Tree-sitter's current line-oriented grammar already accepts the new token sequence; improving its treatment of the existing soft `type` keyword is separate from compiler correctness.
7. No rename of `Statement.ExternalStruct` or its downstream compiler matches and generators.

This is a parser canonicalization change, not a workflow state machine, so a formal model such as TLA+ would add no useful coverage.

## Design

### Extend the existing external declaration parser

In `Statement.parser1`, extend the existing external-struct declaration parser so its introducer accepts either `type` or the legacy `struct`. Both spellings immediately construct the existing `Statement.ExternalStruct` node; the parser does not retain which spelling appeared in the input. Adding a second node would create a semantic distinction that the language does not have, while renaming the existing node would force behavior-neutral changes in every downstream consumer without changing user-visible syntax or semantics.

The `type` token remains soft in the relevant parser position. In particular, the existing external-value declaration `external type: SomeType` must continue to parse as a value named `type`: the new declaration branch requires whitespace followed by a constructor name. Top-level `type Foo = Bar` continues to parse through `Statement.TypeAlias` and is not routed through the external-type path.

### Canonical rendering

The `Document[Statement]` case for `ExternalStruct` always emits:

```bosatsu
external type Name[parameters]
```

It never emits the legacy spelling, even when the parsed input used it. Since synthetic predef function types and generated parser-law statements also use this AST node, the renderer is the unavoidable canonicalization boundary for both parsed and programmatically constructed statements.

Source regions remain source-specific and may differ between the two spellings. Semantic-equivalence tests should normalize regions before comparing statements.

### Preserve lowering and type behavior

`SourceConverter` continues lowering the statement to a `rankn.DefinedType` with no constructors. The special rule that unannotated parameters of external types default to invariant remains unchanged; an external representation may use a parameter even though Bosatsu cannot inspect any fields.

`Package` continues constructing synthetic `FnN` types through the same constructorless external-type node. Export behavior remains unchanged: exporting the type name exposes an opaque type and never exposes constructors. Since the two spellings converge before source conversion, type inference, serialization, evaluators, and code generators cannot distinguish them.

### Migrate the bounded repository-owned occurrence set

Change checked-in Bosatsu declarations in the predef and `test_workspace` to `external type`. Change embedded Bosatsu programs in compiler tests to the preferred spelling, except for focused parser compatibility coverage that deliberately uses `external struct`.

Update documentation examples and present-tense descriptions that currently prescribe `external struct`. Issue-numbered design files and the Markdown/JSON forms of the completed process code plan should receive only the corresponding terminology or code-sample substitutions; their unrelated decisions, formatting, and stable paths must not change. The new #2427 design necessarily discusses the legacy spelling while explaining compatibility.

A final repository search should audit the finite occurrence inventory. Remaining uses of the old phrase should be limited to the legacy parser token construction, the focused compatibility test, this design's compatibility discussion, and immutable issue-derived path or metadata text where changing it would falsify history or break links. This is an implementation audit, not a new permanent lint subsystem.

## Behavioral properties and invariants

1. For every valid external-type name, type-parameter list, and kind annotation, the new and legacy spellings lower to identical semantic type definitions after source regions are ignored.
2. Every parsed or programmatically constructed `ExternalStruct` renders with `external type`, and reparsing that rendering returns the same statement.
3. External types remain opaque and constructorless regardless of source spelling.
4. Unannotated external-type parameters remain invariant; explicit variance and kind annotations retain their current meaning.
5. Existing `external struct` source continues to compile without a warning or migration flag.
6. `external type Foo` and `type Foo = Bar` remain distinct: the former declares an opaque external type and the latter a transparent alias.
7. External values and functions, including a value whose bindable name is `type`, retain their current grammar and behavior.
8. No backend, serialized interface, or runtime representation can observe which accepted spelling was used.

These invariants are structurally enforced by converging both spellings into the existing sealed statement node at the parser boundary and by using its existing lowering case and one canonical rendering case. There is no in-scope alternate parser or lowering path that requires duplicate enforcement.

## Implementation plan

1. In `Statement.scala`, accept `type` and legacy `struct` in the existing external declaration parser, construct `ExternalStruct` for both, and render that node only as `external type`.
2. Add focused compatibility and ambiguity coverage in `ParserTest.scala`, reusing the existing external-statement generator for property coverage.
3. Replace declarations in the predef, workspace libraries, and embedded test programs with the canonical spelling. Existing semantic tests then exercise the new spelling throughout parsing, typechecking, evaluation, library tooling, protobuf conversion, and code-generation preparation.
4. Update only affected syntax examples and terminology in the enumerated documentation and synchronized code-plan files.
5. Audit remaining legacy occurrences against the narrow allowlist and run the repository gates below.

## Testing strategy

Property-check coverage belongs in `ParserTest.scala` alongside the existing ScalaCheck statement round-trip law:

1. Use the existing `ExternalStruct` generator to produce statements with arbitrary valid names, parameter lists, and kind annotations.
2. Render and parse the preferred spelling and assert statement identity after region normalization.
3. Derive the legacy spelling for the same generated declaration, parse it, and assert that it produces the same normalized statement as the preferred spelling.
4. Assert that rendering either parsed result always yields the preferred spelling.

Narrow case-based tests remain appropriate for grammar boundaries:

1. parameterless and parameterized `external type` declarations;
2. explicit covariance, contravariance, phantom variance, and higher-kinded parameters;
3. one direct `external struct` compatibility example;
4. `external type: Foo` remaining an external value named `type`;
5. `type Foo = Bar` remaining a transparent alias;
6. malformed external-type declarations failing at the declaration site.

The existing `KindFormulaTest` case should use the preferred spelling and continue asserting invariant defaults. Existing package, inference, evaluator, protobuf, CLI/library, Python-generation, and C-generation tests should mainly receive fixture spelling changes; duplicating semantic assertions in each suite would add no coverage because both spellings converge in the parser.

Implementation verification follows `coding_style.md`:

1. Compile frequently with `sbt "coreJVM/test:compile"`.
2. Run focused parser and kind tests while iterating.
3. Run `sbt "coreJVM/test; cli/test"` for the completed Scala change.
4. Run `./test_cli.sh` because accepted CLI source syntax changes.
5. Run `sbt "doc; paradox"` because published documentation changes.
6. Use the normal coverage entry point when checking changed-line coverage. No scalafmt run is required.

Repository tests and deterministic fixtures are sufficient for these claims; there is no deployment, credential, endpoint, or controlled-live prerequisite.

## Minimal design

The behavioral change is confined to `Statement.scala` and `ParserTest.scala`: accept `type` beside `struct`, map both to the existing node, render the new spelling, and cover the compatibility and ambiguity boundaries. No AST rename or downstream compiler edit is needed.

The two-file-only version is insufficient by itself because it fails the issue's explicit requirement that repository-owned source and documentation use `external type`; the predef, workspace libraries, embedded test programs, and documentation would continue teaching and primarily exercising the legacy spelling. The design therefore combines the selected parser/renderer fix with a finite mechanical migration of the observed occurrence set.

Removing support for `external struct` would be smaller still, but is rejected because the issue explicitly permits retaining it as an equivalent form and the compatible alias costs only one parser alternative. Creating separate old/new AST nodes or renaming the existing node is also rejected because neither changes the language semantics.

## Acceptance criteria

1. `external type Foo` and parameterized forms parse, typecheck, and behave like today's opaque external declarations.
2. `external struct Foo` remains accepted and lowers identically to `external type Foo`, ignoring source-region differences.
3. Parsing and rendering either spelling produces canonical source containing `external type Foo`.
4. External types remain constructorless, opaque, and subject to the current kind and variance rules, including invariant defaults.
5. Transparent aliases, ordinary structs, external values, and external functions have no grammar or semantic regression.
6. Repository-owned Bosatsu declarations and embedded test programs use `external type`, apart from the focused legacy compatibility fixture.
7. Documentation syntax examples and present-tense guidance use `external type`; changes to older artifacts are limited to this substantive syntax/terminology correction.
8. No serialized format, runtime representation, evaluator contract, or backend code-generation behavior changes.
9. Focused parser/property tests, core and CLI tests, CLI integration tests, and documentation generation pass under the repository's strict warning settings.

## Risks and mitigations

1. Parser ambiguity around a bindable named `type`: keep the declaration keyword soft and add the `external type: Foo` regression case.
2. Accidental semantic drift: retain the existing `ExternalStruct` node and all downstream lowering bodies unchanged, so both spellings necessarily use the established semantics.
3. Legacy compatibility may be under-tested after the repository migration: retain one explicit old/new equivalence property rather than leaving many unrelated tests on the old spelling.
4. Documentation and machine-readable code-plan twins could diverge: update the Markdown and JSON occurrences together and avoid unrelated regeneration or formatting.
5. Large-looking diff despite a small language change: constrain broad edits to exact spelling substitutions in the enumerated fixtures and documents; do not refactor adjacent parser, tooling, or type-system code.

## Rollout notes

This is an additive, backward-compatible parser release. No feature flag, data migration, runtime rollout, or staged activation is needed. New documentation and generated source immediately use `external type`; existing user code using `external struct` continues to work indefinitely unless a separate future issue defines a deprecation policy.

## Scope and size

Estimated size is **L** because the observed bounded inventory contains 30 likely touch paths: 2 compiler/resource files, 10 compiler test files, 6 checked-in Bosatsu workspace sources, 11 existing documentation/code-plan files, and this new design artifact. The semantic implementation is small and localized to the existing statement parser/renderer boundary; most breadth is mechanical migration required by the issue's source-and-documentation criterion.

Uncertainty is low for compiler behavior because all external declarations pass through the same parser and sealed statement hierarchy. The main uncertainty is whether review treats some historical prose occurrences as immutable issue history; omitting such a path would reduce breadth but would not change the architecture or test plan. Stable filenames derived from old issue titles are intentionally not renamed.
