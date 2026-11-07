---
issue: 2248
priority: 3
touch_paths:
  - docs/design/2248-suppress-unknown-export-follow-on-errors-for-definitions-that-already-failed-to-compile.md
  - core/src/main/scala/dev/bosatsu/PackageCustoms.scala
  - core/src/main/scala/dev/bosatsu/PackageError.scala
  - core/src/main/scala/dev/bosatsu/tool/CompilerApi.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-11-07T16:26:04Z
---

# Replace misleading unknown-export errors for failed definitions

_Issue: #2248 (https://github.com/johnynek/bosatsu/issues/2248)_

## Summary

Use source declarations retained by export assembly to distinguish uncompiled definitions from genuinely unknown names. Report an accurate export diagnostic while preserving original errors and package failure behavior.

## Outcome and guardrails

When a declared type fails compilation, exporting it will report `cannot export Module because its definition did not compile` instead of `unknown export Module`. The simplest implementation extends the existing export-assembly diagnostic mapping using source statements already retained in the typed program.

Repository guardrails are immutable public APIs, strict Scala compilation, existing diagnostic accumulation, and preserving export assembly’s failure behavior so an unresolved export does not produce an incomplete package interface. The issue explicitly permits either suppression or an accurate replacement diagnostic; this design selects replacement. No new failure-tracking subsystem or inference changes are required.

## Scope decisions requiring ratification

There are no scope amendments. Replacement is explicitly permitted by the issue and preserves its requested outcome.

Approval or merge of this design PR ratifies only amendments explicitly enumerated here; none are proposed.

## Investigation findings

The supplied issue-tree manifest contains only issue #2248, with no ancestor or dependency artifacts imposing additional requirements. The target design artifact does not exist on the inspected checkout.

The existing path is:

1. `SourceConverter.toProgram` retains the original statements in `Program.from`.
2. `Package.inferBodyUnopt` runs shape and kind inference. `KindFormula.solveShapesAndKinds` can return diagnostics alongside a partial environment, excluding failed declarations and declarations skipped because they depend on failures.
3. `Package.letsForTypeChecking` also excludes values that depend on removed types or constructors.
4. The inferred typed program still contains the original statements.
5. `PackageCustoms.assemble` calls `ExportedName.buildExports` against the surviving types and values. It currently translates every unresolved export into `PackageError.UnknownExport`, losing the distinction between absent and uncompiled declarations.

`ExportedName` already distinguishes binding exports, type exports, and constructor exports. In particular, `export T()` means export the defined type and its constructors; it does not name an individual enum branch. Aliases support type exports but do not support constructor exports.

There are two sites producing `UnknownExport`: assembly and `checkExprDagBindables`. The latter checks binding declarations using the expression DAG before type-recovery filtering. It already retains genuinely unknown binding diagnostics even when other checking fails. The defect here is assembly’s interpretation of its partial environment.

Existing `ErrorMessageTest` fixtures exercise failed local types, aliases, and dependent declarations, but do not assert that their export errors distinguish known declarations. The adjacent #2249 design concerns dependent unknown-type diagnostics inside inference; this design leaves that mechanism intact.

Inspection resolved the implementation choice. No executable experiments or tests were run during design, and the checkout remains unchanged.

## Minimal alternative considered

The selected minimal fix is to refine the existing mapping from unresolved exports to package diagnostics. Successful export construction remains unchanged. For an unresolved item, assembly checks whether the source declares that particular kind of export and chooses either an uncompiled-definition diagnostic or the existing unknown-export diagnostic.

Simply dropping matching errors inside assembly is insufficient: when every unresolved export names a failed declaration, assembly would have neither a nonempty error result nor a complete typed package to return. Correct suppression would require changing how assembly receives or combines earlier diagnostics, or changing its result shape. The permitted replacement avoids those changes and preserves the current failure contract.

Skipping export validation whenever any definition fails is also rejected: it would hide genuinely unknown exports in the same package.

## Detailed design

### Refine diagnostics at export assembly

Keep `ExportedName.buildExports` and the successful branch of `PackageCustoms.assemble` unchanged. In the `Validated.Invalid(unknowns)` branch, classify each unresolved source export against `program.from`.

Use a small local predicate in `PackageCustoms`, evaluated only on unresolved exports. It should use the existing statement hierarchy and declared names:

| Export form | Matching source declaration |
| --- | --- |
| `ExportedName.TypeName(name, _)` | A `TypeStatement` with that name, including definitions and aliases |
| `ExportedName.Constructor(name, _)` | A `TypeDefinitionStatement` with that type name |
| `ExportedName.Binding(name, _)` | A `Statement.ValueStatement` whose `names` contains the binding |

The binding case covers exported local values excluded because they depend on failed types. It is the same loss of declaration information at the same assembly boundary, rather than a separate diagnostic mechanism. `ValueStatement.names` already handles definitions, binding patterns, and external definitions.

Matching is local to the package being assembled and respects export kind. Do not include imported declarations, enum branch names as type names, or nested bindings. In particular, a source alias does not make `export Alias()` a valid constructor-export request.

For each unresolved export:

- If its export kind has a matching local source declaration, emit `PackageError.UncompiledExport`.
- Otherwise, emit the existing `PackageError.UnknownExport`, preserving its candidate regions and suggestions.

The successful lookup always takes precedence: a source declaration is consulted only after `buildExports` reports the item unresolved. This avoids changing successfully compiled exports or inventing replacement referants.

No persistent name registry, public lookup API, placeholder type, or additional inference result field is needed. The source statements are already present at the boundary that needs the distinction.

### Add the accurate diagnostic

Add an immutable `PackageError.UncompiledExport[A]` carrying the source export and package name, following the neighboring diagnostic style. Its message uses the existing source-map lookup and reads:

```text
in <source> cannot export Module because its definition did not compile
```

Use the exported name’s source representation consistently with existing diagnostics. Do not offer spelling suggestions for a name that is already declared. The wording also remains accurate for declarations skipped because a dependency failed; it does not claim that every skipped declaration has its own earlier error.

Classify the new diagnostic as a package error in `CompilerApi.classifyError`. It remains a hard error under the existing default behavior of `PackageError.isPostponable`; do not add it to the postponable cases.

### Preserve error and package lifecycle behavior

Assembly continues to return `Ior.Left` when export construction fails. Its unresolved-export list stays nonempty because each item becomes exactly one diagnostic. Earlier source-conversion and inference errors continue to accumulate through the existing `Ior` composition in `PackageMap`.

Do not filter source exports, rebuild a reduced public interface, or change cache admission, normalization, or dependency inference. Both production callers of `PackageCustoms.assemble`—ordinary package inference and internal predef compilation—use the refined mapping automatically.

The enforcing structure is the unresolved-export mapping inside assembly. Every failed export lookup reaching assembly passes through it. The other producer, `checkExprDagBindables`, receives the original converted lets and external definitions before the type-recovery filtering responsible for this issue; a binding removed later is therefore known at that earlier check. Preserve that check and its existing regression for genuinely missing bindings.

## Behavioral properties and acceptance criteria

1. The issue’s minimal `Module` repro retains the original missing-type diagnostics, emits no `UnknownExport` for `Module`, and reports the accurate replacement message.
2. The same distinction holds for failed struct and enum definitions, failed aliases exported as types, constructor-export requests for failed type definitions, and exported local values skipped because of those failures.
3. Declarations skipped transitively because they depend on failed local types are treated as uncompiled when exported, without requiring a separate error attached to each skipped declaration.
4. A genuinely absent export retains the existing `UnknownExport` diagnostic and suggestions, including when it appears beside exports of failed declarations.
5. Matching respects export kind. An alias does not satisfy a constructor-export request, and an enum branch name does not satisfy a type-export request merely because its enclosing enum failed.
6. Successfully constructed exports retain their existing referants and behavior. Original compilation diagnostics remain visible.
7. Unresolved exports still prevent assembly from returning a typed package. The replacement remains a hard package error and is rendered through the normal CLI diagnostic path.

Repository tests produced during implementation and CI supply the evidence for these criteria. No external service, deployment, or live-environment prerequisite is needed.

## Tests and validation

Extend `ErrorMessageTest.scala` using its existing `compileErrors` helper. Inspect the complete structured diagnostic list so absence assertions cannot pass by checking only a convenient matching error.

Use narrow case-based tests for the exact minimal repro and replacement text; genuine missing names and suggestions; alias-versus-constructor distinctions; and preservation of the root error category and ownership. Include both a missing-name root failure and a known-type shape or kind failure, so coverage does not depend solely on source-conversion recovery.

Add a small deterministic property-style matrix in the same suite. Vary supported export forms, direct versus transitive failed declarations, and the presence of an unrelated missing export. Across every fixture, assert that unresolved declared exports become `UncompiledExport`, absent exports remain `UnknownExport`, and original failures remain. Use existing syntax and fixture patterns; no new generator framework is necessary. This checks the classification and accumulation properties across the bounded operation family.

Extend `ToolAndLibCommandTest.scala` with a `runWithFiles` regression for `tool check`. Assert that the command fails, renders the root error and accurate replacement as normal diagnostics, and does not render `unknown export Module`. Add the corresponding warning/lax invocation to establish that the new diagnostic is not demoted. Existing successful package/export tests cover compatibility.

Follow `coding_style.md` during implementation:

```text
sbt "coreJVM/test:compile"
sbt "coreJVM/testOnly dev.bosatsu.ErrorMessageTest dev.bosatsu.ToolAndLibCommandTest"
sbt "coreJVM/test; cli/test"
./test_cli.sh
sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"
```

Review changed-line coverage against the repository’s goal of at least 95% overall coverage and ideally complete coverage of changed lines. Preserve strict warnings and explicit-null compatibility, import symbols normally, and do not run scalafmt. No code-generation or runtime behavior changes require the full backend matrix.

This change is a deterministic diagnostic classification at an existing boundary, not a workflow state machine. Focused case and property-style tests are sufficient; a separate formal model would not resolve an additional design uncertainty.

## Risks and rollout

The main risk is confusing a declared spelling with a declaration capable of satisfying that export form. The kind-specific predicate and alias/enum negative tests address this. A second risk is matching nested or imported names; use only top-level statements in this package’s `Program.from`.

The classification relies on unresolved local declarations being unavailable in the recovered typed program. It deliberately describes them as not compiled rather than attributing a particular earlier error. Tests must cover both actual failures and skipped dependents.

The public diagnostic ADT gains one case, so the exhaustive classifier must be updated under strict compilation. Existing `UnknownExport` construction and rendering remain unchanged for genuinely missing names.

Roll out through the normal merge process. There is no runtime, schema, serialization, or configuration migration. This selects the issue’s accurate-diagnostic alternative, so diagnostic count need not decrease; the misleading claim does disappear. Merging this issue-numbered design artifact establishes the `planned` milestone, and implementation can continue from issue #2248 afterward.

## Scope and size

Estimated size: **M**, based on six concrete touch paths: this managed design artifact, three production Scala files, and two existing test files. The implementation itself is small: one assembly classification, one diagnostic variant, one exhaustive rendering-classifier case, and focused regression coverage.

No build metadata or new testing infrastructure is expected. The material uncertainty is fixture coverage of values and transitive declarations removed by recovery; existing source retention and recovery code support the proposed approach. Type-inference recovery changes and unrelated diagnostic cleanup are outside this change.
