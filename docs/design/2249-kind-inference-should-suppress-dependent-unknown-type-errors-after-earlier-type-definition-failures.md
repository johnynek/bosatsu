---
issue: 2249
priority: 3
touch_paths:
  - docs/design/2249-kind-inference-should-suppress-dependent-unknown-type-errors-after-earlier-type-definition-failures.md
  - core/src/test/scala/dev/bosatsu/KindFormulaTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
depends_on: []
estimated_size: S
generated_at: 2026-11-07T03:35:50Z
---

# Lock in suppression of unknown-type cascades after local type failures

_Issue: #2249 (https://github.com/johnynek/bosatsu/issues/2249)_

## Summary

The current alias-aware solver already suppresses failures that depend on earlier failed local declarations. Preserve that implementation and add focused regression coverage for shape failures, kind failures, transitive dependencies, aliases, and package diagnostics.

## Outcome and guardrails

Report the original type-definition failure without subsequently describing that failed local type, or its skipped dependents, as unknown. The simplest implementation on the inspected `main` is to preserve the existing failed-set guard in `KindFormula.solveShapesAndKinds` and add regression coverage through the solver and package compiler.

Applicable repository guardrails are immutable public APIs, existing partial-result and error-accumulation behavior, strict Scala compilation, and the testing guidance in `coding_style.md`. The issue permits either suppression or an explicit dependency diagnostic; this design selects suppression, which the current implementation already provides. Export diagnostics are separate from this issue.

## Scope decisions requiring ratification

There are no scope amendments. All requested behavior remains in scope. Selecting suppression is an alternative explicitly allowed by the issue, and retaining an implementation already present on `main` does not narrow the requested outcome.

Approval or merge of this design PR ratifies only amendments explicitly enumerated here; none are proposed.

## Investigation findings

The issue manifest supplies only issue #2249; it contains no ancestor or dependency artifacts imposing additional requirements. The target design document does not yet exist.

The inspected implementation already contains the relevant repair:

- `Package.inferBodyUnopt` calls the `ParsedTypeEnv` overload of `KindFormula.solveShapesAndKinds`.
- That overload processes `parsedTypeEnv.orderedTypes.reverse`, restoring declaration order because `ParsedTypeEnv.addDefinedType` and `addTypeAlias` prepend statements.
- Its fold carries successful definitions, successful aliases, successful ordered statements, and a set of failed type constants.
- Before either statement variant enters shape or kind solving, the fold checks whether its `dependsOn` intersects the failed set. A matching statement is skipped and its own constant is added to the set.
- `solvedStmt` adds declarations that actually fail shape or kind solving to the same set, while retaining their diagnostics through `Ior.Both`.

This guard is already present in the type-alias implementation commit `ad3f46215`. `DefinedType.dependsOn` collects constants from constructor argument types; `TypeAlias.dependsOn` collects constants from the alias body. Both use qualified type identities.

`KindFormulaTest` exercises successful aliases and invalid annotations, but its `makeTE` helper discards partial successful results whenever errors occur. It therefore cannot directly assert the recovery behavior needed here. `ErrorMessageTest` covers related variance and constructor-error cascades, but does not directly assert suppression after a missing-constant shape failure or across mixed alias/definition chains.

Inspection resolves the implementation choice without an executable experiment. No tests were run during this design investigation, and the checkout remains unchanged.

## Minimal alternative considered

The smallest production repair would extend the existing declaration fold with a failed-constant set: record actual failures, check dependencies before both solving branches, and mark skipped dependents failed so suppression propagates transitively.

That mechanism is already implemented, so it is selected and retained. The remaining substantive change is regression coverage. A second failed-name registry, placeholder kinds, a new dependency diagnostic, or filtering rendered `UnknownConst` messages would add mechanisms without improving the requested outcome. Filtering diagnostics would also act too late to preserve solver recovery behavior reliably.

## Detailed design

### Preserve the existing recovery boundary

Keep production code unchanged on the inspected revision. Extend tests around the existing `ParsedTypeEnv` overload rather than refactoring its accumulator or changing public APIs.

For each statement, the existing behavior is:

| Condition | Solve statement? | Additional errors | Resulting environment |
| --- | --- | --- | --- |
| References an earlier failed or skipped declaration | No; mark this statement failed | None from this statement | Unchanged |
| Solving fails | Yes; mark this statement failed | Preserve the original errors | Unchanged |
| Solving succeeds | Yes | None | Include the solved statement |

Failure tracking applies to shape failures and kind failures, and to definitions and aliases alike. Skipping a declaration suppresses all diagnostics that solving that declaration might have produced. This is the existing suppression policy permitted by the issue; independent declarations still receive normal checking.

### Enforcing structure and bounded path inventory

The enforcing structure is the guard in the ordered fold, positioned before dispatch to either `TypeStatement.Defined` or `TypeStatement.Alias`. Both branches record actual failures through `solvedStmt`; neither can bypass the guard. Marking a skipped statement failed establishes transitive suppression without a separate graph traversal.

The in-scope package path is `Package.inferBodyUnopt` → the `ParsedTypeEnv` overload → the guarded fold → shape and kind solving. Source conversion retains local type constants even when their declarations later fail inference, so dependency checks can identify them.

The list overload, used by the predef test, instead runs `Shape.solveAll` followed by `KindFormula.solveAll`. It is not the package path implicated by this regression. Keep it unchanged; extending standalone shape-solving APIs is not necessary to meet this issue.

### Solver regression coverage

Add tests directly against `KindFormula.solveShapesAndKinds(imports, parsedTypeEnv)` in `KindFormulaTest.scala`. Pattern-match the returned `Ior` so tests inspect both diagnostics and the partial solved environment; do not route these assertions through `makeTE`.

Use narrow fixtures for:

1. The issue's `Bad`/`UsesBad` example, plus a third dependent. Assert the original `Shape.UnknownConst` names `Missing` in `Bad`, with no error owned by either dependent.
2. A declaration that passes shape solving but fails kind/variance solving, followed by dependents. Existing incompatible variance-annotation fixtures provide suitable roots. Assert the root error category explicitly so the test cannot accidentally exercise only shape failure.
3. Failed definitions used by aliases and failed aliases used by definitions. Include a failing alias body and a kind-invalid alias, using existing alias syntax and annotation fixtures.
4. Independent valid and invalid declarations interleaved with the failed chain. Valid declarations remain in the partial result; independent failures retain their own errors.
5. An ordinary unknown constant without an earlier failed dependency. Its error must remain visible.

For the missing-name fixture, `TestUtils.parsedTypeEnvOf` deliberately accepts source conversion's recoverable `Ior.Both`. This isolates inference assertions; package-level tests separately retain the source-conversion diagnostics.

Add a small property-style check in the same suite: enumerate chain lengths one through four and all definition/alias choices for the dependent links. Give each link exactly one dependency on its predecessor, alternating the root fixtures between shape and kind failure. For every generated chain, assert that extending the chain adds no solver errors and no solved dependent declarations. This checks transitive suppression across both statement variants without introducing a new generator framework or production abstraction. Include an unrelated valid declaration to ensure checking continues.

Use case-based tests for exact error ownership, root error category, and independent failures; these need precise fixtures rather than broad generation.

### Package diagnostic coverage

Add a regression in `ErrorMessageTest.scala` using its existing `compileErrors` helper and the issue's package example. Inspect structured `PackageError.KindInferenceError` values and their underlying declaration/error identities, then render messages with the existing source map.

Assert that the root failure remains visible and no kind/shape diagnostic describes `Bad` as unknown inside `UsesBad`. Extend the fixture to a third dependent to cover propagation through the real package path. Retain source-conversion errors for the genuinely missing name. Avoid asserting the total package error count, since export diagnostics are explicitly separate.

Use a second small package fixture with a known type applied at an invalid shape, such as the existing `type Bad = Int[Int]` pattern followed by dependents. This reaches inference without relying on a missing-name recovery diagnostic.

## Behavioral properties and acceptance criteria

1. An actual shape or kind failure retains its original diagnostics and declaration ownership.
2. A later statement referencing a previously failed local declaration is skipped before shape/kind solving and is itself tracked as failed. The same property holds transitively across definitions and aliases.
3. Failed and skipped declarations do not enter the partial solved environment. Independent valid declarations still appear in the existing successful-result order.
4. An earlier unrelated failure does not suppress an independent declaration's errors. A genuinely unknown constant still produces its normal diagnostic when its statement is not skipped for a failed dependency.
5. Through package compilation, the issue's repro reports the original failure and produces no dependent unknown-type diagnostic for `Bad` or later skipped declarations.
6. Existing import lookup, successful kind inference, error rendering, external definitions, and export diagnostics retain their current behavior.

The new solver tests establish properties 1–4; the package tests establish property 5; existing suites plus the prescribed validation cover compatibility in property 6. All evidence is produced by repository tests during implementation and CI. No external environment or deployment evidence is required.

This is a finite declaration fold, not a workflow state machine. The key inductive property is that the failed set contains every earlier actual failure and every earlier statement skipped because of that set. Focused property-style tests are sufficient; a separate formal model is unnecessary.

## Validation

Follow `coding_style.md`:

```text
sbt "coreJVM/test:compile"
sbt "coreJVM/testOnly dev.bosatsu.KindFormulaTest dev.bosatsu.ErrorMessageTest"
sbt "coreJVM/test; cli/test"
sbt "coverage; clean; coreJVM/test; cli/test; coverageReport"
```

Keep handwritten Scala compatible with strict warnings and explicit nulls, and do not run scalafmt. Review coverage for the new assertions and exercised recovery paths against the repository's coverage expectations. CLI smoke tests and the code-generation matrix are unnecessary for this test-only compiler change.

## Risks and rollout

The main testing risk is exercising source conversion instead of the intended inference failure. Direct solver assertions and the known-type shape-error package fixture distinguish these stages. A second risk is a test that merely checks compilation fails; assertions must inspect root ownership, absence of dependent errors, and surviving declarations.

The inspected source already implements suppression, so the proposed tests should pass without production changes. If a focused regression fails, investigate whether it exposes an in-scope defect at the existing fold before changing production behavior; do not weaken the assertions to accept a misleading unknown-type diagnostic.

There is no runtime, schema, serialization, or deployment change. Rollout is the normal merge process. Merging this issue-numbered design artifact establishes the `planned` milestone; implementation can continue from issue #2249 afterward.

## Scope and size

Estimated size: **S**. Three concrete touch paths are expected: this design artifact and two existing Scala test files. Production implementation is already present, and no build metadata or new test framework is needed.

Complexity is limited to constructing fixtures that reach the intended stage and checking partial results accurately. The uncertainty is whether execution exposes an overlooked inference-path defect; static inspection found none. The document preserves the required behavior without scheduling a speculative solver rewrite.
