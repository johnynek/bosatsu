---
issue: 2158
priority: 3
touch_paths:
  - docs/design/2158-codecoverage-for-bosatsu.md
  - core/src/main/scala/dev/bosatsu/tool_command/TestCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/RuntimeCommandSupport.scala
  - core/src/main/scala/dev/bosatsu/library/LibraryEvaluation.scala
  - core/src/main/scala/dev/bosatsu/MatchlessFromTypedExpr.scala
  - core/src/main/scala/dev/bosatsu/Matchless.scala
  - core/src/main/scala/dev/bosatsu/MatchlessToValue.scala
  - core/src/main/scala/dev/bosatsu/tool/Output.scala
  - core/src/main/scala/dev/bosatsu/coverage/CoverageModel.scala
  - core/src/main/scala/dev/bosatsu/coverage/CoverageAggregator.scala
  - core/src/main/scala/dev/bosatsu/coverage/LcovWriter.scala
  - core/src/main/scala/dev/bosatsu/coverage/CoberturaWriter.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - core/src/test/scala/dev/bosatsu/MatchlessTests.scala
  - core/src/test/scala/dev/bosatsu/coverage/CoverageWritersTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-13T20:39:29Z
---

# Flag-Gated Code Coverage for Bosatsu Test Execution

_Issue: #2158 (https://github.com/johnynek/bosatsu/issues/2158)_

## Summary

Add an optional `tool test` coverage mode that records source-linked probes during Matchless lowering, counts hits in the evaluator, and emits CI-friendly reports (LCOV and Cobertura first, with JaCoCo-compatible retained data).

## Context
Issue #2158 asks for optional code coverage when running Bosatsu tests, with the expectation that coverage incurs measurable overhead and must be behind a flag.

Today, `tool test` compiles packages, lowers typed expressions into Matchless (`MatchlessFromTypedExpr`), and executes via the interpreter (`MatchlessToValue`) through `LibraryEvaluation`. There is no retained execution counter model tied to source locations.

## Goals
1. Add optional coverage collection to `tool test` without changing default behavior.
2. Produce machine-readable coverage output suitable for common CI/reporting tools.
3. Keep overhead isolated to explicit coverage mode.
4. Retain enough information to support at least two common formats now and a third without redesign.

## Non-goals
1. No `lib test` C-runtime source-level coverage in this issue.
2. No hard coverage thresholds/fail-under policy in this issue.
3. No IDE/UI coverage visualization in this issue.
4. No attempt to cover precompiled dependency packages when source paths are unavailable.

## State Of Practice And Format Choice
Across ecosystems, successful coverage systems share the same shape: static probe-to-source mapping plus runtime hit counters.

1. Python coverage.py tracks line execution and branch transitions (arcs), then exports XML/LCOV reports.
2. JaCoCo uses efficient probes and reports line and branch/instruction counters.
3. Cobertura and LCOV remain common interchange formats in CI platforms.

Chosen output plan:
1. Implement LCOV and Cobertura in this issue.
2. Keep internal data model JaCoCo-compatible so JaCoCo XML can be added without changing instrumentation.

## Coverage Data We Must Retain
For each executed probe:
1. Source package name.
2. Source region (`start`, `end` offsets).
3. Probe kind (`line` probe, `branch alternative` probe).
4. Optional top-level owner (`package::bindable`) for function-style summaries.
5. Runtime hit count.

For branch reporting:
1. Decision group id (one source decision point).
2. Ordered alternatives for that decision.
3. Hit count per alternative.

This is sufficient for:
1. LCOV: `SF`, `DA`, `BRDA`, `FN/FNDA` (optional function section).
2. Cobertura: per-file/per-line hits and branch condition coverage.
3. JaCoCo later: line-level `mi/ci/mb/cb` derivable from static probes + hits.

## Proposed Architecture
### 1) Coverage Mode In `tool test`
Add flag-gated options on `tool test`:
1. `--coverage` enables probe collection.
2. `--coverage_format` selects `lcov|cobertura` (default `lcov` when coverage is enabled).
3. `--coverage_out` writes machine-readable report to file; if omitted, print textual summary only.

When coverage is enabled, compile with `NoOptimize` via `RuntimeCommandSupport` to reduce source-to-execution distortion from aggressive rewrites.

### 2) Static Probe Registration During Matchless Lowering
Instrument at lowering time, not by changing Bosatsu language semantics:
1. Extend `Matchless.fromLet` with optional coverage recorder callbacks.
2. For each compiled typed-expression root that has usable source region metadata, register a probe id against the produced Matchless expression object identity.
3. For each typed `Match`, additionally register a branch decision group with one alternative per branch body.

This avoids invasive AST-wide payload changes while still mapping execution to source regions.

### 3) Runtime Hit Collection In Evaluator
Extend `MatchlessToValue` with an optional expression-entry callback:
1. On each expression evaluation, look up probe id by expression identity.
2. Increment counter if present.
3. No-op callback in default mode keeps current behavior.

`LibraryEvaluation` owns the coverage session (static metadata + mutable counters) for the test run.

### 4) Source Mapping And Filtering
Coverage should only emit files we can map back to source paths:
1. Use `(Path, PackageName)` mapping already returned by compilation (`RuntimeCommandSupport.packMap`) to map package to source file path.
2. Build `LocationMap` per source file to convert probe offsets to line numbers.
3. Skip probes for packages without known source path (typically precompiled deps), and surface a concise note in coverage summary.

### 5) Aggregation And Report Writers
Introduce a small coverage module:
1. Canonical normalized model: files, lines, branch groups, hit counts.
2. `LcovWriter` renderer.
3. `CoberturaWriter` renderer.

`Output.TestOutput` gains optional coverage payload so test execution and coverage emission happen in one reporting pass.

## Implementation Plan
1. Add coverage option parsing to `TestCommand` and plumb configuration through runtime command flow.
2. Update `RuntimeCommandSupport.packMap` to accept compile options from caller.
3. Add coverage model classes (`CoverageModel`, `CoverageAggregator`) for static probes and runtime counters.
4. Extend `Matchless.fromLet` and `MatchlessFromTypedExpr.compile` to register probes and branch groups during lowering.
5. Extend `MatchlessToValue` with optional expression-entry callback.
6. Extend `LibraryEvaluation` to create/store coverage session and expose snapshot after tests.
7. Extend `Output.TestOutput` reporting path to emit summary and optional file output.
8. Add LCOV and Cobertura writers from canonical aggregated data.
9. Add CLI integration and regression tests.
10. Document usage in user docs.

## Testing Strategy
1. Unit tests for probe registration stability and deterministic probe ids per compile run.
2. Unit tests for LCOV rendering (`DA`, `BRDA`, `end_of_record`).
3. Unit tests for Cobertura rendering (line hits, branch condition coverage).
4. Integration test: `tool test` unchanged output when coverage flags are absent.
5. Integration test: `tool test --coverage --coverage_format lcov --coverage_out ...` writes valid report and keeps test pass/fail semantics.
6. Integration test: uncovered branch produces partial branch data in emitted report.
7. Integration test: packages without source path are skipped from report with clear summary note.

## Acceptance Criteria
1. `tool test` supports opt-in coverage flags.
2. Default `tool test` behavior and output remain unchanged when coverage is not requested.
3. Coverage mode records execution counts tied to source regions.
4. Coverage mode emits valid LCOV output.
5. Coverage mode emits valid Cobertura XML output.
6. Coverage collection is performed in the same run as test execution (no second run required).
7. Branch alternatives from source `match` expressions are represented in coverage output.
8. Precompiled dependency packages without source path are not emitted as malformed files.
9. Existing test command pass/fail semantics are unchanged.
10. New tests cover parser flags, runtime collection, and both report writers.

## Risks And Mitigations
1. Risk: Runtime overhead from per-expression callbacks.
   Mitigation: callback only active under `--coverage`; keep default path zero-feature/no-op.
2. Risk: Source fidelity drift due optimization rewrites.
   Mitigation: use `NoOptimize` in coverage mode for `tool test`.
3. Risk: Probe identity collisions from shared singleton expressions.
   Mitigation: register probes only for nodes with real source regions and ignore singleton synthetic constants.
4. Risk: Report incompatibility across consumers.
   Mitigation: golden tests for emitted LCOV/Cobertura structure and sample ingestion checks in CI follow-up.

## Rollout Notes
1. Land instrumentation + canonical aggregation + LCOV first.
2. Land Cobertura writer immediately after, using same canonical aggregation.
3. Keep JaCoCo as a narrow follow-up renderer on the same retained data model.
4. Announce coverage as experimental for one release cycle; solicit real-world report ingestion feedback.

## References
1. LCOV tracefile fields (`SF`, `FNDA`, `BRDA`, `DA`, `end_of_record`): https://manpages.debian.org/trixie/lcov/geninfo.1.en.html
2. Cobertura usage and ecosystem support in GitLab coverage reports: https://docs.gitlab.com/ci/testing/code_coverage/cobertura/
3. JaCoCo line XML example fields (`nr`, `mi`, `ci`, `mb`, `cb`) in GitLab ingestion docs: https://docs.gitlab.com/ci/testing/code_coverage/jacoco/
4. JaCoCo counter model (instructions, branches, lines): https://www.eclemma.org/jacoco/trunk/doc/counters.html
5. Coverage.py branch/arcs model and multi-format outputs: https://coverage.readthedocs.io/en/7.13.1/branch.html and https://coverage.readthedocs.io/en/7.13.4/commands/index.html
6. Broad CI ingestion support for LCOV/Cobertura/JaCoCo in Codecov: https://docs.codecov.com/docs/supported-report-formats
