---
issue: 2092
priority: 3
touch_paths:
  - docs/design/2092-show-test-timing-information.md
  - core/src/main/scala/dev/bosatsu/tool/Output.scala
  - core/src/main/scala/dev/bosatsu/Test.scala
  - c_runtime/bosatsu_runtime.h
  - c_runtime/bosatsu_runtime.c
  - core/src/test/scala/dev/bosatsu/Issue2092Test.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-09T18:45:09Z
---

# Issue #2092 design doc: show test timing information

_Issue: #2092 (https://github.com/johnynek/bosatsu/issues/2092)_

## Summary

Design doc content for adding per-package and total test execution timing to JVM and C test runners, with aligned output format, acceptance criteria, risks, and rollout notes.

---
issue: 2092
priority: 2
touch_paths:
  - docs/design/2092-show-test-timing-information.md
  - core/src/main/scala/dev/bosatsu/tool/Output.scala
  - core/src/main/scala/dev/bosatsu/Test.scala
  - c_runtime/bosatsu_runtime.h
  - c_runtime/bosatsu_runtime.c
  - core/src/test/scala/dev/bosatsu/Issue2092Test.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-03-09T00:00:00Z
---

# Issue #2092 Design: show test timing information

_Issue: #2092 (https://github.com/johnynek/bosatsu/issues/2092)_

Status: proposed  
Base branch: `main`

## Summary

Add elapsed timing to test output for both JVM (`tool test`) and C runtime execution (`lib test` and clang test execution):

1. Show per-package elapsed test time.
2. Show total elapsed test time in the final summary line.
3. Keep output shape as similar as possible between JVM and C, using a shared `in X.XXXs` duration style that is easy to translate to Python-style output where needed.

## Problem statement

Current test output reports pass/fail counts and failure details but does not report runtime duration per package or overall runtime. This makes it hard to identify slow packages and compare backend runtime behavior.

Issue #2092 asks for timing information in both JVM and C execution paths, with formatting that stays as similar as possible across backends.

## Goals

1. Report elapsed test time for each executed package.
2. Report elapsed total test time in the final summary line.
3. Keep JVM and C output formatting aligned.
4. Preserve existing exit-code behavior and failure semantics.
5. Preserve `--quiet` failure-detail suppression while still showing timings.

## Non-goals

1. No compile or typecheck timing in this issue; only test execution timing.
2. No redesign of test discovery or `ProgTest` semantics.
3. No new CLI flags.
4. No Python runtime redesign.

## Proposed architecture

### 1) JVM timing capture at execution boundary

Capture timing where tests are forced today: `Output.report` for `Output.TestOutput`.

Implementation shape:

1. Wrap each `Eval[Test].value` call with `System.nanoTime()` to capture per-package elapsed nanoseconds.
2. Capture total elapsed nanoseconds across the full evaluation loop with the same monotonic clock.
3. Keep sequential evaluation as-is (already required because `MatchlessToValue` is not thread-safe).

This keeps timing semantics clear: measured duration is runtime test execution, not compile/discovery.

### 2) JVM report rendering in `Test.scala`

Add timed rendering support while preserving existing call sites.

1. Keep existing `Test.outputFor(resultList, color, quiet)` behavior available.
2. Add a timed variant (or equivalent optional timing parameter) used by `Output.report`.
3. Add a shared duration formatter with seconds and millisecond precision: `X.XXXs`.
4. Extend output with:
   - a package timing section listing one elapsed value per executed package,
   - a final summary line that appends `in X.XXXs`.

Proposed summary shape:

`<N> tests, <P> passed <F> failed in <T.XXXs>`

Proposed timing section shape:

`package timings:`
`  Package/A: 0.123s`
`  Package/B: 0.004s`

Packages with no discovered tests (`None`) remain in existing missing-tests flow and are excluded from timing rows.

### 3) C runtime timing in `bosatsu_runtime`

Add timing storage and rendering in runtime test functions so generated and library test flows both pick it up.

1. Extend `BSTS_Test_Result` with `elapsed_nanos` (`uint64_t`) in `bosatsu_runtime.h`.
2. Add a monotonic time helper in `bosatsu_runtime.c` using `clock_gettime(CLOCK_MONOTONIC, ...)`.
3. In both `bsts_test_run` and `bsts_test_run_prog`, measure elapsed time around package test execution and store it in `BSTS_Test_Result`.
4. In `bsts_test_result_print_summary`:
   - print package timing rows for all package results,
   - compute and print total elapsed time in the final summary line,
   - preserve existing non-zero exit behavior when failures exist.

This keeps timing logic centralized in runtime code and avoids unnecessary codegen API changes.

### 4) Cross-backend output contract

To keep C and JVM as similar as possible:

1. Same duration unit and precision: `X.XXXs`.
2. Same overall summary suffix: `in X.XXXs`.
3. Both show one timing row per executed package.

Exact surrounding text can still differ slightly where backend output already differs (for example ANSI color handling), but timing tokens and ordering should match.

## Detailed implementation plan

1. Add timing capture in `core/src/main/scala/dev/bosatsu/tool/Output.scala` for `Output.TestOutput` evaluation.
2. Add timed-output rendering support in `core/src/main/scala/dev/bosatsu/Test.scala`.
3. Add or adjust a duration formatting helper in `Test.scala` to produce stable `X.XXXs` output.
4. Extend `BSTS_Test_Result` in `c_runtime/bosatsu_runtime.h` with elapsed timing field.
5. Add monotonic timing helper and per-package elapsed measurement in `c_runtime/bosatsu_runtime.c`.
6. Update `bsts_test_result_print_summary` to print package timings and total elapsed summary.
7. Add JVM-side regression tests for timing rendering and command output.
8. Validate C runtime compile/tests still pass and run a C test smoke path to verify output shape.

## Testing strategy

### JVM renderer tests

Add focused tests in `core/src/test/scala/dev/bosatsu/Issue2092Test.scala` (or equivalent) using fixed synthetic durations to assert deterministic rendering:

1. per-package timing section is present,
2. final summary includes `in X.XXXs`,
3. quiet mode still suppresses passing assertion details while showing timing rows.

### JVM command tests

Extend `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`:

1. `tool test` output contains timing rows for executed packages,
2. `tool test --quiet` still contains failure details and final summary, and also includes timing rows,
3. existing exit code expectations remain unchanged.

Use regex or contains checks for timing tokens, not exact numeric values from real clocks.

### C runtime validation

1. Build/runtime sanity via existing C runtime test flow (`c_runtime/Makefile` targets).
2. Run a C test execution smoke path (for example `lib test` in non-memory mode) and verify:
   - per-package timing rows appear,
   - final summary contains `in X.XXXs`,
   - failure exit code behavior is unchanged.

## Acceptance criteria

1. JVM `tool test` output includes elapsed timing for every executed package.
2. JVM `tool test` final summary line includes total elapsed time.
3. JVM `tool test --quiet` still suppresses passing assertion detail and still shows timing information.
4. C runtime test output includes elapsed timing for every executed package.
5. C runtime final summary line includes total elapsed time.
6. C and JVM timing formats both use `in X.XXXs` summary style and aligned per-package timing rows.
7. Existing success/failure exit code behavior is unchanged in both backends.
8. Regression tests are added or updated for JVM output behavior, and C runtime path is validated in CI/manual smoke.

## Risks and mitigations

1. Risk: output format changes may break downstream log parsers.  
Mitigation: keep existing pass/fail wording intact where possible and add timing in additive sections/suffixes.

2. Risk: flaky tests from runtime variability.  
Mitigation: use fixed synthetic durations in deterministic tests and pattern-based assertions in integration tests.

3. Risk: monotonic clock portability in C environments.  
Mitigation: use the same monotonic clock approach already present in runtime code and keep behavior conservative on clock-read failure.

4. Risk: quiet-mode output becomes noisier due timing lines.  
Mitigation: keep failure-detail suppression unchanged and keep timing section compact.

## Rollout notes

1. Land as a single output-focused PR covering JVM and C paths.
2. No user migration is required, but output snapshots/parsers may need updates.
3. Mention timing-output addition in release notes.
4. If parser breakage is reported, follow up with a machine-readable test-report mode rather than removing timings.
