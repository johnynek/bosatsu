---
issue: 1908
priority: 3
touch_paths:
  - docs/design/1908-lib-build-should-allow-only-giving-the-e-exe-out-option.md
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/test/scala/dev/bosatsu/LibBuildImplicitPackageTest.scala
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Ci.bosatsu
  - .github/workflows/ci.yml
depends_on: []
estimated_size: M
generated_at: 2026-03-02T03:32:15Z
---

# Issue #1908 design doc: allow `lib build` with only `--exe_out`

_Issue: #1908 (https://github.com/johnynek/bosatsu/issues/1908)_

## Summary

Design for making `lib build -e/--exe_out` valid without `--outdir/-o`, ensuring cwd-relative executable output, temp `.bosatsuc/tmp` intermediates, and coverage in both `bosatsuj` and `bosatsu_node` test workflows.

---
issue: 1908
title: lib build should allow only giving the `-e` `--exe_out` option
status: proposed
base_branch: main
touch_paths:
  - docs/design/1908-lib-build-should-allow-only-giving-the-e-exe-out-option.md
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala
  - core/src/test/scala/dev/bosatsu/LibBuildImplicitPackageTest.scala
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu
  - test_workspace/Bosatsu/Example/Json/Github/Workflows/Ci.bosatsu
  - .github/workflows/ci.yml
depends_on: []
estimated_size: M
generated_at: 2026-03-02
---

# Issue #1908 Design: allow `lib build -e/--exe_out` without `--outdir` or `-o`

Issue: #1908 (https://github.com/johnynek/bosatsu/issues/1908)  
Base branch: `main`

## Summary

`lib build` currently requires `--outdir` or `-o`, and executable output is always resolved relative to `outdir`. This breaks the intended behavior for:

`./bosatsu lib build --main_pack MyLib/ParensProbs -e pp`

This design makes `-e` a valid standalone output selector, keeps intermediate C artifacts in a temporary directory under `.bosatsuc/tmp`, and writes the executable to the exact path given by `-e` (relative to current working directory when relative).

## Problem statement

Current behavior has two blockers:

1. `lib build` parser rejects commands when both `--outdir` and `-o` are absent, even if `-e` is provided.
2. `ClangTranspiler` always computes executable path as `resolve(outDir, exeOut)`. When `outdir` is omitted, `lib build` uses a temp outdir, so the built executable lands in temp and is deleted during cleanup.

Result: no-outdir `-e` builds do not leave the executable at the requested target path.

## Goals

1. Allow `lib build` with only `-e/--exe_out`.
2. If `-e` is relative, resolve it relative to current working directory.
3. Preserve existing behavior for explicit `--outdir` flows.
4. Continue using `.bosatsuc/tmp` for temporary output directories when no outdir is given.
5. Add coverage in tests that exercise both `./bosatsuj` and `./bosatsu_node`.

## Non-goals

1. No behavior change for `tool transpile c` output semantics.
2. No behavior change for `lib test` output semantics.
3. No change to compiler/linker flag handling.

## Proposed architecture

### 1) `lib build` output-option validation includes `-e`

In `core/src/main/scala/dev/bosatsu/library/Command.scala`:

1. Replace the current `--outdir`/`-o` requirement with validation that at least one of `--outdir`, `-o`, `-e` is set.
2. Preserve current behavior when `--outdir` is provided.
3. For `-e`-only mode, leave `outDirOpt = None` so existing `withTempPrefix(build_outdir)` is used.
4. Keep C output temp-local in that mode (default `output.c` relative to temp outdir).

### 2) Add executable path-resolution mode to `ClangTranspiler.Output`

In `core/src/main/scala/dev/bosatsu/codegen/clang/ClangTranspiler.scala`:

1. Extend `ClangTranspiler.Output` with an executable path mode flag, analogous to `cOutRelativeToOutDir`.
2. Proposed field: `exeOutRelativeToOutDir: Boolean`.
3. Use this flag in both places where executable paths are resolved:
   - compile target path (`ccConf.compile`)
   - test executable invocation path in test mode (`platformIO.system`)
4. Default this flag to `true` for existing call sites so behavior stays unchanged unless explicitly overridden.

### 3) No copy/move step required

Issue text suggests compile in temp then move. We can avoid move/copy by compiling directly to the final executable path whenever `exeOutRelativeToOutDir = false`. This avoids permission/metadata risks and avoids adding new filesystem API surface.

### 4) Temp directory behavior remains under `.bosatsuc/tmp`

`lib build` already uses `platformIO.withTempPrefix(build_outdir)` when `--outdir` is absent, and platform implementations already place temp dirs under `.bosatsuc/tmp` when git root is known. This design reuses that behavior for no-outdir builds.

## Detailed implementation plan

1. Update `lib build` parser wiring in `Command.scala` to accept `-e` without requiring `--outdir`/`-o`.
2. Keep parse failure when none of `--outdir`, `-o`, `-e` is provided.
3. Add `exeOutRelativeToOutDir` to `ClangTranspiler.Output`.
4. Route executable path resolution through the new flag in compile and test-exec branches.
5. Set flags per `lib build` case:
   - explicit `--outdir`: `exeOutRelativeToOutDir = true`
   - no `--outdir`: `exeOutRelativeToOutDir = false`
6. Keep non-`lib build` output constructors (`lib test`, generic clang opts) on default `true`.
7. Add parser-level unit coverage in `LibBuildImplicitPackageTest.scala`.
8. Add end-to-end workflow coverage for no-outdir `-e` in both `bosatsuj` and `bosatsu_node` command paths.
9. Regenerate/update `.github/workflows/ci.yml` from workflow source changes to keep parity green.

## Testing strategy

### Core parser tests

In `core/src/test/scala/dev/bosatsu/LibBuildImplicitPackageTest.scala`:

1. Add success case for `lib build --repo_root repo -m MyLib/Fib -e pp` at parse level.
2. Keep missing-output-selector failure test, but update expectation to require one of `--outdir`, `-o`, `-e`.

### End-to-end compiler workflow tests

In `test_workspace/Bosatsu/Example/Json/Github/Workflows/Util.bosatsu`, `.../Ci.bosatsu`, and generated `.github/workflows/ci.yml`:

1. JVM CLI (`bosatsuj`) no-outdir executable build:
   - `./bosatsuj lib build --main_pack Bosatsu/FibBench --exe_out fib_bench_no_outdir --cc_flag=-O0 --cc_lib=-lm`
   - `./fib_bench_no_outdir 10`
2. Node CLI (`bosatsu_node`) no-outdir executable build:
   - `./bosatsu_node lib build --main_pack Bosatsu/FibBench --exe_out fib_bench_node --cc_flag=-O0 --cc_lib=-lm`
   - `./fib_bench_node 10`

These checks ensure executable output lands in working directory and is runnable for both compilers.

## Acceptance criteria

1. `./bosatsu lib build --main_pack MyLib/ParensProbs -e pp` succeeds and creates `./pp`.
2. Relative `-e` paths are interpreted relative to current working directory.
3. `lib build` still fails parse when none of `--outdir`, `-o`, `-e` is supplied.
4. Existing `--outdir` behavior remains unchanged.
5. No-outdir builds keep intermediate artifacts in temp directories under `.bosatsuc/tmp`.
6. CI coverage includes a no-outdir `-e` build for `bosatsuj`.
7. CI coverage includes a no-outdir `-e` build for `bosatsu_node`.
8. Workflow parity remains green between workflow source and `.github/workflows/ci.yml`.

## Risks and mitigations

1. Risk: regressions in existing outdir-relative executable behavior.
   Mitigation: default `exeOutRelativeToOutDir = true`; only disable for `lib build` no-outdir cases.

2. Risk: accidentally deleting outputs if still temp-relative.
   Mitigation: explicit no-outdir flag path plus runnable CI assertions in both compiler jobs.

3. Risk: extra CI time from added compile/run checks.
   Mitigation: keep added command small and reuse existing FibBench invocation.

4. Risk: workflow source/yaml drift.
   Mitigation: update `test_workspace` workflow definitions and generated `.github/workflows/ci.yml` in the same PR.

## Rollout notes

1. Ship parser/transpiler update and tests in one PR.
2. Validate with focused tests:
   - `coreJVM/testOnly dev.bosatsu.LibBuildImplicitPackageTest`
   - workflow parity tests for `.github/workflows/ci.yml`
3. Merge after CI confirms no-outdir `-e` scenarios pass in both `testC` (`bosatsuj`) and `testWithNode` (`bosatsu_node`).
4. No migration or feature flag is required; this is backward-compatible for existing `--outdir` usage.
