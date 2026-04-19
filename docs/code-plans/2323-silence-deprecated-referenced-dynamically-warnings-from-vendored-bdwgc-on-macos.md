# Code Plan #2323

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2323` Silence deprecated REFERENCED_DYNAMICALLY warnings from vendored BDWGC on macOS
- Pending steps: `0`
- Completed steps: `2`
- Total steps: `2`

## Summary

Silence the macOS linker warnings at the vendored BDWGC source by teaching the Bosatsu vendored-dependency recipe to build BDWGC without the deprecated `REFERENCED_DYNAMICALLY` `.desc` directives on Darwin. Keep the fix local to BDWGC, preserve inherited compile flags, make vendored caches rebuild when recipe-shaping options change, and verify the result with the repo-required gate plus a real macOS smoke build.

## Current State

On this branch, `VendoredDeps` computes BDWGC CMake configure args through a small pure helper, appends `-DNO_DESC_CATCH_EXCEPTION_RAISE` only for Darwin/macOS builds without dropping inherited `CFLAGS`, and `CDeps.buildKey` now hashes normalized `dependency.options` so the checked-in BDWGC manifest already invalidates stale vendored caches. Focused `CDepsTest` and `VendoredDepsTest` coverage pins the option-aware build key behavior, object-order normalization, Darwin vs Linux configure args, debug vs release build types, and Darwin `CFLAGS` preservation. Verification is now complete on macOS arm64 as of 2026-04-18: `scripts/test_basic.sh` passed, `sbt -batch "cli/testOnly dev.bosatsu.GithubWorkflowJsonParityTest -- --log=failure"` passed after the earlier transient GitHub `504` cleared, a fresh `bosatsuj c-runtime install` plus `bosatsuj build --main_pack Bosatsu/FibBench --exe_out fib_bench` smoke run produced a runnable binary, and neither the install/build logs nor `nm -m` on vendored `libgc.a` showed the deprecated `REFERENCED_DYNAMICALLY` markers.

## Problem

Recent Apple toolchains on `main` emit deprecated `REFERENCED_DYNAMICALLY` linker warnings whenever Bosatsu links against vendored BDWGC, obscuring normal macOS build output. Fixing the Darwin recipe alone was not sufficient until the branch also proved that vendored cache invalidation works, the repo-required gate is green again after the transient GitHub release fetch failure, and a real macOS rebuild removes the targeted warning from actual artifacts.

## Steps

1. [x] `refactor-build-key-and-bdwgc-flags` Make Vendored Build Inputs Explicit and Darwin-Aware

Refactor the BDWGC recipe so the CMake configure arguments are assembled by a small pure helper that takes the normalized host OS, profile, relevant paths, and any inherited `CFLAGS`, then use that helper from `runBdwgcRecipe`. In the same slice, extend `CDeps.buildKey` to include `dependency.options` so manifest options are part of the vendored cache identity. With that plumbing in place, append `-DNO_DESC_CATCH_EXCEPTION_RAISE` only for Darwin/macOS BDWGC builds, while preserving any existing compiler flags instead of overwriting them.

#### Invariants

- Any build-shaping vendored dependency option changes the computed build key; env insertion order and normalized OS/toolchain behavior remain stable.
- Darwin/macOS BDWGC builds add `NO_DESC_CATCH_EXCEPTION_RAISE` without dropping inherited `CFLAGS`.
- Non-Darwin BDWGC configure args remain semantically unchanged aside from the helper extraction, and the existing threaded static-library recipe behavior is preserved.

#### Property Tests

- `CDeps.buildKey` remains stable across env map insertion order but changes when otherwise-identical dependencies differ only in `options`.
- For generated safe `CFLAGS` token lists, the Darwin configure-arg helper retains every original token and injects `-DNO_DESC_CATCH_EXCEPTION_RAISE` exactly once.

#### Assertion Tests

- Case-based unit tests for macOS vs Linux configure args to pin the Darwin-only define and the unchanged common CMake switches (`BUILD_SHARED_LIBS`, `BUILD_TESTING`, `enable_threads`).
- Case-based unit tests for debug vs release profiles so `CMAKE_BUILD_TYPE` remains correct after the helper extraction.
- Case-based unit test that empty inherited `CFLAGS` on macOS still produces a valid compile-flags entry containing `-DNO_DESC_CATCH_EXCEPTION_RAISE`.

#### Completion Notes

Extracted `VendoredDeps.bdwgcConfigureArgs` so `runBdwgcRecipe` now builds its CMake configure args from normalized host data and inherited flags instead of hardcoding a fixed list in place. Darwin builds append `-DNO_DESC_CATCH_EXCEPTION_RAISE` through `-DCMAKE_C_FLAGS=...` while preserving existing `CFLAGS`, including the already-present-define case. `CDeps.buildKey` now hashes recursively normalized `dependency.options`, so the checked-in BDWGC manifest already invalidates stale caches without a separate recipe-version bump. Added focused unit and ScalaCheck coverage for option-sensitive build keys, option-order normalization, Darwin/Linux arg differences, debug/release build types, and Darwin `CFLAGS` preservation.

2. [x] `verify-required-gate-and-macos-smoke` Finish External Verification and macOS Smoke Testing

Rerun the repo-required gate once the external GitHub release fetch used by `GithubWorkflowJsonParityTest` is healthy again, then finish with a real macOS smoke check: rebuild vendored deps, build a representative Bosatsu executable or benchmark, and confirm the deprecated `REFERENCED_DYNAMICALLY` warnings are gone. Optionally inspect `nm -m libgc.a` to confirm `catch_exception_raise*` are no longer marked `[referenced dynamically]`.

#### Invariants

- BDWGC cache invalidation for the checked-in manifest now comes from the option-aware build key, so no additional manifest recipe-version bump is required for this fix.
- The PR is not reviewable until `scripts/test_basic.sh` passes on a healthy network path.
- macOS rebuilds must complete without the deprecated linker warnings.

#### Property Tests

- None recorded.

#### Assertion Tests

- `scripts/test_basic.sh`.
- Focused rerun of `cli/testOnly dev.bosatsu.GithubWorkflowJsonParityTest` once the external GitHub release fetch stops returning 504.
- Manual macOS smoke check: rebuild vendored deps, build a representative binary, and confirm the deprecated linker warnings are gone; optionally inspect `nm -m` output for `catch_exception_raise*`.

#### Completion Notes

Completed verification on 2026-04-18. `scripts/test_basic.sh` passed end-to-end, and a focused rerun of `dev.bosatsu.GithubWorkflowJsonParityTest` passed once the GitHub release fetch recovered from the earlier transient `504`. On macOS arm64, `sbt -batch "cli/assembly"` produced `bosatsuj`, a fresh `./bosatsuj c-runtime install --repo_root . --archive ... --git_sha 9e37e871cc3ae2caf4dfe1d216590d420e51e78c --profile release` rebuilt vendored BDWGC under `.bosatsuc`, and `./bosatsuj build --outdir c_out_build --main_pack Bosatsu/FibBench --exe_out fib_bench --cc_flag=-O1 --cc_flag=-g --cc_lib=-lm` linked and ran successfully (`fib(10) = 89`). `nm -m` on the rebuilt vendored `libgc.a` still shows the `catch_exception_raise*` symbols, but no `[referenced dynamically]` marker remains, and neither install nor build logs emitted `ld: warning: REFERENCED_DYNAMICALLY flag on symbol ...`. The vendored BDWGC build still emits separate AppleClang compile-time deprecation warnings around `get_etext`/`get_end`/`getsectbynamefromheader_64`/`_dyld_bind_fully_image_containing_address`, but those are distinct upstream warnings and not the linker warning addressed by this issue.
