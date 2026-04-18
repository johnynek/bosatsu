# Code Plan #2323

> Generated from code plan JSON.
> Edit the `.json` file, not this `.md` file.

## Metadata

- Flow: `small_job`
- Issue: `#2323` Silence deprecated REFERENCED_DYNAMICALLY warnings from vendored BDWGC on macOS
- Pending steps: `2`
- Completed steps: `0`
- Total steps: `2`

## Summary

Silence the macOS linker warnings at the vendored BDWGC source by teaching the Bosatsu vendored-dependency recipe to build BDWGC without the deprecated `REFERENCED_DYNAMICALLY` `.desc` directives on Darwin. Keep the fix local to BDWGC, preserve inherited compile flags, and make sure updated Bosatsu builds actually rebuild the cached `libgc.a`.

## Current State

Bosatsu currently vendors BDWGC 8.2.8 via `c_runtime/deps.json` and builds it in `VendoredDeps.runBdwgcRecipe` with a fixed CMake configure argument list: static library only, no tests, threads enabled, and no Darwin-specific compile definitions. The manifest is still at `recipe_version = 1`, and `CDeps.buildKey` hashes dependency identity, context, and transitive build keys but does not include `dependency.options`, even though the recipe behavior already depends on options such as `threadsafe`. The required repo gate is `scripts/test_basic.sh`, which exercises Scala tests but does not rebuild vendored C dependencies on macOS.

## Problem

Recent Apple toolchains emit deprecated `REFERENCED_DYNAMICALLY` linker warnings because the vendored BDWGC build preserves Mach-O `.desc` directives in Darwin `os_dep.c`. Suppressing linker warnings globally would hide unrelated problems, so the fix needs to live in the vendored BDWGC recipe itself. A recipe-only code change is not sufficient unless Bosatsu also forces cached BDWGC rebuilds for users who already have `libgc.a`, and the current build key omits manifest `options`, which is an adjacent cache-correctness flaw for any build-shaping dependency option. The final slice needs to remove the Darwin warning at the dependency build, preserve existing user/compiler flags, and make the vendored cache identity and manifest invalidation explicit enough that the change is actually observed on developer machines.

## Steps

1. [ ] `refactor-build-key-and-bdwgc-flags` Make Vendored Build Inputs Explicit and Darwin-Aware

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

2. [ ] `bump-recipe-version-and-verify` Invalidate Cached BDWGC Builds and Verify the Slice

Bump `c_runtime/deps.json` from `recipe_version = 1` to `recipe_version = 2` so the checked-in vendored manifest explicitly invalidates previously cached BDWGC builds under the updated recipe. Add a regression that parses the checked-in manifest and pins the expected BDWGC entry and recipe version, then verify the branch with the repo-required test gate. On a macOS toolchain, finish with a focused smoke check: rebuild vendored deps, build a representative Bosatsu executable or benchmark, and confirm the deprecated `REFERENCED_DYNAMICALLY` warnings no longer appear; optionally inspect `nm -m libgc.a` to confirm the symbols are no longer marked `[referenced dynamically]`.

#### Invariants

- The checked-in vendored dependency manifest remains parseable and keeps the same BDWGC source pin while intentionally changing only recipe invalidation state.
- Users picking up the new manifest do not silently reuse stale warning-producing BDWGC archives.
- The PR is not reviewable until `scripts/test_basic.sh` passes.

#### Property Tests

- None recorded.

#### Assertion Tests

- Unit test that reads and parses the checked-in `c_runtime/deps.json` and asserts the expected `recipe_version` and BDWGC dependency shape.
- `scripts/test_basic.sh`.
- Manual macOS smoke check: rebuild vendored deps, build a representative binary, and confirm the deprecated linker warnings are gone; optionally inspect `nm -m` output for `catch_exception_raise*`.
