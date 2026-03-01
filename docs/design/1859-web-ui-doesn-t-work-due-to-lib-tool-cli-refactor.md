---
issue: 1859
priority: 3
touch_paths:
  - docs/design/1859-web-ui-doesn-t-work-due-to-lib-tool-cli-refactor.md
  - jsui/src/main/scala/dev/bosatsu/jsui/Store.scala
  - jsui/src/test/scala/dev/bosatsu/jsui/StoreTest.scala
depends_on: []
estimated_size: M
generated_at: 2026-02-28T23:17:12Z
---

# Design Doc: Issue #1859 jsUI broken after lib/tool CLI refactor

_Issue: #1859 (https://github.com/johnynek/bosatsu/issues/1859)_

## Summary

Plan to fix jsUI command wiring by routing `eval/test/show` through `tool`, add regression tests that reproduce the current failure and validate the fix, and rely on existing `jsuiJS/test` CI coverage so web UI regressions are caught before publish.

---
issue: 1859
title: web UI doesn't work due to lib/tool CLI refactor
status: proposed
base_branch: main
touch_paths:
  - docs/design/1859-web-ui-doesn-t-work-due-to-lib-tool-cli-refactor.md
  - jsui/src/main/scala/dev/bosatsu/jsui/Store.scala
  - jsui/src/test/scala/dev/bosatsu/jsui/StoreTest.scala
depends_on: []
estimated_size: S
generated_at: 2026-02-28
---

# Issue #1859 Design: Fix jsUI command routing after CLI refactor

Issue: #1859 (https://github.com/johnynek/bosatsu/issues/1859)
Base branch: `main`

## Summary

The jsUI currently invokes `MemoryMain` with root commands (`eval`, `test`, `show`), but the CLI was refactored so these now live under `tool`. That mismatch causes parse/help failures in the browser UI. The fix is to update jsUI command args to `tool <subcommand> ...` and add regression tests in `jsui` that both reproduce the old failure and verify the fixed path stays green in CI.

## Problem statement

Current behavior:

1. `jsui/Store.cmdHandler` builds args like `List("test", "--input", ... )`.
2. `MainModule` top-level commands are now `lib`, `tool`, `version`, and `c-runtime`.
3. `MemoryMain.runWith` returns help parse errors for the old root commands.
4. jsUI shows an error instead of evaluation/test/show results.

Observed error shape (from issue):

1. `Unexpected argument: test`
2. Help text showing top-level subcommands (`lib`, `tool`, `version`, `c-runtime`).

## Goals

1. Restore jsUI evaluate/test/show functionality after the CLI namespace change.
2. Add automated tests that capture this regression in `jsuiJS/test`.
3. Make the failure reproducible in tests and verify the fixed path is green in CI.

## Non-goals

1. No CLI parser refactor in `core`.
2. No browser end-to-end framework introduction (Playwright/Cypress/etc.) in this issue.
3. No UI visual/layout changes.

## Current architecture and failure mode

1. `Action.Cmd` values (`Eval`, `Test`, `Show`) map to argv in `Store.cmdHandler`.
2. `Store.run` passes those argv into `MemoryMain.runWith`.
3. `MemoryMain` delegates to `MainModule.command.parse`.
4. Since the argv omits `tool`, parse fails before any compile/eval/test work starts.

This is an adapter drift problem: jsUI hardcodes CLI surface strings and got out of sync with CLI refactor.

## Proposed architecture

### 1) Make jsUI command construction explicit about top-level command family

In `Store.cmdHandler`, route all three actions through `tool`:

1. `Eval`: `tool eval ...`
2. `Test`: `tool test ...`
3. `Show`: `tool show ...`

Implementation detail:

1. Add a tiny helper (or shared prefix constant) so `"tool"` is defined once and reused.
2. Keep command-specific flags and output handlers unchanged.

### 2) Add regression tests in jsui (command adapter layer)

Add `StoreTest.scala` with two kinds of checks:

1. Reproducer check: legacy root argv (`test --input ...`) fails with the expected parse/help error shape.
2. Green-path checks: argv produced by `Store.cmdHandler` for each `Action.Cmd` executes via `Store.memoryMain.runWith` and returns the expected `Output` variant.

Test assertions should focus on command success and `Output` ADT shape, not brittle full-string output snapshots.

### 3) CI path

No new CI job is required:

1. Existing `.github/workflows/ci.yml` already runs `sbt "jsuiJS/test"`.
2. Once the new regression tests are added, this issue is covered before merge/publish.

## Detailed implementation plan

1. Update `jsui/src/main/scala/dev/bosatsu/jsui/Store.scala` to prepend `"tool"` for Eval/Test/Show argv generation.
2. Add `jsui/src/test/scala/dev/bosatsu/jsui/StoreTest.scala`.
3. Add a small in-test Bosatsu source fixture that can be evaluated, shown, and tested.
4. Implement reproduction test using legacy root args and assert parse/help failure mentions unexpected root subcommand.
5. Implement fixed-path tests using `Store.cmdHandler(cmd)._1` and `Store.memoryMain.runWith(...)`, asserting successful `Output` types.
6. Run `sbt "jsuiJS/test"` locally.
7. Merge once CI `testJS` job is green.

## Testing strategy

### Reproduce error (red)

1. In `StoreTest`, run `Store.memoryMain.runWith` using old argv: `List("test", "--input", "root/WebDemo", ... )`.
2. Assert failure includes parse/help signal for unexpected argument `test`.

### Verify fix (green)

1. Use `Store.cmdHandler(Action.Cmd.Eval/Test/Show)` args.
2. Execute with `Store.memoryMain.runWith` against in-memory fixture source.
3. Assert outputs are `Output.EvaluationResult`, `Output.TestOutput`, and `Output.ShowOutput` respectively.
4. Run `sbt "jsuiJS/test"` and confirm pass.

## Acceptance criteria

1. jsUI evaluate no longer fails due to CLI parse/help error.
2. jsUI test no longer fails with `Unexpected argument: test`.
3. jsUI show no longer fails due to missing `tool` prefix.
4. `Store.cmdHandler` emits argv beginning with `tool` for all three commands.
5. New jsui regression tests cover both legacy failure reproduction and fixed-path success.
6. `sbt "jsuiJS/test"` passes locally and in CI.

## Risks and mitigations

1. Risk: Future CLI namespace changes break jsUI again.
   Mitigation: Keep `tool` prefix centralized in one helper/constant and keep adapter regression tests in `jsuiJS/test`.

2. Risk: Test fragility from exact rendered output text.
   Mitigation: Assert on `Output` ADT type and targeted error substrings only.

3. Risk: Scope creep into broader web/API refactors.
   Mitigation: Limit this issue to jsUI command wiring + regression tests.

## Rollout notes

1. Land as one PR containing adapter fix plus jsui regression tests.
2. Validate with `jsuiJS/test` before merge.
3. After merge, monitor the next deploy for successful evaluate/test/show behavior in the web compiler page.
4. Rollback is low risk: revert `Store` arg wiring change if unexpected regressions appear.
