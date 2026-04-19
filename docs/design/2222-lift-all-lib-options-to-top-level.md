---
issue: 2222
priority: 3
touch_paths:
  - docs/design/2222-lift-all-lib-options-to-top-level.md
  - core/src/main/scala/dev/bosatsu/MainModule.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - cli/src/test/scala/dev/bosatsu/GithubWorkflowJsonParityTest.scala
  - .github/workflows/ci.yml
  - scripts/publish_bosatsu_libs.sh
  - docs/src/main/paradox/getting_started.md
  - docs/src/main/paradox/writing_bosatsu_5_minutes.md
  - docs/src/main/paradox/generating_json.md
  - docs/src/main/paradox/debugging_with_eval.md
  - docs/src/main/paradox/debugging_with_show.md
  - docs/src/main/paradox/language_guide.md
  - docs/src/main/paradox/index.md
depends_on: []
estimated_size: M
generated_at: 2026-03-20T21:55:01Z
---

# Lift Library Commands to the Top-Level CLI

_Issue: #2222 (https://github.com/johnynek/bosatsu/issues/2222)_

## Summary

Make library-mode commands canonical at the CLI root, keep `tool` as the explicit file-based/compiler namespace, remove `lib` from the documented/tested user surface, and update help, docs, tests, and automation to use the new top-level commands.

## Context

`MainModule` currently exposes only `lib`, `tool`, `version`, and `c-runtime` at the CLI root. The actual library-mode commands already exist as individual Decline subcommands inside `library.Command`; they are only hidden under the extra `lib` namespace.

In practice, the common Bosatsu workflows are library-mode workflows: `check`, `test`, `build`, `eval`, `json`, `show`, and `doc`. Requiring `bosatsu lib <cmd>` adds ceremony to the path users type most often and keeps those commands off the main `bosatsu --help` surface.

`tool` still has a real role. It is the explicit file-based/compiler namespace for build-tool integrations and for commands that do not belong in library mode, especially `transpile` and `extract-iface`.

## Goals

1. Make the current library workflows available as canonical `bosatsu <cmd>` workflows.
2. Keep `tool` as the explicit low-level/file-based namespace.
3. Remove `lib` usage from in-repo docs, tests, and automation.
4. Improve `--help` discoverability by surfacing common library commands directly at the root.
5. Keep command semantics, flags, and outputs stable.

## Non-goals

1. Do not change the semantics of existing library commands.
2. Do not lift `tool`-only commands such as `transpile` or `extract-iface` to the root.
3. Do not rename existing flags such as `--repo_root`, `--name`, `--main`, or `--outdir`.
4. Do not change callers that intentionally rely on `tool` behavior, such as jsUI or raw file-based flows.

## Current Architecture

`MainModule.opts` is the assembly point for the CLI root. Today it wraps:

- `library.Command.opts(...)` under `lib`
- `tool_command.ToolCommand.opts(...)` under `tool`
- standalone `version` and `c-runtime` commands

That means the library command tree is already factored at the right abstraction level. This issue is mostly a routing and help-surface change, not a rewrite of `check`/`test`/`build` behavior.

One additional detail matters: `MainModule.run` already does pre-parse argument handling for `eval --run ... -- <args>`, and that logic currently recognizes `lib eval` and `tool eval`. Lifting `eval` to the root will require that pre-parse path logic to understand the new command shape.

## Proposed CLI Surface

| Invocation | Behavior |
| --- | --- |
| `bosatsu check`, `test`, `build`, `eval`, `json`, `show`, `doc`, `deps`, `fetch`, `init`, `list`, `assemble`, `publish` | Current library-mode implementation |
| `bosatsu tool <subcommand>` | Current file-based/compiler implementation |

Canonical usage becomes the top-level form. `tool` remains the escape hatch for compiler/file-based behavior. In-repo docs, tests, and scripts should stop using the `lib` spelling entirely.

Top-level help should also be reordered so the most common workflows are first. A reasonable order is:

1. `check`
2. `test`
3. `build`
4. `eval`
5. `json`
6. `show`
7. `doc`
8. `deps`
9. `fetch`
10. `init`
11. `list`
12. `assemble`
13. `publish`
14. `tool`
15. `version`
16. `c-runtime`

The exact tail ordering is less important than ensuring the everyday commands appear before `tool`.

## Architecture and Implementation Plan

### 1. Lift library commands at the parser composition layer

In `MainModule.opts`, compose `library.Command.opts(...)` directly at the CLI root instead of wrapping it in `Opts.subcommand("lib", ...)`.

Keep `tool`, `version`, and `c-runtime` as explicit root subcommands. This preserves the intended separation:

- root commands default to library/build-tool/package-manager mode
- `tool` remains the explicit low-level compiler mode

No library command bodies need to be rewritten. The existing `library.Command` implementations remain the source of truth.

### 2. Remove `lib` from the public CLI surface

`MainModule.opts` should stop exposing `lib` as a first-class root subcommand. The intended public interface after this change is:

- `bosatsu <library-command>` for normal repo/library workflows
- `bosatsu tool <subcommand>` for file-based/compiler workflows

That means the repository should also stop documenting or testing the old nested spelling.

### 3. Update help text and command ordering

`library.Command` should be reordered so the lifted top-level help emphasizes common workflows first. This is the main place where the issue's discoverability goal is realized.

Decline 2.6.1 does preserve subcommand help order based on construction order. I checked the Decline source for this version:

- `Help.collectCommandHelp` uses `collectCommandHelp(f) ++ collectCommandHelp(a)` for both `Opts.App` and `Opts.OrElse`
- `Help.commandList` uses the same left-to-right `++` traversal

That means the displayed subcommand order follows the order in which the `Opts.subcommand(...)` values are combined. In practice, carefully ordering the `MonoidK[Opts].combineAllK(...)` list in `library.Command` and the top-level `orElse` composition in `MainModule` is sufficient; no custom help renderer is needed.

`MainModule` help text should also be updated to make the model explicit:

- top-level commands are the default library-mode experience
- `tool` is the file-based/compiler escape hatch

`tool` help does not need a semantic change, but its description should remain explicit enough that users understand when to use it.

### 4. Keep `eval` passthrough behavior intact

`MainModule.isEvalCommandPath` and the delimiter split logic should be updated so all supported forms continue to work:

- `bosatsu eval --run ... -- <args>`
- `bosatsu tool eval --run ... -- <args>`

This keeps the existing `--` passthrough behavior stable while the command path changes.

### 5. Update canonical docs and automation

The PR should convert user-facing examples from the old nested spelling to `bosatsu <cmd>` wherever library mode is intended.

This includes:

- getting-started docs
- edit-loop docs (`check`/`test`)
- `eval`/`show`/`json` docs
- index page link text
- workflow smoke commands and publish scripts

`tool` examples should remain unchanged where file-based/compiler behavior is intentional, especially `tool transpile`.

### 6. Add focused regression coverage

The current tests already provide broad behavioral coverage for the existing library and tool flows. The new work should update that coverage to the new top-level spelling rather than preserve dual spellings.

Specifically:

- replace the current expectation that root `eval` is a parse failure
- add representative tests showing root `eval`/`json`/`show`/`check`/`test`/`build` resolve to library mode
- rewrite in-repo `lib`-prefixed tests to the top-level commands
- keep explicit tests showing `tool cmd` still means file-based/compiler mode

## Testing Strategy

1. In `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`, add representative root-command tests and rewrite existing library-mode tests to the top-level commands.
2. In `cli/src/test/scala/dev/bosatsu/PathModuleTest.scala`, add filesystem smoke tests for lifted top-level commands and top-level `eval --run` passthrough handling.
3. In `cli/src/test/scala/dev/bosatsu/GithubWorkflowJsonParityTest.scala`, switch the workflow-generation path to canonical top-level `fetch` / `json write` commands.
4. In `.github/workflows/ci.yml`, exercise top-level `fetch`, `test`, and `build` at least once through the shipped wrappers so the canonical surface is covered end-to-end.

## Acceptance Criteria

1. `bosatsu check`, `test`, `build`, `eval`, `json`, `show`, `doc`, `deps`, `fetch`, `init`, `list`, `assemble`, and `publish` invoke the existing library-mode logic.
2. `bosatsu tool <cmd>` continues to invoke the existing file-based/compiler implementation, including `tool transpile` and `tool extract-iface`.
3. `bosatsu --help` surfaces the lifted library commands directly, with common workflows listed before `tool`, and no longer relies on `lib` as the discoverable entrypoint.
4. `eval --run` passthrough handling via `--` works for canonical top-level `eval` and for `tool eval`.
5. In-repo docs, tests, and automation no longer use the old nested spelling.
6. User-facing docs and at least one CI/release automation path use the new canonical top-level syntax.
7. Regression tests cover canonical top-level commands and continued `tool` separation.

## Risks and Mitigations

1. Risk: users and external scripts may still call the old nested command form after the change.
   Mitigation: update all in-repo usage in the same PR and call out the migration explicitly in release notes.

2. Risk: users may confuse root `check`/`test`/`eval` with `tool check`/`tool test`/`tool eval`.
   Mitigation: make the distinction explicit in top-level help and docs, and keep `tool` described as file-based/compiler mode.

3. Risk: docs and automation drift could leave the new syntax under-tested.
   Mitigation: update docs, CI smoke commands, and publish scripts in the same PR.

4. Risk: help ordering could drift if command composition order changes later.
   Mitigation: rely on Decline's construction-order behavior intentionally and keep the command lists in `library.Command` and `MainModule` arranged explicitly.

## Rollout Notes

1. Ship the CLI change together with the in-repo docs/tests/script rewrites so the repository immediately reflects the new top-level surface.
2. Call out the user-facing migration in release notes: `bosatsu lib <cmd>` becomes `bosatsu <cmd>`, while `bosatsu tool <cmd>` remains the raw compiler mode.
3. Because command implementations are being reused rather than rewritten, rollback is straightforward: restore the old root wiring while leaving the command bodies unchanged.

## Alternatives Considered

1. Keep both lifted commands and a visible `lib` subcommand.
   Rejected because it improves typing ergonomics but still leaves the old ceremony in the primary help surface.

2. Keep an undocumented `lib` parser shim indefinitely.
   Rejected because it weakens the migration to the new surface and leaves unnecessary parser complexity in place.

3. Lift both library and tool commands to the root.
   Rejected because overlapping names like `check`, `test`, `eval`, `json`, `show`, `doc`, `deps`, and `assemble` would make the default mode ambiguous and work against the issue's goal of making library mode the obvious default.
