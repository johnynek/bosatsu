---
issue: 2118
priority: 3
touch_paths:
  - docs/design/2118-lib-eval-run-should-allow-passing-args-to-the-program.md
  - bosatsu
  - core/src/main/scala/dev/bosatsu/MainModule.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - docs/src/main/paradox/debugging_with_eval.md
depends_on: []
estimated_size: M
generated_at: 2026-03-11T05:25:13Z
---

# Issue #2118 Design: lib eval --run should allow passing args to the program

_Issue: #2118 (https://github.com/johnynek/bosatsu/issues/2118)_

## Summary

Design doc content proposing explicit `--` passthrough arg handling for eval run commands, including shared CLI plumbing, launcher forwarding fixes, tests, acceptance criteria, risks, and rollout notes.

---
issue: 2118
priority: 2
touch_paths:
  - docs/design/2118-lib-eval-run-should-allow-passing-args-to-the-program.md
  - bosatsu
  - core/src/main/scala/dev/bosatsu/MainModule.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala
  - core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - docs/src/main/paradox/debugging_with_eval.md
depends_on: []
estimated_size: M
generated_at: 2026-03-11T00:00:00Z
---

# Issue #2118 Design: lib eval --run should allow passing args to the program

_Issue: #2118 (https://github.com/johnynek/bosatsu/issues/2118)_

Status: proposed
Base branch: `main`

## Summary

Enable program argument passthrough for eval run commands when args start with `-`. The command boundary is an explicit `--` delimiter, and all trailing tokens are forwarded to Bosatsu program execution.

This design adds command-scoped passthrough arg plumbing in shared CLI code and updates the `./bosatsu` launcher so it forwards `--` instead of consuming it.

## Problem statement

Current command shape:

`./bosatsu lib eval --run --main Zafu/Tool/JsonFormat::main --compact`

fails because `--compact` is interpreted as a CLI option, not as a program arg.

There are two relevant parse layers:

1. Decline parses CLI flags before eval logic runs.
2. The `./bosatsu` shell launcher currently consumes `--`, so a delimiter may not reach Scala.

The net effect is that flag-shaped args cannot be reliably passed to `lib eval --run`.

## Goals

1. Allow passing program args that begin with `-` to `lib eval --run`.
2. Keep existing positional arg behavior unchanged.
3. Preserve arg order as seen by the Bosatsu program.
4. Keep existing error behavior for args without `--run`.
5. Keep behavior consistent for `./bosatsu` and direct CLI entrypoints.

## Non-goals

1. Do not auto-forward unknown CLI flags without an explicit delimiter.
2. Do not change Bosatsu runtime arg semantics, including synthetic argv0.
3. Do not add passthrough support to unrelated commands in this issue.

## Proposed architecture

### 1) Delimiter split in shared CLI plumbing

Add pre-parse partitioning in `MainModule.run`:

1. For eval command paths (`lib eval` and `tool eval`), split the argv list at the first `--`.
2. Parse only the prefix with Decline.
3. Pass trailing tokens as command-scoped passthrough args.

If no delimiter is present, passthrough args are empty and current behavior remains.

This follows the issue proposal directly while keeping behavior local to commands that need passthrough args.

### 2) Eval command arg merge

In `library.Command` eval and `tool_command.EvalCommand`:

1. Keep existing positional arg parser (`Opts.arguments[String](arg).orEmpty`).
2. Compute `effectiveRunArgs = positionalArgs ++ passthroughArgs`.
3. Use `effectiveRunArgs` for `PredefImpl.evalRunArgs`.
4. Keep existing guard:
   - if `--run` is false and `effectiveRunArgs.nonEmpty`, raise trailing args error.

This preserves compatibility while adding support for `--`-segmented flag args.

### 3) Launcher forwarding fix

Update `bosatsu` shell wrapper so it does not swallow `--` intended for downstream CLI parsing.

Required effect:

- `./bosatsu lib eval --run --main ... -- --compact` forwards `--` and `--compact` to Scala.
- Existing launcher options (`--fetch`, `--artifact`) still work.

Without this change, Scala-side delimiter handling is not observable for `./bosatsu`.

### 4) Help and docs update

Add one explicit usage example in eval docs or help:

`./bosatsu lib eval --run --main MyLib/Foo -- --compact`

and note that `--` is required for args that start with `-`.

## Detailed implementation plan

1. Add an argv partition helper in `core/src/main/scala/dev/bosatsu/MainModule.scala`.
2. Thread passthrough args into command builder calls for `library.Command` and `tool_command.ToolCommand`.
3. Update `core/src/main/scala/dev/bosatsu/library/Command.scala` eval path to merge positional and passthrough args.
4. Update `core/src/main/scala/dev/bosatsu/tool_command/ToolCommand.scala` and `core/src/main/scala/dev/bosatsu/tool_command/EvalCommand.scala` with the same merge behavior.
5. Update `bosatsu` launcher to forward delimiter tokens.
6. Add regressions in `core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala`:
   - `lib eval --run ... -- --compact` succeeds and forwards arg.
   - mixed args preserve order (`foo -- --bar`).
   - args without `--run` still fail with trailing args error.
   - parity case for `tool eval --run`.
7. Add parser-level delimiter sanity checks in `cli/src/test/scala/dev/bosatsu/PathModuleTest.scala`.
8. Update docs in `docs/src/main/paradox/debugging_with_eval.md`.

## Testing strategy

1. Add an issue-style eval test where Bosatsu `Main` validates received args and returns distinct exit codes for mismatch.
2. Keep existing no-delimiter positional arg tests passing.
3. Add negative test for delimiter args without `--run`.
4. Manual smoke check through `./bosatsu` to verify launcher forwarding.
5. Run targeted suites:
   - `core/testOnly dev.bosatsu.ToolAndLibCommandTest`
   - `cli/testOnly dev.bosatsu.PathModuleTest`

## Acceptance criteria

1. `./bosatsu lib eval --run --main <pkgOrValue> -- --compact` parses successfully.
2. Program receives `--compact` in its runtime arg list.
3. Existing positional args for eval run remain supported.
4. Arg ordering is stable: positional args first, then delimiter-passthrough args.
5. Passing args without `--run` still returns trailing args error.
6. The same delimiter behavior works for `tool eval --run`.
7. `./bosatsu` launcher forwards `--` instead of consuming it.
8. Regression tests cover both success and failure paths.
9. User docs include delimiter usage for flag-shaped args.

## Risks and mitigations

1. Risk: delimiter splitting affects non-eval commands.
   Mitigation: gate splitting by command path (`lib eval`, `tool eval`) only.

2. Risk: launcher changes alter behavior of wrapper-specific flags.
   Mitigation: keep launcher option handling explicit and add smoke coverage for `--fetch` and `--artifact`.

3. Risk: ambiguity when both positional and passthrough args are supplied.
   Mitigation: define and test deterministic merge order.

4. Risk: drift between direct CLI execution and wrapper execution.
   Mitigation: verify both invocation paths before merge.

## Rollout notes

1. Land as one PR with parser plumbing, eval integration, wrapper fix, tests, and docs.
2. Verify behavior with both invocations:
   - `./bosatsu ... -- --flag`
   - direct binary or jar invocation with same args.
3. No migration or runtime data changes are required.
4. If regressions appear, passthrough partitioning can be disabled while keeping existing positional arg behavior.

## Alternatives considered

1. Add repeatable `--arg` flags instead of delimiter passthrough.
   Rejected because it is more verbose and less shell-friendly for existing workflows.

2. Fix only the wrapper script.
   Rejected because command ownership for passthrough args remains implicit and less testable in Scala.

3. Auto-forward unknown flags after `--run` without delimiter.
   Rejected due ambiguity and brittle interaction with normal CLI option parsing.
