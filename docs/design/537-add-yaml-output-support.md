---
issue: 537
priority: 3
touch_paths:
  - docs/design/537-add-yaml-output-support.md
  - core/src/main/scala/dev/bosatsu/Json.scala
  - core/src/main/scala/dev/bosatsu/tool_command/JsonCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - cli/src/test/scala/dev/bosatsu/JsonTest.scala
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - docs/src/main/paradox/generating_json.md
depends_on: []
estimated_size: M
generated_at: 2026-02-25T16:15:38Z
---

# Design: Add YAML Output Support for JSON Commands

_Issue: #537 (https://github.com/johnynek/bosatsu/issues/537)_

## Summary

Adds an opt-in `--yaml` output mode to both `tool json` and `lib json` commands while keeping JSON as the default. The plan introduces `Json.toYamlDoc(j: Json): Doc` on the existing `Json` companion object (no new runtime dependency), uses conservative plain-scalar rules for readable YAML, wires the flag through write/apply/traverse flows, and validates behavior with ScalaCheck YAML-vs-JSON semantic parity tests plus command-level regression tests.

---
issue: 537
title: Add yaml output support
status: proposed
depends_on: []
touch_paths:
  - docs/design/537-add-yaml-output-support.md
  - core/src/main/scala/dev/bosatsu/Json.scala
  - core/src/main/scala/dev/bosatsu/tool_command/JsonCommand.scala
  - core/src/main/scala/dev/bosatsu/library/Command.scala
  - core/src/test/scala/dev/bosatsu/ToolAndLibCommandTest.scala
  - cli/src/test/scala/dev/bosatsu/JsonTest.scala
  - cli/src/test/scala/dev/bosatsu/PathModuleTest.scala
  - docs/src/main/paradox/generating_json.md
---

# Issue #537 Design: Add YAML output support

Issue: #537 (https://github.com/johnynek/bosatsu/issues/537)
Base branch: `main`
Status: proposed

## Summary

Add YAML output support to existing JSON commands by introducing a `--yaml` flag on both `tool json` and `lib json` subcommands. Keep JSON as the default output format. Implement YAML rendering directly on the existing `Json` ADT via companion-object API (`Json.toYamlDoc(j: Json): Doc`) without adding new runtime dependencies.

This follows the issue direction:
1. `--yaml` is available on JSON commands in both command families.
2. No new production dependency is introduced for YAML rendering.
3. Rendering aims to be human-friendly, including unquoted plain strings when safe.
4. Property-based testing validates semantic parity between JSON rendering and YAML rendering.

## Problem Statement

Bosatsu currently emits JSON for `write`, `apply`, and `traverse` outputs. JSON is valid YAML, but the current output is JSON-formatted text and not optimized for YAML readability. Users generating configuration files (for example workflow files) want a direct YAML rendering path without changing the data model or introducing a separate YAML value type.

## Goals

1. Add an opt-in YAML output format to both `tool json` and `lib json` command families.
2. Preserve existing default behavior when `--yaml` is not present.
3. Keep output semantics identical to JSON output (format change only, not data change).
4. Implement rendering in core code using the existing `Json` ADT.
5. Avoid adding runtime YAML dependencies to cross-platform compiler modules.
6. Provide property-based coverage that YAML and JSON renderings parse to the same data.

## Non-goals

1. Add YAML input parsing flags (`--yaml_input`) in this issue.
2. Add a new top-level `yaml` command family.
3. Support YAML-only data model features (anchors, tags, comments, multi-doc streams).
4. Redesign `Output` ADT across the tool for generalized format negotiation.

## Proposed Architecture

### 1. Extend `Json` with YAML rendering

Add YAML rendering entry points in `core/src/main/scala/dev/bosatsu/Json.scala`:
1. `def toYamlDoc(j: Json): Doc` on `object Json`
2. Optional helper `def renderYaml(j: Json): String` can remain a thin wrapper around `toYamlDoc` if needed for tests or call-site ergonomics.

The renderer remains pure and dependency-free, using existing `paiges` `Doc` composition.

### 2. Human-readable scalar policy

Use a conservative plain-scalar rule for strings:
1. Render unquoted only when all conditions are true:
2. The string is non-empty.
3. The string contains no whitespace.
4. The string is not parseable as any JSON value via `Json.parserFile`.
5. The lowercase form is not one of `yes`, `no`, `on`, or `off` (case-insensitive guard for YAML boolean-like scalars).
6. The string characters are in a safe plain-scalar subset to avoid YAML syntax ambiguity.

Render quoted (JSON-style double-quoted escaping) otherwise.

This satisfies the issue guidance that simple non-ambiguous strings should be unquoted while values that could be mistaken for booleans/numbers/null/other JSON forms stay quoted.

### 3. Container formatting

Render YAML in block style by default:
1. Arrays become `-` lists with indentation.
2. Objects become `key: value` mappings with indentation.
3. Nested arrays/objects render on subsequent indented lines.
4. Empty arrays and objects remain `[]` and `{}` for concise output.

This keeps output readable and stable while remaining easy to validate via parser parity.

### 4. Command wiring (`tool json` and `lib json`)

Add a `--yaml` boolean option to JSON command modes in:
1. `core/src/main/scala/dev/bosatsu/tool_command/JsonCommand.scala`
2. `core/src/main/scala/dev/bosatsu/library/Command.scala`

Behavior:
1. Default path remains unchanged and returns `Output.JsonOutput`.
2. With `--yaml`, the same computed `Json` value is emitted as `Output.Basic(Json.toYamlDoc(json), outputPath)`.
3. Applies uniformly to `write`, `apply`, and `traverse`.
4. `--yaml` only changes output rendering; command input remains JSON and CLI help text for `--json_input` / `--json_string` should state that clearly.

Rationale:
1. No `Output` ADT migration is needed.
2. Existing JSON consumers remain untouched.
3. YAML output is opt-in and isolated to these commands.

### 5. Documentation updates

Update `docs/src/main/paradox/generating_json.md`:
1. Document `--yaml` as an output-format flag on `lib json write`.
2. Clarify default JSON output remains unchanged.
3. Add one short YAML example.

## Implementation Plan

1. Implement `Json.toYamlDoc` and `Json.renderYaml` in `Json.scala`.
2. Add plain-scalar safety helpers in `Json.scala` and keep them intentionally conservative.
3. Add `--yaml` option parsing in `tool_command/JsonCommand.scala`, update help text to clarify input is JSON-only, and branch output emission accordingly.
4. Add `--yaml` option parsing in `library/Command.scala`, update help text to clarify input is JSON-only, and branch output emission accordingly.
5. Add/extend command tests in `ToolAndLibCommandTest.scala` for `--yaml` write/apply/traverse behavior and default compatibility.
6. Add CLI integration coverage in `PathModuleTest.scala` for at least one `tool json write --yaml` path.
7. Extend `cli/src/test/scala/dev/bosatsu/JsonTest.scala` with ScalaCheck YAML-vs-JSON semantic parity using existing Jackson YAML test parser.
8. Update user docs in `generating_json.md`.

## Testing Strategy

### Property-based semantic parity

In `cli/src/test/scala/dev/bosatsu/JsonTest.scala`:
1. Generate `Json` values using existing `GenJson`.
2. Parse `json.render` with JSON parser (`ObjectMapper`).
3. Parse `Json.toYamlDoc(json).render(...)` (or `Json.renderYaml(json)`) with YAML parser (`YAMLFactory`).
4. Assert parsed trees are equal for all generated cases.

### Deterministic renderer tests

Add specific assertions for string quoting behavior:
1. Safe identifier-like strings are unquoted.
2. Strings like `true`, `false`, `null`, numeric text, and JSON-shaped literals remain quoted.
3. Strings with whitespace remain quoted.
4. Dangerous YAML boolean-like strings `yes`, `no`, `on`, `off` (including mixed/upper case variants) remain quoted.

### Command-level tests

1. `tool json write --yaml` emits `Output.Basic` and YAML text.
2. `tool json apply --yaml` and `traverse --yaml` emit YAML-formatted output.
3. Equivalent tests for `lib json` subcommands.
4. Existing JSON tests continue to pass unchanged when `--yaml` is absent.

## Acceptance Criteria

1. `tool json write|apply|traverse` accepts `--yaml`.
2. `lib json write|apply|traverse` accepts `--yaml`.
3. Without `--yaml`, output behavior and test expectations remain unchanged.
4. With `--yaml`, outputs are valid YAML for all JSON values generated by existing encoders.
5. YAML output is produced by new `Json.toYamlDoc(j: Json)` logic in core, not by a runtime YAML dependency.
6. Safe non-ambiguous strings may be unquoted; ambiguous strings remain quoted.
7. Case-insensitive `yes` / `no` / `on` / `off` string values are always quoted in YAML output.
8. ScalaCheck parity test proves YAML parse tree equals JSON parse tree across generated `Json` values.
9. Command tests cover both tool and lib paths with `--yaml`.
10. CLI help explicitly states that apply/traverse input flags remain JSON-only.
11. User docs mention `--yaml` and default JSON behavior.
12. No new non-test YAML dependency is introduced.

## Risks and Mitigations

1. Risk: YAML edge cases in plain-scalar rendering create parse differences.
Mitigation: keep plain-scalar detection conservative and enforce property-based parser parity.

2. Risk: Readability regressions from over-quoting.
Mitigation: start conservative for correctness, then relax rules only with additional tests.

3. Risk: Behavior drift in existing JSON command output.
Mitigation: keep JSON as default path and preserve existing test suite unchanged.

4. Risk: Duplication between `tool json` and `lib json` option wiring.
Mitigation: mirror behavior exactly in both modules and cover both with tests.

## Rollout Notes

1. Land as an opt-in feature (`--yaml`) with JSON default unchanged.
2. Release note should state scope clearly: output-format change only, no YAML input support added.
3. If regressions appear, users can immediately fall back to default JSON output by removing `--yaml`.
4. Future follow-up can add a generalized `--format json|yaml` option once this path stabilizes.
