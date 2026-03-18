---
issue: 2194
priority: 3
touch_paths:
  - docs/design/2194-supporting-raw-foo-in-patterns-and-interpolation.md
  - core/src/main/scala/dev/bosatsu/StringUtil.scala
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Pattern.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/PatternTest.scala
  - core/src/test/scala/dev/bosatsu/ParserHintsTest.scala
  - docs/src/main/paradox/language_guide.md
  - syntax/vscode/bosatsu/syntaxes/bosatsu.tmLanguage.json
depends_on: []
estimated_size: M
generated_at: 2026-03-18T02:09:59Z
---

# Support Raw `$foo` In Patterns And Interpolation

_Issue: #2194 (https://github.com/johnynek/bosatsu/issues/2194)_

## Summary

Add parser-only support for raw `$name` in string interpolation and string patterns when the suffix is exactly one bindable, keep braced forms as the canonical emitted syntax, and reject ambiguous larger parses such as `$foo(bar)`.

## Context
Bosatsu currently requires braced forms for string interpolation and string-pattern bindings:

1. `Declaration.stringDeclOrLit` parses `${...}` into `StringDecl.StrExpr` and `$.{...}` into `StringDecl.CharExpr`.
2. `Pattern.plit` parses `${name}` into `Pattern.StrPart.NamedStr` and `$.{name}` into `Pattern.StrPart.NamedChar`.
3. Both code paths share `StringUtil.interpolatedString`, which only knows about the braced starts.
4. Downstream phases already operate on the AST nodes above, so they do not distinguish whether a source fragment came from `${foo}` or a hypothetical raw `$foo`.

That architecture means issue #2194 is primarily a parser and user-surface change. Type inference, pattern lowering, evaluation, and code generation should not need new IR or runtime behavior.

## Goals
1. Accept raw `$foo` anywhere `${foo}` is currently valid for string-substring interpolation or string-pattern binding.
2. Keep the parser conservative: if the text after `$` parses as more than a single bindable, fail the parse instead of treating it as `${foo}` followed by trailing literal text.
3. Preserve all existing `${...}` and `$.{...}` forms.
4. Reuse the existing AST so downstream semantics remain unchanged.

## Non-goals
1. No raw char shorthand such as `$.x`; char interpolation and char pattern binds stay `$.{x}`.
2. No raw wildcard shorthand such as `$_`; wildcard string patterns stay `${_}`.
3. No printer/source-rendering change; canonical output may remain braced.
4. No typechecker, matcher, or runtime changes.

## Current Architecture
1. `StringUtil.interpolatedString` alternates between literal chunks and interpolation starts. Literal scanning stops only on the closing quote, `${`, or `$.{`.
2. `Declaration.stringDeclOrLit` passes the non-binding parser into that helper, so braced interpolation can contain any existing expression syntax.
3. `Pattern.plit` passes a smaller parser that accepts either `_` or a bindable, so braced string patterns can bind substrings or chars.
4. Printers in `Declaration.toDoc` and `Pattern.StrPart.document` already render the canonical braced syntax.

Because both declaration strings and pattern strings already funnel through the same low-level scanner, the cleanest change is to extend the scanner once and keep the AST constructors unchanged.

## Proposed Design
### 1. Add raw `$` as a third interpolation entry point
Extend the shared string-scanning logic in `StringUtil.scala` so literal scanning can also stop on `$` when the following characters could begin a raw bindable interpolation.

Required behavior:

1. `${...}` and `$.{...}` continue to work exactly as today.
2. `$` remains literal when the following characters cannot begin interpolation. This preserves cases such as `$5`.
3. Once the parser recognizes a raw-start candidate, it should commit to interpolation parsing. That ensures malformed raw forms become parse failures instead of silently falling back to literal text.

This keeps the interpolation boundary decision in one place rather than duplicating dollar-handling in both `Declaration.scala` and `Pattern.scala`.

### 2. Use declaration-prefix parsing to decide whether raw `$...` is legal
The issue requirement is stricter than “parse a bindable and stop”. Raw `$foo` is only valid when the declaration prefix after `$` is exactly one bindable.

Concretely:

1. `'$foo'` parses the same as `'${foo}'`.
2. `'$foo(bar)'` must be a parse failure, not `'${foo}(bar)'`.
3. `'$foo.bar'` must be a parse failure.
4. `'$foo: T'` must be a parse failure.

The preferred implementation is:

1. After raw `$` is recognized in declaration-string context, run the existing non-binding parser on the remaining source as a prefix parse.
2. Accept the raw form only when that prefix parse yields `Declaration.Var(b: Bindable)`.
3. If the prefix parse yields any other declaration shape, fail with a targeted raw-interpolation error and require braces.

This is more robust than maintaining a manual list of “forbidden continuation” tokens. If Bosatsu later gains new postfix or annotation forms, the raw `$...` rule automatically stays conservative.

### 3. Mirror the same rule for string patterns
In pattern strings, raw `$foo` should map to the same AST node as `${foo}`: `Pattern.StrPart.NamedStr(foo)`.

The same conservatism should apply:

1. Raw shorthand is only for substring binders.
2. Raw forms that would grow into a larger declaration-like fragment should fail.
3. Raw wildcard and raw char shorthand remain out of scope.

This keeps behavior aligned across the two contexts named in the issue and avoids a situation where `'$foo'` is legal in expressions but not in patterns, or vice versa.

### 4. Keep AST and pretty-printing unchanged
No new AST nodes are needed.

1. Raw `$foo` and braced `${foo}` should produce the same parsed tree.
2. Existing type inference and pattern lowering continue to operate on `StringDecl.StrExpr` and `Pattern.StrPart.NamedStr`.
3. `Declaration.toDoc` and `Pattern.StrPart.document` can continue to emit braced syntax.

Keeping printing canonical has two benefits:

1. The new feature stays parse-only and does not force formatting churn.
2. There is still one unambiguous emitted form for snapshots, docs generated from AST, and round-trip tests.

### 5. Update diagnostics and user-facing syntax docs
Introducing raw `$foo` changes the meaning of some strings that are currently literal. The PR should therefore update user guidance in parallel with the parser change.

1. Parser hints should continue to explain escaping for literal interpolation starts.
2. Docs should explain the split between raw `$foo` and braced `${expr}`.
3. Docs should call out that literal `$foo` now needs escaping as `\$foo` when interpolation is not intended.
4. VS Code TextMate highlighting should recognize raw `$name` so editor feedback matches the parser.

## Implementation Plan
1. In `core/src/main/scala/dev/bosatsu/StringUtil.scala`, add a raw-dollar-aware string scanner or overload of `interpolatedString` that can:
   - detect raw-start lookahead,
   - distinguish literal `$` from committed raw interpolation,
   - keep returning the existing literal/interpolation part structure.
2. In `core/src/main/scala/dev/bosatsu/Declaration.scala`, add a raw interpolation parser for `stringDeclOrLit`.
   - Reuse the existing non-binding parser as a prefix parser.
   - Accept only `Var(bindable)`.
   - Fail for any larger declaration shape and direct users to `${...}`.
3. In `core/src/main/scala/dev/bosatsu/Pattern.scala`, add the matching raw parser for `plit`.
   - Map raw `$foo` to `Pattern.StrPart.NamedStr(foo)`.
   - Keep `$.{...}` and `${_}` unchanged.
4. In `core/src/main/scala/dev/bosatsu/ParserHints.scala`, refresh interpolation hint text so the escape guidance remains accurate after raw `$foo` lands.
5. In tests and docs, add positive/negative cases and update the language guide and editor syntax rules.

## Testing Strategy
1. Add declaration-string parser coverage in `core/src/test/scala/dev/bosatsu/ParserTest.scala`.
   - `'$x'` parses the same as `'${x}'`.
   - `'pre $x post'` parses the same as `'pre ${x} post'`.
   - `'\$x'` remains literal text.
2. Add pattern coverage in `core/src/test/scala/dev/bosatsu/PatternTest.scala`.
   - `'$x'` in string-pattern context parses the same as `'${x}'`.
   - Existing `${_}` and `$.{c}` behavior is unchanged.
3. Add negative parser cases.
   - `'$foo(bar)'` fails.
   - `'$foo.bar'` fails.
   - `'$foo: T'` fails.
   - raw `$_` and raw char shorthand remain unsupported.
4. Add canonical-rendering checks where useful.
   - Parsing raw syntax and rendering the AST should produce the existing braced form.
5. Update parser-hint coverage in `core/src/test/scala/dev/bosatsu/ParserHintsTest.scala` so literal-dollar guidance still matches the accepted syntax.

## Acceptance Criteria
1. Raw `$name` is accepted in declaration-string interpolation and string-pattern binding when `name` is a single bindable.
2. Raw `$name` produces the same AST as braced `${name}`.
3. Existing `${...}` and `$.{...}` syntax remains supported unchanged.
4. Raw forms are rejected when the suffix parses as more than a single bindable; at minimum `'$foo(bar)'`, `'$foo.bar'`, and `'$foo: T'` fail to parse.
5. No raw shorthand is added for char interpolation/patterns or wildcard string patterns.
6. Pretty-printing remains canonical and does not require emitting raw `$foo`.
7. Tests cover successful parses, failing parses, escaping, and parser-hint/doc updates.
8. User docs explain when to use `$foo`, when `${...}` is still required, and how to write literal `$foo`.

## Risks And Mitigations
1. Risk: previously literal strings containing `$name` change meaning.
Mitigation: document the change explicitly, keep escaping guidance prominent, and add parser-hint coverage.

2. Risk: naive raw support increases parser ambiguity or backtracking.
Mitigation: commit only after a raw-start candidate is recognized and keep the raw-dollar decision in one shared helper.

3. Risk: declaration strings and pattern strings drift into different raw-dollar rules.
Mitigation: drive both through the same scanner contract and add paired positive/negative tests in both contexts.

4. Risk: editor highlighting lags behind parser behavior.
Mitigation: update the VS Code TextMate grammar in the same PR; if tree-sitter-specific highlighting needs more work, treat that as follow-up rather than blocking the language parser change.

## Rollout Notes
1. This is a front-end syntax change only; there is no runtime, backend, or serialized-format migration.
2. Land parser, tests, docs, and syntax-highlighting updates together so the syntax is coherent at merge time.
3. Release notes should call out the one compatibility hazard: literal `$name` in strings must now be written as `\$name` when interpolation is not intended.
