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

Add parser-only support for a regular raw-dollar family in strings: `$name` for string interpolation/binding, `$.name` for char interpolation/binding, `$_` and `$._` for pattern wildcards, and `$$` for a literal dollar, while still rejecting ambiguous larger parses such as `$foo(bar)`.

## Context
Bosatsu currently requires braced forms for string interpolation and string-pattern bindings:

1. `Declaration.stringDeclOrLit` parses `${...}` into `StringDecl.StrExpr` and `$.{...}` into `StringDecl.CharExpr`.
2. `Pattern.plit` parses `${name}` into `Pattern.StrPart.NamedStr` and `$.{name}` into `Pattern.StrPart.NamedChar`.
3. Both code paths share `StringUtil.interpolatedString`, which only knows about the braced starts.
4. Downstream phases already operate on the AST nodes above, so they do not distinguish whether a source fragment came from `${foo}` or a hypothetical raw `$foo`.

That architecture means issue #2194 is primarily a parser and user-surface change. Type inference, pattern lowering, evaluation, and code generation should not need new IR or runtime behavior.

## Goals
1. Accept raw `$foo` anywhere `${foo}` is currently valid for string-substring interpolation or string-pattern binding.
2. Accept raw `$.foo` anywhere `$.{foo}` is currently valid for char interpolation or char-pattern binding.
3. Accept raw `$_` and `$._` in string-pattern contexts as the regular wildcard spellings corresponding to `${_}` and `$.{_}`.
4. Treat `$$` as a literal `$` so users have a regular, local way to escape interpolation starts inside strings.
5. Keep the parser conservative: if the text after `$` or `$.` parses as more than a single bindable, fail the parse instead of treating it as a smaller interpolation followed by trailing literal text.
6. Preserve all existing `${...}` and `$.{...}` forms.
7. Reuse the existing AST so downstream semantics remain unchanged.

## Non-goals
1. No change to the meaning of braced interpolation; `${...}` and `$.{...}` remain fully supported.
2. No printer/source-rendering change; canonical output may remain braced.
3. No typechecker, matcher, or runtime changes.

## Current Architecture
1. `StringUtil.interpolatedString` alternates between literal chunks and interpolation starts. Literal scanning stops only on the closing quote, `${`, or `$.{`.
2. `Declaration.stringDeclOrLit` passes the non-binding parser into that helper, so braced interpolation can contain any existing expression syntax.
3. `Pattern.plit` passes a smaller parser that accepts either `_` or a bindable, so braced string patterns can bind substrings or chars.
4. Printers in `Declaration.toDoc` and `Pattern.StrPart.document` already render the canonical braced syntax.

Because both declaration strings and pattern strings already funnel through the same low-level scanner, the cleanest change is to extend the scanner once and keep the AST constructors unchanged.

## Proposed Design
### 1. Add a regular raw-dollar family to string parsing
Extend the shared string-scanning logic in `StringUtil.scala` so literal scanning can stop on three regular dollar forms:

1. `$$` for a literal `$`.
2. `$...` for string interpolation/binding.
3. `$.`-style starts, concretely `$.foo` and `$._`, for char interpolation/binding.

Required behavior:

1. `${...}` and `$.{...}` continue to work exactly as today.
2. `$$` always contributes a literal `$` to the string/pattern body and never starts interpolation.
3. `$` remains literal only when the following characters cannot begin any recognized raw form. This preserves cases such as `$5`.
4. Once the parser recognizes a raw-start candidate, it should commit to interpolation parsing. That ensures malformed raw forms become parse failures instead of silently falling back to literal text.

This keeps the interpolation boundary decision in one place rather than duplicating dollar-handling in both `Declaration.scala` and `Pattern.scala`.

### 2. Use declaration-prefix parsing to decide whether raw `$...` is legal
The issue requirement is stricter than “parse a bindable and stop”. Raw `$foo` and `$.foo` are only valid when the declaration prefix after the raw introducer is exactly one bindable.

Concretely:

1. `'$foo'` parses the same as `'${foo}'`.
2. `'$.ch'` parses the same as `'$.{ch}'`.
3. `'$foo(bar)'` must be a parse failure, not `'${foo}(bar)'`.
4. `'$.foo(bar)'` must be a parse failure, not `'$.{foo}(bar)'`.
5. `'$foo.bar'` must be a parse failure.
6. `'$foo: T'` must be a parse failure.

The preferred implementation is:

1. After raw `$` or `$.` is recognized in declaration-string context, run the existing non-binding parser on the remaining source as a prefix parse.
2. Accept the raw form only when that prefix parse yields `Declaration.Var(b: Bindable)`.
3. Route `$name` to `StringDecl.StrExpr` and `$.name` to `StringDecl.CharExpr`.
4. If the prefix parse yields any other declaration shape, fail with a short targeted hint:
   - use `${...}` or `$.{...}` for larger expressions,
   - use `$$` for a literal dollar.

This is more robust than maintaining a manual list of “forbidden continuation” tokens. If Bosatsu later gains new postfix or annotation forms, the raw `$...` rule automatically stays conservative.

### 3. Mirror the same rule for string patterns
In pattern strings, the raw family should stay regular with the declaration-string syntax:

1. `$foo` maps to the same AST node as `${foo}`: `Pattern.StrPart.NamedStr(foo)`.
2. `$.foo` maps to the same AST node as `$.{foo}`: `Pattern.StrPart.NamedChar(foo)`.
3. `$_` maps to the same AST node as `${_}`.
4. `$._` maps to the same AST node as `$.{_}`.
5. Raw forms that would grow into a larger declaration-like fragment still fail.

This keeps behavior aligned across the two contexts named in the issue and avoids a situation where `'$foo'` is legal in expressions but not in patterns, or vice versa.

### 4. Keep AST and pretty-printing unchanged
No new AST nodes are needed.

1. Raw `$foo` and braced `${foo}` should produce the same parsed tree.
2. Raw `$.foo` and braced `$.{foo}` should also produce the same parsed tree.
3. Existing type inference and pattern lowering continue to operate on `StringDecl.StrExpr`, `StringDecl.CharExpr`, `Pattern.StrPart.NamedStr`, and `Pattern.StrPart.NamedChar`.
4. `Declaration.toDoc` and `Pattern.StrPart.document` can continue to emit braced syntax.

Keeping printing canonical has two benefits:

1. The new feature stays parse-only and does not force formatting churn.
2. There is still one unambiguous emitted form for snapshots, docs generated from AST, and round-trip tests.

### 5. Update diagnostics and user-facing syntax docs
Introducing raw `$foo`, `$.foo`, and `$$` changes the meaning of some strings that are currently literal. The PR should therefore update user guidance in parallel with the parser change.

1. Parser hints should teach the regular rules in one short blurb:
   - `$foo` and `$.foo` are the short forms,
   - `${...}` and `$.{...}` are required for larger expressions,
   - `$$` is the literal-dollar form.
2. Docs should explain the split between raw `$foo`/`$.foo` and braced `${expr}`/`$.{expr}`.
3. Docs should call out that `$_` and `$._` are the short wildcard spellings in patterns.
4. VS Code TextMate highlighting should recognize raw `$name`, `$.name`, and `$$` so editor feedback matches the parser.

## Implementation Plan
1. In `core/src/main/scala/dev/bosatsu/StringUtil.scala`, add a raw-dollar-aware string scanner or overload of `interpolatedString` that can:
   - detect raw-start lookahead,
   - distinguish literal `$` from committed raw interpolation,
   - keep returning the existing literal/interpolation part structure.
2. In `core/src/main/scala/dev/bosatsu/Declaration.scala`, add raw interpolation parsers for `stringDeclOrLit`.
   - Reuse the existing non-binding parser as a prefix parser.
   - Accept only `Var(bindable)`.
   - Route `$bindable` to string interpolation and `$.bindable` to char interpolation.
   - Treat `$$` as a literal-dollar token before interpolation parsing.
   - Fail for any larger declaration shape and direct users to `${...}` / `$.{...}`.
3. In `core/src/main/scala/dev/bosatsu/Pattern.scala`, add the matching raw parser for `plit`.
   - Map raw `$foo` to `Pattern.StrPart.NamedStr(foo)`.
   - Map raw `$.foo` to `Pattern.StrPart.NamedChar(foo)`.
   - Map raw `$_` and `$._` to the existing wildcard pattern parts.
   - Keep the braced spellings fully supported.
4. In `core/src/main/scala/dev/bosatsu/ParserHints.scala`, refresh interpolation hint text so the raw-dollar guidance remains accurate after raw `$foo`, `$.foo`, and `$$` land.
5. In tests and docs, add positive/negative cases and update the language guide and editor syntax rules.

## Testing Strategy
1. Add declaration-string parser coverage in `core/src/test/scala/dev/bosatsu/ParserTest.scala`.
   - `'$x'` parses the same as `'${x}'`.
   - `'$.c'` parses the same as `'$.{c}'`.
   - `'pre $x post'` parses the same as `'pre ${x} post'`.
   - `'$$x'` is parsed as literal `$x`.
2. Add pattern coverage in `core/src/test/scala/dev/bosatsu/PatternTest.scala`.
   - `'$x'` in string-pattern context parses the same as `'${x}'`.
   - `'$.c'` in string-pattern context parses the same as `'$.{c}'`.
   - `$_` parses the same as `${_}`.
   - `$._` parses the same as `$.{_}`.
   - `$$` is parsed as a literal `$`.
3. Add negative parser cases.
   - `'$foo(bar)'` fails.
   - `'$.foo(bar)'` fails.
   - `'$foo.bar'` fails.
   - `'$foo: T'` fails.
4. Add canonical-rendering checks where useful.
   - Parsing raw syntax and rendering the AST should produce the existing braced form.
5. Update parser-hint coverage in `core/src/test/scala/dev/bosatsu/ParserHintsTest.scala` so ambiguous raw forms teach braces for larger expressions and `$$` for literal dollars.

## Acceptance Criteria
1. Raw `$name` is accepted in declaration-string interpolation and string-pattern binding when `name` is a single bindable.
2. Raw `$.name` is accepted in declaration-string interpolation and string-pattern binding when `name` is a single bindable.
3. `$_` and `$._` are accepted in string-pattern contexts as the short wildcard spellings.
4. `$$` is accepted as a literal `$`.
5. Existing `${...}` and `$.{...}` syntax remains supported unchanged.
6. Raw forms are rejected when the suffix parses as more than a single bindable; at minimum `'$foo(bar)'`, `'$.foo(bar)'`, `'$foo.bar'`, and `'$foo: T'` fail to parse.
7. Pretty-printing remains canonical and does not require emitting raw `$foo` or `$.foo`.
8. Tests cover successful parses, failing parses, literal-dollar handling, and parser-hint/doc updates.
9. User docs explain when to use raw `$foo` / `$.foo`, when braced `${...}` / `$.{...}` is still required, and how to write a literal dollar with `$$`.

## Risks And Mitigations
1. Risk: previously literal strings containing `$name`, `$.name`, or `$$` change meaning.
Mitigation: document the change explicitly, keep the `$$` rule prominent, and add parser-hint coverage.

2. Risk: naive raw support increases parser ambiguity or backtracking.
Mitigation: commit only after a raw-start candidate is recognized and keep the raw-dollar decision in one shared helper.

3. Risk: declaration strings and pattern strings drift into different raw-dollar rules.
Mitigation: drive both through the same scanner contract and add paired positive/negative tests in both contexts.

4. Risk: editor highlighting lags behind parser behavior.
Mitigation: update the VS Code TextMate grammar in the same PR; if tree-sitter-specific highlighting needs more work, treat that as follow-up rather than blocking the language parser change.

## Rollout Notes
1. This is a front-end syntax change only; there is no runtime, backend, or serialized-format migration.
2. Land parser, tests, docs, and syntax-highlighting updates together so the syntax is coherent at merge time.
3. Release notes should call out the compatibility hazards:
   - literal `$name` and `$.name` inside strings may become interpolation,
   - `$$` now means a literal single `$`,
   - braces remain the required spelling for larger expressions.
