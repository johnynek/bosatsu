---
issue: 1923
priority: 3
touch_paths:
  - docs/design/1923-syntax-for-unit-a-functions.md
  - core/src/main/scala/dev/bosatsu/DefStatement.scala
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ParserHintsTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-02T08:52:34Z
---

# Issue #1923 design: syntax for Unit -> a functions

_Issue: #1923 (https://github.com/johnynek/bosatsu/issues/1923)_

## Summary

Design to add zero-arg def and direct-call surface syntax as sugar for `Unit`-argument functions, with parser changes plus SourceConverter lowering, while preserving existing internal non-empty-argument invariants.

---
issue: 1923
title: Syntax for Unit -> a functions
status: proposed
base_branch: main
touch_paths:
  - docs/design/1923-syntax-for-unit-a-functions.md
  - core/src/main/scala/dev/bosatsu/DefStatement.scala
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/Statement.scala
  - core/src/main/scala/dev/bosatsu/SourceConverter.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/dev/bosatsu/ParserHintsTest.scala
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-03-02
---

# Issue #1923 Design: syntax for Unit -> a functions

Issue: #1923 (https://github.com/johnynek/bosatsu/issues/1923)
Base branch: `main`

## Summary

Add zero-arg def and call surface syntax as sugar for Unit-arg functions.

- `def later(): x` is equivalent to `def later(()): x`
- `later()` is equivalent to `later(())` (lowered in SourceConverter)

The implementation keeps Bosatsu core invariants intact: internal function and application representations still carry at least one argument.

## Current state

1. Type syntax `() -> a` already parses as Unit-arg function type.
2. Lambda syntax `() -> expr` already parses as a single Unit pattern argument.
3. `def f(): ...` is currently rejected.
4. Direct call syntax requires at least one argument (`f(x)`); no-arg calls only exist in dot form (`x.await()`).
5. ParserHints currently includes a rule that says zero-arg defs are unsupported.

## Goals

1. Accept `def f(): body` at top-level and nested def sites.
2. Accept no-arg direct call syntax `f()`.
3. Lower both forms to existing Unit-arg internal representation.
4. Preserve existing typing, recursion checking, and lowering architecture.
5. Keep parser diagnostics and pretty-printing coherent.

## Non-goals

1. No new true nullary function representation in `Expr`, `TypedExpr`, or runtime IR.
2. No change to general application semantics beyond Unit sugar.
3. No change to constructor defaulting semantics.
4. No feature flag or staged runtime rollout.

## Architecture

### 1) Def header sugar at parse time

Keep `DefStatement.args` as `NonEmptyList[NonEmptyList[A]]` so recursion and lambda-building invariants stay unchanged.

In `DefStatement.parser`:

1. Add optional support for empty arg groups.
2. Parse groups with zero-or-more entries.
3. Rewrite an empty group to a singleton Unit pattern group (`Pattern.tuple(Nil)`) at pattern-based call sites.

`Declaration.defP` and `Statement.parser1` enable this mode with the Unit pattern. Generic call sites can keep strict non-empty behavior.

### 2) No-arg direct calls in declaration parser

In `Declaration.parser`:

1. Change direct call suffix parsing from one-or-more args to zero-or-more args.
2. Add an explicit `ApplyKind` marker for empty paren calls (for example `Parens0`).
3. Represent `f()` using that marker while still constructing an `Apply` node with non-empty internal args.

`Declaration.toDoc` renders the new call kind as `f()`.

Constructor compatibility decision:

1. Preserve existing constructor ergonomics by treating `Foo()` as constructor value syntax (not `Foo(())`) when callee is syntactically a constructor.
2. This keeps behavior aligned with existing language guide guidance about bare constructor values and zero-arg constructors.

### 3) SourceConverter lowering for empty-paren calls

In `SourceConverter.fromDecl`:

1. Add handling for `ApplyKind.Parens0`.
2. Convert the callee normally.
3. Build application with a single Unit value argument (`Predef.Unit`).

All downstream compiler phases continue to operate on the same non-empty-arg internal forms.

### 4) Pretty-print and parser-hint updates

1. Update `DefStatement.document` to render singleton unit-pattern groups as `()`.
2. Remove the ParserHints rule that says zero-arg defs are unsupported.
3. Update docs to state equivalences:
   - `def f(): body` == `def f(()): body`
   - `f()` == `f(())`
   - `() -> a` is Unit-arg function type

## Implementation plan

1. Extend `DefStatement.parser` with optional empty-group rewriting support.
2. Enable zero-arg def parsing in `Declaration.scala` and `Statement.scala` call sites.
3. Extend `Declaration.ApplyKind` with the empty-parens call marker.
4. Update direct apply suffix parsing in `Declaration.scala` to emit the new marker.
5. Preserve constructor behavior for syntactic constructor no-arg calls.
6. Add SourceConverter lowering from empty-parens calls to Unit-arg application.
7. Remove/update the zero-arg-def ParserHints rule.
8. Add parser and source-converter tests for new syntax.
9. Update language guide examples and equivalence notes.

## Testing strategy

### Parser tests

1. `main = f()` parses.
2. `main = f()()` parses with correct associativity.
3. `def usage(): 1` parses at top-level.
4. Nested zero-arg defs parse.
5. Round-trip rendering uses `def f():` and `f()` forms.
6. Existing `() -> 1` lambda parsing remains green.
7. Constructor regressions are covered (`Foo` and `Foo()` behavior per decision).

### SourceConverter tests

1. `main = later()` desugars identically to `main = later(())`.
2. `def later(): ...` desugars identically to explicit Unit-arg def.
3. Existing no-arg dot call behavior (`x.await()`) is unchanged.

### ParserHints tests

1. Remove expectation for unsupported zero-arg-def hint.
2. Ensure no contradictory hint remains for this syntax path.

## Acceptance criteria

1. `def f(): body` is accepted by both top-level and nested def parsers.
2. `f()` is accepted as direct call syntax.
3. Source conversion lowers `f()` to Unit-argument application.
4. Core `Expr` and `TypedExpr` non-empty-arg invariants are unchanged.
5. `() -> a` and `() -> expr` continue to parse/typecheck as Unit-arg forms.
6. ParserHints no longer reports zero-arg defs as unsupported.
7. Existing dot-call no-arg behavior remains unchanged.
8. Constructor syntax behavior is preserved per architecture decision.
9. New parser and source-converter tests pass.
10. Language guide includes the new syntax and equivalence notes.

## Risks and mitigations

1. Risk: parser precedence regressions after allowing empty direct-call suffixes.
   Mitigation: add targeted tests for chaining, operators, dot calls, and parentheses.
2. Risk: confusion between explicit `f(())` and sugar `f()`.
   Mitigation: document equivalence and keep canonical pretty-printer behavior.
3. Risk: constructor-call ambiguity (`Foo()`).
   Mitigation: explicit parser decision plus constructor regression tests.
4. Risk: synthetic Unit argument may reduce error-region precision.
   Mitigation: keep original call-site tags and verify message quality in tests.
5. Risk: formatting diffs from new canonical rendering (`def f(()):` to `def f():`).
   Mitigation: update golden expectations in same PR and note normalization in rollout.

## Rollout notes

1. Ship parser, source-converter, tests, and docs in one PR.
2. No migration or feature flag is required.
3. Run focused suites first:
   - `coreJVM/testOnly dev.bosatsu.ParserTest`
   - `coreJVM/testOnly dev.bosatsu.SourceConverterTest`
   - `coreJVM/testOnly dev.bosatsu.ParserHintsTest`
4. Monitor CI for parser and formatting regressions immediately after merge.
