---
issue: 2309
priority: 3
touch_paths:
  - core/src/main/scala/org/typelevel/bosatsu/Declaration.scala
  - core/src/main/scala/org/typelevel/bosatsu/SourceConverter.scala
  - core/src/main/scala/org/typelevel/bosatsu/Parser.scala
  - core/src/test/scala/org/typelevel/bosatsu/SourceConverterTest.scala
  - core/src/test/scala/org/typelevel/bosatsu/ParserTest.scala
  - docs/design/2309-allow-binding-matches-in-if-elif-ternary-conditions-and-lower-directly-to-match.md
depends_on: []
estimated_size: M
generated_at: 2026-04-07T23:30:31Z
---

# Design: Allow binding matches in if/elif/ternary conditions

_Issue: #2309 (https://github.com/johnynek/bosatsu/issues/2309)_

## Summary

This PR proposes allowing pattern-binding `matches` expressions in `if`, `elif`, and ternary conditions. It includes updates to the scope analysis in `Declaration` and a new lowering strategy in `SourceConverter` that translates these constructs directly into efficient `Match` expressions.

## Context

Bosatsu currently supports pattern bindings in standalone `matches` expressions (e.g., `foo matches Some(a)`), but these bindings are only available within the scope of the `matches` expression's optional guard. When `matches` is used as a condition in an `if`, `elif`, or ternary expression, any names introduced by the pattern are currently treated as out of scope for the "true" branch. This forces users to use explicit `match` blocks for even simple conditional bindings, leading to more verbose code and preventing natural composition of conditional logic.

## Goals

- Allow `matches` expressions that bind names to be used as conditions in `if`, `elif`, and ternary expressions.
- Ensure bindings introduced in a condition are correctly scoped to the corresponding "true" branch (and the `matches` guard itself).
- Lower these constructs directly to optimized `Match` expressions in `SourceConverter` to maintain performance and scope integrity.
- Maintain consistency across all conditional forms (block `if`/`elif` and inline ternary).

## Proposed Changes

### 1. Scope Analysis in `Declaration`

The `Declaration` tree and its associated utility methods (like `freeVars`, `allNames`, and scope walkers) must be updated to recognize that a `Matches` condition in an `IfElse` or `Ternary` declaration introduces bindings into the "true" arm.

- For `IfElse(cond, ifTrue, ifFalse)`, if `cond` is a `Matches(expr, pattern, guard)`, the names in `pattern` are in scope for `guard` and `ifTrue`.
- For `Ternary(ifTrue, cond, ifFalse)`, the same scoping rule applies: names in `cond` (if it is a `Matches`) are in scope for `ifTrue` (and the `cond` guard).

### 2. Lowering in `SourceConverter`

`SourceConverter` is responsible for translating `Declaration` (the surface syntax) into `Expr` (the intermediate representation). Currently, `if`/`elif` chains are often lowered to boolean-based `Match` expressions (matching on `True` or `False`). 

We will modify the lowering logic to detect when a condition is a `Matches` expression with bindings:

- **`if cond matches pat: body else: rest`** will lower to:
  ```scala
  Match(cond_expr, List(
    Branch(pat, body),
    Branch(Wildcard, rest_lowered)
  ))
  ```
- **Guarded Matches**: If the condition is `cond matches pat if guard`, it lowers to a `Branch(pat, body, guard)`.
- **Nested `elif`**: Subsequent `elif` arms will be recursively lowered into the `rest_lowered` part of the previous arm's fallback branch.
- **Ternary**: `f(a) if foo matches Some(a) else g` will follow the same logic, lowering to a `Match` on `foo`.

### 3. Parser Enhancements

The parser needs to ensure that `matches` (including guarded forms) is correctly accepted as a condition in ternaries. Precedence rules must be checked to ensure that `f(a) if (foo matches Some(a) if pred(a)) else g` parses intuitively, even without parentheses where possible, though explicit parenthesization will be supported and encouraged for complex guards.

## Acceptance Criteria

- Code like `if x matches Some(a): f(a) else: g` parses and compiles correctly.
- Ternary expressions like `f(a) if x matches Some(a) else g` parse and compile correctly.
- Names bound in the condition are available in the true branch but NOT in the false branch or after the expression.
- The generated `Expr` for these constructs uses a `Match` on the scrutinized expression rather than a `Match` on a boolean result.
- Mixed chains of boolean conditions and binding `matches` conditions lower correctly to nested structures.

## Invariants & Behavioral Properties

1. **Scope Isolation**: Bindings from a condition MUST NOT leak into the `else` branch or the surrounding scope.
2. **Shadowing Consistency**: Bindings in `matches` conditions should follow the same shadowing rules as bindings in explicit `match` branches (e.g., matching the type if shadowing is required by configuration).
3. **Evaluation Order**: The scrutinee of a `matches` condition must be evaluated exactly once.
4. **Guard Precedence**: In `if x matches p if g:`, the guard `g` is evaluated only if `p` matches `x`. Bindings from `p` are available in `g`.

## Test Plan

### Property-Based Tests
- **Round-tripping**: Verify that parsing and then printing these new forms preserves the structure and scoping.
- **Scope Consistency**: Use property tests to ensure that `freeVars` correctly identifies that names bound in the condition are used in the true branch and are not free for the whole expression.

### Case-Based Tests
- **Parser**: Exhaustive tests for ternary combinations, nested `if`/`elif` with varying binding patterns, and parenthesized conditions.
- **SourceConverter**: Unit tests verifying the resulting `Expr` structure (checking for `Match` instead of boolean `If`).
- **Negative Tests**: Ensure that using a bound name in the `else` branch or after the `if` block results in a compilation error.
- **End-to-End**: Evaluation tests in `test_cli.sh` or similar, ensuring correct runtime behavior for true/false match results and guard passes/fails.

## Risks & Rollout

- **Risk**: Breaking existing parser precedence if `matches` + ternary interacts poorly with other operators.
- **Mitigation**: Extensive regression testing against existing `test_workspace` files and new precedence-focused parser tests.
- **Rollout**: This is a pure syntax extension. It should be enabled by default as it does not break backward compatibility (code that currently fails to compile will now succeed).
