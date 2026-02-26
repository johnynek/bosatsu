---
issue: 1172
priority: 3
touch_paths:
  - docs/design/1172-syntax-to-require-recursions-to-be-tail-recursive.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/RecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/SelfCallKind.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/WellTypedGen.scala
  - docs/src/main/paradox/recursion.md
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-02-25T18:03:19Z
---

# Issue #1172 design doc: require tail-recursive recursion syntax

_Issue: #1172 (https://github.com/johnynek/bosatsu/issues/1172)_

## Summary

Adds a contextual `loop` recursion header that preserves existing `recur` termination checks and additionally requires all self-recursive calls to be tail-position, enforced in typed recursion checking with new diagnostics and tests.

---
issue: 1172
priority: 2
touch_paths:
  - docs/design/1172-syntax-to-require-recursions-to-be-tail-recursive.md
  - core/src/main/scala/dev/bosatsu/Declaration.scala
  - core/src/main/scala/dev/bosatsu/ParserHints.scala
  - core/src/main/scala/dev/bosatsu/TypedExprRecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/RecursionCheck.scala
  - core/src/main/scala/dev/bosatsu/SelfCallKind.scala
  - core/src/test/scala/dev/bosatsu/ParserTest.scala
  - core/src/test/scala/dev/bosatsu/TypedExprRecursionCheckTest.scala
  - core/src/test/scala/dev/bosatsu/ErrorMessageTest.scala
  - core/src/test/scala/dev/bosatsu/LegacyDefRecursionCheck.scala
  - core/src/test/scala/dev/bosatsu/WellTypedGen.scala
  - docs/src/main/paradox/recursion.md
  - docs/src/main/paradox/language_guide.md
depends_on: []
estimated_size: M
generated_at: 2026-02-25T00:00:00Z
---

# Design: syntax to require recursions to be tail recursive

Issue: #1172 (https://github.com/johnynek/bosatsu/issues/1172)  
Base branch: `main`  
Status: proposed

## Summary
Add a new recursion header `loop` that behaves like `recur` for termination checking, but additionally requires all recursive self-calls to be in tail position. Compilation fails when a `loop` definition contains any non-tail self-call.

## Problem statement
`recur` proves termination, but it does not guarantee constant-stack execution. Bosatsu already has loop/recur lowering and Matchless tail-call rewriting, but users cannot currently require that optimization. Issue #1172 asks for source syntax equivalent in intent to Scala `@tailrec`: if recursion is not tail recursive, compilation should fail.

## Goals
1. Provide source syntax to opt into mandatory tail-recursion checking.
2. Keep existing `recur` semantics unchanged.
3. Reuse existing recursion legality checks (structural/lexicographic decrease) so guarantees compose.
4. Produce clear compile errors when a `loop` definition has non-tail recursive calls.
5. Avoid broad source breakage for existing code, especially common helper names like `loop`.

## Non-goals
1. Do not force all recursive definitions to be tail recursive.
2. Do not change runtime semantics, proto schema, or backend IR formats.
3. Do not implement mutual-recursion tail checking.
4. Do not redesign recursion lowering passes in this issue.

## Syntax decision
Use a local recursion form:

```bosatsu
def len(lst, acc):
  loop lst:
    case []: acc
    case [_, *tail]: len(tail, acc.add(1))
```

This keeps the guarantee local to the recursive region and aligns with the issue discussion preference for function-local syntax.

### Why this form
1. It is explicit at the recursion site, not hidden at the def header.
2. It composes with existing tuple-target recursion: `loop (a, b): ...`.
3. It avoids adding a new top-level def keyword.

### Compatibility rule
`loop` is a contextual keyword only in recursion-header position (`loop <arg>:`). It is not added to the global identifier keyword set, so existing names like `def loop(...):` remain valid.

## Semantics
1. `match` remains non-recursive.
2. `recur` remains recursive with existing structural/lexicographic termination checks.
3. `loop` is recursive with the same termination checks as `recur`.
4. Additional `loop` requirement: every recursive self-call in the enclosing def must be tail-position.
5. If a `loop` def has any non-tail self-call, compilation fails with a recursion error.
6. If a `loop` block contains no recursive call, existing `recur but no recursive call` behavior remains (with wording updated for `loop` where applicable).

## Architecture

### 1. Declaration AST and parser
Update `Declaration.Match` to use a dedicated match-mode type instead of overloading `RecursionKind` for both `let` recursion and match headers.

Proposed mode model:
1. `Match`
2. `Recur`
3. `Loop`

Changes:
1. Parse `loop` in `Declaration.matchKindParser`.
2. Pretty-print mode back to `match`, `recur`, or `loop`.
3. Keep `Declaration.keywords` unchanged so `loop` remains usable as an identifier outside header context.
4. Update parser-hint header keyword recognition to include `loop` for better missing-colon hints.

#### Parser shape for contextual `loop` (concrete)
`loop` must be accepted as a normal name (for example `def loop(x): ...`) while still being parsed as recursion-header syntax in `loop <target>:` positions. The parser strategy is:
1. Keep the existing `varP` / identifier path unchanged (`loop` is not globally reserved).
2. In `matchP`, extend only the header parser (`matchKindParser`) with `loop`, so contextual interpretation happens only where a recursion header is already expected.
3. After consuming the keyword, commit to parsing a recursion header target and trailing colon without backtracking to plain variable parsing.
4. Restrict backtracking/`soft` usage to the keyword disambiguation boundary, not the whole declaration.

Sketch (intended shape, not exact final code):

```scala
val recurHeaderP: P[MatchMode] =
  kwSpace("match").as(MatchMode.Match)
    .orElse(kwSpace("recur").as(MatchMode.Recur))
    .orElse(kwSpace("loop").as(MatchMode.Loop))

val recurTargetP: Indy[NonBinding] =
  // existing branch-arg parser: single nonbinding or tuple form
  arg

val left: Indy[(MatchMode, NonBinding)] =
  // commit after recognized header keyword so `loop ...` here is not reparsed
  // as a plain variable expression
  Indy.lift(recurHeaderP).cutThen(recurTargetP).cutLeftP(maybeSpace)

// block parser then requires trailing ':' and branch list, as today
```

Operationally, this means:
1. `def loop(x): ...` still parses as a definition name because it goes through identifier parsing, not `matchP`.
2. `loop x:` in recursion-header position is parsed as `MatchMode.Loop`.
3. `loop` as a local value/binder name is also legal, for example:

```bosatsu
loop = x -> fn(x)
loop(12)
```

4. `loop x` without `:` in a context expecting recursion header is a syntax error (with parser hints), not a silent fallback to variable parsing.

### 2. Typed recursion checker integration
Extend `TypedExprRecursionCheck` so `Loop` and `Recur` both enter recursive checking paths. Existing recur-target legality, lexicographic checks, and shadowing checks stay unchanged.

For defs that use `Loop`:
1. Run the existing recursion legality traversal first.
2. If legality succeeds, run self-call classification on the typed body (`SelfCallKind`).
3. Reject when classification is `NonTailCall`.
4. Accept when classification is `TailCall`.
5. Keep existing no-recursive-call rejection behavior.

This approach reuses the current typed recursion boundary and does not add a new compiler phase.

### 3. Error model
Add a new recursion error in `RecursionCheck` for loop-mode tail requirement failures.

Proposed shape:
1. `LoopRequiresTailRecursion(fnname, illegalPosition)`

Proposed message:
`loop requires all recursive calls to <fn> to be in tail position.`

Initial region can be the `loop` header region. If needed, a follow-up can improve precision by teaching `SelfCallKind` to return the first non-tail self-call region.

### 4. Pipeline and ordering
No pipeline reordering is required.

`Package.inferBodyUnopt` remains:
1. Source conversion and existing source checks.
2. Type inference.
3. Typed recursion checking (now including loop tail requirement).
4. Typed loop/recur lowering and normalization.

This preserves the existing trust boundary: recursion legality is validated before lowering.

## Detailed implementation plan
1. In `Declaration.scala`:
   1. Introduce match-mode ADT for `match`/`recur`/`loop`.
   2. Update parser and document rendering.
   3. Keep contextual-keyword behavior for `loop`.
2. In `ParserHints.scala`:
   1. Include `loop` in header keyword heuristics for colon hints.
3. In `TypedExprRecursionCheck.scala`:
   1. Treat `Loop` as recursive in recur-site detection.
   2. Track whether current recursive def is in `Loop` mode.
   3. After existing recursion validation, enforce `SelfCallKind != NonTailCall` for `Loop`.
   4. Emit new recursion error on violation.
4. In `RecursionCheck.scala`:
   1. Add new error case and message.
5. In tests:
   1. `ParserTest.scala`: parse and round-trip `loop` forms.
   2. `TypedExprRecursionCheckTest.scala`: allow tail-recursive `loop`; reject non-tail-recursive `loop`; verify `recur` still allows non-tail recursion; verify tuple `loop` still uses existing lexicographic rules; verify `def loop(...)` remains legal.
   3. `ErrorMessageTest.scala`: assert new diagnostic text.
   4. `LegacyDefRecursionCheck.scala` and `WellTypedGen.scala`: mechanical updates for the new declaration match-mode type so test code compiles.
   5. `ParserTest.scala`: explicitly verify `loop = x -> fn(x); loop(12)` parses to ordinary binding + apply.
6. In docs:
   1. `docs/src/main/paradox/recursion.md`: add `loop` semantics and examples.
   2. `docs/src/main/paradox/language_guide.md`: document when to use `recur` vs `loop`.

## Testing strategy
1. Parser coverage:
   1. `loop x:` and `loop (a, b):` parse and pretty-print.
   2. `def loop(x): ...` still parses.
   3. `loop = x -> fn(x)` and `loop(12)` still parse as normal value binding/application.
   4. malformed `loop` headers produce useful parse hints.
2. Recursion checker coverage:
   1. Tail-recursive accumulator loop succeeds.
   2. Non-tail recursion in branch result (`Succ(f(x))`) fails in `loop`.
   3. Recursive call in guard fails in `loop`.
   4. Passing recursive function value (`cont_fn(loop)`) fails in `loop`.
   5. Equivalent programs using `recur` preserve prior behavior.
3. Regression coverage:
   1. Existing recursion suites remain green.
   2. Existing loop/recur lowering tests remain green.

## Acceptance criteria
1. Bosatsu source accepts `loop <target>:` anywhere `recur <target>:` is currently legal.
2. `loop` enforces all recursive self-calls are tail-position and fails compilation otherwise.
3. `loop` retains all existing `recur` termination guarantees (structural and lexicographic checks).
4. `recur` behavior is unchanged, including acceptance of valid non-tail recursion.
5. Existing identifier usage `loop` outside recursion-header context remains valid.
6. New recursion error messaging exists and is asserted in tests.
7. Parser, recursion-check, and error-message tests are added for positive and negative `loop` cases.
8. User docs describe `loop` and explain when to use `recur` instead.

## Risks and mitigations
1. Risk: tail-position classification mismatches lowering behavior in edge cases.  
Mitigation: use shared `SelfCallKind` logic already relied on by normalization/Matchless paths, plus targeted regression cases.
2. Risk: contextual `loop` parsing could accidentally break code using `loop` identifiers.  
Mitigation: keep `loop` out of global reserved keywords and require exact header form `loop <arg>:`.
3. Risk: diagnostic region may point to the `loop` header instead of the exact non-tail call site.  
Mitigation: ship with clear function-level message first; add precise non-tail site extraction in follow-up if needed.
4. Risk: confusion between `recur` and `loop`.  
Mitigation: docs include explicit decision guidance and examples of non-tail algorithms that should remain on `recur`.

## Rollout notes
1. Ship as a single compiler/docs PR with no runtime or artifact-format changes.
2. No migration is required for existing code because `recur` semantics do not change.
3. Adoption can be incremental: users opt into stronger guarantees by replacing `recur` with `loop`.
4. Follow-up (optional): context-aware VS Code grammar highlighting for `loop` header form.

## Alternatives considered
1. `defloop` at function header:
   1. Pro: no contextual-keyword ambiguity.
   2. Con: guarantee is less local to recursion site and less aligned with issue preference.
2. Def-level modifier or annotation:
   1. Pro: familiar for Scala users.
   2. Con: requires threading def-level metadata through more compiler layers.
3. `recur ... by tail` strategy clause:
   1. Pro: extensible strategy surface.
   2. Con: heavier syntax than needed for this focused guarantee.
