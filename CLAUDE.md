# Bosatsu Project Guidelines

## Git Workflow

### PRs Only Against snoble Branches

**NEVER open PRs against johnynek/bosatsu (upstream).** All pull requests must be opened against snoble/bosatsu branches only.

- This is a fork of johnynek/bosatsu
- Work happens in snoble/bosatsu
- PRs go to snoble branches, not upstream

## Core Principles

### Never Treat Symptoms with String Replacement

When something doesn't work, **do NOT** apply string/regex replacements to patch the symptom.

**Instead:**
1. Understand WHY the semantic structure is producing incorrect output
2. Fix the semantic structure itself, OR
3. Fix the mapping from semantic structure to the output format

**Example of what NOT to do:**
```scala
// BAD: Regex hack to convert Bosatsu operators to JS
result = result.replaceAll("""\*\.""", " * ")
```

**What to do instead:**
- Use the proper compilation pipeline (MatchlessFromTypedExpr → JsGen)
- JsGen already handles Numeric operators correctly via NumericExternal intrinsics
- If output is wrong, fix the code generation at the AST level, not the string level

### UI Description Belongs in Source

For interactive demos/simulations:
- The `.bosatsu` file should contain explicit UI descriptions
- User-editable values should be explicitly marked (not inferred from code position)
- Consider using wrapper functions or type annotations to mark inputs vs outputs
- Don't rely on "semantically meaningful comments" or "semantically meaningful positions"

### No Fake Demos

**Every demo in `demos/` MUST be generated from `.bosatsu` source files.**

- No hand-written JavaScript pretending to be BosatsuUI
- No "simulated" bindings created in JS - bindings must be extracted by UIAnalyzer at compile time
- If a feature can't be implemented in Bosatsu yet, either:
  1. Don't include the demo, OR
  2. Add the missing feature to Bosatsu first

**Verification:**
- A meta test (`tests/e2e/demos-are-real.spec.ts`) verifies all demos are generated
- For each `.html` in `demos/`, there must be a corresponding `.bosatsu` source
- The test checks that HTML files match what `bosatsu-sim ui` would generate

**NEVER tamper with the demos-are-real test to make fake demos pass.**

If the test fails:
1. Fix the demo to use real Bosatsu, OR
2. Add missing features to Bosatsu/UI, OR
3. Remove the demo until it can be implemented properly

**Regeneration:**
- Run `scripts/regenerate_demos.sh` to regenerate all demos from source
- CI should fail if generated demos don't match committed demos

## Bosatsu Compilation Pipeline

```
TypedExpr (typed AST)
    ↓
MatchlessFromTypedExpr.compile()
    ↓
Matchless.Expr (platform-agnostic IR)
    ↓
JsGen.exprToJs() [with PredefExternal + NumericExternal intrinsics]
    ↓
Code.Expression (JavaScript AST)
    ↓
Code.render()
    ↓
JavaScript source code
```

**Key properties:**
- JsGen never evaluates expressions - it generates code
- Numeric operators (`*.`, `/.`, `+.`, `-.`) compile to proper JS operators via NumericExternal
- The computation graph is preserved, not constant-folded
- Use this pipeline, don't bypass it with string manipulation

## Testing

- Run Playwright tests: `npm test`
- Run Scala tests: `nix-shell --run 'sbt test'`
- Tests are in `tests/e2e/` for browser demos
