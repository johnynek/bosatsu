---
title: "feat: BosatsuUI Conditional Rendering via Static Analysis"
type: feat
date: 2026-01-30
---

# feat: BosatsuUI Conditional Rendering via Static Analysis

## Overview

Enhance BosatsuUI's static analysis to extract structured condition information from Match expressions over sum types, enabling React-like conditional rendering while preserving O(1) update performance. This unlocks the full power of Bosatsu's type system for UI programming.

## Problem Statement / Motivation

### Current Limitation

The UIAnalyzer marks bindings as `conditional: Boolean` but discards critical information:

```scala
// Current: UIAnalyzer.scala lines 332-340
case TypedExpr.Match(arg, branches, _) =>
  analyzeExpr(arg, ctx)
  ctx.inConditional = true
  branches.toList.foreach { case (_, branchExpr) =>  // Pattern DISCARDED!
    analyzeExpr(branchExpr, ctx)
  }
```

This means we know a binding is conditional, but not:
- **Which** state path controls it
- **Which** variant activates it
- **How** to evaluate the condition at runtime

### Why This Matters

Bosatsu has properties React lacks:
1. **No mutations** - state transitions are explicit
2. **No runaway recursion** - all recursion terminates
3. **Full type information** - we know every value's shape at compile time
4. **Exhaustive matching** - every branch is statically known

**We can statically enumerate all possible UI states.**

React must diff at runtime because it doesn't know what the next render will produce. BosatsuUI can know at compile time.

## Proposed Solution

### Core Idea: Condition Hoisting

Instead of embedding conditions in DOM structure:
```bosatsu
match status:
  Loading: h("div", [], [text("Loading...")])
  Success(data): h("div", [], [text(data.name)])
```

Hoist conditions into the binding model:
```javascript
{
  elementId: "bosatsu-0",
  property: "textContent",
  when: { discriminant: ["status"], tag: "Loading" },
  value: "Loading..."
}
{
  elementId: "bosatsu-0",
  property: "textContent",
  when: { discriminant: ["status"], tag: "Success" },
  valuePath: ["status", "Success", "data", "name"]
}
```

The binding IS the condition—the DOM structure stays static.

### Key Changes

1. **Enhance `BranchCondition`** with discriminant path and variant tag
2. **Add `when: Option[BranchCondition]`** to `DOMBinding`
3. **Extract pattern information** from Match branches (not discard it)
4. **Track pattern bindings** so data paths work
5. **Generate conditional bindings** in JavaScript output

## Technical Considerations

### Architecture

```
TypedExpr.Match(arg, branches)
          │
          ▼
┌─────────────────────────────────────────────┐
│ Extract discriminant path from `arg`        │
│ (Local, Global, or state read)              │
└─────────────────────┬───────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────┐
│ For each branch:                            │
│   1. Extract variant tag from Pattern       │
│   2. Add pattern bindings to context        │
│   3. Analyze branch body                    │
│   4. Record bindings with BranchCondition   │
└─────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────┐
│ Output: DOMBinding with                     │
│   when: BranchCondition(path, tag)          │
└─────────────────────────────────────────────┘
```

### Data Model Changes

```scala
// Enhanced BranchCondition
final case class BranchCondition(
    discriminant: List[String],  // Path being matched: ["status"]
    tag: String,                 // Variant name: "Success"
    isTotal: Boolean = false     // True for wildcard/fallback
)

// Enhanced DOMBinding
final case class DOMBinding[A](
    elementId: String,
    property: DOMProperty,
    statePath: List[String],
    when: Option[BranchCondition],  // Replaces `conditional: Boolean`
    transform: Option[String],
    sourceExpr: TypedExpr[A]
) {
  def conditional: Boolean = when.isDefined
}
```

### Pattern Extraction

```scala
def extractVariantTag(pattern: Pattern[(PackageName, Constructor), Type]): Option[String] =
  pattern match {
    case Pattern.PositionalStruct((_, cons), _) => Some(cons.asString)
    case Pattern.Named(_, inner) => extractVariantTag(inner)
    case Pattern.Annotation(inner, _) => extractVariantTag(inner)
    case Pattern.WildCard | Pattern.Var(_) => None  // Total match
    case Pattern.Union(h, t) => None  // Complex - needs special handling
    case _ => None
  }
```

### Discriminant Path Extraction

```scala
def extractDiscriminantPath[A](expr: TypedExpr[A], ctx: AnalysisContext[A]): Option[List[String]] =
  expr match {
    case TypedExpr.Local(name, _, _) =>
      ctx.bindingToPath.get(name).orElse(Some(List(name.asString)))
    case TypedExpr.Global(_, name, _, _) =>
      Some(List(name.asString))
    case TypedExpr.App(fn, args, _, _) if isStateRead(fn) =>
      extractStatePathFromRead(args.head)
    case _ => None  // Complex expression - not supported initially
  }
```

### Pattern Binding Propagation

When we match `Success(user)`:
1. `user` becomes bound in the branch scope
2. Path for `user` is `discriminant ++ List(tag, fieldIndex)`
3. Example: `["status", "Success", "0"]` for first field

```scala
def addPatternBindings(
    pattern: Pattern[(PackageName, Constructor), Type],
    discriminant: List[String],
    tag: String,
    ctx: AnalysisContext[A]
): Unit = pattern match {
  case Pattern.PositionalStruct(_, params) =>
    params.zipWithIndex.foreach { case (p, idx) =>
      p match {
        case Pattern.Var(name) =>
          ctx.bindingToPath += (name -> (discriminant ++ List(tag, idx.toString)))
        case Pattern.Named(name, inner) =>
          ctx.bindingToPath += (name -> (discriminant ++ List(tag, idx.toString)))
          addPatternBindings(inner, discriminant, tag, ctx)
        case _ => ()
      }
    }
  case Pattern.Named(name, inner) =>
    ctx.bindingToPath += (name -> discriminant)
    addPatternBindings(inner, discriminant, tag, ctx)
  case _ => ()
}
```

### JavaScript Output

```javascript
const _bindings = {
  "status": [
    {
      elementId: "bosatsu-0",
      property: "textContent",
      when: { discriminant: ["status"], tag: "Loading" },
      value: "Loading..."
    },
    {
      elementId: "bosatsu-0",
      property: "textContent",
      when: { discriminant: ["status"], tag: "Success" },
      valuePath: ["status", "Success", "data", "name"]
    }
  ]
};

// Runtime: Check condition before applying binding
function applyBinding(binding, state) {
  if (binding.when) {
    const value = getAtPath(state, binding.when.discriminant);
    if (value?.tag !== binding.when.tag) return;  // Condition not met
  }
  const newValue = binding.valuePath
    ? getAtPath(state, binding.valuePath)
    : binding.value;
  document.getElementById(binding.elementId)[binding.property] = newValue;
}
```

## Acceptance Criteria

### Phase 1: Core Analysis Enhancement

- [x] Enhance `BranchCondition` with `discriminant`, `tag`, `isTotal` fields
- [x] Modify `DOMBinding` to use `when: Option[BranchCondition]` instead of `conditional: Boolean`
- [x] Implement `extractDiscriminantPath` for Local/Global/StateRead match arguments
- [x] Implement `extractVariantTag` for `Pattern.PositionalStruct`
- [x] Modify Match handling to extract pattern info (not discard it)
- [x] Add pattern binding propagation to context

### Phase 2: Pattern Binding Support

- [x] Track pattern-bound variables in `bindingToPath`
- [x] Support nested field access through variant data
- [x] Handle `Pattern.Named` wrapper patterns
- [x] Handle `Pattern.Annotation` wrapper patterns

### Phase 3: JavaScript Generation

- [x] Update `bindingsToJs` to serialize `when` clause
- [x] Add runtime condition checking in `bosatsu-ui-runtime.js`
- [x] Support static values (not just valuePaths) in conditional bindings
- [ ] Test with BosatsuUI batching

### Phase 4: Demo and Testing

- [x] Create `demos/ui/conditional.bosatsu` with sum type example
- [x] Generate working HTML with conditional rendering
- [x] Add UIAnalyzer tests for pattern extraction
- [x] Add UIAnalyzer tests for discriminant extraction
- [x] Add UIAnalyzer tests for pattern binding propagation
- [x] Add E2E Playwright tests for conditional demo

### Phase 5: Edge Cases

- [x] Handle wildcard patterns (isTotal = true)
- [x] Handle match on state reads directly
- [ ] Handle nested match expressions (compound conditions)
- [ ] Document unsupported patterns (Union, complex expressions)

## Implementation Tasks

### Phase 1: Data Model (`UIAnalyzer.scala`)

**Task 1.1: Enhance BranchCondition**

```scala
// core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala
// Replace lines 84-87

final case class BranchCondition(
    discriminant: List[String],  // State path being matched
    tag: String,                 // Variant tag or literal
    isTotal: Boolean = false     // True for wildcard/catch-all
)
```

**Task 1.2: Modify DOMBinding**

```scala
// Modify DOMBinding case class
final case class DOMBinding[A](
    elementId: String,
    property: DOMProperty,
    statePath: List[String],
    when: Option[BranchCondition],  // NEW: structured condition
    transform: Option[String],
    sourceExpr: TypedExpr[A]
) {
  // Backwards compatibility
  def conditional: Boolean = when.isDefined
}
```

**Task 1.3: Add helper functions**

```scala
private def extractDiscriminantPath[A](
    expr: TypedExpr[A],
    ctx: AnalysisContext[A]
): Option[List[String]]

private def extractVariantTag(
    pattern: Pattern[(PackageName, Constructor), Type]
): Option[String]

private def addPatternBindings[A](
    pattern: Pattern[(PackageName, Constructor), Type],
    discriminant: List[String],
    tag: String,
    ctx: AnalysisContext[A]
): Unit
```

### Phase 2: Match Analysis (`UIAnalyzer.scala`)

**Task 2.1: Rewrite Match handling**

```scala
// Replace lines 332-340
case TypedExpr.Match(arg, branches, _) =>
  analyzeExpr(arg, ctx)

  // Extract discriminant path
  val discriminant = extractDiscriminantPath(arg, ctx)

  // Save context state
  val savedBindingToPath = ctx.bindingToPath
  val wasConditional = ctx.inConditional
  ctx.inConditional = true

  // Set current discriminant for nested bindings
  ctx.currentDiscriminant = discriminant

  branches.toList.foreach { case (pattern, branchExpr) =>
    val variantTag = extractVariantTag(pattern)

    // Create condition for this branch
    val condition = (discriminant, variantTag) match {
      case (Some(d), Some(t)) => Some(BranchCondition(d, t))
      case (Some(d), None) => Some(BranchCondition(d, "_", isTotal = true))
      case _ => None
    }

    // Add pattern bindings to context
    (discriminant, variantTag).tupled.foreach { case (d, t) =>
      addPatternBindings(pattern, d, t, ctx)
    }

    // Set current condition for bindings in this branch
    ctx.currentCondition = condition

    analyzeExpr(branchExpr, ctx)

    // Restore context for next branch
    ctx.bindingToPath = savedBindingToPath
  }

  ctx.inConditional = wasConditional
  ctx.currentCondition = None
  ctx.currentDiscriminant = None
  None
```

**Task 2.2: Update AnalysisContext**

```scala
private final class AnalysisContext[A](
    // ... existing fields ...
    var currentCondition: Option[BranchCondition] = None,
    var currentDiscriminant: Option[List[String]] = None
)
```

**Task 2.3: Update binding recording**

```scala
// In recordBinding, use currentCondition
def recordBinding(binding: DOMBinding[A]): Unit = {
  val bindingWithCondition = binding.copy(when = ctx.currentCondition)
  bindings = bindingWithCondition :: bindings
}
```

### Phase 3: JavaScript Generation (`UIGen.scala` + `bosatsu-ui-runtime.js`)

**Task 3.1: Update bindingsToJs**

```scala
// In bindingsToJs method
private def bindingToJs(binding: DOMBinding[_]): String = {
  val whenJs = binding.when match {
    case Some(BranchCondition(disc, tag, isTotal)) =>
      s"""when: { discriminant: ${disc.mkString("[\"", "\",\"", "\"]")}, tag: "$tag", isTotal: $isTotal }"""
    case None => ""
  }
  // ... rest of binding serialization
}
```

**Task 3.2: Update runtime condition checking**

```javascript
// In bosatsu-ui-runtime.js
function _checkCondition(binding, state) {
  if (!binding.when) return true;
  if (binding.when.isTotal) return true;

  const discriminantValue = _getAtPath(state, binding.when.discriminant);
  if (!discriminantValue) return false;

  // Sum types have a 'tag' property
  return discriminantValue.tag === binding.when.tag;
}

function _applyBindingsForPath(path) {
  const bindings = _bindings[key];
  if (!bindings) return;

  for (const binding of bindings) {
    if (!_checkCondition(binding, _state)) continue;  // NEW: condition check

    // ... rest of binding application
  }
}
```

### Phase 4: Testing

**Task 4.1: Unit tests for pattern extraction**

```scala
// UIAnalyzerTest.scala
test("extractVariantTag from PositionalStruct") {
  val pattern = Pattern.PositionalStruct(
    (PackageName.PredefName, Constructor("Success")),
    List(Pattern.Var(Identifier.unsafeBindable("data")))
  )
  assertEquals(UIAnalyzer.extractVariantTag(pattern), Some("Success"))
}

test("extractVariantTag from Named wrapping PositionalStruct") {
  val inner = Pattern.PositionalStruct(...)
  val pattern = Pattern.Named(Identifier.unsafeBindable("x"), inner)
  assertEquals(UIAnalyzer.extractVariantTag(pattern), Some("Success"))
}

test("extractVariantTag returns None for WildCard") {
  assertEquals(UIAnalyzer.extractVariantTag(Pattern.WildCard), None)
}
```

**Task 4.2: Unit tests for discriminant extraction**

```scala
test("extractDiscriminantPath from Local") {
  val local = makeLocal("status")
  val ctx = new AnalysisContext[Unit]()
  assertEquals(extractDiscriminantPath(local, ctx), Some(List("status")))
}

test("extractDiscriminantPath from state read") {
  val readApp = makeApp(makeGlobal("Bosatsu/UI", "read"), makeLocal("status"))
  val ctx = new AnalysisContext[Unit]()
  ctx.bindingToPath += (Identifier.unsafeBindable("status") -> List("status"))
  assertEquals(extractDiscriminantPath(readApp, ctx), Some(List("status")))
}
```

**Task 4.3: Integration tests**

```scala
test("match expression generates conditional bindings with when clause") {
  TestUtils.checkLast("""
    |package Test
    |
    |enum Status: Loading, Success(msg: String)
    |
    |status = Success("done")
    |
    |ui = match status:
    |  Loading: text("Loading...")
    |  Success(msg): text(msg)
  """.stripMargin.trim) { typedExpr =>
    val analysis = UIAnalyzer.analyze(typedExpr)

    // Should have 2 conditional bindings
    assertEquals(analysis.bindings.length, 2)

    val loadingBinding = analysis.bindings.find(_.when.exists(_.tag == "Loading"))
    assert(loadingBinding.isDefined)
    assertEquals(loadingBinding.get.when.get.discriminant, List("status"))

    val successBinding = analysis.bindings.find(_.when.exists(_.tag == "Success"))
    assert(successBinding.isDefined)
    assertEquals(successBinding.get.statePath, List("status", "Success", "msg"))
  }
}
```

### Phase 5: Demo

**Task 5.1: Create conditional.bosatsu**

```bosatsu
package ConditionalDemo

# Define a status enum
enum Status: Loading, Error(message: String), Success(data: String)

# State
status = state(Loading)

# Event handlers
def setLoading(): Unit = write(status, Loading)
def setError(): Unit = write(status, Error("Something went wrong"))
def setSuccess(): Unit = write(status, Success("Data loaded!"))

# UI with conditional rendering
ui = h("div", [], [
  h("div", [("class", "buttons")], [
    h("button", [on_click(setLoading)], [text("Loading")]),
    h("button", [on_click(setError)], [text("Error")]),
    h("button", [on_click(setSuccess)], [text("Success")])
  ]),
  h("div", [("class", "result")], [
    match read(status):
      Loading: h("div", [("class", "loading")], [text("Loading...")])
      Error(msg): h("div", [("class", "error")], [text(msg)])
      Success(data): h("div", [("class", "success")], [text(data)])
  ])
])
```

**Task 5.2: Add Playwright E2E test**

```typescript
// tests/e2e/bosatsu-ui-conditional.spec.ts
test('conditional rendering updates on state change', async ({ page }) => {
  await page.goto('/demos/ui/conditional.html');

  // Initial state: Loading
  await expect(page.locator('.loading')).toBeVisible();
  await expect(page.locator('.loading')).toHaveText('Loading...');

  // Click Error button
  await page.click('button:has-text("Error")');
  await expect(page.locator('.error')).toBeVisible();
  await expect(page.locator('.error')).toHaveText('Something went wrong');

  // Click Success button
  await page.click('button:has-text("Success")');
  await expect(page.locator('.success')).toBeVisible();
  await expect(page.locator('.success')).toHaveText('Data loaded!');
});
```

## Success Metrics

1. **Static analysis extracts variant info**: Match expressions produce bindings with `when` clauses containing discriminant and tag
2. **Pattern bindings propagate**: Data access through variant fields works (e.g., `Success(data)` -> `data.name`)
3. **Runtime condition checking works**: Bindings only apply when their condition is met
4. **Demo works end-to-end**: Conditional demo shows different UI based on state

## Dependencies & Risks

**Dependencies:**
- Existing UIAnalyzer infrastructure
- Pattern types in Pattern.scala
- TypedExpr.Match node structure

**Risks:**
- Complex patterns (Union, guards) may require fallback to boolean flag
- Nested match expressions need careful condition composition
- Breaking change to DOMBinding requires updating all call sites

**Mitigations:**
- Start with simple sum types; document unsupported patterns
- Add backwards-compatible `conditional` method to DOMBinding
- Comprehensive test coverage before refactoring

## References & Research

### Internal References

- Current UIAnalyzer: `core/src/main/scala/dev/bosatsu/ui/UIAnalyzer.scala`
- Pattern types: `core/src/main/scala/dev/bosatsu/Pattern.scala`
- TypedExpr: `core/src/main/scala/dev/bosatsu/TypedExpr.scala`
- Matchless IR: `core/src/main/scala/dev/bosatsu/Matchless.scala`
- BosatsuUI runtime: `core/src/main/resources/bosatsu-ui-runtime.js`

### Brainstorm Document

- `docs/brainstorms/2026-01-30-conditional-rendering-static-analysis-brainstorm.md`

### Institutional Learnings Applied

- **State monad for AST analysis**: Use functional state threading in analysis
- **Store AST references not copies**: DOMBinding stores sourceExpr reference
- **Selector consistency**: Use constants for element IDs
