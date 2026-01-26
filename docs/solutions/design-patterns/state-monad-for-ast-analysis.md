---
title: "State Monad for AST Analysis"
date: 2026-01-25
category: design-patterns
tags:
  - state-monad
  - cats
  - functional-programming
  - ast-traversal
  - graph-building
  - scala
module: bosatsu/analysis
symptoms:
  - Need to accumulate results while traversing AST
  - Want to avoid mutable state in analysis code
  - Building a graph or other structure from tree traversal
  - Need to thread state through recursive calls
applies_to:
  - Scala
  - Cats library
  - AST analysis
  - Compiler passes
  - Static analysis tools
---

# State Monad for AST Analysis

## Problem

When traversing an AST to build a graph or accumulate information, you need to:
- Generate fresh IDs for each node
- Track mappings (e.g., variable name → node ID)
- Accumulate nodes and edges into a result structure
- Thread this state through recursive calls

The imperative approach uses mutable state:

```scala
// Anti-pattern: Mutable state scattered through analysis
class ProvenanceAnalyzer {
  private var nextId: Long = 0
  private val nodes = mutable.Map[Long, Node]()
  private val deps = mutable.Map[Long, Set[Long]]()

  def analyze(expr: TypedExpr[T]): Graph = {
    nextId = 0
    nodes.clear()
    deps.clear()

    analyzeExpr(expr, mutable.Map.empty)

    Graph(nodes.toMap, deps.toMap)
  }

  private def analyzeExpr(expr: TypedExpr[T], env: mutable.Map[String, Long]): Long = {
    val id = nextId
    nextId += 1
    // ... mutation everywhere
  }
}
```

**Problems**:
- Not thread-safe
- Hard to test (state leaks between calls)
- Mutation scattered across methods
- Can't easily compose with other analyses

## Solution

Use the State monad from Cats to thread state functionally:

```scala
import cats.data.State
import cats.implicits._

object ProvenanceAnalyzer {

  // Define the state we're threading
  private case class AnalysisState[T](
    builder: DerivationGraph.Builder[T]
  )

  // Define the State monad type alias for clarity
  private type Analysis[T, A] = State[AnalysisState[T], A]

  // State operations as small, composable functions
  private def freshId[T]: Analysis[T, Long] =
    State(s => (s, s.builder.freshId()))

  private def addNode[T](node: ProvenanceNode[T]): Analysis[T, Unit] =
    State.modify(s => { s.builder.addNode(node); s })

  private def addDep[T](from: Long, to: Long): Analysis[T, Unit] =
    State.modify(s => { s.builder.addDependency(from, to); s })

  // Main analysis uses for-comprehension to sequence operations
  private def analyzeExpr[T](
    expr: TypedExpr[T],
    env: Map[Identifier.Bindable, Long]
  )(implicit tag: HasRegion[T]): Analysis[T, Long] = {

    expr match {
      case lit: TypedExpr.Literal[T] =>
        for {
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, lit))
        } yield id

      case app: TypedExpr.App[T] =>
        for {
          fnId <- analyzeExpr(app.fn, env)           // Recurse on function
          argIds <- app.args.traverse(a => analyzeExpr(a, env))  // Recurse on args
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, app))
          _ <- addDep(id, fnId)
          _ <- addDeps(id, argIds.toList)
        } yield id

      case let: TypedExpr.Let[T] =>
        for {
          boundId <- analyzeExpr(let.expr, env)
          newEnv = env + (let.arg -> boundId)        // Extend environment
          inId <- analyzeExpr(let.in, newEnv)        // Use extended env
          id <- freshId[T]
          _ <- addNode(ProvenanceNode(id, let))
          _ <- addDep(id, inId)
        } yield id

      // ... other cases
    }
  }

  // Entry point: run the state computation
  def analyze[T](expr: TypedExpr[T])(implicit tag: HasRegion[T]): DerivationGraph[T] = {
    val initialState = AnalysisState(new DerivationGraph.Builder[T])
    val (finalState, rootId) = analyzeExpr(expr, Map.empty).run(initialState).value
    finalState.builder.addRoot(rootId)
    finalState.builder.build()
  }
}
```

## Key Techniques

### 1. Define State Type Alias

```scala
private type Analysis[T, A] = State[AnalysisState[T], A]
```

This makes signatures cleaner and intent clearer.

### 2. Small State Operations

Break state manipulation into small, focused functions:

```scala
private def freshId[T]: Analysis[T, Long] =
  State(s => (s, s.builder.freshId()))

private def addNode[T](node: ProvenanceNode[T]): Analysis[T, Unit] =
  State.modify(s => { s.builder.addNode(node); s })

private def addDep[T](from: Long, to: Long): Analysis[T, Unit] =
  State.modify(s => { s.builder.addDependency(from, to); s })
```

Each operation does one thing. They compose via for-comprehension.

### 3. For-Comprehension for Sequencing

```scala
for {
  fnId <- analyzeExpr(app.fn, env)      // First: analyze function
  argIds <- app.args.traverse(...)       // Then: analyze arguments
  id <- freshId[T]                       // Then: get fresh ID
  _ <- addNode(ProvenanceNode(id, app))  // Then: add node
  _ <- addDep(id, fnId)                  // Then: add dependencies
} yield id                               // Finally: return the ID
```

The for-comprehension sequences operations and threads state automatically.

### 4. Traverse for Collections

Use `.traverse` to apply a stateful operation to each element:

```scala
// Analyze all arguments, collecting their IDs
argIds <- app.args.traverse(arg => analyzeExpr(arg, env))
```

This is like `.map` but threads State through each element.

### 5. Environment Extension for Scopes

For let bindings and lambdas, extend the environment functionally:

```scala
case let: TypedExpr.Let[T] =>
  for {
    boundId <- analyzeExpr(let.expr, env)
    newEnv = env + (let.arg -> boundId)   // Create extended env
    inId <- analyzeExpr(let.in, newEnv)   // Pass to body
    // ...
  } yield id
```

The environment is passed as a parameter, not stored in State.

### 6. Run to Extract Result

```scala
def analyze[T](expr: TypedExpr[T]): DerivationGraph[T] = {
  val initialState = AnalysisState(new DerivationGraph.Builder[T])
  val (finalState, rootId) = analyzeExpr(expr, Map.empty)
    .run(initialState)  // Run with initial state
    .value              // Extract from Eval
  finalState.builder.build()
}
```

## Benefits

| Aspect | Mutable Approach | State Monad |
|--------|------------------|-------------|
| Thread safety | Unsafe | Safe (immutable) |
| Testing | Hard (state leaks) | Easy (pure functions) |
| Composition | Manual state passing | Automatic via monad |
| Readability | Mutation scattered | Sequential in for-comp |
| Reasoning | Track mutation order | Follow data flow |

## When to Use

**Good fit:**
- AST traversal accumulating results
- Graph/tree building from traversal
- Multi-pass analysis needing shared state
- When you'd otherwise use mutable Maps/counters

**Maybe overkill:**
- Simple single-value accumulation (use fold)
- No need to thread state (just return values)
- Performance-critical hot paths (benchmark first)

## Common Patterns

### Optional Dependencies

```scala
case local: TypedExpr.Local[T] =>
  for {
    id <- freshId[T]
    definedAt = env.get(local.name)  // Option[Long]
    _ <- addNode(ProvenanceNode(id, local))
    _ <- definedAt.traverse_(defId => addDep(id, defId))  // Only add if Some
  } yield id
```

Use `.traverse_` to conditionally execute stateful operation.

### Handling Multiple Bindings

```scala
def analyzeBindings[T](bindings: List[(Name, Expr)]): Analysis[T, Unit] = {
  bindings.foldLeft((State.pure[S, Map[Name, Long]](Map.empty))) {
    case (accState, (name, expr)) =>
      for {
        env <- accState
        exprId <- analyzeExpr(expr, env)
      } yield env + (name -> exprId)
  }.void
}
```

Use `foldLeft` with State to process bindings in order.

## Cross-References

- `core/src/main/scala/dev/bosatsu/analysis/ProvenanceAnalyzer.scala` - Full implementation
- `core/src/main/scala/dev/bosatsu/rankn/Infer.scala` - Type inference uses similar pattern
- [Cats State documentation](https://typelevel.org/cats/datatypes/state.html)
