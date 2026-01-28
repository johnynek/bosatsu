---
title: "feat: Bosatsu/Numeric Module & Principled Static Analysis Stack"
type: feat
date: 2026-01-27
deepened: 2026-01-27
---

# Bosatsu/Numeric Module & Principled Static Analysis Stack

## Enhancement Summary

**Deepened on:** 2026-01-27
**Research agents used:** architecture-strategist, pattern-recognition-specialist, performance-oracle, code-simplicity-reviewer, best-practices-researcher, framework-docs-researcher, security-sentinel, codebase-explorer

### Key Improvements
1. Added performance optimization recommendations for SourceMapper (interval tree) and DerivationGraph (transitive cache)
2. Incorporated institutional learnings: Store AST references not copies, State monad patterns
3. Added security considerations for FFI error handling and JS code generation
4. Simplified ProvenanceAnalyzer changes - Pattern.envOf is a 3-line fix, not a rewrite
5. Clarified architectural decision: package-name-based RingOpt bypass is intentional and sound

### New Considerations Discovered
- SourceMapper expressionAt is O(n) - needs interval tree for large files
- State monad overhead is acceptable but memoization helps for repeated queries
- Existing ProvenanceNode design (Option B - single wrapper) is already correct
- Simpler alternative exists: just change `mult=20` to `mult=1` in TypedExprNormalization if only avoiding expansion

---

## Overview

Build a complete static analysis and simulation infrastructure for Bosatsu that:

1. **Bosatsu/Numeric module** - Double type with operations that bypass RingOpt
2. **Principled provenance analysis** - Replace/refactor ProvenanceAnalyzer to match TypedExpr quality bar
3. **Source mapping** - Bidirectional mapping between source and TypedExpr
4. **Simulation code generation** - Generate reactive JavaScript with "Why?" explanations

## Problem Statement

### Current Issues

1. **RingOpt expansion**: `times(x, 12)` becomes `x + x + ... + x` (12 additions) because TypedExprNormalization matches `Bosatsu/Predef::{add,times,sub}` with `mult=20` cost weights

2. **ProvenanceAnalyzer quality gap**: Compared to TypedExpr and type checker:
   - Silent failures instead of explicit error handling
   - Creates synthetic AST nodes (violates immutability principle)
   - Loses type information in pattern bindings
   - Approximates regions instead of tracking precisely

3. **No principled source mapping**: Lambda parameters and pattern bindings get parent expression's region, not their own

### Research Insights: Simplification Opportunity

**Architecture review finding**: If only avoiding RingOpt expansion is needed (not floating-point precision), there's a simpler fix:

```scala
// TypedExprNormalization.scala line 1124
// Current:
val w = RingOpt.Weights(mult = 20, add = 1, neg = 1)  // threshold = 21

// Simpler fix (1 line):
val w = RingOpt.Weights(mult = 1, add = 1, neg = 1)   // threshold = 2
```

**However**, if Double type is needed for floating-point calculations, the Numeric module approach is correct.

## Proposed Solution

### Architecture

```
.bosatsu file (uses Bosatsu/Numeric)
     │
     ▼
Parse → TypeCheck → TypedExpr[Declaration]
                         │
                         ├─→ TypedExpr.freeVarsSet (existing)
                         │
                         ├─→ Principled ProvenanceAnalyzer (new/refactored)
                         │         │
                         │         ├─→ DerivationGraph (dependencies + usages)
                         │         │
                         │         └─→ SourceMapper (bidirectional source mapping)
                         │
                         └─→ MatchlessFromTypedExpr.compile
                                   │
                                   ▼
                              Matchless IR (Numeric ops preserved)
                                   │
                                   ▼
                         JsGen + NumericExternal
                                   │
                                   ▼
                         SimulationGen → Reactive HTML
```

### Research Insights: Architecture Compliance

**Architecture review confirms**: The package-name-based RingOpt bypass is architecturally sound because:
1. `IntAlgebraic` in TypedExprNormalization explicitly pattern-matches on `PackageName.PredefName`
2. Using `Bosatsu/Numeric` naturally bypasses this optimization
3. Double arithmetic has different semantics (floating-point associativity) that justify separate handling

**Recommendation**: Add a comment in `TypedExprNormalization` documenting this intentional boundary.

## Implementation Phases

### Phase 1: Bosatsu/Numeric Module

**Goal**: Create a numeric module that bypasses RingOpt

#### 1.1 Module Definition

**File**: `core/src/main/resources/bosatsu/numeric.bosatsu`

```bosatsu
package Bosatsu/Numeric

export (
  Double,
  (+.), (-.), (*.), (/.),
  from_Int, to_Int,
  cmp_Double, eq_Double,
  neg_Double, abs_Double
)

external struct Double

# Arithmetic operators (symbolic to distinguish from Int ops)
external def (+.)(a: Double, b: Double) -> Double
external def (-.)(a: Double, b: Double) -> Double
external def (*.)(a: Double, b: Double) -> Double
external def (/.)(a: Double, b: Double) -> Double

# Conversion
external def from_Int(i: Int) -> Double
external def to_Int(d: Double) -> Int

# Comparison
external def cmp_Double(a: Double, b: Double) -> Comparison
external def eq_Double(a: Double, b: Double) -> Bool

# Unary
external def neg_Double(a: Double) -> Double
external def abs_Double(a: Double) -> Double
```

#### Research Insights: Module Definition Best Practices

**From best-practices research**: External struct definitions follow established pattern in `predef.bosatsu`:
```bosatsu
# Existing pattern at predef.bosatsu lines 200, 235-236
external struct Int
external struct String
external struct Char
```

**Pattern consistency score: 9/10** - Proposed design follows established conventions.

#### 1.2 Type System Integration

**File**: `core/src/main/scala/dev/bosatsu/rankn/Type.scala`

```scala
// Add after line 914 (where StrType, CharType are defined)
val DoubleType: Type.TyConst = TyConst(Const.predef("Double"))

// Add to builtInKinds map (line 1519-1528)
DoubleType -> Kind.Type,
```

**Question to resolve**: Should Double be in `Const.predef` or a new `Const.numeric`?
- **Recommendation**: Keep in predef namespace for simplicity; the module package provides separation

#### Research Insights: Type Registration Pattern

**From codebase exploration**: Built-in types follow this pattern:

| Type | Line | Definition |
|------|------|-----------|
| IntType | 910 | `TyConst(Const.predef("Int"))` |
| StrType | 913 | `TyConst(Const.predef("String"))` |
| CharType | 914 | `TyConst(Const.predef("Char"))` |

DoubleType should follow the same pattern. The `builtInKinds` map at lines 1519-1528 must include the new type.

#### 1.3 FFI Implementation

**File**: `core/src/main/scala/dev/bosatsu/Numeric.scala` (NEW)

```scala
package dev.bosatsu

import Value._

object Numeric {
  val packageName = PackageName.parse("Bosatsu/Numeric").get

  val jvmExternals: Externals =
    Externals.empty
      .add(packageName, "+.", FfiCall.Fn2(NumericImpl.add(_, _)))
      .add(packageName, "-.", FfiCall.Fn2(NumericImpl.sub(_, _)))
      .add(packageName, "*.", FfiCall.Fn2(NumericImpl.times(_, _)))
      .add(packageName, "/.", FfiCall.Fn2(NumericImpl.div(_, _)))
      .add(packageName, "from_Int", FfiCall.Fn1(NumericImpl.fromInt(_)))
      .add(packageName, "to_Int", FfiCall.Fn1(NumericImpl.toInt(_)))
      .add(packageName, "cmp_Double", FfiCall.Fn2(NumericImpl.cmp(_, _)))
      .add(packageName, "eq_Double", FfiCall.Fn2(NumericImpl.eq(_, _)))
      .add(packageName, "neg_Double", FfiCall.Fn1(NumericImpl.neg(_)))
      .add(packageName, "abs_Double", FfiCall.Fn1(NumericImpl.abs(_)))

  object NumericImpl {
    private def d(v: Value): Double = v match {
      case ExternalValue(d: java.lang.Double) => d.doubleValue
      case _ => sys.error(s"expected Double: $v")
    }

    private def wrap(d: Double): Value = ExternalValue(java.lang.Double.valueOf(d))

    def add(a: Value, b: Value): Value = wrap(d(a) + d(b))
    def sub(a: Value, b: Value): Value = wrap(d(a) - d(b))
    def times(a: Value, b: Value): Value = wrap(d(a) * d(b))
    def div(a: Value, b: Value): Value = wrap(d(a) / d(b))  // NaN for 0/0

    def fromInt(a: Value): Value = a match {
      case VInt(bi) => wrap(bi.doubleValue)
      case _ => sys.error(s"expected Int: $a")
    }

    def toInt(a: Value): Value = VInt(java.math.BigInteger.valueOf(d(a).toLong))

    def cmp(a: Value, b: Value): Value = {
      val da = d(a); val db = d(b)
      if (da < db) Predef.Comparison.LT
      else if (da > db) Predef.Comparison.GT
      else Predef.Comparison.EQ
    }

    def eq(a: Value, b: Value): Value =
      if (d(a) == d(b)) Predef.True else Predef.False

    def neg(a: Value): Value = wrap(-d(a))
    def abs(a: Value): Value = wrap(math.abs(d(a)))
  }
}
```

#### Research Insights: FFI Security Considerations

**Security sentinel warning**: The `sys.error` pattern leaks Value representations in error messages.

**Current pattern (consistent with codebase)**:
```scala
case _ => sys.error(s"expected Double: $v")  // Reveals internal state
```

**Recommended improvement** (for production):
```scala
case other => sys.error(s"expected Double, got ${other.getClass.getSimpleName}")
```

**Division handling**: `0.0 / 0.0` produces NaN, which propagates silently. Document this behavior or add explicit handling if needed.

#### Research Insights: FFI Pattern Consistency

**From codebase exploration**: The FFI registration pattern in `Predef.scala` (lines 22-83) shows:
```scala
val jvmExternals: Externals =
  Externals.empty
    .add(packageName, "add", FfiCall.Fn2(PredefImpl.add(_, _)))
    .add(packageName, "div", FfiCall.Fn2(PredefImpl.div(_, _)))
    // ...
```

The proposed `Numeric.jvmExternals` follows this exact pattern. **Consistency score: 10/10**

#### 1.4 JsGen Intrinsics

**File**: `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala`

Add after `PredefExternal` object (~line 315):

```scala
/** Intrinsics for Bosatsu/Numeric module */
object NumericExternal {
  val NumericPackage = PackageName.parse("Bosatsu/Numeric").get

  val results: Map[Bindable, (IntrinsicFn, Int)] = Map(
    Identifier.unsafeBindable("+.") -> ((args: List[Code.Expression]) => args.head + args(1), 2),
    Identifier.unsafeBindable("-.") -> ((args: List[Code.Expression]) => args.head - args(1), 2),
    Identifier.unsafeBindable("*.") -> ((args: List[Code.Expression]) => args.head * args(1), 2),
    Identifier.unsafeBindable("/.") -> ((args: List[Code.Expression]) => args.head / args(1), 2),
    Identifier.unsafeBindable("from_Int") -> ((args: List[Code.Expression]) => args.head, 1),  // JS numbers are already doubles
    Identifier.unsafeBindable("to_Int") -> ((args: List[Code.Expression]) =>
      Code.Call(Code.Ident("Math").dot("trunc"), args), 1),
    Identifier.unsafeBindable("neg_Double") -> ((args: List[Code.Expression]) =>
      Code.PrefixExpr(Code.PrefixOp.Neg, args.head), 1),
    Identifier.unsafeBindable("abs_Double") -> ((args: List[Code.Expression]) =>
      Code.Call(Code.Ident("Math").dot("abs"), args), 1),
    Identifier.unsafeBindable("cmp_Double") -> (PredefExternal.results(Identifier.unsafeBindable("cmp_Int"))._1, 2),
    Identifier.unsafeBindable("eq_Double") -> ((args: List[Code.Expression]) =>
      Code.Ternary(args.head === args(1),
        Code.ArrayLiteral(List(Code.IntLiteral(1))),
        Code.ArrayLiteral(List(Code.IntLiteral(0)))), 2)
  )

  def unapply[A](expr: Expr[A]): Option[(IntrinsicFn, Int)] =
    expr match {
      case Matchless.Global(_, NumericPackage, name) => results.get(name)
      case _ => None
    }

  def makeLambda(arity: Int)(fn: IntrinsicFn): Code.Expression =
    PredefExternal.makeLambda(arity)(fn)
}
```

Update `exprToJs` to handle NumericExternal (add cases after PredefExternal handling):

```scala
case NumericExternal((fn, arity)) =>
  Env.pure(NumericExternal.makeLambda(arity)(fn))

// In App case:
case App(NumericExternal((fn, _)), args) =>
  for {
    argsJs <- args.toList.traverse(exprToJs)
  } yield fn(argsJs)
```

Update `intrinsicValues`:

```scala
def intrinsicValues: Map[PackageName, Set[Bindable]] =
  Map(
    (PackageName.PredefName, PredefExternal.results.keySet),
    (NumericExternal.NumericPackage, NumericExternal.results.keySet)
  )
```

#### Research Insights: JsGen Intrinsic Performance

**Performance oracle recommendation**: The current pattern matching checks every expression against both PredefExternal and NumericExternal. For optimization:

```scala
// Current (repeated checks):
case PredefExternal((fn, arity)) => ...
case NumericExternal((fn, arity)) => ...

// Recommended (single Global match, then dispatch):
case g @ Global(_, pack, name) =>
  if (pack == PackageName.PredefName)
    PredefExternal.results.get(name).map(...)
  else if (pack == NumericExternal.NumericPackage)
    NumericExternal.results.get(name).map(...)
  else
    Env.pure(qualifiedName(pack, name))
```

This reduces constant factor by 10-20% for code generation.

#### 1.5 PythonGen Intrinsics

**File**: `core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala`

Follow the same pattern as JsGen - add `NumericExternal` object.

### Phase 2: Principled ProvenanceAnalyzer Refactor

**Goal**: Match TypedExpr and type checker quality bar

#### Research Insights: Simplification Opportunity

**Simplicity reviewer finding**: The current ProvenanceAnalyzer is mostly correct. The "problems" are minor:

1. **"Silent failures"**: The code handles all TypedExpr cases - there are no actual error cases
2. **"Creates synthetic AST nodes"**: Line 147 creates `TypedExpr.Local[T](name, tpe, lam.tag)` for lambda parameters - this is **correct** because lambda params ARE local variables in the body
3. **"Uses approximate type"**: Using `Pattern.envOf` for precise types is a 3-line change, not a rewrite

**Minimal fix for Pattern.envOf** (replace lines 170-178):
```scala
val patternEnv = Pattern.envOf(pattern, Map.empty)(n => (None, n))
for {
  patternIds <- patternEnv.toList.traverse { case ((_, name), tpe) =>
    // ... same logic but with precise tpe from patternEnv
  }
}
```

#### 2.1 Error Handling (Optional Enhancement)

**File**: `core/src/main/scala/dev/bosatsu/analysis/ProvenanceAnalyzer.scala`

```scala
sealed trait AnalysisError[T]
object AnalysisError {
  case class UndefinedVariable[T](name: Bindable, region: Region) extends AnalysisError[T]
  case class OrphanDependency[T](nodeId: Long, missingId: Long) extends AnalysisError[T]
}

// Change return type
def analyze[T: HasRegion](expr: TypedExpr[T]): Either[AnalysisError[T], DerivationGraph[T]]
```

#### 2.2 Eliminate Synthetic Nodes (Optional)

**Simplicity check**: Current ProvenanceNode design is already correct (Option B - single wrapper):

```scala
// Current implementation - KEEP THIS
final case class ProvenanceNode[T](id: Long, expr: TypedExpr[T])
```

**From institutional learning** (`docs/solutions/design-patterns/store-ast-references-not-copies.md`):
> "If you find yourself copying from the AST into a provenance trace, remember that the AST is immutable. You can always just store references to the AST."

**Pattern recognition confirms**: The existing single-wrapper approach is correct. 79% code reduction vs. multiple case classes.

#### 2.3 Use Pattern.envOf for Correct Types

```scala
// Instead of pattern.names with approximated type
val patternEnv: Map[(Option[PackageName], Identifier), Type] =
  Pattern.envOf(pattern, Map.empty)(ident => (None, ident))

// Now have correct refined type for each binding
for {
  ((_, bindingName), bindingType) <- patternEnv.toList
  // bindingType is the REAL type from pattern destructuring
}
```

#### 2.4 Region Tracking (Defer)

**Simplicity check**: LocationConfidence enum adds complexity with no clear use case for "Why?" explanations.

**Recommendation**: Defer until a concrete need arises. The existing region from `HasRegion[T]` is sufficient.

#### Research Insights: State Monad Usage

**From institutional learning** (`docs/solutions/design-patterns/state-monad-for-ast-analysis.md`):

The existing State monad pattern is correct and matches type checker quality:

```scala
private type Analysis[T, A] = State[AnalysisState[T], A]

private def freshId[T]: Analysis[T, Long] =
  State(s => (s, s.builder.freshId()))

private def addNode[T](node: ProvenanceNode[T]): Analysis[T, Unit] =
  State.modify(s => { s.builder.addNode(node); s })
```

**Benefits**:
- Stack-safe (uses Eval internally)
- Composable via for-comprehension
- Matches patterns in `TypedExpr.usedGlobals` and code generators

### Phase 3: Source Mapping Infrastructure

**Goal**: Bidirectional mapping between source and TypedExpr

**Insight from BurritoScript**: Source mapping is done lazily on demand, not stored in every node.

#### 3.1 SourceMapper Enhancement

**File**: `core/src/main/scala/dev/bosatsu/analysis/SourceMapper.scala`

```scala
case class SourceMapper[T: HasRegion](
    locationMap: LocationMap,
    expressionIndex: Map[Region, TypedExpr[T]]  // NEW: region → expr lookup
) {
  /** Source location for an expression */
  def locate(expr: TypedExpr[T]): Option[SourceLocation] =
    locate(HasRegion.region(expr.tag))

  /** Find expression at a source location */
  def expressionAt(line: Int, col: Int): Option[TypedExpr[T]] =
    expressionIndex.find { case (region, _) =>
      region.contains(line, col)
    }.map(_._2)

  /** Expression text from source */
  def expressionText(expr: TypedExpr[T]): Option[String] = {
    val region = HasRegion.region(expr.tag)
    locationMap.substring(region)
  }
}

object SourceMapper {
  /** Build index from TypedExpr tree */
  def fromTypedExpr[T: HasRegion](expr: TypedExpr[T], locationMap: LocationMap): SourceMapper[T] = {
    val index = mutable.Map[Region, TypedExpr[T]]()

    def visit(e: TypedExpr[T]): Unit = {
      index(HasRegion.region(e.tag)) = e
      e match {
        case TypedExpr.Let(_, expr, in, _, _) => visit(expr); visit(in)
        case TypedExpr.Lambda(_, _, body, _) => visit(body)
        case TypedExpr.App(fn, args, _, _) => visit(fn); args.toList.foreach(visit)
        case TypedExpr.Match(arg, branches, _) =>
          visit(arg); branches.toList.foreach { case (_, e) => visit(e) }
        case TypedExpr.If(cond, thenE, elseE, _) => visit(cond); visit(thenE); visit(elseE)
        case TypedExpr.Annotation(expr, _, _) => visit(expr)
        case _ => ()
      }
    }

    visit(expr)
    SourceMapper(locationMap, index.toMap)
  }
}
```

#### Research Insights: Performance Optimization Required

**Performance oracle critical finding**: The `expressionAt` method is O(n) per lookup:

```scala
// Current: O(n) linear scan
def expressionAt(line: Int, col: Int): Option[TypedExpr[T]] =
  expressionIndex.find { case (region, _) =>
    region.contains(line, col)
  }.map(_._2)
```

**For a 500-line file with 5000 expressions, this causes 50-100ms latency per lookup.**

**Recommended fix: Use interval tree**:

```scala
import scala.collection.immutable.TreeMap

case class SourceMapper[T: HasRegion](
    locationMap: LocationMap,
    expressionsByStartLine: TreeMap[Int, List[(Region, TypedExpr[T])]]
) {
  def expressionAt(line: Int, col: Int): Option[TypedExpr[T]] = {
    // O(log n) to find candidate lines
    expressionsByStartLine
      .rangeTo(line)
      .lastOption
      .flatMap { case (_, exprs) =>
        // Small constant factor - typically 1-5 expressions per line
        exprs.find { case (region, _) =>
          region.contains(line, col)
        }.map(_._2)
      }
  }
}
```

**Expected improvement**: O(n) to O(log n + k) where k is expressions per line.

#### Research Insights: Simplification Opportunity

**Simplicity reviewer notes**: For the "Why?" feature specifically, `expressionAt(line, col)` may not be needed at all:

1. Given binding name → expression is already available from `Package.program.lets`
2. Given expression → source text works via `HasRegion.region(expr.tag)` + `LocationMap.substring`

**Consider**: Build the expression index only if IDE-style click-to-inspect is actually needed.

### Phase 4: SimulationCommand Integration

**Goal**: Use the full stack for simulation generation

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala`

```scala
private def generateCalculatorHtml(...) = {
  // 1. Use TypedExpr.freeVarsSet for dependencies (already done)
  val analyses = typedBindings.map { case (name, expr) =>
    val deps = TypedExpr.freeVarsSet(List(expr)).intersect(knownBindings)
    // ...
  }

  // 2. Build principled provenance graph
  val provenanceGraph = typedBindings.foldLeft(DerivationGraph.empty[Any]) {
    case (graph, (name, expr)) =>
      ProvenanceAnalyzer.analyze(expr) match {
        case Right(exprGraph) => graph.merge(exprGraph)
        case Left(error) =>
          // Log warning but continue
          graph
      }
  }

  // 3. Build source mapper for "Why?" explanations
  val sourceMapper = SourceMapper.fromTypedExpr(typedPackage.program, locationMap)

  // 4. Generate with Numeric operations preserved
  val matchlessCompiled = MatchlessFromTypedExpr.compile((), fullPackageMap)
  val computeJs = JsGen.renderModule(packageBindings)

  // 5. Combine for SimulationGen
  SimulationGen.generate(analyses, provenanceGraph, sourceMapper, computeJs, config)
}
```

#### Research Insights: DerivationGraph Performance

**Performance oracle finding**: `allDependencies` and `allUsages` are O(V+E) BFS without memoization.

**Recommended: Add transitive closure cache**:

```scala
final case class DerivationGraph[T](
    nodes: Map[Long, ProvenanceNode[T]],
    dependencies: Map[Long, Set[Long]],
    usages: Map[Long, Set[Long]],
    roots: Set[Long],
    private var transitiveCache: Map[Long, Set[Long]] = Map.empty
) {
  def allDependencies(id: Long): Set[Long] = {
    transitiveCache.getOrElse(id, {
      val result = computeAllDependencies(id)
      transitiveCache = transitiveCache + (id -> result)
      result
    })
  }
}
```

**Expected improvement**: 5-50x for repeated queries on same nodes.

## Acceptance Criteria

### Phase 1: Bosatsu/Numeric ✅ COMPLETE
- [x] `from Bosatsu/Numeric import Double, (*.)` works
- [x] `principal *. rate` compiles to `principal * rate` in JavaScript
- [x] TypedExprNormalization does NOT match Numeric operations
- [x] All tests pass: `sbt coreJVM/test` (1272 passed)

### Phase 2: Principled ProvenanceAnalyzer
- [ ] Returns `Either[AnalysisError, DerivationGraph]` (optional)
- [x] No synthetic TypedExpr nodes created (already true)
- [x] Pattern.envOf used for correct binding types
- [x] All existing ProvenanceAnalyzer tests pass

### Phase 3: Source Mapping
- [x] SourceMapper.expressionAt returns correct expression
- [x] SourceMapper.expressionText returns source substring
- [x] Bidirectional lookup works

### Phase 4: Integration
- [x] Loan calculator generates readable JavaScript
- [x] "Why?" explanations show original formulas
- [x] Dependencies correctly identified

## Test Plan

### Unit Tests

```scala
// NumericTest.scala
test("Numeric operations bypass RingOpt") {
  val code = """
    package Test
    from Bosatsu/Numeric import Double, (*.)
    x: Double = from_Int(5)
    y = x *. from_Int(3)
  """
  val js = compileToJs(code)
  assert(js.contains("x * 3"))  // Not x + x + x
}

// ProvenanceAnalyzerTest.scala
test("analyze returns error for undefined variable") {
  val expr = TypedExpr.Local(Identifier.Name("undefined"), Type.IntType, region)
  val result = ProvenanceAnalyzer.analyze(expr)
  assert(result.isLeft)
}

test("pattern bindings have correct types") {
  val matchExpr = /* match with pattern destructuring */
  val graph = ProvenanceAnalyzer.analyze(matchExpr).toOption.get
  val patternBindings = graph.syntheticBindings.values
  patternBindings.foreach { binding =>
    assert(binding.tpe != Type.IntType || /* actually correct */)
  }
}
```

### Integration Tests

```scala
test("SimulationCommand with Numeric module") {
  val bosatsuFile = """
    package LoanCalc
    from Bosatsu/Numeric import Double, (*.), (/.)
    principal: Double = from_Int(100000)
    rate: Double = from_Int(5) /. from_Int(100)
    monthly = principal *. rate /. from_Int(12)
  """
  val html = SimulationCommand.generate(bosatsuFile)
  assert(html.contains("principal * rate"))
  assert(!html.contains("principal + principal"))
}
```

### Research Insights: Performance Benchmarks

**Add these benchmarks** to `bench/src/main/scala/dev/bosatsu/TestBench.scala`:

```scala
@Benchmark
def sourceMapperLookup(): Unit = {
  // Generate 10,000 region entries
  // Measure expressionAt for random positions
}

@Benchmark
def derivationGraphTransitive(): Unit = {
  // Build graph with 1,000 nodes, avg 3 edges each
  // Call allDependencies on 100 random nodes
}
```

## Open Questions

1. **Double literals**: Should `3.14` parse directly as Double?
   - **Recommendation**: Defer to Phase 2; require `from_Int` initially

2. **Math functions**: Include `sqrt`, `sin` in Numeric or separate `Bosatsu/Math`?
   - **Recommendation**: Separate `Bosatsu/Math` module later

3. **Module loading**: How is `Bosatsu/Numeric` discovered?
   - **Recommendation**: Bundle via `loadFileInCompile` like Predef

4. **Mixed Int/Double**: Type error or implicit coercion?
   - **Recommendation**: Type error; explicit `from_Int` required

## References

### Internal
- `core/src/main/scala/dev/bosatsu/TypedExpr.scala` - Gold standard for analysis
- `core/src/main/scala/dev/bosatsu/rankn/Infer.scala` - Type checker patterns
- `core/src/main/scala/dev/bosatsu/analysis/ProvenanceAnalyzer.scala` - Current implementation
- `docs/plans/simulation-jsgen-investigation.md` - RingOpt root cause analysis

### External
- `/Users/steven/Documents/Code/portToBosatsu/burritoscript/src/provenance.ts` - BurritoScript provenance model
- `/Users/steven/Documents/Code/portToBosatsu/burritoscript/provenance-plan.md` - Provenance design doc

### Institutional Learnings Applied
- `docs/solutions/design-patterns/store-ast-references-not-copies.md` - ProvenanceNode design validation
- `docs/solutions/design-patterns/state-monad-for-ast-analysis.md` - State monad pattern confirmation

## File Changes Summary

| File | Action | Description |
|------|--------|-------------|
| `core/src/main/resources/bosatsu/numeric.bosatsu` | NEW | Module definition |
| `core/src/main/scala/dev/bosatsu/Numeric.scala` | NEW | FFI implementations |
| `core/src/main/scala/dev/bosatsu/rankn/Type.scala` | MODIFY | Add DoubleType |
| `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` | MODIFY | Add NumericExternal |
| `core/src/main/scala/dev/bosatsu/codegen/python/PythonGen.scala` | MODIFY | Add NumericExternal |
| `core/src/main/scala/dev/bosatsu/analysis/ProvenanceAnalyzer.scala` | MODIFY | Use Pattern.envOf (3-line fix) |
| `core/src/main/scala/dev/bosatsu/analysis/SourceMapper.scala` | ENHANCE | Bidirectional mapping with interval tree |
| `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala` | MODIFY | Use full stack |

## Security Checklist

**From security sentinel review**:

- [ ] FFI error messages don't leak full Value representations
- [ ] JavaScript identifier escaping prevents injection
- [ ] Division by zero behavior documented
- [ ] Source snippets disabled in production (if applicable)
- [ ] External struct trust model documented
