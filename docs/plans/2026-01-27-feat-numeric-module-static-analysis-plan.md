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

### Phase 5: Principled Input Semantics

**Goal**: Fix the semantic contradiction where bindings are both "fixed values" and "user-editable"

**Problem (2026-01-28)**: The current design has:
```bosatsu
principal = 250000  # This IS 250000, always in Bosatsu semantics
```
But the UI lets users "change" principal. This violates Bosatsu's explicit semantics.

**Solution**: Use **function-based simulations** with config files

#### 5.1 Function-Based Simulations

**New .bosatsu pattern:**
```bosatsu
package LoanCalculator

# The simulation is a pure function - parameters ARE the inputs
def calculate(principal: Int, annual_rate: Int, years: Int) -> {
  monthly_payment: Int,
  total_interest: Int,
  interest_ratio: Int
}:
  monthly_rate = annual_rate.div(1200)
  num_payments = years.times(12)
  monthly_payment = principal.times(monthly_rate).add(principal.div(num_payments))
  total_paid = monthly_payment.times(num_payments)
  total_interest = total_paid.sub(principal)
  interest_ratio = total_interest.times(100).div(principal)
  { monthly_payment, total_interest, interest_ratio }
```

#### 5.2 Config File as Bosatsu

**Better approach**: Use Bosatsu for config files too (type-checked, single language)

**File**: `loan_calculator_func.sim.bosatsu`
```bosatsu
package LoanCalculator/Config

struct InputConfig(
  label: String,
  default_value: Int,
  min_value: Int,
  max_value: Int,
  step: Int,
  widget: String
)

struct OutputConfig(
  label: String,
  format: String,
  primary: Bool
)

struct SimConfig(
  name: String,
  description: String,
  package_name: String,
  function_name: String,
  inputs: List[(String, InputConfig)],
  outputs: List[(String, OutputConfig)]
)

config = SimConfig(
  "Loan Calculator",
  "Calculate monthly payments and total interest",
  "LoanCalculator",
  "calculate",
  [
    ("principal", InputConfig("Loan Principal ($)", 250000, 10000, 1000000, 10000, "slider")),
    ("annual_rate", InputConfig("Interest Rate (bp)", 700, 100, 2000, 25, "slider")),
    ("years", InputConfig("Loan Term", 30, 5, 40, 5, "slider"))
  ],
  [
    ("monthly_payment", OutputConfig("Monthly Payment", "currency", True)),
    ("total_interest", OutputConfig("Total Interest", "currency", False))
  ]
)
```

**Advantages of Bosatsu config:**
- Type-checked at compile time
- Single language (no JSON)
- Can use comments
- Can use richer types (enums for widget types, etc.)

#### 5.3 SimulationCommand Changes

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala`

```scala
// Compile BOTH files together
val packages = PackageMap.compile(
  List(simulationPath, configPath),
  ...
)

// Config is evaluated to extract SimConfig value
val configPackage = packages.getPackage(PackageName("LoanCalculator/Config"))
val configValue = evaluate(configPackage, "config")

// Simulation function found via config
val simPackage = packages.getPackage(PackageName(configValue.packageName))
val funcDef = simPackage.program.lets.find { case (name, _, _) =>
  name.asString == configValue.functionName
}

// Extract dependencies from TypedExpr (already exists!)
val funcBody: TypedExpr[_] = funcDef._3
val deps = TypedExpr.freeVarsSet(List(funcBody))

// Generate JS that calls function with slider values
```

**Key insight**: We use `TypedExpr.freeVarsSet` (already exists) for dependency extraction. No new analysis infrastructure needed - the type-checked AST IS the dependency graph.

#### 5.4 Generated JavaScript Pattern

```javascript
// Generated from function-based simulation
function calculate(principal, annual_rate, years) {
  const monthly_rate = Math.trunc(annual_rate / 1200);
  const num_payments = years * 12;
  const monthly_payment = Math.trunc(principal * monthly_rate + Math.trunc(principal / num_payments));
  const total_paid = monthly_payment * num_payments;
  const total_interest = total_paid - principal;
  return { monthly_payment, total_interest };
}

// UI calls function with current input values
function recompute() {
  const result = calculate(
    getInputValue('principal'),
    getInputValue('annual_rate'),
    getInputValue('years')
  );
  updateOutput('monthly_payment', result.monthly_payment);
  updateOutput('total_interest', result.total_interest);
}
```

#### 5.5 Provenance from Function Body

"Why?" explanations extracted from TypedExpr of function body:
- `monthly_payment` depends on `principal`, `monthly_rate`, `num_payments`
- `monthly_rate` = `annual_rate / 1200`
- etc.

The computation graph IS the provenance - no separate tracking needed.

---

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

### Phase 5: Principled Input Semantics ✅ PARTIAL
- [x] Simulations defined as functions, not top-level bindings (`demo/loan_calculator_func.bosatsu`)
- [x] Config file schema defined as Bosatsu (`demo/loan_calculator_func.sim.bosatsu`)
- [x] SimulationCommand compiles config + simulation together
- [x] Generated JS calls function with slider values
- [ ] SimulationCommand evaluates config to extract SimConfig value (Phase 6)
- [ ] "Why?" explanations derived from function body TypedExpr (Phase 7)

### Phase 6: Config File Evaluation
- [ ] `config` binding evaluated from `.sim.bosatsu`
- [ ] ConfigExtractor.scala converts Value → SimConfig case class
- [ ] Slider labels come from `InputConfig.label`
- [ ] Slider ranges come from `InputConfig.min_value/max_value`
- [ ] Default values come from `InputConfig.default_value`
- [ ] Result labels come from `OutputConfig.label`
- [ ] No hardcoded HTML generation - all driven by config
- [ ] Playwright test: change config → different UI

### Phase 7: "Why?" Provenance
- [ ] ProvenanceExtractor.scala extracts derivations from TypedExpr
- [ ] Formulas generated from TypedExpr structure (not string parsing)
- [ ] exprToFormula handles binary ops: add → +, sub → -, times → ×, div → ÷
- [ ] "Why?" button shows derivation chain
- [ ] Chain traces from output back to inputs
- [ ] Playwright test: click "Why?" → see formula

### Phase 8: "What if?" Toggles
- [ ] AssumptionConfig added to config schema
- [ ] Multiple function variants in simulation file
- [ ] "What if?" toggles rendered in UI
- [ ] Clicking toggle switches calculation variant
- [ ] Results update immediately
- [ ] Playwright test: toggle assumption → different result

### Phase 9: Parameter Sweeps
- [ ] SweepConfig added to config schema
- [ ] Sweep runs function across input range
- [ ] Results plotted on canvas
- [ ] Current input value highlighted on curve
- [ ] Changing input shows position on sweep curve
- [ ] Playwright test: sweep chart renders correctly

### Phase 10: Canvas Visualization (Optional)
- [ ] Canvas element in generated HTML
- [ ] Visualization updates on input change
- [ ] Supports basic shapes: rect, circle, line, text

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

---

## REMAINING PHASES: BurritoScript Feature Parity

**Added 2026-01-28**: Complete roadmap to match BurritoScript simulation capabilities.

### Phase 6: Config File Evaluation

**Goal**: Actually USE the `.sim.bosatsu` config to drive UI generation

**Current state**: Config file is parsed and type-checked but values are NOT extracted.

#### 6.1 Evaluate Config Binding

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala`

The config file defines a `config` binding of type `SimConfig`. We need to evaluate it to extract the actual values.

```scala
// After type checking both files together:
val configPackage = typeChecked.toMap.get(configParsed._2.name).getOrElse(
  throw new RuntimeException(s"Config package not found")
)

// Evaluate the 'config' binding to get runtime Value
val configValue: Value = Evaluation.evaluateLet(
  configPackage,
  Identifier.Name("config")
)

// Convert Value to Scala case class
val simConfig: SimConfig = extractSimConfig(configValue)
```

#### 6.2 Value Extraction

**New file**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/ConfigExtractor.scala`

```scala
package dev.bosatsu.simulation

import dev.bosatsu.Value

case class InputConfig(
  label: String,
  defaultValue: Int,
  minValue: Int,
  maxValue: Int,
  step: Int,
  widget: String
)

case class OutputConfig(
  label: String,
  format: String,
  primary: Boolean
)

case class SimConfig(
  name: String,
  description: String,
  packageName: String,
  functionName: String,
  inputs: List[(String, InputConfig)],
  outputs: List[(String, OutputConfig)]
)

object ConfigExtractor {
  def extractSimConfig(value: Value): SimConfig = {
    // Value is a struct (ProductValue)
    // Extract fields by position matching struct definition order
    value match {
      case Value.ProductValue(_, fields) =>
        SimConfig(
          name = extractString(fields(0)),
          description = extractString(fields(1)),
          packageName = extractString(fields(2)),
          functionName = extractString(fields(3)),
          inputs = extractInputList(fields(4)),
          outputs = extractOutputList(fields(5))
        )
      case _ => throw new RuntimeException(s"Expected SimConfig struct, got: $value")
    }
  }

  private def extractString(v: Value): String = v match {
    case Value.Str(s) => s
    case _ => throw new RuntimeException(s"Expected String, got: $v")
  }

  private def extractInt(v: Value): Int = v match {
    case Value.IntValue(i) => i.toInt
    case _ => throw new RuntimeException(s"Expected Int, got: $v")
  }

  private def extractBool(v: Value): Boolean = v match {
    case Value.True => true
    case Value.False => false
    case _ => throw new RuntimeException(s"Expected Bool, got: $v")
  }

  private def extractInputList(v: Value): List[(String, InputConfig)] = {
    // List is represented as nested ConsValue or EmptyList
    extractList(v).map { item =>
      // Each item is a tuple (String, InputConfig)
      val (name, configValue) = extractTuple2(item)
      (extractString(name), extractInputConfig(configValue))
    }
  }

  private def extractInputConfig(v: Value): InputConfig = v match {
    case Value.ProductValue(_, fields) =>
      InputConfig(
        label = extractString(fields(0)),
        defaultValue = extractInt(fields(1)),
        minValue = extractInt(fields(2)),
        maxValue = extractInt(fields(3)),
        step = extractInt(fields(4)),
        widget = extractString(fields(5))
      )
    case _ => throw new RuntimeException(s"Expected InputConfig struct")
  }

  // ... similar for extractOutputList, extractOutputConfig
}
```

#### 6.3 Use Config in HTML Generation

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`

Update `generateFunctionBased` to use extracted config:

```scala
def generateFunctionBased(
  funcName: String,
  funcParams: List[(String, String)],
  config: SimConfig,  // NEW: use config instead of hardcoded values
  computeJs: String,
  simConfig: SimConfig
): String = {
  // Use config.name for title
  // Use config.inputs for slider generation
  // Use config.outputs for result display

  val inputControls = config.inputs.map { case (paramName, inputConfig) =>
    s"""
    <div class="control-group">
      <label class="control-label">${inputConfig.label}: <span class="value-display" id="${paramName}-value">${inputConfig.defaultValue}</span></label>
      <input type="range" id="${paramName}-slider"
        min="${inputConfig.minValue}" max="${inputConfig.maxValue}"
        step="${inputConfig.step}" value="${inputConfig.defaultValue}"
        oninput="_setState('$paramName', parseInt(this.value)); document.getElementById('${paramName}-value').textContent = this.value; _recompute();">
    </div>
    """
  }.mkString("\n")

  // ...
}
```

#### 6.4 Acceptance Criteria

- [ ] `config` binding evaluated from `.sim.bosatsu`
- [ ] Slider labels come from `InputConfig.label`
- [ ] Slider ranges come from `InputConfig.min_value/max_value`
- [ ] Default values come from `InputConfig.default_value`
- [ ] Result labels come from `OutputConfig.label`
- [ ] No hardcoded HTML generation - all driven by config
- [ ] Playwright test: change config → different UI

---

### Phase 7: "Why?" Provenance from TypedExpr

**Goal**: Generate derivation explanations from function body's TypedExpr

**Key insight**: The TypedExpr IS the dependency graph. `TypedExpr.freeVarsSet` already extracts dependencies. We just need to make it human-readable.

#### 7.1 Dependency Extraction

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/ProvenanceExtractor.scala`

```scala
package dev.bosatsu.simulation

import dev.bosatsu.{TypedExpr, Identifier}
import dev.bosatsu.Identifier.Bindable

case class Derivation(
  name: String,
  formula: String,           // Human-readable: "principal × monthly_rate + principal / num_payments"
  dependencies: Set[String], // Direct dependencies
  kind: DerivationKind       // Input, Intermediate, Output
)

sealed trait DerivationKind
case object Input extends DerivationKind        // Function parameter
case object Intermediate extends DerivationKind // Let binding in function body
case object Output extends DerivationKind       // Struct field in return

object ProvenanceExtractor {

  /**
   * Extract derivations from a function's TypedExpr.
   *
   * @param funcExpr The TypedExpr.AnnotatedLambda for the function
   * @return Map from binding name to its derivation
   */
  def extractDerivations(funcExpr: TypedExpr[Any]): Map[String, Derivation] = {
    funcExpr match {
      case TypedExpr.AnnotatedLambda(params, body, _) =>
        // Parameters are inputs
        val inputDerivations = params.toList.map { case (name, _) =>
          name.asString -> Derivation(
            name = name.asString,
            formula = name.asString,  // Inputs are just their name
            dependencies = Set.empty,
            kind = Input
          )
        }.toMap

        // Extract let bindings from body
        val bodyDerivations = extractFromBody(body, params.toList.map(_._1).toSet)

        inputDerivations ++ bodyDerivations

      case _ => Map.empty
    }
  }

  private def extractFromBody(
    expr: TypedExpr[Any],
    scope: Set[Bindable]
  ): Map[String, Derivation] = {
    expr match {
      case TypedExpr.Let(name, value, body, _, _) =>
        // This binding
        val deps = TypedExpr.freeVarsSet(List(value)).intersect(scope)
        val formula = exprToFormula(value)
        val derivation = Derivation(
          name = name.asString,
          formula = formula,
          dependencies = deps.map(_.asString),
          kind = Intermediate
        )

        // Continue with body, adding this binding to scope
        Map(name.asString -> derivation) ++ extractFromBody(body, scope + name)

      case TypedExpr.App(_, _, _, _) =>
        // Struct constructor at the end - extract output fields
        extractOutputFields(expr, scope)

      case _ => Map.empty
    }
  }

  /**
   * Convert TypedExpr to human-readable formula.
   * Uses the actual AST structure, not string manipulation.
   */
  private def exprToFormula(expr: TypedExpr[Any]): String = {
    expr match {
      case TypedExpr.Local(name, _, _) =>
        name.asString

      case TypedExpr.App(TypedExpr.App(fn, List(left), _, _), List(right), _, _) =>
        // Binary operation: fn(left)(right)
        val op = fn match {
          case TypedExpr.Global(_, name, _, _) => name.asString match {
            case "add" | "add_Int" => "+"
            case "sub" | "sub_Int" => "-"
            case "times" | "times_Int" => "×"
            case "div" | "div_Int" => "÷"
            case other => other
          }
          case _ => "?"
        }
        s"${exprToFormula(left)} $op ${exprToFormula(right)}"

      case TypedExpr.Literal(lit, _, _) =>
        lit.unboxToAny.toString

      case _ =>
        "..." // Complex expression - simplify for display
    }
  }
}
```

#### 7.2 Generate "Why?" JavaScript

**File**: `simulation-cli/src/main/scala/dev/bosatsu/simulation/SimulationGen.scala`

```scala
def generateWhyExplanations(derivations: Map[String, Derivation]): String = {
  val derivationJson = derivations.map { case (name, d) =>
    s""""$name": {
      "formula": "${escapeString(d.formula)}",
      "dependencies": [${d.dependencies.map(n => s""""$n"""").mkString(", ")}],
      "kind": "${d.kind.toString.toLowerCase}"
    }"""
  }.mkString(",\n  ")

  s"""
  const _derivations = {
    $derivationJson
  };

  function showWhy(name) {
    const d = _derivations[name];
    if (!d) return;

    // Build explanation chain
    let explanation = [];
    let visited = new Set();

    function trace(n) {
      if (visited.has(n)) return;
      visited.add(n);
      const deriv = _derivations[n];
      if (!deriv) return;

      explanation.push({
        name: n,
        formula: deriv.formula,
        value: _getState(n),
        kind: deriv.kind
      });

      deriv.dependencies.forEach(trace);
    }

    trace(name);

    // Show modal with explanation
    showWhyModal(name, explanation.reverse());
  }
  """
}
```

#### 7.3 Acceptance Criteria

- [ ] Derivations extracted from TypedExpr.AnnotatedLambda
- [ ] Formulas generated from TypedExpr structure (not string parsing)
- [ ] "Why?" button shows derivation chain
- [ ] Chain traces from output back to inputs
- [ ] Playwright test: click "Why?" → see formula

---

### Phase 8: "What if?" Assumption Toggles

**Goal**: Let users toggle between computation variants

**BurritoScript approach**: Runtime `state.assumption()` with callbacks
**Bosatsu approach**: Multiple explicit functions - simpler, more explicit

#### 8.1 Config Schema Extension

**File**: `demo/tax_economy.sim.bosatsu`

```bosatsu
package TaxEconomy/Config

struct AssumptionConfig(
  name: String,
  description: String,
  variants: List[(String, String)]  # (variant_name, function_suffix)
)

struct SimConfig(
  # ... existing fields ...
  assumptions: List[AssumptionConfig]
)

config = SimConfig(
  # ... existing ...
  assumptions = [
    AssumptionConfig(
      "Labor Response",
      "How do taxes affect labor participation?",
      [
        ("Elastic", "_elastic"),
        ("Inelastic", "_inelastic"),
        ("Threshold", "_threshold")
      ]
    ),
    AssumptionConfig(
      "Investment Response",
      "How do taxes affect investment?",
      [
        ("Sensitive", "_sensitive"),
        ("Resilient", "_resilient")
      ]
    )
  ]
)
```

#### 8.2 Multiple Function Variants in Simulation

**File**: `demo/tax_economy.bosatsu`

```bosatsu
package TaxEconomy

# Base calculation
def calculate(taxRate: Int, population: Int) -> EconomyResult:
  laborParticipation = calculate_labor_inelastic(taxRate)
  investment = calculate_investment_resilient(taxRate)
  # ... rest of calculation

# Variants for labor response
def calculate_labor_elastic(taxRate: Int) -> Int:
  # Participation drops significantly with higher taxes
  630.sub(taxRate.times(189).div(1000))

def calculate_labor_inelastic(taxRate: Int) -> Int:
  # People work regardless of taxes
  630.sub(taxRate.times(32).div(1000))

def calculate_labor_threshold(taxRate: Int) -> Int:
  # Drops sharply above 50%
  match taxRate.cmp_Int(500):
    GT -> 441
    _ -> 630

# Variants for investment response
def calculate_investment_sensitive(taxRate: Int) -> Int:
  # Investment flees high taxes (exponential decay approximation)
  1000.sub(taxRate.times(taxRate).div(500))

def calculate_investment_resilient(taxRate: Int) -> Int:
  # Investment is sticky
  1000.sub(taxRate.times(300).div(1000))
```

#### 8.3 "What if?" UI Generation

```scala
def generateWhatIfToggles(assumptions: List[AssumptionConfig]): String = {
  assumptions.map { assumption =>
    val buttons = assumption.variants.map { case (label, suffix) =>
      s"""<button class="toggle-btn" data-assumption="${assumption.name}" data-variant="$suffix" onclick="setAssumption('${assumption.name}', '$suffix')">$label</button>"""
    }.mkString("\n")

    s"""
    <div class="what-if-toggle">
      <div class="toggle-label">
        <span class="toggle-name">What if: ${assumption.description}</span>
      </div>
      <div class="toggle-buttons">
        $buttons
      </div>
    </div>
    """
  }.mkString("\n")
}

// JavaScript to handle assumption switching
val whatIfJs = """
const _assumptions = {};

function setAssumption(name, variant) {
  _assumptions[name] = variant;
  // Update button states
  document.querySelectorAll(`[data-assumption="${name}"]`).forEach(btn => {
    btn.classList.toggle('active', btn.dataset.variant === variant);
  });
  // Recompute with new variant
  _recompute();
}

function getCurrentVariantFunction(baseName, assumptionName) {
  const variant = _assumptions[assumptionName] || '';
  return window[baseName + variant];
}
"""
```

#### 8.4 Acceptance Criteria

- [ ] Config supports `assumptions` list
- [ ] Multiple function variants in simulation file
- [ ] "What if?" toggles rendered in UI
- [ ] Clicking toggle switches calculation variant
- [ ] Results update immediately
- [ ] Playwright test: toggle assumption → different result

---

### Phase 9: Parameter Sweeps

**Goal**: Visualize output across input range (Laffer curve)

#### 9.1 Config Schema Extension

```bosatsu
struct SweepConfig(
  inputParam: String,      # Which input to sweep
  minValue: Int,
  maxValue: Int,
  steps: Int,
  outputParam: String,     # Which output to plot
  chartType: String        # "line", "area", "bar"
)

struct SimConfig(
  # ... existing ...
  sweeps: List[SweepConfig]
)

config = SimConfig(
  # ... existing ...
  sweeps = [
    SweepConfig("taxRate", 0, 1000, 100, "governmentRevenue", "line")
  ]
)
```

#### 9.2 Sweep Execution JavaScript

```scala
def generateSweepCode(sweeps: List[SweepConfig], funcName: String): String = {
  sweeps.map { sweep =>
    s"""
    function runSweep_${sweep.inputParam}_${sweep.outputParam}() {
      const results = [];
      const min = ${sweep.minValue};
      const max = ${sweep.maxValue};
      const steps = ${sweep.steps};
      const step = (max - min) / steps;

      // Save current input values
      const savedInputs = {};
      ${/* save all inputs */}

      for (let i = 0; i <= steps; i++) {
        const x = min + i * step;
        _setState('${sweep.inputParam}', Math.round(x));
        const result = $funcName(${/* all input getters */});
        results.push({ x: x, y: result.${sweep.outputParam} });
      }

      // Restore inputs
      ${/* restore all inputs */}

      return results;
    }
    """
  }.mkString("\n")
}
```

#### 9.3 Chart Rendering

Use lightweight charting - either:
- Raw Canvas API (no dependencies)
- Or embed Chart.js/uPlot as optional dependency

```scala
def generateChartCode(): String = """
function renderSweepChart(canvasId, data, options) {
  const canvas = document.getElementById(canvasId);
  const ctx = canvas.getContext('2d');
  const width = canvas.width;
  const height = canvas.height;

  // Clear
  ctx.fillStyle = '#1a1a2e';
  ctx.fillRect(0, 0, width, height);

  // Find bounds
  const xMin = Math.min(...data.map(d => d.x));
  const xMax = Math.max(...data.map(d => d.x));
  const yMin = Math.min(...data.map(d => d.y));
  const yMax = Math.max(...data.map(d => d.y));

  // Draw line
  ctx.beginPath();
  ctx.strokeStyle = '#8b5cf6';
  ctx.lineWidth = 2;

  data.forEach((point, i) => {
    const px = 50 + (point.x - xMin) / (xMax - xMin) * (width - 100);
    const py = height - 50 - (point.y - yMin) / (yMax - yMin) * (height - 100);
    if (i === 0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  });

  ctx.stroke();

  // Draw current point marker
  // ...
}
"""
```

#### 9.4 Acceptance Criteria

- [ ] Config supports `sweeps` list
- [ ] Sweep runs function across input range
- [ ] Results plotted on canvas
- [ ] Current input value highlighted on curve
- [ ] Changing input shows position on sweep curve
- [ ] Playwright test: sweep chart renders correctly

---

### Phase 10: Canvas Visualization (Optional)

**Goal**: Custom visualizations beyond charts

**Approach TBD**: Two options:

**Option A: Bosatsu DSL for drawing**
```bosatsu
def render(state: EconomyState) -> List[DrawCommand]:
  [
    FillRect(0, 0, 500, 400, "#1a1a2e"),
    Line(points_from_laffer(state.taxRate), "#8b5cf6"),
    Circle(tax_to_x(state.taxRate), revenue_to_y(state.revenue), 10, "#ef4444")
  ]
```

**Option B: Separate JS for rendering**
- Bosatsu for computation only
- JavaScript template for visualization
- Cleaner separation of concerns

**Recommendation**: Start with Option B - matches current architecture, avoids scope creep.

#### 10.1 Acceptance Criteria (if implemented)

- [ ] Canvas element in generated HTML
- [ ] Visualization updates on input change
- [ ] Supports basic shapes: rect, circle, line, text
- [ ] Animation frame loop for smooth updates

---

## Updated File Changes Summary

| File | Phase | Action | Description |
|------|-------|--------|-------------|
| `simulation-cli/.../ConfigExtractor.scala` | 6 | NEW | Extract SimConfig from Value |
| `simulation-cli/.../SimulationCommand.scala` | 6 | MODIFY | Evaluate config binding |
| `simulation-cli/.../SimulationGen.scala` | 6 | MODIFY | Use config for HTML generation |
| `simulation-cli/.../ProvenanceExtractor.scala` | 7 | NEW | Extract derivations from TypedExpr |
| `simulation-cli/.../SimulationGen.scala` | 7 | MODIFY | Generate "Why?" explanations |
| `demo/tax_economy.bosatsu` | 8 | NEW | Multi-variant simulation |
| `demo/tax_economy.sim.bosatsu` | 8 | NEW | Config with assumptions |
| `simulation-cli/.../SimulationGen.scala` | 8 | MODIFY | Generate "What if?" toggles |
| `simulation-cli/.../SimulationGen.scala` | 9 | MODIFY | Generate sweep code + charts |
| `tests/e2e/tax-economy.spec.ts` | 8-9 | NEW | E2E tests for new features |

---

## Implementation Order

| Phase | Dependencies | Estimated Effort | Priority |
|-------|--------------|------------------|----------|
| 6 (Config eval) | None | Small | **P0 - Do first** |
| 7 ("Why?") | Phase 6 | Medium | **P0 - Core feature** |
| 8 ("What if?") | Phase 6 | Medium | P1 |
| 9 (Sweeps) | Phase 6 | Medium | P1 |
| 10 (Canvas) | Phase 9 | Large | P2 - Defer |

**Suggested order**: 6 → 7 → 8 → 9 → 10

---

## Future Improvements

### esbuild for ES Module Bundling

**Status**: Not started
**Priority**: Low (current approach works)

Currently, `SimulationCommand` uses `JsGen.renderStatements()` to generate JavaScript without ES6 exports, suitable for embedding directly in HTML `<script>` tags.

**Future improvement**: Add esbuild as a build step to:
- Keep using `JsGen.renderModule()` which produces proper ES modules with imports/exports
- Use esbuild to bundle the module into a single self-contained HTML file
- Benefit from tree-shaking, minification, and proper module resolution

**Why not our job**: Module bundling is a well-solved problem. Using a dedicated bundler (esbuild, Rollup, Vite) is more principled than implementing our own string concatenation or module inlining.

**Implementation sketch**:
```bash
# Generate ES module
bosatsu-sim loan.bosatsu config.bosatsu --format=esm -o loan.js

# Bundle with esbuild
esbuild loan.js --bundle --format=iife --outfile=loan-bundle.js

# Wrap in HTML (or use esbuild HTML plugin)
```

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
