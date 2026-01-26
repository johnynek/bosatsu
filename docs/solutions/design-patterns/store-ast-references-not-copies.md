---
title: "Store AST References Instead of Copying Data in Wrapper Types"
date: 2026-01-25
category: design-patterns
tags:
  - immutable-data-structures
  - ast-handling
  - code-simplification
  - memory-efficiency
  - provenance-tracking
  - scala
module: bosatsu/analysis
symptoms:
  - Multiple case classes duplicating structure already present in source AST
  - Wrapper file growing with many case classes mirroring source variants
  - Boilerplate code for each AST node type
  - Maintenance burden when source changes require parallel wrapper updates
applies_to:
  - Scala
  - AST handling
  - Immutable data structures
  - Provenance/tracing systems
  - Compiler/interpreter internals
---

# Store AST References Instead of Copying Data in Wrapper Types

## Problem

When building analysis or tracing systems over an AST, it's tempting to create wrapper types that copy data from AST nodes:

```scala
// Anti-pattern: Copying data from immutable AST
sealed trait ProvenanceNode {
  def id: Long
  def region: Region
}

object ProvenanceNode {
  case class Literal(id: Long, region: Region, repr: String) extends ProvenanceNode
  case class LocalVar(id: Long, region: Region, name: Identifier, definedAt: Option[Long]) extends ProvenanceNode
  case class GlobalRef(id: Long, region: Region, packageName: String, name: Identifier) extends ProvenanceNode
  case class Application(id: Long, region: Region, functionId: Long, argumentIds: List[Long]) extends ProvenanceNode
  case class LetBinding(id: Long, region: Region, name: Identifier, boundId: Long, bodyId: Long) extends ProvenanceNode
  case class Lambda(id: Long, region: Region, params: List[Identifier], bodyId: Long) extends ProvenanceNode
  case class Match(id: Long, region: Region, scrutineeId: Long, branches: List[MatchBranch]) extends ProvenanceNode
  // ... 10+ case classes mirroring TypedExpr variants
}
```

**Result**: 159 lines of code duplicating structure already in `TypedExpr`.

## Symptoms

- Wrapper file has many case classes with similar structure to source AST
- Field names in wrapper match field names in source (e.g., both have `name`, `region`)
- Factory methods extract fields: `Wrapper(source.field1, source.field2, ...)`
- When source AST adds a variant, wrapper needs a new case class too

## Root Cause

**Misunderstanding that AST data needs to be copied.** When the source AST is immutable, references are completely safe. There's no mutation that could invalidate them.

## Solution

Store a single reference to the immutable AST node:

```scala
// Solution: Reference the immutable AST directly
final case class ProvenanceNode[T](
    id: Long,
    expr: TypedExpr[T]
) {
  /** Get the region from the expression's tag */
  def region(implicit hr: HasRegion[T]): Region = hr.region(expr.tag)

  /** Node type derived from the expression variant */
  def nodeType: String = expr match {
    case _: TypedExpr.Literal[_]         => "literal"
    case _: TypedExpr.Local[_]           => "local"
    case _: TypedExpr.Global[_]          => "global"
    case _: TypedExpr.App[_]             => "application"
    case _: TypedExpr.Let[_]             => "let"
    case _: TypedExpr.AnnotatedLambda[_] => "lambda"
    case _: TypedExpr.Match[_]           => "match"
    case _: TypedExpr.Generic[_]         => "generic"
    case _: TypedExpr.Annotation[_]      => "annotation"
  }
}
```

**Result**: 34 lines. 79% reduction.

## Key Insight

> "If you find yourself copying from the AST into a provenance trace, remember that the AST is immutable. You can always just store references to the AST."

When the source is immutable:
- **References are safe** - no mutation can invalidate them
- **Pattern matching extracts data on demand** - no need to pre-copy
- **Single wrapper class replaces entire hierarchy** - simpler API
- **Type information flows through generics** - use typeclasses like `HasRegion[T]`

## Files Changed

| File | Change |
|------|--------|
| `ProvenanceNode.scala` | 10+ case classes → single generic case class |
| `DerivationGraph.scala` | Added type parameter `[T]` |
| `ProvenanceAnalyzer.scala` | Creates `ProvenanceNode(id, expr)` directly |
| `SourceMapper.scala` | Extracts info via pattern matching on `node.expr` |
| Test files | Construct `TypedExpr` values for test data |

## Prevention Checklist

Before creating a wrapper type, verify:

- [ ] **Single Reference**: Does wrapper store only a reference to source?
- [ ] **No Field Duplication**: Are there fields copying data from source?
- [ ] **Source Immutable?**: If yes, copying is unnecessary
- [ ] **Accessor Methods**: Use `def` to extract data, not `val` to copy it

### Code Smell Indicators

```scala
// SMELL: Factory extracts fields then passes individually
def fromSource(src: Source): Wrapper =
  Wrapper(src.field1, src.field2, src.field3)

// SMELL: Wrapper fields have same names as source fields
case class Wrapper(name: String, count: Int)  // Source also has .name, .count

// SMELL: Collection fields stored from immutable source
case class Wrapper(items: List[Item])  // Why copy if source is immutable?
```

### Decision Rule

```
Is source immutable?
├── YES → Store reference only, add accessor methods
└── NO  → Is mutation a concern?
          ├── YES → Defensive copy may be justified
          └── NO  → Store reference only
```

## Related Patterns

- **Flyweight Pattern**: Share immutable data instead of copying
- **Delegation Pattern**: Forward method calls to wrapped object
- **HasRegion Typeclass**: Parametric extraction from generic tag type

## Cross-References

- `core/src/main/scala/dev/bosatsu/analysis/ProvenanceNode.scala` - Implementation
- `core/src/main/scala/dev/bosatsu/TypedExpr.scala` - The immutable AST being wrapped
- `core/src/main/scala/dev/bosatsu/Region.scala` - HasRegion typeclass definition
