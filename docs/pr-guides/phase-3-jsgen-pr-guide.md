# Phase 3: JavaScript Code Generation - PR Guide

## Overview

This PR implements JavaScript code generation for Bosatsu, enabling compilation of Bosatsu programs to JavaScript that can run in browsers or Node.js.

## Architecture

```
Bosatsu Source → TypedExpr → Matchless IR → JsGen → JavaScript Code
```

### Key Components

1. **Code.scala** - JavaScript AST types
2. **JsGen.scala** - Matchless to JavaScript transpiler
3. **JsTranspiler.scala** - Transpiler trait implementation for CLI integration

## Files Changed

### New Files

| File | Purpose |
|------|---------|
| `core/src/main/scala/dev/bosatsu/codegen/js/Code.scala` | JavaScript AST types (Expression, Statement, etc.) |
| `core/src/main/scala/dev/bosatsu/codegen/js/JsGen.scala` | Matchless to JavaScript transpiler |
| `core/src/main/scala/dev/bosatsu/codegen/js/JsTranspiler.scala` | Transpiler trait implementation |
| `core/src/test/scala/dev/bosatsu/codegen/js/CodeTest.scala` | Unit tests for Code.scala |
| `core/src/test/scala/dev/bosatsu/codegen/js/JsGenTest.scala` | Unit tests for JsGen.scala |
| `core/src/test/scala/dev/bosatsu/codegen/js/JsGenGen.scala` | ScalaCheck generators |
| `core/src/test/scala/dev/bosatsu/codegen/js/JsTranspilerTest.scala` | Integration tests |

### Modified Files

| File | Change |
|------|--------|
| `core/src/main/scala/dev/bosatsu/MainModule.scala` | Added JsTranspiler to transpiler options |
| `core/src/main/scala/dev/bosatsu/tool/Output.scala` | Added simulation output types |
| `core/src/main/scala/dev/bosatsu/simulation/SimulationCommand.scala` | Fixed sealed class extension issue |

## Code.scala - JavaScript AST

The Code AST provides a type-safe representation of JavaScript code:

### Expressions

- `Ident` - Identifiers
- `IntLiteral`, `DoubleLiteral`, `StringLiteral`, `BoolLiteral` - Literals
- `ArrowFunction`, `Function` - Function definitions
- `Call`, `NewExpr` - Function calls
- `PropertyAccess`, `IndexAccess` - Member access
- `ArrayLiteral`, `ObjectLiteral` - Compound literals
- `BinExpr`, `PrefixExpr`, `Ternary` - Operators

### Statements

- `Const`, `Let`, `Var` - Variable declarations
- `Assignment` - Assignment
- `Return` - Return statement
- `IfStatement`, `WhileLoop` - Control flow
- `Block` - Statement blocks
- `Export`, `Import` - ES modules

### Rendering

Each Code node has a `toDoc` method that produces paiges `Doc` for pretty printing:

```scala
val code = Code.Const("x", Code.IntLiteral(42))
Code.render(code) // "const x = 42;"
```

## JsGen.scala - Matchless Transpiler

### Environment Monad

Uses `cats.data.State` for tracking:
- Variable bindings
- Temporary variable generation
- Import tracking

```scala
type Env[A] = State[EnvState, A]

case class EnvState(
  bindings: Map[Identifier.Bindable, Code.Ident],
  tempCounter: Int
)
```

### Key Methods

- `exprToJs(expr: Matchless.Expr[A])` - Main expression converter
- `boolExprToJs(expr: Matchless.BoolExpr[A])` - Boolean expression converter
- `escape(bindable: Bindable)` - Identifier escaping for reserved words

### Data Representation

Bosatsu ADTs are represented as JavaScript arrays:

```javascript
// MakeEnum(variant=1, arity=2) produces:
(a, b) => [1, a, b]

// MakeStruct(arity=3) produces:
(a, b, c) => [a, b, c]

// Variant checking:
x[0] === 1  // Check if x is variant 1
```

### Natural Numbers

```javascript
// ZeroNat
0

// SuccNat
n => n + 1

// PrevNat(n)
n - 1
```

## JsTranspiler.scala - CLI Integration

### Usage

```bash
# Generate JavaScript as library (default)
bosatsu transpile js --outdir output/ -i input.bosatsu

# Generate with entry point
bosatsu transpile js --main MyPackage --outdir output/ -i input.bosatsu

# ES modules (default) or CommonJS
bosatsu transpile js --format esm --outdir output/ -i input.bosatsu
bosatsu transpile js --format cjs --outdir output/ -i input.bosatsu
```

### Output Structure

```
output/
├── PackageName/
│   └── index.js
└── main.js (if --main specified)
```

## Testing

### Test Coverage

- **CodeTest.scala**: 69 tests for JavaScript AST rendering
- **JsGenTest.scala**: 28 tests for Matchless transpilation
- **JsTranspilerTest.scala**: 13 integration tests
- **Total**: 110 tests

### Running Tests

```bash
# Run all JS codegen tests
sbt 'coreJVM / testOnly dev.bosatsu.codegen.js.*'

# Run specific test class
sbt 'coreJVM / testOnly dev.bosatsu.codegen.js.CodeTest'
```

### Property-Based Testing

Uses ScalaCheck generators in `JsGenGen.scala`:

- `genIdent` - Valid JavaScript identifiers
- `genExpression` - Arbitrary expressions (depth-bounded)
- `genStatement` - Arbitrary statements

## Design Decisions

### 1. Array-based ADT Representation

Chose arrays over objects for ADT representation:
- Simpler indexing (`[0]` vs `.tag`)
- Better minification
- Matches existing Bosatsu patterns

### 2. IIFE for Let Bindings

Let expressions use immediately-invoked function expressions:

```javascript
// Let(x, 42, body) produces:
(() => {
  const x = 42;
  return body;
})()
```

### 3. Reserved Word Escaping

JavaScript reserved words are prefixed with `_`:

```scala
escape(Name("class")) // Code.Ident("_class")
```

### 4. ES Modules Default

Default to ES modules for modern JavaScript compatibility. CommonJS available via `--format cjs`.

## Known Limitations

1. **No Runtime Library**: External functions need manual JS bindings
2. **No Source Maps**: Generated code has no source mapping
3. **No Tree Shaking**: All bindings in a package are emitted

## Future Work

- Optimization passes (constant folding, dead code elimination)
- Source map generation
- Runtime library for predef functions
- Node.js execution integration tests
