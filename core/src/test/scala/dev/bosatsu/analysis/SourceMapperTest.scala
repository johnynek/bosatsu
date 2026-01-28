package dev.bosatsu.analysis

import dev.bosatsu.{Region, LocationMap, Identifier, Lit, TypedExpr, HasRegion}
import dev.bosatsu.rankn.Type
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

class SourceMapperTest extends munit.ScalaCheckSuite {

  // Use Region as the tag type for test nodes
  given HasRegion[Region] = HasRegion.instance(identity)

  // Helper to create a literal node for testing
  def mkLiteral(id: Long, region: Region, value: Long): ProvenanceNode[Region] =
    ProvenanceNode(id, TypedExpr.Literal(Lit(value), Type.IntType, region))

  // Helper to create a local var node for testing
  def mkLocal(id: Long, region: Region, name: Identifier.Bindable): ProvenanceNode[Region] =
    ProvenanceNode(id, TypedExpr.Local(name, Type.IntType, region))

  test("locate returns correct line and column for single-line region") {
    val source = "x = 42"
    val mapper = SourceMapper.fromSource(source)

    // Region for "42" starts at position 4, ends at 6
    val region = Region(4, 6)
    val locOpt = mapper.locate(region)

    assert(locOpt.isDefined, "Should find location")
    val loc = locOpt.get

    assertEquals(loc.line, 1, "Should be line 1")
    assertEquals(loc.column, 5, "Column should be 5 (1-indexed)")
    assert(loc.isSingleLine, "Should be single line")
  }

  test("locate handles multi-line regions") {
    val source = """def foo(x):
                   |  x
                   |""".stripMargin
    val mapper = SourceMapper.fromSource(source)

    // Region spanning both lines
    val region = Region(0, source.length - 1)
    val locOpt = mapper.locate(region)

    assert(locOpt.isDefined, "Should find location")
    val loc = locOpt.get

    assertEquals(loc.line, 1, "Should start at line 1")
    assert(!loc.isSingleLine, "Should span multiple lines")
  }

  test("locate handles empty region at start of file") {
    val source = "hello"
    val mapper = SourceMapper.fromSource(source)

    val region = Region(0, 0)
    val locOpt = mapper.locate(region)

    assert(locOpt.isDefined, "Should find location for empty region")
    val loc = locOpt.get

    assertEquals(loc.line, 1)
    assertEquals(loc.column, 1)
  }

  test("locate returns None for out-of-bounds region") {
    val source = "short"
    val mapper = SourceMapper.fromSource(source)

    val region = Region(100, 200)
    val locOpt = mapper.locate(region)

    assert(locOpt.isEmpty, "Should return None for out-of-bounds region")
  }

  test("formatNode produces readable output") {
    val source = "x = 42"
    val mapper = SourceMapper.fromSource(source)

    val node = mkLiteral(0L, Region(4, 6), 42)
    val deps = Set.empty[ProvenanceNode[Region]]

    val doc = mapper.formatNode(node, deps)
    val output = doc.render(80)

    assert(output.contains("[0]"), "Should include node ID")
    assert(output.contains("literal"), "Should include node type")
    assert(output.contains("1:"), "Should include line number")
  }

  test("formatNode includes dependencies") {
    val source = "x = 42\ny = x"
    val mapper = SourceMapper.fromSource(source)

    val literalNode = mkLiteral(0L, Region(4, 6), 42)
    val varNode = mkLocal(1L, Region(11, 12), Identifier.Name("x"))

    val doc = mapper.formatNode(varNode, Set(literalNode))
    val output = doc.render(80)

    assert(output.contains("Dependencies:"), "Should show dependencies header")
    assert(output.contains("[0]"), "Should reference dependency node")
  }

  test("formatDerivationChain shows depth correctly") {
    // Create a simple graph: node 0 depends on node 1
    val graph = DerivationGraph[Region](
      nodes = Map(
        0L -> mkLocal(0L, Region(0, 1), Identifier.Name("y")),
        1L -> mkLiteral(1L, Region(4, 6), 42)
      ),
      dependencies = Map(0L -> Set(1L)),
      usages = Map(1L -> Set(0L)),
      roots = Set(0L)
    )

    val source = "y = 42"
    val mapper = SourceMapper.fromSource(source)

    val chain = graph.derivationChain(0L)
    val doc = mapper.formatDerivationChain(chain, graph)
    val output = doc.render(80)

    assert(output.contains("[0]"), "Should include node 0")
    assert(output.contains("[1]"), "Should include node 1")
    assert(output.contains("local"), "Should describe local var")
    assert(output.contains("literal"), "Should describe literal")
  }

  test("explainValue provides full explanation") {
    val graph = DerivationGraph[Region](
      nodes = Map(
        0L -> mkLiteral(0L, Region(0, 2), 42)
      ),
      dependencies = Map.empty,
      usages = Map.empty,
      roots = Set(0L)
    )

    val source = "42"
    val mapper = SourceMapper.fromSource(source)

    val doc = mapper.explainValue(0L, graph)
    val output = doc.render(80)

    assert(output.contains("Derivation of [0]"), "Should have header")
    assert(output.contains("Leaf values"), "Should mention leaf values")
  }

  test("explainValue handles unknown node") {
    val graph = DerivationGraph.empty[Region]
    val mapper = SourceMapper.fromSource("")

    val doc = mapper.explainValue(999L, graph)
    val output = doc.render(80)

    assert(output.contains("Unknown node"), "Should indicate unknown node")
  }

  // Property-based tests

  val regionGen: Gen[Region] = for {
    start <- Gen.choose(0, 100)
    length <- Gen.choose(1, 50)
  } yield Region(start, start + length)

  val sourceGen: Gen[String] = for {
    lines <- Gen.choose(1, 10)
    content <- Gen.listOfN(lines, Gen.alphaNumStr.map(s => if (s.isEmpty) "x" else s))
  } yield content.mkString("\n")

  property("locate always returns 1-indexed positions") {
    forAll(sourceGen, regionGen) { (source, region) =>
      val mapper = SourceMapper.fromSource(source)
      mapper.locate(region) match {
        case Some(loc) =>
          loc.line >= 1 && loc.column >= 1 &&
          loc.endLine >= 1 && loc.endColumn >= 1
        case None =>
          // Out of bounds is acceptable
          true
      }
    }
  }

  property("locate.endLine >= locate.line") {
    forAll(sourceGen, regionGen) { (source, region) =>
      val mapper = SourceMapper.fromSource(source)
      mapper.locate(region) match {
        case Some(loc) => loc.endLine >= loc.line
        case None      => true
      }
    }
  }

  property("isSingleLine is consistent with line numbers") {
    forAll(sourceGen, regionGen) { (source, region) =>
      val mapper = SourceMapper.fromSource(source)
      mapper.locate(region) match {
        case Some(loc) =>
          loc.isSingleLine == (loc.line == loc.endLine)
        case None => true
      }
    }
  }

  property("span format is valid") {
    forAll(sourceGen, regionGen) { (source, region) =>
      val mapper = SourceMapper.fromSource(source)
      mapper.locate(region) match {
        case Some(loc) =>
          val span = loc.span
          // Should contain at least one colon and numbers
          span.contains(":") && span.forall(c => c.isDigit || c == ':' || c == '-')
        case None => true
      }
    }
  }

  // Tests for ExpressionMapper

  test("ExpressionMapper.expressionText returns source substring") {
    val source = "let x = 42 in x"
    // Create a simple literal expression at position 8-10 (where "42" is)
    val literalExpr = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(8, 10))

    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)
    val textOpt = mapper.expressionText(literalExpr)

    assert(textOpt.isDefined, "Should find expression text")
    assertEquals(textOpt.get, "42")
  }

  test("ExpressionMapper.substring extracts correct region") {
    val source = "hello world"
    val literalExpr = TypedExpr.Literal[Region](Lit(0), Type.IntType, Region(0, 5))

    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    assertEquals(mapper.substring(Region(0, 5)), Some("hello"))
    assertEquals(mapper.substring(Region(6, 11)), Some("world"))
    assertEquals(mapper.substring(Region(100, 200)), None) // out of bounds
  }

  test("ExpressionMapper.expressionForRegion looks up by region") {
    val source = "x = 1"
    val region = Region(4, 5)
    val literalExpr = TypedExpr.Literal[Region](Lit(1), Type.IntType, region)

    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    assert(mapper.expressionForRegion(region).isDefined)
    assertEquals(mapper.expressionForRegion(region).get, literalExpr)
    assert(mapper.expressionForRegion(Region(0, 1)).isEmpty)
  }

  test("ExpressionMapper.expressionAt finds expression at position") {
    val source = "let x = 42 in x"
    // Position 9 is the '2' in '42'
    val literalExpr = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(8, 10))

    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    // Line 1, column 9 (1-indexed) should be in the literal
    val found = mapper.expressionAt(1, 9)

    assert(found.isDefined, "Should find expression at position")
    assertEquals(found.get, literalExpr)
  }

  test("ExpressionMapper indexes nested expressions") {
    val source = "let x = 42 in x"
    //           0123456789012345
    val inner = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(8, 10))
    val varRef = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(14, 15))
    val letExpr = TypedExpr.Let[Region](
      Identifier.Name("x"),
      inner,
      varRef,
      dev.bosatsu.RecursionKind.NonRecursive,
      Region(0, 15)
    )

    val mapper = ExpressionMapper.fromTypedExpr(letExpr, source)

    // Should find all three expressions
    assert(mapper.expressionForRegion(Region(0, 15)).isDefined, "Should find let")
    assert(mapper.expressionForRegion(Region(8, 10)).isDefined, "Should find literal")
    assert(mapper.expressionForRegion(Region(14, 15)).isDefined, "Should find var ref")
  }

  test("ExpressionMapper.expressionAt returns smallest containing expression") {
    val source = "let x = 42 in x"
    //           0123456789012345
    val inner = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(8, 10))
    val varRef = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(14, 15))
    val letExpr = TypedExpr.Let[Region](
      Identifier.Name("x"),
      inner,
      varRef,
      dev.bosatsu.RecursionKind.NonRecursive,
      Region(0, 15)
    )

    val mapper = ExpressionMapper.fromTypedExpr(letExpr, source)

    // At position 9 (the '2' in '42'), should find the literal (smallest)
    // not the enclosing let expression
    val found = mapper.expressionAt(1, 9)
    assert(found.isDefined, "Should find expression")
    assert(found.get.isInstanceOf[TypedExpr.Literal[?]], "Should find literal, not let")
  }

  // =========================================================================
  // SourceLocation tests
  // =========================================================================

  test("SourceLocation.toString includes span and snippet") {
    val source = "x = 42"
    val mapper = SourceMapper.fromSource(source)
    val locOpt = mapper.locate(Region(4, 6))

    assert(locOpt.isDefined)
    val loc = locOpt.get
    val str = loc.toString

    // toString should include the span and snippet
    assert(str.contains("["), "Should have bracket")
    assert(str.contains("]"), "Should have closing bracket")
    assert(str.contains(":"), "Should have colon for line:col format")
  }

  test("SourceLocation.span formats correctly for multi-line") {
    val source = "line1\nline2\nline3"
    val mapper = SourceMapper.fromSource(source)
    val locOpt = mapper.locate(Region(0, source.length - 1))

    assert(locOpt.isDefined)
    val loc = locOpt.get

    // Multi-line span should show both start and end
    val span = loc.span
    assert(!loc.isSingleLine, "Should be multi-line")
    assert(span.contains("-"), "Should have dash for range")
  }

  // =========================================================================
  // briefDescription tests for different TypedExpr types
  // =========================================================================

  test("briefDescription handles literal with long repr") {
    val source = "x"
    val mapper = SourceMapper.fromSource(source)
    // Create a literal with a long value that will be truncated
    val longLit = Lit.Str("this is a very long string that should be truncated")
    val node = ProvenanceNode(0L, TypedExpr.Literal(longLit, Type.StrType, Region(0, 1)))

    val desc = mapper.briefDescription(node)
    assert(desc.contains("literal:"), "Should indicate literal")
    assert(desc.contains("..."), "Should truncate long value")
  }

  test("briefDescription handles global reference") {
    val source = "x"
    val mapper = SourceMapper.fromSource(source)
    val globalExpr = TypedExpr.Global[Region](
      dev.bosatsu.PackageName.parts("Foo", "Bar"),
      Identifier.Name("baz"),
      Type.IntType,
      Region(0, 1)
    )
    val node = ProvenanceNode(0L, globalExpr)

    val desc = mapper.briefDescription(node)
    assert(desc.contains("global:"), "Should indicate global")
    assert(desc.contains("Foo/Bar"), "Should include package name")
    assert(desc.contains("baz"), "Should include identifier")
  }

  test("briefDescription handles application") {
    val source = "f(x)"
    val mapper = SourceMapper.fromSource(source)
    val fn = TypedExpr.Local[Region](Identifier.Name("f"), Type.IntType, Region(0, 1))
    val arg = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(2, 3))
    val appExpr = TypedExpr.App[Region](
      fn,
      cats.data.NonEmptyList.one(arg),
      Type.IntType,
      Region(0, 4)
    )
    val node = ProvenanceNode(0L, appExpr)

    val desc = mapper.briefDescription(node)
    assertEquals(desc, "application")
  }

  test("briefDescription handles let expression") {
    val source = "let x = 1 in x"
    val mapper = SourceMapper.fromSource(source)
    val inner = TypedExpr.Literal[Region](Lit(1), Type.IntType, Region(8, 9))
    val varRef = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(13, 14))
    val letExpr = TypedExpr.Let[Region](
      Identifier.Name("x"),
      inner,
      varRef,
      dev.bosatsu.RecursionKind.NonRecursive,
      Region(0, 14)
    )
    val node = ProvenanceNode(0L, letExpr)

    val desc = mapper.briefDescription(node)
    assert(desc.contains("let:"), "Should indicate let")
    assert(desc.contains("x"), "Should include binding name")
  }

  test("briefDescription handles lambda") {
    val source = "\\x -> x"
    val mapper = SourceMapper.fromSource(source)
    val body = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(6, 7))
    val lambdaExpr = TypedExpr.AnnotatedLambda[Region](
      cats.data.NonEmptyList.one((Identifier.Name("x"), Type.IntType)),
      body,
      Region(0, 7)
    )
    val node = ProvenanceNode(0L, lambdaExpr)

    val desc = mapper.briefDescription(node)
    assert(desc.contains("lambda"), "Should indicate lambda")
    assert(desc.contains("x"), "Should include argument name")
  }

  test("briefDescription handles generic") {
    val source = "x"
    val mapper = SourceMapper.fromSource(source)
    val inner = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(0, 1))
    val quantification = Type.Quantification.ForAll(
      cats.data.NonEmptyList.one((Type.Var.Bound("a"), dev.bosatsu.Kind.Type))
    )
    val genericExpr = TypedExpr.Generic[Region](quantification, inner)
    val node = ProvenanceNode(0L, genericExpr)

    val desc = mapper.briefDescription(node)
    assertEquals(desc, "generic")
  }

  test("briefDescription handles annotation") {
    val source = "x"
    val mapper = SourceMapper.fromSource(source)
    val inner = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(0, 1))
    val annExpr = TypedExpr.Annotation[Region](inner, Type.IntType)
    val node = ProvenanceNode(0L, annExpr)

    val desc = mapper.briefDescription(node)
    assert(desc.contains("annotation:"), "Should indicate annotation")
  }

  // =========================================================================
  // formatDerivationChain edge cases
  // =========================================================================

  test("formatDerivationChain handles missing nodes") {
    val graph = DerivationGraph.empty[Region]
    val source = "x"
    val mapper = SourceMapper.fromSource(source)

    // Pass a chain with node IDs that don't exist in the graph
    val chain = List((999L, 0), (1000L, 1))
    val doc = mapper.formatDerivationChain(chain, graph)
    val output = doc.render(80)

    // Should indicate unknown nodes
    assert(output.contains("[999]"), "Should reference node ID")
    assert(output.contains("unknown"), "Should indicate unknown node")
  }

  // =========================================================================
  // ExpressionMapper edge cases
  // =========================================================================

  test("ExpressionMapper.expressionAt handles out-of-bounds line") {
    val source = "x = 1"
    val literalExpr = TypedExpr.Literal[Region](Lit(1), Type.IntType, Region(4, 5))
    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    // Line 100 is way out of bounds
    val found = mapper.expressionAt(100, 1)
    assert(found.isEmpty, "Should return None for out-of-bounds line")
  }

  test("ExpressionMapper.locate delegates to SourceMapper") {
    val source = "x = 42"
    val literalExpr = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(4, 6))
    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    val locOpt = mapper.locate(literalExpr)
    assert(locOpt.isDefined, "Should find location")
    assertEquals(locOpt.get.line, 1)
  }

  test("ExpressionMapper.locate with region delegates correctly") {
    val source = "x = 42"
    val literalExpr = TypedExpr.Literal[Region](Lit(42), Type.IntType, Region(4, 6))
    val mapper = ExpressionMapper.fromTypedExpr(literalExpr, source)

    val locOpt = mapper.locate(Region(4, 6))
    assert(locOpt.isDefined, "Should find location for region")
  }

  test("ExpressionMapper handles nested expressions correctly") {
    val source = "match x: case 1: 2"
    //           0         1
    //           012345678901234567
    val arg = TypedExpr.Local[Region](Identifier.Name("x"), Type.IntType, Region(6, 7))
    val branchResult = TypedExpr.Literal[Region](Lit(2), Type.IntType, Region(17, 18))
    val pattern = (
      dev.bosatsu.Pattern.Literal(Lit(1)),
      branchResult
    )
    val matchExpr = TypedExpr.Match[Region](
      arg,
      cats.data.NonEmptyList.one(pattern),
      Region(0, 18)
    )

    val mapper = ExpressionMapper.fromTypedExpr(matchExpr, source)

    // Should index all three: match, arg, branchResult
    assert(mapper.expressionForRegion(Region(0, 18)).isDefined, "Should find match")
    assert(mapper.expressionForRegion(Region(6, 7)).isDefined, "Should find arg")
    assert(mapper.expressionForRegion(Region(17, 18)).isDefined, "Should find branch result")
  }
}
