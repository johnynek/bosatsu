package dev.bosatsu.analysis

import dev.bosatsu.{Region, LocationMap, Identifier}
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

class SourceMapperTest extends munit.ScalaCheckSuite {

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

    val node = ProvenanceNode.Literal(0L, Region(4, 6), "42")
    val deps = Set.empty[ProvenanceNode]

    val doc = mapper.formatNode(node, deps)
    val output = doc.render(80)

    assert(output.contains("[0]"), "Should include node ID")
    assert(output.contains("literal"), "Should include node type")
    assert(output.contains("1:"), "Should include line number")
  }

  test("formatNode includes dependencies") {
    val source = "x = 42\ny = x"
    val mapper = SourceMapper.fromSource(source)

    val literalNode = ProvenanceNode.Literal(0L, Region(4, 6), "42")
    val varNode = ProvenanceNode.LocalVar(
      1L,
      Region(11, 12),
      Identifier.Name("x"),
      Some(0L)
    )

    val doc = mapper.formatNode(varNode, Set(literalNode))
    val output = doc.render(80)

    assert(output.contains("Dependencies:"), "Should show dependencies header")
    assert(output.contains("[0]"), "Should reference dependency node")
  }

  test("formatDerivationChain shows depth correctly") {
    // Create a simple graph: node 0 depends on node 1
    val graph = DerivationGraph(
      nodes = Map(
        0L -> ProvenanceNode.LocalVar(0L, Region(0, 1), Identifier.Name("y"), Some(1L)),
        1L -> ProvenanceNode.Literal(1L, Region(4, 6), "42")
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
    val graph = DerivationGraph(
      nodes = Map(
        0L -> ProvenanceNode.Literal(0L, Region(0, 2), "42")
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
    val graph = DerivationGraph.empty
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
}
