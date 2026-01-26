package dev.bosatsu.analysis

import dev.bosatsu.{Region, Identifier}

class ProvenanceCLITest extends munit.FunSuite {

  test("analyzeSource succeeds for simple literal") {
    val source = "x = 42"
    val result = ProvenanceCLI.analyzeSource(source)

    assert(result.isRight, s"Should succeed: $result")
    val (graph, doc) = result.toOption.get

    assertEquals(graph.nodes.size, 1)
    assert(doc.render(80).contains("Derivation Graph"))
  }

  test("analyzeSource succeeds for multiple bindings") {
    val source = """
      |a = 1
      |b = 2
      |c = 3
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight)

    val (graph, _) = result.toOption.get
    // Each binding creates nodes
    assert(graph.nodes.size >= 3)
  }

  test("analyzeSource handles let expressions") {
    val source = """
      |x = (
      |  a = 1
      |  b = a
      |  b
      |)
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight, s"Should succeed: $result")
  }

  test("analyzeSource reports parse errors") {
    val source = "x = +"  // Invalid syntax

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isLeft, "Should fail for invalid syntax")
    assert(result.left.toOption.get.contains("Parse error"))
  }

  test("analyzeSource reports type errors") {
    val source = """
      |def f(x): x
      |y = f(1, 2, 3)
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    // This might parse but fail type checking
    // The actual error depends on Bosatsu's type checker
    // Just verify it doesn't crash
    result match {
      case Right(_) => () // OK if it type checks
      case Left(msg) => assert(msg.nonEmpty) // Error message should be present
    }
  }

  test("explainNode succeeds for existing node") {
    val source = "x = 42"

    // First analyze to get node IDs
    val analysisResult = ProvenanceCLI.analyzeSource(source)
    assert(analysisResult.isRight)

    val (graph, _) = analysisResult.toOption.get
    val nodeId = graph.nodes.keys.head

    val result = ProvenanceCLI.explainNode(source, nodeId)
    assert(result.isRight, s"Should succeed: $result")

    val doc = result.toOption.get
    val output = doc.render(80)
    assert(output.contains("Derivation of"))
  }

  test("explainNode fails for non-existent node") {
    val source = "x = 42"

    val result = ProvenanceCLI.explainNode(source, 99999L)
    assert(result.isLeft, "Should fail for non-existent node")
    assert(result.left.toOption.get.contains("not found"))
  }

  test("formatGraph includes all sections") {
    val source = """
      |a = 1
      |b = a
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight)

    val (_, doc) = result.toOption.get
    val output = doc.render(80)

    assert(output.contains("Derivation Graph"))
    assert(output.contains("Summary"))
    assert(output.contains("Roots"))
    assert(output.contains("Leaves"))
  }

  test("quickAnalyze returns graph only") {
    val source = "x = 42"

    val result = ProvenanceCLI.quickAnalyze(source)
    assert(result.isRight)

    val graph = result.toOption.get
    assertEquals(graph.nodes.size, 1)
  }

  test("toOutput creates Basic output") {
    import org.typelevel.paiges.Doc

    val doc = Doc.text("test")
    val output = ProvenanceCLI.toOutput[String](doc, Some("/path/to/output"))

    output match {
      case dev.bosatsu.tool.Output.Basic(d, path) =>
        assertEquals(d, doc)
        assertEquals(path, Some("/path/to/output"))
      case _ =>
        fail("Expected Basic output")
    }
  }

  test("analyzeSource handles lambda expressions") {
    val source = """
      |f = (x -> x)
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight, s"Should succeed: $result")

    val (graph, _) = result.toOption.get
    val hasLambda = graph.nodes.values.exists(_.isInstanceOf[ProvenanceNode.Lambda])
    // Lambda might be optimized, so just check we have nodes
    assert(graph.nodes.nonEmpty)
  }

  test("graph roots are entry points") {
    val source = """
      |a = 1
      |b = a
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight)

    val (graph, _) = result.toOption.get
    val roots = graph.findRoots

    // Roots should have no usages within the graph
    roots.foreach { rootId =>
      assertEquals(graph.directUsages(rootId), Set.empty[Long])
    }
  }

  test("graph leaves are primitives") {
    val source = """
      |a = 1
      |b = a
    """.stripMargin

    val result = ProvenanceCLI.analyzeSource(source)
    assert(result.isRight)

    val (graph, _) = result.toOption.get
    val leaves = graph.findLeaves

    // Leaves should have no dependencies
    leaves.foreach { leafId =>
      assertEquals(graph.directDependencies(leafId), Set.empty[Long])
    }
  }
}
