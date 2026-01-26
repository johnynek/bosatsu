package dev.bosatsu.analysis

import dev.bosatsu.{Region, Identifier, Lit, TypedExpr, HasRegion}
import dev.bosatsu.rankn.Type
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

class DerivationGraphTest extends munit.ScalaCheckSuite {

  // Use Region as the tag type for test nodes
  given HasRegion[Region] = HasRegion.instance(identity)

  // Helper to create a literal node for testing
  def mkLiteral(id: Long, region: Region, value: Long): ProvenanceNode[Region] =
    ProvenanceNode(id, TypedExpr.Literal(Lit(value), Type.IntType, region))

  // Generators for property-based testing
  val regionGen: Gen[Region] = for {
    start <- Gen.choose(0, 1000)
    length <- Gen.choose(1, 100)
  } yield Region(start, start + length)

  val bindableGen: Gen[Identifier.Bindable] =
    Gen.alphaLowerStr.filter(_.nonEmpty).map(Identifier.Name(_))

  def literalNodeGen: Gen[ProvenanceNode[Region]] = for {
    id <- Gen.choose(0L, 10000L)
    region <- regionGen
    value <- Gen.choose(0L, 1000L)
  } yield mkLiteral(id, region, value)

  // Simple graph for testing
  def simpleGraph: DerivationGraph[Region] = {
    // Graph: 0 -> 1 -> 2, 0 -> 3
    val node0 = mkLiteral(0L, Region(0, 10), 100)
    val node1 = mkLiteral(1L, Region(10, 20), 50)
    val node2 = mkLiteral(2L, Region(20, 30), 10)
    val node3 = mkLiteral(3L, Region(30, 40), 20)

    DerivationGraph(
      nodes = Map(0L -> node0, 1L -> node1, 2L -> node2, 3L -> node3),
      dependencies = Map(0L -> Set(1L, 3L), 1L -> Set(2L)),
      usages = Map(1L -> Set(0L), 2L -> Set(1L), 3L -> Set(0L)),
      roots = Set(0L)
    )
  }

  test("empty graph has no nodes") {
    assertEquals(DerivationGraph.empty[Region].nodes.size, 0)
    assertEquals(DerivationGraph.empty[Region].dependencies.size, 0)
    assertEquals(DerivationGraph.empty[Region].usages.size, 0)
  }

  test("directDependencies returns correct dependencies") {
    val g = simpleGraph
    assertEquals(g.directDependencies(0L), Set(1L, 3L))
    assertEquals(g.directDependencies(1L), Set(2L))
    assertEquals(g.directDependencies(2L), Set.empty[Long])
    assertEquals(g.directDependencies(3L), Set.empty[Long])
  }

  test("directUsages returns correct usages") {
    val g = simpleGraph
    assertEquals(g.directUsages(0L), Set.empty[Long])
    assertEquals(g.directUsages(1L), Set(0L))
    assertEquals(g.directUsages(2L), Set(1L))
    assertEquals(g.directUsages(3L), Set(0L))
  }

  test("allDependencies returns transitive dependencies") {
    val g = simpleGraph
    assertEquals(g.allDependencies(0L), Set(1L, 2L, 3L))
    assertEquals(g.allDependencies(1L), Set(2L))
    assertEquals(g.allDependencies(2L), Set.empty[Long])
  }

  test("allUsages returns transitive usages") {
    val g = simpleGraph
    assertEquals(g.allUsages(2L), Set(1L, 0L))
    assertEquals(g.allUsages(1L), Set(0L))
    assertEquals(g.allUsages(0L), Set.empty[Long])
  }

  test("derivationChain returns correct chain") {
    val g = simpleGraph
    val chain = g.derivationChain(0L)

    // Should include all nodes in the derivation
    val nodeIds = chain.map(_._1).toSet
    assertEquals(nodeIds, Set(0L, 1L, 2L, 3L))

    // Root should be at depth 0
    assert(chain.exists { case (id, depth) => id == 0L && depth == 0 })

    // Direct dependencies at depth 1
    assert(chain.exists { case (id, depth) => id == 1L && depth == 1 })
    assert(chain.exists { case (id, depth) => id == 3L && depth == 1 })

    // Transitive dependency at depth 2
    assert(chain.exists { case (id, depth) => id == 2L && depth == 2 })
  }

  test("findRoots returns nodes with no usages") {
    val g = simpleGraph
    assertEquals(g.findRoots, Set(0L))
  }

  test("findLeaves returns nodes with no dependencies") {
    val g = simpleGraph
    assertEquals(g.findLeaves, Set(2L, 3L))
  }

  test("graph merge combines all elements") {
    val g1 = DerivationGraph[Region](
      nodes = Map(0L -> mkLiteral(0L, Region(0, 1), 1)),
      dependencies = Map(0L -> Set(1L)),
      usages = Map(1L -> Set(0L)),
      roots = Set(0L)
    )
    val g2 = DerivationGraph[Region](
      nodes = Map(1L -> mkLiteral(1L, Region(1, 2), 2)),
      dependencies = Map.empty,
      usages = Map.empty,
      roots = Set(1L)
    )

    val merged = g1 ++ g2
    assertEquals(merged.nodes.size, 2)
    assertEquals(merged.roots, Set(0L, 1L))
    assertEquals(merged.directDependencies(0L), Set(1L))
  }

  test("reachableFrom filters to reachable nodes") {
    val g = simpleGraph
    val filtered = g.reachableFrom(Set(1L))

    assertEquals(filtered.nodes.keySet, Set(1L, 2L))
    assertEquals(filtered.directDependencies(1L), Set(2L))
    assert(!filtered.nodes.contains(0L))
    assert(!filtered.nodes.contains(3L))
  }

  // Property-based tests

  property("builder creates consistent graph") {
    forAll(Gen.listOf(literalNodeGen)) { nodes =>
      val builder = new DerivationGraph.Builder[Region]
      nodes.foreach { n =>
        val id = builder.freshId()
        val node = ProvenanceNode(id, n.expr)
        builder.addNode(node)
      }
      val graph = builder.build()

      // All nodes should be present
      graph.nodes.size == nodes.size
    }
  }

  property("dependencies and usages are consistent") {
    forAll(Gen.choose(1, 10)) { n =>
      val builder = new DerivationGraph.Builder[Region]

      // Create n nodes in a chain: 0 -> 1 -> 2 -> ... -> n-1
      val ids = (0 until n).map { i =>
        val id = builder.freshId()
        builder.addNode(mkLiteral(id, Region(0, 1), i.toLong))
        id
      }

      // Add chain dependencies
      ids.sliding(2).foreach {
        case Seq(from, to) => builder.addDependency(from, to)
        case _             => ()
      }

      val graph = builder.build()

      // Check consistency: if A depends on B, then B is used by A
      graph.dependencies.forall { case (from, tos) =>
        tos.forall { to =>
          graph.usages.getOrElse(to, Set.empty).contains(from)
        }
      }
    }
  }

  property("allDependencies is a superset of directDependencies") {
    val g = simpleGraph
    forAll(Gen.oneOf(g.nodes.keys.toSeq)) { id =>
      val direct = g.directDependencies(id)
      val all = g.allDependencies(id)
      direct.subsetOf(all)
    }
  }

  property("derivationChain includes the starting node at depth 0") {
    val g = simpleGraph
    forAll(Gen.oneOf(g.nodes.keys.toSeq)) { id =>
      val chain = g.derivationChain(id)
      chain.exists { case (nodeId, depth) => nodeId == id && depth == 0 }
    }
  }

  property("derivationChain depths are non-negative and increase") {
    val g = simpleGraph
    forAll(Gen.oneOf(g.nodes.keys.toSeq)) { id =>
      val chain = g.derivationChain(id)
      val depths = chain.map(_._2)
      depths.forall(_ >= 0) && depths.sliding(2).forall {
        case Seq(a, b) => b >= a
        case _         => true
      }
    }
  }
}
