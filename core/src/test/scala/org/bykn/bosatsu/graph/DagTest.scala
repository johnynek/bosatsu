package org.bykn.bosatsu.graph

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite
import org.bykn.bosatsu.MonadGen.genMonad
import org.bykn.bosatsu.ListOrdering
import scala.collection.immutable.SortedSet

import cats.syntax.all._

class DagTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 1000)

  val genDag: Gen[Map[Int, Set[Int]]] =
    Gen
      .listOf(Gen.choose(0, 1000000))
      .flatMap { nodes0 =>
        val nodes = nodes0.distinct.sorted
        val nodesIdx = nodes.zipWithIndex
        nodesIdx
          .traverse { case (n, idx) =>
            if (idx == 0) Gen.const((n, Set.empty[Int]))
            else {
              Gen
                .listOf(Gen.choose(0, idx - 1))
                .map(ns => (n, ns.map(nodes(_)).toSet))
            }
          }
          .map(_.to(Map))
      }

  val genGraph: Gen[Map[Int, Set[Int]]] =
    Gen
      .listOf(Gen.choose(0, 1000000))
      .flatMap { nodes0 =>
        val nodes = nodes0.distinct.sorted
        nodes
          .traverse { n =>
            Gen
              .listOf(Gen.oneOf(nodes))
              .map(ns => (n, ns.toSet))
          }
          .map(_.to(Map))
      }

  test("on dags, dagify makes singleton sets") {
    forAll(genDag) { dagMap =>
      val (mapping, dag) = Dag.dagify(dagMap.keys.toList)(dagMap(_))
      dag.nodes.foreach { cluster =>
        assert(cluster.size == 1)
      }
      dagMap.keys.foreach { n =>
        assert(mapping(n) == Some(SortedSet(n)))
      }
      assert(dag.unsingleton.isDefined)
    }
  }

  test(
    "we never crash, all nodes are in a cluster, cluster edges exist originally"
  ) {
    forAll(genGraph) { graph =>
      // don't crash
      val (mapping, dag) = Dag.dagify(graph.keys.toList)(graph(_))
      // all nodes are in a cluster
      graph.keys.foreach { n =>
        assert(mapping(n).isDefined)
      }
      // if we can't unsingleton, then there is at least one cluster with > 1
      if (dag.unsingleton.isEmpty) {
        assert(dag.nodes.exists(_.size > 1))
      }

      dag.nodes.foreach { cluster =>
        dag.deps(cluster).foreach { thatCluster =>
          // there must be at least one edge from cluster -> thatCluster
          assert(cluster.exists { n =>
            graph(n).exists(thatCluster)
          })
        }
      }
      // no node is in two clusters
      val clusterVec = dag.nodes.toVector.zipWithIndex
      for {
        (c1, idx) <- clusterVec
        (c2, idx2) <- (0 until idx).map(clusterVec(_))
      } assert((c1 & c2).isEmpty)

      // every node is in one cluster
      val allNodes = dag.nodes.foldLeft(SortedSet.empty[Int])(_ | _)
      assert(allNodes == graph.keys.to(SortedSet))

      // if we toposort a dag, we always succeed
      implicit val setOrd = ListOrdering.byIterator[SortedSet[Int], Int]
      val sortRes @ Toposort.Success(_) = Toposort.sort(dag.nodes) { n =>
        dag.deps(n).toList
      }
      assert(sortRes.isSuccess)
      sortRes.layers.zipWithIndex.foreach { case (nodes, layer) =>
        nodes.toList.foreach { n =>
          assert(dag.layerOf(n) == layer)
        }
      }
      assert(dag.layers == sortRes.layers.length)
      assert(sortRes.layers == dag.toToposorted.layers)

      // if we dagify again we get singletons:
      val (_, dag1) = Dag.dagify(dag.nodes)(dag.deps(_))
      assert(dag1.unsingleton == Some(dag))
    }
  }
}
