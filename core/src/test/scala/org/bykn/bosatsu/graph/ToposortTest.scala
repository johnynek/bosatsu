package org.bykn.bosatsu.graph

import cats.Order
import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite

class ToposortTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 1000)

  test("toposort can recover full sort") {
    def law[A: Order](items: Iterable[A]) = {
      def neighbor(a: A): List[A] =
        items
          .filter(Order[A].lt(_, a))
          .reduceLeftOption(Order[A].max(_, _))
          .toList

      val res = Toposort.sort(items)(neighbor(_))
      assert(res.isSuccess)
      assert(res.isFailure == res.loopNodes.nonEmpty)
      assert(res.toSuccess == Some(res.layers))
      assert(
        res.layers == items.toVector
          .sorted(Order[A].toOrdering)
          .map(NonEmptyList(_, Nil))
      )
      assert(res.layersAreTotalOrder)
    }

    forAll { (ints: List[Int]) => law(ints.distinct) }
    forAll { (strings: List[String]) => law(strings.distinct) }
  }

  /*
   * There can't be any edges from layer x into layer y if y >= x
   */
  def noEdgesToLater[A](layers: Vector[NonEmptyList[A]])(fn: A => List[A]) = {
    layers.zipWithIndex.foreach { case (layer, id) =>
      // we can't point to any nodes in our layer or later
      layer.foldMap { n =>
        val nset = fn(n).toSet
        if (nset.nonEmpty) {
          (id until layers.size).foreach { id1 =>
            assert(
              layers(id1).filter(nset).isEmpty,
              s"node $n in layer $id has points to later layers: $id1"
            )
          }
        }
      }
    }
  }

  def layersAreSorted[A: Order](layers: Vector[NonEmptyList[A]]) =
    layers.foreach { layer => assert(layer.sorted == layer) }

  test("we can sort general dags") {
    case class Dag(graph: Map[Int, List[Int]])

    val nid = Gen.choose(0, 100)
    val pair = for {
      n <- nid
      neighbor <- Gen
        .listOf(nid)
        .map(_.filter(_ < n).distinct) // make sure it is a dag
    } yield (n, neighbor)

    val genDag = Gen.mapOf(pair).map(Dag(_))
    forAll(genDag) { case Dag(graph) =>
      val allNodes = graph.flatMap { case (h, t) => h :: t }.toSet
      val Toposort.Success(sorted, _) =
        Toposort.sort(allNodes)(graph.getOrElse(_, Nil))
      assert(sorted.flatMap(_.toList).sorted == allNodes.toList.sorted)
      noEdgesToLater(sorted)(n => graph.getOrElse(n, Nil))
      layersAreSorted(sorted)
    }
  }

  test("good nodes obey the layer law, sets of nodes are distinct") {
    val nid = Gen.choose(0, 100)
    val pair = for {
      n <- nid
      neighbor <- Gen.listOf(nid).map(_.distinct)
    } yield (n, neighbor)
    forAll(Gen.mapOf(pair)) { graph =>
      val allNodes = graph.flatMap { case (h, t) => h :: t }.toSet
      val fn = graph.getOrElse(_: Int, Nil)
      val res = Toposort.sort(allNodes)(fn)
      val layers = res.layers
      noEdgesToLater(layers)(fn)
      layersAreSorted(layers)
      // all the nodes is the same set:
      val goodNodes = layers.flatMap(_.toList)
      assert(
        (goodNodes.toList ::: res.loopNodes).sorted == allNodes.toList.sorted
      )
      // good nodes are distinct
      assert(goodNodes == goodNodes.distinct)
      // loop nodes are distinct
      assert(res.loopNodes == res.loopNodes.distinct)
      // no nodes is good and bad
      assert((res.loopNodes.toSet & goodNodes.toSet).isEmpty)
      // loop nodes are sorted
      assert(res.loopNodes.sorted == res.loopNodes)
      assert(res.isFailure == res.loopNodes.nonEmpty)
      assert(res.isSuccess == res.loopNodes.isEmpty)
      assert(res.toSuccess == (if (res.isSuccess) Some(res.layers) else None))
    }
  }

  test("we return the least node with a loop") {
    assert(
      Toposort.sort(List(1, 2))(Function.const(List(1, 2))) == Toposort.Failure(
        List(1, 2),
        Vector.empty
      )
    )
    assert(
      Toposort.sort(List("bb", "aa"))(
        Function.const(List("aa", "bb"))
      ) == Toposort.Failure(List("aa", "bb"), Vector.empty)
    )
  }
}
