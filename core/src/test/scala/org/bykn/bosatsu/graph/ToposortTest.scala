package org.bykn.bosatsu.graph

import cats.Order
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.forAll

import cats.implicits._

class ToposortTest extends FunSuite {
  test("toposort can recover full sort") {
    def law[A: Order](items: Iterable[A]) = {
      def neighbor(a: A): List[A] =
        items
          .filter(Order[A].lt(_, a))
          .reduceLeftOption(Order[A].max(_, _))
          .toList

      assert(Toposort.sort(items)(neighbor(_)) == items.toList.sorted(Order[A].toOrdering).map(_ :: Nil))
    }

    forAll { ints: List[Int] => law(ints.distinct) }
    forAll { strings: List[String] => law(strings.distinct) }
  }

  test("we can sort general dags") {
    case class Dag(graph: Map[Int, List[Int]])

    val nid = Gen.choose(0, 100)
    val pair = for {
      n <- nid
      neighbor <- Gen.listOf(nid).map(_.filter(_ < n).distinct) // make sure it is a dag
    } yield (n, neighbor)

    val genDag = Gen.mapOf(pair).map(Dag(_))
    forAll(genDag) { case Dag(graph) =>
      val allNodes = graph.flatMap { case (h, t) => h :: t }.toSet
      val sorted = Toposort.sort(allNodes)(graph.getOrElse(_, Nil))
      assert(sorted.flatten.toSet == allNodes)
      sorted.foreach { xs =>
        assert(xs.sorted == xs)
      }
    }
  }
}
