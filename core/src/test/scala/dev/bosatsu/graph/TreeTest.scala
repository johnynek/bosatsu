package dev.bosatsu.graph

import cats.data.{NonEmptyList, Validated}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import cats.implicits._

class TreeTest extends munit.ScalaCheckSuite {

  test("explicit dags never fail") {
    val dagFn: Gen[Int => List[Int]] =
      Gen.choose(1L, Long.MaxValue).map { seed =>
        val rng = new java.util.Random(seed)

        val cache = scala.collection.mutable.Map[Int, List[Int]]()

        { (node: Int) =>
          // the expected number of neighbors is 1.5, that means the graph is expected to be finite
          cache.getOrElseUpdate(
            node, {
              val count = rng.nextInt(3)
              (node + 1 until (node + count + 1)).toList.filter(_ > node)
            }
          )
        }

      }

    forAll(Gen.choose(0, Int.MaxValue), dagFn) { (start, nfn) =>
      Tree.dagToTree(start)(nfn) match {
        case v @ Validated.Valid(tree) =>
          // the neightbor function should give the same tree:
          val treeFn = Tree.neighborsFn(tree)
          val tree2 = Tree.dagToTree(tree.item)(treeFn)
          assertEquals(tree2, v)
          assert(Paths.allCycle0(start)(nfn).isEmpty)
        case Validated.Invalid(circs) =>
          fail(s"circular paths found: $circs")
      }
    }
  }

  test("circular graphs are invalid") {
    val prime = Gen.oneOf(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
      47, 53, 59, 61, 67, 71, 73, 79, 83, 89)

    val dagFn: Gen[(Int, Int => List[Int])] =
      for {
        p <- prime
        p1 = p - 1
        nodeGen = Gen.choose(0, p1)
        init <- nodeGen
        a <- Gen.choose(1, p1)
        b <- nodeGen
      } yield {

        (
          init,
          { (node: Int) =>
            // only 1 neighbor, but this is in a cyclic group so it can't be a dag
            List((node * a + b) % p)
          }
        )
      }

    forAll(dagFn) { case (start, nfn) =>
      // all the cycles should be the same
      val cycles = Paths.allCycle0(start)(nfn).map(_.sorted)
      assert(cycles.tail.forall(_ === cycles.head))

      def reachable(s: Set[Int]): Set[Int] = {
        val s1 = s ++ s.iterator.flatMap(nfn)
        if (s1 != s) reachable(s1)
        else s
      }

      val reached = Dag.transitiveSet(start :: Nil)(nfn)

      val expectReachable = reachable(Set(start)).toList.sorted
      assertEquals(reached.toList, expectReachable)
      assertEquals(expectReachable, cycles.head.toList)
      assert(Tree.dagToTree(start)(nfn).isInvalid)
    }
  }

  test("distinctBy matches distinct") {
    forAll { (h: Int, tail: List[Int]) =>
      val nel = NonEmptyList(h, tail)
      assertEquals(Tree.distinctBy(nel)(identity).toList, (h :: tail).distinct)
    }
  }

  test("if everything is the same, we keep the first item, in distinctBy") {
    forAll { (h: Int, tail: List[Int]) =>
      val nel = NonEmptyList(h, tail)
      assertEquals(Tree.distinctBy(nel)(Function.const(1)).toList, (h :: Nil))
    }
  }

  test("distinctBy concat law") {
    forAll { (h0: Int, tail0: List[Int], l1: List[Int]) =>
      val nel0 = NonEmptyList(h0, tail0)
      // filter all items in nel1 that are in nel0
      NonEmptyList.fromList(l1.filterNot(nel0.toList.toSet)) match {
        case None        => ()
        case Some(diffs) =>
          val got =
            Tree.distinctBy(nel0)(identity) ::: Tree.distinctBy(diffs)(identity)
          val expected = Tree.distinctBy(nel0 ::: diffs)(identity)
          assertEquals(got, expected)
      }
    }
  }
}
