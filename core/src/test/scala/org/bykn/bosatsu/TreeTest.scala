package org.bykn.bosatsu

import cats.data.Validated
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.forAll

class TreeTest extends FunSuite {

  test("explicit dags never fail") {
    val dagFn:  Gen[Int => List[Int]] =
      Gen.choose(1L, Long.MaxValue).map { seed =>

        val rng = new java.util.Random(seed)

        val cache = scala.collection.mutable.Map[Int, List[Int]]()

        { node: Int =>
          // the expected number of neighbors is 1.5, that means the graph is expected to be finite
          cache.getOrElseUpdate(node, {
            val count = rng.nextInt(3)
            (node + 1 until (node + count + 1)).toList.filter(_ > node)
          })
        }

      }

    forAll(Gen.choose(0, Int.MaxValue), dagFn) { (start, nfn) =>
      Tree.dagToTree(start)(nfn) match {
        case v@Validated.Valid(tree) =>
          // the neightbor function should give the same tree:
          val treeFn = Tree.neighborsFn(tree)
          val tree2 = Tree.dagToTree(tree.item)(treeFn)
          assert(tree2 == v)
        case Validated.Invalid(circs) =>
          fail(s"circular paths found: $circs")
      }
    }
  }

  test("circular graphs are invalid") {
    val prime = Gen.oneOf(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89)

    val dagFn:  Gen[(Int, Int => List[Int])] =
      for {
        p <- prime
        p1 = p - 1
        nodeGen = Gen.choose(0, p1)
        init <- nodeGen
        a <- Gen.choose(1, p1)
        b <- nodeGen
      } yield {

        (init, { node: Int =>
          // only 1 neighbor, but this is in a cyclic group so it can't be a dag
          List((node * a + b) % p)
        })
      }

    forAll(dagFn) { case (start, nfn) =>
      assert(Tree.dagToTree(start)(nfn).isInvalid)
    }
  }
}
