package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Gen}

class ListUtilTest extends AnyFunSuite {

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 5000)

  def genNEL[A](ga: Gen[A]): Gen[NonEmptyList[A]] =
    Gen.sized { sz =>
      if (sz <= 1) ga.map(NonEmptyList.one)
      else
        Gen.zip(ga, Gen.listOfN(sz - 1, ga)).map { case (h, t) =>
          NonEmptyList(h, t)
        }
    }

  implicit def arbNEL[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(genNEL(Arbitrary.arbitrary[A]))

  test("unit group has 1 item") {
    forAll { (nel: NonEmptyList[Int]) =>
      val unit = ListUtil.greedyGroup(nel)(_ => ())((_, _) => Some(()))
      assert(unit == NonEmptyList.one(()))
    }
  }

  test("groups satisfy edge property") {
    forAll { (nel: NonEmptyList[Int], accept: (Int, Int) => Boolean) =>
      val groups = ListUtil.greedyGroup(nel)(a => Set(a))((s, i) =>
        if (s.forall(accept(_, i))) Some(s + i) else None
      )
      groups.toList.foreach { g =>
        val items = g.toList.zipWithIndex
        for {
          (i1, idx1) <- items
          (i2, idx2) <- items
        } assert((idx1 == idx2) || accept(i1, i2) || accept(i2, i1))
      }
    }
  }

  test("there are as most as many groups as inputs") {
    forAll {
      (
          nel: NonEmptyList[Int],
          one: Int => Int,
          accept: (Int, Int) => Option[Int]
      ) =>
        val groups = ListUtil.greedyGroup(nel)(one)(accept)
        assert(groups.length <= nel.length)
    }
  }

  test("if we always accept there is one group") {
    forAll { (nel: NonEmptyList[Int], one: Int => Int) =>
      val groups = ListUtil.greedyGroup(nel)(one)((x, y) => Some(x + y))
      assert(groups.length == 1)
    }
  }

  test("if we never accept there are as many groups as came in") {
    forAll { (nel: NonEmptyList[Int], one: Int => Int) =>
      val groups = ListUtil.greedyGroup(nel)(one)((_, _) => None)
      assert(groups.length == nel.length)
    }
  }

  test("groups direct property") {
    forAll { (nel: NonEmptyList[Int], accept: (List[Int], Int) => Boolean) =>
      val groups = ListUtil.greedyGroup(nel)(a => a :: Nil)((s, i) =>
        if (accept(s, i)) Some(i :: s) else None
      )
      groups.toList.foreach { g =>
        def check(g: List[Int]): Unit =
          g match {
            case Nil      => fail("expected at least one item")
            case _ :: Nil =>
              // this can always happen
              ()
            case head :: tail =>
              assert(accept(tail, head))
              check(tail)
          }

        check(g)
      }
    }
  }

  test("distinctByHashSet works like List.distinct") {
    forAll { (nel: NonEmptyList[Byte]) =>
      val asList = nel.toList.distinct
      val viaFn = ListUtil.distinctByHashSet(nel).toList

      assert(viaFn == asList)
    }
  }
}
