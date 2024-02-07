package org.bykn.bosatsu

import cats.Order
import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop.forAll

class FirstOrSecondTests extends munit.ScalaCheckSuite {

  implicit def arbFirstSec[A: Arbitrary, B: Arbitrary]: Arbitrary[FirstOrSecond[A, B]] =
    Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[A].map(FirstOrSecond.First(_)),
        Arbitrary.arbitrary[B].map(FirstOrSecond.Second(_))
      )
    )

  property("first or second ordering is lawful") {
    forAll { (f1: FirstOrSecond[Int, String], f2: FirstOrSecond[Int, String], f3: FirstOrSecond[Int, String]) =>
      OrderingLaws.forOrder(f1, f2, f3)  
    }
  }

  property("first < second") {
    forAll { (f1: FirstOrSecond[Int, String], f2: FirstOrSecond[Int, String]) =>
      (f1, f2) match {
        case (FirstOrSecond.First(_), FirstOrSecond.Second(_)) =>
          assert(Order[FirstOrSecond[Int, String]].lt(f1, f2))
        case (FirstOrSecond.Second(_), FirstOrSecond.First(_)) =>
          assert(Order[FirstOrSecond[Int, String]].gt(f1, f2))
        case _ => ()
      }
    }
  }
  property("someFirst is the reverse of the usual order") {
    forAll { (o1: Option[Byte], o2: Option[Byte]) =>
      val ord1 = Order[Option[Byte]].toOrdering.reverse  
      val ord2 = FirstOrSecond.orderFS(Order.fromOrdering(Ordering[Byte].reverse), Order[Unit])

      assertEquals(ord1.compare(o1, o2).signum,
        ord2.compare(FirstOrSecond.someFirst(o1), FirstOrSecond.someFirst(o2)).signum)
    }
  }
}