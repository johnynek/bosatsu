package dev.bosatsu

import cats.{Eq, Order}
import munit.Assertions._

object OrderingLaws {
  def law[A: Ordering](a: A, b: A, c: A): Unit = {
    val ord = implicitly[Ordering[A]]

    if (ord.lteq(a, b) && ord.lteq(b, c)) assert(ord.lteq(a, c), s"$a $b $c")
    if (ord.gteq(a, b) && ord.gteq(b, c)) assert(ord.gteq(a, c), s"$a $b $c")
    if (ord.lt(a, b) && ord.lt(b, c)) assert(ord.lt(a, c), s"$a $b $c")
    if (ord.gt(a, b) && ord.gt(b, c)) assert(ord.gt(a, c), s"$a $b $c")
    if (ord.equiv(a, b) && ord.equiv(b, c)) assert(ord.equiv(a, c), s"$a $b $c")

    val eqA = Eq.fromUniversalEquals[A]
    if (eqA.eqv(a, b)) assert(ord.equiv(a, b))
    if (ord.lteq(a, b) && ord.gteq(a, b)) assert(ord.equiv(a, b))

    assertEquals(ord.lt(a, b), !ord.gteq(a, b))
    assertEquals(ord.lt(a, b), (ord.compare(a, b) < 0))
    assertEquals(ord.lteq(a, b), (ord.lt(a, b) || ord.equiv(a, b)))
    assertEquals(ord.lteq(a, b), (ord.compare(a, b) <= 0))
    assertEquals(ord.gt(a, b), !ord.lteq(a, b))
    assertEquals(ord.gt(a, b), (ord.compare(a, b) > 0))
    assertEquals(ord.gteq(a, b), (ord.gt(a, b) || ord.equiv(a, b)))
    assertEquals(ord.gteq(a, b), (ord.compare(a, b) >= 0))
    assertEquals(ord.equiv(a, b), !(ord.lt(a, b) || ord.gt(a, b)))
    assertEquals(ord.equiv(a, b), (ord.compare(a, b) == 0))

    assertEquals(ord.lteq(a, b), ord.gteq(b, a))
    assertEquals(ord.lteq(a, c), ord.gteq(c, a))
    assertEquals(ord.lteq(b, c), ord.gteq(c, b))

    assert(ord.equiv(a, a))
    assert(ord.equiv(b, b))
    assert(ord.equiv(c, c))

    ()
  }

  def forOrder[A: Order](a: A, b: A, c: A) =
    law(a, b, c)(using Order[A].toOrdering)
}
