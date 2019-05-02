package org.bykn.bosatsu

import org.scalatest.Assertions._

object OrderingLaws {
  def law[A: Ordering](a: A, b: A, c: A): Unit = {
    val ord = implicitly[Ordering[A]]

    if (ord.lteq(a, b) && ord.lteq(b, c)) assert(ord.lteq(a, c), s"$a $b $c")
    if (ord.gteq(a, b) && ord.gteq(b, c)) assert(ord.gteq(a, c), s"$a $b $c")
    if (ord.lt(a, b) && ord.lt(b, c)) assert(ord.lt(a, c), s"$a $b $c")
    if (ord.gt(a, b) && ord.gt(b, c)) assert(ord.gt(a, c), s"$a $b $c")
    if (ord.equiv(a, b) && ord.equiv(b, c)) assert(ord.equiv(a, c), s"$a $b $c")

    if (a == b) assert(ord.equiv(a, b))

    assert(ord.lt(a, b) == !ord.gteq(a, b))
    assert(ord.gt(a, b) == !ord.lteq(a, b))
    assert(ord.equiv(a, b) == !(ord.lt(a, b) || ord.gt(a, b)))

    assert(ord.lteq(a, b) == ord.gteq(b, a))
    assert(ord.lteq(a, c) == ord.gteq(c, a))
    assert(ord.lteq(b, c) == ord.gteq(c, b))

    assert(ord.equiv(a, a))
    assert(ord.equiv(b, b))
    assert(ord.equiv(c, c))

    assert(!ord.lt(a, a))
    assert(!ord.lt(b, b))
    assert(!ord.lt(c, c))

    ()
  }
}
