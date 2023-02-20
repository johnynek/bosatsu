package org.bykn.bosatsu

object ListOrdering {

  def onType[A](o: Ordering[A]): Ordering[List[A]] =
    apply(o)

  def apply[A: Ordering]: Ordering[List[A]] =
    new Ordering[List[A]] {
      val ordA = implicitly[Ordering[A]]
      def compare(a: List[A], b: List[A]): Int =
        (a, b) match {
          case (Nil, Nil) => 0
          case (h0 :: t0, h1 :: t1) =>
            val c = ordA.compare(h0, h1)
            if (c != 0) c else compare(t0, t1)
          case (_ :: _, Nil) => 1
          case (Nil, _ :: _) => -1
        }
    }

  def byIterator[C <: Iterable[A], A: Ordering]: Ordering[C] =
    new Ordering[C] {
      val ordA = implicitly[Ordering[A]]
      def compare(a: C, b: C): Int = {
        val itA = a.iterator
        val itB = b.iterator
        while (true) {
          (itA.hasNext, itB.hasNext) match {
            case (false, false) => return 0
            case (true, false) => return 1
            case (false, true) => return -1
            case (true, true) =>
              val c = ordA.compare(itA.next(), itB.next())
              if (c != 0) return c 
          }
        }
        return 0
      }
    }
}
