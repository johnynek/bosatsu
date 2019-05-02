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
}
