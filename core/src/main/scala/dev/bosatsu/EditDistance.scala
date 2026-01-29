package dev.bosatsu

import Math.min
import cats.Eq
import cats.syntax.all._
// stolen from: https://gist.github.com/tixxit/1246894/e79fa9fbeda695b9e8a6a5d858b61ec42c7a367d
object EditDistance {
  def apply[A: Eq](a: Iterable[A], b: Iterable[A]): Int =
    a.foldLeft((0 to b.size).toList) { (prev, x) =>
      (prev zip prev.tail zip b)
        .scanLeft(prev.head + 1) { case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x === y) 0 else 1))
        }
    }.last

  def string(a: String, b: String): Int =
    apply(a, b)
}
