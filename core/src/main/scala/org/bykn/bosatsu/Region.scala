package org.bykn.bosatsu

import cats.{Order, Semigroup}

case class Region(start: Int, end: Int) {
  def +(that: Region): Region =
    Region(start, that.end)

  def -(that: Region): Region =
    if (start < that.start && that.start <= end) Region(start, that.start - 1)
    else this
}

object Region {
  implicit val ordering: Ordering[Region] =
    Ordering.by((r: Region) => (r.start, r.end))

  implicit val regionOrder: Order[Region] =
    Order.fromOrdering(using ordering)

  implicit val regionSemigroup: Semigroup[Region] =
    Semigroup.instance(_ + _)
}

trait HasRegion[T] {
  def region(t: T): Region
}

object HasRegion {
  def instance[T](fn: T => Region): HasRegion[T] =
    new HasRegion[T] {
      def region(t: T) = fn(t)
    }

  def region[T](t: T)(implicit hr: HasRegion[T]): Region =
    hr.region(t)
}
