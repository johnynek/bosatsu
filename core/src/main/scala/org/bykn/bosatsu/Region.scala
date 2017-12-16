package org.bykn.bosatsu

import cats.kernel.Band

case class Region(start: Int, end: Int) {
  def +(that: Region): Region =
    Region(start, that.end)
}

object Region {
  implicit val regionBand: Band[Region] =
    new Band[Region] {
      def combine(a: Region, b: Region): Region = a + b
    }

  implicit val ordering: Ordering[Region] =
    Ordering.by { r: Region => (r.start, r.end) }
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
