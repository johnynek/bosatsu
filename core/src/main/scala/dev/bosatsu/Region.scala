package dev.bosatsu

import cats.{Hash, Order, Semigroup, Show}

type Region = Region.Tpe

object Region extends RegionLowPriority {
  // Use an opaque type to pack two Int values into a Long
  opaque type Tpe = Long
  extension (tpe: Tpe) {
    inline def start: Int = {
      (tpe >>> 32).toInt
    }

    inline def end: Int = tpe.toInt

    inline def eqv(that: Tpe): Boolean = tpe == that

    inline def +(that:  Tpe): Tpe =
      Region(tpe.start, that.end)

    def -(that: Tpe): Tpe =
      if (start < that.start && that.start <= end) Region(start, that.start - 1)
      else tpe

    def withEnd(end: Int): Tpe =
      Region(tpe.start, end)
  }

  def apply(start: Int, end: Int): Tpe =
    (start.toLong << 32) | (end.toLong & 0xffffffffL)


  implicit val ordering: Ordering[Region] =
    Ordering.Long

  implicit val regionOrder: Order[Region] =
    new cats.kernel.instances.LongOrder

  implicit val regionSemigroup: Semigroup[Region] =
    Semigroup.instance(_ + _)

  implicit val regionShow: Show[Region] =
    Show.show(r => s"[${r.start}, ${r.end})")
}

private[bosatsu] trait RegionLowPriority {
  implicit val regionHash: Hash[Region] =
    new Hash[Region] {
      def hash(x: Region): Int = x.hashCode
      def eqv(x: Region, y: Region): Boolean = x.eqv(y)
    }
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
