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
}
