package org.bykn.bosatsu

import java.util.Arrays

/**
 * Build a cache of the rows and columns in a given
 * string. This is for showing error messages to users
 */
case class LocationMap(fromString: String) { self =>

  private[this] val lines: Array[String] =
    fromString.split("\n", -1)

  // The position of the first element of the ith line
  private[this] val firstPos: Array[Int] = {
    val it = lines.iterator.map(_.length)
    val it2 = new Iterator[(Int, Boolean)] {
      def hasNext = it.hasNext
      def next = {
        val hn = hasNext
        val i = it.next
        (i, hn)
      }
    }
    it2.map {
      case (i, true) => i + 1 // add 1 for the newline
      case (i, false) => i
    }
    .toArray
    .scanLeft(0)(_ + _)
  }

  /**
   * Given a string offset return the line and column
   */
  def toLineCol(offset: Int): Option[(Int, Int)] =
    if (offset < 0 || offset >= fromString.length ) None
    else {
      val idx = Arrays.binarySearch(firstPos, offset)
      if (idx == firstPos.length) {
        // greater than all elements
        None
      }
      else if (idx < 0) {
        // idx = (~(insertion pos) - 1)
        // The insertion point is defined as the point at which the key would be
        // inserted into the array: the index of the first element greater than
        // the key, or a.length if all elements in the array are less than the specified key.
        //
        // so insertion pos = ~(idx + 1)
        val row = ~(idx + 1)
        // so we are pointing into a row
        val rowStart = firstPos(row)
        val col = offset - rowStart
        Some((row, col))
      }
      else {
        // idx is exactly the right value because offset is beginning of a line
        Some((idx, 0))
      }
    }
  /**
   * return the line without a newline
   */
  def getLine(i: Int): Option[String] =
    if (i >= 0 && i < lines.length) Some(lines(i))
    else None


  def showContext(offset: Int, previousLines: Int = 2): Option[String] =
    toLineCol(offset)
      .map { case (r, c) =>
        val lines = (r - previousLines to r)
          .toList
          .filter(_ >= 0)
          .map { r =>
            val liner = getLine(r).get // should never throw
            // lines are usually 1 offset labeled
            (r + 1, liner)
          }

        val padding = lines.iterator.map { case (l, _) => LocationMap.charsLineNumber(l) }.max
        def toLineStr(i: Int) = {
          val istr = i.toString
          val pad = padding - istr.length
          (" " * pad) + istr + "|"
        }
        val pointerPad = " " * toLineStr(r).length
        lines.map { case (no, l) => s"${toLineStr(no)}$l" }
          .mkString("", "\n", "\n" + pointerPad + LocationMap.pointerTo(c) + "\n")
      }
}

object LocationMap {
  /**
   * Provide a string that points with a carat to a given column
   * with 0 based indexing:
   * e.g. pointerTo(2) == "  ^"
   */
  def pointerTo(column: Int, colorString: Option[String] = Some(Console.RED)): String =
    (" " * column) + colorString.getOrElse("") + "^" + colorString.map(_ => Console.RESET).getOrElse("")

  def charsLineNumber(i: Int): Int = {
    require(i >= 0, s"expect line > 0, found $i")
    def go(i: Int, acc: Int): Int =
      if (i < 10) acc
      else go(i / 10, acc + 1)

    go(i, 1)
  }

}
