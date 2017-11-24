package org.bykn.bosatsu

import java.util.Arrays

/**
 * Build a cache of the rows and columns in a given
 * string. This is for showing error messages to users
 */
case class LocationMap(fromString: String) { self =>

  private[this] val lines: Array[String] =
    fromString.split("\n", -1)

  // The position of the last element of this line including the newline
  private[this] val endPos: Array[Int] = {
    val it = lines.map(_.length).iterator
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
    .drop(1) // remove the 0 added
  }

  /**
   * Given a string offset return the line and column
   */
  def toLineCol(offset: Int): Option[(Int, Int)] =
    if (offset < 0 || offset >= fromString.length ) None
    else {
      val idx = Arrays.binarySearch(endPos, offset)
      if (idx == endPos.length) {
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
        val rowStart = if (row <= 0) 0 else endPos(row - 1)
        val col = offset - rowStart
        Some((math.max(row, 0), col))
      }
      else {
        // idx is exactly the right value because offset is the end of a line
        val row = idx
        val newlineInc = if (row == (lines.length - 1)) 0 else 1 // +1 because we are pointing at the newline we removed
        val col = lines(row).length + newlineInc
        Some((row, col))
      }
    }
  /**
   * return the line without a newline
   */
  def getLine(i: Int): Option[String] =
    if (i >= 0 && i < lines.length) Some(lines(i))
    else None
}

object LocationMap {
  /**
   * Provide a string that points with a carat to a given column
   * with 0 based indexing:
   * e.g. pointerTo(2) == "  ^"
   */
  def pointerTo(column: Int): String =
    (" " * column) + "^"
}
