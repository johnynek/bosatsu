package org.bykn.bosatsu.pattern

import cats.{Eq, Monoid}

import cats.implicits._

trait Splitter[Elem, Item, Sequence, R] {
  def matcher: Matcher[Elem, Item, R]
  def monoidResult: Monoid[R]

  def toResult(i: Item): R

  def foldMap(s: Sequence): R

  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Elem, str: Sequence): Stream[(Sequence, R, Sequence)]

  // splits skipping a single character to match AnyElem
  def anySplits(str: Sequence): Stream[(Sequence, R, Sequence)]

  def isEmpty(s: Sequence): Boolean

  def uncons(s: Sequence): Option[(Item, Sequence)]

  def anyMatch(s: Sequence): R
}

object Splitter {
  val stringSpltter: Splitter[Char, Char, String, String] =
    new Splitter[Char, Char, String, String] {
      val matcher =
        Matcher.eqMatcher(Eq.fromUniversalEquals[Char])
          .mapWithInput { (c, _) => c.toString }

      val monoidResult = Monoid[String]
      def toResult(i: Char): String = i.toString
      def foldMap(s: String) = s
      def positions(c: Char, str: String): Stream[(String, String, String)] = {
        def loop(init: Int): Stream[(String, String, String)] =
          if (init >= str.length) Stream.Empty
          else if (str.charAt(init) == c) {
            (str.substring(0, init), c.toString, str.substring(init + 1)) #:: loop(init + 1)
          }
          else loop(init + 1)

        loop(0)
      }
      def anySplits(str: String): Stream[(String, String, String)] =
        (0 until str.length)
          .toStream
          .map { idx =>
            val prefix = str.substring(0, idx)
            val post = str.substring(idx + 1)
            (prefix, str.charAt(idx).toString, post)
          }

      def isEmpty(s: String): Boolean = s.isEmpty

      def uncons(s: String) =
        if (s.isEmpty) None
        else Some((s.head, s.tail))

      def anyMatch(s: String) = ""
    }
}
