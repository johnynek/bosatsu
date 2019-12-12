package org.bykn.bosatsu.pattern

import cats.{Eq, Monoid}

trait Splitter[-Elem, Item, Sequence, R] {
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
  def cons(h: Item, t: Sequence): Sequence

  def toList(s: Sequence): List[Item] = {
    @annotation.tailrec
    def loop(s: Sequence, acc: List[Item]): List[Item] =
      uncons(s) match {
        case None => acc.reverse
        case Some((h, t)) => loop(t, h :: acc)
      }

    loop(s, Nil)
  }

  def fromList(list: List[Item]): Sequence = {
    @annotation.tailrec
    def loop(list: List[Item], acc: Sequence): Sequence =
      list match {
        case Nil => acc
        case h :: t => loop(t, cons(h, acc))
      }

    loop(list.reverse, emptySeq)
  }

  def catSeqs(s: List[Sequence]): Sequence
  def emptySeq: Sequence

  def anyMatch(s: Sequence): R
}

object Splitter {
  def stringSplitter[R](fn: String => R)(implicit m: Monoid[R]): Splitter[Char, Char, String, R] =
    new Splitter[Char, Char, String, R] {
      val matcher =
        Matcher.eqMatcher(Eq.fromUniversalEquals[Char])
          .mapWithInput { (s, _) => fn(s.toString) }

      val monoidResult = m
      def foldMap(s: String) = fn(s)
      def toResult(i: Char) = foldMap(i.toString)
      def positions(c: Char, str: String): Stream[(String, R, String)] = {
        def loop(init: Int): Stream[(String, R, String)] =
          if (init >= str.length) Stream.Empty
          else if (str.charAt(init) == c) {
            (str.substring(0, init), toResult(c), str.substring(init + 1)) #:: loop(init + 1)
          }
          else loop(init + 1)

        loop(0)
      }

      def anySplits(str: String): Stream[(String, R, String)] =
        (0 until str.length)
          .toStream
          .map { idx =>
            val prefix = str.substring(0, idx)
            val post = str.substring(idx + 1)
            (prefix, toResult(str.charAt(idx)), post)
          }

      def isEmpty(s: String): Boolean = s.isEmpty

      def uncons(s: String) =
        if (s.isEmpty) None
        else Some((s.head, s.tail))

      def cons(c: Char, s: String) = s"$c$s"

      def emptySeq = ""
      def catSeqs(s: List[String]) = s.mkString
      override def toList(s: String) = s.toList
      override def fromList(cs: List[Char]) = cs.mkString

      def anyMatch(s: String) = fn(s)
    }
}
