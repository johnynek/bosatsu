package org.bykn.bosatsu.pattern

import cats.Monoid

import cats.implicits._

abstract class Splitter[-Elem, Item, Sequence, R] {
  def matcher: Matcher[Elem, Item, R]
  def monoidResult: Monoid[R]

  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Elem): Sequence => LazyList[(Sequence, Item, R, Sequence)]

  // splits skipping a single character to match AnyElem
  def anySplits(str: Sequence): LazyList[(Sequence, Item, R, Sequence)]

  def isEmpty(s: Sequence): Boolean
  def uncons(s: Sequence): Option[(Item, Sequence)]
  def cons(h: Item, t: Sequence): Sequence
  def toList(s: Sequence): List[Item]
  def fromList(list: List[Item]): Sequence
  def catSeqs(s: List[Sequence]): Sequence
  def emptySeq: Sequence
}

object Splitter {
  def stringSplitter[R](
      fn: Char => R
  )(implicit m: Monoid[R]): Splitter[Char, Char, String, R] =
    new Splitter[Char, Char, String, R] {
      val matcher =
        Matcher.charMatcher
          .mapWithInput { (s, _) => fn(s) }

      val monoidResult = m
      def positions(c: Char): String => LazyList[(String, Char, R, String)] = {
        str =>
          def loop(init: Int): LazyList[(String, Char, R, String)] =
            if (init >= str.length) LazyList.empty
            else if (str.charAt(init) == c) {
              (
                str.substring(0, init),
                c,
                fn(c),
                str.substring(init + 1)
              ) #:: loop(init + 1)
            } else loop(init + 1)

          loop(0)
      }

      def anySplits(str: String): LazyList[(String, Char, R, String)] =
        (0 until str.length)
          .to(LazyList)
          .map { idx =>
            val prefix = str.substring(0, idx)
            val post = str.substring(idx + 1)
            val c = str.charAt(idx)
            (prefix, c, fn(c), post)
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
    }

  val stringUnit: Splitter[Char, Char, String, Unit] =
    stringSplitter(_ => ())

  abstract class ListSplitter[P, V, R] extends Splitter[P, V, List[V], R] {
    final def positions(c: P): List[V] => LazyList[(List[V], V, R, List[V])] = {
      val matchFn = matcher(c)

      { (str: List[V]) =>
        def loop(
            tail: List[V],
            acc: List[V]
        ): LazyList[(List[V], V, R, List[V])] =
          tail match {
            case Nil => LazyList.empty
            case h :: t =>
              matchFn(h) match {
                case None    => loop(t, h :: acc)
                case Some(r) => (acc.reverse, h, r, t) #:: loop(t, h :: acc)
              }
          }

        loop(str, Nil)
      }
    }

    final def anySplits(str: List[V]): LazyList[(List[V], V, R, List[V])] = {
      def loop(str: List[V], acc: List[V]): LazyList[(List[V], V, R, List[V])] =
        str match {
          case Nil => LazyList.empty
          case h :: t =>
            (acc.reverse, h, monoidResult.empty, t) #:: loop(t, h :: acc)
        }
      loop(str, Nil)
    }

    final def isEmpty(s: List[V]): Boolean = s.isEmpty

    final def uncons(s: List[V]) =
      if (s.isEmpty) None
      else Some((s.head, s.tail))

    final def cons(c: V, s: List[V]) = c :: s

    final def emptySeq = Nil
    final def catSeqs(s: List[List[V]]) = s.flatten
    final override def toList(s: List[V]) = s
    final override def fromList(cs: List[V]) = cs
  }

  def listSplitter[P, V, R](
      m: Matcher[P, V, R]
  )(implicit mon: Monoid[R]): Splitter[P, V, List[V], R] =
    new ListSplitter[P, V, R] {
      val matcher = m
      val monoidResult = mon
    }
}
