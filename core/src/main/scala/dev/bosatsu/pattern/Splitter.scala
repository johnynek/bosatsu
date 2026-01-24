package dev.bosatsu.pattern

import cats.Monoid

import dev.bosatsu.StringUtil

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
      fn: Int => R
  )(implicit m: Monoid[R]): Splitter[Int, Int, String, R] =
    new Splitter[Int, Int, String, R] {
      val matcher =
        Matcher.intMatcher
          .mapWithInput((s, _) => fn(s))

      val monoidResult = m
      def positions(c: Int): String => LazyList[(String, Int, R, String)] = {
        str =>
          def loop(strOffset: Int): LazyList[(String, Int, R, String)] =
            if (strOffset >= str.length) LazyList.empty
            else {
              // we have to skip the entire codepoint
              val cp = str.codePointAt(strOffset)
              val csize = Character.charCount(cp)
              if (cp == c) {
                // this is a valid match
                (
                  str.substring(0, strOffset),
                  c,
                  fn(c),
                  str.substring(strOffset + csize)
                ) #:: loop(strOffset + csize)
              } else {
                loop(strOffset + csize)
              }
            }

          loop(0)
      }

      def anySplits(str: String): LazyList[(String, Int, R, String)] =
        (0 until str.codePointCount(0, str.length))
          .to(LazyList)
          .map { idx =>
            // we have to skip to valid offsets
            val offset = str.offsetByCodePoints(0, idx)
            val prefix = str.substring(0, offset)
            val c = str.codePointAt(offset)
            val csize = Character.charCount(c)
            val post = str.substring(offset + csize)
            (prefix, c, fn(c), post)
          }

      def isEmpty(s: String): Boolean = s.isEmpty

      def uncons(s: String) =
        if (s.isEmpty) None
        else {
          val c = s.codePointAt(0)
          val csize = Character.charCount(c)
          Some((c, s.substring(csize)))
        }

      def toStr(cp: Int): String =
        (new java.lang.StringBuilder).appendCodePoint(cp).toString

      def cons(c: Int, s: String) = s"${toStr(c)}$s"

      def emptySeq = ""
      def catSeqs(s: List[String]) = s.mkString
      override def toList(s: String) =
        StringUtil.codePoints(s)

      override def fromList(cs: List[Int]) =
        cs.iterator.map(toStr(_)).mkString
    }

  val stringUnit: Splitter[Int, Int, String, Unit] =
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
            case Nil    => LazyList.empty
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
          case Nil    => LazyList.empty
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

    final def emptySeq = List.empty[V]
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
