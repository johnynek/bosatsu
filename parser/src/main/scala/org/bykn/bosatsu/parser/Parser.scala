package org.bykn.bosatsu.parser

import cats.{Eval, Monad, Defer, Alternative, FlatMap, Now, MonoidK}
import cats.data.NonEmptyList
import java.util.BitSet

import cats.implicits._

/**
 * Following the haskell library trifecta,
 * these parsers either parse successfully, parse 0 and fail,
 * or parse more than 1 and fail. The "or" operation
 * only works for parsing 0 and failing.
 *
 * We can convert a failure after parsing more than one
 * to a zero parse with .backtrack
 *
 * Parses 0 or more characters to return A
 */
sealed abstract class Parser[+A] {
  final def parse(str: String): Either[Parser.Error, (String, A)] = {
    val state = new Parser.Impl.State(str)
    val result = parseMut(state)
    val err = state.error
    if (err eq null) Right((str.substring(state.offset), result))
    else Left(err)
  }

  def ? : Parser[Option[A]] =
    Parser.oneOf(Parser.map(this)(Some(_)) :: Parser.pure(None) :: Nil)

  def void: Parser[Unit] =
    Parser.void(this)

  def string: Parser[String] =
    Parser.string(this)

  /**
   * If this fails, rewind the offset to the starting point
   * so we can be used with oneOf
   */
  def backtrack: Parser[A] =
    Parser.backtrack(this)

  def ~[B](that: Parser[B]): Parser[(A, B)] =
    Parser.product(this, that)

  def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
    Parser.oneOf(this :: that :: Nil)

  protected def parseMut(state: Parser.Impl.State): A
}

/**
 * Parses 1 or more characters if it succeeds to return A
 */
sealed abstract class Parser1[+A] extends Parser[A] {
  override def void: Parser1[Unit] =
    Parser.void1(this)

  override def string: Parser1[String] =
    Parser.string1(this)

  /**
   * If this fails, rewind the offset to the starting point
   * so we can be used with oneOf
   */
  override def backtrack: Parser1[A] =
    Parser.backtrack1(this)

  override def ~[B](that: Parser[B]): Parser1[(A, B)] =
    Parser.product10(this, that)

  def ~:[B](that: Parser[B]): Parser1[(B, A)] =
    Parser.product01(that, this)

  def orElse1[A1 >: A](that: Parser1[A1]): Parser1[A1] =
    Parser.oneOf1(this :: that :: Nil)
}

object Parser extends ParserInstances {
  sealed abstract class Expectation {
    def toError: Error = Error.MissedExpectation(this)
  }

  object Expectation {
    case class Str(offset: Int, str: String) extends Expectation
    // expected a character in a given range
    case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
    case object Failure extends Expectation
    case class StartOfString(offset: Int) extends Expectation
    case class EndOfString(offset: Int, length: Int) extends Expectation
    case class Length(offset: Int, expected: Int, actual: Int) extends Expectation
  }
  sealed abstract class Error {
    def expected: NonEmptyList[Expectation]
  }
  object Error {
    case class MissedExpectation(expectation: Expectation) extends Error {
      val expected = NonEmptyList(expectation, Nil)
    }

    case class Combined(errors: NonEmptyList[Error]) extends Error {
      def expected = errors.flatMap(_.expected)
    }

    def combined(errors: NonEmptyList[Error]): Error =
      if (errors.tail.isEmpty) errors.head
      else Combined(errors)

    case object AlwaysFail extends Error {
      def expected = NonEmptyList(Expectation.Failure, Nil)
    }
  }

  def pure[A](a: A): Parser[A] =
    Impl.Pure(a)

  /**
   * this is an error if string is empty
   */
  def expect(str: String): Parser1[Unit] =
    Impl.Expect(str)

  def oneOf1[A](parsers: List[Parser1[A]]): Parser1[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser1[A]], acc: List[Parser1[A]]): Parser1[A] =
      ls match {
        case Nil =>
          acc match {
            case h :: Nil => h
            case other => Impl.OneOf1(other.reverse)
          }
        case Impl.OneOf1(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case notOneOf :: rest =>
          flatten(rest, notOneOf :: acc)
      }

    flatten(parsers, Nil)
  }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser[A]], acc: List[Parser[A]]): Parser[A] =
      ls match {
        case Nil =>
          acc match {
            case h :: Nil => h
            case other => Impl.OneOf(other.reverse)
          }
        case Impl.OneOf(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case notOneOf :: rest =>
          flatten(rest, notOneOf :: acc)
      }

    flatten(ps, Nil)
  }

  /**
   * Parse the next len characters where len > 0
   */
  def length(len: Int): Parser1[String] =
    Impl.Length(len)

  def rep[A](p1: Parser1[A], min: Int = 0): Parser[List[A]] =
    Impl.Rep(p1, min)

  def rep1[A](p1: Parser1[A], min: Int = 1): Parser1[NonEmptyList[A]] =
    Impl.Rep1(p1, min)

  def product10[A, B](first: Parser1[A], second: Parser[B]): Parser1[(A, B)] =
    Impl.Prod1(first, second)

  def product01[A, B](first: Parser[A], second: Parser1[B]): Parser1[(A, B)] =
    Impl.Prod1(first, second)

  def product[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] =
    Impl.Prod(first, second)

  def map[A, B](p: Parser[A])(fn: A => B): Parser[B] =
    Impl.Map(p, fn)

  def map1[A, B](p: Parser1[A])(fn: A => B): Parser1[B] =
    Impl.Map1(p, fn)

  def flatMap[A, B](pa: Parser[A])(fn: A => Parser[B]): Parser[B] =
    Impl.FlatMap(pa, fn)

  def flatMap1[A, B](pa: Parser1[A])(fn: A => Parser[B]): Parser1[B] =
    Impl.FlatMap1(pa, fn)

  def flatMap1[A, B](pa: Parser[A])(fn: A => Parser1[B]): Parser1[B] =
    Impl.FlatMap1(pa, fn)

  def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
    Impl.TailRecM(init, fn)

  def tailRecM1[A, B](init: A)(fn: A => Parser1[Either[A, B]]): Parser1[B] =
    Impl.TailRecM1(init, fn)

  def defer1[A](pa: => Parser1[A]): Parser1[A] =
    Impl.Defer1(() => pa)

  def defer[A](pa: => Parser[A]): Parser[A] =
    Impl.Defer(() => pa)

  val fail: Parser1[Nothing] = oneOf1(Nil)

  def charIn(c0: Char, cs: Char*): Parser1[Char] =
    charIn1(c0 :: cs.toList)

  def charIn1(cs: Iterable[Char]): Parser1[Char] = {
    val ary = cs.toArray
    java.util.Arrays.sort(ary)
    Impl.CharIn(Impl.bitSetFor(ary), Impl.rangesFor(ary))
  }

  def charIn(cs: Iterable[Char]): Parser[Char] =
    if (cs.isEmpty) fail
    else charIn1(cs)

  def charWhere(fn: Char => Boolean): Parser[Char] =
    charIn((Char.MinValue to Char.MaxValue).filter(fn))

  def charWhere1(fn: Char => Boolean): Parser1[Char] =
    charIn1((Char.MinValue to Char.MaxValue).filter(fn))

  def void[A](pa: Parser[A]): Parser[Unit] =
    pa match {
      case p1: Parser1[Any] => void1(p1)
      case Impl.Map(p, _) => void(p)
      case Impl.StringP(s) => void(s)
      case v@Impl.Void(_) => v
      case _ => Impl.Void(pa)
    }

  def void1[A](pa: Parser1[A]): Parser1[Unit] =
    pa match {
      case Impl.Map1(e, _) => void1(e)
      case Impl.StringP1(s) => void1(s)
      case v@Impl.Void1(_) => v
      case _ => Impl.Void1(pa)
    }

  def string[A](pa: Parser[A]): Parser[String] =
    pa match {
      case p1: Parser1[Any] => string1(p1)
      case Impl.Map(p, _) => string(p)
      case Impl.Void(v) => string(v)
      case s@Impl.StringP(_) => s
      case _ => Impl.StringP(pa)
    }

  def string1[A](pa: Parser1[A]): Parser1[String] =
    pa match {
      case e@Impl.Expect(str) => e.as(str)
      case Impl.Map1(e, _) => string1(e)
      case Impl.Void1(v) => string1(v)
      case s1@Impl.StringP1(_) => s1
      case _ => Impl.StringP1(pa)
    }

  val index: Parser[Int] =
    Impl.Index

  // succeeds when we are at the start
  val start: Parser[Unit] = Impl.StartParser

  // succeeds when we are at the end
  val end: Parser[Unit] = Impl.EndParser

  /**
   * If we fail, rewind the offset back so that
   * we can try other branches. This tends
   * to harm debuggability and ideally should be
   * minimized
   */
  def backtrack[A](pa: Parser[A]): Parser[A] =
    pa match {
      case bt: Impl.Backtrack[A] => bt
      case bt: Impl.Backtrack1[A] => bt
      case nbt => Impl.Backtrack(nbt)
    }

  /**
   * If we fail, rewind the offset back so that
   * we can try other branches. This tends
   * to harm debuggability and ideally should be
   * minimized
   */
  def backtrack1[A](pa: Parser1[A]): Parser1[A] =
    pa match {
      case bt: Impl.Backtrack1[A] => bt
      case nbt => Impl.Backtrack1(nbt)
    }

  implicit val catsInstancesParser1: FlatMap[Parser1] with Defer[Parser1] with MonoidK[Parser1]=
    new FlatMap[Parser1] with Defer[Parser1] with MonoidK[Parser1] {
      def empty[A] = fail

      def defer[A](pa: => Parser1[A]): Parser1[A] =
        defer1(pa)

      def map[A, B](fa: Parser1[A])(fn: A => B): Parser1[B] =
        map1(fa)(fn)

      def flatMap[A, B](fa: Parser1[A])(fn: A => Parser1[B]): Parser1[B] =
        flatMap1(fa)(fn)

      override def product[A, B](pa: Parser1[A], pb: Parser1[B]): Parser1[(A, B)] =
        product10(pa, pb)

      override def map2[A, B, C](pa: Parser1[A], pb: Parser1[B])(fn: (A, B) => C): Parser1[C] =
        map(product(pa, pb)) { case (a, b) => fn(a, b) }

      override def map2Eval[A, B, C](pa: Parser1[A], pb: Eval[Parser1[B]])(fn: (A, B) => C): Eval[Parser1[C]] =
        Now(pb match {
          case Now(pb) => map2(pa, pb)(fn)
          case later => map2(pa, defer(later.value))(fn)
        })

      def tailRecM[A, B](init: A)(fn: A => Parser1[Either[A, B]]): Parser1[B] =
        tailRecM1(init)(fn)

      def combineK[A](pa: Parser1[A], pb: Parser1[A]): Parser1[A] =
        Parser.oneOf1(pa :: pb :: Nil)

      override def void[A](pa: Parser1[A]): Parser1[Unit] =
        pa.void
    }

  private object Impl {

    final class State(val str: String) {
      var offset: Int = 0
      var error: Error = null
      var capture: Boolean = true

      override def toString: String = s"State(str = $str, offset = $offset, error = $error, capture = $capture)"
    }

    case class Pure[A](result: A) extends Parser[A] {
      override def parseMut(state: State): A = result
    }

    case class Length(len: Int) extends Parser1[String] {
      if (len < 1) throw new IllegalArgumentException(s"expected length > 0, found $len")

      override def parseMut(state: State): String = {
        val end = state.offset + len
        if (end <= state.str.length) {
          val res = if (state.capture) state.str.substring(state.offset, end) else null
          state.offset = end
          res
        }
        else {
          state.error = Expectation.Length(state.offset, len, state.str.length - state.offset).toError
          null
        }
      }
    }

    def void[A](pa: Parser[A], state: State): Unit = {
      val s0 = state.capture
      state.capture = false
      pa.parseMut(state)
      state.capture = s0
      ()
    }

    case class Void[A](parser: Parser[A]) extends Parser[Unit] {
      override def parseMut(state: State): Unit =
        Impl.void(parser, state)
    }

    case class Void1[A](parser: Parser1[A]) extends Parser1[Unit] {
      override def parseMut(state: State): Unit =
        Impl.void(parser, state)
    }

    def string[A](pa: Parser[A], state: State): String = {
      val s0 = state.capture
      state.capture = false
      val init = state.offset
      pa.parseMut(state)
      val str = state.str.substring(init, state.offset)
      state.capture = s0
      str
    }

    case class StringP[A](parser: Parser[A]) extends Parser[String] {
      override def parseMut(state: State): String =
        Impl.string(parser, state)
    }

    case class StringP1[A](parser: Parser1[A]) extends Parser1[String] {
      override def parseMut(state: State): String =
        Impl.string(parser, state)
    }

    case object StartParser extends Parser[Unit] {
      override def parseMut(state: State): Unit = {
        if (state.offset != 0) {
          state.error = Expectation.StartOfString(state.offset).toError
        }
        ()
      }
    }

    case object EndParser extends Parser[Unit] {
      override def parseMut(state: State): Unit = {
        if (state.offset != state.str.length) {
          state.error = Expectation.EndOfString(state.offset, state.str.length).toError
        }
        ()
      }
    }

    case object Index extends Parser[Int] {
      override def parseMut(state: State): Int = state.offset
    }

    def backtrack[A](pa: Parser[A], state: State): A = {
      val offset = state.offset
      val a = pa.parseMut(state)
      if (state.error ne null) {
        state.offset = offset
      }
      a
    }

    case class Backtrack[A](parser: Parser[A]) extends Parser[A] {
      override def parseMut(state: State): A =
        Impl.backtrack(parser, state)
    }

    case class Backtrack1[A](parser: Parser1[A]) extends Parser1[A] {
      override def parseMut(state: State): A =
        Impl.backtrack(parser, state)
    }

    case class Expect(message: String) extends Parser1[Unit] {
      if (message.isEmpty) throw new IllegalArgumentException("we need a non-empty string to expect a message")

      override def parseMut(state: State): Unit = {
        if (state.str.regionMatches(state.offset, message, 0, message.length)) {
          state.offset += message.length
          ()
        }
        else {
          state.error = Expectation.Str(state.offset, message).toError
          ()
        }
      }
    }

    object OneOfImpl {
      def parse[A](all: List[Parser[A]], state: State): A = {
        var ps = all
        val offset = state.offset
        var errs: List[Error] = Nil
        while (ps.nonEmpty) {
          val thisParser = ps.head
          ps = ps.tail
          val res = thisParser.parseMut(state)
          // we stop if there was no error
          // or if we consumed some input
          if ((state.error eq null) || (state.offset != offset)) {
            return res
          }
          else {
            // we failed to parse, but didn't consume input
            // is unchanged we continue
            // else we stop
            errs = state.error :: errs
            state.error = null
          }
        }
        // if we got here, all of them failed, but we
        // never advanced the offset
        state.error =
          NonEmptyList.fromList(errs.reverse) match {
            case None => Error.AlwaysFail
            case Some(errsNEL) => Error.combined(errsNEL)
          }
        null.asInstanceOf[A]
      }
    }

    case class OneOf1[A](all: List[Parser1[A]]) extends Parser1[A] {
      override def parseMut(state: State): A = OneOfImpl.parse(all, state)
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      override def parseMut(state: State): A = OneOfImpl.parse(all, state)
    }

    object ProdImpl {
      def parse[A, B](pa: Parser[A], pb: Parser[B], state: State): (A, B) = {
        val a = pa.parseMut(state)
        if (state.error eq null) {
          val b = pb.parseMut(state)
          if (state.capture && (state.error eq null)) (a, b)
          else null
        }
        else null
      }
    }

    // we know that at least one of first | second is Parser1
    case class Prod1[A, B](first: Parser[A], second: Parser[B]) extends Parser1[(A, B)] {
      override def parseMut(state: State): (A, B) = ProdImpl.parse(first, second, state)
    }

    case class Prod[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)] {
      override def parseMut(state: State): (A, B) = ProdImpl.parse(first, second, state)
    }

    def map[A, B](parser: Parser[A], fn: A => B, state: State): B = {
      val a = parser.parseMut(state)
      if ((state.error eq null) && state.capture) fn(a)
      else null.asInstanceOf[B]
    }

    case class Map[A, B](parser: Parser[A], fn: A => B) extends Parser[B] {
      override def parseMut(state: State): B = Impl.map(parser, fn, state)
    }

    case class Map1[A, B](parser: Parser1[A], fn: A => B) extends Parser1[B] {
      override def parseMut(state: State): B = Impl.map(parser, fn, state)
    }

    def flatMap[A, B](parser: Parser[A], fn: A => Parser[B], state: State): B = {
      val a = parser.parseMut(state)
      if (state.error eq null) {
        fn(a).parseMut(state)
      }
      else null.asInstanceOf[B]
    }

    case class FlatMap[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser[B] {
      override def parseMut(state: State): B = flatMap(parser, fn, state)
    }

    // at least one of the parsers needs to be a Parser1
    case class FlatMap1[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser1[B] {
      override def parseMut(state: State): B = flatMap(parser, fn, state)
    }

    def tailRecM[A, B](init: Parser[Either[A, B]], fn: A => Parser[Either[A, B]], state: State): B = {
      var p: Parser[Either[A, B]] = init
      while (state.error eq null) {
        val res = p.parseMut(state)
        if (state.error eq null) {
          res match {
            case Right(b) =>
              return b
            case Left(a) =>
              p = fn(a)
          }
        }
      }
      null.asInstanceOf[B]
    }

    case class TailRecM[A, B](init: A, fn: A => Parser[Either[A, B]]) extends Parser[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = Impl.tailRecM(p1, fn, state)
    }

    case class TailRecM1[A, B](init: A, fn: A => Parser1[Either[A, B]]) extends Parser1[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = Impl.tailRecM(p1, fn, state)
    }

    @annotation.tailrec
    final def compute[A](fn: () => Parser[A]): Parser[A] =
      fn() match {
        case Defer1(f) => compute(f)
        case Defer(f) => compute(f)
        case notDefer => notDefer
      }

    case class Defer1[A](fn: () => Parser1[A]) extends Parser1[A] {
      lazy val computed: Parser[A] = compute(fn)
      override def parseMut(state: State): A = computed.parseMut(state)
    }

    case class Defer[A](fn: () => Parser[A]) extends Parser[A] {
      lazy val computed: Parser[A] = compute(fn)
      override def parseMut(state: State): A = computed.parseMut(state)
    }

    case class Rep[A](p1: Parser1[A], min: Int) extends Parser1[List[A]] {
      override def parseMut(state: State): List[A] = {
        val cap = state.capture
        val bldr = if (cap) List.newBuilder[A] else null
        var cnt = 0
        var offset = state.offset

        while (true) {
          val a = p1.parseMut(state)
          if (state.error eq null) {
            cnt += 1
            if (cap) {
              bldr += a
            }
            offset = state.offset
          }
          else if (state.offset != offset) {
            // this is a partial parse, which is a failure
            return null
          }
          else {
            if (cnt >= min) {
              // return the latest
              state.offset = offset
              state.error = null
              return if (cap) bldr.result() else null
            }
            else {
              return null
            }
          }
        }
        sys.error("unreachable")
      }
    }

    case class Rep1[A](p1: Parser1[A], min: Int) extends Parser1[NonEmptyList[A]] {
      if (min < 1) throw new IllegalArgumentException(s"expected min >= 1, found: $min")

      override def parseMut(state: State): NonEmptyList[A] = {
        val cap = state.capture
        val bldr = if (cap) List.newBuilder[A] else null
        val a0 = p1.parseMut(state)

        if (state.error ne null) return null
        var cnt = 1
        var offset = state.offset


        while (true) {
          val a = p1.parseMut(state)
          if (state.error eq null) {
            cnt += 1
            if (cap) {
              bldr += a
            }
            offset = state.offset
          }
          else {
            if (cnt >= min) {
              // return the latest
              state.offset = offset
              state.error = null
              return if (cap) NonEmptyList(a0, bldr.result()) else null
            }
            else {
              return null
            }
          }
        }
        sys.error("unreachable")
      }
    }

    // invariant: array is sorted
    def bitSetFor(charArray: Array[Char]): BitSet = {
      var idx = 0
      val bs = new BitSet(charArray(charArray.length - 1).toInt + 1)
      while (idx < charArray.length) {
        bs.set(charArray(idx).toInt)
        idx += 1
      }

      bs
    }

    def rangesFor(charArray: Array[Char]): NonEmptyList[(Char, Char)] = {
      def rangesFrom(start: Char, end: Char, idx: Int): NonEmptyList[(Char, Char)] =
        if (idx >= charArray.length || (idx < 0)) NonEmptyList((start, end), Nil)
        else {
          val end1 = charArray(idx)
          if (end1.toInt == end.toInt + 1) rangesFrom(start, end1, idx + 1)
          else {
            // we had a break:
            (start, end) :: rangesFrom(end1, end1, idx + 1)
          }
        }

      rangesFrom(charArray(0), charArray(0), 1)
    }

    case class CharIn(bitSet: BitSet, ranges: NonEmptyList[(Char, Char)]) extends Parser1[Char] {

      def makeError(offset: Int): Error =
        Error.combined(ranges.map { case (s, e) => Expectation.InRange(offset, s, e).toError })

      override def parseMut(state: State): Char = {
        if (state.offset < state.str.length) {
          val char = state.str.charAt(state.offset)
          if (bitSet.get(char.toInt)) {
            // we found the character
            state.offset += 1
            char
          }
          else {
            state.error = makeError(state.offset)
            '\u0000'
          }
        }
        else {
          state.error = makeError(state.offset)
          '\u0000'
        }
      }
    }
  }
}

abstract class ParserInstances {
  implicit val catInstancesParser: Monad[Parser] with Alternative[Parser] with Defer[Parser] =
    new Monad[Parser] with Alternative[Parser] with Defer[Parser] {
      def pure[A](a: A): Parser[A] = Parser.pure(a)

      def defer[A](a: => Parser[A]) = Parser.defer(a)

      def empty[A]: Parser[A] = Parser.oneOf(Nil)

      override def map[A, B](fa: Parser[A])(fn: A => B): Parser[B] = Parser.map(fa)(fn)

      override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] = Parser.product(fa, fb)

      override def map2[A, B, C](pa: Parser[A], pb: Parser[B])(fn: (A, B) => C): Parser[C] =
        map(product(pa, pb)) { case (a, b) => fn(a, b) }

      override def map2Eval[A, B, C](pa: Parser[A], pb: Eval[Parser[B]])(fn: (A, B) => C): Eval[Parser[C]] =
        Now(pb match {
          case Now(pb) => map2(pa, pb)(fn)
          case later => map2(pa, defer(later.value))(fn)
        })

      def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
        Parser.flatMap(fa)(fn)

      def combineK[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
        Parser.oneOf(pa :: pb :: Nil)

      def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
        Parser.tailRecM(init)(fn)

      override def void[A](pa: Parser[A]): Parser[Unit] =
        Parser.void(pa)
    }
}
