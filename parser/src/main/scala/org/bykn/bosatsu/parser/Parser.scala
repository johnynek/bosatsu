package org.bykn.bosatsu.parser

import cats.{Eval, Monad, Defer, Alternative, FlatMap, Now, MonoidK}
import cats.data.NonEmptyList

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
    Parser.oneOf(Parser.map(this)(Some(_)) :: Parser.Impl.optTail)

  def void: Parser[Unit] =
    Parser.void(this)

  /**
   * Discard the value and get the substring that this parser
   * matches
   */
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

  /**
   * If this parser fails without consuming any input,
   * moving on to that. See backtrack to make any failure
   * rewind and not consume input
   */
  def orElse[A1 >: A](that: Parser[A1]): Parser[A1] =
    Parser.oneOf(this :: that :: Nil)

  def map[B](fn: A => B): Parser[B] =
    Parser.map(this)(fn)

  /**
   * This is the usual monadic composition, but you
   * should much prefer to use ~ or Apply.product, *>, <*, etc
   * if you can since it is much more efficient. This
   * has to call fn on each parse, which could be a lot
   * of extra work is you already know the result as is
   * the case for ~
   */
  def flatMap[B](fn: A => Parser[B]): Parser[B] =
    Parser.flatMap(this)(fn)

  /**
   * Allows us to compose with Parser1 using ~ or flatMap
   * to create a Parser1 (which is needed for .rep, for instance)
   */
  def with1: Parser.With1[A] =
    new Parser.With1(this)

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

  override def map[B](fn: A => B): Parser1[B] =
    Parser.map1(this)(fn)

  /**
   * This is the usual monadic composition, but you
   * should much prefer to use ~ or Apply.product, *>, <*, etc
   * if you can since it is much more efficient. This
   * has to call fn on each parse, which could be a lot
   * of extra work is you already know the result as is
   * the case for ~
   */
  override def flatMap[B](fn: A => Parser[B]): Parser1[B] =
    Parser.flatMap10(this)(fn)

  /**
   * If this parser fails without consuming any input,
   * moving on to that. See backtrack to make any failure
   * rewind and not consume input
   */
  def orElse1[A1 >: A](that: Parser1[A1]): Parser1[A1] =
    Parser.oneOf1(this :: that :: Nil)

  /**
   * Repeat zero or more times
   */
  def rep: Parser[List[A]] =
    Parser.rep(this)

  /**
   * Repeat one or more times
   */
  def rep1: Parser1[NonEmptyList[A]] =
    Parser.rep1(this, min = 1)

  /**
   * Repeat min, with min >= 1, or more times
   */
  def rep1(min: Int): Parser1[NonEmptyList[A]] =
    Parser.rep1(this, min = min)
}

object Parser extends ParserInstances {
  sealed abstract class Expectation {
    import Expectation._

    def offset: Int
    def toError: Error = Error.MissedExpectation(this)

    def translateOffset(inc: Int): Expectation = {
      this match {
        case Str(o, str) => Str(o + inc, str)
        case InRange(o, l, c) => InRange(o + inc, l, c)
        case Failure(o) => Failure(o + inc)
        case StartOfString(o) => StartOfString(o + inc)
        case EndOfString(o, length) => EndOfString(o + inc, length)
        case Length(o, expected, actual) => Length(o + inc, expected, actual)
      }
    }
  }

  object Expectation {
    case class Str(offset: Int, str: String) extends Expectation
    // expected a character in a given range
    case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
    case class Failure(offset: Int) extends Expectation
    case class StartOfString(offset: Int) extends Expectation
    case class EndOfString(offset: Int, length: Int) extends Expectation
    case class Length(offset: Int, expected: Int, actual: Int) extends Expectation
  }

  sealed abstract class Error {
    def expected: NonEmptyList[Expectation]
    def offsets: NonEmptyList[Int] =
      expected.map(_.offset).distinct

    def translateOffset(inc: Int): Error =
      this match {
        case Error.MissedExpectation(x) => Error.MissedExpectation(x.translateOffset(inc))
        case Error.Combined(es) => Error.Combined(es.map(_.translateOffset(inc)))
      }
  }
  object Error {
    case class MissedExpectation(expectation: Expectation) extends Error {
      def expected = NonEmptyList(expectation, Nil)
    }

    case class Combined(errors: NonEmptyList[Error]) extends Error {
      def expected = errors.flatMap(_.expected)
    }

    def combined(errors: NonEmptyList[Error]): Error =
      if (errors.tail.isEmpty) errors.head
      else Combined(errors)
  }

  final class With1[+A](val parser: Parser[A]) extends AnyVal {
    def ~[B](that: Parser1[B]): Parser1[(A, B)] =
      Parser.product01(parser, that)

    /**
     * This is the usual monadic composition, but you
     * should much prefer to use ~ or Apply.product, *>, <*, etc
     * if you can since it is much more efficient. This
     * has to call fn on each parse, which could be a lot
     * of extra work is you already know the result as is
     * the case for ~
     */
     def flatMap[B](fn: A => Parser1[B]): Parser1[B] =
        Parser.flatMap01(parser)(fn)
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

  /**
   * Repeat this parser 0 or more times
   * note: this can wind up parsing nothing
   */
  def rep[A](p1: Parser1[A]): Parser[List[A]] =
    Impl.Rep(p1)

  /**
   * Repeat this parser 1 or more times
   */
  def rep1[A](p1: Parser1[A], min: Int): Parser1[NonEmptyList[A]] =
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

  def flatMap10[A, B](pa: Parser1[A])(fn: A => Parser[B]): Parser1[B] =
    Impl.FlatMap1(pa, fn)

  def flatMap01[A, B](pa: Parser[A])(fn: A => Parser1[B]): Parser1[B] =
    Impl.FlatMap1(pa, fn)

  def tailRecM[A, B](init: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
    Impl.TailRecM(init, fn)

  def tailRecM1[A, B](init: A)(fn: A => Parser1[Either[A, B]]): Parser1[B] =
    Impl.TailRecM1(init, fn)

  def defer1[A](pa: => Parser1[A]): Parser1[A] =
    Impl.Defer1(() => pa)

  def defer[A](pa: => Parser[A]): Parser[A] =
    Impl.Defer(() => pa)

  val Fail: Parser1[Nothing] = oneOf1(Nil)

  def fail[A]: Parser1[A] = Fail

  def charIn(c0: Char, cs: Char*): Parser1[Char] =
    charIn1(c0 :: cs.toList)

  def charIn1(cs: Iterable[Char]): Parser1[Char] = {
    val ary = cs.toArray
    java.util.Arrays.sort(ary)
    Impl.CharIn(ary(0).toInt, BitSetUtil.bitSetFor(ary), Impl.rangesFor(ary))
  }

  def charIn(cs: Iterable[Char]): Parser[Char] =
    if (cs.isEmpty) fail
    else charIn1(cs)

  def charWhere(fn: Char => Boolean): Parser[Char] = {
    // we use defer to avoid enumerating this function unless needed
    defer(charIn(Impl.allChars.filter(fn)))
  }

  def charWhere1(fn: Char => Boolean): Parser1[Char] =
    // we use defer to avoid enumerating this function unless needed
    defer1(charIn1(Impl.allChars.filter(fn)))

  def void[A](pa: Parser[A]): Parser[Unit] =
    pa match {
      case v@Impl.Void(_) => v
      case notVoid => Impl.Void(Impl.unmap(pa))
    }

  def void1[A](pa: Parser1[A]): Parser1[Unit] =
    pa match {
      case v@Impl.Void1(_) => v
      case notVoid => Impl.Void1(Impl.unmap1(pa))
    }

  def string[A](pa: Parser[A]): Parser[String] =
    pa match {
      case str@Impl.StringP(_) => str
      case notVoid => Impl.StringP(Impl.unmap(pa))
    }

  def string1[A](pa: Parser1[A]): Parser1[String] =
    pa match {
      case str@Impl.StringP1(_) => str
      case notVoid => Impl.StringP1(Impl.unmap1(pa))
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
      def empty[A] = Fail

      def defer[A](pa: => Parser1[A]): Parser1[A] =
        defer1(pa)

      def map[A, B](fa: Parser1[A])(fn: A => B): Parser1[B] =
        map1(fa)(fn)

      def flatMap[A, B](fa: Parser1[A])(fn: A => Parser1[B]): Parser1[B] =
        flatMap10(fa)(fn)

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

    val allChars = Char.MinValue to Char.MaxValue

    val optTail: List[Parser[Option[Nothing]]] = Parser.pure(None) :: Nil

    /**
     * This removes any trailing map functions which
     * can cause wasted allocations if we are later going
     * to void or return strings. This stops
     * at StringP or VoidP since those are markers
     * that anything below has already been transformed
     */
    def unmap[A](pa: Parser[A]): Parser[Any] =
      pa match {
        case p1: Parser1[Any] => unmap1(p1)
        case Map(p, _) =>
          // we discard any allocations done by fn
          unmap(p)
        case StringP(s) =>
          // StringP is added privately, and only after unmap
          s
        case Void(v) =>
          // Void is added privately, and only after unmap
          v
        case Backtrack(p) => Backtrack(unmap(p))
        case OneOf(ps) => OneOf(ps.map(unmap))
        case Prod(p1, p2) => Prod(unmap(p1), unmap(p2))
        case Defer(fn) =>
          Defer(() => unmap(compute(fn)))
        case FlatMap(p1, fn) =>
          FlatMap(p1, fn.andThen(unmap(_)))
        case Rep(p) => Rep(unmap1(p))
        case Pure(_) | Index | StartParser | EndParser | TailRecM(_, _) =>
          // we can't transform this significantly
          pa
      }

    /**
     * This removes any trailing map functions which
     * can cause wasted allocations if we are later going
     * to void or return strings. This stops
     * at StringP or VoidP since those are markers
     * that anything below has already been transformed
     */
    def unmap1[A](pa: Parser1[A]): Parser1[Any] =
      pa match {
        case Map1(p, _) =>
          // we discard any allocations done by fn
          unmap1(p)
        case StringP1(s) =>
          // StringP is added privately, and only after unmap
          s
        case Void1(v) =>
          // Void is added privately, and only after unmap
          v
        case Backtrack1(p) => Backtrack1(unmap1(p))
        case OneOf1(ps) => OneOf1(ps.map(unmap1))
        case Prod1(p1, p2) => Prod1(unmap(p1), unmap(p2))
        case Defer1(fn) =>
          Defer1(() => unmap1(compute1(fn)))
        case FlatMap1(p1, fn) =>
          FlatMap1(p1, fn.andThen(unmap(_)))
        case Rep1(p, m) => Rep1(unmap1(p), m)
        case CharIn(_, _, _) | Expect(_) | Length(_) | TailRecM1(_, _) =>
          // we can't transform this significantly
          pa

      }

    final class State(val str: String) {
      var offset: Int = 0
      var error: Error = null
      var capture: Boolean = true
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

    final def backtrack[A](pa: Parser[A], state: State): A = {
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

    final def oneOf[A](all: List[Parser[A]], state: State): A = {
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
          case None => Expectation.Failure(offset).toError
          case Some(errsNEL) => Error.combined(errsNEL)
        }
      null.asInstanceOf[A]
    }

    case class OneOf1[A](all: List[Parser1[A]]) extends Parser1[A] {
      override def parseMut(state: State): A = oneOf(all, state)
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      override def parseMut(state: State): A = oneOf(all, state)
    }

    final def prod[A, B](pa: Parser[A], pb: Parser[B], state: State): (A, B) = {
      val a = pa.parseMut(state)
      if (state.error eq null) {
        val b = pb.parseMut(state)
        if (state.capture && (state.error eq null)) (a, b)
        else null
      }
      else null
    }

    // we know that at least one of first | second is Parser1
    case class Prod1[A, B](first: Parser[A], second: Parser[B]) extends Parser1[(A, B)] {
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    case class Prod[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)] {
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    final def map[A, B](parser: Parser[A], fn: A => B, state: State): B = {
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

    final def flatMap[A, B](parser: Parser[A], fn: A => Parser[B], state: State): B = {
      // we can't void before flatMap unfortunately, because
      // we need to be able to produce the next parser
      val cap = state.capture
      state.capture = true
      val a = parser.parseMut(state)
      state.capture = cap

      if (state.error eq null) {
        fn(a).parseMut(state)
      }
      else null.asInstanceOf[B]
    }

    case class FlatMap[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser[B] {
      override def parseMut(state: State): B = Impl.flatMap(parser, fn, state)
    }

    // at least one of the parsers needs to be a Parser1
    case class FlatMap1[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser1[B] {
      override def parseMut(state: State): B = Impl.flatMap(parser, fn, state)
    }

    final def tailRecM[A, B](init: Parser[Either[A, B]], fn: A => Parser[Either[A, B]], state: State): B = {
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
        case Defer1(f) => compute1(f)
        case Defer(f) => compute(f)
        case notDefer => notDefer
      }
    @annotation.tailrec
    final def compute1[A](fn: () => Parser1[A]): Parser1[A] =
      fn() match {
        case Defer1(f) => compute1(f)
        case notDefer => notDefer
      }

    case class Defer1[A](fn: () => Parser1[A]) extends Parser1[A] {
      private[this] var computed: Parser[A] = null
      override def parseMut(state: State): A = {

        val p0 = computed
        val p =
          if (p0 ne null) p0
          else {
            val res = compute1(fn)
            computed = res
            res
          }

        p.parseMut(state)
      }
    }

    case class Defer[A](fn: () => Parser[A]) extends Parser[A] {
      private[this] var computed: Parser[A] = null
      override def parseMut(state: State): A = {

        val p0 = computed
        val p =
          if (p0 ne null) p0
          else {
            val res = compute(fn)
            computed = res
            res
          }

        p.parseMut(state)
      }
    }

    case class Rep[A](p1: Parser1[A]) extends Parser[List[A]] {
      override def parseMut(state: State): List[A] = {
        val cap = state.capture
        val bldr = if (cap) List.newBuilder[A] else null
        var offset = state.offset

        while (true) {
          val a = p1.parseMut(state)
          if (state.error eq null) {
            if (cap) { bldr += a }
            offset = state.offset
          }
          else if (state.offset != offset) {
            // this is a partial parse, which is a failure
            return null
          }
          else {
            // return the latest
            state.error = null
            return if (cap) bldr.result() else null
          }
        }
        // $COVERAGE-OFF$
        sys.error("unreachable")
        // $COVERAGE-ON$
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
            if (cap) { bldr += a }
            offset = state.offset
          }
          else if (state.offset != offset) {
            // we partially consumed, this is an error
            return null
          }
          else if (cnt >= min) {
            // return the latest
            state.error = null
            return if (cap) NonEmptyList(a0, bldr.result()) else null
          }
          else {
            return null
          }
        }
        // $COVERAGE-OFF$
        sys.error("unreachable")
        // $COVERAGE-ON$
      }
    }

    def rangesFor(charArray: Array[Char]): NonEmptyList[(Char, Char)] = {
      def rangesFrom(start: Char, end: Char, idx: Int): NonEmptyList[(Char, Char)] =
        if (idx >= charArray.length || (idx < 0)) NonEmptyList((start, end), Nil)
        else {
          val end1 = charArray(idx)
          if ((end1.toInt == end.toInt + 1) || (end1 == end)) rangesFrom(start, end1, idx + 1)
          else {
            // we had a break:
            (start, end) :: rangesFrom(end1, end1, idx + 1)
          }
        }

      rangesFrom(charArray(0), charArray(0), 1)
    }

    case class CharIn(min: Int, bitSet: BitSetUtil.Tpe, ranges: NonEmptyList[(Char, Char)]) extends Parser1[Char] {

      def makeError(offset: Int): Error =
        Error.combined(ranges.map { case (s, e) => Expectation.InRange(offset, s, e).toError })

      override def parseMut(state: State): Char = {
        if (state.offset < state.str.length) {
          val char = state.str.charAt(state.offset)
          val cInt = char.toInt
          if (BitSetUtil.isSet(bitSet, cInt - min)) {
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

      def empty[A]: Parser[A] = Parser.Fail

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
