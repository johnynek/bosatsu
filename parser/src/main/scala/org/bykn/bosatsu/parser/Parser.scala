package org.bykn.bosatsu.parser

import cats.{Eval, Monad, Defer, Alternative, FlatMap, Now, MonoidK, Order}
import cats.data.{Chain, NonEmptyList}

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
    val offset = state.offset
    if (err eq null) Right((str.substring(offset), result))
    else Left(Parser.Error(offset, Parser.Expectation.unify(NonEmptyList.fromListUnsafe(err.toList))))
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

  /**
   * If we can parse this then that, do so,
   * if we fail that without consuming, rewind
   * before this without consuming.
   * If either consume 1 or more, do not rewind
   */
  def soft: Parser.Soft[A] = new Parser.Soft(this)

  /**
   * A parser that succeeds consuming nothing if this
   * current parser would fail
   */
  def unary_! : Parser[Unit] =
    Parser.not(this)

  /**
   * Convert this parser to one
   * that rewinds on success
   */
  def peek: Parser[Unit] =
    Parser.peek(this)

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

  def *>[B](that: Parser[B]): Parser1[B] =
    (this ~ that).map(_._2)

  def <*[B](that: Parser[B]): Parser1[A] =
    (this ~ that).map(_._1)


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
   * Repeat min, with min >= 0, or more times
   */
  def rep(min: Int): Parser[List[A]] =
    if (min == 0) rep
    else rep1(min).map(_.toList)

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

  /**
   * If we can parse this then that, do so,
   * if we fail that without consuming, rewind
   * before this without consuming.
   * If either consume 1 or more, do not rewind
   */
  override def soft: Parser.Soft1[A] = new Parser.Soft1(this)
}

object Parser extends ParserInstances {
  sealed abstract class Expectation {
    def offset: Int
  }

  object Expectation {
    case class Str(offset: Int, str: String) extends Expectation
    // expected a character in a given range
    case class InRange(offset: Int, lower: Char, upper: Char) extends Expectation
    case class StartOfString(offset: Int) extends Expectation
    case class EndOfString(offset: Int, length: Int) extends Expectation
    case class Length(offset: Int, expected: Int, actual: Int) extends Expectation
    case class ExpectedFailureAt(offset: Int, matched: String) extends Expectation
    // this is the result of oneOf(Nil) at a given location
    case class Fail(offset: Int) extends Expectation

    implicit val catsOrderExpectation: Order[Expectation] =
      new Order[Expectation] {
        def compare(left: Expectation, right: Expectation): Int = {
          val c = Integer.compare(left.offset, right.offset)
          if (c != 0) c
          else if (left == right) 0
          else {
            // these are never equal
            (left, right) match {
              case (Str(_, s1), Str(_, s2)) => s1.compare(s2)
              case (Str(_, _), _) => -1
              case (InRange(_, _, _), Str(_, _)) => 1
              case (InRange(_, l1, u1), InRange(_, l2, u2)) =>
                val c1 = Character.compare(l1, l2)
                if (c1 == 0) Character.compare(u1, u2)
                else c1
              case (InRange(_, _, _), _) => -1
              case (StartOfString(_), Str(_, _) | InRange(_, _, _)) => 1
              case (StartOfString(_), _) => -1 // if they have the same offset, already handled above
              case (EndOfString(_, _), Str(_, _) | InRange(_, _, _) | StartOfString(_)) => 1
              case (EndOfString(_, l1), EndOfString(_, l2)) =>
                Integer.compare(l1, l2)
              case (EndOfString(_, _), _) => -1
              case (Length(_, _, _), Str(_, _) | InRange(_, _, _) | StartOfString(_) | EndOfString(_, _)) => 1
              case (Length(_, e1, a1), Length(_, e2, a2)) =>
                val c1 = Integer.compare(e1, e2)
                if (c1 == 0) Integer.compare(a1, a2)
                else c1
              case (Length(_, _, _), _) => -1
              case (ExpectedFailureAt(_, _), Fail(_)) => -1
              case (ExpectedFailureAt(_, m1), ExpectedFailureAt(_, m2)) =>
                m1.compare(m2)
              case (ExpectedFailureAt(_, _), _) => 1
              case (Fail(_), _) => 1
            }
          }
        }
      }

    /**
     * Sort, dedup and unify ranges for the errors accumulated
     * This is called just before finally returning an error in Parser.parse
     */
    def unify(errors: NonEmptyList[Expectation]): NonEmptyList[Expectation] = {
      // merge all the ranges:
      val rangeMerge: List[InRange] =
        errors
          .toList
          .collect { case InRange(o, l, u) => (o, l to u) }
          .groupBy(_._1)
          .iterator
          .flatMap { case (o, ranges) =>
            // TODO: this could be optimized to not enumerate the set
            // for instance, a cheap thing to do is see if they
            // overlap or not
            val ary = ranges.iterator.map(_._2).flatten.toArray
            java.util.Arrays.sort(ary)
            Impl.rangesFor(ary)
              .map { case (l, u) => InRange(o, l, u) }
              .toList
          }
          .toList

      if (rangeMerge.isEmpty) errors.distinct.sorted
      else {
        val nonRanges = errors.toList.filterNot(_.isInstanceOf[InRange])

        NonEmptyList.fromListUnsafe(
          (rangeMerge reverse_::: nonRanges).distinct
        )
        .sorted
      }
    }
  }

  final case class Error(failedAtOffset: Int, expected: NonEmptyList[Expectation]) {
    def offsets: NonEmptyList[Int] =
      expected.map(_.offset).distinct
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

    def *>[B](that: Parser1[B]): Parser1[B] =
      product01(void(parser), that).map(_._2)

    def <*[B](that: Parser1[B]): Parser1[A] =
      product01(parser, void1(that)).map(_._1)

    /**
     * If we can parse this then that, do so,
     * if we fail that without consuming, rewind
     * before this without consuming.
     * If either consume 1 or more, do not rewind
     */
    def soft: Soft01[A] = new Soft01(parser)
  }

  /**
   * If we can parse this then that, do so,
   * if we fail that without consuming, rewind
   * before this without consuming.
   * If either consume 1 or more, do not rewind
   */
  sealed class Soft[+A](parser: Parser[A]) {
    def ~[B](that: Parser[B]): Parser[(A, B)] =
      softProduct(parser, that)

    def *>[B](that: Parser[B]): Parser[B] =
      softProduct(void(parser), that).map(_._2)

    def <*[B](that: Parser[B]): Parser[A] =
      softProduct(parser, void(that)).map(_._1)
  }
  final class Soft1[+A](parser: Parser1[A]) extends Soft(parser) {
    override def ~[B](that: Parser[B]): Parser1[(A, B)] =
      softProduct10(parser, that)

    override def *>[B](that: Parser[B]): Parser1[B] =
      softProduct10(void1(parser), that).map(_._2)

    override def <*[B](that: Parser[B]): Parser1[A] =
      softProduct10(parser, void(that)).map(_._1)
  }
  final class Soft01[+A](val parser: Parser[A]) extends AnyVal {
    def ~[B](that: Parser1[B]): Parser1[(A, B)] =
      softProduct01(parser, that)

    def *>[B](that: Parser1[B]): Parser1[B] =
      softProduct01(void(parser), that).map(_._2)

    def <*[B](that: Parser1[B]): Parser1[A] =
      softProduct01(parser, void1(that)).map(_._1)
  }

  def pure[A](a: A): Parser[A] =
    Impl.Pure(a)

  /**
   * Parse a given string or
   * fail. This backtracks on failure
   * this is an error if the string is empty
   */
  def string1(str: String): Parser1[Unit] =
    if (str.length == 1) char(str.charAt(0))
    else Impl.Str(str)

  /**
   * Parse a potentially empty string or
   * fail. This backtracks on failure
   */
  def string(str: String): Parser[Unit] =
    if (str.length == 0) unit
    else string1(str)

  def oneOf1[A](parsers: List[Parser1[A]]): Parser1[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser1[A]], acc: List[Parser1[A]]): List[Parser1[A]] =
      ls match {
        case Nil => acc.reverse.distinct
        case Impl.OneOf1(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.Fail() :: rest =>
          flatten(rest, acc)
        case notOneOf :: rest =>
          flatten(rest, notOneOf :: acc)
      }

    val flat = flatten(parsers, Nil)
    Impl.mergeCharIn[A, Parser1[A]](flat) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf1(two)
    }
  }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser[A]], acc: List[Parser[A]]): List[Parser[A]] =
      ls match {
        case Nil => acc.reverse.distinct
        case Impl.OneOf(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.OneOf1(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case Impl.Fail() :: rest =>
          flatten(rest, acc)
        case notOneOf :: rest =>
          flatten(rest, notOneOf :: acc)
      }

    val flat = flatten(ps, Nil)
    Impl.mergeCharIn[A, Parser[A]](flat) match {
      case Nil => fail
      case p :: Nil => p
      case two => Impl.OneOf(two)
    }
  }

  /**
   * if len < 1, the same as pure("")
   * else length1(len)
   */
  def length(len: Int): Parser[String] =
    if (len > 0) length1(len) else pure("")

  /**
   * Parse the next len characters where len > 0
   * if (len < 1) throw IllegalArgumentException
   */
  def length1(len: Int): Parser1[String] =
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

  /**
   *
   * Repeat 1 or more times with a separator
   */
  def rep1Sep[A](p1: Parser1[A], min: Int, sep: Parser[Any]): Parser1[NonEmptyList[A]] = {
    if (min <= 0) throw new IllegalArgumentException(s"require min > 0, found: $min")

    val rest = (sep.void.with1.soft *> p1).rep(min - 1)
    (p1 ~ rest).map { case (h, t) => NonEmptyList(h, t) }
  }

  /**
   * Repeat 0 or more times with a separator
   */
  def repSep[A](p1: Parser1[A], min: Int, sep: Parser[Any]): Parser[List[A]] = {
    if (min <= 0) rep1Sep(p1, 1, sep).?.map {
      case None => Nil
      case Some(nel) => nel.toList
    }
    else rep1Sep(p1, min, sep).map(_.toList)
  }

  def product10[A, B](first: Parser1[A], second: Parser[B]): Parser1[(A, B)] =
    Impl.Prod1(first, second)

  def product01[A, B](first: Parser[A], second: Parser1[B]): Parser1[(A, B)] =
    Impl.Prod1(first, second)

  def product[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] =
    Impl.Prod(first, second)

  def softProduct10[A, B](first: Parser1[A], second: Parser[B]): Parser1[(A, B)] =
    Impl.SoftProd1(first, second)

  def softProduct01[A, B](first: Parser[A], second: Parser1[B]): Parser1[(A, B)] =
    Impl.SoftProd1(first, second)

  def softProduct[A, B](first: Parser[A], second: Parser[B]): Parser[(A, B)] =
    Impl.SoftProd(first, second)

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

  val Fail: Parser1[Nothing] = Impl.Fail()
  def fail[A]: Parser1[A] = Fail

  val unit: Parser[Unit] = pure(())

  /**
   * Parse 1 character from the string
   */
  def anyChar: Parser1[Char] =
    Impl.AnyChar

  def char(c: Char): Parser1[Unit] =
    charIn(c).void

  def charIn(c0: Char, cs: Char*): Parser1[Char] =
    charIn(c0 :: cs.toList)

  /**
   * An empty iterable is the same as fail
   */
  def charIn(cs: Iterable[Char]): Parser1[Char] =
    if (cs.isEmpty) fail
    else {
      val ary = cs.toArray
      java.util.Arrays.sort(ary)
      Impl.rangesFor(ary) match {
        case NonEmptyList((low, high), Nil) if low == Char.MinValue && high == Char.MaxValue =>
          anyChar
        case notAnyChar =>
          Impl.CharIn(ary(0).toInt, BitSetUtil.bitSetFor(ary), notAnyChar)
      }
    }

  def charWhere(fn: Char => Boolean): Parser1[Char] =
    charIn(Impl.allChars.filter(fn))

  /**
   * Parse a string while the given function is true
   */
  def charsWhile(fn: Char => Boolean): Parser[String] =
    charWhere(fn).rep.string

  /**
   * Parse a string while the given function is true
   * parses at least one character
   */
  def charsWhile1(fn: Char => Boolean): Parser1[String] =
    charWhere(fn).rep1.string

  def until(p: Parser[Any]): Parser[String] =
    (not(p).with1 ~ anyChar).rep.string

  def void[A](pa: Parser[A]): Parser[Unit] =
    pa match {
      case v@Impl.Void(_) => v
      case Impl.StartParser => Impl.StartParser
      case Impl.EndParser => Impl.EndParser
      case n@Impl.Not(_) => n
      case p@Impl.Peek(_) => p
      case p1: Parser1[A] => void1(p1)
      case notVoid => Impl.Void(Impl.unmap(pa))
    }

  def void1[A](pa: Parser1[A]): Parser1[Unit] =
    pa match {
      case v@Impl.Void1(_) => v
      case p: Impl.Str => p
      case notVoid => Impl.Void1(Impl.unmap1(pa))
    }

  def string[A](pa: Parser[A]): Parser[String] =
    pa match {
      case str@Impl.StringP(_) => str
      case s1: Parser1[A] => string1(s1)
      case notStr => Impl.StringP(Impl.unmap(pa))
    }

  def string1[A](pa: Parser1[A]): Parser1[String] =
    pa match {
      case str@Impl.StringP1(_) => str
      case len@Impl.Length(_) => len
      case notStr => Impl.StringP1(Impl.unmap1(pa))
    }

  /**
   * returns a parser that succeeds if the
   * current parser fails.
   */
  def not(pa: Parser[Any]): Parser[Unit] =
    Impl.Not(void(pa))

  /**
   * a parser that consumes nothing when
   * it succeeds, basically rewind on success
   */
  def peek(pa: Parser[Any]): Parser[Unit] =
    // TODO: we can adjust Rep/Rep1 to do minimal
    // work since we rewind after we are sure there is
    // a match
    Impl.Peek(void(pa))

  /**
   * return the current position in the string
   * we are parsing. This lets you record position information
   * in your ASTs you are parsing
   */
  def index: Parser[Int] = Impl.Index

  // succeeds when we are at the start
  def start: Parser[Unit] = Impl.StartParser

  // succeeds when we are at the end
  def end: Parser[Unit] = Impl.EndParser

  /**
   * If we fail, rewind the offset back so that
   * we can try other branches. This tends
   * to harm debuggability and ideally should be
   * minimized
   */
  def backtrack[A](pa: Parser[A]): Parser[A] =
    pa match {
      case bt: Impl.Backtrack[A] => bt
      case p1: Parser1[A] => backtrack1(p1)
      case Impl.StartParser | Impl.EndParser | Impl.Index | Impl.Pure(_) =>
        // these already backtrack (either nothing or everything)
        pa
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
      case Impl.AnyChar | Impl.CharIn(_, _, _) | Impl.Str(_) | Impl.Length(_) =>
        // these already backtrack (either nothing or everything)
        pa
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

      override def as[A, B](pa: Parser1[A], b: B): Parser1[B] =
        pa.void.map(_ => b)

      override def productL[A, B](pa: Parser1[A])(pb: Parser1[B]): Parser1[A] =
        map(product(pa, pb.void)) { case (a, _) => a }

      override def productR[A, B](pa: Parser1[A])(pb: Parser1[B]): Parser1[B] =
        map(product(pa.void, pb)) { case (_, b) => b }
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
        case n@Not(_) =>
          // not is already voided
          n
        case p@Peek(_) =>
          // peek is already voided
          p
        case Backtrack(p) => Backtrack(unmap(p))
        case OneOf(ps) => OneOf(ps.map(unmap))
        case Prod(p1, p2) => Prod(unmap(p1), unmap(p2))
        case SoftProd(p1, p2) => SoftProd(unmap(p1), unmap(p2))
        case Defer(fn) =>
          Defer(() => unmap(compute(fn)))
        case Rep(p) => Rep(unmap1(p))
        case Pure(_) => Parser.unit
        case Index | StartParser | EndParser | TailRecM(_, _) | FlatMap(_, _) =>
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
        case SoftProd1(p1, p2) => SoftProd1(unmap(p1), unmap(p2))
        case Defer1(fn) =>
          Defer1(() => unmap1(compute1(fn)))
        case Rep1(p, m) => Rep1(unmap1(p), m)
        case AnyChar | CharIn(_, _, _) | Str(_) | Fail() | Length(_) | TailRecM1(_, _) | FlatMap1(_, _) =>
          // we can't transform this significantly
          pa

      }

    final class State(val str: String) {
      var offset: Int = 0
      var error: Chain[Expectation] = null
      var capture: Boolean = true
    }

    case class Pure[A](result: A) extends Parser[A] {
      override def parseMut(state: State): A = result
    }

    case class Length(len: Int) extends Parser1[String] {
      if (len < 1) throw new IllegalArgumentException(s"required length > 0, found $len")

      override def parseMut(state: State): String = {
        val offset = state.offset
        val end = offset + len
        if (end <= state.str.length) {
          val res = if (state.capture) state.str.substring(offset, end) else null
          state.offset = end
          res
        }
        else {
          state.error = Chain.one(Expectation.Length(offset, len, state.str.length - offset))
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
          state.error = Chain.one(Expectation.StartOfString(state.offset))
        }
        ()
      }
    }

    case object EndParser extends Parser[Unit] {
      override def parseMut(state: State): Unit = {
        if (state.offset != state.str.length) {
          state.error = Chain.one(Expectation.EndOfString(state.offset, state.str.length))
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

    case class Str(message: String) extends Parser1[Unit] {
      if (message.isEmpty) throw new IllegalArgumentException("we need a non-empty string to expect a message")

      override def parseMut(state: State): Unit = {
        val offset = state.offset
        if (state.str.regionMatches(offset, message, 0, message.length)) {
          state.offset += message.length
          ()
        }
        else {
          state.error = Chain.one(Expectation.Str(offset, message))
          ()
        }
      }
    }

    case class Fail[A]() extends Parser1[A] {
      override def parseMut(state: State): A = {
        state.error = Chain.one(Expectation.Fail(state.offset));
        null.asInstanceOf[A]
      }
    }

    final def oneOf[A](all: List[Parser[A]], state: State): A = {
      var ps = all
      val offset = state.offset
      var errs: Chain[Expectation] = Chain.nil
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
          errs = errs ++ state.error
          state.error = null
        }
      }
      // if we got here, all of them failed, but we
      // never advanced the offset
      state.error = errs
      null.asInstanceOf[A]
    }

    case class OneOf1[A](all: List[Parser1[A]]) extends Parser1[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
      override def parseMut(state: State): A = oneOf(all, state)
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      require(all.lengthCompare(2) >= 0, s"expected more than two items, found: ${all.size}")
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
      require(first.isInstanceOf[Parser1[_]] || second.isInstanceOf[Parser1[_]])
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    case class Prod[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)] {
      override def parseMut(state: State): (A, B) = prod(first, second, state)
    }

    final def softProd[A, B](pa: Parser[A], pb: Parser[B], state: State): (A, B) = {
      val offset = state.offset
      val a = pa.parseMut(state)
      if (state.error eq null) {
        val offseta = state.offset
        val b = pb.parseMut(state)
        // pa passed, if pb fails without consuming, rewind to offset
        if (state.error ne null) {
          if (state.offset == offseta) {
            state.offset = offset
          }
          // else partial parse of b, don't rewind
          null
        }
        else if (state.capture) (a, b)
        else null
      }
      else null
    }

    // we know that at least one of first | second is Parser1
    case class SoftProd1[A, B](first: Parser[A], second: Parser[B]) extends Parser1[(A, B)] {
      require(first.isInstanceOf[Parser1[_]] || second.isInstanceOf[Parser1[_]])
      override def parseMut(state: State): (A, B) = softProd(first, second, state)
    }

    case class SoftProd[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)] {
      override def parseMut(state: State): (A, B) = softProd(first, second, state)
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
      // we have to capture
      val c0 = state.capture
      state.capture = true
      while (state.error eq null) {
        val res = p.parseMut(state)
        if (state.error eq null) {
          res match {
            case Right(b) =>
              state.capture = c0
              return b
            case Left(a) =>
              p = fn(a)
          }
        }
      }
      state.capture = c0
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

    final def rep[A](p: Parser1[A], min: Int, state: State): List[A] = {
      val cap = state.capture
      val bldr = if (cap) List.newBuilder[A] else null
      var offset = state.offset
      var cnt = 0

      while (true) {
        val a = p.parseMut(state)
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
          return if (cap) bldr.result() else null
        }
        else {
          return null
        }
      }
      // $COVERAGE-OFF$
      sys.error("unreachable")
      // $COVERAGE-ON$
    }

    case class Rep[A](p1: Parser1[A]) extends Parser[List[A]] {
      override def parseMut(state: State): List[A] = Impl.rep(p1, 0, state)
    }

    case class Rep1[A](p1: Parser1[A], min: Int) extends Parser1[NonEmptyList[A]] {
      if (min < 1) throw new IllegalArgumentException(s"expected min >= 1, found: $min")

      override def parseMut(state: State): NonEmptyList[A] = {
        val head = p1.parseMut(state)

        if (state.error ne null) null
        else {
          val tail = Impl.rep(p1, min - 1, state)
          if (state.capture) NonEmptyList(head, tail) else null
        }
      }
    }

    // invariant: input must be sorted
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

    /*
     * Merge CharIn bitsets
     */
    def mergeCharIn[A, P <: Parser[A]](ps: List[P]): List[P] = {
      def loop(ps: List[P], front: List[(Int, BitSetUtil.Tpe)]): List[P] = {
        @inline
        def frontRes: List[P] =
          front match {
            case Nil => Nil
            case nel => Parser.charIn(BitSetUtil.union(nel)).asInstanceOf[P] :: Nil
          }

        ps match {
          case Nil => frontRes
          case AnyChar :: tail =>
            // AnyChar is bigger than all subsequent CharIn:
            // and any direct prefix CharIns
            val tail1 = tail.filterNot(_.isInstanceOf[CharIn])
            AnyChar.asInstanceOf[P] :: tail1
          case CharIn(m, bs, _) :: tail =>
            loop(tail, (m, bs) :: front)
          case h :: tail =>
            // h is not an AnyChar or CharIn
            // we make our prefix frontRes
            // and resume working on the tail
            frontRes ::: (h :: loop(tail, Nil))
        }
      }

      loop(ps, Nil)
    }

    case object AnyChar extends Parser1[Char] {
      override def parseMut(state: State): Char = {
        val offset = state.offset
        if (offset < state.str.length) {
          val char = state.str.charAt(offset)
          state.offset += 1
          char
        }
        else {
          state.error = Chain.one(Expectation.InRange(offset, Char.MinValue, Char.MaxValue))
          '\u0000'
        }
      }
    }

    case class CharIn(min: Int, bitSet: BitSetUtil.Tpe, ranges: NonEmptyList[(Char, Char)]) extends Parser1[Char] {

      override def toString = s"CharIn($min, bitSet = ..., $ranges)"

      def makeError(offset: Int): Chain[Expectation] =
        Chain.fromSeq(ranges.toList.map { case (s, e) => Expectation.InRange(offset, s, e) })

      override def parseMut(state: State): Char = {
        val offset = state.offset
        if (offset < state.str.length) {
          val char = state.str.charAt(offset)
          val cInt = char.toInt
          if (BitSetUtil.isSet(bitSet, cInt - min)) {
            // we found the character
            state.offset += 1
            char
          }
          else {
            state.error = makeError(offset)
            '\u0000'
          }
        }
        else {
          state.error = makeError(offset)
          '\u0000'
        }
      }
    }

    /*
     * If pa fails, succeed parsing nothing
     * else fail
     */
    case class Not(under: Parser[Unit]) extends Parser[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        under.parseMut(state)
        if (state.error ne null) {
          // under failed, so we succeed
          state.error = null
          state.offset = offset
        }
        else {
          // under succeeded but we expected failure here
          val matchedStr = state.str.substring(offset, state.offset)
          // we don't reset the offset, so if the underlying parser
          // advanced it will fail in a OneOf
          state.error = Chain.one(Expectation.ExpectedFailureAt(offset, matchedStr))
          state.offset = offset
        }

        ()
      }
    }

    /*
     * succeeds if the underlying parser succeeds, but we do
     * not advance
     */
    case class Peek(under: Parser[Unit]) extends Parser[Unit] {
      override def parseMut(state: State): Unit = {
        val offset = state.offset
        under.parseMut(state)
        if (state.error eq null) {
          // under passed, so we succeed
          state.offset = offset
        }
        // else under failed, so we fail
        ()
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

      override def as[A, B](pa: Parser[A], b: B): Parser[B] =
        Parser.void(pa).map(_ => b)

      override def productL[A, B](pa: Parser[A])(pb: Parser[B]): Parser[A] =
        map(product(pa, pb.void)) { case (a, _) => a }

      override def productR[A, B](pa: Parser[A])(pb: Parser[B]): Parser[B] =
        map(product(pa.void, pb)) { case (_, b) => b }
    }
}
