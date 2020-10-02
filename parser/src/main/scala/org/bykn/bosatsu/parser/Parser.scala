package org.bykn.bosatsu.parser

import cats.{Eval, Monad, Defer, Alternative, FlatMap, Now}
import cats.data.NonEmptyList

/**
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

  def ~[B](that: Parser[B]): Parser[(A, B)] =
    Parser.product(this, that)

  def ~[B](that1: Parser1[B]): Parser1[(A, B)] =
    Parser.product01(this, that1)

  protected def parseMut(state: Parser.Impl.State): A
}

/**
 * Parses 1 or more characters to return A
 */
sealed abstract class Parser1[+A] extends Parser[A] {
  override def ~[B](that: Parser[B]): Parser1[(A, B)] =
    Parser.product10(this, that)
}

object Parser extends ParserInstances {
  sealed abstract class Expectation
  object Expectation {
    case class Str(offset: Int, str: String) extends Expectation
    case object Failure extends Expectation
  }
  sealed abstract class Error {
    def expected: NonEmptyList[Expectation]
  }
  object Error {
    case class MissedExpected(offset: Int, expectedString: String) extends Error {
      val expected = NonEmptyList(Expectation.Str(offset, expectedString), Nil)
    }

    case class Combined(errors: NonEmptyList[Error]) extends Error {
      def expected = errors.flatMap(_.expected)
    }

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

  def oneOf1[A](first: Parser1[A], rest: List[Parser1[A]]): Parser1[A] =
    rest match {
      case Nil => first
      case nel => Impl.OneOf1(first, nel)
    }

  def oneOf[A](ps: List[Parser[A]]): Parser[A] = {
    @annotation.tailrec
    def flatten(ls: List[Parser[A]], acc: List[Parser[A]]): Parser[A] =
      ls match {
        case Nil => Impl.OneOf(acc.reverse)
        case Impl.OneOf(ps) :: rest =>
          flatten(ps ::: rest, acc)
        case notOneOf :: rest =>
          flatten(rest, notOneOf :: acc)
      }

    flatten(ps, Nil)
  }

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

  def fail[A]: Parser[A] = oneOf(Nil)

  implicit val catsInstancesParser1: FlatMap[Parser1] with Defer[Parser1] =
    new FlatMap[Parser1] with Defer[Parser1] {
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
    }

  private object Impl {
    final class State(val str: String) {
      var offset: Int = 0
      var error: Error = null

      override def toString: String = s"State(str = $str, offset = $offset, error = $error)"
    }

    case class Pure[A](result: A) extends Parser[A] {
      override def parseMut(state: State): A = result
    }

    case class Expect(message: String) extends Parser1[Unit] {
      if (message.isEmpty) throw new IllegalArgumentException("we need a non-empty string to expect a message")

      override def parseMut(state: State): Unit = {
        if (state.str.regionMatches(state.offset, message, 0, message.length)) {
          state.offset += message.length
          ()
        }
        else {
          state.error = Error.MissedExpected(state.offset, message)
          ()
        }
      }
    }

    case class OneOf1[A](first: Parser1[A], rest: List[Parser1[A]]) extends Parser1[A] {
      private[this] val all = first :: rest

      override def parseMut(state: State): A = {
        var ps = all
        val offset = state.offset
        var errs: List[Error] = Nil
        while (ps.nonEmpty) {
          val thisParser = ps.head
          ps = ps.tail
          val res = thisParser.parseMut(state)
          if (state.error eq null) {
            return res
          }
          else {
            // we failed to parse, try the next
            errs = state.error :: errs
            state.error = null
            state.offset = offset
          }
        }
        // if we got here, all of them failed, and errs is nonEmpty
        state.offset = offset
        state.error = Error.Combined(NonEmptyList.fromListUnsafe(errs.reverse))
        null.asInstanceOf[A]
      }
    }

    case class OneOf[A](all: List[Parser[A]]) extends Parser[A] {
      override def parseMut(state: State): A = {
        var ps = all
        val offset = state.offset
        var errs: List[Error] = Nil
        while (ps.nonEmpty) {
          val thisParser = ps.head
          ps = ps.tail
          val res = thisParser.parseMut(state)
          if (state.error eq null) {
            return res
          }
          else {
            // we failed to parse, try the next
            errs = state.error :: errs
            state.error = null
            state.offset = offset
          }
        }
        // if we got here, all of them failed
        state.offset = offset
        state.error =
          NonEmptyList.fromList(errs.reverse) match {
            case None => Error.AlwaysFail
            case Some(errsNEL) => Error.Combined(errsNEL)
          }
        null.asInstanceOf[A]
      }
    }

    // we know that at least one of first | second is Parser1
    case class Prod1[A, B](first: Parser[A], second: Parser[B]) extends Parser1[(A, B)] {
      override def parseMut(state: State): (A, B) = {
        val a = first.parseMut(state)
        if (state.error eq null) {
          val b = second.parseMut(state)
          if (state.error eq null) (a, b)
          else null
        }
        else null
      }
    }

    case class Prod[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)] {
      override def parseMut(state: State): (A, B) = {
        val a = first.parseMut(state)
        if (state.error eq null) {
          val b = second.parseMut(state)
          if (state.error eq null) (a, b)
          else null
        }
        else null
      }
    }

    case class Map[A, B](parser: Parser[A], fn: A => B) extends Parser[B] {
      override def parseMut(state: State): B = {
        val a = parser.parseMut(state)
        if (state.error eq null) fn(a)
        else null.asInstanceOf[B]
      }
    }

    case class Map1[A, B](parser: Parser1[A], fn: A => B) extends Parser1[B] {
      override def parseMut(state: State): B = {
        val a = parser.parseMut(state)
        if (state.error eq null) fn(a)
        else null.asInstanceOf[B]
      }
    }

    case class FlatMap[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser[B] {
      override def parseMut(state: State): B = {
        val a = parser.parseMut(state)
        if (state.error eq null) {
          fn(a).parseMut(state)
        }
        else null.asInstanceOf[B]
      }
    }

    // at least one of the parsers needs to be a Parser1
    case class FlatMap1[A, B](parser: Parser[A], fn: A => Parser[B]) extends Parser1[B] {
      override def parseMut(state: State): B = {
        val a = parser.parseMut(state)
        if (state.error eq null) {
          fn(a).parseMut(state)
        }
        else null.asInstanceOf[B]
      }
    }

    case class TailRecM[A, B](init: A, fn: A => Parser[Either[A, B]]) extends Parser[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = {
        var p: Parser[Either[A, B]] = p1
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
    }

    case class TailRecM1[A, B](init: A, fn: A => Parser1[Either[A, B]]) extends Parser1[B] {
      private[this] val p1 = fn(init)

      override def parseMut(state: State): B = {
        var p: Parser[Either[A, B]] = p1
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
    }

    case class Defer1[A](fn: () => Parser1[A]) extends Parser1[A] {
      lazy val computed: Parser1[A] = fn() match {
        case c@Defer1(_) => c.computed
        case notDefer => notDefer
      }

      override def parseMut(state: State): A = computed.parseMut(state)
    }

    case class Defer[A](fn: () => Parser[A]) extends Parser[A] {
      lazy val computed: Parser[A] = fn() match {
        case c@Defer1(_) => c.computed
        case c@Defer(_) => c.computed
        case notDefer => notDefer
      }

      override def parseMut(state: State): A = computed.parseMut(state)
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
    }
}
