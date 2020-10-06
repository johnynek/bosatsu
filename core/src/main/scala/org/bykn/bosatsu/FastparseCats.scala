package org.bykn.bosatsu

/*
import cats.{Alternative, Eval, Now, Monad}
import fastparse.Api
import fastparse.core
import fastparse.core.{ParserApi, ParserApiImpl}

object FastParseCats {

  /**
   * This is a module that gives the instances for any Elem or Repr
   */
  abstract class FastParseCatsGeneric[Elem, Repr] {

    val api: Api[Elem, Repr]

    import api._
    import fastparse.core.{ParseCtx, Mutable}

    private implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Elem, Repr]): ParserApi[V, Elem, Repr] =
      new ParserApiImpl[V, Elem, Repr](p)

    /**
     * This creates a namespace so we don't pollute
     * when users do import StringInstances._
     */
    object buildParser {
      def pure[A](a: A): Parser[A] =
        PassWith(a)

      def fromEval[A](e: Eval[A]): Parser[A] =
        e match {
          case Now(a) => pure(a)
          case notNow => delay(notNow.value)
        }
      /**
       * From an Eval, wait to build the Parser until we actually need to parse
       * if it is not already constructed.
       *
       * Note, we always store the value and only call .value on time since parsers
       * are often applied at many locations.
       */
      def fromEvalParser[A](e: Eval[Parser[A]]): Parser[A] =
        e match {
          case Now(p) => p
          case notNow => suspend(notNow.value)
        }

      /**
       * Create a lazy value that is only evaluate when we parse
       */
      def delay[A](a: => A): Parser[A] =
        suspend(PassWith(a))

      /**
       * wait to build the Parser until we actually need to parse.
       *
       * Note, we always store the value and only call the thunk once
       * since Parsers are often applied at many locations.
       */
      def suspend[A](thunk: => Parser[A]): Parser[A] =
        new Parser[A] {
          private[this] lazy val parser = thunk

          def parseRec(cfg: ParseCtx[Elem, Repr], index: Int): Mutable[A, Elem, Repr] =
            parser.parseRec(cfg, index)
        }
    }

    /**
     * This gives you a lawful Monad and Alternative, which allows you to write
     * many parser combinators purely on Monad or Alternative.
     */
    implicit val monadParser: Monad[Parser] with Alternative[Parser] = new Monad[Parser] with Alternative[Parser] {
      def empty[A]: Parser[A] = Fail
      def combineK[A](left: Parser[A], right: Parser[A]): Parser[A] = left | right

      def pure[A](a: A): Parser[A] = PassWith(a)
      override def unit = Pass

      override def map[A, B](fa: Parser[A])(fn: A => B): Parser[B] =
        fa.map(fn)

      override def map2Eval[A, B, C](fa: Parser[A], fb: Eval[Parser[B]])(fn: (A, B) => C): Eval[Parser[C]] =
        Eval.later(map2(fa, buildParser.fromEvalParser(fb))(fn))

      override def replicateA[A](cnt: Int, fa: Parser[A]): Parser[List[A]] =
        fa.rep(cnt).map(_.toList)

      override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
        fa ~ fb

      def flatMap[A, B](fa: Parser[A])(fn: A => Parser[B]): Parser[B] =
        fa.flatMap(fn)

      def tailRecM[A, B](a: A)(fn: A => Parser[Either[A, B]]): Parser[B] =
        new Parser[B] {
          def parseRec(cfg: ParseCtx[Elem, Repr], index: Int): Mutable[B, Elem, Repr] = {
            @annotation.tailrec
            def loop(a: A, idx: Int): Mutable[B, Elem, Repr] =
              fn(a).parseRec(cfg, idx) match {
                case Mutable.Success(Right(b), i, t, c) =>
                  Mutable.Success(b, i, t, c)
                case Mutable.Success(Left(nexta), nextIdx, _, _) =>
                  loop(nexta, nextIdx)
                case f: Mutable.Failure[_, _] => f
                case subclass =>
                  subclass.toResult match {
                    case Parsed.Success(Right(b), i) =>
                      Mutable.Success(b, i, subclass.traceParsers, subclass.cut)
                    case Parsed.Success(Left(nexta), nextIdx) =>
                      loop(nexta, nextIdx)
                    case Parsed.Failure(_, _, _) =>
                      // this cast is safe because Failure has type Nothing
                      subclass.asInstanceOf[Mutable[B, Elem, Repr]]
                  }
              }

            loop(a, index)
          }
        }
    }
  }

  object StringInstances extends FastParseCatsGeneric[Char, String] {
    val api = fastparse.all
  }
}
*/
