package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent }
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }
import cats.Functor

sealed abstract class Pattern[+T] {
  def map[U](fn: T => U): Pattern[U] =
    this match {
      case Pattern.WildCard => Pattern.WildCard
      case Pattern.Var(v) => Pattern.Var(v)
      case Pattern.PositionalStruct(name, params) =>
        Pattern.PositionalStruct(fn(name), params.map(_.map(fn)))
    }
}

object Pattern {
  implicit val patternFunctor: Functor[Pattern] =
    new Functor[Pattern] {
      def map[A, B](fa: Pattern[A])(fn: A => B): Pattern[B] =
        fa.map(fn)
    }

  case object WildCard extends Pattern[Nothing]
  case class Var(name: String) extends Pattern[Nothing]
  case class PositionalStruct[T](name: T, params: List[Pattern[T]]) extends Pattern[T]

  implicit lazy val document: Document[Pattern[String]] =
    Document.instance[Pattern[String]] {
      case WildCard => Doc.char('_')
      case Var(n) => Doc.text(n)
      case PositionalStruct(n, Nil) => Doc.text(n)
      case PositionalStruct(n, nonEmpty) =>
        Doc.text(n) +
          Doc.char('(') + Doc.intercalate(Doc.text(", "), nonEmpty.map(document.document(_))) + Doc.char(')')
    }

  lazy val parser: P[Pattern[String]] = {
    val recurse = P(parser) // this is lazy
    val pwild = P("_").map(_ => WildCard)
    val pvar = lowerIdent.map(Var(_))
    val positional = P(upperIdent ~/ (recurse.listN(1).parens).?)
      .map {
        case (n, None) => PositionalStruct(n, Nil)
        case (n, Some(ls)) => PositionalStruct(n, ls)
      }

    pvar | pwild | positional
  }
}

