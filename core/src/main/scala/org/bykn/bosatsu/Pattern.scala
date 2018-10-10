package org.bykn.bosatsu

import cats.Applicative
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace }
import cats.implicits._

sealed abstract class Pattern[+N, +T] {
  def mapName[U](fn: N => U): Pattern[U, T] =
    this match {
      case Pattern.WildCard => Pattern.WildCard
      case Pattern.Var(v) => Pattern.Var(v)
      case Pattern.Annotation(p, tpe) => Pattern.Annotation(p.mapName(fn), tpe)
      case Pattern.PositionalStruct(name, params) =>
        Pattern.PositionalStruct(fn(name), params.map(_.mapName(fn)))
    }
  def mapType[U](fn: T => U): Pattern[N, U] =
    this match {
      case Pattern.WildCard => Pattern.WildCard
      case Pattern.Var(v) => Pattern.Var(v)
      case Pattern.Annotation(p, tpe) => Pattern.Annotation(p.mapType(fn), fn(tpe))
      case Pattern.PositionalStruct(name, params) =>
        Pattern.PositionalStruct(name, params.map(_.mapType(fn)))
    }
}

object Pattern {

  implicit class InvariantPattern[N, T](val pat: Pattern[N, T]) extends AnyVal {
    def traverseType[F[_]: Applicative, T1](fn: T => F[T1]): F[Pattern[N, T1]] =
      pat match {
        case Pattern.WildCard => Applicative[F].pure(Pattern.WildCard)
        case Pattern.Var(v) => Applicative[F].pure(Pattern.Var(v))
        case Pattern.Annotation(p, tpe) =>
          (p.traverseType(fn), fn(tpe)).mapN(Pattern.Annotation(_, _))
        case Pattern.PositionalStruct(name, params) =>
          params.traverse(_.traverseType(fn)).map { ps =>
            Pattern.PositionalStruct(name, ps)
          }
      }
  }

  case object WildCard extends Pattern[Nothing, Nothing]
  case class Var(name: String) extends Pattern[Nothing, Nothing]
  case class Annotation[N, T](pattern: Pattern[N, T], tpe: T) extends Pattern[N, T]
  case class PositionalStruct[N, T](name: N, params: List[Pattern[N, T]]) extends Pattern[N, T]

  implicit lazy val document: Document[Pattern[String, TypeRef]] =
    Document.instance[Pattern[String, TypeRef]] {
      case WildCard => Doc.char('_')
      case Var(n) => Doc.text(n)
      case Annotation(_, _) =>
        /*
         * We need to know what package we are in and what imports we depend on here.
         * This creates some challenges we need to deal with:
         *   1. how do we make sure we don't have duplicate short names
         *   2. how do we make sure we have imported the names we need
         *   3. at the top level we need parens to distinguish a: Integer from being the rhs of a
         *      case
         */
        ???
      case PositionalStruct(n, Nil) => Doc.text(n)
      case PositionalStruct(n, nonEmpty) =>
        Doc.text(n) +
          Doc.char('(') + Doc.intercalate(Doc.text(", "), nonEmpty.map(document.document(_))) + Doc.char(')')
    }

  lazy val parser: P[Pattern[String, TypeRef]] = {

    def go(isTop: Boolean): P[Pattern[String, TypeRef]] = {
      val recurse = P(go(false)) // this is lazy

      val pwild = P("_").map(_ => WildCard)
      val pvar = lowerIdent.map(Var(_))
      val pparen = recurse.parens

      val positional = P(upperIdent ~/ (recurse.listN(1).parens).?)
        .map {
          case (n, None) => PositionalStruct(n, Nil)
          case (n, Some(ls)) => PositionalStruct(n, ls)
        }

      val nonAnnotated = pvar | pwild | pparen | positional
      val typeAnnot = P(maybeSpace ~ ":" ~/ maybeSpace ~ TypeRef.parser)
      val withType = (nonAnnotated ~ typeAnnot.?).map {
        case (p, None) => p
        case (p, Some(t)) => Annotation(p, t)
      }

      // We only allow type annotation not at the top level, must be inside
      // Struct or parens
      if (isTop) nonAnnotated
      else withType
    }

    go(true)
  }
}

