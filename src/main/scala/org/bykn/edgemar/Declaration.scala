package org.bykn.edgemar

import cats.data.NonEmptyList
import fastparse.all._
import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace }
import org.typelevel.paiges.Doc

sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    this match {
      case TypeVar(v) => Doc.text(v)
      case TypeName(n) => Doc.text(n)
      case TypeArrow(inner@TypeArrow(_, _), right) =>
        Doc.char('(') + inner.toDoc + Doc.char(')') + Doc.text(" -> ") + right.toDoc
      case TypeArrow(left, right) =>
        left.toDoc + Doc.text(" -> ") + right.toDoc
      case TypeApply(of, args) =>
        Doc.text(of.fold(_.asString, _.asString)) +
        Doc.char('[') + Doc.intercalate(Doc.text(", "), args.toList.map(_.toDoc)) + Doc.char(']')
    }
}

object TypeRef {
  case class TypeVar(asString: String) extends TypeRef {
    require(asString.charAt(0).isLower)
  }
  case class TypeName(asString: String) extends TypeRef {
    require(asString.charAt(0).isUpper)
  }
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: Either[TypeVar, TypeName], args: NonEmptyList[TypeRef]) extends TypeRef

  lazy val parser: P[TypeRef] = {
    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))
    val either: P[Either[TypeVar, TypeName]] = tvar.map(Left(_)) | tname.map(Right(_))

    val nonParens = either.flatMap { varOrName =>
      val uneither = varOrName.fold(v => v, n => n)
      val arrow = P(maybeSpace ~ "->" ~/ maybeSpace ~ parser).map(TypeArrow(uneither, _))
      val app = P("[" ~/ maybeSpace ~ parser.nonEmptyList ~ maybeSpace ~ "]").map(TypeApply(varOrName, _))
      val done = P("").map(_ => uneither)
      arrow | app | done
    }

    val parensArrow =
      P(parser.parens ~ (maybeSpace ~ "->" ~/ maybeSpace ~ parser).?)
        .map {
          case (p, None) => p
          case (a, Some(b)) => TypeArrow(a, b)
        }

    nonParens | parensArrow
  }
}

/**
 * Represents the syntax of declarations
 */
sealed abstract class Declaration

object Declaration {

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration]) extends Declaration
  case class Binding(name: String, value: Declaration, emptyLines: Int, in: Declaration) extends Declaration
  case class Comment(message: String, emptyLines: Int, on: Declaration) extends Declaration
  case class DefFn(name: String,
    args: NonEmptyList[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: Declaration) extends Declaration
  case class FfiLambda(lang: String, callsite: String, tpe: TypeRef) extends Declaration
  case class Lambda(style: String, args: NonEmptyList[String], body: Declaration) extends Declaration
  case class LiteralBool(toBoolean: Boolean) extends Declaration
  case class LiteralInt(asString: String) extends Declaration
  case class Package(name: String,
    exports: List[String],
    emptyLines: Int,
    decl: Declaration) extends Declaration
  case class Parens(of: Declaration) extends Declaration
  case class Var(name: String) extends Declaration
  case object EndOfFile extends Declaration

  val applyParser: P[Apply] =
    P(parser ~ parser.nonEmptyList.parens)
      .map { case (fn, args) => Apply(fn, args) }

  val bindingParser: P[Binding] = {
    val eqP = P("=" ~ !"=")
    P(lowerIdent ~ maybeSpace ~ eqP ~ maybeSpace ~ parser ~ ("\n").rep().! ~ parser)
      .map { case (name, value, lines, decl) => Binding(name, value, lines.length, decl) }
  }

  val commentP: P[Comment] =
    P("#" ~/ CharsWhile(_ != '\n').?.! ~ "\n".rep().! ~ parser)
      .map { case (m, ls, rest) => Comment(m, ls.size, rest) }

  def defP: P[DefFn] = {
    ???// P("def" ~ spaces ~ lowerIdent ~
  }

  val varParser: P[Var] =
    lowerIdent.map(Var(_))

  val eofParser: P[Declaration] = P(CharIn(" \t\n").rep() ~ End).map(_ => EndOfFile)

  lazy val parser: P[Declaration] =
    commentP | eofParser
}
