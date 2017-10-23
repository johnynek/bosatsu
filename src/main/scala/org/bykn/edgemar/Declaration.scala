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
        Doc.char('(') + inner.toDoc + Doc.char(')') + spaceArrow + right.toDoc
      case TypeArrow(left, right) =>
        left.toDoc + spaceArrow + right.toDoc
      case TypeApply(of, args) =>
        of.toDoc + Doc.char('[') + Doc.intercalate(commaSpace, args.toList.map(_.toDoc)) + Doc.char(']')
    }
}

object TypeRef {
  private val spaceArrow = Doc.text(" -> ")
  private val commaSpace = Doc.text(", ")

  case class TypeVar(asString: String) extends TypeRef {
    require(asString.charAt(0).isLower)
  }
  case class TypeName(asString: String) extends TypeRef {
    require(asString.charAt(0).isUpper)
  }
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  lazy val parser: P[TypeRef] = {
    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))

    val maybeArrow: P[TypeRef => TypeRef] =
      P((maybeSpace ~ "->" ~/ maybeSpace ~ parser))
        .map { right => TypeArrow(_, right) }

    val maybeApp: P[TypeRef => TypeRef] =
      P(("[" ~/ maybeSpace ~ parser.nonEmptyList ~ maybeSpace ~ "]"))
        .map { args => TypeApply(_, args) }

    ((tvar | tname | P(parser.parens)) ~ maybeApp.? ~ maybeArrow.?)
      .map {
        case (t, optF1, optF2) =>
          val t1 = optF1.fold(t)(_(t))
          optF2.fold(t1)(_(t1))
      }
  }
}

/**
 * Represents the syntax of declarations
 */
sealed abstract class Declaration {
  import Declaration._

  def toDoc: Doc = {

    this match {
      case Comment(lines, empties, on) =>
        val block = Doc.intercalate(Doc.line, lines.toList.map { mes => commentPrefix + Doc.text(mes) })
        val emptyLines = Doc.line.repeat(empties + 1) // add 1 line for the comment
        block + emptyLines + on.toDoc
      case LiteralBool(b) => if (b) trueDoc else falseDoc
      case LiteralInt(str) => Doc.text(str)
      case Var(name) => Doc.text(name)
      case EndOfFile => Doc.empty
      case _ => ???
    }
  }
}

object Declaration {
  private val commentPrefix = Doc.char('#')
  private val trueDoc = Doc.text("True")
  private val falseDoc = Doc.text("False")

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration]) extends Declaration
  case class Binding(name: String, value: Declaration, emptyLines: Int, in: Declaration) extends Declaration
  case class Comment(message: NonEmptyList[String], emptyLines: Int, on: Declaration) extends Declaration
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

  val commentP: P[Comment] = {
    val commentPart: P[String] = P("#" ~/ CharsWhile(_ != '\n').?.!)
    val commentBlock: P[NonEmptyList[String]] =
      P(commentPart ~ ("\n" ~ commentPart).rep() ~ ("\n" | End))
        .map { case (c1, cs) => NonEmptyList(c1, cs.toList) }

    val newLines: P[Int] = P("\n".rep().!).map(_.length)

    P(commentBlock ~ newLines ~ parser)
      .map { case (m, ls, rest) => Comment(m, ls, rest) }
  }

  def defP: P[DefFn] = {
    ???// P("def" ~ spaces ~ lowerIdent ~
  }

  val literalBoolP: P[LiteralBool] =
    Parser.tokenP("True", LiteralBool(true)) | Parser.tokenP("False", LiteralBool(false))

  val literalIntP: P[LiteralInt] =
    Parser.integerString.map(LiteralInt(_))

  val varP: P[Var] =
    lowerIdent.map(Var(_))

  val eofParser: P[Declaration] = P(CharIn(" \t\n").rep() ~ End).map(_ => EndOfFile)

  lazy val parser: P[Declaration] =
    literalIntP | literalBoolP | varP | commentP | eofParser
}
