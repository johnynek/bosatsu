package org.bykn.edgemar

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import fastparse.all._
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
  private val colonSpace = Doc.text(": ")

  def argDoc(st: (String, Option[TypeRef])): Doc =
    st match {
      case (s, None) => Doc.text(s)
      case (s, Some(tr)) => Doc.text(s) + colonSpace + (tr.toDoc)
    }

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
      case Apply(fn, args) =>
        val fnDoc = fn match {
          case Var(n) => Doc.text(n)
          case other => Doc.char('(') + other.toDoc + Doc.char(')')
        }
        fnDoc + Doc.char('(') + Doc.intercalate(Doc.text(", "), args.toList.map(_.toDoc)) + Doc.char(')')
      case Binding(name, value, lines, in) =>
        Doc.text(name) + eqDoc + value.toDoc + Doc.line.repeat(lines) + (in.fold(Doc.empty)(_.toDoc))
      case Comment(lines, empties, on) =>
        val block = Doc.intercalate(Doc.line, lines.toList.map { mes => Doc.char('#') + Doc.text(mes) })
        val emptyLines = Doc.line.repeat(empties + 1) // add 1 line for the comment
        block + emptyLines + on.fold(Doc.empty)(_.toDoc)
      case DefFn(name, args, retType, result) =>
        val res = retType.fold(Doc.empty) { t => Doc.text(" -> ") + t.toDoc }
        val line0 = defDoc + Doc.text(name) + Doc.char('(') +
          Doc.intercalate(Doc.text(", "), args.toList.map(TypeRef.argDoc _)) +
          Doc.char(')') + res + Doc.text(":") + Doc.line
        (line0 + result.toDoc).nested(4)
      case LiteralBool(b) => if (b) trueDoc else falseDoc
      case LiteralInt(str) => Doc.text(str)
      case Op(left, op, right) =>
        left.toDoc + Doc.space + Doc.text(op.asString) + Doc.space + right.toDoc
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case Var(name) => Doc.text(name)
      case _ => ???
    }
  }
}

object Declaration {
  private val trueDoc = Doc.text("True")
  private val falseDoc = Doc.text("False")
  private val defDoc = Doc.text("def ")
  private val eqDoc = Doc.text(" = ")

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration]) extends Declaration
  // in == None means end of file
  case class Binding(name: String, value: Declaration, emptyLines: Int, in: Option[Declaration]) extends Declaration
  // on == None means end of file
  case class Comment(message: NonEmptyList[String], emptyLines: Int, on: Option[Declaration]) extends Declaration
  case class DefFn(name: String,
    args: NonEmptyList[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: Declaration) extends Declaration
  case class FfiLambda(lang: String, callsite: String, tpe: TypeRef) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration) extends Declaration
  case class LiteralBool(toBoolean: Boolean) extends Declaration
  case class LiteralInt(asString: String) extends Declaration
  case class Package(name: String,
    exports: List[String],
    emptyLines: Int,
    decl: Declaration) extends Declaration
  case class Op(left: Declaration, op: Operator, right: Declaration) extends Declaration
  case class Parens(of: Declaration) extends Declaration
  case class Var(name: String) extends Declaration

  // This is something we check after variables
  private def bindingOp(indent: String): P[String => Binding] = {
    val eqP = P("=" ~ !"=")
    val eofP: P[Option[Declaration]] = P(End).map(_ => None)

    val lines: P[Int] = P(End).map(_ => 0) | P((maybeSpace ~ "\n").!.rep(1)).map(_.size)
    P(maybeSpace ~ eqP ~/ maybeSpace ~ parser(indent) ~ lines ~ ((indent ~ parser(indent)).? | eofP))
      .map { case (value, lines, decl) => Binding(_, value, lines, decl) }
  }

  def commentP(indent: String): P[Comment] = {
    val commentPart: P[String] = P("#" ~/ CharsWhile(_ != '\n').?.!)
    val commentBlock: P[NonEmptyList[String]] =
      P(commentPart ~ ("\n" ~ indent ~ commentPart).rep() ~ ("\n" | End))
        .map { case (c1, cs) => NonEmptyList(c1, cs.toList) }

    val newLines: P[Int] = P((maybeSpace ~ "\n").!.rep()).map(_.size)

    val eofP: P[Option[Declaration]] = P(End).map(_ => None)

    P(commentBlock ~ newLines ~ indent ~ (parser(indent).? | eofP) )
      .map { case (m, ls, rest) => Comment(m, ls, rest) }
  }

  def defP(indent: String): P[DefFn] = {
    val args = P(lowerIdent ~ (":" ~/ maybeSpace ~ TypeRef.parser).?).nonEmptyList
    val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser ~ maybeSpace).?
    P("def" ~ spaces ~/ Parser.lowerIdent ~ "(" ~ maybeSpace ~ args ~ maybeSpace ~ ")" ~
      result ~ ":" ~ ("\n" ~ indent).rep(1) ~ spaces.!)
      .flatMap {
        case (name, args, res, nextIndent) =>
          parser(nextIndent).map(DefFn(name, args, res, _))
      }
  }

  val literalBoolP: P[LiteralBool] =
    Parser.tokenP("True", LiteralBool(true)) | Parser.tokenP("False", LiteralBool(false))

  val literalIntP: P[LiteralInt] =
    Parser.integerString.map(LiteralInt(_))

  val varP: P[Var] =
    lowerIdent.map(Var(_))

  private def varOrBind(indent: String): P[Declaration] =
    P(varP ~ bindingOp(indent).?)
      .map {
        case (v, None) => v
        case (Var(v), Some(fn)) => fn(v)
      }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val postOperators: List[P[Declaration => Declaration]] = {
        val applySuffix = P(rec(indent).nonEmptyList.parens).map { args => Apply(_: Declaration, args) }

        def parseOp(o: Operator): P[Declaration => Declaration] =
          P(maybeSpace ~ o.asString ~/ maybeSpace ~ rec(indent)).map { right => Op(_, o, right) }

        applySuffix :: Operator.allOps.map(parseOp _)
      }
      val prefix = defP(indent) | literalIntP | literalBoolP | varOrBind(indent) | commentP(indent) |
        P(rec(indent).parens).map(Parens(_))

      def checkOps(head: P[Declaration], ops: List[P[Declaration => Declaration]]): P[Declaration] =
        ops match {
          case Nil => head
          case h :: tail =>
            val h1 = P(head ~ h.?).map {
              case (h, None) => h
              case (h, Some(f)) => f(h)
            }
            checkOps(h1, tail)
        }

      checkOps(prefix, postOperators)
    }

  def parser(indent: String): P[Declaration] =
    parserCache(indent)
}
