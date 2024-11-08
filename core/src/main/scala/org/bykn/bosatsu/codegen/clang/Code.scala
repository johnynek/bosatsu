package org.bykn.bosatsu.codegen.clang

import org.typelevel.paiges.Doc
import cats.data.NonEmptyList
import scala.language.implicitConversions

sealed trait Code

object Code {
  sealed trait Attr
  object Attr {
    case object Static extends Attr

    private val staticDoc = Doc.text("static")

    def toDoc(a: Attr): Doc =
      a match {
        case Static => staticDoc
      }
  }

  sealed trait TypeIdent {
    def ptr: TypeIdent = TypeIdent.Ptr(this)

    def typedefAs(n: Ident): Statement = Typedef(this, n)

    def cast(expr: Expression): Expression = Cast(this, expr)
  }
  private val asterisk = Doc.char('*')

  object TypeIdent {
    sealed trait ComplexType extends TypeIdent
    case class StructType(name: String) extends ComplexType
    case class UnionType(name: String) extends ComplexType
    case class Named(name: String) extends TypeIdent
    case class Ptr(tpe: TypeIdent) extends TypeIdent

    private val structDoc = Doc.text("struct ")
    private val unionDoc = Doc.text("union ")

    def toDoc(te: TypeIdent): Doc =
      te match {
        case StructType(n) => structDoc + Doc.text(n)
        case UnionType(n) => unionDoc + Doc.text(n)
        case Named(n) => Doc.text(n)
        case Ptr(ptr) => toDoc(ptr) + asterisk
      }
  }

  sealed trait Expression extends Code {
    def :=(rhs: Expression): Statement =
      Assignment(this, rhs)

    def ret: Statement = Return(Some(this))

    def apply(args: Expression*): Apply = Apply(this, args.toList)

    def select(i: Ident): Select = Select(this, i)

    def deref: Expression = Deref(this)

    def bracket(arg: Expression): Expression = Bracket(this, arg)

    def addr: Expression = AddrOf(this)
  }

  case class Ident(name: String) extends Expression
  object Ident {
    implicit def fromString(str: String): Ident = Ident(str)
  }
  case class IntLiteral(value: BigInt) extends Expression
  case class Cast(tpe: TypeIdent, expr: Expression) extends Expression
  case class Apply(fn: Expression, args: List[Expression]) extends Expression with Statement
  case class Select(target: Expression, name: Ident) extends Expression
  case class Deref(targets: Expression) extends Expression
  case class Bracket(target: Expression, item: Expression) extends Expression
  case class AddrOf(targets: Expression) extends Expression


  case class Param(tpe: TypeIdent, name: Ident) {
    def toDoc: Doc = TypeIdent.toDoc(tpe) + Doc.space + Doc.text(name.name)
  }

  sealed trait Statement extends Code
  case class Assignment(target: Expression, value: Expression) extends Statement
  case class DeclareArray(tpe: TypeIdent, ident: Ident, values: Either[Int, List[Expression]]) extends Statement
  case class DeclareVar(attrs: List[Attr], tpe: TypeIdent, ident: Ident, value: Option[Expression]) extends Statement
  case class DeclareFn(attrs: List[Attr], returnTpe: TypeIdent, ident: Ident, args: List[Param], value: Option[Block]) extends Statement
  case class Typedef(tpe: TypeIdent, name: Ident) extends Statement
  case class DefineComplex(tpe: TypeIdent.ComplexType, elements: List[(TypeIdent, Ident)]) extends Statement
  case class Return(expr: Option[Expression]) extends Statement
  case class Block(items: NonEmptyList[Statement]) extends Statement {
    def doWhile(cond: Expression): Statement = DoWhile(this, cond)
  }
  case class IfElse(ifs: NonEmptyList[(Expression, Block)], elseCond: Option[Block]) extends Statement
  case class DoWhile(block: Block, whileCond: Expression) extends Statement

  val returnVoid: Statement = Return(None)

  def block(item: Statement, rest: Statement*): Block =
    Block(NonEmptyList(item, rest.toList))

  private val equalsDoc = Doc.text(" = ")
  private val semiDoc = Doc.char(';')
  private val typeDefDoc = Doc.text("typedef ")
  private val leftCurly = Doc.char('{')
  private val rightCurly = Doc.char('}')
  private val leftPar = Doc.char('(')
  private val rightPar = Doc.char(')')
  private val leftBracket = Doc.char('[')
  private val rightBracket = Doc.char(']')
  private val dot = Doc.char('.')
  private val returnSemi = Doc.text("return;")
  private val returnSpace = Doc.text("return ")
  private val ifDoc = Doc.text("if ")
  private val elseIfDoc = Doc.text("else if ")
  private val elseDoc = Doc.text("else ")
  private val commaLine = Doc.char(',') + Doc.line
  private val doDoc = Doc.text("do ")
  private val whileDoc = Doc.text("while")
  private val arrow = Doc.text("->")
  private val ampDoc = Doc.char('&')

  private def par(d: Doc): Doc = leftPar + d + rightPar

  private def curlyBlock[A](items: Iterable[A])(fn: A => Doc): Doc =
    if (items.isEmpty) {
      leftCurly + rightCurly
    }
    else {
      val inner =
        (Doc.line + Doc.intercalate(Doc.line, items.map(fn))).nested(4)

      leftCurly + inner + Doc.line + rightCurly
    }

  def toDoc(c: Code): Doc = 
    c match {
      case Ident(n) => Doc.text(n)
      case IntLiteral(bi) => Doc.str(bi)
      case Cast(tpe, expr) =>
        val edoc = expr match {
          case Ident(n) => Doc.text(n)
          case _ => par(toDoc(expr))
        }
        par(TypeIdent.toDoc(tpe)) + edoc
      case Apply(fn, args) =>
        val fnDoc = fn match {
          case Ident(n) => Doc.text(n)
          case notIdent => par(toDoc(notIdent))
        }
        fnDoc + par(Doc.intercalate(commaLine, args.map(expr => toDoc(expr))).grouped.nested(4))
      case Deref(expr) =>
        asterisk + toDoc(expr)
      case Select(target, Ident(nm)) =>
        target match {
          case Deref(noPar @ (Ident(_) | Apply(_, _) | Select(_, _))) => toDoc(noPar) + arrow + Doc.text(nm)
          case Deref(notIdent) => par(toDoc(notIdent)) + arrow + Doc.text(nm)
          case noPar @ (Ident(_) | Apply(_, _) | Select(_, _)) => toDoc(noPar) + dot + Doc.text(nm)
          case notIdent => par(toDoc(notIdent)) + dot + Doc.text(nm)
        }
      case Bracket(target, item) =>
        val left = target match {
          case noPar @ (Ident(_) | Apply(_, _) | Select(_, _)) =>
            toDoc(noPar)
          case yesPar => par(toDoc(yesPar))
        }
        left + leftBracket + toDoc(item) + rightBracket
      case AddrOf(expr) =>
        val e = expr match {
          case noPar @ (Ident(_) | Select(_, _)) =>
            toDoc(noPar)
          case yesPar => par(toDoc(yesPar))
        }
        ampDoc + e
      // Statements
      case Assignment(t, v) => toDoc(t) + (equalsDoc + (toDoc(v) + semiDoc))
      case DeclareArray(tpe, nm, values) =>
        // Foo bar[size] = {v(0), v(1), ...};
        // or
        // Foo bar[size];
        val tpeName = TypeIdent.toDoc(tpe) + Doc.space + toDoc(nm)
        values match {
          case Right(init) =>
            val len = init.size
            val begin = tpeName + leftBracket + Doc.str(len) + rightBracket + equalsDoc + leftCurly;
            val items =
              if (init.isEmpty) Doc.empty
              else {
                ((Doc.line + Doc.intercalate(commaLine, init.map(e => toDoc(e)))).nested(4) + Doc.line).grouped
              }

            begin + items + rightCurly + semiDoc
          case Left(len) =>
            tpeName + leftBracket + Doc.str(len) + rightBracket + semiDoc
        }
      case DeclareVar(attrs, tpe, ident, v) =>
        val attrDoc =
          if (attrs.isEmpty) Doc.empty
          else {
            Doc.intercalate(Doc.space, attrs.map(a => Attr.toDoc(a))) + Doc.space
          }

        val prefix = Doc.intercalate(Doc.space,
          (attrDoc + TypeIdent.toDoc(tpe)) ::
          toDoc(ident) ::
          Nil)

        v match {
          case Some(rhs) => prefix + equalsDoc + toDoc(rhs) + semiDoc
          case None => prefix + semiDoc
        }
      case DeclareFn(attrs, tpe, ident, args, v) =>
        val attrDoc =
          if (attrs.isEmpty) Doc.empty
          else {
            Doc.intercalate(Doc.space, attrs.map(a => Attr.toDoc(a))) + Doc.space
          }

        val paramDoc = Doc.intercalate(Doc.line, args.map(_.toDoc)).nested(4).grouped

        val prefix = Doc.intercalate(Doc.space,
          (attrDoc + TypeIdent.toDoc(tpe)) ::
          (toDoc(ident) + par(paramDoc)) ::
          Nil)

        v match {
          case Some(rhs) => prefix + Doc.space + toDoc(rhs)
          case None => prefix + semiDoc
        }
      case Typedef(td, n) =>
        typeDefDoc + TypeIdent.toDoc(td) + Doc.space + toDoc(n) + semiDoc
      case DefineComplex(tpe, els) =>
        val pre = TypeIdent.toDoc(tpe) + Doc.space
        val code = pre + curlyBlock(els) { case (t, n) =>
          TypeIdent.toDoc(t) + Doc.space + toDoc(n) + semiDoc 
        }

        code + semiDoc
      case Return(opt) =>
        opt match {
          case None => returnSemi
          case Some(expr) => returnSpace + toDoc(expr) + semiDoc
        }
      case Block(items) => curlyBlock(items.toList) { s => toDoc(s) }
      case IfElse(ifs, els) =>
        //"if (ex) {} else if"
        val (fcond, fblock) = ifs.head
        val first = ifDoc + par(toDoc(fcond)) + Doc.space + toDoc(fblock)
        val middle = ifs.tail match {
          case Nil => Doc.empty
          case nonEmpty =>
            Doc.line + Doc.intercalate(Doc.line, nonEmpty.map { case (c, b) =>
              elseIfDoc + par(toDoc(c)) + Doc.space + toDoc(b)
            })
        }
        val end = els match {
          case None => Doc.empty
          case Some(e) => Doc.line + elseDoc + toDoc(e)
        }

        first + middle + end
      case DoWhile(block, cond) =>
        doDoc + toDoc(block) + Doc.space + whileDoc + par(toDoc(cond)) + semiDoc
    }
}