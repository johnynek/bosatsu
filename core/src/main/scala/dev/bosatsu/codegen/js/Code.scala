package dev.bosatsu.codegen.js

import scala.language.implicitConversions

import cats.data.NonEmptyList
import org.typelevel.paiges.Doc

/**
 * JavaScript AST types for code generation.
 * Follows the same pattern as ClangGen's Code.scala but for JavaScript.
 */
sealed trait Code derives CanEqual

object Code {

  sealed trait Expression extends Code {
    def apply(args: Expression*): Call = Call(this, args.toList)
    def dot(name: String): PropertyAccess = PropertyAccess(this, name)
    def bracket(index: Expression): IndexAccess = IndexAccess(this, index)
    def bin(op: BinOp, rhs: Expression): BinExpr = BinExpr(this, op, rhs)

    def +(that: Expression): BinExpr = bin(BinOp.Plus, that)
    def -(that: Expression): BinExpr = bin(BinOp.Minus, that)
    def *(that: Expression): BinExpr = bin(BinOp.Times, that)
    def /(that: Expression): BinExpr = bin(BinOp.Div, that)
    def %(that: Expression): BinExpr = bin(BinOp.Mod, that)
    def ===(that: Expression): BinExpr = bin(BinOp.Eq, that)
    def !==(that: Expression): BinExpr = bin(BinOp.NotEq, that)
    def <(that: Expression): BinExpr = bin(BinOp.Lt, that)
    def <=(that: Expression): BinExpr = bin(BinOp.LtEq, that)
    def >(that: Expression): BinExpr = bin(BinOp.Gt, that)
    def >=(that: Expression): BinExpr = bin(BinOp.GtEq, that)
    def &&(that: Expression): BinExpr = bin(BinOp.And, that)
    def ||(that: Expression): BinExpr = bin(BinOp.Or, that)

    def unary_! : PrefixExpr = PrefixExpr(PrefixOp.Not, this)
    def unary_- : PrefixExpr = PrefixExpr(PrefixOp.Neg, this)
  }

  sealed trait Statement extends Code {
    def +(that: Statement): Statements = Statements.combine(this, that)
  }

  // Expressions
  case class Ident(name: String) extends Expression
  object Ident {
    implicit def fromString(s: String): Ident = Ident(s)
  }

  sealed trait Literal extends Expression derives CanEqual
  case class IntLiteral(value: BigInt) extends Literal
  case class DoubleLiteral(value: Double) extends Literal
  case class StringLiteral(value: String) extends Literal
  case class BoolLiteral(value: Boolean) extends Literal
  case object NullLiteral extends Literal
  case object UndefinedLiteral extends Literal

  object IntLiteral {
    def apply(i: Int): IntLiteral = IntLiteral(BigInt(i))
    def apply(i: Long): IntLiteral = IntLiteral(BigInt(i))
    val Zero: IntLiteral = IntLiteral(0)
    val One: IntLiteral = IntLiteral(1)
  }

  val TrueLit: BoolLiteral = BoolLiteral(true)
  val FalseLit: BoolLiteral = BoolLiteral(false)

  case class ArrowFunction(params: List[String], body: Either[Expression, Block]) extends Expression
  object ArrowFunction {
    def apply(params: List[String], body: Expression): ArrowFunction =
      ArrowFunction(params, Left(body))
    def apply(params: List[String], body: Block): ArrowFunction =
      ArrowFunction(params, Right(body))
  }

  case class Function(name: Option[String], params: List[String], body: Block) extends Expression
  case class Call(fn: Expression, args: List[Expression]) extends Expression
  case class PropertyAccess(obj: Expression, prop: String) extends Expression
  case class IndexAccess(obj: Expression, index: Expression) extends Expression
  case class ArrayLiteral(elements: List[Expression]) extends Expression
  case class ObjectLiteral(props: List[(String, Expression)]) extends Expression
  case class BinExpr(left: Expression, op: BinOp, right: Expression) extends Expression
  case class PrefixExpr(op: PrefixOp, expr: Expression) extends Expression
  case class Ternary(cond: Expression, whenTrue: Expression, whenFalse: Expression) extends Expression
  case class NewExpr(constructor: Expression, args: List[Expression]) extends Expression

  sealed abstract class BinOp(val repr: String) {
    val toDoc: Doc = Doc.text(repr)
  }
  object BinOp {
    case object Plus extends BinOp("+")
    case object Minus extends BinOp("-")
    case object Times extends BinOp("*")
    case object Div extends BinOp("/")
    case object Mod extends BinOp("%")

    case object Eq extends BinOp("===")
    case object NotEq extends BinOp("!==")
    case object Lt extends BinOp("<")
    case object LtEq extends BinOp("<=")
    case object Gt extends BinOp(">")
    case object GtEq extends BinOp(">=")

    case object And extends BinOp("&&")
    case object Or extends BinOp("||")

    // Bitwise operators
    case object BitAnd extends BinOp("&")
    case object BitOr extends BinOp("|")
    case object BitXor extends BinOp("^")
    case object BitShiftLeft extends BinOp("<<")
    case object BitShiftRight extends BinOp(">>")
  }

  sealed abstract class PrefixOp(val repr: String) {
    val toDoc: Doc = Doc.text(repr)
  }
  object PrefixOp {
    case object Not extends PrefixOp("!")
    case object Neg extends PrefixOp("-")
    case object Pos extends PrefixOp("+")
    case object TypeOf extends PrefixOp("typeof ")
    case object BitNot extends PrefixOp("~")
  }

  // Statements
  case class Const(name: String, value: Expression) extends Statement
  case class Let(name: String, value: Option[Expression]) extends Statement
  case class Var(name: String, value: Option[Expression]) extends Statement
  case class Assignment(target: Expression, value: Expression) extends Statement
  case class IfStatement(
    cond: Expression,
    thenBlock: Block,
    elseBlock: Option[Either[IfStatement, Block]]
  ) extends Statement
  case class Return(value: Option[Expression]) extends Statement
  case class Block(statements: NonEmptyList[Statement]) extends Statement
  case class Statements(items: List[Statement]) extends Statement
  object Statements {
    def combine(a: Statement, b: Statement): Statements =
      (a, b) match {
        case (Statements(as), Statements(bs)) => Statements(as ++ bs)
        case (Statements(as), _) => Statements(as :+ b)
        case (_, Statements(bs)) => Statements(a :: bs)
        case _ => Statements(List(a, b))
      }
  }
  case class ExprStatement(expr: Expression) extends Statement
  case class Export(name: String) extends Statement
  case class ExportDefault(expr: Expression) extends Statement
  case class Import(names: List[String], from: String) extends Statement
  case class WhileLoop(cond: Expression, body: Block) extends Statement
  case class ForLoop(init: Option[Statement], cond: Option[Expression], update: Option[Expression], body: Block) extends Statement
  case class Throw(value: Expression) extends Statement
  case class TryCatch(tryBlock: Block, catchVar: String, catchBlock: Block, finallyBlock: Option[Block]) extends Statement

  val returnVoid: Return = Return(None)

  def block(stmt: Statement, rest: Statement*): Block =
    stmt match {
      case b @ Block(_) if rest.isEmpty => b
      case _ => Block(NonEmptyList(stmt, rest.toList))
    }

  def ifThenElse(cond: Expression, thenStmt: Statement, elseStmt: Statement): IfStatement =
    IfStatement(cond, block(thenStmt), Some(Right(block(elseStmt))))

  // Rendering
  private val leftParen = Doc.char('(')
  private val rightParen = Doc.char(')')
  private val leftBracket = Doc.char('[')
  private val rightBracket = Doc.char(']')
  private val leftBrace = Doc.char('{')
  private val rightBrace = Doc.char('}')
  private val semicolon = Doc.char(';')
  private val colon = Doc.char(':')
  private val comma = Doc.char(',')
  private val dot = Doc.char('.')
  private val arrow = Doc.text(" => ")
  private val assign = Doc.text(" = ")
  private val constDoc = Doc.text("const ")
  private val letDoc = Doc.text("let ")
  private val varDoc = Doc.text("var ")
  private val ifDoc = Doc.text("if ")
  private val elseDoc = Doc.text(" else ")
  private val returnDoc = Doc.text("return")
  private val exportDoc = Doc.text("export ")
  private val defaultDoc = Doc.text("default ")
  private val importDoc = Doc.text("import ")
  private val fromDoc = Doc.text(" from ")
  private val whileDoc = Doc.text("while ")
  private val forDoc = Doc.text("for ")
  private val throwDoc = Doc.text("throw ")
  private val tryDoc = Doc.text("try ")
  private val catchDoc = Doc.text("catch ")
  private val finallyDoc = Doc.text("finally ")
  private val newDoc = Doc.text("new ")
  private val functionDoc = Doc.text("function")
  private val nullDoc = Doc.text("null")
  private val undefinedDoc = Doc.text("undefined")
  private val trueDoc = Doc.text("true")
  private val falseDoc = Doc.text("false")
  private val question = Doc.text(" ? ")
  private val colonSpace = Doc.text(" : ")
  private val commaSpace = Doc.text(", ")
  private val commaLine = comma + Doc.line
  private val quoteDoc = Doc.char('"')

  private def par(d: Doc): Doc = leftParen + d + rightParen

  private def curlyBlock(stmts: List[Statement]): Doc =
    if (stmts.isEmpty) leftBrace + rightBrace
    else {
      val inner = (Doc.line + Doc.intercalate(Doc.line, stmts.map(toDoc))).nested(2)
      leftBrace + inner + Doc.line + rightBrace
    }

  /** Check if expression is "tight" - doesn't need parentheses in most contexts */
  object Tight {
    def unapply(e: Expression): Option[Expression] =
      e match {
        case _: Ident | _: Literal | _: Call | _: PropertyAccess |
             _: IndexAccess | _: ArrayLiteral | _: ObjectLiteral => Some(e)
        case _ => None
      }
  }

  /** Escape a string for JavaScript */
  def escapeString(s: String): String = {
    val sb = new StringBuilder
    s.foreach {
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case '\\' => sb.append("\\\\")
      case '"' => sb.append("\\\"")
      case c if c < 32 || c > 126 =>
        sb.append("\\u")
        sb.append(String.format("%04x", Int.box(c.toInt)))
      case c => sb.append(c)
    }
    sb.toString
  }

  def toDoc(c: Code): Doc = c match {
    // Expressions
    case Ident(name) => Doc.text(name)
    case IntLiteral(v) =>
      if (v < 0) par(Doc.text(v.toString))
      else Doc.text(v.toString)
    case DoubleLiteral(v) =>
      // Ensure double always has decimal point (JS toString may omit it for whole numbers)
      val str = v.toString
      val withDecimal = if (str.contains('.') || str.contains('e') || str.contains('E')) str else str + ".0"
      if (v < 0) par(Doc.text(withDecimal))
      else Doc.text(withDecimal)
    case StringLiteral(v) => quoteDoc + Doc.text(escapeString(v)) + quoteDoc
    case BoolLiteral(true) => trueDoc
    case BoolLiteral(false) => falseDoc
    case NullLiteral => nullDoc
    case UndefinedLiteral => undefinedDoc

    case ArrowFunction(params, body) =>
      val paramsDoc = params match {
        case single :: Nil => Doc.text(single)
        case _ => par(Doc.intercalate(commaSpace, params.map(Doc.text)))
      }
      body match {
        case Left(expr) =>
          expr match {
            case _: ObjectLiteral => paramsDoc + arrow + par(toDoc(expr))
            case _ => paramsDoc + arrow + toDoc(expr)
          }
        case Right(blk) => paramsDoc + arrow + toDoc(blk)
      }

    case Function(name, params, body) =>
      val nameDoc = name.map(n => Doc.text(" " + n)).getOrElse(Doc.empty)
      val paramsDoc = par(Doc.intercalate(commaSpace, params.map(Doc.text)))
      functionDoc + nameDoc + paramsDoc + Doc.space + toDoc(body)

    case Call(fn, args) =>
      val fnDoc = fn match {
        case Tight(e) => toDoc(e)
        case ArrowFunction(_, _) => par(toDoc(fn))
        case Function(_, _, _) => par(toDoc(fn))
        case _ => toDoc(fn)
      }
      fnDoc + par(Doc.intercalate(commaSpace, args.map(toDoc)).grouped.nested(2))

    case PropertyAccess(obj, prop) =>
      val objDoc = obj match {
        case Tight(e) => toDoc(e)
        case _ => par(toDoc(obj))
      }
      objDoc + dot + Doc.text(prop)

    case IndexAccess(obj, index) =>
      val objDoc = obj match {
        case Tight(e) => toDoc(e)
        case _ => par(toDoc(obj))
      }
      objDoc + leftBracket + toDoc(index) + rightBracket

    case ArrayLiteral(elements) =>
      if (elements.isEmpty) leftBracket + rightBracket
      else {
        val inner = Doc.intercalate(commaLine, elements.map(toDoc)).nested(2).grouped
        leftBracket + inner + rightBracket
      }

    case ObjectLiteral(props) =>
      if (props.isEmpty) leftBrace + rightBrace
      else {
        val inner = Doc.intercalate(commaLine, props.map { case (k, v) =>
          Doc.text(k) + colon + Doc.space + toDoc(v)
        }).nested(2).grouped
        leftBrace + Doc.space + inner + Doc.space + rightBrace
      }

    case BinExpr(left, op, right) =>
      val leftDoc = left match {
        case Tight(e) => toDoc(e)
        case BinExpr(_, _, _) => toDoc(left)  // Keep associativity
        case _ => par(toDoc(left))
      }
      val rightDoc = right match {
        case Tight(e) => toDoc(e)
        case _ => par(toDoc(right))
      }
      leftDoc + Doc.space + op.toDoc + Doc.space + rightDoc

    case PrefixExpr(op, expr) =>
      val exprDoc = expr match {
        case Tight(e) => toDoc(e)
        case _ => par(toDoc(expr))
      }
      op.toDoc + exprDoc

    case Ternary(cond, t, f) =>
      val condDoc = cond match {
        case Tight(e) => toDoc(e)
        case _ => par(toDoc(cond))
      }
      val tDoc = toDoc(t)
      val fDoc = toDoc(f)
      (condDoc + (question + tDoc + colonSpace + fDoc).nested(2)).grouped

    case NewExpr(constructor, args) =>
      val ctorDoc = toDoc(constructor)
      newDoc + ctorDoc + par(Doc.intercalate(commaSpace, args.map(toDoc)))

    // Statements
    case Const(name, value) =>
      constDoc + Doc.text(name) + assign + toDoc(value) + semicolon

    case Let(name, value) =>
      value match {
        case Some(v) => letDoc + Doc.text(name) + assign + toDoc(v) + semicolon
        case None => letDoc + Doc.text(name) + semicolon
      }

    case Var(name, value) =>
      value match {
        case Some(v) => varDoc + Doc.text(name) + assign + toDoc(v) + semicolon
        case None => varDoc + Doc.text(name) + semicolon
      }

    case Assignment(target, value) =>
      toDoc(target) + assign + toDoc(value) + semicolon

    case IfStatement(cond, thenBlock, elseBlock) =>
      val condDoc = ifDoc + par(toDoc(cond)) + Doc.space + toDoc(thenBlock)
      elseBlock match {
        case None => condDoc
        case Some(Left(elseIf)) => condDoc + elseDoc + toDoc(elseIf)
        case Some(Right(elseBlk)) => condDoc + elseDoc + toDoc(elseBlk)
      }

    case Return(None) => returnDoc + semicolon
    case Return(Some(value)) => returnDoc + Doc.space + toDoc(value) + semicolon

    case Block(stmts) => curlyBlock(stmts.toList)

    case Statements(stmts) =>
      Doc.intercalate(Doc.line, stmts.map(toDoc))

    case ExprStatement(expr) => toDoc(expr) + semicolon

    case Export(name) => exportDoc + leftBrace + Doc.text(name) + rightBrace + semicolon

    case ExportDefault(expr) => exportDoc + defaultDoc + toDoc(expr) + semicolon

    case Import(names, from) =>
      val namesDoc = leftBrace + Doc.intercalate(commaSpace, names.map(Doc.text)) + rightBrace
      importDoc + namesDoc + fromDoc + quoteDoc + Doc.text(from) + quoteDoc + semicolon

    case WhileLoop(cond, body) =>
      whileDoc + par(toDoc(cond)) + Doc.space + toDoc(body)

    case ForLoop(init, cond, update, body) =>
      // For loop format: for (init; cond; update) body
      // All semicolons are required even when clauses are empty
      val initDoc = init.map(i => toDoc(i)).getOrElse(Doc.empty)
      val condDoc = cond.map(toDoc).getOrElse(Doc.empty)
      val updateDoc = update.map(toDoc).getOrElse(Doc.empty)
      forDoc + par(initDoc + semicolon + Doc.space + condDoc + semicolon + Doc.space + updateDoc) + Doc.space + toDoc(body)

    case Throw(value) => throwDoc + toDoc(value) + semicolon

    case TryCatch(tryBlock, catchVar, catchBlock, finallyBlock) =>
      val tryPart = tryDoc + toDoc(tryBlock)
      val catchPart = catchDoc + par(Doc.text(catchVar)) + Doc.space + toDoc(catchBlock)
      val finallyPart = finallyBlock.map(fb => finallyDoc + toDoc(fb)).getOrElse(Doc.empty)
      tryPart + Doc.space + catchPart + finallyPart
  }

  /** Render code to a string with given line width */
  def render(c: Code, width: Int = 80): String = toDoc(c).render(width)
}
