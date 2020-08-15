package org.bykn.bosatsu.codegen.python

import cats.data.NonEmptyList
import java.math.BigInteger
import org.bykn.bosatsu.{Lit, StringUtil}
import org.typelevel.paiges.Doc

// Structs are represented as tuples
// Enums are represented as tuples with an additional first field holding
// the variant

sealed trait Code

object Code {

  // Not necessarily code, but something that has a final value
  // this allows us to add IfElse as a an expression (which
  // is not yet valid python) a series of lets before an expression
  sealed trait ValueLike

  sealed abstract class Expression extends ValueLike with Code {
    def identOrParens: Expression =
      this match {
        case i: Code.Ident => i
        case p => Code.Parens(p)
      }

    def evalPlus(that: Expression): Expression =
      (this, that) match {
        case (Code.PyInt(a), Code.PyInt(b)) =>
          Code.PyInt(a.add(b))
        case (Code.PyInt(a), Code.Op(b, Code.Const.Plus, Code.PyInt(c))) =>
          b.evalPlus(Code.PyInt(a.add(c)))
        case (Code.Op(b, Code.Const.Plus, Code.PyInt(c)), Code.PyInt(a)) =>
          b.evalPlus(Code.PyInt(c.add(a)))
        case (Code.Op(a1, Code.Const.Plus, Code.PyInt(a2)), Code.Op(b1, Code.Const.Plus, Code.PyInt(b2))) =>
          a1.evalPlus(b1.evalPlus(Code.PyInt(a2.add(b2))))
        case (_, _) =>
          // TODO, this could also optimize when we have Minus
          Code.Op(this, Code.Const.Plus, that)
      }

    def evalMinus(that: Expression): Expression =
      (this, that) match {
        case (Code.PyInt(a), Code.PyInt(b)) =>
          Code.PyInt(a.subtract(b))
        case (Code.PyInt(a), Code.Op(b, Code.Const.Minus, Code.PyInt(c))) =>
          b.evalMinus(Code.PyInt(a.subtract(c)))
        case (Code.Op(b, Code.Const.Minus, Code.PyInt(c)), Code.PyInt(a)) =>
          b.evalPlus(Code.PyInt(a.subtract(c)))
        case (Code.Op(a1, Code.Const.Minus, Code.PyInt(a2)), Code.Op(b1, Code.Const.Minus, Code.PyInt(b2))) =>
          a1.evalMinus(b1).evalMinus(Code.PyInt(b2.subtract(a2)))
        case (_, _) =>
          // TODO, this could also optimize when we have Plus
          Code.Op(this, Code.Const.Minus, that)
      }

    def evalTimes(that: Expression): Expression =
      (this, that) match {
        case (Code.PyInt(a), Code.PyInt(b)) =>
          Code.PyInt(a.multiply(b))
        case (Code.PyInt(a), Code.Op(b, Code.Const.Times, Code.PyInt(c))) =>
          b.evalTimes(Code.PyInt(a.multiply(c)))
        case (Code.Op(b, Code.Const.Times, Code.PyInt(c)), Code.PyInt(a)) =>
          b.evalTimes(Code.PyInt(c.multiply(a)))
        case (Code.Op(a1, Code.Const.Times, Code.PyInt(a2)), Code.Op(b1, Code.Const.Times, Code.PyInt(b2))) =>
          a1.evalTimes(b1.evalTimes(Code.PyInt(a2.multiply(b2))))
        case (_, _) =>
          // TODO, this could also optimize when we have Minus
          Code.Op(this, Code.Const.Times, that)
      }
  }

  // something we can a . after
  sealed abstract class Dotable extends Expression

  sealed abstract class Statement extends Code {
    def statements: NonEmptyList[Statement] =
      this match {
        case Block(ss) => ss
        case notBlock => NonEmptyList(notBlock, Nil)
      }

    def +:(stmt: Statement): Statement =
      stmt match {
        case Pass => this
        case _ =>
          if (this == Pass) stmt
          else Block(stmt :: statements)
      }

    def :+(stmt: Statement): Statement =
      stmt match {
        case Pass => this
        case _ =>
          if (this == Pass) stmt
          else Block(statements :+ stmt)
      }
  }

  private def par(d: Doc): Doc =
    Doc.char('(') + d + Doc.char(')')

  private def maybePar(c: Expression): Doc =
    c match {
      case Lambda(_, _) => par(toDoc(c))
      case _ => toDoc(c)
      //case Apply(_, _) | DotSelect(_, _) | Literal(_) | PyInt(_) | PyString(_) | Ident(_) | Parens(_) | MakeTuple(_) => toDoc(c)
      //case _ => par(toDoc(c))
    }

  private def iflike(name: String, cond: Doc, body: Doc): Doc =
    Doc.text(name) + Doc.space + cond + Doc.char(':') + (Doc.hardLine + body).nested(4)

  def toDoc(c: Code): Doc =
    c match {
      case Literal(s) => Doc.text(s)
      case PyInt(bi) => Doc.text(bi.toString)
      case PyString(s) => Doc.char('"') + Doc.text(StringUtil.escape('"', s)) + Doc.char('"')
      case Ident(i) => Doc.text(i)
      case o@Op(_, _, _) => o.toDoc
      case Parens(inner@Parens(_)) => toDoc(inner)
      case Parens(p) => par(toDoc(p))
      case SelectItem(x, i) =>
        maybePar(x) + Doc.char('[') + Doc.str(i) + Doc.char(']')
      case MakeTuple(items) =>
        items match {
          case Nil => Doc.text("()")
          case h :: Nil => par(toDoc(h) + Doc.comma)
          case twoOrMore => par(Doc.intercalate(Doc.comma + Doc.lineOrSpace, twoOrMore.map(toDoc)))
        }
      case Lambda(args, res) =>
        Doc.text("lambda ") + Doc.intercalate(Doc.comma + Doc.space, args.map(toDoc)) + Doc.text(": ") + toDoc(res)

      case Apply(fn, args) =>
        maybePar(fn) + par(Doc.intercalate(Doc.comma + Doc.lineOrSpace, args.map(toDoc)))

      case DotSelect(left, right) =>
        toDoc(left) + Doc.char('.') + toDoc(right)

      case IfStatement(conds, optElse) =>
        val condsDoc = conds.map { case (x, b) => (toDoc(x), toDoc(b)) }
        val i1 = iflike("if", condsDoc.head._1, condsDoc.head._2)
        val i2 = condsDoc.tail.map { case (x, b) => iflike("elif", x, b) }
        val el = optElse.fold(Doc.empty) { els => Doc.hardLine + Doc.text("else:") + (Doc.hardLine + toDoc(els)).nested(4) }

        Doc.intercalate(Doc.hardLine, i1 :: i2) + el

      case Block(stmts) =>
        Doc.intercalate(Doc.hardLine, stmts.map(toDoc).toList)

      case Def(nm, args, body) =>
        Doc.text("def") + Doc.space + Doc.text(nm.name) +
          par(Doc.intercalate(Doc.comma + Doc.lineOrSpace, args.map(toDoc))).nested(4) + Doc.char(':') + (Doc.hardLine + toDoc(body)).nested(4)

      case Return(expr) => Doc.text("return ") + toDoc(expr)

      case Assign(nm, expr) => Doc.text(nm.name) + Doc.text(" = ") + toDoc(expr)
      case Pass => Doc.text("pass")
      case While(cond, body) =>
        Doc.text("while") + Doc.space + toDoc(cond) + Doc.char(':') + (Doc.hardLine + toDoc(body)).nested(4)
      case Import(name, aliasOpt) =>
        // import name as alias
        val imp = Doc.text("import") + Doc.space + Doc.text(name)
        aliasOpt.fold(imp) { a => imp + Doc.space + Doc.text("as") + Doc.space + toDoc(a) }
    }

  /////////////////////////
  // Here are all the expressions
  /////////////////////////

  // True, False, None, numbers
  case class Literal(asString: String) extends Expression
  case class PyInt(toBigInteger: BigInteger) extends Expression
  case class PyString(content: String) extends Expression
  case class Ident(name: String) extends Dotable // a kind of expression
  // Binary operator used for +, -, and, == etc...
  case class Op(left: Expression, op: Operator, right: Expression) extends Expression {
    // operators like + can associate
    //
    def toDoc: Doc = {
      def loop(left: Expression, rights: NonEmptyList[(Operator, Expression)]): Doc =
        // a op1 b op2 c if op1 and op2 associate no need for a parens
        // left match {
        //   case Op(_,
        // par(maybePar(left) + Doc.space + Doc.text(on.name) + Doc.space + maybePar(right))
        left match {
          case Op(l1, o1, r1) =>
            if (o1.associates(rights.head._1)) loop(l1, (o1, r1) :: rights)
            else loop(Parens(left), rights)
          case leftNotOp =>
            rights.head match {
              case (ol, Op(r1, o2, r2)) =>
                loop(left, NonEmptyList((ol, r1), (o2, r2) :: rights.tail))
              case (ol, rightNotOp) =>
                rights.tail match {
                  case Nil =>
                    maybePar(leftNotOp) + Doc.space + Doc.text(ol.name) + Doc.space + maybePar(rightNotOp)
                  case (o2, r2) :: rest =>
                    val leftDoc = maybePar(leftNotOp) + Doc.space + Doc.text(ol.name) + Doc.space
                    if (ol.associates(o2)) {
                      leftDoc + loop(rightNotOp, NonEmptyList((o2, r2), rest))
                    }
                    else {
                      // we need to put a parens ending after rightNotOp
                      // leftNotOp ol (rightNotOp o2 r2 :: rest)
                      leftDoc + par(loop(rightNotOp, NonEmptyList((o2, r2), rest)))
                    }
                }

            }
        }

      loop(left, NonEmptyList((op, right), Nil))
    }
  }

  case class Parens(expr: Expression) extends Expression
  case class SelectItem(arg: Expression, position: Int) extends Expression
  case class MakeTuple(args: List[Expression]) extends Expression
  case class Lambda(args: List[Ident], result: Expression) extends Expression
  case class Apply(fn: Expression, args: List[Expression]) extends Expression {
    def uncurry: (Expression, List[List[Expression]]) =
      fn match {
        case a@Apply(_, _) =>
          val (fn0, args0) = a.uncurry
          (fn0, args0 :+ args)
        case _ =>
          (fn, args :: Nil)
      }
  }
  case class DotSelect(ex: Dotable, ident: Ident) extends Dotable

  /////////////////////////
  // Here are all the ValueLike
  /////////////////////////

  // this prepares an expression with a number of statements
  case class WithValue(statement: Statement, value: ValueLike) extends ValueLike {
    def +:(stmt: Statement): WithValue =
      WithValue(stmt +: statement, value)

    def :+(stmt: Statement): WithValue =
      WithValue(statement :+ stmt, value)
  }
  case class IfElse(conds: NonEmptyList[(Expression, ValueLike)], elseCond: ValueLike) extends ValueLike


  /////////////////////////
  // Here are all the Statements
  /////////////////////////

  case class Block(stmts: NonEmptyList[Statement]) extends Statement
  case class IfStatement(conds: NonEmptyList[(Expression, Statement)], elseCond: Option[Statement]) extends Statement
  case class Def(name: Ident, args: List[Ident], body: Statement) extends Statement
  case class Return(expr: Expression) extends Statement
  case class Assign(variable: Ident, value: Expression) extends Statement
  case object Pass extends Statement
  case class While(cond: Expression, body: Statement) extends Statement
  case class Import(modname: String, alias: Option[Ident]) extends Statement

  def addAssign(variable: Ident, code: ValueLike): Statement =
    code match {
      case x: Expression =>
        Assign(variable, x)
      case WithValue(stmt, v) =>
        stmt +: addAssign(variable, v)
      case IfElse(conds, elseCond) =>
        IfStatement(
          conds.map { case (b, v) =>
            (b, addAssign(variable, v))
          },
          Some(addAssign(variable, elseCond))
        )
    }

  def toReturn(v: ValueLike): Statement =
    v match {
      case x: Expression => Code.Return(x)
      case WithValue(stmt, v) =>
        stmt :+ toReturn(v)
      case ie@IfElse(conds, elseCond) =>
        IfStatement(
          conds.map { case (c, v) =>
            (c, toReturn(v))
          },
          Some(toReturn(elseCond)))
    }


  // boolean expressions can contain side effects
  // this runs the side effects but discards
  // and resulting value
  // we could assert the value, statically
  // that assertion should always be true
  def always(v: ValueLike): Statement =
    v match {
      case x: Expression => Pass
      case WithValue(stmt, v) =>
        stmt +: always(v)
      case IfElse(conds, elseCond) =>
        IfStatement(
          conds.map { case (b, v) =>
            (b, always(v))
          },
          Some(always(elseCond))
        )
    }

  def litToExpr(lit: Lit): Expression =
    lit match {
      case Lit.Str(s) => PyString(s)
      case Lit.Integer(bi) => PyInt(bi)
    }

  def fromInt(i: Int): Expression =
    if (i == 0) Const.Zero
    else if (i == 1) Const.One
    else PyInt(BigInteger.valueOf(i.toLong))

  sealed abstract class Operator(val name: String) {
    def associates(that: Operator): Boolean = {
      // true if (a this b) that c == a this (b that c)
      this match {
        case Const.Plus => (that == Const.Plus) || (that == Const.Minus)
        case Const.Minus => false
        case Const.And => that == Const.And
        case Const.Times =>
          // (a * b) * c == a * (b * c)
          // (a * b) + c != a * (b + c)
          (that == Const.Times)
        case _ => false
      }
    }
  }
  object Const {
    case object Plus extends Operator("+")
    case object Minus extends Operator("-")
    case object Times extends Operator("*")
    case object And extends Operator("and")
    case object Eq extends Operator("==")
    case object Gt extends Operator(">")

    val True = Literal("True")
    val False = Literal("False")

    val Zero = PyInt(BigInteger.ZERO)
    val One = PyInt(BigInteger.ONE)
  }

  val python2Name: java.util.regex.Pattern =
    "[_A-Za-z][_0-9A-Za-z]*".r.pattern

  val pyKeywordList: Set[String] = Set(
    "and",       "del",       "from",      "not",       "while",
    "as",        "elif",      "global",    "or",        "with",
    "assert",    "else",      "if",        "pass",      "yield",
    "break",     "except",    "import",    "print",
    "class",     "exec",      "in",        "raise",
    "continue",  "finally",   "is",        "return",
    "def",       "for",       "lambda",    "try"
  )

}

