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

    def apply(args: Expression*): Apply =
      Apply(this, args.toList)

    def get(idx: Int): SelectItem =
      SelectItem(this, idx)


    def eval(op: Operator, x: Expression): Expression =
      Op(this, op, x).simplify

    def evalAnd(that: Expression): Expression =
      eval(Const.And, that)

    def evalPlus(that: Expression): Expression =
      eval(Const.Plus, that)

    def evalMinus(that: Expression): Expression =
      eval(Const.Minus, that)

    def evalTimes(that: Expression): Expression =
      eval(Const.Times, that)

    def simplify: Expression = this
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
    }

  private def iflike(name: String, cond: Doc, body: Doc): Doc =
    Doc.text(name) + Doc.space + cond + Doc.char(':') + (Doc.hardLine + body).nested(4)

  def toDoc(c: Code): Doc =
    c match {
      case PyInt(bi) => Doc.text(bi.toString)
      case PyString(s) => Doc.char('"') + Doc.text(StringUtil.escape('"', s)) + Doc.char('"')
      case PyBool(b) =>
        if (b) Doc.text("True")
        else Doc.text("False")
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

  case class PyInt(toBigInteger: BigInteger) extends Expression
  case class PyString(content: String) extends Expression
  case class PyBool(toBoolean: Boolean) extends Expression
  case class Ident(name: String) extends Dotable { // a kind of expression
    def :=(vl: ValueLike): Statement =
      addAssign(this, vl)
  }
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

    // prefer constants on the right
    override def simplify: Expression =
      this match {
        case Op(PyInt(a), io: IntOp, PyInt(b)) =>
          PyInt(io(a, b))
        case Op(i@PyInt(a), Const.Times, right) =>
          if (a == BigInteger.ZERO) i
          else if (a == BigInteger.ONE) right.simplify
          else right.simplify.evalTimes(i)
        case Op(left, Const.Times, i@PyInt(b)) =>
          if (b == BigInteger.ZERO) i
          else if (b == BigInteger.ONE) left.simplify
          else {
            val l1 = left.simplify
            if (l1 == left) this
            else (l1.evalTimes(i))
          }
        case Op(i@PyInt(a), Const.Plus, right) =>
          if (a == BigInteger.ZERO) right.simplify
          else {
            val r1 = right.simplify
            // put the constant on the right
            r1.evalPlus(i)
          }
        case Op(left, Const.Plus, i@PyInt(b)) =>
          if (b == BigInteger.ZERO) left.simplify
          else {
            val l1 = left.simplify
            if (l1 == left) {
              l1 match {
                case Op(ll, io: IntOp, rl) =>
                  io match {
                    case Const.Plus =>
                      // right associate
                      ll.evalPlus(rl.evalPlus(i))
                    case Const.Minus =>
                      //(ll - rl) + i == ll - (rl - i)
                      ll.evalMinus(rl.evalMinus(i))
                    case _ => this
                  }
                case _ => this
              }
            }
            else (l1.evalPlus(i))
          }
        case Op(i@PyInt(_), Const.Minus, right) =>
          val r1 = right.simplify
          if (r1 == right) {
            r1 match {
              case Op(rl, io: IntOp, rr) =>
                io match {
                  case Const.Plus =>
                    // right associate
                    rl.evalPlus(rr.evalPlus(i))
                  case Const.Minus =>
                    //i - (rl - rr)
                    rr match {
                      case ri@PyInt(_) =>
                        Op(i.evalPlus(ri), Const.Minus, rl)
                      case _ => this
                    }
                  case _ => this
                }
              case _ => this
            }
          }
          else (i.evalMinus(r1))
        case Op(left, Const.Minus, i@PyInt(b)) =>
          if (b == BigInteger.ZERO) left.simplify
          else {
            val l1 = left.simplify
            if (l1 == left) {
              l1 match {
                case Op(ll, io: IntOp, rl) =>
                  io match {
                    case Const.Plus =>
                      // (ll + rl) - i == ll + (rl - i)
                      ll.evalPlus(rl.evalMinus(i))
                    case Const.Minus =>
                      //(ll - rl) - i == ll - (rl + i)
                      ll.evalMinus(rl.evalPlus(i))
                    case _ => this
                  }
                case _ => this
              }
            }
            else (l1.evalMinus(i))
          }
        case Op(a, Const.Eq, b) =>
          if (a == b) Const.True
          else this
        case Op(a, Const.Gt | Const.Lt, b) if a == b => Const.False
        case Op(PyInt(a), Const.Gt, PyInt(b)) =>
          if (a.compareTo(b) > 0) Const.True
          else Const.False
        case Op(PyInt(a), Const.Lt, PyInt(b)) =>
          if (a.compareTo(b) < 0) Const.True
          else Const.False
        case Op(a, Const.And, b) =>
          (a, b) match {
            case (Const.True, _) => b
            case (_, Const.True) => a
            case (Const.False, _) => Const.False
            case (_, Const.False) => Const.False
            case _ => this
          }
        case _ =>
          val l1 = left.simplify
          val r1 = right.simplify
          if ((l1 != left) || (r1 != right)) {
            Op(l1, op, r1).simplify
          }
          else {
            (left, op) match {
              case (Op(ll, Const.Plus, lr), Const.Plus) =>
                // right associate
                ll.evalPlus(lr.evalPlus(right))
              case (Op(ll, Const.Minus, lr), Const.Plus) =>
                // right associate
                ll.evalPlus(right.evalMinus(lr))
              case (Op(ll, Const.Plus, lr), Const.Minus) =>
                // right associate
                ll.evalMinus(right.evalMinus(lr))
              case (Op(ll, Const.Times, lr), Const.Times) =>
                // right associate
                ll.evalTimes(lr.evalTimes(right))
              case _ => this
            }
          }
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

  def block(stmt: Statement, rest: Statement*): Statement =
    if (rest.isEmpty) stmt
    else Block(NonEmptyList(stmt, rest.toList))

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
    fromLong(i.toLong)

  def fromLong(i: Long): Expression =
    if (i == 0L) Const.Zero
    else if (i == 1L) Const.One
    else PyInt(BigInteger.valueOf(i))

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

  sealed abstract class IntOp(nm: String) extends Operator(nm) {
    def apply(a: BigInteger, b: BigInteger): BigInteger =
      this match {
        case Const.Plus => a.add(b)
        case Const.Minus => a.subtract(b)
        case Const.Times => a.multiply(b)
      }
  }

  object Const {
    case object Plus extends IntOp("+")
    case object Minus extends IntOp("-")
    case object Times extends IntOp("*")
    case object And extends Operator("and")
    case object Eq extends Operator("==")
    case object Gt extends Operator(">")
    case object Lt extends Operator("<")

    val True = PyBool(true)
    val False = PyBool(false)

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

