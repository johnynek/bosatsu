package org.bykn.bosatsu.codegen.python

import cats.data.NonEmptyList
import java.math.BigInteger
import org.bykn.bosatsu.{Lit, PredefImpl, StringUtil}
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
        case i: Code.Ident      => i
        case p @ Code.Parens(_) => p
        case other              => Code.Parens(other)
      }

    def apply(args: Expression*): Apply =
      Apply(this, args.toList)

    def get(idx: Int): SelectItem =
      SelectItem(this, idx)

    def dot(ident: Code.Ident): DotSelect =
      DotSelect(this, ident)

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

    def :=(vl: ValueLike): Statement =
      addAssign(this, vl)

    def simplify: Expression
  }

  sealed abstract class Statement extends Code {
    def statements: NonEmptyList[Statement] =
      this match {
        case Block(ss) => ss
        case notBlock  => NonEmptyList(notBlock, Nil)
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

    def withValue(vl: ValueLike): ValueLike =
      this match {
        case Pass => vl
        case _ =>
          vl match {
            case wv @ WithValue(_, _) => this +: wv
            case _                    => WithValue(this, vl)
          }
      }
  }

  private def par(d: Doc): Doc =
    Doc.char('(') + d + Doc.char(')')

  private def maybePar(c: Expression): Doc =
    c match {
      case Lambda(_, _) | Ternary(_, _, _) => par(toDoc(c))
      case _                               => toDoc(c)
    }

  private def iflike(name: String, cond: Doc, body: Doc): Doc =
    Doc.text(name) + Doc.space + cond + Doc.char(':') + (Doc.hardLine + body)
      .nested(4)

  private val trueDoc = Doc.text("True")
  private val falseDoc = Doc.text("False")
  private val lamDoc = Doc.text("lambda ")
  private val colonSpace = Doc.text(": ")
  private val spaceIfSpace = Doc.text(" if ")
  private val spaceElseSpace = Doc.text(" else ")
  private val unitDoc = Doc.text("()")
  private val elseColon = Doc.text("else:")
  private val defDoc = Doc.text("def")
  private val retSpaceDoc = Doc.text("return ")
  private val whileDoc = Doc.text("while")
  private val spaceEqSpace = Doc.text(" = ")

  def exprToDoc(expr: Expression): Doc =
    expr match {
      case PyInt(bi) => Doc.text(bi.toString)
      case PyString(s) =>
        Doc.char('"') + Doc.text(StringUtil.escape('"', s)) + Doc.char('"')
      case PyBool(b) =>
        if (b) trueDoc
        else falseDoc
      case Ident(i)                  => Doc.text(i)
      case o @ Op(_, _, _)           => o.toDoc
      case Parens(inner @ Parens(_)) => exprToDoc(inner)
      case Parens(p)                 => par(exprToDoc(p))
      case SelectItem(x, i) =>
        maybePar(x) + Doc.char('[') + Doc.str(i) + Doc.char(']')
      case SelectRange(x, os, oe) =>
        val middle = os.fold(Doc.empty)(exprToDoc) + Doc.char(':') + oe.fold(
          Doc.empty
        )(exprToDoc)
        maybePar(x) + (Doc.char('[') + middle + Doc.char(']')).nested(4)
      case Ternary(ift, cond, iff) =>
        // python parses the else condition as the rest of experssion, so
        // no need to put parens around it
        maybePar(ift) + spaceIfSpace + maybePar(
          cond
        ) + spaceElseSpace + exprToDoc(iff)
      case MakeTuple(items) =>
        items match {
          case Nil      => unitDoc
          case h :: Nil => par(exprToDoc(h) + Doc.comma).nested(4)
          case twoOrMore =>
            par(
              Doc
                .intercalate(Doc.comma + Doc.line, twoOrMore.map(exprToDoc))
                .grouped
            ).nested(4)
        }
      case MakeList(items) =>
        val inner = items.map(exprToDoc)
        (Doc.char('[') + Doc
          .intercalate(Doc.comma + Doc.line, inner)
          .grouped + Doc.char(']')).nested(4)
      case Lambda(args, res) =>
        lamDoc + Doc.intercalate(
          Doc.comma + Doc.space,
          args.map(exprToDoc)
        ) + colonSpace + exprToDoc(res)

      case Apply(fn, args) =>
        maybePar(fn) + par(
          Doc.intercalate(Doc.comma + Doc.line, args.map(exprToDoc)).grouped
        ).nested(4)

      case DotSelect(left, right) =>
        val ld = left match {
          case PyInt(_) => par(exprToDoc(left))
          case _        => exprToDoc(left)
        }
        ld + Doc.char('.') + exprToDoc(right)
    }

  def toDoc(c: Code): Doc =
    c match {
      case expr: Expression => exprToDoc(expr)
      case Call(ap)         => toDoc(ap)

      case ClassDef(name, ex, body) =>
        val exDoc =
          if (ex.isEmpty) Doc.empty
          else par(Doc.intercalate(Doc.comma + Doc.space, ex.map(toDoc)))

        Doc.text("class") + Doc.space + Doc.text(name.name) + exDoc + Doc.char(
          ':'
        ) + (Doc.hardLine +
          toDoc(body)).nested(4)

      case IfStatement(conds, Some(Pass)) =>
        toDoc(IfStatement(conds, None))

      case IfStatement(conds, optElse) =>
        val condsDoc = conds.map { case (x, b) => (toDoc(x), toDoc(b)) }
        val i1 = iflike("if", condsDoc.head._1, condsDoc.head._2)
        val i2 = condsDoc.tail.map { case (x, b) => iflike("elif", x, b) }
        val el = optElse.fold(Doc.empty) { els =>
          Doc.hardLine + elseColon + (Doc.hardLine + toDoc(els)).nested(4)
        }

        Doc.intercalate(Doc.hardLine, i1 :: i2) + el

      case Block(stmts) =>
        Doc.intercalate(Doc.hardLine, stmts.map(toDoc).toList)

      case Def(nm, args, body) =>
        defDoc + Doc.space + Doc.text(nm.name) +
          par(Doc.intercalate(Doc.comma + Doc.lineOrSpace, args.map(toDoc)))
            .nested(4) + Doc.char(':') + (Doc.hardLine + toDoc(body)).nested(4)

      case Return(expr) => retSpaceDoc + toDoc(expr)

      case Assign(nm, expr) => toDoc(nm) + spaceEqSpace + toDoc(expr)
      case Pass             => Doc.text("pass")
      case While(cond, body) =>
        whileDoc + Doc.space + toDoc(cond) + Doc.char(
          ':'
        ) + (Doc.hardLine + toDoc(body)).nested(4)
      case Import(name, aliasOpt) =>
        // import name as alias
        val imp = Doc.text("import") + Doc.space + Doc.text(name)
        aliasOpt.fold(imp) { a =>
          imp + Doc.space + Doc.text("as") + Doc.space + toDoc(a)
        }
    }

  /////////////////////////
  // Here are all the expressions
  /////////////////////////

  case class PyInt(toBigInteger: BigInteger) extends Expression {
    def simplify = this
  }
  case class PyString(content: String) extends Expression {
    def simplify = this
  }
  case class PyBool(toBoolean: Boolean) extends Expression {
    def simplify = this
  }
  case class Ident(name: String) extends Expression {
    def simplify = this
  }
  // Binary operator used for +, -, and, == etc...
  case class Op(left: Expression, op: Operator, right: Expression)
      extends Expression {
    // operators like + can associate
    //
    def toDoc: Doc = {
      // invariant: all items in right associate
      def loop(
          left: Expression,
          rights: NonEmptyList[(Operator, Expression)]
      ): Doc =
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
              case (ol, or @ Op(_, o2, _)) if !ol.associates(o2) =>
                // we we can't break rights.head because the ops
                // don't associate. We wrap or in Parens
                val rights1 = (ol, Parens(or))
                loop(leftNotOp, NonEmptyList(rights1, rights.tail))
              case (ol, Op(r1, o2, r2)) =>
                // maintain the invariant that all the rights associate
                loop(leftNotOp, NonEmptyList((ol, r1), (o2, r2) :: rights.tail))
              case (ol, rightNotOp) =>
                rights.tail match {
                  case Nil =>
                    maybePar(leftNotOp) + Doc.space + Doc.text(
                      ol.name
                    ) + Doc.space + maybePar(rightNotOp)
                  case (o2, r2) :: rest =>
                    // everything in rights associate
                    val leftDoc = maybePar(leftNotOp) + Doc.space + Doc.text(
                      ol.name
                    ) + Doc.space
                    if (ol.associates(o2)) {
                      leftDoc + loop(rightNotOp, NonEmptyList((o2, r2), rest))
                    } else {
                      // we need to put a parens ending after rightNotOp
                      // leftNotOp ol (rightNotOp o2 r2 :: rest)
                      leftDoc + par(
                        loop(rightNotOp, NonEmptyList((o2, r2), rest))
                      )
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
        case Op(i @ PyInt(a), Const.Times, right) =>
          if (a == BigInteger.ZERO) i
          else if (a == BigInteger.ONE) right.simplify
          else right.simplify.evalTimes(i)
        case Op(left, Const.Times, i @ PyInt(b)) =>
          if (b == BigInteger.ZERO) i
          else if (b == BigInteger.ONE) left.simplify
          else {
            val l1 = left.simplify
            if (l1 == left) this
            else (l1.evalTimes(i))
          }
        case Op(i @ PyInt(a), Const.Plus, right) =>
          if (a == BigInteger.ZERO) right.simplify
          else {
            val r1 = right.simplify
            // put the constant on the right
            r1.evalPlus(i)
          }
        case Op(left, Const.Plus, i @ PyInt(b)) =>
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
                      // (ll - rl) + i == ll - (rl - i)
                      ll.evalMinus(rl.evalMinus(i))
                    case _ => this
                  }
                case _ => this
              }
            } else (l1.evalPlus(i))
          }
        case Op(i @ PyInt(_), Const.Minus, right) =>
          val r1 = right.simplify
          if (r1 == right) {
            r1 match {
              case Op(rl, io: IntOp, rr) =>
                io match {
                  case Const.Plus =>
                    // right associate
                    rl.evalPlus(rr.evalPlus(i))
                  case Const.Minus =>
                    // i - (rl - rr)
                    rr match {
                      case ri @ PyInt(_) =>
                        Op(i.evalPlus(ri), Const.Minus, rl)
                      case _ => this
                    }
                  case _ => this
                }
              case _ => this
            }
          } else (i.evalMinus(r1))
        case Op(left, Const.Minus, i @ PyInt(b)) =>
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
                      // (ll - rl) - i == ll - (rl + i)
                      ll.evalMinus(rl.evalPlus(i))
                    case _ => this
                  }
                case _ => this
              }
            } else (l1.evalMinus(i))
          }
        case Op(a, Const.Eq, b) if a == b                        => Const.True
        case Op(a, Const.Gt | Const.Lt | Const.Neq, b) if a == b => Const.False
        case Op(PyInt(a), Const.Gt, PyInt(b)) =>
          fromBoolean(a.compareTo(b) > 0)
        case Op(PyInt(a), Const.Lt, PyInt(b)) =>
          fromBoolean(a.compareTo(b) < 0)
        case Op(PyInt(a), Const.Neq, PyInt(b)) =>
          fromBoolean(a != b)
        case Op(PyInt(a), Const.Eq, PyInt(b)) =>
          fromBoolean(a == b)
        case Op(a, Const.And, b) =>
          a.simplify match {
            case Const.True  => b.simplify
            case Const.False => Const.False
            case a1 =>
              b.simplify match {
                case Const.True  => a1
                case Const.False => Const.False
                case b1          => Op(a1, Const.And, b1)
              }
          }
        case _ =>
          val l1 = left.simplify
          val r1 = right.simplify
          if ((l1 != left) || (r1 != right)) {
            Op(l1, op, r1).simplify
          } else {
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

  case class Parens(expr: Expression) extends Expression {
    def simplify: Expression =
      expr.simplify match {
        case x @ (PyBool(_) | Ident(_) | PyInt(_) | PyString(_) | Parens(_)) =>
          x
        case exprS => Parens(exprS)
      }
  }
  case class SelectItem(arg: Expression, position: Int) extends Expression {
    def simplify: Expression =
      arg.simplify match {
        case MakeTuple(items) if items.lengthCompare(position) > 0 =>
          items(position)
        case MakeList(items) if items.lengthCompare(position) > 0 =>
          items(position)
        case simp =>
          SelectItem(simp, position)
      }
  }
  // foo[a:b]
  case class SelectRange(
      arg: Expression,
      start: Option[Expression],
      end: Option[Expression]
  ) extends Expression {
    def simplify = SelectRange(arg, start.map(_.simplify), end.map(_.simplify))
  }
  case class Ternary(ifTrue: Expression, cond: Expression, ifFalse: Expression)
      extends Expression {
    def simplify: Expression =
      cond.simplify match {
        case PyBool(b) =>
          if (b) ifTrue.simplify else ifFalse.simplify
        case PyInt(i) =>
          if (i != BigInteger.ZERO) ifTrue.simplify else ifFalse.simplify
        case notStatic =>
          Ternary(ifTrue.simplify, notStatic, ifFalse.simplify)
      }
  }
  case class MakeTuple(args: List[Expression]) extends Expression {
    def simplify = MakeTuple(args.map(_.simplify))
  }
  case class MakeList(args: List[Expression]) extends Expression {
    def simplify = MakeList(args.map(_.simplify))
  }
  case class Lambda(args: List[Ident], result: Expression) extends Expression {
    def simplify = Lambda(args, result.simplify)
  }
  case class Apply(fn: Expression, args: List[Expression]) extends Expression {
    def uncurry: (Expression, List[List[Expression]]) =
      fn match {
        case a @ Apply(_, _) =>
          val (fn0, args0) = a.uncurry
          (fn0, args0 :+ args)
        case _ =>
          (fn, args :: Nil)
      }

    def simplify = Apply(fn.simplify, args.map(_.simplify))
  }
  case class DotSelect(ex: Expression, ident: Ident) extends Expression {
    def simplify = DotSelect(ex.simplify, ident)
  }

  /////////////////////////
  // Here are all the ValueLike
  /////////////////////////

  // this prepares an expression with a number of statements
  case class WithValue(statement: Statement, value: ValueLike)
      extends ValueLike {
    def +:(stmt: Statement): WithValue =
      WithValue(stmt +: statement, value)

    def :+(stmt: Statement): WithValue =
      WithValue(statement :+ stmt, value)
  }
  case class IfElse(
      conds: NonEmptyList[(Expression, ValueLike)],
      elseCond: ValueLike
  ) extends ValueLike

  /////////////////////////
  // Here are all the Statements
  /////////////////////////

  case class Call(sideEffect: Apply) extends Statement
  // extends are really certain DotSelects, but we can't constrain that much
  case class ClassDef(
      name: Ident,
      extendList: List[Expression],
      body: Statement
  ) extends Statement
  case class Block(stmts: NonEmptyList[Statement]) extends Statement
  case class IfStatement(
      conds: NonEmptyList[(Expression, Statement)],
      elseCond: Option[Statement]
  ) extends Statement
  case class Def(name: Ident, args: List[Ident], body: Statement)
      extends Statement
  case class Return(expr: Expression) extends Statement
  case class Assign(target: Expression, value: Expression) extends Statement
  case object Pass extends Statement
  case class While(cond: Expression, body: Statement) extends Statement
  case class Import(modname: String, alias: Option[Ident]) extends Statement

  def ifStatement(
      conds: NonEmptyList[(Expression, Statement)],
      elseCond: Option[Statement]
  ): Statement = {
    val simpConds = conds.map { case (e, s) => (e.simplify, s) }

    val allBranches: NonEmptyList[(Expression, Statement)] =
      elseCond match {
        case Some(s) => simpConds :+ ((Code.Const.True, s))
        case None    => simpConds
      }

    // we know the returned expression is never a constant expression
    def untilTrue(
        lst: List[(Expression, Statement)]
    ): (List[(Expression, Statement)], Statement) =
      lst match {
        case Nil                          => (Nil, Pass)
        case (Code.Const.True, last) :: _ => (Nil, last)
        case head :: tail =>
          val (rest, e) = untilTrue(tail)
          (head :: rest, e)
      }

    val (branches, last) = untilTrue(allBranches.toList)
    NonEmptyList.fromList(branches) match {
      case Some(nel) =>
        val ec = if (last == Pass) None else Some(last)
        IfStatement(nel, ec)
      case None =>
        last
    }
  }

  /*
   * if __name__ == "__main__":
   *   stmt
   */
  def mainStatement(stmt: Statement): Statement = {
    val cond = Op(Ident("__name__"), Const.Eq, PyString("__main__"))
    IfStatement(NonEmptyList.of((cond, stmt)), None)
  }

  def addAssign(variable: Expression, code: ValueLike): Statement =
    code match {
      case x: Expression =>
        Assign(variable, x)
      case WithValue(stmt, v) =>
        stmt +: addAssign(variable, v)
      case IfElse(conds, elseCond) =>
        ifStatement(
          conds.map { case (b, v) =>
            (b, addAssign(variable, v))
          },
          Some(addAssign(variable, elseCond))
        )
    }

  def flatten(s: Statement): List[Statement] =
    s match {
      case Pass         => Nil
      case Block(stmts) => stmts.toList.flatMap(flatten)
      case single       => single :: Nil
    }

  def block(stmt: Statement, rest: Statement*): Statement = {
    val all = (stmt :: rest.toList).flatMap(flatten)
    all match {
      case Nil        => Pass
      case one :: Nil => one
      case head :: tail =>
        Block(NonEmptyList(head, tail))
    }
  }

  // just for better type inference
  def pass: Statement = Pass

  private object FinalAssign {
    def unapply(stmt: Statement): Option[(Statement, Ident, Expression)] =
      stmt match {
        case Block(stmts) =>
          unapply(stmts.last).map { case (s0, i, e) =>
            val s1 =
              NonEmptyList.fromList(stmts.init) match {
                case None        => s0
                case Some(inits) => Block(inits) :+ s0
              }
            (s1, i, e)
          }

        case Assign(i @ Ident(_), expr) => Some((Pass, i, expr))
        case _                          => None
      }
  }

  def toReturn(v: ValueLike): Statement =
    v match {
      case x: Expression => Code.Return(x)
      case WithValue(FinalAssign(s0, v0, x0), v) if v0 == v =>
        s0 :+ Code.Return(x0)
      case WithValue(stmt, v) =>
        stmt :+ toReturn(v)
      case IfElse(conds, elseCond) =>
        ifStatement(
          conds.map { case (c, v) =>
            (c, toReturn(v))
          },
          Some(toReturn(elseCond))
        )
    }

  // boolean expressions can contain side effects
  // this runs the side effects but discards
  // and resulting value
  // we could assert the value, statically
  // that assertion should always be true
  def always(v: ValueLike): Statement =
    v match {
      case _: Expression => Pass
      case WithValue(stmt, v) =>
        stmt +: always(v)
      case IfElse(conds, elseCond) =>
        ifStatement(
          conds.map { case (b, v) =>
            (b, always(v))
          },
          Some(always(elseCond))
        )
    }

  def litToExpr(lit: Lit): Expression =
    lit match {
      case Lit.Str(s)      => PyString(s)
      case Lit.Integer(bi) => PyInt(bi)
    }

  def fromInt(i: Int): Expression =
    fromLong(i.toLong)

  def fromLong(i: Long): Expression =
    if (i == 0L) Const.Zero
    else if (i == 1L) Const.One
    else PyInt(BigInteger.valueOf(i))

  def fromBoolean(b: Boolean): Expression =
    if (b) Code.Const.True else Code.Const.False

  sealed abstract class Operator(val name: String) {
    def associates(that: Operator): Boolean = {
      // true if (a this b) that c == a this (b that c)
      this match {
        case Const.Plus  => (that == Const.Plus) || (that == Const.Minus)
        case Const.Minus => false
        case Const.And   => that == Const.And
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
        case Const.Plus  => a.add(b)
        case Const.Minus => a.subtract(b)
        case Const.Times => a.multiply(b)
        case Const.Div   => PredefImpl.divBigInteger(a, b)
        case Const.Mod   => PredefImpl.modBigInteger(a, b)
      }
  }

  object Const {
    case object Plus extends IntOp("+")
    case object Minus extends IntOp("-")
    case object Times extends IntOp("*")
    case object Div extends IntOp("//")
    case object Mod extends IntOp("%")
    case object And extends Operator("and")
    case object Eq extends Operator("==")
    case object Neq extends Operator("!=")
    case object Gt extends Operator(">")
    case object Lt extends Operator("<")

    val True = PyBool(true)
    val False = PyBool(false)

    val Zero = PyInt(BigInteger.ZERO)
    val One = PyInt(BigInteger.ONE)

    val Unit = MakeTuple(Nil)
  }

  val python2Name: java.util.regex.Pattern =
    "[_A-Za-z][_0-9A-Za-z]*".r.pattern

  val pyKeywordList: Set[String] = Set(
    "and",
    "del",
    "from",
    "not",
    "while",
    "as",
    "elif",
    "global",
    "or",
    "with",
    "assert",
    "else",
    "if",
    "pass",
    "yield",
    "break",
    "except",
    "import",
    "print",
    "class",
    "exec",
    "in",
    "raise",
    "continue",
    "finally",
    "is",
    "return",
    "def",
    "for",
    "lambda",
    "try"
  )
}
