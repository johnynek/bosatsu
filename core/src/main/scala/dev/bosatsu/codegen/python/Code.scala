package dev.bosatsu.codegen.python

import cats.Eq
import cats.data.{Chain, NonEmptyList}
import java.math.BigInteger
import dev.bosatsu.{Lit, PredefImpl, StringUtil}
import org.typelevel.paiges.Doc
import scala.language.implicitConversions
import cats.syntax.all._

// Structs are represented as tuples
// Enums are represented as tuples with an additional first field holding
// the variant

sealed trait Code derives CanEqual

object Code {

  // Not necessarily code, but something that has a final value
  // this allows us to add IfElse as a an expression (which
  // is not yet valid python) a series of lets before an expression
  sealed trait ValueLike {
    def returnsBool: Boolean =
      this match {
        case PyBool(_)             => true
        case _: Expression         => false
        case WithValue(_, v)       => v.returnsBool
        case IfElse(ifs, elseCond) =>
          elseCond.returnsBool && ifs.forall { case (_, v) => v.returnsBool }
      }
  }

  sealed abstract class Expression extends ValueLike with Code derives CanEqual {

    def countOf(ident: Ident): Int

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

    def :>(that: Expression): Expression =
      Code.Op(this, Code.Const.Gt, that)

    def :<(that: Expression): Expression =
      Code.Op(this, Code.Const.Lt, that)

    def =:=(that: Expression): Expression =
      Code.Op(this, Code.Const.Eq, that)

    def =!=(that: Expression): Expression =
      Code.Op(this, Code.Const.Neq, that)

    def evalAnd(that: Expression): Expression =
      eval(Const.And, that)

    def evalPlus(that: Expression): Expression =
      eval(Const.Plus, that)

    def +(that: Expression): Expression =
      evalPlus(that)

    def unary_! : Expression =
      Not(this)

    def evalMinus(that: Expression): Expression =
      eval(Const.Minus, that)

    def -(that: Expression): Expression = evalMinus(that)

    def evalTimes(that: Expression): Expression =
      eval(Const.Times, that)

    def :=(vl: ValueLike): Statement =
      addAssign(this, vl)

    def len(): Expression =
      dot(Code.Ident("__len__"))()

    def simplify: Expression
  }

  sealed abstract class Statement extends Code derives CanEqual {
    def statements: NonEmptyList[Statement] =
      this match {
        case Block(ss) => ss
        case notBlock  => NonEmptyList.one(notBlock)
      }

    def +:(stmt: Statement): Statement =
      stmt match {
        case Pass => this
        case _    =>
          if (this == Pass) stmt
          else Block(stmt :: statements)
      }

    def :+(stmt: Statement): Statement =
      stmt match {
        case Pass => this
        case _    =>
          if (this == Pass) stmt
          else Block(statements ::: stmt.statements)
      }

    def withValue(vl: ValueLike): ValueLike =
      this match {
        case Pass => vl
        case _    =>
          vl match {
            case wv @ WithValue(_, _) => this +: wv
            case _                    => WithValue(this, vl)
          }
      }
  }

  implicit val eqExpression: Eq[Expression] =
    Eq.fromUniversalEquals
  implicit val eqStatement: Eq[Statement] =
    Eq.fromUniversalEquals
  implicit val eqOperator: Eq[Operator] =
    Eq.fromUniversalEquals

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
      case PyInt(bi)   => Doc.text(bi.toString)
      case PyString(s) =>
        Doc.char('"') + Doc.text(StringUtil.escape('"', s)) + Doc.char('"')
      case PyBool(b) =>
        if (b) trueDoc
        else falseDoc
      case Not(n) =>
        val nd = n match {
          case Ident(_) | Parens(_) | PyBool(_) | PyInt(_) | Apply(_, _) |
              DotSelect(_, _) | SelectItem(_, _) | SelectRange(_, _, _) =>
            exprToDoc(n)
          case p => par(exprToDoc(p))
        }
        Doc.text("not ") + nd
      case Ident(i)                  => Doc.text(i)
      case o @ Op(_, _, _)           => o.toDoc
      case Parens(inner @ Parens(_)) => exprToDoc(inner)
      case Parens(p)                 => par(exprToDoc(p))
      case SelectItem(x, i)          =>
        maybePar(x) + Doc.char('[') + exprToDoc(i) + Doc.char(']')
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
          case Nil       => unitDoc
          case h :: Nil  => par(exprToDoc(h) + Doc.comma).nested(4)
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
          case PyInt(_) | Op(_, _, _) => par(exprToDoc(left))
          case _                      => exprToDoc(left)
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

      case Assign(nm, expr)  => toDoc(nm) + spaceEqSpace + toDoc(expr)
      case Pass              => Doc.text("pass")
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
    def simplify: Expression = this
    def countOf(i: Ident) = 0
  }
  case class PyString(content: String) extends Expression {
    def simplify: Expression = this
    def countOf(i: Ident) = 0
  }
  case class PyBool(toBoolean: Boolean) extends Expression {
    def simplify: Expression = this
    def countOf(i: Ident) = 0
  }
  case class Ident(name: String) extends Expression {
    def simplify: Expression = this
    def countOf(i: Ident) = if (i == this) 1 else 0
  }
  implicit val eqIdent: Eq[Ident] =
    Eq.fromUniversalEquals
  case class Not(arg: Expression) extends Expression {
    def simplify: Expression =
      arg.simplify match {
        case Not(a)                    => a
        case PyBool(b)                 => PyBool(true ^ b)
        case Const.Zero                => Const.True
        case Const.One                 => Const.False
        case Op(left, Const.Eq, right) =>
          Op(left, Const.Neq, right)
        case Op(left, Const.Neq, right) =>
          Op(left, Const.Eq, right)
        case other => Not(other)
      }

    def countOf(i: Ident) = arg.countOf(i)
  }
  // Binary operator used for +, -, and, == etc...
  case class Op(left: Expression, op: Operator, right: Expression)
      extends Expression {
    def countOf(i: Ident) = left.countOf(i) + right.countOf(i)

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

      loop(left, NonEmptyList.one((op, right)))
    }

    // prefer constants on the right
    override def simplify: Expression =
      this match {
        // handle static integer ops, if we get past here
        // we know that at least one side isn't a pure integer op
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
                // if the left has a trailing constant combine it
                case Op(ll, io: IntOp, rl @ PyInt(_)) =>
                  io match {
                    case Const.Plus =>
                      // right associate
                      ll.evalPlus(rl.evalPlus(i))
                    case Const.Minus =>
                      // (ll - rl) + i == ll - (rl - i)
                      ll.evalMinus(rl.evalMinus(i))
                    case _ => this
                  }
                // left doesn't have a trailing constant
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
                  case Const.Minus =>
                    rr match {
                      case ri @ PyInt(_) =>
                        // i - (rl - rr) == (i + rr) - rl
                        Op(i.evalPlus(ri), Const.Minus, rl)
                      case _ =>
                        rl match {
                          case ri @ PyInt(_) =>
                            // i - (rl - rr) == (i - ri) + rl
                            // combine the integers
                            Op(i.evalMinus(ri), Const.Plus, rr)
                          case _ => this
                        }
                    }
                  case Const.Plus =>
                    rr match {
                      case ri @ PyInt(_) =>
                        // i - (rl + rr) == (i - rr) - rl
                        Op(i.evalMinus(ri), Const.Minus, rl)
                      case _ =>
                        rl match {
                          case ri @ PyInt(_) =>
                            // i - (rl + rr) == (i - rl) - rr
                            // combine the integers
                            Op(i.evalMinus(ri), Const.Minus, rr)
                          case _ => this
                        }
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
        case Op(PyInt(a), Const.Gt, PyInt(b))                    =>
          fromBoolean(a.compareTo(b) > 0)
        case Op(PyInt(a), Const.Lt, PyInt(b)) =>
          fromBoolean(a.compareTo(b) < 0)
        case Op(PyInt(a), Const.Neq, PyInt(b)) =>
          fromBoolean(a != b)
        case Op(PyInt(a), Const.Eq, PyInt(b)) =>
          fromBoolean(a == b)
        case Op(a, Const.And, b) =>
          a.simplify match {
            case Const.True                      => b.simplify
            case as @ (Const.False | Const.Zero) => as
            case a1                              =>
              b.simplify match {
                case Const.True  => a1
                case Const.False => Const.False
                case b1          => Op(a1, Const.And, b1)
              }
          }
        case _ =>
          val l1 = left.simplify
          val r1 = right.simplify
          if (l1 != left || r1 != right) {
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

    def countOf(i: Ident) = expr.countOf(i)
  }
  case class SelectItem(arg: Expression, position: Expression)
      extends Expression {
    def countOf(i: Ident) = arg.countOf(i) + position.countOf(i)

    def simplify: Expression =
      (arg.simplify, position.simplify) match {
        case (MakeTuple(items), PyInt(bi))
            if (items.lengthCompare(bi.intValue()) > 0) && (bi
              .intValue() >= 0) =>
          items(bi.intValue())
        case (MakeList(items), PyInt(bi))
            if (items.lengthCompare(bi.intValue()) > 0) && (bi
              .intValue() >= 0) =>
          items(bi.intValue())
        case (simp, spos) =>
          SelectItem(simp, spos)
      }
  }
  object SelectItem {
    def apply(arg: Expression, position: Int): SelectItem =
      SelectItem(arg, Code.fromInt(position))
  }
  // foo[a:b]
  case class SelectRange(
      arg: Expression,
      start: Option[Expression],
      end: Option[Expression]
  ) extends Expression {
    def simplify: Expression =
      SelectRange(arg, start.map(_.simplify), end.map(_.simplify))

    def countOf(i: Ident) =
      arg.countOf(i) + start.fold(0)(_.countOf(i)) + end.fold(0)(_.countOf(i))
  }
  case class Ternary(ifTrue: Expression, cond: Expression, ifFalse: Expression)
      extends Expression {
    def countOf(i: Ident) =
      ifTrue.countOf(i) + cond.countOf(i) + ifFalse.countOf(i)
    def simplify: Expression =
      cond.simplify match {
        case PyBool(b) =>
          if (b) ifTrue.simplify else ifFalse.simplify
        case PyInt(i) =>
          if (i != BigInteger.ZERO) ifTrue.simplify else ifFalse.simplify
        case notStatic =>

          (ifTrue.simplify, ifFalse.simplify) match {
            case (Const.One | Const.True, Const.Zero | Const.False) =>
              // this is just the condition
              notStatic
            case (Const.Zero | Const.False, Const.One | Const.True) =>
              // this is just the not(condition)
              Not(notStatic).simplify
            case (st, sf) =>
              Ternary(st, notStatic, sf)
          }
      }
  }
  case class MakeTuple(args: List[Expression]) extends Expression {
    def simplify: Expression = MakeTuple(args.map(_.simplify))
    def countOf(i: Ident) = args.iterator.map(_.countOf(i)).sum
  }
  case class MakeList(args: List[Expression]) extends Expression {
    def simplify: Expression = MakeList(args.map(_.simplify))
    def countOf(i: Ident) = args.iterator.map(_.countOf(i)).sum
  }
  case class Lambda(args: List[Ident], result: Expression) extends Expression {
    def simplify: Expression = Lambda(args, result.simplify)

    def countOf(i: Ident) = if (args.exists(_ == i)) 0 else result.countOf(i)
    // make sure args don't shadow the given freeSet
    def unshadow(freeSet: Set[Ident]): Lambda = {
      val clashIdent =
        if (freeSet.isEmpty) Set.empty[Ident]
        else args.iterator.filter(freeSet).toSet

      if (clashIdent.isEmpty) this
      else {
        // we have to allocate new variables
        def alloc(rename: List[Ident], avoid: Set[Ident]): List[Ident] =
          rename match {
            case Nil                     => Nil
            case (i @ Ident(nm)) :: tail =>
              val nm1 =
                if (clashIdent(i)) {
                  // the following iterator is infinite and distinct, and the avoid
                  // set is finite, so the get here must terminate in at most avoid.size
                  // steps
                  Iterator
                    .from(0)
                    .map(i => Ident(nm + i.toString))
                    .collectFirst { case n if !avoid(n) => n }
                    .get

                } else i

              nm1 :: alloc(tail, avoid + nm1)
          }

        val avoids = freeSet | freeIdents(result)
        val newArgs = alloc(args, avoids)
        val resSub = args.iterator.zip(newArgs).toMap
        val res1 = substitute(resSub, result)
        Lambda(newArgs, res1)
      }
    }
  }

  case class Apply(fn: Expression, args: List[Expression]) extends Expression {
    def countOf(i: Ident) = fn.countOf(i) + args.iterator.map(_.countOf(i)).sum
    def simplify: Expression =
      fn.simplify match {
        case Lambda(largs, result) if largs.length == args.length =>
          // if this is a lambda, but the args don't match, let
          // the python error
          val subMap = largs.iterator.zip(args).toMap
          val subs = substitute(subMap, result)
          // now we can simplify after we have inlined the args
          subs.simplify
        case Parens(Lambda(largs, result)) if largs.length == args.length =>
          // if this is a lambda, but the args don't match, let
          // the python error
          val subMap = largs.iterator.zip(args).toMap
          val subs = substitute(subMap, result)
          // now we can simplify after we have inlined the args
          subs.simplify

        case notLambda =>
          Apply(notLambda, args.map(_.simplify))
      }
  }
  case class DotSelect(ex: Expression, ident: Ident) extends Expression {
    def simplify: Expression = DotSelect(ex.simplify, ident)
    def countOf(i: Ident) = ex.countOf(i)
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

  object ValueLike {
    def ifThenElse(c: Expression, t: ValueLike, e: ValueLike): ValueLike =
      c match {
        case PyBool(b)  => if (b) t else e
        case Const.Zero => e
        case Const.One  => t
        case _          =>
          // we can't evaluate now
          e match {
            case IfElse(econds, eelse) => IfElse((c, t) :: econds, eelse)
            case ex: Expression        =>
              t match {
                case tx: Expression => Ternary(tx, c, ex).simplify
                case _              => IfElse(NonEmptyList.one((c, t)), ex)
              }
            case notIf => IfElse(NonEmptyList.one((c, t)), notIf)
          }
      }
  }

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
        case Nil                           => (Nil, Pass)
        case (Code.Const.True, last) :: _  => (Nil, last)
        case (Code.Const.False, _) :: tail => untilTrue(tail)
        case head :: tail                  =>
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

  def ifElseS(
      cond: Expression,
      ifCase: Statement,
      elseCase: Statement
  ): Statement =
    ifStatement(NonEmptyList.one((cond, ifCase)), Some(elseCase))

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

  def block(stmt: Statement, rest: Statement*): Statement =
    blockFromList(stmt :: rest.toList)

  def blockFromList(list: List[Statement]): Statement = {
    val all = list.flatMap(flatten)
    all match {
      case Nil          => Pass
      case one :: Nil   => one
      case head :: tail =>
        Block(NonEmptyList(head, tail))
    }
  }

  // just for better type inference
  def pass: Statement = Pass

  // Convert chains of x = y; return x into return y
  private def simplifyReturn(stmt: Statement): Statement =
    stmt match {
      case Block(stmts) =>
        def simplifyStack(expr: Expression, stmts: List[Statement]): Statement =
          stmts match {
            case Assign(ident: Ident, ex0) :: tail
                if ex0.isInstanceOf[Ident] || expr.countOf(ident) == 1 =>
              simplifyStack(substitute(Map(ident -> ex0), expr), tail)
            case Block(items) :: tail =>
              simplifyStack(expr, items.toList reverse_::: tail)
            case _ =>
              blockFromList((Return(expr) :: stmts).reverse)
          }
        stmts.toList.reverse match {
          case Return(expr) :: tail => simplifyStack(expr, tail)
          case _                    => stmt
        }
      case _ => stmt
    }

  private def copyableExpr(expr: Expression, depth: Int = 1): Boolean =
    expr match {
      case Ident(_) | PyInt(_) | PyString(_) | PyBool(_) => true
      case Op(left, _, right) if depth > 0 =>
        copyableExpr(left, depth - 1) && copyableExpr(right, depth - 1)
      case SelectItem(arg: Ident, pos) =>
        pos match {
          case Ident(_) | PyInt(_) | PyBool(_) | PyString(_) => true
          case _                                            => false
        }
      case SelectRange(arg: Ident, start, end) =>
        def simpleIdx(e: Expression): Boolean =
          e match {
            case Ident(_) | PyInt(_) | PyBool(_) | PyString(_) => true
            case _                                            => false
          }
        start.forall(simpleIdx) && end.forall(simpleIdx)
      case _ => false
    }

  private def substituteTop(
      stmt: Statement,
      subMap: Map[Ident, Expression]
  ): Statement =
    stmt match {
      case Assign(target: Ident, value) =>
        Assign(target, substitute(subMap, value).simplify)
      case Assign(target, value) =>
        // don't rewrite assignment targets; they are definitions/mutations
        Assign(target, substitute(subMap, value).simplify)
      case Return(expr) =>
        Return(substitute(subMap, expr).simplify)
      case Call(apply) =>
        Call(substitute(subMap, apply).asInstanceOf[Apply])
      case IfStatement(conds, elseCond) =>
        val conds1 = conds.map { case (c, s) =>
          (substitute(subMap, c).simplify, s)
        }
        IfStatement(conds1, elseCond)
      case While(cond, body) =>
        val assigned = assignedIdents(body)
        val subMap1 = subMap.filterNot { case (i, _) => assigned(i) }
        While(substitute(subMap1, cond).simplify, body)
      case ClassDef(name, extendList, body) =>
        ClassDef(
          name,
          extendList.map(substitute(subMap, _).simplify),
          body
        )
      case other => other
    }

  private def copyPropBlock(
      stmts: List[Statement]
  ): (List[Statement], Boolean) = {
    var changed = false
    var subMap = Map.empty[Ident, Expression]
    val out = List.newBuilder[Statement]

    def dropDeps(
        m: Map[Ident, Expression],
        ident: Ident
    ): Map[Ident, Expression] =
      m.filterNot { case (_, ex) =>
        freeIdents(ex).contains(ident)
      }

    stmts.foreach { stmt0 =>
      val stmt = substituteTop(stmt0, subMap)
      if (stmt != stmt0) changed = true

      val subMap1 =
        stmt match {
          case Assign(t: Ident, expr) =>
            val expr1 = expr
            val m0 = dropDeps(subMap - t, t)
            if (copyableExpr(expr1) && !freeIdents(expr1).contains(t))
              m0.updated(t, expr1)
            else m0
          case Assign(_, _) =>
            // mutation/side-effect boundary
            Map.empty
          case _ =>
            // don't propagate across control flow boundaries
            Map.empty
        }

      if (subMap1 != subMap) changed = true
      subMap = subMap1
      out += stmt
    }

    (out.result(), changed)
  }

  private def freeIdentsInStatement(
      stmt: Statement,
      bound: Set[Ident]
  ): Set[Ident] =
    stmt match {
      case Assign(target: Ident, value) =>
        (freeIdents(value) -- bound) ++ (freeIdents(target) -- bound) - target
      case Assign(target, value) =>
        (freeIdents(value) -- bound) ++ (freeIdents(target) -- bound)
      case Return(expr) =>
        freeIdents(expr) -- bound
      case Call(apply) =>
        freeIdents(apply) -- bound
      case IfStatement(conds, elseCond) =>
        val condsFree = conds.foldLeft(Set.empty[Ident]) {
          case (acc, (cond, stmt)) =>
            acc ++ (freeIdents(cond) -- bound) ++ freeIdentsInStatement(
              stmt,
              bound
            )
        }
        condsFree ++ elseCond.fold(Set.empty[Ident])(
          freeIdentsInStatement(_, bound)
        )
      case While(cond, body) =>
        (freeIdents(cond) -- bound) ++ freeIdentsInStatement(body, bound)
      case Block(stmts) =>
        stmts.toList.foldLeft(Set.empty[Ident]) { (acc, st) =>
          acc ++ freeIdentsInStatement(st, bound)
        }
      case Def(name, args, body) =>
        freeIdentsInStatement(body, bound ++ args.toSet + name)
      case ClassDef(name, extendList, body) =>
        val extFree =
          extendList.iterator.flatMap(e => freeIdents(e) -- bound).toSet
        extFree ++ freeIdentsInStatement(body, bound + name)
      case Pass =>
        Set.empty
      case Import(_, _) =>
        Set.empty
    }

  private def assignedIdents(stmt: Statement): Set[Ident] =
    stmt match {
      case Assign(t: Ident, _) => Set(t)
      case Assign(_, _)        => Set.empty
      case Return(_) | Call(_) | Pass | Import(_, _) =>
        Set.empty
      case Block(stmts) =>
        stmts.toList.foldLeft(Set.empty[Ident])(_ ++ assignedIdents(_))
      case IfStatement(conds, elseCond) =>
        val condsAssigned =
          conds.foldLeft(Set.empty[Ident]) { case (acc, (_, s)) =>
            acc ++ assignedIdents(s)
          }
        condsAssigned ++ elseCond.fold(Set.empty[Ident])(assignedIdents)
      case While(_, body) =>
        assignedIdents(body)
      case Def(_, _, _) | ClassDef(_, _, _) =>
        // nested definitions don't assign in the outer scope
        Set.empty
    }

  private def optimizeBlockOnce(
      stmts: List[Statement],
      liveOut: Set[Ident]
  ): (List[Statement], Set[Ident], Boolean) = {
    val (stmts1, ch1) = copyPropBlock(stmts)
    var changed = ch1

    def optStmt(
        stmt: Statement,
        liveOut: Set[Ident]
    ): (Option[Statement], Set[Ident], Boolean) =
      stmt match {
        case Assign(t: Ident, expr) =>
          if (!liveOut.contains(t)) (None, liveOut, true)
          else {
            val uses = freeIdents(expr)
            val liveIn = (liveOut - t) ++ uses
            (Some(stmt), liveIn, false)
          }
        case Assign(target, expr) =>
          val uses = freeIdents(target) ++ freeIdents(expr)
          (Some(stmt), liveOut ++ uses, false)
        case Return(expr) =>
          (Some(stmt), freeIdents(expr), false)
        case Call(apply) =>
          (Some(stmt), liveOut ++ freeIdents(apply), false)
        case IfStatement(conds, elseCond) =>
          val condUses = conds.foldLeft(Set.empty[Ident]) {
            case (acc, (c, _)) => acc ++ freeIdents(c)
          }
          val (conds1, liveIns, chs) = conds.foldLeft(
            (List.empty[(Expression, Statement)], List.empty[Set[Ident]], false)
          ) { case ((acc, lives, ch), (c, s)) =>
            val (opt, liveIn, ch1) = optStmt(s, liveOut)
            val stmt1 = opt.getOrElse(Pass)
            (acc :+ (c, stmt1), liveIn :: lives, ch || ch1)
          }
          val (else1, elseLiveIn, chElse) = elseCond match {
            case Some(s) =>
              val (opt, liveIn, ch) = optStmt(s, liveOut)
              (Some(opt.getOrElse(Pass)), liveIn, ch)
            case None =>
              (None, liveOut, false)
          }
          val liveIn =
            liveIns.foldLeft(elseLiveIn)(_ union _) ++ condUses
          (Some(IfStatement(NonEmptyList.fromListUnsafe(conds1), else1)), liveIn, chs || chElse)
        case While(cond, body) =>
          val condUses = freeIdents(cond)
          val bodyUses = freeIdentsInStatement(body, Set.empty)
          val loopLiveOut = liveOut ++ condUses ++ bodyUses
          val (opt, _, ch) = optStmt(body, loopLiveOut)
          val bodyStmt = opt.getOrElse(Pass)
          (Some(While(cond, bodyStmt)), loopLiveOut, ch)
        case Block(items) =>
          val (opt, liveIn, ch) = optimizeBlockOnce(items.toList, liveOut)
          (Some(blockFromList(opt)), liveIn, ch)
        case Def(name, args, body) =>
          val (bodyOpt, _, ch) = optStmt(body, Set.empty)
          val bodyStmt = bodyOpt.getOrElse(Pass)
          val frees = freeIdentsInStatement(bodyStmt, args.toSet + name)
          val liveIn = (liveOut - name) ++ frees
          (Some(Def(name, args, bodyStmt)), liveIn, ch)
        case ClassDef(name, extendList, body) =>
          val (bodyOpt, _, ch) = optStmt(body, Set.empty)
          val bodyStmt = bodyOpt.getOrElse(Pass)
          val frees = freeIdentsInStatement(bodyStmt, Set(name))
          val liveIn = (liveOut - name) ++ frees
          (Some(ClassDef(name, extendList, bodyStmt)), liveIn, ch)
        case Pass =>
          (Some(Pass), liveOut, false)
        case Import(_, _) =>
          (Some(stmt), liveOut, false)
      }

    var live = liveOut
    val out = List.newBuilder[Statement]
    stmts1.reverseIterator.foreach { stmt =>
      val (opt, liveIn, ch) = optStmt(stmt, live)
      changed = changed || ch
      live = liveIn
      opt.foreach(out += _)
    }

    (out.result().reverse, live, changed)
  }

  private def optimizeOnceStatements(
      stmts: List[Statement]
  ): Option[List[Statement]] = {
    val (opt, _, changed) = optimizeBlockOnce(stmts, Set.empty)
    if (changed) Some(opt) else None
  }

  def optimizeStatements(
      stmts: List[Statement],
      maxPasses: Int = 6
  ): List[Statement] = {
    var current = stmts
    var i = 0
    var changed = true
    while (changed && i < maxPasses) {
      optimizeOnceStatements(current) match {
        case Some(next) =>
          current = next
          i += 1
        case None =>
          changed = false
      }
    }
    current
  }

  def substitute(subMap: Map[Ident, Expression], in: Expression): Expression =
    in match {
      case PyInt(_) | PyString(_) | PyBool(_) => in
      case Not(n)                             => Not(substitute(subMap, n))
      case i @ Ident(_)                       =>
        subMap.get(i) match {
          case Some(value) => value
          case None        => i
        }
      case Op(left, op, right) =>
        Op(substitute(subMap, left), op, substitute(subMap, right))
      case Parens(expr) =>
        Parens(substitute(subMap, expr))
      case SelectItem(arg, position) =>
        SelectItem(substitute(subMap, arg), substitute(subMap, position))
      case SelectRange(arg, start, end) =>
        SelectRange(
          substitute(subMap, arg),
          start.map(substitute(subMap, _)),
          end.map(substitute(subMap, _))
        )
      case Ternary(ifTrue, cond, ifFalse) =>
        Ternary(
          substitute(subMap, ifTrue),
          substitute(subMap, cond),
          substitute(subMap, ifFalse)
        )
      case MakeTuple(args) =>
        MakeTuple(args.map(substitute(subMap, _)))
      case MakeList(args) =>
        MakeList(args.map(substitute(subMap, _)))
      case lam @ Lambda(args, _) =>
        // the args here can shadow, so we have to remove any
        // items from subMap that have the same Ident
        val argsSet = args.toSet
        val nonShadowed = subMap.filterNot { case (i, _) => argsSet(i) }
        // if subFrees is empty, unshadow is a no-op.
        // but that is efficiently handled by unshadow
        val subFrees = nonShadowed.iterator
          .map { case (_, v) => freeIdents(v) }
          .foldLeft(nonShadowed.keySet)(_ | _)

        val Lambda(args1, res1) = lam.unshadow(subFrees)
        // now we know that none of args1 shadow anything in subFrees
        // so we can just directly substitute nonShadowed on res1
        // put another way: unshadow make substitute "commute" with lambda.
        Lambda(args1, substitute(nonShadowed, res1))
      case Apply(fn, args) =>
        Apply(substitute(subMap, fn), args.map(substitute(subMap, _)))
      case DotSelect(ex, ident) =>
        DotSelect(substitute(subMap, ex), ident)
    }

  def freeIdents(ex: Expression): Set[Ident] = {
    def loop(ex: Expression, bound: Set[Ident]): Set[Ident] =
      ex match {
        case PyInt(_) | PyString(_) | PyBool(_) => Set.empty
        case Not(e)                             => loop(e, bound)
        case i @ Ident(n)                       =>
          if (pyKeywordList(n) || bound(i)) Set.empty
          else Set(i)
        case Op(left, _, right) => loop(left, bound) | loop(right, bound)
        case Parens(expr)       =>
          loop(expr, bound)
        case SelectItem(arg, position) =>
          loop(arg, bound) | loop(position, bound)
        case SelectRange(arg, start, end) =>
          loop(arg, bound) |
            start.map(loop(_, bound)).getOrElse(Set.empty) |
            end.map(loop(_, bound)).getOrElse(Set.empty)
        case Ternary(ifTrue, cond, ifFalse) =>
          loop(ifTrue, bound) | loop(cond, bound) | loop(ifFalse, bound)
        case MakeTuple(args) =>
          args.foldLeft(Set.empty[Ident])(_ | loop(_, bound))
        case MakeList(args) =>
          args.foldLeft(Set.empty[Ident])(_ | loop(_, bound))
        case Lambda(args, result) =>
          loop(result, bound ++ args)
        case Apply(fn, args) =>
          loop(fn, bound) | args.foldLeft(Set.empty[Ident])(_ | loop(_, bound))
        case DotSelect(ex, _) =>
          loop(ex, bound)
      }

    loop(ex, Set.empty)
  }

  def toReturn(v: ValueLike): Statement = {

    def nosimplify(prefix: Chain[Statement], v: ValueLike): Statement =
      v match {
        case x: Expression => blockFromList((prefix :+ Code.Return(x)).toList)
        case WithValue(stmt, v) =>
          // re don't simplify here and wait until the end
          nosimplify(prefix :+ stmt, v)
        case IfElse(conds, elseCond) =>
          // we do simplify inside the if/else because simplify doesn't
          // look inside those
          blockFromList(
            (prefix :+ ifStatement(
              conds.map { case (c, v) =>
                (c, toReturn(v))
              },
              Some(toReturn(elseCond))
            )).toList
          )
      }

    simplifyReturn(nosimplify(Chain.empty, v))
  }

  // boolean expressions can contain side effects
  // this runs the side effects but discards
  // and resulting value
  // we could assert the value, statically
  // that assertion should always be true
  def always(v: ValueLike): Statement =
    v match {
      case _: Expression      => Pass
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
      case Lit.Chr(s)      => PyString(s)
    }

  implicit def fromInt(i: Int): Expression =
    fromLong(i.toLong)

  implicit def fromString(str: String): Expression =
    PyString(str)

  implicit def fromLong(i: Long): Expression =
    if (i == 0L) Const.Zero
    else if (i == 1L) Const.One
    else PyInt(BigInteger.valueOf(i))

  implicit def fromBoolean(b: Boolean): Expression =
    if (b) Code.Const.True else Code.Const.False

  sealed abstract class Operator(val name: String) derives CanEqual {
    def associates(that: Operator): Boolean =
      // true if (a this b) that c == a this (b that c)
      this match {
        case Const.Plus  => (that == Const.Plus) || (that == Const.Minus)
        case Const.Minus => false
        case Const.And   => that == Const.And
        case Const.Or    => that == Const.Or
        case Const.Times =>
          // (a * b) * c == a * (b * c)
          // (a * b) + c != a * (b + c)
          (that == Const.Times)
        case Const.BitwiseAnd | Const.BitwiseOr | Const.BitwiseXor =>
          // these all associate with themselves
          (this == that)
        case _ => false
      }
  }

  sealed abstract class IntOp(nm: String) extends Operator(nm) {
    def apply(a: BigInteger, b: BigInteger): BigInteger =
      this match {
        case Const.Plus              => a.add(b)
        case Const.Minus             => a.subtract(b)
        case Const.Times             => a.multiply(b)
        case Const.Div               => PredefImpl.divBigInteger(a, b)
        case Const.Mod               => PredefImpl.modBigInteger(a, b)
        case Const.BitwiseAnd        => a.and(b)
        case Const.BitwiseOr         => a.or(b)
        case Const.BitwiseXor        => a.xor(b)
        case Const.BitwiseShiftLeft  => PredefImpl.shiftLeft(a, b)
        case Const.BitwiseShiftRight => PredefImpl.shiftRight(a, b)
      }
  }

  object Const {
    case object Plus extends IntOp("+")
    case object Minus extends IntOp("-")
    case object Times extends IntOp("*")
    case object Div extends IntOp("//")
    case object Mod extends IntOp("%")
    case object BitwiseAnd extends IntOp("&")
    case object BitwiseOr extends IntOp("|")
    case object BitwiseXor extends IntOp("^")
    case object BitwiseShiftLeft extends IntOp("<<")
    case object BitwiseShiftRight extends IntOp(">>")
    case object And extends Operator("and")
    case object Or extends Operator("or")
    case object Eq extends Operator("==")
    case object Neq extends Operator("!=")
    case object Gt extends Operator(">")
    case object Lt extends Operator("<")
    case object In extends Operator("in")

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
    "await",
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
