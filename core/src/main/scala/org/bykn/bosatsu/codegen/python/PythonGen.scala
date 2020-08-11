package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.{PackageName, Identifier, Matchless, Lit}
import cats.Monad
import cats.data.NonEmptyList

import Identifier.Bindable
import Matchless._

import cats.implicits._

object PythonGen {
  // Structs are represented as tuples
  // Enums are represented as tuples with an additional first field holding
  // the variant

  sealed trait Code
  // Expressions or Code that has a final value
  sealed trait ValueLike extends Code
  sealed abstract class Expression extends ValueLike {
    def identOrParens: Expression =
      this match {
        case i: Code.Ident => i
        case p => Code.Parens(p)
      }
  }
  sealed abstract class Statement extends Code {
    import Code.Block

    def statements: NonEmptyList[Statement] =
      this match {
        case Block(ss) => ss
        case notBlock => NonEmptyList(notBlock, Nil)
      }

    def +:(stmt: Statement): Block =
      Block(stmt :: statements)
    def :+(stmt: Statement): Block =
      Block(statements :+ stmt)
  }

  object Code {
    // True, False, None, numbers
    case class Literal(asString: String) extends Expression
    case class PyString(content: String) extends Expression
    case class Ident(name: String) extends Expression
    // Binary operator used for +, -, and, == etc...
    case class Op(left: Expression, name: String, right: Expression) extends Expression
    case class Parens(expr: Expression) extends Expression
    case class SelectTuple(arg: Expression, position: Expression) extends Expression
    case class MakeTuple(args: List[Expression]) extends Expression
    case class Lambda(args: List[Ident], result: Expression) extends Expression
    case class Apply(fn: Expression, args: List[Expression]) extends Expression

    // this prepares an expression with a number of statements
    case class WithValue(statement: Statement, value: ValueLike) extends ValueLike {
      def +:(stmt: Statement): WithValue =
        WithValue(stmt +: statement, value)

      def :+(stmt: Statement): WithValue =
        WithValue(statement :+ stmt, value)
    }
    case class IfElse(conds: NonEmptyList[(Expression, ValueLike)], elseCond: ValueLike) extends ValueLike

    case class Block(stmts: NonEmptyList[Statement]) extends Statement
    case class IfStatement(conds: NonEmptyList[(Expression, Statement)], elseCond: Option[Statement]) extends Statement
    case class Def(name: Ident, args: List[Ident], body: Statement) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Assign(variable: Ident, value: Expression) extends Statement

    def addAssign(variable: Ident, code: ValueLike): Statement =
      code match {
        case x: Expression =>
          Assign(variable, x)
        case WithValue(stmt, v) =>
          stmt +: addAssign(variable, v)
        case IfElse(conds, elseCond) =>
          IfStatement(
            conds.map { case (b, v) =>
              (b, addAssign(variable, b))
            },
            Some(addAssign(variable, elseCond))
          )
      }

    def let(n: Ident, v: ValueLike, in: ValueLike): ValueLike =
      WithValue(addAssign(n, v), in)

    def onLasts(cs: List[ValueLike])(fn: List[Expression] => ValueLike): Env[ValueLike] = {
      def loop(cs: List[ValueLike], setup: List[Statement], args: List[Expression]): Env[ValueLike] =
        cs match {
          case Nil => Monad[Env].pure {
            val res = fn(args.reverse)
            NonEmptyList.fromList(setup) match {
              case None => res
              case Some(nel) =>
                WithValue(Block(nel.reverse), res)
            }
          }
          case (e: Expression) :: t => loop(t, setup, e :: args)
          case (ifelse@IfElse(_, _)) :: tail =>
            // we allocate a result and assign
            // the result on each value
            Env.newAssignableVar.flatMap { v =>
              loop(tail, addAssign(v, ifelse) :: setup, v :: args)
            }
          case WithValue(decl, v) :: tail =>
            loop(v :: tail, decl :: setup, args)
        }

      loop(cs, Nil, Nil)
    }

    def onLast(c: ValueLike)(fn: Expression => ValueLike): Env[ValueLike] =
      onLasts(c :: Nil) {
        case x :: Nil => fn(x)
        case other =>
          throw new IllegalStateException(s"expected list to have size 1: $other")
      }

    def andCode(c1: ValueLike, c2: ValueLike): Env[ValueLike] =
      onLasts(c1 :: c2 :: Nil) {
        case e1 :: e2 :: Nil =>
          Op(e1, Const.And, e2)
        case other =>
          throw new IllegalStateException(s"expected list to have size 2: $other")
      }

    object Const {
      val True = Literal("True")
      val None = Literal("None")
      val Zero = Literal("0")
      val One = Literal("1")
      val Minus = "-"
      val And = "and"
      val Eq = "=="
      val Gt = ">"
    }

  }

  sealed abstract class Env[+A]
  object Env {
    implicit def envMonad: Monad[Env] = ???

    def render(env: Env[Code]): String = ???

    def escape(b: Bindable): Env[Code.Ident] = ???
    def nameForAnon(long: Long): Env[Code.Ident] = ???
    def newAssignableVar: Env[Code.Ident] = ???
  }

  def apply(me: Expr)(resolve: (PackageName, Identifier) => Env[ValueLike]): Env[ValueLike] = {
    val ops = new Impl.Ops(resolve)
    ops.loop(me)
  }

  private object Impl {
    class Ops(resolve: (PackageName, Identifier) => Env[ValueLike]) {
      /**
       * enums and structs are tuples
       * enums first parameter is their index
       * nats are just integers
       */
      def makeCons(ce: ConsExpr, args: List[ValueLike]): Env[ValueLike] =
        ???
        /*
        c match {
          case MakeEnum(variant, arity) =>
            if (arity == 0) SumValue(variant, UnitValue)
            else if (arity == 1) {
              FnValue { v => SumValue(variant, ConsValue(v, UnitValue)) }
            }
            else
              FnValue.curry(arity) { args =>
                val prod = ProductValue.fromList(args)
                SumValue(variant, prod)
              }
          case MakeStruct(arity) =>
            if (arity == 0) UnitValue
            else if (arity == 1) FnValue.identity
            else FnValue.curry(arity)(ProductValue.fromList(_))
          case ZeroNat => zeroNat
          case SuccNat => succNat
        }
      */
      def litToExpr(lit: Lit): Expression =
        lit match {
          case Lit.Str(s) => Code.PyString(s)
          case Lit.Integer(bi) => Code.Literal(bi.toString)
        }

      def boolExpr(ix: BoolExpr): Env[ValueLike] =
        ix match {
          case EqualsLit(expr, lit) =>
            val literal = litToExpr(lit)
            loop(expr).flatMap(Code.onLast(_) { ex => Code.Op(ex, Code.Const.Eq, literal) })
          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.flatMap(Code.onLast(_) { x =>
                Code.Op(x, Code.Const.Eq, Code.Const.Zero)
              })
            else
              natF.flatMap(Code.onLast(_) { x =>
                Code.Op(x, Code.Const.Gt, Code.Const.Zero)
              })

          case TrueConst => Monad[Env].pure(Code.Const.True)
          case And(ix1, ix2) =>
            (boolExpr(ix1), boolExpr(ix2))
              .mapN(Code.andCode(_, _))
              .flatten
          case CheckVariant(enumV, idx) =>
            loop(enumV).flatMap { tup =>
              Code.onLast(tup) { t =>
                Code.Op(Code.SelectTuple(t, Code.Const.Zero), Code.Const.Eq, Code.Literal(idx.toString))
              }
            }
          case SetMut(LocalAnonMut(mut), expr) =>
            (Env.nameForAnon(mut), loop(expr))
              .mapN { (ident, result) =>
                Code.onLast(result) { resx =>
                  val a = Code.Assign(ident, resx)
                  Code.WithValue(a, Code.Const.True)
                }
              }
              .flatten
          case SearchList(LocalAnonMut(mutV), init, check, optLeft) => ???
        }

      def makeDef(defName: Code.Ident, arg: Code.Ident, v: ValueLike): Env[Code.Def] =
        Env.newAssignableVar.map { resName =>
          Code.Def(defName, arg :: Nil,
            Code.addAssign(resName, v) :+
              Code.Return(resName)
            )
        }

      def loop(expr: Expr): Env[ValueLike] =
        expr match {
          case Lambda(_, arg, res) =>
            // python closures work the same so we don't
            // need to worry about what we capture
            (Env.escape(arg), loop(expr)).mapN { (arg, res) =>
              res match {
                case x: Expression =>
                  Monad[Env].pure(Code.Lambda(arg :: Nil, x))
                case v =>
                  for {
                    defName <- Env.newAssignableVar
                    defn <- makeDef(defName, arg, v)
                  } yield Code.WithValue(defn, defName)
              }
            }
            .flatten
          case LoopFn(caps, thisName, argshead, argstail, body) => ???
          case Global(p, n) => resolve(p, n)
          case Local(b) => Env.escape(b)
          case LocalAnon(a) => Env.nameForAnon(a)
          case LocalAnonMut(m) => Env.nameForAnon(m)
          case App(cons: ConsExpr, args) =>
            args.traverse(loop).flatMap { pxs => makeCons(cons, pxs.toList) }
          case App(expr, args) =>
            (loop(expr), args.traverse(loop))
              .mapN { (fn, args) =>
                Code.onLasts(fn :: args.toList) {
                  case fn :: ah :: atail =>
                    // all functions are curried, a future
                    // optimization would improve that
                    atail.foldLeft(Code.Apply(fn.identOrParens, ah :: Nil)) { (left, arg) =>
                      Code.Apply(Code.Parens(left), arg :: Nil)
                    }
                  case other => throw new IllegalStateException(s"got $other, expected to match $expr")
                }
              }
              .flatten
          case Let(localOrBind, value, in) =>
            val valueF = loop(value)
            val inF = loop(in)

            localOrBind match {
              case Right((b, rec)) =>
                if (rec.isRecursive) {
                  value match {
                    case Lambda(_, arg, res) =>
                      // python defs already support recursion:
                      for {
                        dn <- Env.escape(b)
                        an <- Env.escape(arg)
                        rV <- loop(res)
                        defstmt <- makeDef(dn, an, rV)
                        inV <- loop(in)
                      } yield Code.WithValue(defstmt, inV)
                    case notLambda =>
                      // this shouldn't come up strictly,
                      // but due to some normalization, it could
                      // be that some let was floated here and
                      // the lambda is inside
                      sys.error(s"???, support recursive value of $notLambda")
                  }
                }
                else {
                  (Env.escape(b), valueF, inF).mapN { (n, v, i) =>
                    Code.let(n, v, i)
                  }
                }
              case Left(LocalAnon(l)) =>
                (Env.nameForAnon(l), valueF, inF).mapN { (n, v, i) =>
                    Code.let(n, v, i)
                  }
            }
          case LetMut(LocalAnonMut(_), in) =>
            // we could delete this name, but
            // there is no need to
            loop(in)
          case Literal(lit) => Monad[Env].pure(litToExpr(lit))
          case If(cond, thenExpr, elseExpr) =>
            (boolExpr(cond), loop(thenExpr), loop(elseExpr))
              .mapN { (cV, thenV, elseV) =>
                ???
              }
          case Always(cond, expr) => ???
          case MatchString(str, pat, binds) => ???
          case GetEnumElement(expr, _, idx, _) =>
            // enums are just structs with the first element being the variant
            // we could assert the v matches when debugging, but typechecking
            // should assure this
            loop(expr).flatMap { tup =>
              Code.onLast(tup) { t =>
                Code.SelectTuple(t, Code.Literal((idx + 1).toString))
              }
            }
          case GetStructElement(expr, idx, sz) =>
            val exprR = loop(expr)
            if (sz == 1) {
              // we don't bother to wrap single item structs
              exprR
            }
            else {
              // structs are just tuples
              exprR.flatMap { tup =>
                Code.onLast(tup) { t =>
                  Code.SelectTuple(t, Code.Literal(idx.toString))
                }
              }
            }
          case PrevNat(expr) =>
            // Nats are just integers
            loop(expr).flatMap { nat =>
              Code.onLast(nat)(Code.Op(_, Code.Const.Minus, Code.Const.One))
            }
          case cons: ConsExpr => makeCons(cons, Nil)
        }
    }
  }
}
