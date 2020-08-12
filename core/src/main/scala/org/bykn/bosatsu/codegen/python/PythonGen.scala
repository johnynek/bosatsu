package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.{PackageName, Identifier, Matchless, Lit, RecursionKind}
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
    // this is used for scope management
    case class Del(ident: Ident) extends Statement
    case object Pass extends Statement

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

    def ifElse(conds: NonEmptyList[(ValueLike, ValueLike)], elseV: ValueLike): Env[ValueLike] = {
      // for all the non-expression conditions, we need to defer evaluating them
      // until they are really needed
      conds match {
        case NonEmptyList((cx: Expression, t), Nil) =>
          Monad[Env].pure(IfElse(NonEmptyList((cx, t), Nil), elseV))
        case NonEmptyList((cx: Expression, t), rh :: rt) =>
          val head = (cx, t)
          ifElse(NonEmptyList(rh, rt), elseV).map {
            case IfElse(crest, er) =>
              // preserve IfElse chains
              IfElse(head :: crest, er)
            case nest =>
              IfElse(NonEmptyList(head, Nil), nest)
          }
        case NonEmptyList((cx, t), rest) =>
          for {
            // allocate a new unshadowable var
            cv <- Env.newAssignableVar
            res <- ifElse(NonEmptyList((cv, t), rest), elseV)
          } yield WithValue(addAssign(cv, t), res)
      }
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

    // these are always recursive so we can use def to define them
    def buildLoop(name: Ident, args: NonEmptyList[Ident], body: ValueLike): Env[Statement] = {
      ???
    }

    object Const {
      val True = Literal("True")
      val None = Literal("None")
      val Zero = Literal("0")
      val One = Literal("1")
      val Minus = "-"
      val Plus = "+"
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
    def endScope(bi: Code.Ident, vl: ValueLike): Env[ValueLike] = ???
  }

  def apply(name: Bindable, me: Expr)(resolve: (PackageName, Identifier) => Env[ValueLike]): Env[Statement] = {
    val ops = new Impl.Ops(resolve)
    Env.escape(name)
      .flatMap(ops.topLet(_, me))
  }

  private object Impl {
    class Ops(resolve: (PackageName, Identifier) => Env[ValueLike]) {
      /**
       * Unit struct is None
       * enums with no fields are integers
       * enums and structs are tuples
       * enums first parameter is their index
       * nats are just integers
       */
      def makeCons(ce: ConsExpr, args: List[ValueLike]): Env[ValueLike] = {
        // invariant: args.size == arity
        def applyAll(args: List[ValueLike]): Env[ValueLike] =
          ce match {
            case MakeEnum(variant, arity) =>
              if (arity == 0) Monad[Env].pure(Code.Literal(variant.toString))
              else {
                // we make a tuple with the variant in the first position
                val vExpr = Code.Literal(variant.toString)
                Code.onLasts(vExpr :: args)(Code.MakeTuple(_))
              }
            case MakeStruct(arity) =>
                if (arity == 0) Monad[Env].pure(Code.Const.None)
                else if (arity == 1) Monad[Env].pure(args.head)
                else Code.onLasts(args)(Code.MakeTuple(_))
            case ZeroNat =>
              Monad[Env].pure(Code.Const.Zero)
            case SuccNat =>
              Code.onLast(args.head)(Code.Op(_, Code.Const.Plus, Code.Const.One))
          }

        val sz = args.size
        def makeLam(cnt: Int, args: List[ValueLike]): Env[ValueLike] =
          if (cnt == 0) applyAll(args)
          else if (cnt < 0) {
            // too many args, this shouldn't typecheck
            throw new IllegalStateException(s"invalid arity $sz for $ce")
          }
          else {
            // add an arg to the right
            for {
              v <- Env.newAssignableVar
              body <- makeLam(cnt - 1, args :+ v)
              res <- Code.onLast(body)(Code.Lambda(v :: Nil, _))
            } yield res
          }

        makeLam(ce.arity - sz, args)
      }

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
          case CheckVariant(enumV, idx, size) =>
            loop(enumV).flatMap { tup =>
              Code.onLast(tup) { t =>
                val idxExpr = Code.Literal(idx.toString)
                if (size == 0) {
                  // this is represented as an integer
                  Code.Op(t, Code.Const.Eq, idxExpr)
                }
                else
                  Code.Op(Code.SelectTuple(t, Code.Const.Zero), Code.Const.Eq, idxExpr)
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
          case MatchString(str, pat, binds) => ???
          case SearchList(LocalAnonMut(mutV), init, check, optLeft) => ???
        }

      def makeDef(defName: Code.Ident, arg: Code.Ident, v: ValueLike): Env[Code.Def] =
        Env.newAssignableVar.map { resName =>
          Code.Def(defName, arg :: Nil,
            Code.addAssign(resName, v) :+
              Code.Return(resName)
            )
        }

      def topLet(name: Code.Ident, expr: Expr): Env[Statement] = {

        /*
         * def anonF():
         *   code
         *
         * name = anonF()
         */
        lazy val worstCase: Env[Statement] =
          (Env.newAssignableVar, Env.newAssignableVar, loop(expr)).mapN { (defName, resName, v) =>
            val newDef = Code.Def(defName, Nil,
              Code.addAssign(resName, v) :+
                Code.Return(resName)
              )

            newDef :+ Code.Assign(name, Code.Apply(defName, Nil))
          }

        expr match {
          case l@LoopFn(_, nm, h, t, b) =>
            Env.escape(nm)
              .flatMap { nm1 =>
                if (nm1 == name) {
                  (NonEmptyList(h, t).traverse(Env.escape), loop(b))
                    .mapN(Code.buildLoop(nm1, _, _))
                    .flatten
                }
                else {
                  // we need to reassign the def to name
                  worstCase
                }
              }
          case Lambda(caps, arg, body) =>
            // this isn't recursive, or it would be in a Let
            if (caps.exists(_ == name)) {
              // this is refering to a previous value
              worstCase
            }
            else {
              // no shadow, so we can redefine
              (Env.escape(arg), loop(body))
                .mapN(makeDef(name, _, _))
                .flatten
            }
          case Let(Right((n, RecursionKind.Recursive)), Lambda(_, arg, body), Local(n2)) if n == n2 =>
            (Env.escape(n), Env.escape(arg))
              .mapN { (ni, arg) =>
                if (ni == name) {
                  // this is a recursive value in scope for itself
                  loop(body).flatMap(makeDef(ni, arg, _))
                }
                else {
                  worstCase
                }
              }
              .flatten

          case _ => worstCase
        }
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
          case LoopFn(_, thisName, argshead, argstail, body) =>
            // closures capture the same in python, we can ignore captures
            (Env.escape(thisName), NonEmptyList(argshead, argstail).traverse(Env.escape), loop(body))
              .mapN { (n, args, body) => Code.buildLoop(n, args, body).map(Code.WithValue(_, n)) }
              .flatten
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
            val inF = loop(in)

            localOrBind match {
              case Right((b, rec)) =>
                Env.escape(b)
                  .flatMap { bi =>
                    val vE = rec match {
                      case RecursionKind.NonRecursive =>
                        value

                      case RecursionKind.Recursive =>
                        // push the recursion just into the value
                        Let(localOrBind, value, Local(b))
                    }

                    (topLet(bi, vE), inF)
                      .mapN(Code.WithValue(_, _))
                      .flatMap(Env.endScope(bi, _))
                  }
              case Left(LocalAnon(l)) =>
                // anonymous names never shadow
                Env.nameForAnon(l)
                  .flatMap { bi =>
                    (topLet(bi, value), inF)
                      .mapN(Code.WithValue(_, _))
                  }
            }

          case LetMut(LocalAnonMut(_), in) =>
            // we could delete this name, but
            // there is no need to
            loop(in)
          case Literal(lit) => Monad[Env].pure(litToExpr(lit))
          case If(cond, thenExpr, elseExpr) =>
            def combine(expr: Expr): (List[(BoolExpr, Expr)], Expr) =
              expr match {
                case If(c1, t1, e1) =>
                  val (ifs, e2) = combine(e1)
                  (ifs :+ ((c1, t1)), e1)
                case last => (Nil, last)
              }

            val (rest, last) = combine(elseExpr)
            val ifs = NonEmptyList((cond, thenExpr), rest)

            val ifsV = ifs.traverse { case (c, t) =>
              (boolExpr(c), loop(t)).tupled
            }

            (ifsV, loop(elseExpr))
              .mapN { (ifs, elseV) =>
                Code.ifElse(ifs, elseV)
              }
              .flatten

          case Always(cond, expr) =>
            (boolExpr(cond).map(Code.always), loop(expr))
              .mapN {
                case (Code.Pass, v) => v
                case (notPass, v) =>
                  Code.WithValue(notPass, v)
              }

          case GetEnumElement(expr, _, idx, sz) =>
            // nonempty enums are just structs with the first element being the variant
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
