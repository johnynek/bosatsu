package org.bykn.bosatsu.codegen.clang

import cats.{Monad, Traverse}
import cats.data.NonEmptyList
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.{Matchless, PackageName, RecursionKind}
import org.bykn.bosatsu.Matchless.Expr
import org.bykn.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc

import cats.syntax.all._

object ClangGen {
  sealed abstract class Error
  object Error {
    case class UnknownValue(pack: PackageName, value: Bindable) extends Error
    case class InvariantViolation(message: String, expr: Expr) extends Error
  }

  def renderMain(
      sortedEnv: Vector[NonEmptyList[(PackageName, List[(Bindable, Expr)])]],
      externals: Map[(PackageName, Bindable), (Code.Include, Code.Ident)],
      value: (PackageName, Bindable),
      evaluator: (Code.Include, Code.Ident)
  ): Either[Error, Doc] = {
    val env = Impl.Env.impl
    import env._

    val trav2 = Traverse[Vector].compose[NonEmptyList]

    val res =
      trav2.traverse_(sortedEnv) { case (pn, values) =>
        values.traverse_ { case (bindable, expr) =>
          renderTop(pn, bindable, expr)      
        }
      } *> env.renderMain(value._1, value._2, evaluator._1, evaluator._2)

    val allValues: Impl.AllValues =
      sortedEnv
        .iterator.flatMap(_.iterator)
        .toMap
    
    run(allValues, externals, res)
  }

  private object Impl {
    type AllValues = Map[PackageName, List[(Bindable, Expr)]]
    type Externals = Map[(PackageName, Bindable), (Code.Include, Code.Ident)]

    trait Env {
      import Matchless._

      type T[A]
      implicit val monadImpl: Monad[T]
      def run(pm: AllValues, externals: Externals, t: T[Unit]): Either[Error, Doc]
      def appendStatement(stmt: Code.Statement): T[Unit]
      def error[A](e: Error): T[A]
      def getBinding(bn: Bindable): T[Code.Ident]
      def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident]
      def bind[A](bn: Bindable)(in: T[A]): T[A]
      // a recursive function needs to remap the Bindable to the top-level mangling
      def recursiveName[A](bn: Bindable)(in: T[A]): T[A]
      // used for temporary variables of type BValue
      def newLocalName: T[Code.Ident]
      // record that this name is a top level function, so applying it can be direct
      def declareTopLevelFn(p: Option[PackageName], bn: Bindable, rec: RecursionKind): T[Unit]
      def liftToTop[A](b: Bindable)(in: T[A]): T[A]
      def isLifted(b: Bindable): T[Boolean]
      def staticValueName(p: Option[PackageName], b: Bindable): T[Code.Ident]
      def constructorFn(p: Option[PackageName], b: Bindable): T[Code.Ident]
      def isFn(p: Option[PackageName], b: Bindable): T[Boolean]
      def anonName(idx: Long): T[Code.Ident]
      def anonMutName(idx: Long): T[Code.Ident]
      def withAnon[A](idx: Long)(t: T[A]): T[A]
      def withAnonMut[A](idx: Long)(t: T[A]): T[A]

      /////////////////////////////////////////
      // the below are independent of the environment implementation
      /////////////////////////////////////////
    
      def maybeGlobalIdent(p: Option[PackageName], b: Bindable): T[Code.Ident] =
        p match {
          case Some(value) => globalIdent(value, b)
          case None => getBinding(b)
        }

      val slotsArgName: Code.Ident = Code.Ident("__bsts_slot")

      // assign any results to result and set the condition to false
      // and replace any calls to nm(args) with assigning args to those values
      def toWhileBody(nm: Bindable, args: NonEmptyList[Bindable], cond: Code.Ident, result: Code.Ident, body: Code.ValueLike): T[Code.Block] =
        ???

      def bindAll[A](nel: NonEmptyList[Bindable])(in: T[A]): T[A] =
        bind(nel.head) {
          NonEmptyList.fromList(nel.tail) match {
            case None => in
            case Some(rest) => bindAll(rest)(in)
          }
        }

      object LocalOrGlobal {
        def unapply(expr: Expr): Option[(Option[PackageName], Bindable)] =
          expr match {
            case Global(p, n) => Some((Some(p), n))
            case Local(n) => Some((None, n))
            case _ => None
          }
      }

      def boolToValue(boolExpr: BoolExpr): T[Code.ValueLike] =
        boolExpr match {
          case EqualsLit(expr, lit) => ???
          case EqualsNat(expr, nat) => ???
          case And(e1, e2) => ???
          case CheckVariant(expr, expect, size, famArities) => ???
          case SearchList(lst, init, check, leftAcc) => ???
          case MatchString(arg, parts, binds) => ???
          case SetMut(target, expr) => ???
          case TrueConst => monadImpl.pure(Code.IntLiteral.One)
        }

      def innerToValue(expr: Expr): T[Code.ValueLike] =
        expr match {
          case Lambda(captures, name, args, expr) => ???
          case LoopFn(captures, name, arg, body) => ???
          case Global(pack, name) =>
            isFn(Some(pack), name)
              .flatMap {
                case true =>
                  // global functions are converted to values by tagging the pointer
                  globalIdent(pack, name)
                    .map { nm =>
                      Code.Ident("STATIC_PUREFN")(nm): Code.ValueLike 
                    }
                case false =>
                  // read_or_build(&__bvalue_foo, make_foo);
                  for {
                    value <- staticValueName(Some(pack), name)
                    consFn <- constructorFn(Some(pack), name)
                  } yield Code.Ident("read_or_build")(value.addr, consFn): Code.ValueLike
              }
          case Local(arg) =>
            isLifted(arg)
              .flatMap {
                case false => getBinding(arg).widen
                case true =>
                  for {
                    value <- staticValueName(None, arg)
                    consFn <- constructorFn(None, arg)
                  } yield Code.Ident("read_or_build")(value.addr, consFn): Code.ValueLike
              }
          case ClosureSlot(idx) =>
            // we must be inside a closure function, so we should have a slots argument to access
            monadImpl.pure(slotsArgName.bracket(Code.IntLiteral(BigInt(idx))))
          // Anons are never lifted
          case LocalAnon(ident) => anonName(ident).widen
          case LocalAnonMut(ident) => anonMutName(ident).widen
          case App(LocalOrGlobal(optPack, fnName), args) =>
            isFn(optPack, fnName).flatMap {
              case true =>
                // just directly invoke
                (maybeGlobalIdent(optPack, fnName), args.traverse(innerToValue(_))).mapN { (ident, argsVL) =>
                  Code.ValueLike.applyArgs(ident, argsVL)
                }
              case false =>
                // the ref be holding the result of another function call
                (maybeGlobalIdent(optPack, fnName), args.traverse(innerToValue(_))).mapN { (fnVL, argsVL) =>
                  // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                  // but since these are ValueLike, we need to handle more carefully
                  val fnSize = argsVL.length
                  val callFn = Code.Ident(s"call_fn$fnSize")
                  Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)  
                }
            }
          case App(fn, args) =>
            (innerToValue(fn), args.traverse(innerToValue(_))).mapN { (fnVL, argsVL) =>
              // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
              // but since these are ValueLike, we need to handle more carefully
              val fnSize = argsVL.length
              val callFn = Code.Ident(s"call_fn$fnSize")
              Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)  
            }
          case Let(arg, expr, in) => ???
          case LetMut(name, span) => ???
          case Literal(lit) => ???
          case If(cond, thenExpr, elseExpr) => ???
          case Always(cond, thenExpr) => ???
          case GetEnumElement(arg, variant, index, size) => ???
          case GetStructElement(arg, index, size) => ???
          case MakeEnum(variant, arity, famArities) => ???
          case MakeStruct(arity) => ???
          case ZeroNat => ???
          case SuccNat => ???
          case PrevNat(of) => ???
        }

      def topFn(fnName: Code.Ident, fn: FnExpr): T[Code.Statement] =
         fn match {
          case Lambda(_ :: _, _, _, _) | LoopFn(_ :: _, _, _, _) =>
            // TODO: handle this by adding the slots parameter first
            error(Error.InvariantViolation(s"unexpected top level captures", fn))
          case Lambda(Nil, name, args, expr) =>
            val body = bindAll(args) { innerToValue(expr).map(Code.returnValue(_)) }
            val body1 = name match {
              case None => body
              case Some(rec) => recursiveName(rec)(body)
            }

            for {
              argParams <- args.traverse { b =>
                getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
              }
              fnBody <- body1
            } yield Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, argParams.toList, Some(Code.block(fnBody)))
          case LoopFn(Nil, nm, args, body) =>
            recursiveName(nm) {
              bindAll(args) {
                for {
                  cond <- newLocalName
                  res <- newLocalName
                  bodyVL <- innerToValue(body)
                  whileBody <- toWhileBody(nm, args, cond = cond, result = res, body = bodyVL)
                  argParams <- args.traverse { b =>
                    getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
                  }
                  fnBody = Code.block(
                    Code.DeclareVar(Nil, Code.TypeIdent.Int, cond, Some(Code.IntLiteral.One)),
                    Code.DeclareVar(Nil, Code.TypeIdent.BValue, res, None),
                    Code.While(cond, whileBody),
                    Code.Return(Some(res))
                  )
                } yield Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, argParams.toList, Some(fnBody))
              }
            }
        }

      def renderTop(p: PackageName, b: Bindable, expr: Expr): T[Unit] =
        renderTopInner(Some(p), b, expr)

      private def renderTopInner(p: Option[PackageName], b: Bindable, expr: Expr): T[Unit] =
        expr match {
          case fn: FnExpr =>
            for {
              _ <- declareTopLevelFn(p, b, fn.recursionKind)
              fnName <- maybeGlobalIdent(p, b)
              stmt <- topFn(fnName, fn)
              _ <- appendStatement(stmt)
            } yield ()
          case Let(Right((n1, RecursionKind.NonRecursive)), inner, Local(n2)) if n1 === n2 =>
            // let x = y in x == y (this probably never comes up, since it should be optimized away be here)
            renderTopInner(p, b, inner)
          case Let(Right((n1, RecursionKind.NonRecursive)), inner, result) =>
            liftToTop(n1) {
              // TODO: now that we have lifted n1, we may have created a closure
              // at the top level... this seems too complex to do at this stage
              // and we should push this optimization into Matchless or TypedExpr Normalization
              // where it can be tested on all the backends.
              renderTopInner(None, n1, inner) *> renderTopInner(p, b, result)
            }
          case Let(Right((n1, RecursionKind.Recursive)), fn: FnExpr, Local(n2)) if (n1 === b) && (n2 === b) =>
            // this is a pattern used by matchless emitting recursive top level functions
            // see the letrec function
            for {
              _ <- declareTopLevelFn(p, b, RecursionKind.Recursive)
              fnName <- maybeGlobalIdent(p, b)
              stmt <- topFn(fnName, fn)
              _ <- appendStatement(stmt)
            } yield ()
          case someValue =>
            // we materialize an Atomic value to hold the static data
            // then we generate a function to populate the value
            for {
              vl <- innerToValue(someValue)
              value <- staticValueName(p, b)
              consFn <- constructorFn(p, b)
              _ <- appendStatement(Code.DeclareVar(
                Code.Attr.Static :: Nil,
                Code.TypeIdent.AtomicBValue,
                value,
                Some(Code.IntLiteral.Zero)
              ))
              _ <- appendStatement(Code.DeclareFn(
                Code.Attr.Static :: Nil,
                Code.TypeIdent.AtomicBValue,
                consFn,
                Nil,
                Some(Code.block(Code.returnValue(vl)))
              ))
            } yield ()
        }

      def renderMain(p: PackageName, b: Bindable, evalInc: Code.Include, evalFn: Code.Ident): T[Unit]
    }

    object Env {
      def impl: Env = ???
    }
  }
}