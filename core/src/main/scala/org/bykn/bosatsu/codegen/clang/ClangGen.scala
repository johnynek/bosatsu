package org.bykn.bosatsu.codegen.clang

import cats.{Monad, Traverse}
import cats.data.NonEmptyList
import java.math.BigInteger
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.rankn.DataRepr
import org.bykn.bosatsu.{Identifier, Lit, Matchless, PackageName, RecursionKind}
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
      def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident]
      def bind[A](bn: Bindable)(in: T[A]): T[A]
      def getBinding(bn: Bindable): T[Code.Ident]
      def bindAnon[A](idx: Long)(in: T[A]): T[A]
      def getAnon(idx: Long): T[Code.Ident]
      // a recursive function needs to remap the Bindable to the top-level mangling
      def recursiveName[A](fnName: Code.Ident, bn: Bindable)(in: T[A]): T[A]
      // used for temporary variables of type BValue
      def newLocalName(tag: String): T[Code.Ident]
      def newTopName(tag: String): T[Code.Ident]
      // record that this name is a top level function, so applying it can be direct
      def declareTopLevelFn(p: PackageName, bn: Bindable, rec: RecursionKind): T[Unit]
      // when we directly call a function, we want to be able to directly invoke
      def scopedLocalFn[A](bn: Bindable)(ta: T[A]): T[A]
      def directFn(p: Option[PackageName], b: Bindable): T[Option[Code.Ident]]
      def inTop[A](p: PackageName, bn: Bindable)(ta: T[A]): T[A]
      def staticValueName(p: PackageName, b: Bindable): T[Code.Ident]
      def constructorFn(p: PackageName, b: Bindable): T[Code.Ident]
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

      // This name has to be impossible to give out for any other purpose
      val slotsArgName: Code.Ident = Code.Ident("__bstsi_slot")

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

      def equalsString(expr: Code.Expression, str: String): Code.Expression = ???
      def equalsInt(expr: Code.Expression, i: BigInteger): Code.Expression = ???
      def equalsChar(expr: Code.Expression, codePoint: Int): Code.Expression = ???

      def boolToValue(boolExpr: BoolExpr): T[Code.ValueLike] =
        boolExpr match {
          case EqualsLit(expr, lit) =>
            innerToValue(expr).flatMap { vl =>
              lit match {
                case Lit.Str(str) => vl.onExpr { e => monadImpl.pure[Code.ValueLike](equalsString(e, str)) }(newLocalName)
                case c @ Lit.Chr(_) => vl.onExpr { e => monadImpl.pure[Code.ValueLike](equalsChar(e, c.toCodePoint)) }(newLocalName)
                case Lit.Integer(i) => vl.onExpr { e => monadImpl.pure[Code.ValueLike](equalsInt(e, i)) }(newLocalName)
              }  
            }
          case EqualsNat(expr, nat) =>
            val fn = nat match {
              case DataRepr.ZeroNat => Code.Ident("BSTS_NAT_IS_0")
              case DataRepr.SuccNat => Code.Ident("BSTS_NAT_GT_0")
            }
            innerToValue(expr).flatMap { vl =>
              vl.onExpr { expr => monadImpl.pure[Code.ValueLike](fn(expr)) }(newLocalName)  
            }
          case And(e1, e2) =>
            (boolToValue(e1), boolToValue(e2))
              .mapN { (a, b) =>
                Code.ValueLike.applyArgs(Code.Ident("BSTS_AND"), NonEmptyList(a, b :: Nil))  
              }
          case CheckVariant(expr, expect, size, famArities) => ???
          case SearchList(lst, init, check, leftAcc) => ???
          case MatchString(arg, parts, binds) => ???
          case SetMut(LocalAnonMut(idx), expr) =>
            for {
              name <- getAnon(idx)
              vl <- innerToValue(expr)
            } yield (name := vl) +: Code.TrueLit
          case TrueConst => monadImpl.pure(Code.TrueLit)
        }

      // We have to lift functions to the top level and not
      // create any nesting
      def innerFn(fn: FnExpr): T[Code.ValueLike] = 
        if (fn.captures.isEmpty) {
          for {
            ident <- newTopName("lambda")
            stmt <- topFn(ident, fn)
            _ <- appendStatement(stmt)
          } yield Code.Ident("STATIC_PUREFN")(ident)
        }
        else {
          // we create the function, then we allocate
          // values for the capture
          // alloc_closure<n>(capLen, captures, fnName)
          for {
            ident <- newTopName("closure")
            stmt <- topFn(ident, fn)
            _ <- appendStatement(stmt)
            capName <- newLocalName("captures")
            capValues <- fn.captures.traverse(innerToValue(_))
            decl <- Code.ValueLike.declareArray(capName, Code.TypeIdent.BValue, capValues)(newLocalName)
          } yield Code.WithValue(decl,
            Code.Ident(s"alloc_closure${fn.arity}")(
              Code.IntLiteral(BigInt(fn.captures.length)),
              capName.addr,
              ident
            )
          )
        }

      def literal(lit: Lit): Code.Expression = ???

      def innerApp(app: App): T[Code.ValueLike] =
        app match {
          case App(LocalOrGlobal(optPack, fnName), args) =>
            directFn(optPack, fnName).flatMap {
              case Some(ident) =>
                // directly invoke instead of by treating them like lambdas
                args.traverse(innerToValue(_)).map { argsVL =>
                  Code.ValueLike.applyArgs(ident, argsVL)
                }
              case None =>
                // the ref be holding the result of another function call
                (maybeGlobalIdent(optPack, fnName), args.traverse(innerToValue(_))).mapN { (fnVL, argsVL) =>
                  // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                  // but since these are ValueLike, we need to handle more carefully
                  val fnSize = argsVL.length
                  val callFn = Code.Ident(s"call_fn$fnSize")
                  Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)  
                }
            }
          case App(MakeEnum(variant, arity, _), args) =>
            // to type check, we know that the arity must have the same length as args
            args.traverse(innerToValue).map { argsVL =>
              val tag = Code.IntLiteral(variant)
              Code.ValueLike.applyArgs(Code.Ident(s"alloc_enum$arity"), tag :: argsVL)  
            }
          case App(MakeStruct(arity), args) =>
            if (arity == 1) {
              // this is a new-type, just return the arg
              innerToValue(args.head)
            }
            else {
              // to type check, we know that the arity must have the same length as args
              args.traverse(innerToValue).map { argsVL =>
                Code.ValueLike.applyArgs(Code.Ident(s"alloc_struct$arity"), argsVL)  
              }
          }
          case App(SuccNat, args) =>
            innerToValue(args.head).map { arg =>
              Code.ValueLike.applyArgs(Code.Ident("BSTS_NAT_SUCC"), NonEmptyList.one(arg))
            }
          case App(fn, args) =>
            (innerToValue(fn), args.traverse(innerToValue(_))).mapN { (fnVL, argsVL) =>
              // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
              // but since these are ValueLike, we need to handle more carefully
              val fnSize = argsVL.length
              val callFn = Code.Ident(s"call_fn$fnSize")
              Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)  
            }
          }

      def innerToValue(expr: Expr): T[Code.ValueLike] =
        expr match {
          case fn: FnExpr => innerFn(fn)
          case Let(Right(arg), argV, in) =>
            bind(arg) {
              for {
                name <- getBinding(arg)
                v <- innerToValue(argV)
                result <- innerToValue(in)
                stmt <- Code.ValueLike.declareVar(name, Code.TypeIdent.BValue, v)(newLocalName)
              } yield stmt +: result
            }
          case Let(Left(LocalAnon(idx)), argV, in) =>
            bindAnon(idx) {
              for {
                name <- getAnon(idx)
                v <- innerToValue(argV)
                result <- innerToValue(in)
                stmt <- Code.ValueLike.declareVar(name, Code.TypeIdent.BValue, v)(newLocalName)
              } yield stmt +: result
            }
          case app @ App(_, _) => innerApp(app)
          case Global(pack, name) =>
            directFn(Some(pack), name)
              .flatMap {
                case Some(nm) =>
                  monadImpl.pure(Code.Ident("STATIC_PUREFN")(nm): Code.ValueLike)
                case None =>
                  // read_or_build(&__bvalue_foo, make_foo);
                  for {
                    value <- staticValueName(pack, name)
                    consFn <- constructorFn(pack, name)
                  } yield Code.Ident("read_or_build")(value.addr, consFn): Code.ValueLike
              }
          case Local(arg) =>
            directFn(None, arg)
              .flatMap {
                case Some(nm) =>
                  monadImpl.pure(Code.Ident("STATIC_PUREFN")(nm): Code.ValueLike)
                case None =>
                  getBinding(arg).widen
              }
          case ClosureSlot(idx) =>
            // we must be inside a closure function, so we should have a slots argument to access
            monadImpl.pure(slotsArgName.bracket(Code.IntLiteral(BigInt(idx))))
          case LocalAnon(ident) => anonName(ident).widen
          case LocalAnonMut(ident) => anonMutName(ident).widen
          case LetMut(LocalAnonMut(m), span) =>
            bindAnon(m) {
              for {
                ident <- getAnon(m)
                decl = Code.DeclareVar(Nil, Code.TypeIdent.BValue, ident, None)
                rest <- innerToValue(span)
              } yield decl +: rest
            }
          case Literal(lit) => monadImpl.pure(literal(lit))
          case If(cond, thenExpr, elseExpr) =>
            (boolToValue(cond), innerToValue(thenExpr), innerToValue(elseExpr))
              .flatMapN { (c, thenC, elseC) =>
                Code.ValueLike.ifThenElseV(c, thenC, elseC)(newLocalName)
              }
          case Always(cond, thenExpr) =>
            boolToValue(cond).flatMap { bv =>
              bv.discardValue match {
                case None => innerToValue(thenExpr)
                case Some(effect) => innerToValue(thenExpr).map(effect +: _)
              }
            }
          case GetEnumElement(arg, _, index, _) =>
            // call get_enum_index(v, index)
            innerToValue(arg).flatMap { v =>
              v.onExpr(e => monadImpl.pure[Code.ValueLike](Code.Ident("get_enum_index")(e, Code.IntLiteral(index))))(newLocalName)
            }
          case GetStructElement(arg, index, size) =>
            if (size == 1) {
              // this is just a new-type wrapper, ignore it
              innerToValue(arg)
            }
            else {
              // call get_struct_index(v, index)
              innerToValue(arg).flatMap { v =>
                v.onExpr { e =>
                  monadImpl.pure[Code.ValueLike](Code.Ident("get_struct_index")(e, Code.IntLiteral(index)))
                }(newLocalName)
              }
            }
          case makeEnum @ MakeEnum(variant, arity, _) =>
            // this is a closure over variant, we rewrite this
            NonEmptyList.fromList((0 until arity).toList) match {
              case None => monadImpl.pure(Code.Ident("alloc_enum0")(Code.IntLiteral(variant)))
              case Some(args) =>
                val named = args.map { idx => Identifier.Name(s"arg$idx") }
                // This relies on optimizing App(MakeEnum, _) otherwise
                // it creates an infinite loop.
                // Also, this we should cache creation of Lambda/Closure values
                innerToValue(Lambda(Nil, None, named, App(makeEnum, named.map(Local(_)))))
            }
          case MakeStruct(arity) =>
            monadImpl.pure {
              if (arity == 0) Code.Ident("PURE_VALUE_TAG")
              else {
                val allocStructFn = s"alloc_struct$arity"
                Code.Ident("STATIC_PUREFN")(Code.Ident(allocStructFn))
              }
            }
          case ZeroNat =>
            monadImpl.pure(Code.Ident("BSTS_NAT_0"))
          case SuccNat =>
            val arg = Identifier.Name("arg0")
            // This relies on optimizing App(SuccNat, _) otherwise
            // it creates an infinite loop.
            // Also, this we should cache creation of Lambda/Closure values
            innerToValue(Lambda(Nil, None, NonEmptyList.one(arg),
              App(SuccNat, NonEmptyList.one(Local(arg)))))
          case PrevNat(of) =>
            innerToValue(of).map { argVL =>
              Code.ValueLike.applyArgs(Code.Ident("BSTS_NAT_PREV"), NonEmptyList.one(argVL))  
            }
        }

      def topFn(fnName: Code.Ident, fn: FnExpr): T[Code.Statement] =
         fn match {
          case Lambda(captures, name, args, expr) =>
            val body = bindAll(args) { innerToValue(expr).map(Code.returnValue(_)) }
            val body1 = name match {
              case None => body
              case Some(rec) => recursiveName(fnName, rec)(body)
            }

            for {
              argParams <- args.traverse { b =>
                getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
              }
              fnBody <- body1
              allArgs =
                if (captures.isEmpty) argParams
                else {
                  Code.Param(Code.TypeIdent.BValue.ptr, slotsArgName) :: argParams
                }
            } yield Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, allArgs.toList, Some(Code.block(fnBody)))
          case LoopFn(captures, nm, args, body) =>
            recursiveName(fnName, nm) {
              bindAll(args) {
                for {
                  cond <- newLocalName("cond")
                  res <- newLocalName("res")
                  bodyVL <- innerToValue(body)
                  whileBody <- toWhileBody(nm, args, cond = cond, result = res, body = bodyVL)
                  argParams <- args.traverse { b =>
                    getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
                  }
                  fnBody = Code.block(
                    Code.DeclareVar(Nil, Code.TypeIdent.Bool, cond, Some(Code.TrueLit)),
                    Code.DeclareVar(Nil, Code.TypeIdent.BValue, res, None),
                    Code.While(cond, whileBody),
                    Code.Return(Some(res))
                  )
                  allArgs =
                    if (captures.isEmpty) argParams
                    else {
                      Code.Param(Code.TypeIdent.BValue.ptr, slotsArgName) :: argParams
                    }
                } yield Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, allArgs.toList, Some(fnBody))
              }
            }
        }

      def renderTop(p: PackageName, b: Bindable, expr: Expr): T[Unit] =
        inTop(p, b) { expr match {
          case fn: FnExpr =>
            for {
              _ <- declareTopLevelFn(p, b, fn.recursionKind)
              fnName <- globalIdent(p, b)
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
                Code.TypeIdent.BValue,
                consFn,
                Nil,
                Some(Code.block(Code.returnValue(vl)))
              ))
            } yield ()
        }
      }

      def renderMain(p: PackageName, b: Bindable, evalInc: Code.Include, evalFn: Code.Ident): T[Unit]
    }

    object Env {
      def impl: Env = ???
    }
  }
}