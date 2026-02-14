package dev.bosatsu.codegen.clang

import cats.{Eval, Functor, Monad, Show, Traverse}
import cats.data.{StateT, EitherT, NonEmptyList, Chain}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import dev.bosatsu.codegen.{CompilationNamespace, CompilationSource, Idents}
import dev.bosatsu.rankn.{DataRepr, Type}
import dev.bosatsu.{Identifier, Lit, Matchless, Predef, PackageName}
import dev.bosatsu.Matchless.Expr
import dev.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc
import scala.collection.mutable
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.annotation.nowarn

import cats.syntax.all._

import ClangGen.Error

class ClangGen[K](ns: CompilationNamespace[K]) {
  given Show[K] = ns.keyShow
  // (function ident, isClosure, arity)
  type DirectFnRef = (Code.Ident, Boolean, Int)
  def generateExternalsStub: SortedMap[String, Doc] =
    ExternalResolver.stdExternals.generateExternalsStub

  trait ExternalResolver {
    def names: Iterable[(K, PackageName, SortedSet[Bindable])]
    def apply(
        key: K,
        p: PackageName,
        b: Bindable
    ): Option[(Code.Include, Code.Ident, Int)]

    final def generateExternalsStub: SortedMap[String, Doc] = {
      val includes = Code.Include.quote("bosatsu_runtime.h") :: Nil

      def toStmt(cIdent: Code.Ident, arity: Int): Code.Statement = {
        val args = Idents.allSimpleIdents.take(arity).map { nm =>
          Code.Param(Code.TypeIdent.BValue, Code.Ident(nm))
        }
        Code.DeclareFn(Nil, Code.TypeIdent.BValue, cIdent, args.toList, None)
      }

      val line2 = Doc.hardLine + Doc.hardLine
      val includeRuntime =
        Doc.intercalate(Doc.hardLine, includes.map(Code.toDoc))

      SortedMap.empty[String, Doc] ++ names.iterator
        .flatMap { case (k, p, binds) =>

          val fns = binds.iterator
            .flatMap { n =>
              apply(k, p, n)
            }
            .map { case (i, n, arity) =>
              i.filename -> Code.toDoc(toStmt(n, arity))
            }
            .toList
            .groupByNel(_._1)

          fns.iterator.map { case (incl, nelInner) =>
            incl -> (includeRuntime +
              line2 +
              Doc.intercalate(line2, nelInner.toList.map(_._2)))
          }
        }
    }

  }

  object ExternalResolver {
    def stdExtFileName(pn: PackageName): String =
      s"${Idents.escape("bosatsu_ext_", ns.identOf(ns.rootKey, pn).mkString_("/"))}.h"

    def stdExternals: ExternalResolver = {

      val keyedExt = ns.externals
      val extMap = keyedExt.iterator.flatMap { case (k, allExt) =>
        allExt.iterator.map { case (p, vs) =>
          val fileName = ExternalResolver.stdExtFileName(p)

          val fns = vs.iterator.map { case (n, tpe) =>
            val cIdent = generatedName(ns.rootKey, p, n)
            n -> (cIdent, Type.Fun.arity(tpe))
          }.toMap

          (k, p) -> (Code.Include.quote(fileName), fns)
        }
      }.toMap

      new ExternalResolver {
        lazy val names: Iterable[(K, PackageName, SortedSet[Bindable])] =
          (for {
            kPmap <- keyedExt.iterator
            (k, pmap) = kPmap
            packBinds <- pmap.iterator
            (pack, binds) = packBinds
            bindSet = binds.iterator.map(_._1).to(SortedSet)
          } yield (k, pack, bindSet)).to(LazyList)

        def apply(
            k: K,
            p: PackageName,
            b: Bindable
        ): Option[(Code.Include, Code.Ident, Int)] =
          for {
            (include, inner) <- extMap.get((k, p))
            (ident, arity) <- inner.get(b)
          } yield (include, ident, arity)
      }
    }

    val FromJvmExternals: ExternalResolver =
      new ExternalResolver {
        def external(
            p: PackageName,
            s: String,
            arity: Int
        ) =
          (p -> Identifier.Name(s)) -> (
            Code.Include.quote(stdExtFileName(p)),
            generatedName(
              ns.rootKey,
              p,
              Identifier.Name(s)
            ),
            arity
          )

        val ext = Predef.jvmExternals.toMap.iterator
          .map { case ((p, n), ffi) =>
            external(p, n, ffi.arity)
          }
          .toMap[(PackageName, Identifier), (Code.Include, Code.Ident, Int)]

        lazy val names: Iterable[(K, PackageName, SortedSet[Bindable])] = {
          val sm = Predef.jvmExternals.toMap.iterator
            .map { case (pn, _) => pn }
            .toList
            .groupByNel(_._1)

          sm.map { case (k, vs) =>
            (
              ns.rootKey,
              k,
              vs.toList.iterator
                .map { case (_, n) => (Identifier.Name(n): Bindable) }
                .to(SortedSet)
            )
          }
        }
        def apply(k: K, p: PackageName, b: Bindable) = ext.get((p, b))
      }
  }

  val sortedEnv
      : Vector[NonEmptyList[(K, PackageName, List[(Bindable, Expr[K])])]] =
    Functor[Vector]
      .compose[NonEmptyList]
      .map(ns.topoSort.layers) { case (k, p) =>
        val exprs = for {
          pms <- ns.compiled.get(k).toList
          exprs <- pms.get(p).toList
          (b, expr) <- exprs
        } yield (b, Matchless.recoverTopLevelLambda(expr))

        (k, p, exprs)
      }

  private val allValues: Impl.AllValues =
    sortedEnv.iterator
      .flatMap(_.iterator)
      .flatMap { case (k, p, vs) =>
        vs.iterator.map { case (b, e) =>
          (k, p, b) -> (e, generatedName(k, p, b))
        }
      }
      .toMap

  private val env = Impl.Env.impl
  private val deps = {
    import env._
    Traverse[Vector]
      .compose[NonEmptyList]
      .traverse_(sortedEnv) { case (k, pn, values) =>
        values.traverse_ { case (bindable, expr) =>
          renderTop(k, pn, bindable, expr)
        }
      }
  }

  private def render(res: env.T[Unit]): Either[Error, Doc] = {
    import env.monadImpl
    env.run(allValues, ExternalResolver.stdExternals, deps *> res)
  }

  // the Code.Ident should be a function with signature:
  // int run_main(BValue main_value, int argc, char** args)
  def renderMain(
      pn: PackageName,
      value: Bindable,
      runner: Code.Ident
  ): Either[Error, Doc] =
    render(env.renderMain(pn, value, runner))

  def renderTests(
      values: List[(PackageName, Bindable)]
  ): Either[Error, Doc] =
    render(env.renderTests(values))

  private def fullName(k: K, p: PackageName, b: Bindable): String =
    ns.identOf(k, p).mkString_("/") + "/" + b.asString

  def generatedName(k: K, p: PackageName, b: Bindable): Code.Ident =
    Code.Ident(Idents.escape("___bsts_g_", fullName(k, p, b)))

  private object Impl {
    type AllValues = Map[(K, PackageName, Bindable), (Expr[K], Code.Ident)]

    trait Env {
      import Matchless._

      type T[A]
      implicit val monadImpl: Monad[T]
      def run(
          pm: AllValues,
          externals: ExternalResolver,
          t: T[Unit]
      ): Either[Error, Doc]
      def appendStatement(stmt: Code.Statement): T[Unit]
      def error[A](e: => Error): T[A]
      def globalIdent(k: K, pn: PackageName, bn: Bindable): T[Code.Ident]
      def bind[A](
          bn: Bindable,
          directFn: Option[DirectFnRef]
      )(in: T[A]): T[A]
      def getBinding(bn: Bindable): T[Code.Ident]
      def bindAnon[A](idx: Long)(in: T[A]): T[A]
      def getAnon(idx: Long): T[Code.Ident]
      // a recursive function needs to remap the Bindable to the top-level mangling
      def recursiveName[A](
          fnName: Code.Ident,
          bn: Bindable,
          isClosure: Boolean,
          arity: Int
      )(in: T[A]): T[A]
      // used for temporary variables of type BValue
      def newLocalName(tag: String): T[Code.Ident]
      def newTopName(tag: String): T[Code.Ident]
      def directFn(
          k: K,
          p: PackageName,
          b: Bindable
      ): T[Option[(Code.Ident, Int)]]
      def directFn(b: Bindable): T[Option[DirectFnRef]]
      def inTop[A](k: K, p: PackageName, bn: Bindable)(ta: T[A]): T[A]
      def inFnStatement[A](in: T[A]): T[A]
      def currentTop: T[Option[(K, PackageName, Bindable)]]
      def staticValueName(k: K, p: PackageName, b: Bindable): T[Code.Ident]
      def constructorFn(k: K, p: PackageName, b: Bindable): T[Code.Ident]

      def cachedIdent(key: Expr[K])(value: => T[Code.Ident]): T[Code.Ident]

      /////////////////////////////////////////
      // the below are independent of the environment implementation
      /////////////////////////////////////////

      // This name has to be impossible to give out for any other purpose
      val slotsArgName: Code.Ident = Code.Ident("__bstsi_slot")

      def bindAll[A](nel: NonEmptyList[Bindable])(in: T[A]): T[A] =
        bind(nel.head, directFn = None) {
          NonEmptyList.fromList(nel.tail) match {
            case None       => in
            case Some(rest) => bindAll(rest)(in)
          }
        }

      def equalsChar(expr: Code.Expression, codePoint: Int): Code.Expression =
        Code.Ident("bsts_char_code_point_from_value")(expr) =:= Code.IntLiteral(
          codePoint
        )

      def pv(e: Code.ValueLike): T[Code.ValueLike] = monadImpl.pure(e)

      def andCode(l: Code.ValueLike, r: Code.ValueLike): T[Code.ValueLike] =
        l match {
          case Code.IntLiteral(lit) =>
            if (lit != 0) pv(r)
            else pv(l)
          case le: Code.Expression =>
            r match {
              case re: Code.Expression =>
                // todo: we can generate more efficient code by evaluating this when possible
                // that said, the compiler will definitely handle this too
                pv(Code.evalAnd(le, re))
              case _ =>
                pv(le.evalToInt match {
                  case None =>
                    // we can only run the statements in r if le is true,
                    // since boolean expressions are where the side effects are.
                    //
                    // and(x, y) == if x: y else: False
                    Code.IfElseValue(le, r, Code.FalseLit)
                  case Some(Code.IntLiteral(i)) =>
                    if (i == 0) Code.FalseLit
                    else r
                })
            }
          case Code.WithValue(sl, sv) => andCode(sv, r).map(sl +: _)
          case ife @ Code.IfElseValue(c, t, f)
              if ife.returnsBool || r.isInstanceOf[Code.Expression] =>
            // push down into the lhs since this won't increase the final branch count
            (andCode(t, r), andCode(f, r)).mapN { (t1, r1) =>
              Code.IfElseValue(c, t1, r1)
            }
          case ife @ Code.IfElseValue(_, _, _) =>
            for {
              resIdent <- newLocalName("branch_res")
              value <- andCode(resIdent, r)
            } yield (
              // Assign branchCond to a temp variable in both branches
              // and then use it so we don't exponentially blow up the code
              // size
              // TODO: not all onExpr uses have this type, we need to take type argument
              (Code.DeclareVar(Nil, Code.TypeIdent.Bool, resIdent, None) +
                (resIdent := ife)) +: value
            )
        }

      def handleLet(
          name: Either[LocalAnon, Bindable],
          argV: Expr[K],
          in: T[Code.ValueLike]
      ): T[Code.ValueLike] =
        name match {
          case Right(arg) =>
            argV match {
              case fn: Lambda[K] if fn.captures.isEmpty =>
                // Non-closure lambdas can be called directly by lifted static name.
                for {
                  fnName <- liftedFnName(fn)
                  result <- bind(arg, Some((fnName, false, fn.arity)))(in)
                } yield result
              case _ =>
                // arg isn't in scope for argV
                innerToValue(argV).flatMap { v =>
                  bind(arg, directFn = None) {
                    for {
                      name <- getBinding(arg)
                      result <- in
                      stmt <- Code.ValueLike.declareVar(
                        Code.TypeIdent.BValue,
                        name,
                        v
                      )(newLocalName)
                    } yield stmt +: result
                  }
                }
            }
          case Left(LocalAnon(idx)) =>
            // LocalAnon(idx) isn't in scope for argV
            innerToValue(argV)
              .flatMap { v =>
                bindAnon(idx) {
                  for {
                    name <- getAnon(idx)
                    result <- in
                    stmt <- Code.ValueLike.declareVar(
                      Code.TypeIdent.BValue,
                      name,
                      v
                    )(newLocalName)
                  } yield stmt +: result
                }
              }
        }
      // The type of this value must be a C _Bool
      def boolToValue(boolExpr: BoolExpr[K]): T[Code.ValueLike] =
        boolExpr match {
          case EqualsLit(expr, lit) =>
            innerToValue(expr).flatMap { vl =>
              lit match {
                case c @ Lit.Chr(_) =>
                  vl.onExpr(e => pv(equalsChar(e, c.toCodePoint)))(newLocalName)
                case Lit.Str(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(
                        Code.Ident("bsts_string_equals"),
                        NonEmptyList(e, litStr :: Nil)
                      )(newLocalName)
                    }
                  }(newLocalName)
                case Lit.Integer(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(
                        Code.Ident("bsts_integer_equals"),
                        NonEmptyList(e, litStr :: Nil)
                      )(newLocalName)
                    }
                  }(newLocalName)
                case _: Lit.Float64 =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litFloat =>
                      Code.ValueLike.applyArgs(
                        Code.Ident("bsts_float64_equals"),
                        NonEmptyList(e, litFloat :: Nil)
                      )(newLocalName)
                    }
                  }(newLocalName)
              }
            }
          case EqualsNat(expr, nat) =>
            val fn = nat match {
              case DataRepr.ZeroNat => Code.Ident("BSTS_NAT_IS_0")
              case DataRepr.SuccNat => Code.Ident("BSTS_NAT_GT_0")
            }
            innerToValue(expr).flatMap { vl =>
              vl.onExpr(expr => pv(fn(expr)))(newLocalName)
            }
          case And(e1, e2) =>
            (boolToValue(e1), boolToValue(e2))
              .flatMapN((a, b) => andCode(a, b))

          case CheckVariant(expr, expect, _, famArities) =>
            innerToValue(expr).flatMap { vl =>
              val fn =
                if (famArities.forall(_ == 0)) "get_variant_value"
                else "get_variant"
              vl.onExpr { expr =>
                pv(Code.Ident(fn)(expr) =:= Code.IntLiteral(expect))
              }(newLocalName)
            }
          case SetMut(LocalAnonMut(idx), expr) =>
            for {
              name <- getAnon(idx)
              vl <- innerToValue(expr)
            } yield (name := vl) +: Code.TrueLit
          case TrueConst               => pv(Code.TrueLit)
          case LetBool(name, argV, in) =>
            handleLet(name, argV, boolToValue(in))
          case LetMutBool(LocalAnonMut(m), span) =>
            bindAnon(m) {
              for {
                ident <- getAnon(m)
                decl = Code.DeclareVar(Nil, Code.TypeIdent.BValue, ident, None)
                res <- boolToValue(span)
              } yield decl +: res
            }
        }

      object StringApi {
        def fromString(s: String): T[Code.ValueLike] = {
          // convert to utf8 and then to a literal array of bytes
          val bytes = s.getBytes(StandardCharsets.UTF_8)
          if (bytes.forall(_.toInt != 0)) {
            // just send the utf8 bytes as a string to C
            pv(
              Code.Ident("bsts_string_from_utf8_bytes_static")(
                Code.IntLiteral(bytes.length),
                Code.StrLiteral(s)
              )
            )
          } else {
            // We have some null bytes, we have to encode the length
            val lits =
              bytes.iterator.map { byte =>
                Code.IntLiteral(byte.toInt & 0xff)
              }.toList
            // call:
            // bsts_string_from_utf8_bytes_copy(size_t size, char* bytes);
            newLocalName("str").map { ident =>
              // TODO: this could be a static top level definition to initialize
              // one time and avoid the copy probably, but copies are fast....
              Code.DeclareArray(Code.TypeIdent.Char, ident, Right(lits)) +:
                Code.Ident("bsts_string_from_utf8_bytes_copy")(
                  Code.IntLiteral(lits.length),
                  ident
                )
            }
          }
        }

      }

      def boxFn(ident: Code.Ident, arity: Int): Code.Expression =
        Code.Ident(s"alloc_boxed_pure_fn$arity")(ident)

      def liftedFnName(fn: Lambda[K]): T[Code.Ident] = {
        val nameSuffix = fn.recursiveName match {
          case None    => ""
          case Some(n) => Idents.escape("_", n.asString)
        }
        val prefix = if (fn.captures.isEmpty) "lambda" else "closure"
        cachedIdent(fn) {
          for {
            ident <- newTopName(prefix + nameSuffix)
            stmt <- fnStatement(ident, fn)
            _ <- appendStatement(stmt)
          } yield ident
        }
      }

      // We have to lift functions to the top level and not
      // create any nesting
      def innerFn(fn: Lambda[K]): T[Code.ValueLike] =
        if (fn.captures.isEmpty) {
          liftedFnName(fn).map { ident =>
            boxFn(ident, fn.arity);
          }
        } else {
          // we create the function, then we allocate
          // values for the capture
          // alloc_closure<n>(capLen, captures, fnName)
          for {
            ident <- liftedFnName(fn)
            capName <- newLocalName("captures")
            capValues <- fn.captures.traverse(innerToValue(_))
            decl <- Code.ValueLike.declareArray(
              capName,
              Code.TypeIdent.BValue,
              capValues
            )(newLocalName)
          } yield Code.WithValue(
            decl,
            Code.Ident(s"alloc_closure${fn.arity}")(
              Code.IntLiteral(BigInt(fn.captures.length)),
              capName,
              ident
            )
          )
        }

      def literal(lit: Lit): T[Code.ValueLike] =
        lit match {
          case c @ Lit.Chr(_) =>
            // encoded as integers in pure values
            pv(
              Code.Ident("bsts_char_from_code_point")(
                Code.IntLiteral(c.toCodePoint)
              )
            )
          case Lit.Integer(toBigInteger) =>
            try {
              val iv = toBigInteger.intValueExact()
              pv(Code.Ident("bsts_integer_from_int")(Code.IntLiteral(iv)))
            } catch {
              case _: ArithmeticException =>
                // emit the uint32 words and sign
                val isPos = toBigInteger.signum >= 0
                var current = if (isPos) toBigInteger else toBigInteger.negate()
                val two32 = BigInteger.ONE.shiftLeft(32)
                val bldr = List.newBuilder[Code.IntLiteral]
                while (current.compareTo(BigInteger.ZERO) > 0) {
                  bldr += Code.IntLiteral(current.mod(two32).longValue())
                  current = current.shiftRight(32)
                }
                val lits = bldr.result()
                // call:
                // bsts_integer_from_words_copy(_Bool is_pos, size_t size, int32_t* words);
                newLocalName("int").map { ident =>
                  Code.DeclareArray(
                    Code.TypeIdent.UInt32,
                    ident,
                    Right(lits)
                  ) +:
                    Code.Ident("bsts_integer_from_words_copy")(
                      if (isPos) Code.TrueLit else Code.FalseLit,
                      Code.IntLiteral(lits.length),
                      ident
                    )
                }
            }

          case Lit.Str(toStr) => StringApi.fromString(toStr)
          case f: Lit.Float64 =>
            pv(
              Code.Ident("bsts_float64_from_bits")(
                Code.IntLiteral(BigInt(f.toRawLongBits))
              )
            )
        }

      def innerApp[K1 <: K](app: App[K1]): T[Code.ValueLike] =
        app match {
          case App(Global(k, pack, fnName), args) =>
            directFn(k, pack, fnName).flatMap {
              case Some((ident, _)) =>
                // directly invoke instead of by treating them like lambdas
                args.traverse(innerToValue(_)).flatMap { argsVL =>
                  Code.ValueLike.applyArgs(ident, argsVL)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (globalIdent(k, pack, fnName), args.traverse(innerToValue(_)))
                  .flatMapN { (fnVL, argsVL) =>
                    // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                    // but since these are ValueLike, we need to handle more carefully
                    val fnValue = fnVL.onExpr(e => pv(e()))(newLocalName);
                    fnValue.flatMap { fnValue =>
                      val fnSize = argsVL.length
                      val callFn = Code.Ident(s"call_fn$fnSize")
                      Code.ValueLike.applyArgs(callFn, fnValue :: argsVL)(
                        newLocalName
                      )
                    }
                  }
            }
          case App(Local(fnName), args) =>
            directFn(fnName).flatMap {
              case Some((ident, isClosure, _)) =>
                // this can be an recursive call
                args.traverse(innerToValue(_)).flatMap { argsVL =>
                  // if we don't have a closure, which is lifted to top level
                  // directly invoke instead of by treating them like lambdas
                  val withSlot =
                    if (isClosure) slotsArgName :: argsVL
                    else argsVL
                  Code.ValueLike.applyArgs(ident, withSlot)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (getBinding(fnName), args.traverse(innerToValue(_))).flatMapN {
                  (fnVL, argsVL) =>
                    // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                    // but since these are ValueLike, we need to handle more carefully
                    val fnSize = argsVL.length
                    val callFn = Code.Ident(s"call_fn$fnSize")
                    Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(
                      newLocalName
                    )
                }
            }
          case App(MakeEnum(variant, arity, _), args) =>
            // to type check, we know that the arity must have the same length as args
            args.traverse(innerToValue).flatMap { argsVL =>
              val tag = Code.IntLiteral(variant)
              Code.ValueLike.applyArgs(
                Code.Ident(s"alloc_enum$arity"),
                tag :: argsVL
              )(newLocalName)
            }
          case App(MakeStruct(arity), args) =>
            if (arity == 1) {
              // this is a new-type, just return the arg
              innerToValue(args.head)
            } else {
              // to type check, we know that the arity must have the same length as args
              args.traverse(innerToValue).flatMap { argsVL =>
                Code.ValueLike.applyArgs(
                  Code.Ident(s"alloc_struct$arity"),
                  argsVL
                )(newLocalName)
              }
            }
          case App(SuccNat, args) =>
            innerToValue(args.head).flatMap { arg =>
              Code.ValueLike.applyArgs(
                Code.Ident("BSTS_NAT_SUCC"),
                NonEmptyList.one(arg)
              )(newLocalName)
            }
          case App(fn, args) =>
            (innerToValue(fn), args.traverse(innerToValue(_))).flatMapN {
              (fnVL, argsVL) =>
                // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                // but since these are ValueLike, we need to handle more carefully
                val fnSize = argsVL.length
                val callFn = Code.Ident(s"call_fn$fnSize")
                Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(newLocalName)
            }
        }

      def innerToValue(expr: Expr[K]): T[Code.ValueLike] =
        expr match {
          case fn @ Lambda(_, _, _, _) => innerFn(fn)
          case Let(name, argV, in)     =>
            handleLet(name, argV, innerToValue(in))
          case app @ App(_, _)       => innerApp(app)
          case Global(k, pack, name) =>
            directFn(k, pack, name)
              .flatMap {
                case Some((ident, arity)) =>
                  pv(boxFn(ident, arity))
                case None =>
                  globalIdent(k, pack, name).map(nm => nm())
              }
          case Local(arg) =>
            directFn(arg)
              .flatMap {
                case Some((nm, isClosure, arity)) =>
                  if (!isClosure) {
                    // a closure can't be a static name
                    pv(boxFn(nm, arity))
                  } else {
                    // recover the pointer to this closure from the slots argument
                    pv(Code.Ident("bsts_closure_from_slots")(slotsArgName))
                  }
                case None =>
                  getBinding(arg).widen
              }
          case ClosureSlot(idx) =>
            // we must be inside a closure function, so we should have a slots argument to access
            pv(slotsArgName.bracket(Code.IntLiteral(BigInt(idx))))
          case LocalAnon(ident)              => getAnon(ident).widen
          case LocalAnonMut(ident)           => getAnon(ident).widen
          case LetMut(LocalAnonMut(m), span) =>
            bindAnon(m) {
              for {
                ident <- getAnon(m)
                decl = Code.DeclareVar(Nil, Code.TypeIdent.BValue, ident, None)
                res <- innerToValue(span)
              } yield decl +: res
            }
          case Literal(lit)                 => literal(lit)
          case If(cond, thenExpr, elseExpr) =>
            (boolToValue(cond), innerToValue(thenExpr), innerToValue(elseExpr))
              .flatMapN { (c, thenC, elseC) =>
                Code.ValueLike.ifThenElseV(c, thenC, elseC)(newLocalName)
              }
          case Always.SetChain(setmuts, result) =>
            (
              setmuts.traverse { case (LocalAnonMut(mut), v) =>
                for {
                  name <- getAnon(mut)
                  vl <- innerToValue(v)
                } yield (name := vl)
              },
              innerToValue(result)
            ).mapN { (assigns, result) =>
              Code.Statements(assigns) +: result
            }
          case Always(cond, thenExpr) =>
            boolToValue(cond).flatMap { bv =>
              bv.discardValue match {
                case None         => innerToValue(thenExpr)
                case Some(effect) => innerToValue(thenExpr).map(effect +: _)
              }
            }
          case GetEnumElement(arg, _, index, _) =>
            // call get_enum_index(v, index)
            innerToValue(arg).flatMap { v =>
              v.onExpr(e =>
                pv(Code.Ident("get_enum_index")(e, Code.IntLiteral(index)))
              )(newLocalName)
            }
          case GetStructElement(arg, index, size) =>
            if (size == 1) {
              // this is just a new-type wrapper, ignore it
              innerToValue(arg)
            } else {
              // call get_struct_index(v, index)
              innerToValue(arg).flatMap { v =>
                v.onExpr { e =>
                  pv(Code.Ident("get_struct_index")(e, Code.IntLiteral(index)))
                }(newLocalName)
              }
            }
          case makeEnum @ MakeEnum(variant, arity, _) =>
            // this is a closure over variant, we rewrite this
            if (arity == 0)
              pv(Code.Ident("alloc_enum0")(Code.IntLiteral(variant)))
            else {
              val named =
                // safe because arity > 0
                NonEmptyList.fromListUnsafe(
                  Idents.allSimpleIdents
                    .take(arity)
                    .map(nm => Identifier.Name(nm))
                    .toList
                )
              // This relies on optimizing App(MakeEnum, _) otherwise
              // it creates an infinite loop.
              // Also, this we should cache creation of Lambda/Closure values
              innerToValue(
                Lambda(
                  Nil,
                  None,
                  named,
                  applyArgs(makeEnum, named.map(Local(_)))
                )
              )
            }
          case MakeStruct(arity) =>
            pv {
              if (arity == 0) Code.Ident("bsts_unit_value")()
              else {
                val allocStructFn = s"alloc_struct$arity"
                boxFn(Code.Ident(allocStructFn), arity)
              }
            }
          case ZeroNat =>
            pv(Code.Ident("BSTS_NAT_0"))
          case SuccNat =>
            val arg = Identifier.Name("nat")
            // This relies on optimizing App(SuccNat, _) otherwise
            // it creates an infinite loop.
            // Also, this we should cache creation of Lambda/Closure values
            innerToValue(
              Lambda(
                Nil,
                None,
                NonEmptyList.one(arg),
                applyArgs(SuccNat, NonEmptyList.one(Local(arg)))
              )
            )
          case PrevNat(of) =>
            innerToValue(of).flatMap { argVL =>
              Code.ValueLike.applyArgs(
                Code.Ident("BSTS_NAT_PREV"),
                NonEmptyList.one(argVL)
              )(newLocalName)
            }
          case WhileExpr(cond, effect, res) =>
            (
              boolToValue(cond),
              innerToValue(effect),
              innerToValue(res),
              newLocalName("cond")
            )
              .mapN { (cond, effect, res, condVar) =>
                Code.Statements(
                  Code.DeclareVar(Nil, Code.TypeIdent.Bool, condVar, None),
                  condVar := cond,
                  Code.While(
                    condVar,
                    Code.Block(
                      NonEmptyList
                        .one(
                          condVar := cond
                        )
                        .prependList(
                          effect.discardValue.toList
                        )
                    )
                  )
                ) +: res
              }
        }

      def fnStatement[K1 <: K](
          fnName: Code.Ident,
          fn: Lambda[K1]
      ): T[Code.Statement] =
        inFnStatement(fn match {
          case Lambda(captures, name, args, expr) =>
            val body = innerToValue(expr).map(Code.returnValue(_))
            val body1 = name match {
              case None      => body
              case Some(rec) =>
                recursiveName(
                  fnName,
                  rec,
                  isClosure = captures.nonEmpty,
                  arity = fn.arity
                )(body)
            }

            bindAll(args) {
              for {
                argParams <- args.traverse { b =>
                  getBinding(b).map(i => Code.Param(Code.TypeIdent.BValue, i))
                }
                fnBody <- body1
                allArgs =
                  if (captures.isEmpty) argParams
                  else {
                    Code.Param(
                      Code.TypeIdent.BValue.ptr,
                      slotsArgName
                    ) :: argParams
                  }
              } yield Code.DeclareFn(
                Nil,
                Code.TypeIdent.BValue,
                fnName,
                allArgs.toList,
                Some(Code.block(fnBody))
              )
            }
        })

      def renderTop(k: K, p: PackageName, b: Bindable, expr: Expr[K]): T[Unit] =
        inTop(k, p, b) {
          expr match {
            case fn: Lambda[K] =>
              for {
                fnName <- globalIdent(k, p, b)
                stmt <- fnStatement(fnName, fn)
                _ <- appendStatement(stmt)
              } yield ()
            case someValue =>
              // TODO: if we can create the value statically, we don't
              // need the read_or_build trick
              //
              // we materialize an Atomic value to hold the static data
              // then we generate a function to populate the value
              for {
                vl <- innerToValue(someValue)
                value <- staticValueName(k, p, b)
                consFn <- constructorFn(k, p, b)
                readFn <- globalIdent(k, p, b)
                _ <- makeConstructorsStatement(value, consFn, vl, readFn)
              } yield ()
          }
        }

      def makeConstructorsStatement(
          value: Code.Ident,
          consFn: Code.Ident,
          vl: Code.ValueLike,
          readFn: Code.Ident
      ): T[Unit] =
        // TODO: if we can create the value statically, we don't
        // need the read_or_build trick
        //
        // we materialize an Atomic value to hold the static data
        // then we generate a function to populate the value
        for {
          _ <- appendStatement(
            Code.DeclareVar(
              Code.Attr.Static :: Nil,
              Code.TypeIdent.AtomicBValue,
              value,
              Some(Code.IntLiteral.Zero)
            )
          )
          _ <- appendStatement(
            Code.DeclareFn(
              Code.Attr.Static :: Nil,
              Code.TypeIdent.BValue,
              consFn,
              Nil,
              Some(Code.block(Code.returnValue(vl)))
            )
          )
          res = Code.Ident("read_or_build")(value.addr, consFn)
          _ <- appendStatement(
            Code.DeclareFn(
              Code.Attr.Static :: Nil,
              Code.TypeIdent.BValue,
              readFn,
              Nil,
              Some(Code.block(Code.returnValue(res)))
            )
          )
        } yield ()

      def renderMain(p: PackageName, b: Bindable, mainRun: Code.Ident): T[Unit]
      def renderTests(values: List[(PackageName, Bindable)]): T[Unit]
    }

    object Env {
      def impl: Env = {
        type ErrorOr[A] = EitherT[Eval, Error, A]
        type StateF[S] = [A] =>> StateT[ErrorOr, S, A]

        def catsMonad[S]: Monad[StateF[S]] =
          implicitly

        new Env {
          sealed abstract class BindingKind {
            def ident: Code.Ident
          }
          object BindingKind {
            case class Normal(
                bn: Bindable,
                idx: Int,
                localFn: Option[DirectFnRef]
            ) extends BindingKind {
              val ident = Code.Ident(
                Idents.escape("__bsts_b_", bn.asString + idx.toString)
              )
            }
            case class Recursive(
                ident: Code.Ident,
                isClosure: Boolean,
                arity: Int,
                idx: Int
            ) extends BindingKind
          }
          case class BindState(count: Int, stack: List[BindingKind]) {
            def pop: BindState =
              // by invariant this tail should never fail
              copy(stack = stack.tail)

            def nextBind(
                bn: Bindable,
                directFn: Option[DirectFnRef] = None
            ): BindState =
              copy(
                count = count + 1,
                BindingKind.Normal(bn, count, directFn) :: stack
              )

            def nextRecursive(
                fnName: Code.Ident,
                isClosure: Boolean,
                arity: Int
            ): BindState =
              copy(
                count = count + 1,
                BindingKind.Recursive(fnName, isClosure, arity, count) :: stack
              )
          }

          object BindState {
            val empty: BindState = BindState(0, Nil)
          }

          case class State(
              allValues: AllValues,
              externals: ExternalResolver,
              includeSet: Set[Code.Include],
              includes: Chain[Code.Include],
              stmts: Chain[Code.Statement],
              currentTop: Option[(K, PackageName, Bindable)],
              binds: Map[Bindable, BindState],
              counter: Long,
              identCache: Map[Expr[K], Code.Ident]
          ) {
            def finalFile: Doc =
              Doc.intercalate(
                Doc.hardLine,
                includes.iterator.map(Code.toDoc(_)).toList
              ) +
                Doc.hardLine + Doc.hardLine +
                Doc.intercalate(
                  Doc.hardLine + Doc.hardLine,
                  stmts.iterator.map(Code.toDoc(_)).toList
                )

            def include(incl: Code.Include): State =
              if (includeSet(incl)) this
              else
                copy(
                  includeSet = includeSet + incl,
                  includes = includes :+ incl
                )
          }

          object State {
            def init(
                allValues: AllValues,
                externals: ExternalResolver
            ): State = {
              val defaultIncludes =
                List(Code.Include.quote("bosatsu_runtime.h"))

              State(
                allValues,
                externals,
                Set.empty ++ defaultIncludes,
                Chain.fromSeq(defaultIncludes),
                Chain.empty,
                None,
                Map.empty,
                0L,
                Map.empty
              )
            }
          }

          type T[A] = StateT[ErrorOr, State, A]

          implicit val monadImpl: Monad[T] = catsMonad[State]

          def run(
              pm: AllValues,
              externals: ExternalResolver,
              t: T[Unit]
          ): Either[Error, Doc] =
            t.run(State.init(pm, externals))
              .value // get the value out of the EitherT
              .value // evaluate the Eval
              .map(_._1.finalFile)

          def appendStatement(stmt: Code.Statement): T[Unit] =
            StateT.modify(s =>
              s.copy(stmts = s.stmts :+ ClangGen.StackAllocPass.optimize(stmt))
            )

          def errorRes[A](e: => Error): EitherT[Eval, Error, A] =
            EitherT[Eval, Error, A](Eval.later(Left(e)))

          def error[A](e: => Error): T[A] =
            StateT(_ => errorRes(e))

          def result[A](s: State, a: A): EitherT[Eval, Error, (State, A)] =
            EitherT[Eval, Error, (State, A)](
              Eval.now(Right((s, a)))
            )

          def update[A](fn: State => (State, A)): T[A] =
            StateT(s =>
              EitherT[Eval, Error, (State, A)](Eval.now(Right(fn(s))))
            )

          def tryUpdate[A](fn: State => Either[Error, (State, A)]): T[A] =
            StateT(s => EitherT[Eval, Error, (State, A)](Eval.now(fn(s))))

          def read[A](fn: State => A): T[A] =
            StateT(s =>
              EitherT[Eval, Error, (State, A)](Eval.now(Right((s, fn(s)))))
            )

          def tryRead[A](fn: State => Either[Error, A]): T[A] =
            StateT(s =>
              EitherT[Eval, Error, (State, A)](Eval.now(fn(s).map((s, _))))
            )

          def globalIdent(k: K, pn: PackageName, bn: Bindable): T[Code.Ident] =
            tryUpdate { s =>
              val depKey = ns.depFor(k, pn)
              s.externals(depKey, pn, bn) match {
                case Some((incl, ident, _)) =>
                  // TODO: suspect that we are ignoring arity here
                  val withIncl = s.include(incl)
                  Right((withIncl, ident))
                case None =>
                  val key = (depKey, pn, bn)
                  s.allValues.get(key) match {
                    case Some((_, ident)) => Right((s, ident))
                    case None             =>
                      val scope: String = Show[K].show(depKey)
                      Left(
                        ClangGen.Error.UnknownValue(
                          scope,
                          pn,
                          bn,
                          s.currentTop.map { case (k1, p1, b1) =>
                            (Show[K].show(k1), p1, b1)
                          }
                        )
                      )
                  }
              }
            }

          def bind[A](
              bn: Bindable,
              directFn: Option[DirectFnRef]
          )(in: T[A]): T[A] = {
            val init: T[Unit] = update { s =>
              val bs0 = s.binds.get(bn) match {
                case None     => BindState.empty
                case Some(bs) => bs
              }
              val bs1 = bs0.nextBind(bn, directFn)
              (s.copy(binds = s.binds.updated(bn, bs1)), ())
            }

            val uninit: T[Unit] = update { s =>
              val bs1 = s.binds.get(bn) match {
                case Some(bs) => bs.pop
                case None     => sys.error(s"bindable $bn no longer in $s")
              }
              (s.copy(binds = s.binds.updated(bn, bs1)), ())
            }

            for {
              _ <- init
              a <- in
              _ <- uninit
            } yield a
          }
          def getBinding(bn: Bindable): T[Code.Ident] =
            tryRead { s =>
              s.binds.get(bn) match {
                case Some(bs) => Right(bs.stack.head.ident)
                case None     => Left(ClangGen.Error.Unbound(bn, s.currentTop))
              }
            }
          def bindAnon[A](idx: Long)(in: T[A]): T[A] =
            // in the future we see the scope of the binding which matters for GC, but here
            // we don't care
            in

          def getAnon(idx: Long): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("__bsts_a_", idx.toString)))

          // a recursive function needs to remap the Bindable to the top-level mangling
          def recursiveName[A](
              fnName: Code.Ident,
              bn: Bindable,
              isClosure: Boolean,
              arity: Int
          )(in: T[A]): T[A] = {
            val init: T[Unit] = update { s =>
              val bs0 = s.binds.get(bn) match {
                case Some(bs) => bs
                case None     => BindState.empty
              }
              val bs1 = bs0.nextRecursive(fnName, isClosure, arity)
              (s.copy(binds = s.binds.updated(bn, bs1)), ())
            }

            val uninit: T[Unit] = update { s =>
              val bs1 = s.binds.get(bn) match {
                case Some(bs) => bs.pop
                case None     => sys.error(s"bindable $bn no longer in $s")
              }
              (s.copy(binds = s.binds.updated(bn, bs1)), ())
            }

            for {
              _ <- init
              a <- in
              _ <- uninit
            } yield a
          }

          val nextCnt: T[Long] =
            StateT { s =>
              val cnt = s.counter
              val s1 = s.copy(counter = cnt + 1L)
              result(s1, cnt)
            }

          // used for temporary variables of type BValue
          def newLocalName(tag: String): T[Code.Ident] =
            nextCnt.map { cnt =>
              Code.Ident(Idents.escape("__bsts_l_", tag + cnt.toString))
            }
          def newTopName(tag: String): T[Code.Ident] =
            nextCnt.map { cnt =>
              Code.Ident(Idents.escape("__bsts_t_", tag + cnt.toString))
            }
          // record that this name is a top level function, so applying it can be direct
          def directFn(
              k: K,
              pack: PackageName,
              b: Bindable
          ): T[Option[(Code.Ident, Int)]] =
            StateT { s =>
              val depKey = ns.depFor(k, pack)
              val key = (depKey, pack, b)
              s.allValues.get(key) match {
                case Some((fn: Matchless.Lambda[K], ident)) =>
                  result(s, Some((ident, fn.arity)))
                case None =>
                  // this is external
                  s.externals(depKey, pack, b) match {
                    case Some((incl, ident, arity)) if arity > 0 =>
                      val withIncl = s.include(incl)
                      result(withIncl, Some((ident, arity)))
                    case _ => result(s, None)
                  }
                case _ => result(s, None)
              }
            }

          def directFn(b: Bindable): T[Option[DirectFnRef]] =
            read { s =>
              s.binds.get(b) match {
                case Some(
                      BindState(_, BindingKind.Recursive(n, c, a, _) :: _)
                    ) =>
                  Some((n, c, a))
                case Some(
                      BindState(
                        _,
                        BindingKind.Normal(_, _, some @ Some(_)) :: _
                      )
                    ) =>
                  some
                case _ =>
                  None
              }
            }

          def inFnStatement[A](ta: T[A]): T[A] =
            for {
              bindState <- update { (s: State) =>
                (s.copy(binds = Map.empty), s.binds)
              }
              a <- ta
              _ <- update((s: State) => (s.copy(binds = bindState), ()))
            } yield a

          def inTop[A](k: K, p: PackageName, bn: Bindable)(ta: T[A]): T[A] =
            for {
              bindState <- update { (s: State) =>
                (
                  s.copy(binds = Map.empty, currentTop = Some((k, p, bn))),
                  s.binds
                )
              }
              a <- ta
              _ <- update { (s: State) =>
                (s.copy(currentTop = None, binds = bindState), ())
              }
            } yield a

          val currentTop: T[Option[(K, PackageName, Bindable)]] =
            StateT((s: State) => result(s, s.currentTop))

          def staticValueName(
              k: K,
              p: PackageName,
              b: Bindable
          ): T[Code.Ident] =
            monadImpl.pure(
              Code.Ident(Idents.escape("___bsts_s_", fullName(k, p, b)))
            )
          def constructorFn(k: K, p: PackageName, b: Bindable): T[Code.Ident] =
            monadImpl.pure(
              Code.Ident(Idents.escape("___bsts_c_", fullName(k, p, b)))
            )

          // the Code.Ident should be a function with signature:
          // int run_main(BValue main_value, int argc, char** args)
          def renderMain(
              p: PackageName,
              b: Bindable,
              mainRun: Code.Ident
          ): T[Unit] =
            /*
            int main(int argc, char** argv) {
              GC_init();
              init_statics();
              atexit(free_statics);
              BValue main_value = (main_value)();
              int code = run_main(main_value, argc, argv);
              return code;
            }
             */
            globalIdent(ns.rootKey, p, b).flatMap { mainCons =>
              val mainValue = Code.Ident("main_value")
              val mainBody = Code.Statements(
                Code.Ident("GC_init")().stmt,
                Code.Ident("init_statics")().stmt,
                Code.Ident("atexit")(Code.Ident("free_statics")).stmt,
                Code.DeclareVar(
                  Nil,
                  Code.TypeIdent.BValue,
                  mainValue,
                  Some(mainCons())
                ),
                Code.Return(
                  Some(
                    mainRun(mainValue, Code.Ident("argc"), Code.Ident("argv"))
                  )
                )
              )

              val mainFn = Code.declareMain(mainBody)
              appendStatement(mainFn)
            } *> StateT { s =>
              result(
                s.include(Code.Include.angle("stdlib.h"))
                  .include(Code.Include.quote("gc.h")),
                ()
              )
            }

          def renderTests(values: List[(PackageName, Bindable)]): T[Unit] =
            values
              .traverse { case (p, b) =>
                globalIdent(ns.rootKey, p, b).map { i =>
                  (Code.StrLiteral(p.asString), i)
                }
              }
              .flatMap { packVals =>
                /*
              int main(int argc, char** argv) {
                GC_init();
                init_statics();
                atexit(free_statics);

                BSTS_Test_Result[size] results;
                results[0] = bsts_test_run(pack[0], testVal[0]);
                ...
                int code = bsts_test_result_print_summary(size, results);
                return code;
              }
                 */
                val results = Code.Ident("results")
                val runFn = Code.Ident("bsts_test_run")
                val summaryFn = Code.Ident("bsts_test_result_print_summary")
                val testCount = packVals.length
                val allTests = packVals.mapWithIndex { case ((n, tv), idx) =>
                  results.bracket(Code.IntLiteral(idx)) := runFn(n, tv)
                }
                val header = Code.Statements(
                  Code.Ident("GC_init")().stmt,
                  Code.Ident("init_statics")().stmt,
                  Code.Ident("atexit")(Code.Ident("free_statics")).stmt,
                  Code.DeclareArray(
                    Code.TypeIdent.Named("BSTS_Test_Result"),
                    results,
                    Left(testCount)
                  )
                )

                val mainFn = Code.declareMain(
                  header ++
                    allTests +
                    Code.returnValue(
                      summaryFn(Code.IntLiteral(testCount), results)
                    )
                )

                appendStatement(mainFn)
              } *> StateT { s =>
              result(
                s.include(Code.Include.angle("stdlib.h"))
                  .include(Code.Include.quote("gc.h")),
                ()
              )
            }

          def cachedIdent(
              key: Expr[K]
          )(value: => T[Code.Ident]): T[Code.Ident] =
            StateT { s =>
              s.identCache.get(key) match {
                case Some(ident) => result(s, ident)
                case None        =>
                  for {
                    s1Ident <- value.run(s)
                    (s1, ident) = s1Ident
                    s2 = s1.copy(identCache = s.identCache + (key -> ident))
                    res <- result(s2, ident)
                  } yield res
              }
            }
        }
      }
    }
  }
}

object ClangGen {
  sealed abstract class Error {
    def display: Doc =
      this match {
        case Error.UnknownValue(scope, pack, value, inside) =>
          val base =
            Doc.text(show"unknown value in clang codegen: $pack.$value") +
              Doc.text(s" (scope: $scope)")
          inside match {
            case Some((scope1, p, b)) =>
              base + Doc.text(show" referenced from $p.$b") +
                Doc.text(s" (scope: $scope1)")
            case None =>
              base
          }
        case Error.InvariantViolation(message, expr) =>
          Doc.text(s"invariant violation: $message in $expr")
        case Error.Unbound(bn, inside) =>
          val base = Doc.text(show"unbound local value: $bn")
          inside match {
            case Some((_, p, b)) =>
              base + Doc.text(show" in $p.$b")
            case None =>
              base
          }
      }
  }

  object Error {
    case class UnknownValue(
        scope: String,
        pack: PackageName,
        value: Bindable,
        inside: Option[(String, PackageName, Bindable)]
    ) extends Error
    case class InvariantViolation(message: String, expr: Expr[Any])
        extends Error
    case class Unbound(
        bn: Bindable,
        inside: Option[(Any, PackageName, Bindable)]
    ) extends Error
  }

  private object StackAllocPass {
    private final case class Ref[A <: AnyRef](get: A) {
      override def equals(that: Any): Boolean =
        that match {
          case ref: Ref[?] => get eq ref.get
          case _           => false
        }
      override def hashCode(): Int =
        System.identityHashCode(get)
    }

    private enum AllocKind {
      case Enum
      case Struct
    }
    private given CanEqual[AllocKind, AllocKind] = CanEqual.derived
    private case class AllocMeta(kind: AllocKind, arity: Int) {
      def heapLike: Boolean =
        kind match {
          case AllocKind.Enum   => arity > 0
          case AllocKind.Struct => arity > 1
        }

      def apply(args: List[Code.Expression]): Code.Expression =
        kind match {
          case AllocKind.Enum   => stackAllocEnum(arity, args)
          case AllocKind.Struct => stackAllocStruct(arity, args)
        }
    }

    private enum ParentRef {
      case ParentExpr(parent: Code.Expression, role: ExprRole)
      case ParentStmt(parent: Code.Statement, role: StmtRole)
    }
    private enum ExprRole {
      case ApplyFn
      case ApplyArg(index: Int)
      case Wrapped
    }
    private given CanEqual[ExprRole, ExprRole] = CanEqual.derived
    private enum StmtRole {
      case AssignTarget
      case AssignValue
      case DeclareInit
      case DeclareArrayValue
      case ReturnValue
      case Other
    }
    private given CanEqual[StmtRole, StmtRole] = CanEqual.derived
    private enum GraphNode {
      case AllocNode(id: Int)
      case VarNode(id: Code.Ident)
      case EscapeNode
    }

    private enum Consumer {
      case ToVar(id: Code.Ident)
      case ToAlloc(id: Int)
      case Escape
      case Local
      case Ignore
    }
    @nowarn("msg=unused private member")
    private given CanEqual[Consumer, Consumer] = CanEqual.derived

    private val getterFns: Set[String] =
      Set("get_variant", "get_variant_value", "get_enum_index", "get_struct_index")

    private def isEscapeBoundaryCall(name: String): Boolean =
      name == "read_or_build" ||
        name.startsWith("call_fn") ||
        name.startsWith("alloc_closure") ||
        name.startsWith("alloc_boxed_pure_fn")

    private def isBValueType(tpe: Code.TypeIdent): Boolean =
      tpe match {
        case Code.TypeIdent.Named("BValue") => true
        case _                              => false
      }

    private def calleeName(app: Code.Apply): Option[String] =
      app.fn match {
        case Code.Ident(name) => Some(name)
        case _                => None
      }

    private def allocInfoOf(app: Code.Apply): Option[AllocMeta] =
      calleeName(app).flatMap { name =>
        if (name.startsWith("alloc_enum")) {
          name.stripPrefix("alloc_enum").toIntOption.map { arity =>
            AllocMeta(AllocKind.Enum, arity)
          }
        } else if (name.startsWith("alloc_struct")) {
          name.stripPrefix("alloc_struct").toIntOption.map { arity =>
            AllocMeta(AllocKind.Struct, arity)
          }
        } else None
      }

    private case class Analysis(
        parentOf: Map[Ref[Code.Expression], ParentRef],
        allocIds: Map[Ref[Code.Expression], Int],
        allocExprById: Map[Int, Code.Expression],
        allocById: Map[Int, AllocMeta],
        bValueVars: Set[Code.Ident],
        mutableVars: Set[Code.Ident],
        identExprs: List[(Code.Expression, Code.Ident)]
    )

    private def allocIdOf(
        allocIds: Map[Ref[Code.Expression], Int],
        expr: Code.Expression
    ): Option[Int] =
      allocIds.get(Ref(expr))

    private def analyze(fn: Code.DeclareFn): Option[(Analysis, Set[Int])] =
      fn.value match {
        case None => None
        case Some(body) =>
          val parentOf = mutable.Map.empty[Ref[Code.Expression], ParentRef]
          val allocIds = mutable.Map.empty[Ref[Code.Expression], Int]
          val allocExprById = mutable.Map.empty[Int, Code.Expression]
          val allocById = mutable.Map.empty[Int, AllocMeta]
          val bValueVars = mutable.Set.empty[Code.Ident]
          val assignmentTargets = mutable.Set.empty[Code.Ident]
          val identExprs = mutable.ListBuffer.empty[(Code.Expression, Code.Ident)]
          var nextAllocId = 0

          fn.args.iterator.foreach {
            case Code.Param(tpe, name) if isBValueType(tpe) =>
              bValueVars += name
            case _ => ()
          }

          def setParent(
              child: Code.Expression,
              parent: ParentRef
          ): Unit =
            parentOf.update(Ref(child), parent)

          def visitExpr(expr: Code.Expression): Unit = {
            expr match {
              case id @ Code.Ident(name) =>
                identExprs += ((id, Code.Ident(name)))

              case app @ Code.Apply(fnExpr, args) =>
                allocInfoOf(app).foreach { info =>
                  val allocId = nextAllocId
                  nextAllocId += 1
                  allocIds.update(Ref(app), allocId)
                  allocExprById.update(allocId, app)
                  allocById.update(allocId, info)
                }
                setParent(fnExpr, ParentRef.ParentExpr(app, ExprRole.ApplyFn))
                visitExpr(fnExpr)
                args.zipWithIndex.foreach { case (argExpr, idx) =>
                  setParent(
                    argExpr,
                    ParentRef.ParentExpr(app, ExprRole.ApplyArg(idx))
                  )
                  visitExpr(argExpr)
                }

              case cast @ Code.Cast(_, in) =>
                setParent(in, ParentRef.ParentExpr(cast, ExprRole.Wrapped))
                visitExpr(in)

              case sel @ Code.Select(target, _) =>
                setParent(target, ParentRef.ParentExpr(sel, ExprRole.Wrapped))
                visitExpr(target)

              case bin @ Code.BinExpr(left, _, right) =>
                setParent(left, ParentRef.ParentExpr(bin, ExprRole.Wrapped))
                setParent(right, ParentRef.ParentExpr(bin, ExprRole.Wrapped))
                visitExpr(left)
                visitExpr(right)

              case pre @ Code.PrefixExpr(_, target) =>
                setParent(target, ParentRef.ParentExpr(pre, ExprRole.Wrapped))
                visitExpr(target)

              case post @ Code.PostfixExpr(target, _) =>
                setParent(target, ParentRef.ParentExpr(post, ExprRole.Wrapped))
                visitExpr(target)

              case bracket @ Code.Bracket(target, item) =>
                setParent(
                  target,
                  ParentRef.ParentExpr(bracket, ExprRole.Wrapped)
                )
                setParent(item, ParentRef.ParentExpr(bracket, ExprRole.Wrapped))
                visitExpr(target)
                visitExpr(item)

              case tern @ Code.Ternary(cond, whenTrue, whenFalse) =>
                setParent(cond, ParentRef.ParentExpr(tern, ExprRole.Wrapped))
                setParent(
                  whenTrue,
                  ParentRef.ParentExpr(tern, ExprRole.Wrapped)
                )
                setParent(
                  whenFalse,
                  ParentRef.ParentExpr(tern, ExprRole.Wrapped)
                )
                visitExpr(cond)
                visitExpr(whenTrue)
                visitExpr(whenFalse)

              case Code.IntLiteral(_) | Code.StrLiteral(_) =>
                ()
            }
          }

          def visitStatement(stmt: Code.Statement): Unit =
            stmt match {
              case assign @ Code.Assignment(target, value) =>
                setParent(
                  target,
                  ParentRef.ParentStmt(assign, StmtRole.AssignTarget)
                )
                setParent(
                  value,
                  ParentRef.ParentStmt(assign, StmtRole.AssignValue)
                )
                visitExpr(target)
                visitExpr(value)
                target match {
                  case id: Code.Ident => assignmentTargets += id
                  case _              => ()
                }

              case decl @ Code.DeclareVar(_, tpe, ident, valueOpt) =>
                if (isBValueType(tpe)) {
                  bValueVars += ident
                }
                valueOpt.foreach { value =>
                  setParent(
                    value,
                    ParentRef.ParentStmt(decl, StmtRole.DeclareInit)
                  )
                  visitExpr(value)
                }

              case Code.DeclareArray(_, _, values) =>
                values match {
                  case Left(_)    => ()
                  case Right(vs) =>
                    vs.foreach { value =>
                      setParent(
                        value,
                        ParentRef.ParentStmt(stmt, StmtRole.DeclareArrayValue)
                      )
                      visitExpr(value)
                    }
                }

              case Code.Return(valueOpt) =>
                valueOpt.foreach { value =>
                  setParent(
                    value,
                    ParentRef.ParentStmt(stmt, StmtRole.ReturnValue)
                  )
                  visitExpr(value)
                }

              case Code.Effect(expr) =>
                setParent(expr, ParentRef.ParentStmt(stmt, StmtRole.Other))
                visitExpr(expr)

              case Code.While(cond, body) =>
                setParent(cond, ParentRef.ParentStmt(stmt, StmtRole.Other))
                visitExpr(cond)
                visitStatement(body)

              case Code.DoWhile(body, cond) =>
                visitStatement(body)
                setParent(cond, ParentRef.ParentStmt(stmt, StmtRole.Other))
                visitExpr(cond)

              case Code.IfElse(ifs, elseCond) =>
                ifs.iterator.foreach { case (cond, blk) =>
                  setParent(cond, ParentRef.ParentStmt(stmt, StmtRole.Other))
                  visitExpr(cond)
                  visitStatement(blk)
                }
                elseCond.foreach(visitStatement)

              case Code.Block(items) =>
                items.iterator.foreach(visitStatement)

              case Code.Statements(items) =>
                items.iterator.foreach(visitStatement)

              case _: Code.DeclareFn | _: Code.Include | _: Code.Typedef |
                  _: Code.DefineComplex =>
                ()
            }

          visitStatement(body)

          if (allocById.isEmpty) None
          else {
            val mutableVars = assignmentTargets.toSet.intersect(bValueVars.toSet)
            val analysis = Analysis(
              parentOf = parentOf.toMap,
              allocIds = allocIds.toMap,
              allocExprById = allocExprById.toMap,
              allocById = allocById.toMap,
              bValueVars = bValueVars.toSet,
              mutableVars = mutableVars,
              identExprs = identExprs.toList
            )
            val stackAllocIds = findStackAllocIds(analysis)
            Some((analysis, stackAllocIds))
          }
      }

    private def findStackAllocIds(analysis: Analysis): Set[Int] = {
      import Consumer._
      import GraphNode._
      import ParentRef._

      val reverseEdges =
        mutable.Map.empty[GraphNode, mutable.Set[GraphNode]]

      def addEdge(src: GraphNode, dst: GraphNode): Unit = {
        val predSet = reverseEdges.getOrElseUpdate(dst, mutable.Set.empty)
        predSet += src
      }

      def asVarTarget(id: Code.Ident): Consumer =
        if (analysis.mutableVars(id)) Escape
        else ToVar(id)

      def consumerOf(expr: Code.Expression): Consumer = {
        var current: Code.Expression = expr
        var done = false
        var result: Consumer = Local

        while (!done) {
          analysis.parentOf.get(Ref(current)) match {
            case None =>
              done = true
              result = Local

            case Some(ParentExpr(parent, role)) =>
              parent match {
                case app @ Code.Apply(_, _) =>
                  role match {
                    case ExprRole.ApplyFn =>
                      current = parent
                    case ExprRole.ApplyArg(_) =>
                      allocInfoOf(app) match {
                        case Some(_) =>
                          allocIdOf(analysis.allocIds, app) match {
                            case Some(id) =>
                              done = true
                              result = ToAlloc(id)
                            case None =>
                              done = true
                              result = Local
                          }
                        case None =>
                          calleeName(app) match {
                            case Some(name) if getterFns(name) =>
                              done = true
                              result = Local
                            case Some(name) if isEscapeBoundaryCall(name) =>
                              done = true
                              result = Escape
                            case _ =>
                              done = true
                              // Without interprocedural escape facts, any non-constructor
                              // call argument is treated as potentially escaping.
                              result = Escape
                          }
                      }
                    case ExprRole.Wrapped =>
                      current = parent
                  }
                case _ =>
                  current = parent
              }

            case Some(ParentStmt(parent, role)) =>
              parent match {
                case Code.Assignment(target, _) =>
                  role match {
                    case StmtRole.AssignTarget =>
                      done = true
                      result = Ignore
                    case StmtRole.AssignValue  =>
                      target match {
                        case id: Code.Ident if analysis.bValueVars(id) =>
                          done = true
                          result = asVarTarget(id)
                        case _ =>
                          done = true
                          result = Escape
                      }
                    case _ =>
                      done = true
                      result = Local
                  }

                case Code.DeclareVar(_, tpe, ident, _) =>
                  role match {
                    case StmtRole.DeclareInit if isBValueType(tpe) =>
                      done = true
                      result = asVarTarget(ident)
                    case StmtRole.DeclareInit =>
                      done = true
                      result = Escape
                    case StmtRole.DeclareArrayValue =>
                      done = true
                      result = Escape
                    case _ =>
                      done = true
                      result = Local
                  }

                case _: Code.DeclareArray =>
                  role match {
                    case StmtRole.DeclareArrayValue =>
                      done = true
                      result = Escape
                    case _ =>
                      done = true
                      result = Local
                  }

                case Code.Return(_) =>
                  role match {
                    case StmtRole.ReturnValue =>
                      done = true
                      result = Escape
                    case _ =>
                      done = true
                      result = Local
                  }

                case _ =>
                  done = true
                  result = Local
              }
          }
        }

        result
      }

      analysis.allocById.iterator.foreach { case (id, _) =>
        analysis.allocExprById.get(id).foreach { allocExpr =>
          consumerOf(allocExpr) match {
            case ToVar(varId)   => addEdge(AllocNode(id), VarNode(varId))
            case ToAlloc(dstId) => addEdge(AllocNode(id), AllocNode(dstId))
            case Escape         => addEdge(AllocNode(id), EscapeNode)
            case Local | Ignore => ()
          }
        }
      }

      analysis.identExprs.foreach { case (expr, ident) =>
        if (analysis.bValueVars(ident)) {
          consumerOf(expr) match {
            case ToVar(dstVar)  => addEdge(VarNode(ident), VarNode(dstVar))
            case ToAlloc(dstId) => addEdge(VarNode(ident), AllocNode(dstId))
            case Escape         => addEdge(VarNode(ident), EscapeNode)
            case Local | Ignore => ()
          }
        }
      }

      val escaping = mutable.Set.empty[GraphNode]
      val seen = mutable.Set.empty[GraphNode]
      val stack = mutable.Stack[GraphNode](EscapeNode)
      while (stack.nonEmpty) {
        val node = stack.pop()
        if (!seen(node)) {
          seen += node
          reverseEdges.get(node).foreach { preds =>
            preds.foreach { pred =>
              escaping += pred
              stack.push(pred)
            }
          }
        }
      }

      analysis.allocById.iterator.collect {
        case (id, meta) if meta.heapLike && !escaping(AllocNode(id)) => id
      }.toSet
    }

    private def stackAllocEnum(
        arity: Int,
        args: List[Code.Expression]
    ): Code.Expression =
      Code.Apply(Code.Ident(s"BSTS_STACK_ALLOC_ENUM$arity"), args)

    private def stackAllocStruct(
        arity: Int,
        args: List[Code.Expression]
    ): Code.Expression =
      Code.Apply(Code.Ident(s"BSTS_STACK_ALLOC_STRUCT$arity"), args)

    private def rewriteExpr(
        expr: Code.Expression,
        analysis: Analysis,
        stackAllocIds: Set[Int]
    ): Code.Expression = {
      val rewrittenChildren: Code.Expression =
        expr match {
          case app @ Code.Apply(fnExpr, args) =>
            val fn1 = rewriteExpr(fnExpr, analysis, stackAllocIds)
            val args1 = args.map(rewriteExpr(_, analysis, stackAllocIds))
            app.copy(fn = fn1, args = args1)
          case cast @ Code.Cast(_, in) =>
            cast.copy(expr = rewriteExpr(in, analysis, stackAllocIds))
          case sel @ Code.Select(target, _) =>
            sel.copy(target = rewriteExpr(target, analysis, stackAllocIds))
          case Code.BinExpr(left, op, right) =>
            Code.BinExpr(
              rewriteExpr(left, analysis, stackAllocIds),
              op,
              rewriteExpr(right, analysis, stackAllocIds)
            )
          case Code.PrefixExpr(op, target) =>
            Code.PrefixExpr(op, rewriteExpr(target, analysis, stackAllocIds))
          case Code.PostfixExpr(target, op) =>
            Code.PostfixExpr(rewriteExpr(target, analysis, stackAllocIds), op)
          case Code.Bracket(target, item) =>
            Code.Bracket(
              rewriteExpr(target, analysis, stackAllocIds),
              rewriteExpr(item, analysis, stackAllocIds)
            )
          case Code.Ternary(cond, whenTrue, whenFalse) =>
            Code.Ternary(
              rewriteExpr(cond, analysis, stackAllocIds),
              rewriteExpr(whenTrue, analysis, stackAllocIds),
              rewriteExpr(whenFalse, analysis, stackAllocIds)
            )
          case id @ Code.Ident(_)      => id
          case i @ Code.IntLiteral(_)  => i
          case s @ Code.StrLiteral(_)  => s
        }

      allocIdOf(analysis.allocIds, expr) match {
        case Some(id) if stackAllocIds(id) =>
          analysis.allocById.get(id) match {
            case Some(allocMeta) =>
              rewrittenChildren match {
                case Code.Apply(_, args) =>
                  allocMeta(args)
                case _ =>
                  rewrittenChildren
              }
            case None =>
              rewrittenChildren
          }
        case _ =>
          rewrittenChildren
      }
    }

    private def rewriteBlock(
        block: Code.Block,
        analysis: Analysis,
        stackAllocIds: Set[Int]
    ): Code.Block =
      block.copy(items = block.items.map(rewriteStatement(_, analysis, stackAllocIds)))

    private def rewriteStatement(
        stmt: Code.Statement,
        analysis: Analysis,
        stackAllocIds: Set[Int]
    ): Code.Statement =
      stmt match {
        case Code.Assignment(target, value) =>
          Code.Assignment(
            rewriteExpr(target, analysis, stackAllocIds),
            rewriteExpr(value, analysis, stackAllocIds)
          )
        case arr @ Code.DeclareArray(tpe, ident, values) =>
          val values1 = values match {
            case Left(size)  => Left(size)
            case Right(exprs) =>
              Right(exprs.map(rewriteExpr(_, analysis, stackAllocIds)))
          }
          arr.copy(values = values1)
        case decl @ Code.DeclareVar(attrs, tpe, ident, value) =>
          decl.copy(
            attrs = attrs,
            tpe = tpe,
            ident = ident,
            value = value.map(rewriteExpr(_, analysis, stackAllocIds))
          )
        case ret @ Code.Return(expr) =>
          ret.copy(expr = expr.map(rewriteExpr(_, analysis, stackAllocIds)))
        case block @ Code.Block(items) =>
          rewriteBlock(block, analysis, stackAllocIds)
        case stmts @ Code.Statements(items) =>
          stmts.copy(
            items = items.map(rewriteStatement(_, analysis, stackAllocIds))
          )
        case ifElse @ Code.IfElse(ifs, elseCond) =>
          val ifs1 = ifs.map { case (cond, body) =>
            (
              rewriteExpr(cond, analysis, stackAllocIds),
              rewriteBlock(body, analysis, stackAllocIds)
            )
          }
          val else1 = elseCond.map(rewriteBlock(_, analysis, stackAllocIds))
          ifElse.copy(ifs = ifs1, elseCond = else1)
        case dw @ Code.DoWhile(block, cond) =>
          dw.copy(
            block = rewriteBlock(block, analysis, stackAllocIds),
            whileCond = rewriteExpr(cond, analysis, stackAllocIds)
          )
        case wh @ Code.While(cond, body) =>
          wh.copy(
            cond = rewriteExpr(cond, analysis, stackAllocIds),
            body = rewriteBlock(body, analysis, stackAllocIds)
          )
        case eff @ Code.Effect(expr) =>
          eff.copy(expr = rewriteExpr(expr, analysis, stackAllocIds))
        case stmt0 @ (_: Code.DeclareFn | _: Code.Include | _: Code.Typedef |
            _: Code.DefineComplex) =>
          stmt0
      }

    def optimize(stmt: Code.Statement): Code.Statement =
      stmt match {
        case fn @ Code.DeclareFn(_, _, _, _, Some(_)) =>
          analyze(fn) match {
            case Some((analysis, stackAllocIds)) if stackAllocIds.nonEmpty =>
              fn.copy(
                value = fn.value.map(block =>
                  rewriteBlock(block, analysis, stackAllocIds)
                )
              )
            case _ =>
              fn
          }
        case _ =>
          stmt
      }
  }

  def apply[S](src: S)(implicit
      CS: CompilationSource[S]
  ): ClangGen[CS.ScopeKey] =
    new ClangGen(CS.namespace(src))
}
