package org.bykn.bosatsu.codegen.clang

import cats.{Eval, Monad, Traverse}
import cats.data.{StateT, EitherT, NonEmptyList, Chain}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.rankn.DataRepr
import org.bykn.bosatsu.{Identifier, Lit, Matchless, PackageName}
import org.bykn.bosatsu.Matchless.Expr
import org.bykn.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc

import cats.syntax.all._

object ClangGen {
  sealed abstract class Error
  object Error {
    case class UnknownValue(pack: PackageName, value: Bindable) extends Error
    case class InvariantViolation(message: String, expr: Expr) extends Error
    case class Unbound(bn: Bindable) extends Error
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
        .flatMap { case (p, vs) =>
          vs.iterator.map { case (b, e) =>
            (p, b) -> (e, Impl.generatedName(p, b)) 
          }  
        }
        .toMap
    
    run(allValues, externals, res)
  }

  private object Impl {
    type AllValues = Map[(PackageName, Bindable), (Expr, Code.Ident)]
    type Externals = Map[(PackageName, Bindable), (Code.Include, Code.Ident)]

    def fullName(p: PackageName, b: Bindable): String =
      p.asString + "/" + b.asString

    def generatedName(p: PackageName, b: Bindable): Code.Ident =
      Code.Ident(Idents.escape("___bsts_g_", fullName(p, b)))

    trait Env {
      import Matchless._

      type T[A]
      implicit val monadImpl: Monad[T]
      def run(pm: AllValues, externals: Externals, t: T[Unit]): Either[Error, Doc]
      def appendStatement(stmt: Code.Statement): T[Unit]
      def error[A](e: => Error): T[A]
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
      def directFn(p: Option[PackageName], b: Bindable): T[Option[Code.Ident]]
      def inTop[A](p: PackageName, bn: Bindable)(ta: T[A]): T[A]
      def staticValueName(p: PackageName, b: Bindable): T[Code.Ident]
      def constructorFn(p: PackageName, b: Bindable): T[Code.Ident]

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
      // and replace any tail calls to nm(args) with assigning args to those values
      def toWhileBody(fnName: Code.Ident, args: NonEmptyList[Code.Param], cond: Code.Ident, result: Code.Ident, body: Code.ValueLike): Code.Block = {
      
        import Code._

        def returnValue(vl: ValueLike): Statement =
          (cond := FalseLit) +
            (result := vl)

        def loop(vl: ValueLike): Option[Statement] =
          vl match {
            case Apply(fn, argHead :: argTail) if fn == fnName =>
              // this is a tail call
              val newArgs = NonEmptyList(argHead, argTail)
              val assigns = args.zipWith(newArgs) {
                case (Param(_, name), value) =>
                  Assignment(name, value)
              }
              Some(Statements(assigns))
            case IfElseValue(c, t, f) =>
              // this can possible have tail calls inside the branches
              (loop(t), loop(f)) match {
                case (Some(t), Some(f)) =>
                  Some(ifThenElse(c, t, f))
                case (None, Some(f)) =>
                  Some(ifThenElse(c, returnValue(t), f))
                case (Some(t), None) =>
                  Some(ifThenElse(c, t, returnValue(f)))
                case (None, None) => None
              }
            case Ternary(c, t, f) => loop(IfElseValue(c, t, f))
            case WithValue(s, vl) => loop(vl).map(s + _)
            case Apply(_, _) | Cast(_, _) | BinExpr(_, _, _) | Bracket(_, _) | Ident(_) |
              IntLiteral(_) | PostfixExpr(_, _) | PrefixExpr(_, _) | Select(_, _) |
              StrLiteral(_) => None
          }

        loop(result) match {
          case Some(stmt) => block(stmt)
          case None =>
            sys.error("invariant violation: could not find tail calls in:" +
              s"toWhileBody(fnName = $fnName, body = $body)")
        }
      }

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

      def equalsChar(expr: Code.Expression, codePoint: Int): Code.Expression =
        expr =:= Code.Ident("BSTS_TO_CHAR")(Code.IntLiteral(codePoint))

      def pv(e: Code.ValueLike): T[Code.ValueLike] = monadImpl.pure(e)

      // The type of this value must be a C _Bool
      def boolToValue(boolExpr: BoolExpr): T[Code.ValueLike] =
        boolExpr match {
          case EqualsLit(expr, lit) =>
            innerToValue(expr).flatMap { vl =>
              lit match {
                case c @ Lit.Chr(_) => vl.onExpr { e => pv(equalsChar(e, c.toCodePoint)) }(newLocalName)
                case Lit.Str(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(Code.Ident("bsts_equals_string"),
                        NonEmptyList(e, litStr :: Nil)
                      )(newLocalName)
                    }
                  }(newLocalName)
                case Lit.Integer(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(Code.Ident("bsts_equals_int"),
                        NonEmptyList(e, litStr :: Nil)
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
              vl.onExpr { expr => pv(fn(expr)) }(newLocalName)  
            }
          case And(e1, e2) =>
            (boolToValue(e1), boolToValue(e2))
              .flatMapN { (a, b) =>
                Code.ValueLike.applyArgs(
                  Code.Ident("BSTS_AND"),
                  NonEmptyList(a, b :: Nil)
                )(newLocalName)
              }
          case CheckVariant(expr, expect, _, _) =>
            innerToValue(expr).flatMap { vl =>
            // this is just get_variant(expr) == expect
              vl.onExpr { expr => pv(Code.Ident("get_variant")(expr) =:= Code.IntLiteral(expect)) }(newLocalName)
            }
          case sl @ SearchList(lst, init, check, leftAcc) =>
            // TODO: ???
            println(s"TODO: implement boolToValue($sl) returning false")
            pv(Code.FalseLit)
          case ms @ MatchString(arg, parts, binds) =>
            // TODO: ???
            println(s"TODO: implement boolToValue($ms) returning false")
            pv(Code.FalseLit)
          case SetMut(LocalAnonMut(idx), expr) =>
            for {
              name <- getAnon(idx)
              vl <- innerToValue(expr)
            } yield (name := vl) +: Code.TrueLit
          case TrueConst => pv(Code.TrueLit)
        }

      // We have to lift functions to the top level and not
      // create any nesting
      def innerFn(fn: FnExpr): T[Code.ValueLike] = 
        if (fn.captures.isEmpty) {
          for {
            ident <- newTopName("lambda")
            stmt <- fnStatement(ident, fn)
            _ <- appendStatement(stmt)
          } yield Code.Ident("STATIC_PUREFN")(ident)
        }
        else {
          // we create the function, then we allocate
          // values for the capture
          // alloc_closure<n>(capLen, captures, fnName)
          for {
            ident <- newTopName("closure")
            stmt <- fnStatement(ident, fn)
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

      def literal(lit: Lit): T[Code.ValueLike] =
        lit match {
          case c @ Lit.Chr(_) =>
            // encoded as integers in pure values
            pv(Code.Ident("BSTS_TO_CHAR")(Code.IntLiteral(c.toCodePoint)))
          case Lit.Integer(toBigInteger) =>
            try {
              val iv = toBigInteger.intValueExact()
              pv(Code.Ident("bsts_integer_from_int")(Code.IntLiteral(iv)))
            }
            catch {
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
                //call:
                // bsts_integer_from_words_copy(_Bool is_pos, size_t size, int32_t* words);
                newLocalName("int").map { ident =>
                  Code.DeclareArray(Code.TypeIdent.UInt32, ident, Right(lits)) +:
                    Code.Ident("bsts_integer_from_words_copy")(
                      if (isPos) Code.TrueLit else Code.FalseLit,
                      Code.IntLiteral(lits.length),
                      ident
                    )
                }
            }

          case Lit.Str(toStr) =>
            // convert to utf8 and then to a literal array of bytes
            val bytes = toStr.getBytes(StandardCharsets.UTF_8)
            if (bytes.forall(_.toInt != 0)) {
              // just send the utf8 bytes as a string to C
              pv(
                Code.Ident("BSTS_NULL_TERM_STATIC_STR")(Code.StrLiteral(
                  new String(bytes.map(_.toChar))
                ))
              )
            }
            else {
              // We have some null bytes, we have to encode the length
              val lits =
                bytes.iterator.map { byte =>
                  Code.IntLiteral(byte.toInt & 0xff)
                }.toList
              //call:
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

      def innerApp(app: App): T[Code.ValueLike] =
        app match {
          case App(LocalOrGlobal(optPack, fnName), args) =>
            directFn(optPack, fnName).flatMap {
              case Some(ident) =>
                // directly invoke instead of by treating them like lambdas
                args.traverse(innerToValue(_)).flatMap { argsVL =>
                  Code.ValueLike.applyArgs(ident, argsVL)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (maybeGlobalIdent(optPack, fnName), args.traverse(innerToValue(_))).flatMapN { (fnVL, argsVL) =>
                  // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                  // but since these are ValueLike, we need to handle more carefully
                  val fnSize = argsVL.length
                  val callFn = Code.Ident(s"call_fn$fnSize")
                  Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(newLocalName)
                }
            }
          case App(MakeEnum(variant, arity, _), args) =>
            // to type check, we know that the arity must have the same length as args
            args.traverse(innerToValue).flatMap { argsVL =>
              val tag = Code.IntLiteral(variant)
              Code.ValueLike.applyArgs(Code.Ident(s"alloc_enum$arity"), tag :: argsVL)(newLocalName)
            }
          case App(MakeStruct(arity), args) =>
            if (arity == 1) {
              // this is a new-type, just return the arg
              innerToValue(args.head)
            }
            else {
              // to type check, we know that the arity must have the same length as args
              args.traverse(innerToValue).flatMap { argsVL =>
                Code.ValueLike.applyArgs(Code.Ident(s"alloc_struct$arity"), argsVL)(newLocalName)
              }
          }
          case App(SuccNat, args) =>
            innerToValue(args.head).flatMap { arg =>
              Code.ValueLike.applyArgs(Code.Ident("BSTS_NAT_SUCC"), NonEmptyList.one(arg))(newLocalName)
            }
          case App(fn, args) =>
            (innerToValue(fn), args.traverse(innerToValue(_))).flatMapN { (fnVL, argsVL) =>
              // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
              // but since these are ValueLike, we need to handle more carefully
              val fnSize = argsVL.length
              val callFn = Code.Ident(s"call_fn$fnSize")
              Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(newLocalName)
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
                  pv(Code.Ident("STATIC_PUREFN")(nm))
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
                  pv(Code.Ident("STATIC_PUREFN")(nm))
                case None =>
                  getBinding(arg).widen
              }
          case ClosureSlot(idx) =>
            // we must be inside a closure function, so we should have a slots argument to access
            pv(slotsArgName.bracket(Code.IntLiteral(BigInt(idx))))
          case LocalAnon(ident) => getAnon(ident).widen
          case LocalAnonMut(ident) => getAnon(ident).widen
          case LetMut(LocalAnonMut(m), span) =>
            bindAnon(m) {
              for {
                ident <- getAnon(m)
                decl = Code.DeclareVar(Nil, Code.TypeIdent.BValue, ident, None)
                res <- innerToValue(span)
              } yield decl +: res
            }
          case Literal(lit) => literal(lit)
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
              v.onExpr(e => pv(Code.Ident("get_enum_index")(e, Code.IntLiteral(index))))(newLocalName)
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
                  pv(Code.Ident("get_struct_index")(e, Code.IntLiteral(index)))
                }(newLocalName)
              }
            }
          case makeEnum @ MakeEnum(variant, arity, _) =>
            // this is a closure over variant, we rewrite this
            NonEmptyList.fromList((0 until arity).toList) match {
              case None => pv(Code.Ident("alloc_enum0")(Code.IntLiteral(variant)))
              case Some(args) =>
                val named = args.map { idx => Identifier.Name(s"arg$idx") }
                // This relies on optimizing App(MakeEnum, _) otherwise
                // it creates an infinite loop.
                // Also, this we should cache creation of Lambda/Closure values
                innerToValue(Lambda(Nil, None, named, App(makeEnum, named.map(Local(_)))))
            }
          case MakeStruct(arity) =>
            pv {
              if (arity == 0) Code.Ident("PURE_VALUE_TAG")
              else {
                val allocStructFn = s"alloc_struct$arity"
                Code.Ident("STATIC_PUREFN")(Code.Ident(allocStructFn))
              }
            }
          case ZeroNat =>
            pv(Code.Ident("BSTS_NAT_0"))
          case SuccNat =>
            val arg = Identifier.Name("arg0")
            // This relies on optimizing App(SuccNat, _) otherwise
            // it creates an infinite loop.
            // Also, this we should cache creation of Lambda/Closure values
            innerToValue(Lambda(Nil, None, NonEmptyList.one(arg),
              App(SuccNat, NonEmptyList.one(Local(arg)))))
          case PrevNat(of) =>
            innerToValue(of).flatMap { argVL =>
              Code.ValueLike.applyArgs(
                Code.Ident("BSTS_NAT_PREV"),
                NonEmptyList.one(argVL)
              )(newLocalName)
            }
        }

      def fnStatement(fnName: Code.Ident, fn: FnExpr): T[Code.Statement] =
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
                  argParams <- args.traverse { b =>
                    getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
                  }
                  whileBody = toWhileBody(fnName, argParams, cond = cond, result = res, body = bodyVL)
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
              fnName <- globalIdent(p, b)
              stmt <- fnStatement(fnName, fn)
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
      def impl: Env = {
        def catsMonad[S]: Monad[StateT[EitherT[Eval, Error, *], S, *]] = implicitly

        new Env {
          case class State(
            allValues: AllValues,
            externals: Externals,
            includeSet: Set[Code.Include],
            includes: Chain[Code.Include],
            stmts: Chain[Code.Statement],
            currentTop: Option[(PackageName, Bindable)],
            binds: Map[Bindable, NonEmptyList[Either[(Code.Ident, Int), Int]]],
            counter: Long
          ) {
            def finalFile: Doc =
              Doc.intercalate(Doc.line, includes.iterator.map(Code.toDoc(_)).toList) +
                Doc.intercalate(Doc.line + Doc.line, stmts.iterator.map(Code.toDoc(_)).toList)
          }

          object State {
            def init(allValues: AllValues, externals: Externals): State =
              State(allValues, externals, Set.empty, Chain.empty, Chain.empty,
                None, Map.empty, 0L
              )
          }

          type T[A] = StateT[EitherT[Eval, Error, *], State, A]

          implicit val monadImpl: Monad[T] = catsMonad[State]

          def run(pm: AllValues, externals: Externals, t: T[Unit]): Either[Error, Doc] =
            t.run(State.init(pm, externals))
              .value // get the value out of the EitherT
              .value // evaluate the Eval
              .map(_._1.finalFile)

          def appendStatement(stmt: Code.Statement): T[Unit] =
            StateT.modify(s => s.copy(stmts = s.stmts :+ stmt))

          def errorRes[A](e: => Error): EitherT[Eval, Error, A] =
              EitherT[Eval, Error, A](Eval.later(Left(e)))

          def error[A](e: => Error): T[A] =
            StateT(_ => errorRes(e))

          def result[A](s: State, a: A): EitherT[Eval, Error, (State, A)] =
            EitherT[Eval, Error, (State, A)](
              Eval.now(Right((s, a)))
            )

          def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident] =
            StateT { s =>
              val key = (pn, bn)
              s.externals.get(key) match {
                case Some((incl, ident)) =>
                  val withIncl =
                    if (s.includeSet(incl)) s
                    else s.copy(includeSet = s.includeSet + incl, includes = s.includes :+ incl)

                  result(withIncl, ident)
                case None =>
                  s.allValues.get(key) match {
                    case Some((_, ident)) => result(s, ident)
                    case None => errorRes(Error.UnknownValue(pn, bn))
                  }
              }
            }

          def bind[A](bn: Bindable)(in: T[A]): T[A] = {
            val init: T[Unit] = StateT { s =>
              val v = s.binds.get(bn) match {
                case None => NonEmptyList.one(Right(0))
                case Some(items @ NonEmptyList(Right(idx), _)) =>
                  Right(idx + 1) :: items
                case Some(items @ NonEmptyList(Left((_, idx)), _)) =>
                  Right(idx + 1) :: items
              }  
              result(s.copy(binds = s.binds.updated(bn, v)), ())
            }

            val uninit: T[Unit] = StateT { s =>
              s.binds.get(bn) match {
                case Some(NonEmptyList(_, tail)) =>
                  val s1 = NonEmptyList.fromList(tail) match {
                    case None =>
                      s.copy(binds = s.binds - bn)
                    case Some(prior) =>
                      s.copy(binds = s.binds.updated(bn, prior))
                  }
                  result(s1, ())
                case None => sys.error(s"bindable $bn no longer in $s")
              }  
            }

            for {
              _ <- init
              a <- in 
              _ <- uninit
            } yield a
          }
          def getBinding(bn: Bindable): T[Code.Ident] =
            StateT { s =>
              s.binds.get(bn) match {
                case Some(stack) =>
                  stack.head match {
                    case Right(idx) =>
                      result(s, Code.Ident(Idents.escape("__bsts_b_", bn.asString + idx.toString)))
                    case Left((ident, _)) =>
                      result(s, ident)
                  }
                case None => errorRes(Error.Unbound(bn))
              }  
            }
          def bindAnon[A](idx: Long)(in: T[A]): T[A] =
            // in the future we see the scope of the binding which matters for GC, but here
            // we don't care
            in

          def getAnon(idx: Long): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("__bsts_a_", idx.toString)))

          // a recursive function needs to remap the Bindable to the top-level mangling
          def recursiveName[A](fnName: Code.Ident, bn: Bindable)(in: T[A]): T[A] = {
            val init: T[Unit] = StateT { s =>
              val v = s.binds.get(bn) match {
                case None => NonEmptyList.one(Left((fnName, -1)))
                case Some(items @ NonEmptyList(Right(idx), _)) =>
                  Left((fnName, idx)) :: items
                case Some(items @ NonEmptyList(Left((_, idx)), _)) =>
                  Left((fnName, idx)) :: items
              }  
              result(s.copy(binds = s.binds.updated(bn, v)), ())
            }

            val uninit: T[Unit] = StateT { s =>
              s.binds.get(bn) match {
                case Some(NonEmptyList(_, tail)) =>
                  val s1 = NonEmptyList.fromList(tail) match {
                    case None =>
                      s.copy(binds = s.binds - bn)
                    case Some(prior) =>
                      s.copy(binds = s.binds.updated(bn, prior))
                  }
                  result(s1, ())
                case None => sys.error(s"bindable $bn no longer in $s")
              }  
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
          def directFn(p: Option[PackageName], b: Bindable): T[Option[Code.Ident]] =
            p match {
              case None =>
                StateT { s =>
                  s.binds.get(b) match {
                    case Some(NonEmptyList(Left((c, _)), _)) =>
                      result(s, Some(c))
                    case _ =>
                      result(s, None)
                  } 
                }
              case Some(pack) =>
                StateT { s =>
                  s.allValues.get((pack, b)) match {
                    case Some((_: Matchless.FnExpr, ident)) =>
                      result(s, Some(ident))
                    case _ => result(s, None)
                  }  
                }
            }
          def inTop[A](p: PackageName, bn: Bindable)(ta: T[A]): T[A] =
            for {
              _ <- StateT { (s: State) => result(s.copy(currentTop = Some((p, bn))), ())}
              a <- ta
              _ <- StateT { (s: State) => result(s.copy(currentTop = None), ()) }
            } yield a

          def staticValueName(p: PackageName, b: Bindable): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("___bsts_s_", fullName(p, b))))
          def constructorFn(p: PackageName, b: Bindable): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("___bsts_c_", fullName(p, b))))

          def renderMain(p: PackageName, b: Bindable, evalInc: Code.Include, evalFn: Code.Ident): T[Unit] =
            // TODO ???
            monadImpl.unit
        }
      }
    }
  }
}