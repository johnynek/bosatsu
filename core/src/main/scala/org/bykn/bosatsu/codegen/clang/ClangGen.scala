package org.bykn.bosatsu.codegen.clang

import cats.{Eval, Monad, Traverse}
import cats.data.{StateT, EitherT, NonEmptyList, Chain}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.rankn.{DataRepr, Type}
import org.bykn.bosatsu.{Identifier, Lit, Matchless, Predef, PackageName, PackageMap}
import org.bykn.bosatsu.pattern.StrPart
import org.bykn.bosatsu.Matchless.Expr
import org.bykn.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc
import scala.collection.immutable.{SortedMap, SortedSet}

import cats.syntax.all._

object ClangGen {
  sealed abstract class Error {
    // TODO: implement this in a nice way
    def display: Doc = Doc.text(this.toString)
  }

  object Error {
    case class UnknownValue(pack: PackageName, value: Bindable) extends Error
    case class InvariantViolation(message: String) extends Error
    case class Unbound(bn: Bindable, inside: Option[(PackageName, Bindable)]) extends Error
    case class ExpectedStaticString(str: String) extends Error
  }

  trait ExternalResolver {
    def names: SortedMap[PackageName, SortedSet[Bindable]]
    def apply(p: PackageName, b: Bindable): Option[(Code.Include, Code.Ident, Int)]

    final def generateExternalsStub: SortedMap[String, Doc] = {
      val includes = Code.Include.quote("bosatsu_runtime.h") :: Nil

      def toStmt(cIdent: Code.Ident, arity: Int): Code.Statement = {
        val args = Idents.allSimpleIdents.take(arity).map { nm =>
          Code.Param(Code.TypeIdent.BValue, Code.Ident(nm))
        }
        Code.DeclareFn(Nil, Code.TypeIdent.BValue, cIdent, args.toList, None)
      }

      val line2 = Doc.hardLine + Doc.hardLine
      val includeRuntime = Doc.intercalate(Doc.hardLine, includes.map(Code.toDoc))

      SortedMap.empty[String, Doc] ++ names
        .iterator
        .flatMap { case (p, binds) =>

          val fns = binds.iterator.flatMap { n =>
            apply(p, n)
          }
          .map { case (i, n, arity) =>
            i.filename -> Code.toDoc(toStmt(n, arity))
          }
          .toList
          .groupByNel(_._1)


          fns.iterator.map { case (incl, nelInner) =>
            incl -> (
              includeRuntime +
                line2 +
                Doc.intercalate(line2, nelInner.toList.map(_._2)))
          }
        }
    }

  }

  object ExternalResolver {
    def stdExtFileName(pn: PackageName): String =
      s"${Idents.escape("bosatsu_ext_", pn.asString)}.h"

    def stdExternals(pm: PackageMap.Typed[Any]): ExternalResolver = {

      def tpeArity(t: Type): Int =
        t match {
          case Type.Fun.MaybeQuant(_, args, _) => args.length
          case _ => 0
        }

      val allExt = pm.allExternals
      val extMap = allExt
        .iterator
        .map { case (p, vs) =>
          val fileName = ExternalResolver.stdExtFileName(p)

          val fns = vs.iterator.map { case (n, tpe) =>
            val cIdent = generatedName(p, n)
            n -> (cIdent, tpeArity(tpe))
          }
          .toMap

          p -> (Code.Include.quote(fileName), fns)
        }
        .toMap

      new ExternalResolver {
        lazy val names: SortedMap[PackageName, SortedSet[Bindable]] =
          allExt.iterator.map { case (p, vs) =>
            p -> vs.iterator.map { case (b, _) => b }.to(SortedSet)
          }
          .to(SortedMap)

        def apply(p: PackageName, b: Bindable): Option[(Code.Include, Code.Ident, Int)] =
          for {
            (include, inner) <- extMap.get(p)
            (ident, arity) <- inner.get(b)
          } yield (include, ident, arity)
      }
    }

    val FromJvmExternals: ExternalResolver =
      new ExternalResolver {
        val predef_c = Code.Include.quote(stdExtFileName(PackageName.PredefName))

        def predef(s: String, arity: Int) =
          (PackageName.PredefName -> Identifier.Name(s)) -> (predef_c,
            ClangGen.generatedName(PackageName.PredefName, Identifier.Name(s)), arity)

        val ext = Predef
          .jvmExternals.toMap.iterator.map { case ((_, n), ffi) =>
            predef(n, ffi.arity)
          }
          .toMap[(PackageName, Identifier), (Code.Include, Code.Ident, Int)]

        lazy val names: SortedMap[PackageName, SortedSet[Bindable]] = {
          val sm = Predef
            .jvmExternals.toMap.iterator.map { case (pn, _) => pn }
            .toList
            .groupByNel(_._1)

          sm.map { case (k, vs) =>
            (k, vs.toList.iterator.map { case (_, n) => Identifier.Name(n) }.to(SortedSet))
          }
        }
        def apply(p: PackageName, b: Bindable) = ext.get((p, b))
      }
  }

  private def renderDeps(
      env: Impl.Env,
      sortedEnv: Vector[NonEmptyList[(PackageName, List[(Bindable, Expr)])]]
  ): env.T[Unit] = {
    import env._

    Traverse[Vector].compose[NonEmptyList]
      .traverse_(sortedEnv) { case (pn, values) =>
        values.traverse_ { case (bindable, expr) =>
          renderTop(pn, bindable, expr)      
        }
      }
  }

  def renderMain(
      sortedEnv: Vector[NonEmptyList[(PackageName, List[(Bindable, Expr)])]],
      externals: ExternalResolver,
      value: (PackageName, Bindable)
  ): Either[Error, Doc] = {
    val env = Impl.Env.impl
    import env.monadImpl

    val res = renderDeps(env, sortedEnv) *> env.renderMain(value._1, value._2)

    val allValues: Impl.AllValues =
      sortedEnv
        .iterator.flatMap(_.iterator)
        .flatMap { case (p, vs) =>
          vs.iterator.map { case (b, e) =>
            (p, b) -> (e, generatedName(p, b)) 
          }  
        }
        .toMap
    
    env.run(allValues, externals, res)
  }

  def renderTests(
      sortedEnv: Vector[NonEmptyList[(PackageName, List[(Bindable, Expr)])]],
      externals: ExternalResolver,
      values: List[(PackageName, Bindable)]
  ): Either[Error, Doc] = {
    val env = Impl.Env.impl
    import env.monadImpl

    val res = renderDeps(env, sortedEnv) *> env.renderTests(values)

    val allValues: Impl.AllValues =
      sortedEnv
        .iterator.flatMap(_.iterator)
        .flatMap { case (p, vs) =>
          vs.iterator.map { case (b, e) =>
            (p, b) -> (e, generatedName(p, b)) 
          }  
        }
        .toMap
    
    env.run(allValues, externals, res)
  }

  private def fullName(p: PackageName, b: Bindable): String =
    p.asString + "/" + b.asString

  def generatedName(p: PackageName, b: Bindable): Code.Ident =
    Code.Ident(Idents.escape("___bsts_g_", fullName(p, b)))

  private object Impl {
    type AllValues = Map[(PackageName, Bindable), (Expr, Code.Ident)]

    sealed abstract class MemState
    object MemState {
      sealed abstract class NotCounted extends MemState

      sealed abstract class Lifetime
      object Lifetime {
        // lasts forever
        case object Static extends Lifetime
        // lasts as long as both last
        case class Intersection(left: Lifetime, right: Lifetime) extends Lifetime
        case class FromRoot(rootIdent: Long) extends Lifetime
        // last as long as the current closure exists
        case object ClosureVariable extends Lifetime
      }
      case object Static extends NotCounted
      case class Ref(lifetime: Lifetime) extends NotCounted

      case class Owned(rootIdent: Long) extends MemState
    }

    trait Env {
      import Matchless._

      type T[A]
      implicit val monadImpl: Monad[T]
      def run(pm: AllValues, externals: ExternalResolver, t: T[Unit]): Either[Error, Doc]
      def appendStatement(stmt: Code.Statement): T[Unit]
      def error[A](e: => Error): T[A]
      def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident]
      def bind[A](bn: Bindable)(in: T[A]): T[A]
      def getBinding(bn: Bindable): T[Code.Ident]
      def bindAnon[A](idx: Long)(in: T[A]): T[A]
      def getAnon(idx: Long): T[Code.Ident]
      // a recursive function needs to remap the Bindable to the top-level mangling
      def recursiveName[A](fnName: Code.Ident, bn: Bindable, isClosure: Boolean, arity: Int)(in: T[A]): T[A]
      // used for temporary variables of type BValue
      def newLocalName(tag: String): T[Code.Ident]
      def newTopName(tag: String): T[Code.Ident]
      def directFn(p: PackageName, b: Bindable): T[Option[(Code.Ident, Int)]]
      def directFn(b: Bindable): T[Option[(Code.Ident, Boolean, Int)]]
      def inTop[A](p: PackageName, bn: Bindable)(ta: T[A]): T[A]
      def currentTop: T[Option[(PackageName, Bindable)]]
      def staticValueName(p: PackageName, b: Bindable): T[Code.Ident]
      def constructorFn(p: PackageName, b: Bindable): T[Code.Ident]

      def cachedIdent(key: Expr)(value: => T[Code.Ident]): T[Code.Ident]

      /////////////////////////////////////////
      // the below are memory management related functions
      /////////////////////////////////////////
      def scope[A](ta: T[A]): T[(A, Map[Long, Int])]
      // sometimes we jump back out to a top scope when rendering closures and lambdas
      // return the final root counts so we can repair them
      def topScope[A](ta: T[A]): T[(A, Map[Long, Int])]
      def passToFn(rootId: Long): T[Unit]
      // creates a new owned ident that points to root memory with the given id
      def createOwned(ident: Code.Ident, root: Long): T[Unit]
      def bindMemory[A](name: Code.Ident, ms: MemState, vl: Code.ValueLike)(in: T[A]): T[A]
      def memState(ident: Code.Ident): T[MemState]
      def nextRootId: T[Long]
      // this has to make sure the current scope has an additional count so it
      // is safe to borrow
      def borrowLifetime(lt: MemState.Lifetime): T[Unit]
      // when we have an if/else we need to update two separate scopes to have the same
      // static memory behavior, this may involve adding counts or releases
      def mergeValues(
        left: T[((MemState, Code.ValueLike), Map[Long, Int])],
        right: T[((MemState, Code.ValueLike), Map[Long, Int])]): T[(MemState, Code.ValueLike, Code.ValueLike)]


      /////////////////////////////////////////
      // the below are independent of the environment implementation
      /////////////////////////////////////////
    
      // This name has to be impossible to give out for any other purpose
      val slotsArgName: Code.Ident = Code.Ident("__bstsi_slot")

      // assign any results to result and set the condition to false
      // and replace any tail calls to nm(args) with assigning args to those values
      def toWhileBody(fnName: Code.Ident, argTemp: NonEmptyList[(Code.Param, Code.Ident)], isClosure: Boolean, cond: Code.Ident, result: Code.Ident, body: Code.ValueLike): Code.Block = {
      
        import Code._

        def returnValue(vl: ValueLike): Statement =
          (cond := FalseLit) +
            (result := vl)

        def loop(vl: ValueLike): Option[Statement] =
          vl match {
            case Apply(fn, appArgs) if fn == fnName =>
              // this is a tail call
              val newArgsList =
                if (isClosure) appArgs.tail
                else appArgs
              // we know the length of appArgs must match args or the code wouldn't have compiled
              // we have to first assign to the temp variables, and then assign the temp variables
              // to the results to make sure we don't have any data dependency issues with the values;
              val tmpAssigns = argTemp
                .iterator
                .zip(newArgsList.iterator)
                .flatMap { case ((Param(_, name), tmp), value) =>
                  if (name != value)
                    // don't create self assignments
                    Iterator.single((Assignment(tmp, value), Assignment(name, tmp)))
                  else
                    Iterator.empty
                }
                .toList

              // there must be at least one assignment
              val assignNEL = NonEmptyList.fromListUnsafe(
                tmpAssigns.map(_._1) ::: tmpAssigns.map(_._2)
              )
              Some(Statements(assignNEL))
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

        loop(body) match {
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

      def equalsChar(expr: Code.Expression, codePoint: Int): Code.Expression =
        Code.Ident("bsts_char_code_point_from_value")(expr) =:= Code.IntLiteral(codePoint)

      def pure[A](a: A): T[A] = monadImpl.pure(a)
      def pv(e: Code.ValueLike): T[Code.ValueLike] = pure(e)

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
                // we can only run the statements in r if le is true,
                // since boolean expressions are where the side effects are.
                //
                // and(x, y) == if x: y else: False
                pv(Code.IfElseValue(le, r, Code.FalseLit))
            }
          case Code.WithValue(sl, sv) => andCode(sv, r).map(sl +: _)
          case ife @ Code.IfElseValue(c, t, f) if ife.returnsBool || r.isInstanceOf[Code.Expression] =>
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

      // The type of this value must be a C _Bool
      def boolToValue(boolExpr: BoolExpr): T[Code.ValueLike] =
        boolExpr match {
          case EqualsLit(expr, lit) =>
            innerRef(expr).flatMap { case (lt, vl) =>
              lit match {
                case c @ Lit.Chr(_) =>
                  // chars are values
                  vl.onExpr { e => pv(equalsChar(e, c.toCodePoint)) }(newLocalName)
                case s @ Lit.Str(_) =>
                  vl.onExpr { e =>
                    for {
                      sv <- innerRef(Literal(s))
                      (sLt, litStr) = sv
                      ap <- Code.ValueLike.applyArgs(Code.Ident("bsts_string_equals"),
                          NonEmptyList(e, litStr :: Nil)
                        )(newLocalName)
                      _ <- borrowLifetime(MemState.Lifetime.Intersection(lt, sLt))
                    } yield ap
                  }(newLocalName)
                case i @ Lit.Integer(_) =>
                  vl.onExpr { e =>
                    for {
                      sv <- innerRef(Literal(i))
                      (sLt, litInt) = sv
                      ap <- Code.ValueLike.applyArgs(Code.Ident("bsts_integer_equals"),
                          NonEmptyList(e, litInt :: Nil)
                        )(newLocalName)
                      _ <- borrowLifetime(MemState.Lifetime.Intersection(lt, sLt))
                    } yield ap
                  }(newLocalName)
                }
              }  
          case EqualsNat(expr, nat) =>
            val fn = nat match {
              case DataRepr.ZeroNat => Code.Ident("BSTS_NAT_IS_0")
              case DataRepr.SuccNat => Code.Ident("BSTS_NAT_GT_0")
            }
            innerRef(expr).flatMap { case (_, vl) =>
              // nats are always values can ignore lifetime
              vl.onExpr { expr => pv(fn(expr)) }(newLocalName)  
            }
          case And(e1, e2) =>
            (boolToValue(e1), boolToValue(e2))
              .flatMapN { (a, b) => andCode(a, b) }

          case CheckVariant(expr, expect, _, _) =>
            innerRef(expr).flatMap { case (elt, vl) =>
            // this is just get_variant(expr) == expect
              vl.onExpr { expr => borrowLifetime(elt).as(
                (Code.Ident("get_variant")(expr) =:= Code.IntLiteral(expect)): Code.ValueLike
              ) }(newLocalName)
            }
          case SearchList(lst, init, check, leftAcc) =>
            (getAnon(lst.ident), innerRef(init))
              .flatMapN { case (lst, (vlt, initV)) =>
                // TODO use vlt ???
                searchList(lst, initV, boolToValue(check), leftAcc)
              }
          case MatchString(arg, parts, binds, mustMatch) =>
            (
              // TODO: maybe this should be owned so tail could avoid copies on count == 1
              innerRef(arg),
              binds.traverse { case LocalAnonMut(m) => getAnon(m) }
            ).flatMapN { case ((argLt, strVL), binds) =>
              // TODO use argLt ???
              strVL.onExpr { arg => matchString(arg, parts, binds, mustMatch) }(newLocalName)
            }
          case SetMut(LocalAnonMut(idx), expr) =>
            for {
              name <- getAnon(idx)
              msVl <- innerToValue(expr)
              (ms, vl) = msVl
              stmt <- assignMut(name, vl, ms)
            } yield stmt +: Code.TrueLit
          case TrueConst => pv(Code.TrueLit)
        }

      object StringApi {
        import Code.{Expression}
        private val prefix = "bsts_string_"
        def fn(nm: String): Expression = Code.Ident(prefix + nm)

        // string -> int
        def utf8ByteLength(e: Expression): Expression =
          fn("utf8_len")(e)

        // (string, int) -> int
        def codePointBytes(str: Expression, byteOffset: Expression): Expression =
          fn("code_point_bytes")(str, byteOffset)

        // (string, int) -> char
        def getCharAt(str: Expression, byteOffset: Expression): Expression = 
          fn("char_at")(str, byteOffset)

        // (string, int) -> string
        def substringTail(str: Expression, byteOffset: Expression): Expression =
          fn("substring_tail")(str, byteOffset)

        // (string, int, int) -> string
        def substring(str: Expression, startOffset: Expression, endOff: Expression): Expression =
          fn("substring")(str, startOffset, endOff)

        // return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
        // (string, string, int) -> int
        def find(haystack: Expression, needle: Expression, byteOffset: Expression): Expression =
          fn("find")(haystack, needle, byteOffset)

        // basically python src.startswith(expected, _) but with utf8 byte offsets
        // (string, int, string) -> _Bool
        def matchesAt(src: Expression, byteOffset: Expression, expected: Expression): Expression =
          find(src, expected, byteOffset) =:= byteOffset

        def staticString(s: String): T[Code.StrLiteral] = {
          // convert to utf8 and then to a literal array of bytes
          val bytes = s.getBytes(StandardCharsets.UTF_8)
          if (bytes.forall(_.toInt != 0)) {
            // just send the utf8 bytes as a string to C
            pure(Code.StrLiteral(new String(bytes.map(_.toChar))))
          }
          else {
            error(Error.ExpectedStaticString(s))
          }
        }

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

      }

      def matchString(
          strEx: Code.Expression,
          pat: List[StrPart],
          binds: List[Code.Ident],
          mustMatch: Boolean
      ): T[Code.ValueLike] = {
        import StrPart.{LitStr, Glob, CharPart}
        val bindArray = binds.toArray
        // return a value like expression that contains the boolean result
        // and assigns all the bindings along the way
        def loop(
            knownOffset: Option[Int],
            offsetIdent: Code.Ident,
            pat: List[StrPart],
            next: Int,
            mustMatch: Boolean
        ): T[Code.ValueLike] =
          pat match {
            case _ if mustMatch && next == bindArray.length =>
              // we have to match and we've captured everything
              pv(Code.TrueLit)
            case Nil =>
              // offset == str.length
              pv(
                if (mustMatch) Code.TrueLit
                else (offsetIdent =:= StringApi.utf8ByteLength(strEx))
              )
            case LitStr(expect) :: tail =>
              // val len = expect.length
              // str.regionMatches(offset, expect, 0, len) && loop(offset + len, tail, next)
              //
              // strEx.startswith(expect, offsetIdent)
              // note: a literal string can never be a total match, so mustMatch is false
              val utf8bytes = expect.getBytes(StandardCharsets.UTF_8).length
              for {
                str <- StringApi.fromString(expect)
                loopRes <- loop(knownOffset.map(_ + utf8bytes), offsetIdent, tail, next, mustMatch = false)
                regionMatches <- str.onExpr { s => pv(StringApi.matchesAt(strEx, offsetIdent, s)) }(newLocalName)
                rest = (offsetIdent := offsetIdent + Code.IntLiteral(utf8bytes)) +: (loopRes)
                bothMatch <- andCode(regionMatches, rest)
              } yield bothMatch
            case (c: CharPart) :: tail =>
              val matches =
                if (mustMatch) Code.TrueLit
                else {
                  offsetIdent :< StringApi.utf8ByteLength(strEx)
                }
              // the character at the current byte offset is how many bytes?
              val n1 = if (c.capture) (next + 1) else next
              val updateOffset =
                offsetIdent := offsetIdent + StringApi.codePointBytes(strEx, offsetIdent)

              val stmtT =
                if (c.capture) {
                  // b = str[offset]
                  val b = bindArray(next)

                  // chars are values, we don't need to increment them
                  assignMut(b, StringApi.getCharAt(strEx, offsetIdent), MemState.Static)
                    .map { assign =>
                      Code
                        .Statements(
                          assign,
                          updateOffset
                        ) +: Code.TrueLit
                    }

                } else {
                  pure(updateOffset +: Code.TrueLit)
                }
              for {
                // we don't know how many bytes this character took, so
                // we have lost track of the total offset
                stmt <- stmtT
                tailRes <- loop(knownOffset = None, offsetIdent, tail, n1, mustMatch)
                and2 <- andCode(stmt, tailRes)
                and1 <- andCode(matches, and2)
              } yield and1
            case (h: Glob) :: tail =>
              tail match {
                case Nil =>
                  // we capture all the rest
                  if (h.capture) {
                    // b = str[offset:]
                    val b = bindArray(next)
                    for {
                      root <- nextRootId
                      stmt <- assignMut(b, StringApi.substringTail(strEx, offsetIdent), MemState.Owned(root))
                    } yield (stmt +: Code.TrueLit)
                  } else pure(Code.TrueLit)
                case LitStr(expect) :: tail2 =>
                  // here we have to make a loop
                  // searching for expect, and then see if we
                  // can match the rest of the pattern
                  val next1 = if (h.capture) next + 1 else next

                  /*
                   * this is the scala code for the below
                   * it is in MatchlessToValue but left here
                   * as an aid to read the code below
                   *
                  var start = offset
                  var result = false
                  while (start >= 0) {
                    val candidate = str.indexOf(expect, start)
                    if (candidate >= 0) {
                      // we have to skip the current expect string
                      val candidateOffset = candidate + expect.lenth
                      val check1 = loop(candidateOffset, tail2, next1)
                      if (check1) {
                        // this was a match, write into next if needed
                        if (h.capture) {
                          results(next) = str.substring(offset, candidate)
                        }
                        result = true
                        start = -1
                      }
                      else {
                        // we couldn't match here, try just after candidate
                        start = candidate + 1
                      }
                    }
                    else {
                      // no more candidates
                      start = -1
                    }
                  }
                  result
                   */
                  (
                    newLocalName("start"), // int
                    newLocalName("result"), // bool
                    newLocalName("candidate"), // int
                    newLocalName("cand_offset") // int
                  ).flatMapN { (start, result, candidate, candOffset) =>
                    // TODO we have to declare these new names

                    // note, a literal prefix can never be a total match
                    // also, note, since we have a glob on the left we don't know the
                    // offset the tail is matching at
                    val searchEnv = loop(knownOffset = None, candOffset, tail2, next1, mustMatch = false)

                    def onSearch(search: Code.ValueLike): T[Code.Statement] =
                      search.exprToStatement { search =>
                        val matchCase = {
                          // we have matched
                          val after = Code.Statements(
                            result := Code.TrueLit,
                            start := Code.IntLiteral(-1)
                          )

                          if (h.capture) {
                            val b = bindArray(next)
                            for {
                              root <- nextRootId
                              stmt <- assignMut(b, StringApi.substring(strEx, offsetIdent, candidate), MemState.Owned(root))
                            } yield (stmt + after)
                          }
                          else pure(after)
                        }
                        matchCase.map { m =>
                          Code.ifThenElse(search,
                            m,
                            // we couldn't match at start, advance just after the
                            // candidate
                            start := candidate + Code.IntLiteral.One
                          )
                        }
                    }(newLocalName)

                    val utf8bytes = expect.getBytes(StandardCharsets.UTF_8).length
                    def findBranch(search: Code.ValueLike): T[Code.Statement] =
                      onSearch(search)
                        .map { onS =>
                          Code.ifThenElse(
                            candidate :> Code.IntLiteral(-1),
                            // update candidate and search
                            Code.Statements(
                              candOffset := candidate + Code.IntLiteral(utf8bytes),
                              onS
                            ),
                            // else no more candidates
                            start := Code.IntLiteral(-1)
                          )
                        }

                    for {
                      search <- searchEnv
                      expectStr <- StringApi.fromString(expect)
                      // these happen in a scope of the while loop
                      findFound <- scope {
                        findBranch(search)
                          .product(
                            expectStr.onExpr { es =>
                              pv(StringApi.find(strEx, es, start))  
                            }(newLocalName))
                          }
                      // TODO: maybe don't ignore root counts?
                      ((find, found), _rootCounts) = findFound
                    } yield (Code
                      .Statements(
                        Code.DeclareVar(Nil, Code.TypeIdent.Int, start, Some(offsetIdent)),
                        // these are mutable variables used in the loop below
                        Code.DeclareVar(Nil, Code.TypeIdent.Int, candidate, None),
                        Code.DeclareVar(Nil, Code.TypeIdent.Int, candOffset, None),
                        Code.declareBool(result, Some(false)),
                        Code.While(
                          (start :> -1),
                          Code.block(
                            candidate := found,
                            find
                          )
                        )
                      ) +: result)
                  }
                case (_: CharPart) :: _ =>
                  // we no longer know the offset after this because
                  // the character could be a multi-byte codepoint
                  val next1 = if (h.capture) (next + 1) else next
                  for {
                    matched <- newLocalName("matched")
                    decMatch = Code.declareBool(matched, Some(false))
                    off1 <- newLocalName("off1")
                    // the tail match isn't true, because we loop until we find
                    // a case
                    tailMatched <- loop(knownOffset = None, off1, tail, next1, false)

                    matchStmt = Code
                      .Statements(
                        decMatch,
                        Code.DeclareVar(Nil, Code.TypeIdent.Int, off1, Some(offsetIdent)),
                        Code.While(
                          // TODO: we should only compute the length 1 time per loop
                          // if needed
                          (!matched) && (off1 :< StringApi.utf8ByteLength(strEx)),
                          Code.block(matched := tailMatched) // the tail match increments the
                        )
                      ) +: (if (mustMatch) Code.TrueLit else matched)

                    fullMatch <-
                      if (!h.capture) pv(matchStmt)
                      else {
                        val b = bindArray(next)
                        val capture =
                          for {
                            root <- nextRootId
                            stmt <- assignMut(b, StringApi.substring(strEx, offsetIdent, off1), MemState.Owned(root))
                          } yield (stmt +: (Code.TrueLit))

                        capture.flatMap(andCode(matchStmt, _))
                      }

                  } yield fullMatch
                // $COVERAGE-OFF$
                case (_: Glob) :: _ =>
                  throw new IllegalArgumentException(
                    s"pattern: $pat should have been prevented: adjacent globs are not permitted (one is always empty)"
                  )
                // $COVERAGE-ON$
              }
          }

          for {
            offsetIdent <- newLocalName("offset")
            res <- loop(Some(0), offsetIdent, pat, 0, mustMatch)
          } yield Code.declareInt(offsetIdent, Some(0)) +: res
      }

      def searchList(
          currentList: Code.Ident,
          initVL: Code.ValueLike,
          checkVLT: T[Code.ValueLike],
          optLeft: Option[LocalAnonMut]
      ): T[Code.ValueLike] = {
        import Code.Expression

        val emptyList: Expression =
          Code.Ident("alloc_enum0")(Code.IntLiteral(0))

        def isNonEmptyList(expr: Expression): Expression =
          Code.Ident("get_variant")(expr) =:= Code.IntLiteral(1)

        def headList(expr: Expression): Expression =
          Code.Ident("get_enum_index")(expr, Code.IntLiteral(0))

        def tailList(expr: Expression): Expression =
          Code.Ident("get_enum_index")(expr, Code.IntLiteral(1))

        // accepts Refs to build a new list
        def consList(head: Expression, tail: Expression): Expression =
          Code.Ident("alloc_enum2")(Code.IntLiteral(1), Code.Ident("clone_value")(head), tail)
        /*
         * here is the implementation from MatchlessToValue
         *
            Dynamic { (scope: Scope) =>
              var res = false
              var currentList = initF(scope)
              var leftList = VList.VNil
              while (currentList ne null) {
                currentList match {
                  case nonempty@VList.Cons(head, tail) =>
                    scope.updateMut(mutV, nonempty)
                    scope.updateMut(left, leftList)
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else {
                      currentList = tail
                      leftList = VList.Cons(head, leftList)
                    }
                  case _ =>
                    currentList = null
                    // we don't match empty lists
                }
              }
              res
            }
         */
        for {
          optLeft <- optLeft.traverse { lm => getAnon(lm.ident) }
          // holds a boolean, which isn't memory but a value
          res <- newLocalName("result")
          // this holds a reference to list as we iterate through
          tmpList <- newLocalName("tmp_list")
          declTmpList <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, tmpList, initVL)(newLocalName)
          // TODO: we need to check that the memstate of currentList is compatible and handled in the loop
          (checkVL, _rootCounts) <- scope(checkVLT)
          /*
          top <- currentTop
          _ = println(s"""in $top: searchList(
          $locMut: LocalAnonMut,
          $initVL: Code.ValueLike,
          $checkVL: Code.ValueLike,
          $optLeft: Option[LocalAnonMut]
          )""")
          */
        } yield
            (Code
              .Statements(
                Code.DeclareVar(Nil, Code.TypeIdent.Bool, res, Some(Code.FalseLit)),
                declTmpList
              )
              .maybeCombine(
                optLeft.map(_ := emptyList),
              ) +
                // we don't match empty lists, so if currentList reaches Empty we are done
                Code.While(
                  isNonEmptyList(tmpList),
                  Code.block(
                    currentList := tmpList,
                    res := checkVL,
                    Code.ifThenElse(res,
                      { tmpList := emptyList },
                      {
                        (tmpList := tailList(tmpList))
                          .maybeCombine(
                            optLeft.map { left =>
                              // consList allocates an owned value
                              left := consList(headList(currentList), left)
                            }
                          )
                      }
                    )
                  )
                )
              ) :+ res
      }

      def boxFn(ident: Code.Ident, arity: Int): Code.Expression =
        Code.Ident(s"alloc_boxed_pure_fn$arity")(ident)

      // We have to lift functions to the top level and not
      // create any nesting
      def innerFn(fn: FnExpr): T[Code.ValueLike] = {
        val nameSuffix = fn.recursiveName match {
          case None => ""
          case Some(n) => Idents.escape("_", n.asString)
        }
        if (fn.captures.isEmpty) {
          cachedIdent(fn) {
            for {
              ident <- newTopName("lambda" + nameSuffix)
              stmt <- fnStatement(ident, fn)
              _ <- appendStatement(stmt)
            } yield ident
          }.map { ident =>
            boxFn(ident, fn.arity);
          }
        }
        else {
          // we create the function, then we allocate
          // values for the capture
          // alloc_closure<n>(capLen, captures, fnName)
          val maybeCached = cachedIdent(fn) {
            for {
              ident <- newTopName("closure" + nameSuffix)
              stmt <- fnStatement(ident, fn)
              _ <- appendStatement(stmt)
            } yield ident
          }

          for {
            ident <- maybeCached
            capName <- newLocalName("captures")
            capValues <- fn.captures.traverse(innerToArg(_))
            decl <- Code.ValueLike.declareArray(capName, Code.TypeIdent.BValue, capValues)(newLocalName)
          } yield Code.WithValue(decl,
            Code.Ident(s"alloc_closure${fn.arity}")(
              Code.IntLiteral(BigInt(fn.captures.length)),
              capName,
              ident
            )
          )
        }
      }

      def literal(lit: Lit): T[(MemState, Code.ValueLike)] =
        lit match {
          case c @ Lit.Chr(_) =>
            // encoded as integers in pure values
            pure((MemState.Static, Code.Ident("bsts_char_from_code_point")(Code.IntLiteral(c.toCodePoint))))
          case Lit.Integer(toBigInteger) =>
            try {
              val iv = toBigInteger.intValueExact()
              pure((MemState.Static, Code.Ident("bsts_integer_from_int")(Code.IntLiteral(iv))))
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
                (newLocalName("int"), nextRootId).mapN { (ident, root) =>
                  (MemState.Owned(root), Code.DeclareArray(Code.TypeIdent.UInt32, ident, Right(lits)) +:
                    Code.Ident("bsts_integer_from_words_copy")(
                      if (isPos) Code.TrueLit else Code.FalseLit,
                      Code.IntLiteral(lits.length),
                      ident
                    ))
                }
            }

          case Lit.Str(toStr) =>
            // TODO: we should lift all string literals to be globals or static values
            for {
              root <- nextRootId
              expr <- StringApi.fromString(toStr)
            } yield (MemState.Owned(root), expr)
        }

      def clone(v: Code.ValueLike): T[Code.ValueLike] =
        v.onExpr { res => pv(Code.Ident("clone_value")(res)) }(newLocalName)

      def decrementRef(ident: Code.Ident, cnt: Int): Code.Statement =
        Code.Ident("bsts_increment_value")(ident, Code.IntLiteral(-cnt)).stmt

      def innerToArg(expr: Expr): T[Code.ValueLike] =
        innerToValue(expr).flatMap {
          case (MemState.Static, v) => pv(v)
          case (MemState.Ref(lt), v) =>
            // we require that that count > 0, so the object is still alive
            // and we increment a count that is not accounted for in the count (we could
            // statically change the target value for this root, but for now we just call
            // clone).
            clone(v) <* borrowLifetime(lt)
          case (MemState.Owned(root), v) =>
            // arguments decrement count
            passToFn(root).as(v)
        }
      
      // when we only need a ref to this value
      def innerRef(expr: Expr): T[(MemState.Lifetime, Code.ValueLike)] =
        // we can ignore the count, every thing can be used as a ref
        innerToValue(expr).flatMap {
          case (MemState.Static, v) => pure((MemState.Lifetime.Static, v))
          case (MemState.Ref(lt), v) => pure((lt, v))
          case (MemState.Owned(root), ident: Code.Ident) =>
            // this ident has already been consumed
            pure((MemState.Lifetime.FromRoot(root), ident))
          case (MemState.Owned(root), notIdent) =>
            for {
              name <- newLocalName("ref")
              // TODO: we need to point all roots to special variables that are defined at the top level
              stmt <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, name, notIdent)(newLocalName)
              _ <- createOwned(name, root)
            } yield (MemState.Lifetime.FromRoot(root), stmt +: name)
        }


      def innerApp(app: App): T[Code.ValueLike] =
        app match {
          case App(Global(pack, fnName), args) =>
            directFn(pack, fnName).flatMap {
              case Some((ident, _)) =>
                // directly invoke instead of by treating them like lambdas
                args.traverse(innerToArg(_)).flatMap { argsVL =>
                  Code.ValueLike.applyArgs(ident, argsVL)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (globalIdent(pack, fnName), args.traverse(innerToArg(_))).flatMapN { (fnVL, argsVL) =>
                  // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                  // but since these are ValueLike, we need to handle more carefully
                  val fnValue = fnVL.onExpr { e => pv(e()) }(newLocalName);
                  fnValue.flatMap { fnValue =>
                    val fnSize = argsVL.length
                    val callFn = Code.Ident(s"call_fn$fnSize")
                    Code.ValueLike.applyArgs(callFn, fnValue :: argsVL)(newLocalName)
                  }
                }
            }
          case App(Local(fnName), args) =>
            directFn(fnName).flatMap {
              case Some((ident, isClosure, _)) =>
                // this can be an recursive call
                args.traverse(innerToArg(_)).flatMap { argsVL =>
                  // if we don't have a closure, which is lifted to top level
                  // directly invoke instead of by treating them like lambdas
                  val withSlot =
                    if (isClosure) slotsArgName :: argsVL
                    else argsVL
                  Code.ValueLike.applyArgs(ident, withSlot)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (getBinding(fnName), args.traverse(innerToArg(_))).flatMapN { (fnVL, argsVL) =>
                  // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
                  // but since these are ValueLike, we need to handle more carefully
                  val fnSize = argsVL.length
                  val callFn = Code.Ident(s"call_fn$fnSize")
                  Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(newLocalName)
                }
            }
          case App(MakeEnum(variant, arity, _), args) =>
            // to type check, we know that the arity must have the same length as args
            args.traverse(innerToArg).flatMap { argsVL =>
              val tag = Code.IntLiteral(variant)
              Code.ValueLike.applyArgs(Code.Ident(s"alloc_enum$arity"), tag :: argsVL)(newLocalName)
            }
          case App(MakeStruct(arity), args) =>
            if (arity == 1) {
              // this is a new-type, just return the arg
              innerToArg(args.head)
            }
            else {
              // to type check, we know that the arity must have the same length as args
              args.traverse(innerToArg).flatMap { argsVL =>
                Code.ValueLike.applyArgs(Code.Ident(s"alloc_struct$arity"), argsVL)(newLocalName)
              }
          }
          case App(SuccNat, args) =>
            // SuccNat applies to Nats, which are always values, and don't need counting
            innerRef(args.head).flatMap { case (_, arg) =>
              Code.ValueLike.applyArgs(Code.Ident("BSTS_NAT_SUCC"), NonEmptyList.one(arg))(newLocalName)
            }
          case App(fn, args) =>
            (innerRef(fn), args.traverse(innerToArg(_))).flatMapN { case ((fnLt, fnVL), argsVL) =>
              // we need to invoke call_fn<idx>(fn, arg0, arg1, ....)
              // but since these are ValueLike, we need to handle more carefully
              val fnSize = argsVL.length
              val callFn = Code.Ident(s"call_fn$fnSize")
              Code.ValueLike.applyArgs(callFn, fnVL :: argsVL)(newLocalName) <* borrowLifetime(fnLt)
            }
          }

      def assignMut(target: Code.Ident, vl: Code.ValueLike, rhs: MemState): T[Code.Statement] =
        // TODO
        pure(target := vl)

      def assignRootFromAlloc(vl: Code.ValueLike): T[(MemState, Code.ValueLike)] =
        for {
          r <- nextRootId
          vl1 <- assignRootFromAlloc(r, vl)
        } yield (MemState.Owned(r), vl1)

      def assignRootFromAlloc(r: Long, vl: Code.ValueLike): T[Code.ValueLike] = {
        val rootId = rootIdName(r)
        val decl = rootId := vl
        createOwned(rootId, r).as(decl +: rootId)
      }

      val bosatsuUnitValue: Code.Expression = Code.Ident("bsts_unit_value")()

      def innerToValue(expr: Expr): T[(MemState, Code.ValueLike)] =
        expr match {
          case fn: FnExpr => innerFn(fn).flatMap(assignRootFromAlloc(_))
          case Let(Right(arg), argV, in) =>
            // arg isn't in scope for argV
            innerToValue(argV).flatMap { case (ms, v) =>
              bind(arg) {
                for {
                  name <- getBinding(arg)
                  // we create the name before calling innerToValue(in) because the memory state needs to be updated
                  msResult <- bindMemory(name, ms, v) {
                    innerToValue(in)
                  }
                  (resMs, result) = msResult
                  stmt <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, name, v)(newLocalName)
                } yield (resMs, stmt +: result)
              }
            }
          case Let(Left(LocalAnon(idx)), argV, in) =>
            // LocalAnon(idx) isn't in scope for argV
            innerToValue(argV)
              .flatMap { case (ms, v) =>
                bindAnon(idx) {
                  for {
                    name <- getAnon(idx)
                    msResult <- bindMemory(name, ms, v) {
                      // we create the name before calling innerToValue(in) because the memory state needs to be updated
                      innerToValue(in)
                    }
                    (msRes, result) = msResult
                    stmt <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, name, v)(newLocalName)
                  } yield (msRes, stmt +: result)
                }
              }
          case app @ App(_, _) =>
            // we always own the result of an App
            innerApp(app).flatMap(assignRootFromAlloc(_))
          case Global(pack, name) =>
            directFn(pack, name)
              .flatMap {
                case Some((ident, arity)) =>
                  assignRootFromAlloc(boxFn(ident, arity))
                case None =>
                  globalIdent(pack, name).map(nm => (MemState.Static, nm()))
              }
          case Local(arg) =>
            directFn(arg)
              .flatMap {
                case Some((nm, isClosure, arity)) =>
                  if (!isClosure) {
                    // a closure can't be a static name
                    assignRootFromAlloc(boxFn(nm, arity))
                  }
                  else {
                    // recover the pointer to this closure from the slots argument
                    pure((MemState.Ref(MemState.Lifetime.ClosureVariable), Code.Ident("bsts_closure_from_slots")(slotsArgName)))
                  }
                case None =>
                  for {
                    ident <- getBinding(arg)
                    ms <- memState(ident)
                  } yield (ms, ident)
              }
          case ClosureSlot(idx) =>
            // we must be inside a closure function, so we should have a slots argument to access
            pure((MemState.Ref(MemState.Lifetime.ClosureVariable), slotsArgName.bracket(Code.IntLiteral(BigInt(idx)))))
          case LocalAnon(i) =>
            for {
              ident <- getAnon(i)
              ms <- memState(ident)
            } yield (ms, ident)
          case LocalAnonMut(i) =>
            for {
              ident <- getAnon(i)
              ms <- memState(ident)
            } yield (ms, ident)
          case LetMut(LocalAnonMut(m), span) =>
            bindAnon(m) {
              for {
                ident <- getAnon(m)
                // LocalAnonMuts are always owned
                anonRoot <- nextRootId
                _ <- createOwned(ident, anonRoot)
                decl = Code.DeclareVar(Nil, Code.TypeIdent.BValue, ident, None)
                msRes <- innerToValue(span)
                (ms, res) = msRes
              } yield (ms, decl +: res)
            }
          case Literal(lit) => literal(lit)
          case If(cond, thenExpr, elseExpr) =>
            // here we have to merge
            for {
              c <- boolToValue(cond)
              left = scope(innerToValue(thenExpr))
              right = scope(innerToValue(elseExpr))
              mte <- mergeValues(left, right)
              (ms, thenC, elseC) = mte
              v <- Code.ValueLike.ifThenElseV(c, thenC, elseC)(newLocalName)
            } yield (ms, v)
          case Always(cond, thenExpr) =>
            boolToValue(cond).flatMap { bv =>
              bv.discardValue match {
                case None => innerToValue(thenExpr)
                case Some(effect) => innerToValue(thenExpr).map { case (ms, res) => (ms, effect +: res) }
              }
            }
          case GetEnumElement(arg, _, index, _) =>
            // call get_enum_index(v, index)
            for {
              ltArgV <- innerRef(arg)
              (lt, argV) = ltArgV
              eV <- argV.onExpr(e => pv(Code.Ident("get_enum_index")(e, Code.IntLiteral(index))))(newLocalName)
            } yield (MemState.Ref(lt), eV)
          case GetStructElement(arg, index, size) =>
            if (size == 1) {
              // this is just a new-type wrapper, ignore it
              innerToValue(arg)
            }
            else {
              // call get_struct_index(v, index)
              for {
                ltArgV <- innerRef(arg)
                (lt, argV) = ltArgV
                eV <- argV.onExpr { e =>
                    pv(Code.Ident("get_struct_index")(e, Code.IntLiteral(index)))
                  }(newLocalName)
              } yield (MemState.Ref(lt), eV)
            }
          case makeEnum @ MakeEnum(variant, arity, _) =>
            // this is a closure over variant, we rewrite this
            if (arity == 0) pure((MemState.Static, Code.Ident("alloc_enum0")(Code.IntLiteral(variant))))
            else {
              val named =
                // safe because arity > 0
                NonEmptyList.fromListUnsafe(
                  Idents.allSimpleIdents.take(arity).map { nm => Identifier.Name(nm) }.toList
                )
              // This relies on optimizing App(MakeEnum, _) otherwise
              // it creates an infinite loop.
              // Also, this we should cache creation of Lambda/Closure values
              innerToValue(Lambda(Nil, None, named, App(makeEnum, named.map(Local(_)))))
            }
          case MakeStruct(arity) =>
            if (arity == 0) pure((MemState.Static, bosatsuUnitValue))
            else {
              val allocStructFn = s"alloc_struct$arity"
              assignRootFromAlloc(
                boxFn(Code.Ident(allocStructFn), arity)
              )
            }
          case ZeroNat =>
            pure((MemState.Static, Code.Ident("BSTS_NAT_0")))
          case SuccNat =>
            val arg = Identifier.Name("nat")
            // This relies on optimizing App(SuccNat, _) otherwise
            // it creates an infinite loop.
            // Also, this we should cache creation of Lambda/Closure values
            innerToValue(Lambda(Nil, None, NonEmptyList.one(arg),
              App(SuccNat, NonEmptyList.one(Local(arg)))))
          case PrevNat(of) =>
            // nats are always static
            for {
              resOf <- innerToValue(of)
              (_, argVL) = resOf
              prev <- Code.ValueLike.applyArgs(
                Code.Ident("BSTS_NAT_PREV"),
                NonEmptyList.one(argVL)
              )(newLocalName)
            } yield (MemState.Static, prev)
        }

      def rootIdName(rootId: Long): Code.Ident =
        Code.Ident(s"__bsts_r_${rootId}")

      def rootIdInitCount(rootId: Long): Code.Ident =
        Code.Ident(s"__bsts_ri_${rootId}")

      def fnStatement(fnName: Code.Ident, fn: FnExpr): T[Code.Statement] =
         fn match {
          case Lambda(captures, name, args, expr) =>
            val body = innerToArg(expr)
            val body1 = name match {
              case None => body
              case Some(rec) => recursiveName(fnName, rec, isClosure = captures.nonEmpty, arity = fn.arity)(body)
            }

            val argParamsTRoot = args.traverse { b =>
                for {
                  i <- getBinding(b)
                  r <- nextRootId
                  _ <- createOwned(i, r)
                } yield (Code.Param(Code.TypeIdent.BValue, i), r)
              }
            topScope(bindAll(args) { argParamsTRoot.product(body1) })
              .map { case ((argParamsRoot, fnBodyVL), finalCounts) =>
                val rootCntList = finalCounts.toList.sorted
                val rootDecls = rootCntList.flatMap { case (root, c) =>
                  val init = if (c < 0) -c else 0
                  Code.DeclareVar(Nil, Code.TypeIdent.BValue, rootIdName(root), Some(bosatsuUnitValue)) ::
                  Code.DeclareVar(Nil, Code.TypeIdent.Int, rootIdInitCount(root), Some(Code.IntLiteral(init))) ::
                  Nil
                }
                val extraReleases = rootCntList.flatMap { case (root, c) =>
                  if (c > 0) (decrementRef(rootIdName(root), c) :: Nil)
                  else Nil
                }

                val argParams = argParamsRoot.map(_._1)
                val argAssigns = argParamsRoot.map { case (param, r) =>
                  rootIdName(r) := param.name  
                }
                val allArgs =
                  if (captures.isEmpty) argParams
                  else {
                    Code.Param(Code.TypeIdent.BValue.ptr, slotsArgName) :: argParams
                  }
              
                val fullBody = Code.Statements.prepend(
                  rootDecls,
                  Code.Statements.prepend(
                    argAssigns.toList,
                    Code.returnAfterValue(extraReleases, fnBodyVL)
                  )
                )
                
                Code.DeclareFn(Nil, Code.TypeIdent.BValue, fnName, allArgs.toList, Some(Code.block(fullBody)))
              }
          case LoopFn(captures, nm, args, body) =>
            recursiveName(fnName, nm, isClosure = captures.nonEmpty, arity = fn.arity) {
              (newLocalName("cond"), newLocalName("res"))
                .flatMapN { (cond, res) =>

                  val argsT = args.traverse { b =>
                    for {
                      i <- getBinding(b)
                      iRoot <- nextRootId
                      _ <- createOwned(i, iRoot)
                      t <- newLocalName("loop_temp")
                      tRoot <- nextRootId
                      _ <- createOwned(t, tRoot)
                    } yield (Code.Param(Code.TypeIdent.BValue, i), t) 
                  }

                  val bodyArgsT = topScope {
                      bindAll(args) {
                        argsT.product(innerToArg(body))
                      }
                    }

                  for {
                    bodyArgsCounts <- bodyArgsT
                    // the newLocalName and the body exists inside the while loop scope
                    // TODO: add any clone/releases to manage function memory
                    ((argParamsTemps, bodyVL), finalCounts) = bodyArgsCounts
                    whileBody = toWhileBody(fnName, argParamsTemps, isClosure = captures.nonEmpty, cond = cond, result = res, body = bodyVL)
                    declTmps = Code.Statements(
                      argParamsTemps.map { case (_, tmp) =>
                        Code.DeclareVar(Nil, Code.TypeIdent.BValue, tmp, None)
                      }
                    )
                    fnBody = Code.block(
                      declTmps,
                      Code.DeclareVar(Nil, Code.TypeIdent.Bool, cond, Some(Code.TrueLit)),
                      Code.DeclareVar(Nil, Code.TypeIdent.BValue, res, None),
                      Code.While(cond, whileBody),
                      Code.Return(Some(res))
                    )
                    argParams = argParamsTemps.map(_._1)
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
            // TODO: if we can create the value statically, we don't
            // need the read_or_build trick
            //
            // we materialize an Atomic value to hold the static data
            // then we generate a function to populate the value
            for {
              vlRootCnt <- topScope(innerToValue(someValue))
              // TODO: use _rootCnt here to repair final root counts and memstate to see if we have ownership
              ((_memState, vl), _rootCnt) = vlRootCnt
              value <- staticValueName(p, b)
              consFn <- constructorFn(p, b)
              readFn <- globalIdent(p, b)
              _ <- makeConstructorsStatement(value, consFn, vl, readFn)
            } yield ()
        }
      }

      def makeConstructorsStatement(value: Code.Ident, consFn: Code.Ident, vl: Code.ValueLike, readFn: Code.Ident): T[Unit] =
        // TODO: if we can create the value statically, we don't
        // need the read_or_build trick
        //
        // we materialize an Atomic value to hold the static data
        // then we generate a function to populate the value
        for {
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
          res = Code.Ident("read_or_build")(value.addr, consFn)
          _ <- appendStatement(Code.DeclareFn(
            Code.Attr.Static :: Nil,
            Code.TypeIdent.BValue,
            readFn,
            Nil,
            Some(Code.block(Code.returnValue(res)))
          ))
        } yield ()

      def renderMain(p: PackageName, b: Bindable): T[Unit]
      def renderTests(values: List[(PackageName, Bindable)]): T[Unit]
    }

    object Env {
      def impl: Env = {
        def catsMonad[S]: Monad[StateT[EitherT[Eval, Error, *], S, *]] = implicitly

        new Env {

          case class State(
            allValues: AllValues,
            externals: ExternalResolver,
            includeSet: Set[Code.Include],
            includes: Chain[Code.Include],
            stmts: Chain[Code.Statement],
            currentTop: Option[(PackageName, Bindable)],
            binds: Map[Bindable, NonEmptyList[Either[((Code.Ident, Boolean, Int), Int), Int]]],
            counter: Long,
            rootCounter: Long,
            identCache: Map[Expr, Code.Ident],
            memScope: List[Map[Code.Ident, MemState]],
            rootCounts: Map[Long, Int]
          ) {
            def finalFile: Doc =
              Doc.intercalate(Doc.hardLine, includes.iterator.map(Code.toDoc(_)).toList) +
                Doc.hardLine + Doc.hardLine +
                Doc.intercalate(Doc.hardLine + Doc.hardLine, stmts.iterator.map(Code.toDoc(_)).toList)

            def include(incl: Code.Include): State =
              if (includeSet(incl)) this
              else copy(includeSet = includeSet + incl, includes = includes :+ incl)

            def enterScope: State =
              copy(memScope = Map.empty[Code.Ident, Nothing] :: memScope)

            def exitScope: (State, Map[Code.Ident, MemState]) =
              memScope match {
                case Nil => (this, Map.empty)
                case h :: tail => (copy(memScope = tail), h)
              }

            def resolveMemStateOf(i: Code.Ident): Option[MemState] = {
              @annotation.tailrec
              def lookup(i: Code.Ident, scopes: List[Map[Code.Ident, MemState]]): Option[MemState] =
                scopes match {
                  case Nil => None
                  case h :: tail =>
                    h.get(i) match {
                      case Some(ms) => Some(ms)
                      case None => lookup(i, tail)
                    }
                }

              lookup(i, memScope)
            }

            def decrementCount(root: Long): Either[Error, State] =
              // we have to ensure the count is at least one
              rootCounts.get(root) match {
                case Some(i) =>
                  Right {
                    // we need to decrement again so when it is initialized at runtime this value will be at least 1 
                    val rc1 = rootCounts.updated(root, i - 1)
                    copy(rootCounts = rc1)
                  }
                case None =>
                  Left(Error.InvariantViolation(s"root id $root does not exist in ${rootCounts} in ${currentTop} with memScope ${memScope}"))
              }  
          }

          object State {
            def init(allValues: AllValues, externals: ExternalResolver): State = {
              val defaultIncludes =
                List(Code.Include.quote("bosatsu_runtime.h"))

              State(allValues, externals, Set.empty ++ defaultIncludes, Chain.fromSeq(defaultIncludes), Chain.empty,
                None, Map.empty, 0L, 0L, Map.empty, Nil, Map.empty
              )
            }
          }

          type T[A] = StateT[EitherT[Eval, Error, *], State, A]

          implicit val monadImpl: Monad[T] = catsMonad[State]

          def run(pm: AllValues, externals: ExternalResolver, t: T[Unit]): Either[Error, Doc] =
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

          def read[A](fn: State => A): T[A] =
            StateT(s => EitherT[Eval, Error, (State, A)](Eval.now(Right((s, fn(s))))))

          def tryRead[A](fn: State => Either[Error, A]): T[A] =
            StateT(s => EitherT[Eval, Error, (State, A)](Eval.now(fn(s).map((s, _)))))

          def update[A](fn: State => (State, A)): T[A] =
            StateT(s => EitherT[Eval, Error, (State, A)](Eval.now(Right(fn(s)))))

          def tryUpdate[A](fn: State => Either[Error, (State, A)]): T[A] =
            StateT(s => EitherT[Eval, Error, (State, A)](Eval.now(fn(s))))

          def scope[A](ta: T[A]): T[(A, Map[Long, Int])] =
            for {
              _ <- update(s => (s.enterScope, ()))
              a <- ta
              counts <- update { s =>
                val s1 = s.exitScope._1
                (s1, s.rootCounts)
              }
            } yield (a, counts)

          // This denotes that we are working on a net top-level scope
          // which can't alias memory from another scope
          def topScope[A](ta: T[A]): T[(A, Map[Long, Int])] =
            for {
              ms0 <- update { s =>
                val saved = (s.memScope, s.rootCounter, s.rootCounts)
                val s1 = s.copy(memScope = Nil, rootCounter = 0L, rootCounts = Map.empty)
                (s1.enterScope, saved)
              }
              a <- ta
              finalRootCounts <- read(_.rootCounts)
              _ <- update { s =>
                (s.copy(memScope = ms0._1, rootCounter = ms0._2, rootCounts = ms0._3), ())
              }
            } yield (a, finalRootCounts)

          def passToFn(root: Long): T[Unit] =
            tryUpdate { s => s.decrementCount(root).map((_, ())) }

          def initMemState(ident: Code.Ident, is: MemState): T[Unit] =
            tryUpdate { s =>
              s.memScope match {
                case h :: tail =>
                  h.get(ident) match {
                    case None =>
                      val memScope1 = h.updated(ident, is) :: tail
                      Right((s.copy(memScope = memScope1), ()))
                    case Some(st) =>
                      Left(Error.InvariantViolation(s"invariant violation: existing identState=$st for $ident in scope=${s.memScope} in ${s.currentTop} while setting to: $is"))
                  }
                case Nil =>
                  Left(Error.InvariantViolation(s"no memScope for initMemState($ident, $is), invariant violation, should only be called inside a scope"))
              }
            }

          def createOwned(ident: Code.Ident, root: Long): T[Unit] =
            initMemState(ident, MemState.Owned(root)) *>
              tryUpdate { s =>
                s.rootCounts.get(root) match {
                  case None =>
                    // every created value starts with a count of 1
                    val rc1 = s.rootCounts.updated(root, 1)
                    val s1 = s.copy(rootCounts = rc1)
                    Right((s1, ()))
                  case Some(cnt) =>
                    Left(Error.InvariantViolation(s"createOwned($ident, $root) but the root already has count: $cnt"))
                }
              }

          def bindMemory[A](name: Code.Ident, ms: MemState, vl: Code.ValueLike)(in: T[A]): T[A] =
            initMemState(name, ms) *> in

          def memState(ident: Code.Ident): T[MemState] =
            tryRead { s =>
              s.resolveMemStateOf(ident) match {
                case Some(ms) =>
                  Right(ms)
                case None => 
                  Left(Error.InvariantViolation(s"no memState for memState($ident), invariant violation. current=${s.currentTop}, memScope=${s.memScope}"))
              }
            }
          // this has to make sure the current scope has an additional count so it
          // is safe to borrow
          def borrowLifetime(lt: MemState.Lifetime): T[Unit] = {
            import MemState.Lifetime._

            lt match {
              case Static | ClosureVariable => monadImpl.unit
              case Intersection(left, right) => borrowLifetime(left) *> borrowLifetime(right)
              case FromRoot(root) =>
                // we have to ensure the count is at least one
                tryUpdate { s =>
                  s.rootCounts.get(root) match {
                    case Some(i) =>
                      Right(if (i > 0) {
                        // the value is still alive
                        (s, ())
                      }
                      else {
                        // we need to decrement again so when it is initialized at runtime this value will be at least 1 
                        val rc1 = s.rootCounts.updated(root, i - 1)
                        val s1 = s.copy(rootCounts = rc1)
                        (s1, ())
                      })
                    case None =>
                      Left(Error.InvariantViolation(s"root id $root does not exist in ${s.rootCounts} in ${s.currentTop} with memScope ${s.memScope}"))
                  }  
                }
            }
          }

          def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident] =
            tryUpdate { s =>
              s.externals(pn, bn) match {
                case Some((incl, ident, _)) =>
                  // TODO: suspect that we are ignoring arity here
                  val withIncl = s.include(incl)
                  Right((withIncl, ident))
                case None =>
                  val key = (pn, bn)
                  s.allValues.get(key) match {
                    case Some((_, ident)) => Right((s, ident))
                    case None => Left(Error.UnknownValue(pn, bn))
                  }
              }
            }

          def bind[A](bn: Bindable)(in: T[A]): T[A] = {
            val init: T[Unit] = update { s =>
              val v = s.binds.get(bn) match {
                case None => NonEmptyList.one(Right(0))
                case Some(items @ NonEmptyList(Right(idx), _)) =>
                  Right(idx + 1) :: items
                case Some(items @ NonEmptyList(Left((_, idx)), _)) =>
                  Right(idx + 1) :: items
              }  
              
              (s.copy(binds = s.binds.updated(bn, v)), ())
            }

            val uninit: T[Unit] = update { s =>
              s.binds.get(bn) match {
                case Some(NonEmptyList(_, tail)) =>
                  val s1 = NonEmptyList.fromList(tail) match {
                    case None =>
                      s.copy(binds = s.binds - bn)
                    case Some(prior) =>
                      s.copy(binds = s.binds.updated(bn, prior))
                  }
                  (s1, ())
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
            tryRead { s =>
              s.binds.get(bn) match {
                case Some(stack) =>
                  stack.head match {
                    case Right(idx) =>
                      Right(Code.Ident(Idents.escape("__bsts_b_", bn.asString + idx.toString)))
                    case Left(((ident, _, _), _)) =>
                      // TODO: suspicious to ignore isClosure and arity here
                      // probably need to conv
                      Right(ident)
                  }
                case None => Left(Error.Unbound(bn, s.currentTop))
              }  
            }
          def bindAnon[A](idx: Long)(in: T[A]): T[A] =
            // in the future we see the scope of the binding which matters for GC, but here
            // we don't care
            in

          def mergeValues(
            left: T[((MemState, Code.ValueLike), Map[Long, Int])],
            right: T[((MemState, Code.ValueLike), Map[Long, Int])]): T[(MemState, Code.ValueLike, Code.ValueLike)] =
            // TODO: we have not unified counts
            (left, right)
              .flatMapN { case (((msL, l), lcnt), ((msR, r), rcnt)) =>
                // we only decrement counts statically, so we can only lower them when they don't match
                // which we need to do by inserting a dynamic call to decrement
                val allRoots = (lcnt.keySet | rcnt.keySet).toList.sorted
                val (resultRoots, leftAdj, rightAdj) = allRoots.foldLeft(
                    (Map.empty[Long, Int], List.empty[Code.Statement], List.empty[Code.Statement])
                  ) { case ((res, leftAdj, rightAdj), root) =>
                    (lcnt.get(root), rcnt.get(root)) match {
                      case (Some(l), Some(r)) =>
                        if (l == r) {
                          // these counts are the same
                          (res.updated(root, l), leftAdj, rightAdj)
                        }
                        else {
                          val diff = l - r
                          if (diff < 0) {
                            // we need to decrease l to match r, (l = r + diff) and diff < 0
                            val l1 = decrementRef(rootIdName(root), -diff) :: leftAdj
                            (res.updated(root, r), l1, rightAdj)
                          }
                          else {
                            // diff > 0
                            // we need to decrease r to match l, (r = l - diff) and diff > 0
                            val r1 = decrementRef(rootIdName(root), diff) :: rightAdj
                            (res.updated(root, r), leftAdj, r1)
                          }
                        }
                      case (None, Some(r)) =>
                        // the root was not created at all on the left branch
                        (res.updated(root, r), leftAdj, rightAdj)
                      case (Some(l), None) =>
                        // the root was not created at all on the right branch
                        (res.updated(root, l), leftAdj, rightAdj)
                      case (None, None) =>
                        sys.error("unreachable due to iterating keys")
                    }
                  }

                update(s => (s.copy(rootCounts = resultRoots), ())) *> {
                  val l1 = Code.ValueLike.beforeValue(leftAdj, l)
                  val r1 = Code.ValueLike.beforeValue(rightAdj, r)
                  
                  (msL, msR) match {
                    case _ if msL == msR =>
                      pure((msL, l1, r1))
                    case (MemState.Static, _) =>
                      pure((msR, l1, r1))
                    case (_, MemState.Static) =>
                      pure((msL, l1, r1))
                    case (MemState.Ref(lLife), MemState.Ref(rLife)) =>
                      val m1 = MemState.Ref(MemState.Lifetime.Intersection(lLife, rLife))
                      pure((m1, l1, r1))
                    case (_, _) =>
                      // In the rest of the cases, as least one side is
                      // owned and they aren't the same, so instead we clone both sides and
                      // assign to the new variable
                      for {
                        r <- nextRootId
                        rootId = rootIdName(r)
                        _ <- createOwned(rootId, r)
                        cl1 <- clone(l1)
                        cr1 <- clone(r1)
                      } yield (MemState.Owned(r), (rootId := cl1) +: rootId, (rootId := cr1) +: rootId)
                  }
                }
              }


          def getAnon(idx: Long): T[Code.Ident] =
            pure(Code.Ident(Idents.escape("__bsts_a_", idx.toString)))

          // a recursive function needs to remap the Bindable to the top-level mangling
          def recursiveName[A](fnName: Code.Ident, bn: Bindable, isClosure: Boolean, arity: Int)(in: T[A]): T[A] = {
            val init: T[Unit] = StateT { s =>
              val entry = (fnName, isClosure, arity)
              val v = s.binds.get(bn) match {
                case None => NonEmptyList.one(Left((entry, -1)))
                case Some(items @ NonEmptyList(Right(idx), _)) =>
                  Left((entry, idx)) :: items
                case Some(items @ NonEmptyList(Left((_, idx)), _)) =>
                  Left((entry, idx)) :: items
              }  
              result(s.copy(binds = s.binds.updated(bn, v)), ())
            }

            val uninit: T[Unit] = update { s =>
              s.binds.get(bn) match {
                case Some(NonEmptyList(_, tail)) =>
                  val s1 = NonEmptyList.fromList(tail) match {
                    case None =>
                      s.copy(binds = s.binds - bn)
                    case Some(prior) =>
                      s.copy(binds = s.binds.updated(bn, prior))
                  }
                  (s1, ())
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
              if (cnt < Long.MaxValue) {
                val s1 = s.copy(counter = cnt + 1L)
                result(s1, cnt)
              }
              else {
                // should be impossible since iterating 2^63 times would take too long
                errorRes(Error.InvariantViolation("counter overflow"))
              }
            }

          val nextRootId: T[Long] =
            StateT { s =>
              val cnt = s.rootCounter  
              if (cnt < Long.MaxValue) {
                val s1 = s.copy(rootCounter = cnt + 1L)
                result(s1, cnt)
              }
              else {
                // should be impossible since iterating 2^63 times would take too long
                errorRes(Error.InvariantViolation("root counter overflow"))
              }
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
          def directFn(pack: PackageName, b: Bindable): T[Option[(Code.Ident, Int)]] =
            StateT { s =>
              val key = (pack, b)
              s.allValues.get(key) match {
                case Some((fn: Matchless.FnExpr, ident)) =>
                  result(s, Some((ident, fn.arity)))
                case None =>
                  // this is external
                  s.externals(pack, b) match {
                    case Some((incl, ident, arity)) if arity > 0 =>
                      val withIncl = s.include(incl)
                      result(withIncl, Some((ident, arity)))
                    case _ => result(s, None)
                  }
                case _ => result(s, None)
              }  
            }

          def directFn(b: Bindable): T[Option[(Code.Ident, Boolean, Int)]] =
            StateT { s =>
              s.binds.get(b) match {
                case Some(NonEmptyList(Left((c, _)), _)) =>
                  result(s, Some(c))
                case _ =>
                  result(s, None)
              } 
            }

          def inTop[A](p: PackageName, bn: Bindable)(ta: T[A]): T[A] =
            for {
              _ <- StateT { (s: State) => result(s.copy(currentTop = Some((p, bn))), ())}
              a <- ta
              _ <- StateT { (s: State) => result(s.copy(currentTop = None), ()) }
            } yield a

          val currentTop: T[Option[(PackageName, Bindable)]] =
            StateT { (s: State) => result(s, s.currentTop) }

          def staticValueName(p: PackageName, b: Bindable): T[Code.Ident] =
            pure(Code.Ident(Idents.escape("___bsts_s_", fullName(p, b))))
          def constructorFn(p: PackageName, b: Bindable): T[Code.Ident] =
            pure(Code.Ident(Idents.escape("___bsts_c_", fullName(p, b))))

          // this is the name of a function when it is directly invokable
          def renderMain(p: PackageName, b: Bindable): T[Unit] =
            // TODO ???
            monadImpl.unit

          def renderTests(values: List[(PackageName, Bindable)]): T[Unit] = {
            values.traverse { case (p, b) =>
              (StringApi.staticString(p.asString), globalIdent(p, b)).tupled
            }
            .flatMap { packVals =>
              /*
              int main(int argc, char** argv) {
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
                Code.Ident("init_statics")().stmt,
                Code.Ident("atexit")(Code.Ident("free_statics")).stmt,
                Code.DeclareArray(Code.TypeIdent.Named("BSTS_Test_Result"), results, Left(testCount))
              )

              val mainFn = Code.declareMain(header ++
                allTests +
                Code.returnValue(summaryFn(Code.IntLiteral(testCount), results)))

              appendStatement(mainFn)
            } *> StateT(s => result(s.include(Code.Include.angle("stdlib.h")), ()))
          }

          def cachedIdent(key: Expr)(value: => T[Code.Ident]): T[Code.Ident] =
            StateT { s =>
              s.identCache.get(key) match {
                case Some(ident) => result(s, ident)
                case None =>
                  for {
                    s1Ident <- value.run(s)
                    (s1, ident) = s1Ident
                    s2 = s1.copy(identCache = s.identCache + (key -> ident))
                    res <- result(s2, ident)
                  }  yield res
              }  
            }
        }
      }
    }
  }
}