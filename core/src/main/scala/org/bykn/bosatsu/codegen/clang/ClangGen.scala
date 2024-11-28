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
    case class InvariantViolation(message: String, expr: Expr) extends Error
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

      /////////////////////////////////////////
      // the below are independent of the environment implementation
      /////////////////////////////////////////
    
      // This name has to be impossible to give out for any other purpose
      val slotsArgName: Code.Ident = Code.Ident("__bstsi_slot")

      // assign any results to result and set the condition to false
      // and replace any tail calls to nm(args) with assigning args to those values
      def toWhileBody(fnName: Code.Ident, args: NonEmptyList[Code.Param], isClosure: Boolean, cond: Code.Ident, result: Code.Ident, body: Code.ValueLike): Code.Block = {
      
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
              val assigns = args
                .iterator
                .zip(newArgsList.iterator)
                .flatMap { case (Param(_, name), value) =>
                  if (name != value)
                    // don't create self assignments
                    Iterator.single(Assignment(name, value))
                  else
                    Iterator.empty
                }
                .toList

              // there is always at least one new argument or the loop
              // won't terminate
              val assignNEL = NonEmptyList.fromListUnsafe(assigns)
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
        expr =:= Code.Ident("BSTS_TO_CHAR")(Code.IntLiteral(codePoint))

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
            innerToValue(expr).flatMap { vl =>
              lit match {
                case c @ Lit.Chr(_) => vl.onExpr { e => pv(equalsChar(e, c.toCodePoint)) }(newLocalName)
                case Lit.Str(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(Code.Ident("bsts_string_equals"),
                        NonEmptyList(e, litStr :: Nil)
                      )(newLocalName)
                    }
                  }(newLocalName)
                case Lit.Integer(_) =>
                  vl.onExpr { e =>
                    literal(lit).flatMap { litStr =>
                      Code.ValueLike.applyArgs(Code.Ident("bsts_integer_equals"),
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
              .flatMapN { (a, b) => andCode(a, b) }

          case CheckVariant(expr, expect, _, _) =>
            innerToValue(expr).flatMap { vl =>
            // this is just get_variant(expr) == expect
              vl.onExpr { expr => pv(Code.Ident("get_variant")(expr) =:= Code.IntLiteral(expect)) }(newLocalName)
            }
          case SearchList(lst, init, check, leftAcc) =>
            (boolToValue(check), innerToValue(init))
              .flatMapN { (condV, initV) =>
                searchList(lst, initV, condV, leftAcc)
              }
          case MatchString(arg, parts, binds, mustMatch) =>
            (
              innerToValue(arg),
              binds.traverse { case LocalAnonMut(m) => getAnon(m) }
            ).flatMapN { (strVL, binds) =>
              strVL.onExpr { arg => matchString(arg, parts, binds, mustMatch) }(newLocalName)
            }
          case SetMut(LocalAnonMut(idx), expr) =>
            for {
              name <- getAnon(idx)
              vl <- innerToValue(expr)
            } yield (name := vl) +: Code.TrueLit
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
            monadImpl.pure(
              Code.StrLiteral(new String(bytes.map(_.toChar)))
            )
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
                Code.StrLiteral(new String(bytes.map(_.toChar)))
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

              val stmt =
                if (c.capture) {
                  // b = str[offset]
                  Code
                    .Statements(
                      bindArray(next) := StringApi.getCharAt(strEx, offsetIdent),
                      updateOffset
                    ) +: Code.TrueLit

                } else {
                  updateOffset +: Code.TrueLit
                }
              for {
                // we don't know how many bytes this character took, so
                // we have lost track of the total offset
                tailRes <- loop(knownOffset = None, offsetIdent, tail, n1, mustMatch)
                and2 <- andCode(stmt, tailRes)
                and1 <- andCode(matches, and2)
              } yield and1
            case (h: Glob) :: tail =>
              tail match {
                case Nil =>
                  // we capture all the rest
                  pv(
                    if (h.capture) {
                      // b = str[offset:]
                      (bindArray(next) := StringApi.substringTail(strEx, offsetIdent)) +: Code.TrueLit
                    } else Code.TrueLit
                  )
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
                        monadImpl.pure(Code.ifThenElse(search, {
                          // we have matched
                          val after = Code.Statements(
                            result := Code.TrueLit,
                            start := Code.IntLiteral(-1)
                          )

                          if (h.capture) {
                            val capture: Code.Statement =
                              bindArray(next) := StringApi.substring(strEx, offsetIdent, candidate)

                            capture + after
                          }
                          else after
                        },
                        // we couldn't match at start, advance just after the
                        // candidate
                        start := candidate + Code.IntLiteral.One
                      ))
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
                      find <- findBranch(search)
                      expectStr <- StringApi.fromString(expect)
                      found <- expectStr.onExpr { es =>
                        pv(StringApi.find(strEx, es, start))  
                      }(newLocalName)
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
                        val capture =
                          (bindArray(next) := StringApi.substring(strEx, offsetIdent, off1)) +: (Code.TrueLit)

                        andCode(matchStmt, capture)
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
          locMut: LocalAnonMut,
          initVL: Code.ValueLike,
          checkVL: Code.ValueLike,
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

        def consList(head: Expression, tail: Expression): Expression =
          Code.Ident("alloc_enum2")(Code.IntLiteral(1), head, tail)
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
          currentList <- getAnon(locMut.ident)
          optLeft <- optLeft.traverse(lm => getAnon(lm.ident))
          res <- newLocalName("result")
          tmpList <- newLocalName("tmp_list")
          declTmpList <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, tmpList, initVL)(newLocalName)
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
      def innerFn(fn: FnExpr): T[Code.ValueLike] = 
        if (fn.captures.isEmpty) {
          for {
            ident <- newTopName("lambda")
            stmt <- fnStatement(ident, fn)
            _ <- appendStatement(stmt)
          } yield boxFn(ident, fn.arity);
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
              capName,
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

          case Lit.Str(toStr) => StringApi.fromString(toStr)
        }

      def innerApp(app: App): T[Code.ValueLike] =
        app match {
          case App(Global(pack, fnName), args) =>
            directFn(pack, fnName).flatMap {
              case Some((ident, _)) =>
                // directly invoke instead of by treating them like lambdas
                args.traverse(innerToValue(_)).flatMap { argsVL =>
                  Code.ValueLike.applyArgs(ident, argsVL)(newLocalName)
                }
              case None =>
                // the ref be holding the result of another function call
                (globalIdent(pack, fnName), args.traverse(innerToValue(_))).flatMapN { (fnVL, argsVL) =>
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
                (getBinding(fnName), args.traverse(innerToValue(_))).flatMapN { (fnVL, argsVL) =>
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
            // arg isn't in scope for argV
            innerToValue(argV).flatMap { v =>
              bind(arg) {
                for {
                  name <- getBinding(arg)
                  result <- innerToValue(in)
                  stmt <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, name, v)(newLocalName)
                } yield stmt +: result
              }
            }
          case Let(Left(LocalAnon(idx)), argV, in) =>
            // LocalAnon(idx) isn't in scope for argV
            innerToValue(argV)
              .flatMap { v =>
                bindAnon(idx) {
                  for {
                    name <- getAnon(idx)
                    result <- innerToValue(in)
                    stmt <- Code.ValueLike.declareVar(Code.TypeIdent.BValue, name, v)(newLocalName)
                  } yield stmt +: result
                }
              }
          case app @ App(_, _) => innerApp(app)
          case Global(pack, name) =>
            directFn(pack, name)
              .flatMap {
                case Some((ident, arity)) =>
                  pv(boxFn(ident, arity))
                case None =>
                  globalIdent(pack, name).map(nm => nm())
              }
          case Local(arg) =>
            directFn(arg)
              .flatMap {
                case Some((nm, false, arity)) =>
                  // a closure can't be a static name
                  pv(boxFn(nm, arity))
                case _ =>
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
            if (arity == 0) pv(Code.Ident("alloc_enum0")(Code.IntLiteral(variant)))
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
            pv {
              if (arity == 0) Code.Ident("PURE_VALUE_TAG").castTo(Code.TypeIdent.BValue)
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
            val body = innerToValue(expr).map(Code.returnValue(_))
            val body1 = name match {
              case None => body
              case Some(rec) => recursiveName(fnName, rec, isClosure = captures.nonEmpty, arity = fn.arity)(body)
            }

            bindAll(args) {
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
            }
          case LoopFn(captures, nm, args, body) =>
            recursiveName(fnName, nm, isClosure = captures.nonEmpty, arity = fn.arity) {
              bindAll(args) {
                for {
                  cond <- newLocalName("cond")
                  res <- newLocalName("res")
                  bodyVL <- innerToValue(body)
                  argParams <- args.traverse { b =>
                    getBinding(b).map { i => Code.Param(Code.TypeIdent.BValue, i) }
                  }
                  whileBody = toWhileBody(fnName, argParams, isClosure = captures.nonEmpty, cond = cond, result = res, body = bodyVL)
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
            // TODO: if we can create the value statically, we don't
            // need the read_or_build trick
            //
            // we materialize an Atomic value to hold the static data
            // then we generate a function to populate the value
            for {
              vl <- innerToValue(someValue)
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
            counter: Long
          ) {
            def finalFile: Doc =
              Doc.intercalate(Doc.hardLine, includes.iterator.map(Code.toDoc(_)).toList) +
                Doc.hardLine + Doc.hardLine +
                Doc.intercalate(Doc.hardLine + Doc.hardLine, stmts.iterator.map(Code.toDoc(_)).toList)

            def include(incl: Code.Include): State =
              if (includeSet(incl)) this
              else copy(includeSet = includeSet + incl, includes = includes :+ incl)
          }

          object State {
            def init(allValues: AllValues, externals: ExternalResolver): State = {
              val defaultIncludes =
                List(Code.Include.quote("bosatsu_runtime.h"))

              State(allValues, externals, Set.empty ++ defaultIncludes, Chain.fromSeq(defaultIncludes), Chain.empty,
                None, Map.empty, 0L
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

          def globalIdent(pn: PackageName, bn: Bindable): T[Code.Ident] =
            StateT { s =>
              s.externals(pn, bn) match {
                case Some((incl, ident, _)) =>
                  // TODO: suspect that we are ignoring arity here
                  val withIncl = s.include(incl)
                  result(withIncl, ident)
                case None =>
                  val key = (pn, bn)
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
                    case Left(((ident, _, _), _)) =>
                      // TODO: suspicious to ignore isClosure and arity here
                      // probably need to conv
                      result(s, ident)
                  }
                case None => errorRes(Error.Unbound(bn, s.currentTop))
              }  
            }
          def bindAnon[A](idx: Long)(in: T[A]): T[A] =
            // in the future we see the scope of the binding which matters for GC, but here
            // we don't care
            in

          def getAnon(idx: Long): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("__bsts_a_", idx.toString)))

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
            monadImpl.pure(Code.Ident(Idents.escape("___bsts_s_", fullName(p, b))))
          def constructorFn(p: PackageName, b: Bindable): T[Code.Ident] =
            monadImpl.pure(Code.Ident(Idents.escape("___bsts_c_", fullName(p, b))))

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
        }
      }
    }
  }
}