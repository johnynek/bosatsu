package dev.bosatsu.codegen.clang

import cats.{Eval, Functor, Monad, Show, Traverse}
import cats.data.{StateT, EitherT, NonEmptyList, Chain}
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import dev.bosatsu.codegen.{CompilationNamespace, CompilationSource, Idents}
import dev.bosatsu.rankn.{DataRepr, Type}
import dev.bosatsu.{Identifier, Lit, Matchless, Predef, PackageName}
import dev.bosatsu.pattern.StrPart
import dev.bosatsu.Matchless.Expr
import dev.bosatsu.Identifier.Bindable
import org.typelevel.paiges.Doc
import scala.collection.immutable.{SortedMap, SortedSet}

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
          case MatchString(arg, parts, binds, mustMatch) =>
            (
              innerToValue(arg),
              binds.traverse { case LocalAnonMut(m) => getAnon(m) }
            ).flatMapN { (strVL, binds) =>
              strVL.onExpr(arg => matchString(arg, parts, binds, mustMatch))(
                newLocalName
              )
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
        import Code.Expression
        private val prefix = "bsts_string_"
        def fn(nm: String): Expression = Code.Ident(prefix + nm)

        // string -> int
        def utf8ByteLength(e: Expression): Expression =
          fn("utf8_len")(e)

        // (string, int) -> int
        def codePointBytes(
            str: Expression,
            byteOffset: Expression
        ): Expression =
          fn("code_point_bytes")(str, byteOffset)

        // (string, int) -> char
        def getCharAt(str: Expression, byteOffset: Expression): Expression =
          fn("char_at")(str, byteOffset)

        // (string, int) -> string
        def substringTail(str: Expression, byteOffset: Expression): Expression =
          fn("substring_tail")(str, byteOffset)

        // (string, int, int) -> string
        def substring(
            str: Expression,
            startOffset: Expression,
            endOff: Expression
        ): Expression =
          fn("substring")(str, startOffset, endOff)

        // return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
        // (string, string, int) -> int
        def find(
            haystack: Expression,
            needle: Expression,
            byteOffset: Expression
        ): Expression =
          fn("find")(haystack, needle, byteOffset)

        // basically python src.startswith(expected, _) but with utf8 byte offsets
        // (string, int, string) -> _Bool
        def matchesAt(
            src: Expression,
            byteOffset: Expression,
            expected: Expression
        ): Expression =
          find(src, expected, byteOffset) =:= byteOffset

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
                loopRes <- loop(
                  knownOffset.map(_ + utf8bytes),
                  offsetIdent,
                  tail,
                  next,
                  mustMatch = false
                )
                regionMatches <- str.onExpr { s =>
                  pv(StringApi.matchesAt(strEx, offsetIdent, s))
                }(newLocalName)
                rest = (offsetIdent := offsetIdent + Code.IntLiteral(
                  utf8bytes
                )) +: (loopRes)
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
                offsetIdent := offsetIdent + StringApi.codePointBytes(
                  strEx,
                  offsetIdent
                )

              val stmt =
                if (c.capture) {
                  // b = str[offset]
                  Code
                    .Statements(
                      bindArray(next) := StringApi
                        .getCharAt(strEx, offsetIdent),
                      updateOffset
                    ) +: Code.TrueLit

                } else {
                  updateOffset +: Code.TrueLit
                }
              for {
                // we don't know how many bytes this character took, so
                // we have lost track of the total offset
                tailRes <- loop(
                  knownOffset = None,
                  offsetIdent,
                  tail,
                  n1,
                  mustMatch
                )
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
                      (bindArray(next) := StringApi
                        .substringTail(strEx, offsetIdent)) +: Code.TrueLit
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
                    val searchEnv = loop(
                      knownOffset = None,
                      candOffset,
                      tail2,
                      next1,
                      mustMatch = false
                    )

                    def onSearch(search: Code.ValueLike): T[Code.Statement] =
                      search.exprToStatement { search =>
                        monadImpl.pure(
                          Code.ifThenElse(
                            search, {
                              // we have matched
                              val after = Code.Statements(
                                result := Code.TrueLit,
                                start := Code.IntLiteral(-1)
                              )

                              if (h.capture) {
                                val capture: Code.Statement =
                                  bindArray(next) := StringApi.substring(
                                    strEx,
                                    offsetIdent,
                                    candidate
                                  )

                                capture + after
                              } else after
                            },
                            // we couldn't match at start, advance just after the
                            // candidate
                            start := candidate + Code.IntLiteral.One
                          )
                        )
                      }(newLocalName)

                    val utf8bytes =
                      expect.getBytes(StandardCharsets.UTF_8).length
                    def findBranch(search: Code.ValueLike): T[Code.Statement] =
                      onSearch(search)
                        .map { onS =>
                          Code.ifThenElse(
                            candidate :> Code.IntLiteral(-1),
                            // update candidate and search
                            Code.Statements(
                              candOffset := candidate + Code.IntLiteral(
                                utf8bytes
                              ),
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
                        Code.DeclareVar(
                          Nil,
                          Code.TypeIdent.Int,
                          start,
                          Some(offsetIdent)
                        ),
                        // these are mutable variables used in the loop below
                        Code
                          .DeclareVar(Nil, Code.TypeIdent.Int, candidate, None),
                        Code.DeclareVar(
                          Nil,
                          Code.TypeIdent.Int,
                          candOffset,
                          None
                        ),
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
                    tailMatched <- loop(
                      knownOffset = None,
                      off1,
                      tail,
                      next1,
                      false
                    )

                    matchStmt = Code
                      .Statements(
                        decMatch,
                        Code.DeclareVar(
                          Nil,
                          Code.TypeIdent.Int,
                          off1,
                          Some(offsetIdent)
                        ),
                        Code.While(
                          // TODO: we should only compute the length 1 time per loop
                          // if needed
                          (!matched) && (off1 :< StringApi.utf8ByteLength(
                            strEx
                          )),
                          Code.block(
                            matched := tailMatched
                          ) // the tail match increments the
                        )
                      ) +: (if (mustMatch) Code.TrueLit else matched)

                    fullMatch <-
                      if (!h.capture) pv(matchStmt)
                      else {
                        val capture =
                          (bindArray(next) := StringApi.substring(
                            strEx,
                            offsetIdent,
                            off1
                          )) +: (Code.TrueLit)

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
      def innerFn(fn: Lambda[K]): T[Code.ValueLike] = {
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
            StateT.modify(s => s.copy(stmts = s.stmts :+ stmt))

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
                    case None =>
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

  def apply[S](src: S)(implicit
      CS: CompilationSource[S]
  ): ClangGen[CS.ScopeKey] = {
    new ClangGen(CS.namespace(src))
  }
}
