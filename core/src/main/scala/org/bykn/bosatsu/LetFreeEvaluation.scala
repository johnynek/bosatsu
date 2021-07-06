package org.bykn.bosatsu

import cats.Eval
// import LetFreeConversion.LetFreeExpressionTag
import scala.annotation.tailrec
import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}
import scala.collection.immutable.IntMap
import java.math.BigInteger
import org.bykn.bosatsu.rankn.DataFamily
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.TypeEnv

import Identifier.{Bindable, Constructor}

/*
 * LetFreeEvaluation exists so that we can verify that LetFreeExpressions do describe the same
 * output that the TypedExpressions that they are converted from describe.
 *
 * It is also useful for prototyping applications that heavily use LetFreeExpressions.
 */
object LetFreeEvaluation {
  type ExtEnv = Map[Identifier, Eval[Value]]
  type Cache = Option[CMap[String, (Future[Value], rankn.Type)]]

  case class LitValue(toAny: Any) {
    def equivToLit(lit: Lit) = toAny == lit.unboxToAny
  }

  object LitValue {
    def fromLit(lit: Lit) = LitValue(lit.unboxToAny)
  }

  type PatternEnv[T] = Map[Bindable, T]
  type PatternPNC = org.bykn.bosatsu.Pattern[
    (PackageName, Identifier.Constructor),
    rankn.Type
  ]

  sealed trait PatternMatch[+A]
  case class Matches[A](env: A) extends PatternMatch[A]
  case object NoMatch extends PatternMatch[Nothing]
  case object NotProvable extends PatternMatch[Nothing]

  def noop[T]: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = { (_, env) =>
    Matches(env)
  }
  def neverMatch[T]: (T, PatternEnv[T]) => PatternMatch[Nothing] = { (_, _) =>
    NoMatch
  }

  /*
   * MaybeBind is an implementation of determining if a T satisfies a Pattern and if it does it creates an environment
   * for right hand evaluation. This has been implemented for T as a LetFreeExpression, a Value, and a LetFreeValue. When
   * attempting to bind it result in a Matches, NoMatch, or NotProvable which means there's not enough present information to
   * determine what this would do at runtime (eg this may be running at compile time.)
   */
  abstract class MaybeBind[T](
      pat: PatternPNC
  ) {
    def toLitValue(t: T): Option[LitValue]
    def toStruct(t: T, df: DataFamily): Option[(Int, List[T])]
    def toList(t: T): Option[List[T]]
    def fromList(lst: List[T]): T
    def fromString(str: String): T
    def maybeBind(
        pat: PatternPNC
    ): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]

    val apply: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = pat match {
      case Pattern.WildCard => noop
      case Pattern.Literal(lit) => { (v, env) =>
        toLitValue(v) match {
          case Some(lv) => if (lv.equivToLit(lit)) Matches(env) else NoMatch
          case _        => NotProvable
        }
      }
      case Pattern.Var(n) => { (v, env) => Matches(env + (n -> v)) }
      case Pattern.Named(n, p) =>
        val inner = maybeBind(p)

        { (v, env) =>
          inner(v, env) match {
            case Matches(env1) => Matches(env1 + (n -> v))
            case notMatch      => notMatch
          }
        }
      case lp @ Pattern.ListPat(_) => {
        sealed abstract class StructListResult
        case object Empty extends StructListResult
        case class Cons(h: T, t: StructList) extends StructListResult
        case class StructList(asT: T) {
          lazy val parts = {
            val strct = toStruct(asT, DataFamily.Enum)
            strct.map {
              case (1, h :: t :: Nil) => Cons(h, StructList(t))
              case (0, Nil)           => Empty
              case _ =>
                sys.error(
                  "type checking should ensure this is always a bosatsu List"
                )
            }
          }
        }

        def consumePrefix(
            prefix: List[PatternPNC]
        ): (StructList, PatternEnv[T]) => PatternMatch[
          (StructList, PatternEnv[T])
        ] = {
          val matchers = prefix.map(maybeBind(_))
          val initial: (StructList, PatternEnv[T]) => PatternMatch[
            (StructList, PatternEnv[T])
          ] = { (x: StructList, env: PatternEnv[T]) => Matches((x, env)) }
          matchers.foldRight[(StructList, PatternEnv[T]) => PatternMatch[
            (StructList, PatternEnv[T])
          ]](initial) {
            case (fn, acc) => { (x: StructList, env: PatternEnv[T]) =>
              x.parts match {
                case None => NotProvable
                // Possible optimization below because we hit the end of the list and there's no point in going on
                case Some(Empty) => NoMatch
                case Some(Cons(head, tail)) =>
                  fn(head, env) match {
                    case Matches(nextEnv) => acc(tail, nextEnv)
                    case NotProvable      => NotProvable
                    case NoMatch          => NoMatch
                  }
              }
            }
          }
        }
        def consumeMatch(
            glob: Pattern.ListPart.Glob,
            suffix: List[PatternPNC]
        ): (StructList, PatternEnv[T]) => PatternMatch[
          (StructList, PatternEnv[T])
        ] = {
          val suffixConsumer = consumePrefix(suffix)
          @tailrec
          def loop(
              v: StructList,
              env: PatternEnv[T],
              acc: List[T]
          ): PatternMatch[(StructList, PatternEnv[T], List[T])] =
            suffixConsumer(v, env) match {
              case Matches((vRest, nextEnv)) => Matches((vRest, nextEnv, acc))
              case NotProvable               => NotProvable
              case NoMatch =>
                v.parts match {
                  case None                => NotProvable
                  case Some(Empty)         => NoMatch
                  case Some(Cons(h, tail)) => loop(tail, env, h :: acc)
                }
            }
          { (x: StructList, env: PatternEnv[T]) =>
            loop(x, env, Nil) match {
              case Matches((x, env, acc)) =>
                glob match {
                  case None => Matches((x, env))
                  case Some(n) =>
                    Matches((x, env + (n -> fromList(acc.reverse))))
                }
              case NoMatch     => NoMatch
              case NotProvable => NotProvable
            }
          }
        }

        def loop(
            matchList: NonEmptyList[(ListPart.Glob, List[PatternPNC])]
        ): (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
          matchList match {
            case NonEmptyList((None, Nil), Nil) => { (v, env) => Matches(env) }
            case NonEmptyList((Some(n), Nil), Nil) => { (v, env) =>
              Matches(env + (n -> v.asT))
            }
            case NonEmptyList((glob, suffix), Nil) => { (v, env) =>
              toList(v.asT) match {
                case None => NotProvable
                case Some(lst) =>
                  if (lst.length < suffix.length) NoMatch
                  else {
                    val (globList, tail) =
                      lst.splitAt(lst.length - suffix.length)
                    val globEnv = glob match {
                      case None    => env
                      case Some(n) => env + (n -> fromList(globList))
                    }
                    tail
                      .zip(suffix)
                      .foldLeft[PatternMatch[PatternEnv[T]]](Matches(globEnv)) {
                        case (Matches(env), (x, pat)) =>
                          maybeBind(pat).apply(x, env)
                        case (noMatch, _) => noMatch
                      }
                  }
              }
            }
            case NonEmptyList((glob, suffix), h :: ltail) => {
              val tail = NonEmptyList(h, ltail)
              val tailMatcher
                  : (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
                loop(tail)
              val prefixConsumer: (StructList, PatternEnv[T]) => PatternMatch[
                (StructList, PatternEnv[T])
              ] = consumeMatch(glob, suffix)
              val result: (StructList, PatternEnv[T]) => PatternMatch[
                PatternEnv[T]
              ] = { (v, env) =>
                prefixConsumer(v, env) match {
                  case Matches((remainder, nextEnv)) =>
                    tailMatcher(remainder, nextEnv)
                  case NoMatch     => NoMatch
                  case NotProvable => NotProvable
                }
              }
              result
            }
          }
        val (prefix, matchList) = lp.toMatchList
        val prefixConsumer = consumePrefix(prefix)
        val matchesConsumer = NonEmptyList.fromList(matchList) match {
          // $COVERAGE-OFF$ this case is actually optimized away. If there are no globs the listpattern becomes a positionalstruct
          case None => { (v: StructList, env: PatternEnv[T]) =>
            v.parts match {
              case None        => NotProvable
              case Some(Empty) => Matches(env)
              case Some(_)     => NoMatch
            }
          }
          // $COVERAGE-ON$
          case Some(lst) => loop(lst)
        }
        (v, env) => {
          val structList = StructList(v)
          prefixConsumer(structList, env) match {
            case Matches((remainder, env)) => matchesConsumer(remainder, env)
            case NoMatch                   => NoMatch
            case NotProvable               => NotProvable
          }
        }
      }

      case LetFreePattern.Union(h, t) =>
        // we can just loop expanding these out:
        def loop(
            ps: List[LetFreePattern]
        ): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
          ps match {
            case Nil => neverMatch
            case head :: tail =>
              val fnh = maybeBind(head)
              val fnt: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = loop(
                tail
              )
              val result: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = {
                case (arg, acc) =>
                  fnh(arg, acc) match {
                    case NoMatch    => fnt(arg, acc)
                    case notNoMatch => notNoMatch
                  }
              }
              result
          }
        loop(h :: t.toList)
      case LetFreePattern.PositionalStruct(maybeIdx, items, df) =>
        // The type in question is not the outer dt, but the type associated
        // with this current constructor
        val itemFns = items.map(maybeBind(_))

        def processArgs(
            as: List[T],
            acc: PatternEnv[T]
        ): PatternMatch[PatternEnv[T]] = {
          // manually write out foldM hoping for performance improvements
          @annotation.tailrec
          def loop(
              vs: List[T],
              fns: List[(T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]],
              env: (PatternEnv[T], PatternMatch[PatternEnv[T]])
          ): PatternMatch[PatternEnv[T]] =
            vs match {
              case Nil => env._2
              case vh :: vt =>
                fns match {
                  case fh :: ft =>
                    (fh(vh, env._1), env._2) match {
                      case (_, NoMatch) => NoMatch
                      case (NoMatch, _) => NoMatch
                      case (Matches(env1), Matches(_)) =>
                        loop(vt, ft, (env1, Matches(env1)))
                      case (Matches(env1), NotProvable) =>
                        loop(vt, ft, (env1, NotProvable))
                      case (NotProvable, _) =>
                        loop(vt, ft, (env._1, NotProvable))
                    }
                  case Nil =>
                    env._2 // mismatch in size, shouldn't happen statically
                }
            }
          loop(as, itemFns, (acc, Matches(acc)))
        }

        maybeIdx match {
          case None =>
            // this is a struct, which means we expect it
            { (arg: T, acc: PatternEnv[T]) =>
              toStruct(arg, df) match {
                case Some((_, args)) =>
                  processArgs(args, acc)
                case _ =>
                  NotProvable
              }
            }

          case Some(idx) =>
            // we don't check if idx < 0, because if we compiled, it can't be
            val result = { (arg: T, acc: PatternEnv[T]) =>
              toStruct(arg, df) match {
                case Some((enumId, args)) =>
                  if (enumId == idx) processArgs(args, acc)
                  else NoMatch
                case _ =>
                  NotProvable
              }
            }
            result
        }
      case Pattern.StrPat(parts) =>
        val listParts: List[LetFreePattern.ListPart] = parts.toList.flatMap {
          case Pattern.StrPart.WildStr     => List(Left(None))
          case Pattern.StrPart.NamedStr(n) => List(Left(Some(n)))
          case Pattern.StrPart.LitStr(str) =>
            str.toList.map(c => Right(Pattern.Literal(Lit.Str(c.toString))))
        }
        val listMaybeBind = maybeBind(LetFreePattern.ListPat(listParts))

        (v, env) =>
          toLitValue(v) match {
            case None => NotProvable
            case Some(LitValue(str)) =>
              listMaybeBind(
                fromList(
                  str
                    .asInstanceOf[String]
                    .toList
                    .map(c => fromString(c.toString))
                ),
                IntMap.empty
              ) match {
                case Matches(listEnv) => {
                  val strEnv: PatternEnv[T] = listEnv.map { case (i, lst) =>
                    val str = toList(lst).get
                      .flatMap(toLitValue(_))
                      .map(lv => lv.toAny.asInstanceOf[String])
                      .mkString
                    (i -> fromString(str))
                  }
                  Matches(env ++ strEnv)
                }
                case noMatch => noMatch
              }
          }
    }
  }

}

case class LetFreeEvaluation[T](
    pm: PackageMap.Typed[T],
    externals: Externals,
    extEnv: LetFreeEvaluation.ExtEnv,
    cache: LetFreeEvaluation.Cache
) {
  import LetFreeEvaluation._

  val valueToLitValue: Value => Option[LitValue] = { v =>
    Some(LitValue(v.asExternal.toAny))
  }
  val valueToStruct: (Value, rankn.DataFamily) => Option[(Int, List[Value])] = {
    case (v, df) =>
      df match {
        case rankn.DataFamily.Enum => {
          val vSum = v.asSum
          Some((vSum.variant, vSum.value.toList))
        }
        case rankn.DataFamily.Nat => {
          val vExt = v.asExternal
          val length: BigInteger = vExt.toAny.asInstanceOf[BigInteger]
          if (length.compareTo(BigInteger.ZERO) <= 0) {
            Some((0, Nil))
          } else {
            Some(
              (1, List(Value.ExternalValue(length.add(BigInteger.valueOf(-1)))))
            )
          }
        }
        case rankn.DataFamily.Struct => {
          val vProd = v.asProduct
          Some((0, vProd.toList))
        }
        case rankn.DataFamily.NewType => {
          Some((0, List(v)))
        }
      }
  }

  sealed trait LetFreeValue {
    def toJson: Json = this match {
      case ilv @ LazyValue(expression, scope, p) =>
        Json.JObject(
          List(
            "state" -> Json.JString("expression"),
            "expression" -> Json.JString(expressionToString(expression)),
            "scope" -> Json.JArray(ilv.cleanedScope.map { case (n, nv) =>
              Json.JArray(Vector(Json.JNumberStr(n.toString), nv.toJson))
            }.toVector)
          )
        )
      case ComputedValue(value) =>
        Json.JObject(
          List(
            "state" -> Json.JString("computed"),
            "data" -> Json.JString(value.toString)
          )
        )
    }

    def toLeaf: Leaf

    def toValue: Value

    def toStructNat: Option[(Int, List[LetFreeValue])]
    def toStructEnum: Option[(Int, List[LetFreeValue])]
    def toStructStruct: Option[(Int, List[LetFreeValue])]
    def toStructNewType: Option[(Int, List[LetFreeValue])]

    def toStruct(df: rankn.DataFamily): Option[(Int, List[LetFreeValue])] =
      df match {
        case rankn.DataFamily.Nat     => toStructNat
        case rankn.DataFamily.Enum    => toStructEnum
        case rankn.DataFamily.Struct  => toStructStruct
        case rankn.DataFamily.NewType => toStructNewType
      }
  }

  def expressionToString(expr: TypedExpr[T]): String = ???

  case class LazyValue(
      expression: TypedExpr[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ) extends LetFreeValue {
    def cleanedScope: List[(String, LetFreeValue)] = ???
    //  expression.tag.varSet.toList.sorted.map { n => (n, scope(n)) }

    lazy val toStructNat: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.NewType)

    lazy val toLeaf = evalToLeaf(expression, scope, p)
    lazy val toValue = toLeaf.toValue
  }

  case class ComputedValue(value: Value) extends LetFreeValue {
    lazy val toStructNat: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.NewType)

    lazy val toLeaf = Leaf.Value(this)
    val toValue = value
  }

  def nvToLitValue(implicit
      extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[LitValue] = { lfv =>
    valueToLitValue(lfv.toValue)
  }

  def nvToStruct(
      extEnv: ExtEnv,
      cache: Cache
  ): (LetFreeValue, rankn.DataFamily) => Option[(Int, List[LetFreeValue])] = {
    case (nv, df) => nv.toStruct(df)
  }

  def computedToStructImpl(cv: ComputedValue, df: DataFamily) = cv match {
    case ComputedValue(v) =>
      valueToStruct(v, df).map { case (n, lst) =>
        (n, lst.map(vv => ComputedValue(vv)))
      }
  }

  def lazyToStructImpl(
      lv: LazyValue,
      df: rankn.DataFamily
  ) = lv.toLeaf match {
    case Leaf.Struct(n, values, _)         => Some((n, values))
    case Leaf.Value(cv @ ComputedValue(_)) => cv.toStruct(df)
    // $COVERAGE-OFF$
    case other => sys.error(s"we should not get $other")
    // $COVERAGE-ON$
  }

  sealed abstract class Leaf {
    lazy val toValue = this match {
      case Leaf.Struct(enum, args, df)   => evaluateStruct(enum, args, df)
      case Leaf.Value(ComputedValue(v))  => v
      case Leaf.Lambda(expr, arg, scope) => ???
      /* new Value.FnValue(
          LetFreeFnValue(expr, scope)(extEnv, cache)
        ) */
      case Leaf.Literal(TypedExpr.Literal(lit, _, _)) => Value.fromLit(lit)
    }
  }
  object Leaf {
    case class Struct(
        n: Int,
        values: List[LazyValue],
        df: DataFamily
    ) extends Leaf
    case class Lambda(
        expr: TypedExpr[T],
        arg: Bindable,
        scope: Map[String, LetFreeValue]
    ) extends Leaf
    case class Literal(expr: TypedExpr.Literal[T]) extends Leaf
    case class Value(value: ComputedValue) extends Leaf
  }

  def annotationToLeaf(
      a: TypedExpr.Annotation[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf =
    evalToLeaf(a.term, scope, p)

  def genericToLeaf(
      g: TypedExpr.Generic[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf =
    evalToLeaf(g.in, scope, p)

  def localToLeaf(
      v: TypedExpr.Local[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf = {
    scope(v.name.asString).toLeaf
  }

  private def norm(
      pack: Package.Typed[T],
      item: Identifier,
      scope: Map[String, LetFreeValue]
  ): Leaf =
    NameKind(pack, item) match {
      case Some(namekind) =>
        namekind match {
          case NameKind.Let(name, recursive, expr) =>
            nameKindLetToLeaf(
              name,
              recursive,
              expr,
              pack,
              scope
            )
          case NameKind.Constructor(cn, _, dt, _) => constructor(cn, dt)
          case NameKind.Import(from, orig)        =>
            // we reset the environment in the other package
            norm(pm.toMap(from.name), orig, Map.empty)

          case NameKind.ExternalDef(pn, n, defType) =>
            Leaf.Value(ComputedValue(extEnv(n).value))
        }
      case None =>
        sys.error(s"we didn't find an item $item in pack.name ${pack.name}")
    }

  def globalToLeaf(
      v: TypedExpr.Global[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf = {
    val name = if (p.name == v.pack) {
      v.name
    } else {
      (for {
        items <- p.imports.collectFirst {
          case im if im.pack.name == v.pack => im.items
        }
        localName <- items.toList.collectFirst {
          case importedName if importedName.originalName == v.name =>
            importedName.localName
        }
      } yield localName) match {
        case Some(n) => n
        case None    => sys.error(s"typechecking means we should find ${v.name}")
      }
    }

    norm(p, name, scope)
  }

  def annotatedLambdaToLeaf(
      al: TypedExpr.AnnotatedLambda[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf =
    Leaf.Lambda(al.expr, al.arg, scope)

  def letToLeaf(
      l: TypedExpr.Let[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf =
    l.recursive match {
      case RecursionKind.Recursive =>
        lazy val lv: LazyValue = LazyValue(l.expr, nextScope, p)
        lazy val nextScope = scope + (l.arg.asString -> lv)
        evalToLeaf(l.in, nextScope, p)

      case _ =>
        val nextScope = scope + (l.arg.asString -> LazyValue(l.expr, scope, p))
        evalToLeaf(l.in, nextScope, p)
    }

  def appToLeaf(
      a: TypedExpr.App[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf = {
    val fn = evalToLeaf(a.fn, scope, p)
    val arg = LazyValue(a.arg, scope, p)
    applyLeaf(fn, arg, p).toLeaf
  }

  def literalToLeaf(
      l: TypedExpr.Literal[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf = Leaf.Literal(l)

  def matchToLeaf(
      m: TypedExpr.Match[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf = ???
  /*for {
    arg <- letFreeConvertExpr(m.arg, env, p)
    branches <- (m.branches.map { case branch =>
      letFreeConvertBranch(branch, env, p)
    }).sequence
    letFreeBranches = branches.map { case (p, e) =>
      (letFreeConvertPattern(p), e.tag._2)
    }
    lfeTag = normalOrderReduction(
      LetFreeExpression.Match(arg.tag._2, letFreeBranches)
    )
  } yield Match(arg = arg, branches = branches, tag = (m.tag, lfeTag)) */

  def simplifyMatch(
      mtch: TypedExpr.Match[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): LetFreeValue = {
    val (_, patEnv, result) = mtch.branches.toList
      .collectFirst(Function.unlift({ case (pat, result) =>
        LetFreeValueMaybeBind(pat)
          .apply(LazyValue(mtch.arg, scope), IntMap.empty) match {
          case LetFreeConversion.Matches(env) => Some((pat, env, result))
          case LetFreeConversion.NoMatch      => None
          // $COVERAGE-OFF$
          case LetFreeConversion.NotProvable =>
            sys.error("For value we should never be NotProvable")
          // $COVERAGE-ON$
        }
      }))
      .get

    ((patEnv.size - 1) to 0 by -1)
      .map(patEnv.get(_).get)
      .foldLeft[LetFreeValue](LazyValue(result, scope)) { (fn, arg) =>
        applyLeaf(fn.toLeaf, arg)
      }
  }

  private def nameKindLetToLeaf(
      name: Identifier.Bindable,
      recursive: RecursionKind,
      expr: TypedExpr[T],
      pack: Package.Typed[T],
      scope: Map[String, LetFreeValue]
  ): Leaf = ??? /*
    for {
      lookup <- State.inspect {
        lets: Map[(PackageName, Identifier), TypedExpr[
          (Declaration, LetFreeExpressionTag)
        ]] =>
          lets.get((pack.name, name))
      }
      outExpr <- lookup match {
        case Some(res) =>
          State.pure(res): NormState[
            TypedExpr[(Declaration, LetFreeExpressionTag)]
          ]
        case None =>
          recursive match {
            case RecursionKind.Recursive =>
              val nextEnv = (env - name).mapValues { lfe =>
                LetFreeConversion.incrementLambdaVars(lfe, 0)
              } + (name -> LetFreeExpression.LambdaVar(0))

              for {
                res <- letFreeConvertExpr(expr, nextEnv, pack)
                tag = res.tag
                wrappedNe = normalOrderReduction(
                  LetFreeExpression.Recursion(LetFreeExpression.Lambda(tag._2))
                )
                finalRes = res.updatedTag((res.tag._1, wrappedNe))
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[
                    (Declaration, LetFreeExpressionTag)
                  ]] =>
                    lets + ((pack.name, name) -> finalRes)
                }
              } yield finalRes
            case _ =>
              for {
                res <- letFreeConvertExpr(expr, env, pack)
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[
                    (Declaration, LetFreeExpressionTag)
                  ]] =>
                    lets + ((pack.name, name) -> res)
                }
              } yield res
          }
      }
    } yield outExpr */

  private def constructor(
      c: Constructor,
      dt: rankn.DefinedType[Any]
  ): Leaf = ??? /* {
    val (enum, arity) =
      dt.constructors.toList.iterator.zipWithIndex.collectFirst {
        case (cf, idx) if cf.name == c => (idx, cf.args.size)
      }.get

    def loop(params: Int, expr: LetFreeExpression): LetFreeExpression =
      if (params == 0) expr
      else loop(params - 1, LetFreeExpression.Lambda(expr))

    loop(
      arity,
      LetFreeExpression.Struct(
        enum,
        ((arity - 1) to 0 by -1).iterator
          .map(LetFreeExpression.LambdaVar(_))
          .toList,
        dt.dataFamily
      )
    )
  } */

  def evalToLeaf(
      expr: TypedExpr[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Leaf =
    expr match {
      case a @ TypedExpr.Annotation(_, _, _) => annotationToLeaf(a, scope, p)
      case g @ TypedExpr.Generic(_, _, _)    => genericToLeaf(g, scope, p)
      case v @ TypedExpr.Local(_, _, _)      => localToLeaf(v, scope, p)
      case v @ TypedExpr.Global(_, _, _, _)  => globalToLeaf(v, scope, p)
      case al @ TypedExpr.AnnotatedLambda(_, _, _, _) =>
        annotatedLambdaToLeaf(al, scope, p)
      case a @ TypedExpr.App(_, _, _, _)    => appToLeaf(a, scope, p)
      case l @ TypedExpr.Let(_, _, _, _, _) => letToLeaf(l, scope, p)
      case l @ TypedExpr.Literal(_, _, _) =>
        ??? //letFreeConvertLiteral(l, env, p)
      case m @ TypedExpr.Match(_, _, _) => ??? //letFreeConvertMatch(m, env, p)

      /*
      case LetFreeExpression.Struct(n, lst, df) =>
        Leaf.Struct(
          n,
          lst.zipWithIndex.map { case (argExpr, i) =>
            LazyValue(argExpr, scope)
          },
          df
        )
      case LetFreeExpression.App(fn, arg) => {
        applyLeaf(
          evalToLeaf(fn, scope),
          LazyValue(arg, scope)
        ).toLeaf
      }
      case LetFreeExpression.ExternalVar(p, n, tpe) =>
        Leaf.Value(ComputedValue(extEnv(n).value))
      case LetFreeExpression.Recursion(
            LetFreeExpression.Lambda(lambdaExpr)
          ) => {
        lazy val leaf: Leaf =
          evalToLeaf(lambdaExpr, LazyValue(expr, scope) :: scope)
        leaf
      }
      // $COVERAGE-OFF$ we don't have Recursions without lambdas
      case LetFreeExpression.Recursion(notLambda) =>
        sys.error(s"Recursion should always contain a Lambda")
      // $COVERAGE-ON$
      case LetFreeExpression.LambdaVar(index) =>
        scope(index).toLeaf
      case mtch @ LetFreeExpression.Match(_, _) =>
        simplifyMatch(mtch, scope).toLeaf
      case lambda @ LetFreeExpression.Lambda(_) =>
        Leaf.Lambda(lambda, scope, extEnv, cache)
      case lit @ LetFreeExpression.Literal(_) => Leaf.Literal(lit)
       */
    }

  def nvToList(implicit
      extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[List[LetFreeValue]] = { normalValue =>
    @tailrec
    def loop(nv: LetFreeValue, acc: List[LetFreeValue]): List[LetFreeValue] = {
      nv.toStruct(rankn.DataFamily.Enum).get match {
        case (0, _)          => acc
        case (1, List(h, t)) => loop(t, h :: acc)
        case _               =>
          // $COVERAGE-OFF$ this should be unreachable
          sys.error(
            "Type checking should only allow this to be applied to a list struct"
          )
        // $COVERAGE-ON$
      }
    }
    Some(loop(normalValue, Nil).reverse)
  }

  def nvFromList(p: Package.Typed[T]): List[LetFreeValue] => LetFreeValue = {
    scope =>
      @tailrec
      def loop(n: Int, acc: LetFreeExpression): LetFreeExpression = n match {
        case 0 => acc
        case _ =>
          loop(
            n - 1,
            LetFreeExpression
              .Struct(
                1,
                List(LetFreeExpression.LambdaVar(n - 1), acc),
                rankn.DataFamily.Enum
              )
          )
      }
      val expr = loop(
        scope.length,
        LetFreeExpression.Struct(0, Nil, rankn.DataFamily.Enum)
      )
      LazyValue(expr, scope, p)
  }

  type Applyable = Either[Value, (LetFreeExpression.Lambda, List[LetFreeValue])]
  type ToLFV = Option[LetFreeValue => Future[Value]]

  case class ExprFnValue(toExprFn: LetFreeValue => Value)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v))
    }
  }

  case class LetFreeFnValue(
      lambda: TypedExpr.Lambda,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      evalToValue(lambda.expr, ComputedValue(v) :: scope)(extEnv, cache)
    }
  }

  def attemptExprFn(
      v: Value
  ): Either[LetFreeValue => Value, Value => Value] = v match {
    case fv @ Value.FnValue(f) =>
      fv.arg match {
        case ExprFnValue(ef) => Left(ef)
        case _               => Right(f)
      }
    case other =>
      // $COVERAGE-OFF$ this should be unreachable
      sys.error(s"invalid cast to Fn: $other")
    // $COVERAGE-ON$

  }

  def applyLeaf(
      applyable: Leaf,
      arg: LetFreeValue,
      p: Package.Typed[T]
  ): LetFreeValue = applyable match {
    case Leaf.Lambda(expr, argName, scope) => {
      // By evaluating when we add to the scope we don't have to overflow the stack later when we
      // need to use the value
      val _ = arg match {
        case lv @ LazyValue(_, _, _) => {
          lv.toValue
          ()
        }
        case _ => ()
      }
      val nextScope = scope + (argName.asString -> arg)
      LazyValue(expr, nextScope, arg.p)
    }
    case Leaf.Value(lfv) =>
      attemptExprFn(lfv.toValue) match {
        case Left(eFn) =>
          ComputedValue(eFn(arg))
        case Right(fn) => {
          val v = arg.toValue
          ComputedValue(fn(v))
        }
      }
    // $COVERAGE-OFF$ structs aren't applyable
    case _ => sys.error("structs aren't applyable")
    // $COVERAGE-ON$
  }

  case class LetFreeValueMaybeBind(
      pat: org.bykn.bosatsu.Pattern[
        (PackageName, Identifier.Constructor),
        rankn.Type
      ]
  ) extends LetFreeConversion.MaybeBind[LetFreeValue](pat) {
    def toLitValue(t: LetFreeValue): Option[LitValue] =
      nvToLitValue(extEnv, cache)(t)
    def toStruct(t: LetFreeValue, df: DataFamily) =
      nvToStruct(extEnv, cache)(t, df)
    def toList(t: LetFreeValue): Option[List[LetFreeValue]] =
      nvToList(extEnv, cache)(t)
    def fromList(lst: List[LetFreeValue]): LetFreeValue =
      nvFromList(extEnv, cache)(lst)
    def fromString(str: String): LetFreeValue =
      LazyValue(LetFreeExpression.Literal(Lit.Str(str)), Nil)
    def maybeBind(pat: LetFreePattern) = LetFreeValueMaybeBind(pat).apply(_, _)
  }

  def evaluateStruct(
      enum: Int,
      args: List[LetFreeValue],
      df: DataFamily
  ): Value = df match {
    case rankn.DataFamily.Enum =>
      Value.SumValue(
        enum,
        Value.ProductValue.fromList(
          args.map(_.toValue)
        )
      )
    case rankn.DataFamily.Nat =>
      if (args.isEmpty) {
        Value.ExternalValue(BigInteger.valueOf(0))
      } else {
        Value.ExternalValue(
          args.head.toValue.asExternal.toAny
            .asInstanceOf[BigInteger]
            .add(BigInteger.ONE)
        )
      }
    case rankn.DataFamily.Struct =>
      Value.ProductValue.fromList(args.map(_.toValue))
    case rankn.DataFamily.NewType => args.head.toValue
  }

  def evalToValue(
      ne: TypedExpr[T],
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Value = LazyValue(ne, scope).toValue

  def evaluate(
      ne: LetFreeExpression,
      extEnv: Map[Identifier, Eval[Value]],
      cache: LetFreeEvaluation.Cache
  ): Value = evalToValue(ne, Nil)(extEnv, cache)

  def exprFn(
      arity: Int,
      wrapper: (
          rankn.Type,
          List[LetFreeValue]
      ) => Value
  ): FfiCall = {
    def evalExprFn(t: rankn.Type, revArgs: List[LetFreeValue]): ExprFnValue =
      if (revArgs.length + 1 < arity) {
        ExprFnValue { arg =>
          new Value.FnValue(evalExprFn(t, arg :: revArgs))
        }
      } else {
        ExprFnValue { case arg =>
          wrapper(t, (arg :: revArgs).reverse)
        }
      }

    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t, Nil)) }
  }

  type Pack = Package[Package.Interface, NonEmptyList[
    Referant[Variance]
  ], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[
    (Declaration, LetFreeExpressionTag)
  ], Any]]

  def evaluateCollect(
      p: PackageName,
      fn: Pack => Option[
        (
            Identifier.Bindable,
            RecursionKind,
            TypedExpr[(Declaration, LetFreeExpressionTag)]
        )
      ]
  ) = {
    for {
      pack <- packs.toMap.get(p)
      (name, _, tpe) <- fn(pack)
      lfe = tpe.tag._2
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      lfe,
      tpe.getType,
      extEnv
    )
  }

  def evaluateLast(
      p: PackageName
  ): Option[(LetFreeExpression, Type, Map[Identifier, Eval[Value]])] =
    evaluateCollect(p, { pack => pack.program.lets.lastOption })

  def evalLastTest(p: PackageName) =
    evaluateCollect(p, { pack => Package.testValue(pack) })
      .map { case (lfe, tpe, extEnv) =>
        LetFreeEvaluation.evaluate(lfe, extEnv, None)
      }
      .map(v => Eval.later(Test.fromValue(v)))

  private def externalEnv(
      p: Package.Typed[(Declaration, LetFreeExpressionTag)]
  ): LetFreeEvaluation.ExtEnv = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None    =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
        // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None      =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
        // $COVERAGE-ON$
      }
    }.toMap
  }

  private def importedEnv(
      p: Package.Typed[(Declaration, LetFreeExpressionTag)]
  ): LetFreeEvaluation.ExtEnv =
    p.imports.iterator.flatMap { imp =>
      val pack = packs.toMap.get(imp.pack.name) match {
        case Some(p) => p
        case None    =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown package: ${imp.pack.name}")
        // $COVERAGE-ON$
      }
      val exts = externalEnv(pack)
      imp.items.toList.iterator
        .flatMap { in =>
          exts.get(in.originalName).map { value => (in.localName, value) }
        }
    }.toMap

  val valueToJson: ValueToJson = ValueToJson({ case Type.Const.Defined(pn, t) =>
    for {
      pack <- packs.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
  })
}
