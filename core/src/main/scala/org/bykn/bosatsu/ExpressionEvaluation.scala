package org.bykn.bosatsu

import cats.Eval
import scala.annotation.tailrec
import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}
import java.math.BigInteger
import org.bykn.bosatsu.rankn.DataFamily
import cats.data.{NonEmptyList, State}
import org.bykn.bosatsu.rankn.TypeEnv

import Identifier.{Bindable, Constructor}
import cats.Applicative

object ExpressionEvaluation {
  type ExtEnv = Map[Identifier, Eval[Value]]

  case class LitValue(toAny: Any) {
    def equivToLit(lit: Lit) = toAny == lit.unboxToAny
  }

  object LitValue {
    def fromLit(lit: Lit) = LitValue(lit.unboxToAny)
  }

  type PatternEnv[T] = Map[Bindable, T]
  type PatternPNC = Pattern[
    (PackageName, Constructor),
    rankn.Type
  ]

  sealed trait PatternMatch[+A]
  case class Matches[A](env: A) extends PatternMatch[A]
  case object NoMatch extends PatternMatch[Nothing]
  case object NotProvable extends PatternMatch[Nothing]

  def noop[T]
      : (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T] = {
    (_, env) =>
      State.pure(Matches(env))
  }
  def neverMatch[T]
      : (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T] = {
    (_, _) =>
      State.pure(NoMatch)
  }

  def listPatToMatchList(
      parts: List[Pattern.ListPart[PatternPNC]]
  ): (List[PatternPNC], List[(Pattern.ListPart.Glob, List[PatternPNC])]) = {
    parts match {
      case Nil => (Nil, Nil)
      case (w1 @ Pattern.ListPart.WildList) :: (w2 @ Pattern.ListPart.Item(
            Pattern.WildCard
          )) :: tail =>
        listPatToMatchList(w2 :: w1 :: tail)
      case (glob: Pattern.ListPart.Glob) :: tail =>
        listPatToMatchList(tail) match {
          case (prefix, rest) => (Nil, (glob, prefix) :: rest)
        }
      case Pattern.ListPart.Item(item) :: tail =>
        listPatToMatchList(tail) match {
          case (prefix, rest) => (item :: prefix, rest)
        }
    }
  }

  /*
   * MaybeBind is an implementation of determining if a T satisfies a Pattern and if it does it creates an environment
   * for right hand evaluation. This has been implemented for T as a Value, and a ExpressionValue. When
   * attempting to bind it result in a Matches, NoMatch, or NotProvable which means there's not enough present information to
   * determine what this would do at runtime (eg this may be running at compile time.)
   */
  abstract class MaybeBind[T](
      pat: PatternPNC
  ) {
    def toLitValue(t: T): NormState[Option[LitValue], T]
    def toStruct(t: T, df: DataFamily): NormState[Option[(Int, List[T])], T]
    def toList(t: T): NormState[Option[List[T]], T]
    def fromList(lst: List[T]): T
    def fromString(str: String): T
    def maybeBind(
        pat: PatternPNC
    ): (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T]
    def definedForCons(pc: (PackageName, Constructor)): rankn.DefinedType[Any]

    val apply: (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T] =
      pat match {
        case Pattern.Annotation(p, _) => maybeBind(p)
        case Pattern.WildCard         => noop
        case Pattern.Literal(lit) => { (v, env) =>
          toLitValue(v).map {
            case Some(lv) => if (lv.equivToLit(lit)) Matches(env) else NoMatch
            case _        => NotProvable
          }
        }
        case Pattern.Var(n) => { (v, env) =>
          State.pure(Matches(env + (n -> v)))
        }
        case Pattern.Named(n, p) =>
          val inner = maybeBind(p)

          { (v, env) =>
            inner(v, env).map {
              case Matches(env1) => Matches(env1 + (n -> v))
              case notMatch      => notMatch
            }
          }
        case lp @ Pattern.ListPat(_) => {
          sealed abstract class StructListResult
          case object Empty extends StructListResult
          case class Cons(h: T, t: StructList) extends StructListResult
          case class StructList(asT: T) {
            lazy val parts: NormState[Option[StructListResult], T] = {
              val strct = toStruct(asT, DataFamily.Enum)
              strct.map(_.map {
                case (1, h :: t :: Nil) => Cons(h, StructList(t))
                case (0, Nil)           => Empty
                case _ =>
                  sys.error(
                    "type checking should ensure this is always a bosatsu List"
                  )
              })
            }
          }

          def consumePrefix(
              prefix: List[PatternPNC]
          ): (StructList, PatternEnv[T]) => NormState[PatternMatch[
            (StructList, PatternEnv[T])
          ], T] = {
            val matchers = prefix.map(maybeBind(_))
            val initial: (StructList, PatternEnv[T]) => NormState[PatternMatch[
              (StructList, PatternEnv[T])
            ], T] = { (x: StructList, env: PatternEnv[T]) =>
              State.pure(Matches((x, env)))
            }
            matchers
              .foldRight[(StructList, PatternEnv[T]) => NormState[PatternMatch[
                (StructList, PatternEnv[T])
              ], T]](initial) {
                case (fn, acc) => { (x: StructList, env: PatternEnv[T]) =>
                  x.parts.flatMap {
                    case None => State.pure(NotProvable)
                    // Possible optimization below because we hit the end of the list and there's no point in going on
                    case Some(Empty) => State.pure(NoMatch)
                    case Some(Cons(head, tail)) =>
                      fn(head, env).flatMap {
                        case Matches(nextEnv) => acc(tail, nextEnv)
                        case NotProvable      => State.pure(NotProvable)
                        case NoMatch          => State.pure(NoMatch)
                      }
                  }
                }
              }
          }
          def consumeMatch(
              glob: Pattern.ListPart.Glob,
              suffix: List[PatternPNC]
          ): (StructList, PatternEnv[T]) => NormState[PatternMatch[
            (StructList, PatternEnv[T])
          ], T] = {
            val suffixConsumer = consumePrefix(suffix)
            type NormStateT[X] = NormState[X, T]
            val F = implicitly[cats.FlatMap[NormStateT]]

            def loop(
                vInit: StructList,
                env: PatternEnv[T],
                accInit: List[T]
            ): NormState[PatternMatch[
              (StructList, PatternEnv[T], List[T])
            ], T] = F.tailRecM((vInit, accInit)) { case (v, acc) =>
              suffixConsumer(v, env).flatMap { case (pm) =>
                pm match {
                  case Matches((vRest, nextEnv)) =>
                    State.pure(Right(Matches((vRest, nextEnv, acc))))
                  case NotProvable => State.pure(Right(NotProvable))
                  case NoMatch =>
                    v.parts.map {
                      case None                => Right(NotProvable)
                      case Some(Empty)         => Right(NoMatch)
                      case Some(Cons(h, tail)) => Left((tail, h :: acc))
                    }
                }
              }
            }

            { (x: StructList, env: PatternEnv[T]) =>
              loop(x, env, Nil).map {
                case Matches((x, env, acc)) =>
                  glob match {
                    case Pattern.ListPart.WildList => Matches((x, env))
                    case Pattern.ListPart.NamedList(n) =>
                      Matches((x, env + (n -> fromList(acc.reverse))))
                  }
                case NoMatch     => NoMatch
                case NotProvable => NotProvable
              }
            }
          }

          def loop(
              matchList: NonEmptyList[(Pattern.ListPart.Glob, List[PatternPNC])]
          ): (
              StructList,
              PatternEnv[T]
          ) => NormState[PatternMatch[PatternEnv[T]], T] =
            matchList match {
              case NonEmptyList((Pattern.ListPart.WildList, Nil), Nil) => {
                (v, env) => State.pure(Matches(env))
              }
              case NonEmptyList((Pattern.ListPart.NamedList(n), Nil), Nil) => {
                (v, env) =>
                  State.pure(Matches(env + (n -> v.asT)))
              }
              case NonEmptyList((glob, suffix), Nil) => { (v, env) =>
                toList(v.asT).flatMap {
                  case None => State.pure(NotProvable)
                  case Some(lst) =>
                    if (lst.length < suffix.length) State.pure(NoMatch)
                    else {
                      val (globList, tail) =
                        lst.splitAt(lst.length - suffix.length)
                      val globEnv = glob match {
                        case Pattern.ListPart.WildList => env
                        case Pattern.ListPart.NamedList(n) =>
                          env + (n -> fromList(globList))
                      }
                      tail
                        .zip(suffix)
                        .foldLeft[NormState[PatternMatch[PatternEnv[T]], T]](
                          State.pure(Matches(globEnv))
                        ) { case (acc, (x, pat)) =>
                          acc.flatMap {
                            case Matches(env) =>
                              maybeBind(pat).apply(x, env)
                            case _ => acc
                          }
                        }
                    }
                }
              }
              case NonEmptyList((glob, suffix), h :: ltail) => {
                val tail = NonEmptyList(h, ltail)
                val tailMatcher
                    : (StructList, PatternEnv[T]) => NormState[PatternMatch[
                      PatternEnv[T]
                    ], T] =
                  loop(tail)
                val prefixConsumer
                    : (StructList, PatternEnv[T]) => NormState[PatternMatch[
                      (StructList, PatternEnv[T])
                    ], T] = consumeMatch(glob, suffix)
                val result
                    : (StructList, PatternEnv[T]) => NormState[PatternMatch[
                      PatternEnv[T]
                    ], T] = { (v, env) =>
                  prefixConsumer(v, env).flatMap {
                    case Matches((remainder, nextEnv)) =>
                      tailMatcher(remainder, nextEnv)
                    case NoMatch     => State.pure(NoMatch)
                    case NotProvable => State.pure(NotProvable)
                  }
                }
                result
              }
            }
          val (prefix, matchList) = listPatToMatchList(lp.parts)
          val prefixConsumer = consumePrefix(prefix)
          val matchesConsumer: (
              StructList,
              PatternEnv[T]
          ) => NormState[PatternMatch[PatternEnv[T]], T] =
            NonEmptyList.fromList(matchList) match {
              // $COVERAGE-OFF$ this case is actually optimized away. If there are no globs the listpattern becomes a positionalstruct
              case None => { (v: StructList, env: PatternEnv[T]) =>
                v.parts.map {
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
            prefixConsumer(structList, env).flatMap {
              case Matches((remainder, env)) =>
                matchesConsumer(remainder, env)
              case NoMatch     => State.pure(NoMatch)
              case NotProvable => State.pure(NotProvable)
            }
          }
        }

        case Pattern.Union(h, t) =>
          // we can just loop expanding these out:
          def loop(
              ps: List[PatternPNC]
          ): (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T] =
            ps match {
              case Nil => neverMatch[T]
              case head :: tail =>
                val fnh = maybeBind(head)
                val fnt: (
                    T,
                    PatternEnv[T]
                ) => NormState[PatternMatch[PatternEnv[T]], T] = loop(
                  tail
                )
                val result: (
                    T,
                    PatternEnv[T]
                ) => NormState[PatternMatch[PatternEnv[T]], T] = {
                  case (arg, acc) =>
                    fnh(arg, acc).flatMap {
                      case NoMatch    => fnt(arg, acc)
                      case notNoMatch => State.pure(notNoMatch)
                    }
                }
                result
            }
          loop(h :: t.toList)
        case Pattern.PositionalStruct(pc, items) =>
          // The type in question is not the outer dt, but the type associated
          // with this current constructor
          val itemFns: List[
            (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T]
          ] = items.map(maybeBind(_))

          def processArgs(
              as: List[T],
              acc: PatternEnv[T]
          ): NormState[PatternMatch[PatternEnv[T]], T] = {
            type NormStateT[X] = NormState[X, T]
            val F = implicitly[cats.FlatMap[NormStateT]]

            // manually write out foldM hoping for performance improvements
            // @annotation.tailrec
            def loop(
                vsInit: List[T],
                fnsInit: List[
                  (
                      T,
                      PatternEnv[T]
                  ) => NormState[PatternMatch[PatternEnv[T]], T]
                ],
                envInit: (PatternEnv[T], PatternMatch[PatternEnv[T]])
            ): NormState[PatternMatch[PatternEnv[T]], T] =
              F.tailRecM((vsInit, fnsInit, envInit)) { case (vs, fns, env) =>
                vs match {
                  case Nil => State.pure(Right(env._2))
                  case vh :: vt =>
                    fns match {
                      case fh :: ft =>
                        fh(vh, env._1).map { h =>
                          (h, env._2) match {
                            case (_, NoMatch) => Right(NoMatch)
                            case (NoMatch, _) => Right(NoMatch)
                            case (Matches(env1), Matches(_)) =>
                              Left((vt, ft, (env1, Matches(env1))))
                            case (Matches(env1), NotProvable) =>
                              Left((vt, ft, (env1, NotProvable)))
                            case (NotProvable, _) =>
                              Left((vt, ft, (env._1, NotProvable)))
                          }
                        }
                      case Nil =>
                        State.pure(
                          Right(env._2)
                        ) // mismatch in size, shouldn't happen statically
                    }
                }
              }

            loop(as, itemFns, (acc, Matches(acc)))
          }

          val dt = definedForCons(pc)
          val df = dt.dataFamily

          if (dt.isStruct) {
            // this is a struct, which means we expect it
            { (arg: T, acc: PatternEnv[T]) =>
              toStruct(arg, df).flatMap {
                case Some((_, args)) =>
                  processArgs(args, acc)
                case _ =>
                  State.pure(NotProvable)
              }
            }
          } else {
            val ctor = pc._2
            val idx: Int = dt.constructors.indexWhere(_.name == ctor)
            val result: (
                T,
                PatternEnv[T]
            ) => NormState[PatternMatch[PatternEnv[T]], T] = {
              (arg: T, acc: PatternEnv[T]) =>
                toStruct(arg, df).flatMap {
                  case Some((enumId, args)) =>
                    if (enumId == idx) processArgs(args, acc)
                    else State.pure(NoMatch)
                  case _ =>
                    State.pure(NotProvable)
                }
            }
            result
          }
        case Pattern.StrPat(parts) =>
          val listParts: List[Pattern.ListPart[PatternPNC]] =
            parts.toList.flatMap {
              case Pattern.StrPart.WildStr => List(Pattern.ListPart.WildList)
              case Pattern.StrPart.NamedStr(n) =>
                List(Pattern.ListPart.NamedList(n))
              case Pattern.StrPart.LitStr(str) =>
                str.toList.map(c =>
                  Pattern.ListPart.Item(Pattern.Literal(Lit.Str(c.toString)))
                )
            }
          val listMaybeBind = maybeBind(Pattern.ListPat(listParts))

          (v, env) =>
            toLitValue(v).flatMap {
              case None => State.pure(NotProvable)
              case Some(LitValue(str)) =>
                listMaybeBind(
                  fromList(
                    str
                      .asInstanceOf[String]
                      .toList
                      .map(c => fromString(c.toString))
                  ),
                  Map.empty
                ).flatMap {
                  case Matches(listEnv: PatternEnv[T]) => {
                    val nsListPE = listEnv.toList.map { case (i, lst) =>
                      val lstOfStates: NormState[List[Option[LitValue]], T] =
                        toList(lst).flatMap(opt =>
                          traverseForNS(opt.get.map(toLitValue(_)))
                        )
                      val nsStr = lstOfStates
                        .map(
                          _.flatten
                            .map(lv => lv.toAny.asInstanceOf[String])
                            .mkString
                        )
                      nsStr.map(s => (i -> fromString(s)))
                    }
                    val nsStrEnv = traverseForNS(nsListPE)
                    nsStrEnv.map(strEnv => Matches(env ++ strEnv.toMap))
                  }
                  case noMatch => State.pure(noMatch)
                }
            }
      }
  }
  type NormState[A, V] = State[Map[(PackageName, Identifier), V], A]
  def traverseForNS[A, V](lst: List[NormState[A, V]]) = lst
    .foldRight[NormState[List[A], V]](State.pure(Nil)) { (x, nsList) =>
      for {
        h <- x
        lst <- nsList
      } yield h :: lst
    }

}

case class ExpressionEvaluation[T](
    pm: PackageMap.Typed[T],
    externals: Externals
) {
  import ExpressionEvaluation._

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

  sealed trait ExpressionValue {
    def toJson: Json = this match {
      case ilv @ LazyValue(expression, scope, p, extEnv, _) =>
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
      case StructValue(_, _, _) => ???
    }

    def toLeaf: NormState[Leaf]
    def toValue: NormState[Value]

    def toStructNat: NormState[Option[(Int, List[ExpressionValue])]]
    def toStructEnum: NormState[Option[(Int, List[ExpressionValue])]]
    def toStructStruct: NormState[Option[(Int, List[ExpressionValue])]]
    def toStructNewType: NormState[Option[(Int, List[ExpressionValue])]]

    def toStruct(
        df: rankn.DataFamily
    ): NormState[Option[(Int, List[ExpressionValue])]] =
      df match {
        case rankn.DataFamily.Nat     => toStructNat
        case rankn.DataFamily.Enum    => toStructEnum
        case rankn.DataFamily.Struct  => toStructStruct
        case rankn.DataFamily.NewType => toStructNewType
      }
  }

  type NormState[A] = State[Map[(PackageName, Identifier), ExpressionValue], A]

  def expressionToString(expr: TypedExpr[T]): String = ???

  case class LazyValue(
      expression: TypedExpr[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv,
      recursiveId: Option[String] = None
  ) extends ExpressionValue {
    def cleanedScope: List[(String, ExpressionValue)] = ???
    //  expression.tag.varSet.toList.sorted.map { n => (n, scope(n)) }

    lazy val toStructNat: NormState[Option[(Int, List[ExpressionValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: NormState[Option[(Int, List[ExpressionValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: NormState[Option[(Int, List[ExpressionValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: NormState[Option[(Int, List[ExpressionValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.NewType)

    lazy val toLeaf =
      evalToLeaf(expression, scope, p, extEnv, (recursiveId.map((_, this))))
    lazy val toValue = toLeaf.flatMap(_.toValue)
  }

  case class ComputedValue(value: Value) extends ExpressionValue {
    lazy val toStructNat =
      State.pure(computedToStructImpl(this, rankn.DataFamily.Nat))
    lazy val toStructEnum =
      State.pure(computedToStructImpl(this, rankn.DataFamily.Enum))
    lazy val toStructStruct =
      State.pure(computedToStructImpl(this, rankn.DataFamily.Struct))
    lazy val toStructNewType =
      State.pure(computedToStructImpl(this, rankn.DataFamily.NewType))

    lazy val toLeaf = State.pure(Leaf.Value(this))
    val toValue = State.pure(value)
  }

  case class StructValue(n: Int, values: List[ExpressionValue], df: DataFamily)
      extends ExpressionValue {

    lazy val toLeaf: NormState[Leaf] = State.pure(Leaf.Struct(n, values, df))
    lazy val toValue: NormState[Value] =
      evaluateStruct(n, values, df)

    lazy val toStructNat = State.pure(Some((n, values)))
    lazy val toStructEnum = State.pure(Some((n, values)))
    lazy val toStructStruct = State.pure(Some((n, values)))
    lazy val toStructNewType = State.pure(Some((n, values)))
  }

  val nvToLitValue: ExpressionValue => NormState[Option[LitValue]] = { lfv =>
    lfv.toValue.map(valueToLitValue)
  }

  val nvToStruct: (ExpressionValue, rankn.DataFamily) => NormState[
    Option[(Int, List[ExpressionValue])]
  ] = { case (nv, df) =>
    nv.toStruct(df)
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
  ): NormState[Option[(Int, List[ExpressionValue])]] = lv.toLeaf.flatMap {
    case Leaf.Struct(n, values, _)         => State.pure(Some((n, values)))
    case Leaf.Value(cv @ ComputedValue(_)) => cv.toStruct(df)
    // $COVERAGE-OFF$
    case other => sys.error(s"we should not get df: $df, leaf: $other")
    // $COVERAGE-ON$
  }

  sealed abstract class Leaf {
    lazy val toValue: NormState[Value] = this match {
      case Leaf.Struct(enum, args, df)  => evaluateStruct(enum, args, df)
      case Leaf.Value(ComputedValue(v)) => State.pure(v)
      case Leaf.Lambda(lambda, scope, p, extEnv, recursiveId) =>
        State.inspect(sa =>
          new Value.FnValue(
            ExpressionFnValue(
              lambda.expr,
              lambda.arg,
              scope ++ recursiveId.toList.toMap,
              p,
              extEnv,
              sa
            )
          )
        )
      case Leaf.Literal(TypedExpr.Literal(lit, _, _)) =>
        State.pure(Value.fromLit(lit))
    }
  }
  object Leaf {
    case class Struct(
        n: Int,
        values: List[ExpressionValue],
        df: DataFamily
    ) extends Leaf
    case class Lambda(
        lambda: TypedExpr.AnnotatedLambda[T],
        scope: Map[String, ExpressionValue],
        p: Package.Typed[T],
        extEnv: ExtEnv,
        recursiveId: Option[(String, ExpressionValue)]
    ) extends Leaf
    case class Literal(expr: TypedExpr.Literal[T]) extends Leaf
    case class Value(value: ComputedValue) extends Leaf
  }

  def annotationToLeaf(
      a: TypedExpr.Annotation[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] =
    evalToLeaf(a.term, scope, p, extEnv)

  def genericToLeaf(
      g: TypedExpr.Generic[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] =
    evalToLeaf(g.in, scope, p, extEnv)

  def localToLeaf(
      v: TypedExpr.Local[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T]
  ): NormState[Leaf] = {
    scope(v.name.asString).toLeaf
  }

  private def norm(
      pack: Package.Typed[T],
      extEnv: ExtEnv,
      item: Identifier,
      scope: Map[String, ExpressionValue],
      selfValue: ExpressionValue
  ): NormState[Leaf] =
    NameKind(pack, item) match {
      case Some(namekind) =>
        namekind match {
          case NameKind.Let(name, recursive, expr) =>
            nameKindLetToLeaf(
              name,
              recursive,
              expr,
              pack,
              extEnv,
              scope,
              selfValue
            )
          case c @ NameKind.Constructor(cn, _, dt, _) =>
            State.pure(constructor(cn, dt))
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            val impPack = pm.toMap(from.name)
            val impExtEnv = externalEnv(impPack) ++ importedEnv(impPack)
            norm(impPack, impExtEnv, orig, Map.empty, selfValue)

          case NameKind.ExternalDef(pn, n, defType) => {
            State.pure(Leaf.Value(ComputedValue(extEnv(n).value)))
          }
        }
      case None =>
        sys.error(s"we didn't find an item $item in pack.name ${pack.name}")
    }

  def globalToLeaf(
      v: TypedExpr.Global[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] = {
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
        case None => sys.error(s"typechecking means we should find ${v.name}")
      }
    }

    norm(p, extEnv, name, scope, LazyValue(v, scope, p, extEnv))
  }

  def annotatedLambdaToLeaf(
      al: TypedExpr.AnnotatedLambda[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv,
      recursiveId: Option[(String, ExpressionValue)]
  ): NormState[Leaf] = recursiveId match {
    case None     => State.pure(Leaf.Lambda(al, scope, p, extEnv, None))
    case Some(id) => State.pure(Leaf.Lambda(al, scope, p, extEnv, recursiveId))
  }

  def letToLeaf(
      l: TypedExpr.Let[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] =
    l.recursive match {
      case RecursionKind.Recursive =>
        val lv: LazyValue =
          LazyValue(l.expr, scope, p, extEnv, Some(l.arg.asString))
        val nextScope = scope + (l.arg.asString -> lv)
        evalToLeaf(l.in, nextScope, p, extEnv)

      case _ =>
        val nextScope =
          scope + (l.arg.asString -> LazyValue(l.expr, scope, p, extEnv))
        evalToLeaf(l.in, nextScope, p, extEnv)
    }

  def appToLeaf(
      a: TypedExpr.App[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] = for {
    fn <- evalToLeaf(a.fn, scope, p, extEnv)
    arg = LazyValue(a.arg, scope, p, extEnv)
    leaf <- applyLeaf(fn, arg, p, extEnv)
  } yield leaf

  def literalToLeaf(
      l: TypedExpr.Literal[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T]
  ): NormState[Leaf] = State.pure(Leaf.Literal(l))

  def matchToLeaf(
      m: TypedExpr.Match[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] = simplifyMatch(m, scope, p, extEnv).flatMap(_.toLeaf)

  def collectFirst[A, B, S[_]](
      lstInit: List[A],
      fn: (A => S[Option[B]])
  )(implicit mon: cats.Monad[S]): S[Option[B]] = mon.tailRecM(lstInit) {
    case Nil => mon.pure(Right(None))
    case h :: tail =>
      val nsOptB = fn(h)
      mon.map(nsOptB) {
        case None  => Left(tail)
        case someV => Right(someV)
      }
  }

  def simplifyMatch(
      mtch: TypedExpr.Match[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[ExpressionValue] = {
    collectFirst[
      (Pattern[(PackageName, Constructor), Type], TypedExpr[T]),
      (
          Pattern[(PackageName, Constructor), Type],
          PatternEnv[ExpressionValue],
          TypedExpr[T]
      ),
      NormState
    ](
      mtch.branches.toList,
      { case (pat, result) =>
        ExpressionValueMaybeBind(pat, p)
          .apply(LazyValue(mtch.arg, scope, p, extEnv), Map.empty)
          .map {
            case Matches(env) => Some((pat, env, result))
            case NoMatch      => None
            // $COVERAGE-OFF$
            case NotProvable =>
              sys.error("For value we should never be NotProvable")
            // $COVERAGE-ON$
          }
      }
    ).map(_.get).map {
      case (_, patEnv: Map[Bindable, ExpressionValue], result) =>
        LazyValue(
          result,
          scope ++ (patEnv.map { case (k, v) => (k.asString, v) }),
          p,
          extEnv
        )
    }
  }

  private def nameKindLetToLeaf(
      name: Identifier.Bindable,
      recursive: RecursionKind,
      expr: TypedExpr[T],
      pack: Package.Typed[T],
      extEnv: ExtEnv,
      scope: Map[String, ExpressionValue],
      selfValue: ExpressionValue
  ): NormState[Leaf] =
    for {
      lookup <- State.inspect {
        lets: Map[(PackageName, Identifier), ExpressionValue] =>
          lets.get((pack.name, name))
      }
      outExpr <- lookup match {
        case Some(res) =>
          res.toLeaf
        case None =>
          recursive match {
            case RecursionKind.Recursive =>
              val nextScope = scope + (name.asString -> selfValue)
              val v: ExpressionValue = LazyValue(expr, nextScope, pack, extEnv)

              for {
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), ExpressionValue] =>
                    lets + ((pack.name, name) -> v)
                }
                res <- v.toLeaf
              } yield res
            case _ =>
              val v = LazyValue(expr, scope, pack, extEnv)
              for {
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), ExpressionValue] =>
                    lets + ((pack.name, name) -> v)
                }
                res <- v.toLeaf
              } yield res
          }
      }
    } yield outExpr

  def evalToLeaf(
      expr: TypedExpr[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv,
      recursiveId: Option[(String, ExpressionValue)] = None
  ): NormState[Leaf] =
    expr match {
      case a @ TypedExpr.Annotation(_, _, _) =>
        annotationToLeaf(a, scope, p, extEnv)
      case g @ TypedExpr.Generic(_, _)  => genericToLeaf(g, scope, p, extEnv)
      case v @ TypedExpr.Local(_, _, _) => localToLeaf(v, scope, p)
      case v @ TypedExpr.Global(_, _, _, _) => globalToLeaf(v, scope, p, extEnv)
      case al @ TypedExpr.AnnotatedLambda(_, _, _, _) =>
        annotatedLambdaToLeaf(al, scope, p, extEnv, recursiveId)
      case a @ TypedExpr.App(_, _, _, _)    => appToLeaf(a, scope, p, extEnv)
      case l @ TypedExpr.Let(_, _, _, _, _) => letToLeaf(l, scope, p, extEnv)
      case l @ TypedExpr.Literal(_, _, _)   => literalToLeaf(l, scope, p)
      case m @ TypedExpr.Match(_, _, _)     => matchToLeaf(m, scope, p, extEnv)

    }

  val nvToList: ExpressionValue => NormState[Option[List[ExpressionValue]]] = {
    normalValue =>
      val F = implicitly[cats.FlatMap[NormState]]

      def loop(
          nvInit: ExpressionValue,
          accInit: List[ExpressionValue]
      ): NormState[List[ExpressionValue]] = F.tailRecM((nvInit, accInit)) {
        case (nv, acc) =>
          nv.toStruct(rankn.DataFamily.Enum)
            .map(_.get match {
              case (0, _)          => Right(acc)
              case (1, List(h, t)) => Left((t, h :: acc))
              case _               =>
                // $COVERAGE-OFF$ this should be unreachable
                sys.error(
                  "Type checking should only allow this to be applied to a list struct"
                )
              // $COVERAGE-ON$
            })
      }
      loop(normalValue, Nil).map(_.reverse).map(Some(_))
  }

  def nvFromList(
      p: Package.Typed[T]
  ): List[ExpressionValue] => ExpressionValue = { lst =>
    @tailrec
    def loop(vals: List[ExpressionValue], acc: StructValue): StructValue =
      vals match {
        case Nil => acc
        case h :: tail =>
          loop(
            tail,
            StructValue(
              1,
              List(h, acc),
              rankn.DataFamily.Enum
            )
          )
      }
    loop(lst.reverse, StructValue(0, Nil, rankn.DataFamily.Enum))
  }

  type Applyable =
    Either[Value, (TypedExpr.AnnotatedLambda[T], List[ExpressionValue])]
  type ToLFV = Option[ExpressionValue => Future[Value]]

  val self = this
  case class ExprFnValue(
      toExprFn: (ExpressionValue, ExpressionEvaluation[T]) => Value
  ) extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v), self)
    }
  }

  case class ExpressionFnValue(
      lambda: TypedExpr[T],
      arg: Bindable,
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv,
      startState: Map[(PackageName, Identifier), ExpressionValue]
  ) extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      evalToValue(lambda, scope + (arg.asString -> ComputedValue(v)), p, extEnv)
        .run(startState)
        .value
        ._2
    }
  }

  def attemptExprFn(
      v: Value
  ): Either[
    (ExpressionValue, ExpressionEvaluation[T]) => Value,
    Value => Value
  ] = v match {
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
      arg: ExpressionValue,
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Leaf] = applyable match {
    case Leaf.Lambda(lambda, scope, _, _, recursiveId) => {
      // By evaluating when we add to the scope we don't have to overflow the stack later when we
      // need to use the value
      val _ = arg match {
        case lv @ LazyValue(_, _, _, _, _) => {
          lv.toValue
          ()
        }
        case _ => ()
      }
      val nextScope =
        (scope + (lambda.arg.asString -> arg)) ++ recursiveId.toList.toMap
      LazyValue(lambda.expr, nextScope, p, extEnv).toLeaf
    }
    case Leaf.Value(lfv) =>
      lfv.toValue.flatMap { v =>
        attemptExprFn(v) match {
          case Left(eFn) =>
            ComputedValue(eFn(arg, self)).toLeaf
          case Right(fn) => {
            arg.toValue.flatMap { argV =>
              ComputedValue(fn(argV)).toLeaf
            }
          }
        }
      }
    case Leaf.Struct(n, values, df) =>
      State.pure(Leaf.Struct(n, values.:+(arg), df))
    // $COVERAGE-OFF$ structs aren't applyable
    case _ => sys.error("structs aren't applyable")
    // $COVERAGE-ON$
  }

  case class ExpressionValueMaybeBind(
      pat: Pattern[
        (PackageName, Identifier.Constructor),
        rankn.Type
      ],
      p: Package.Typed[T]
  ) extends MaybeBind[ExpressionValue](pat) {
    def toLitValue(t: ExpressionValue): NormState[Option[LitValue]] =
      nvToLitValue(t)
    def toStruct(t: ExpressionValue, df: DataFamily) =
      nvToStruct(t, df)
    def toList(t: ExpressionValue): NormState[Option[List[ExpressionValue]]] =
      nvToList(t)
    def fromList(lst: List[ExpressionValue]): ExpressionValue =
      nvFromList(p)(lst)
    def fromString(str: String): ExpressionValue =
      ComputedValue(Value.Str(str))
    def maybeBind(
        pat: Pattern[
          (PackageName, Identifier.Constructor),
          rankn.Type
        ]
    ) = (v: ExpressionValue, env: PatternEnv[ExpressionValue]) =>
      ExpressionValueMaybeBind(pat, p).apply(v, env)
    def definedForCons(pc: (PackageName, Constructor)): rankn.DefinedType[Any] =
      pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2
  }

  def evaluateStruct(
      variant: Int,
      args: List[ExpressionValue],
      df: DataFamily
  ): NormState[Value] = df match {
    case rankn.DataFamily.Enum =>
      traverseForNS(args.map(_.toValue)).map(lst =>
        Value.SumValue(
          variant,
          Value.ProductValue.fromList(
            lst
          )
        )
      )
    case rankn.DataFamily.Nat =>
      if (args.isEmpty) {
        State.pure(Value.ExternalValue(BigInteger.valueOf(0)))
      } else {

        args.head.toValue
          .map(
            _.asExternal.toAny
              .asInstanceOf[BigInteger]
              .add(BigInteger.ONE)
          )
          .map(Value.ExternalValue(_))
      }
    case rankn.DataFamily.Struct =>
      traverseForNS(args.map(_.toValue))
        .map(Value.ProductValue.fromList(_))
    case rankn.DataFamily.NewType => args.head.toValue
  }

  def evalToValue(
      ne: TypedExpr[T],
      scope: Map[String, ExpressionValue],
      p: Package.Typed[T],
      extEnv: ExtEnv
  ): NormState[Value] = LazyValue(ne, scope, p, extEnv).toValue

  def evaluate(
      ne: TypedExpr[T],
      extEnv: Map[Identifier, Eval[Value]],
      p: Package.Typed[T]
  ): (Eval[Value], rankn.Type) =
    (evalToValue(ne, Map.empty, p, extEnv).run(Map.empty).map(_._2), ne.getType)

  def exprFn(
      arity: Int,
      wrapper: (
          rankn.Type,
          List[ExpressionValue]
      ) => Value
  ): FfiCall = {
    def evalExprFn(t: rankn.Type, revArgs: List[ExpressionValue]): ExprFnValue =
      if (revArgs.length + 1 < arity) {
        ExprFnValue { (arg, ev) =>
          new Value.FnValue(evalExprFn(t, arg :: revArgs))
        }
      } else {
        ExprFnValue { case (arg, ev) =>
          wrapper(t, (arg :: revArgs).reverse)
        }
      }
      
    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t, Nil)) }
  }

  type Pack = Package[Package.Interface, NonEmptyList[
    Referant[Variance]
  ], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[T], Any]]

  private def constructor(
      c: Constructor,
      dt: rankn.DefinedType[Any]
  ): Leaf.Struct = {
    val (enum, arity) =
      dt.constructors.toList.iterator.zipWithIndex.collectFirst {
        case (cf, idx) if cf.name == c => (idx, cf.args.size)
      }.get

    Leaf.Struct(enum, Nil, dt.dataFamily)
  }

  private def externalEnv(p: Package.Typed[T]): ExpressionEvaluation.ExtEnv = {
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
      p: Package.Typed[T]
  ): ExpressionEvaluation.ExtEnv =
    p.imports.iterator.flatMap { imp =>
      val pack = pm.toMap.get(imp.pack.name) match {
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

  def evaluateCollect(
      p: PackageName,
      fn: Pack => Option[(Identifier.Bindable, RecursionKind, TypedExpr[T])]
  ) = for {
    pack <- pm.toMap.get(p)
    (name, _, tpe) <- fn(pack)
    extEnv = externalEnv(pack) ++ importedEnv(pack)
  } yield (
    tpe,
    extEnv,
    pack
  )

  def evaluateLast(
      p: PackageName
  ) = evaluateCollect(p, { pack => pack.program.lets.lastOption })
    .map { case (tpe, extEnv, pack) =>
      evaluate(tpe, extEnv, pack)
    }

  def evalLastTest(p: PackageName) =
    evaluateCollect(p, { pack => Package.testValue(pack) })
      .map { case (tpe, extEnv, pack) =>
        evaluate(tpe, extEnv, pack)
      }
      .map(v => Eval.later(Test.fromValue(v._1.value)))

  def evaluateName(
      p: PackageName,
      name: Bindable
  ): Option[(Eval[Value], Type)] =
    evaluateCollect(
      p,
      { pack =>
        pack.program.lets.filter { case (n, _, _) => n == name }.lastOption
      }
    )
      .map { case (tpe, extEnv, pack) =>
        evaluate(tpe, extEnv, pack)
      }

  val valueToDoc: ValueToDoc = ValueToDoc({ case Type.Const.Defined(pn, t) =>
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
  })
}
