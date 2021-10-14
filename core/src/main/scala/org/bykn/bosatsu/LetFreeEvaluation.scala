package org.bykn.bosatsu

import cats.Eval
// import LetFreeConversion.LetFreeExpressionTag
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
   * for right hand evaluation. This has been implemented for T as a LetFreeExpression, a Value, and a LetFreeValue. When
   * attempting to bind it result in a Matches, NoMatch, or NotProvable which means there's not enough present information to
   * determine what this would do at runtime (eg this may be running at compile time.)
   */
  abstract class MaybeBind[T](
      pat: PatternPNC
  ) {
    def toLitValue(t: T): NormState[Option[LitValue], T]
    def toStruct(t: T, df: DataFamily): Option[(Int, List[T])]
    def toList(t: T): Option[List[T]]
    def fromList(lst: List[T]): T
    def fromString(str: String): T
    def maybeBind(
        pat: PatternPNC
    ): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]
    def definedForCons(pc: (PackageName, Constructor)): rankn.DefinedType[Any]

    val apply: (T, PatternEnv[T]) => NormState[PatternMatch[PatternEnv[T]], T] =
      pat match {
        case Pattern.WildCard => noop
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
            inner(v, env) match {
              case Matches(env1) => State.pure(Matches(env1 + (n -> v)))
              case notMatch      => State.pure(notMatch)
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
          ): (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
            matchList match {
              case NonEmptyList((Pattern.ListPart.WildList, Nil), Nil) => {
                (v, env) => Matches(env)
              }
              case NonEmptyList((Pattern.ListPart.NamedList(n), Nil), Nil) => {
                (v, env) =>
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
                        case Pattern.ListPart.WildList => env
                        case Pattern.ListPart.NamedList(n) =>
                          env + (n -> fromList(globList))
                      }
                      tail
                        .zip(suffix)
                        .foldLeft[PatternMatch[PatternEnv[T]]](
                          Matches(globEnv)
                        ) {
                          case (Matches(env), (x, pat)) =>
                            maybeBind(pat).apply(x, env)
                          case (noMatch, _) => noMatch
                        }
                    }
                }
              }
              case NonEmptyList((glob, suffix), h :: ltail) => {
                val tail = NonEmptyList(h, ltail)
                val tailMatcher: (StructList, PatternEnv[T]) => PatternMatch[
                  PatternEnv[T]
                ] =
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
          val (prefix, matchList) = listPatToMatchList(lp.parts)
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
              case Matches((remainder, env)) =>
                State.pure(matchesConsumer(remainder, env))
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
                    fnh(arg, acc) match {
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
          val itemFns = items.map(maybeBind(_))

          def processArgs(
              as: List[T],
              acc: PatternEnv[T]
          ): NormState[PatternMatch[PatternEnv[T]], T] = {
            // manually write out foldM hoping for performance improvements
            @annotation.tailrec
            def loop(
                vs: List[T],
                fns: List[(T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]],
                env: (PatternEnv[T], PatternMatch[PatternEnv[T]])
            ): NormState[PatternMatch[PatternEnv[T]], T] =
              vs match {
                case Nil => State.pure(env._2)
                case vh :: vt =>
                  fns match {
                    case fh :: ft =>
                      (fh(vh, env._1), env._2) match {
                        case (_, NoMatch) => State.pure(NoMatch)
                        case (NoMatch, _) => State.pure(NoMatch)
                        case (Matches(env1), Matches(_)) =>
                          loop(vt, ft, (env1, Matches(env1)))
                        case (Matches(env1), NotProvable) =>
                          loop(vt, ft, (env1, NotProvable))
                        case (NotProvable, _) =>
                          loop(vt, ft, (env._1, NotProvable))
                      }
                    case Nil =>
                      State.pure(
                        env._2
                      ) // mismatch in size, shouldn't happen statically
                  }
              }
            loop(as, itemFns, (acc, Matches(acc)))
          }

          val dt = definedForCons(pc)
          val df = dt.dataFamily

          if (dt.isStruct) {
            // this is a struct, which means we expect it
            { (arg: T, acc: PatternEnv[T]) =>
              toStruct(arg, df) match {
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
                toStruct(arg, df) match {
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
                ) match {
                  case Matches(listEnv: PatternEnv[T]) => {
                    val nsListPE = listEnv.toList.map { case (i, lst) =>
                      val lstOfStates: List[NormState[Option[LitValue], T]] =
                        toList(lst).get.map(toLitValue(_))
                      val nsStr = traverseForNS(lstOfStates)
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

    def toLeaf: NormState[Leaf]
    def toValue: NormState[Value]

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

  type NormState[A] = State[Map[(PackageName, Identifier), LetFreeValue], A]

  def expressionToString(expr: TypedExpr[T]): String = ???

  case class LazyValue(
      expression: TypedExpr[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ) extends LetFreeValue {
    def cleanedScope: List[(String, LetFreeValue)] = ???
    //  expression.tag.varSet.toList.sorted.map { n => (n, scope(n)) }

    lazy val toStructNat: NormState[Option[(Int, List[LetFreeValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: NormState[Option[(Int, List[LetFreeValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: NormState[Option[(Int, List[LetFreeValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: NormState[Option[(Int, List[LetFreeValue])]] =
      lazyToStructImpl(this, rankn.DataFamily.NewType)

    lazy val toLeaf = evalToLeaf(expression, scope, p)
    lazy val toValue = toLeaf.map(_.toValue)
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

    lazy val toLeaf = State.pure(Leaf.Value(this))
    val toValue = State.pure(value)
  }

  case class StructValue(n: Int, values: List[LetFreeValue], df: DataFamily)
      extends LetFreeValue {

    lazy val toLeaf: NormState[Leaf] = State.pure(Leaf.Struct(n, values, df))
    lazy val toValue: NormState[Value] =
      State.pure(evaluateStruct(n, values, df))

    lazy val toStructNat: Option[(Int, List[LetFreeValue])] = Some((n, values))
    lazy val toStructEnum: Option[(Int, List[LetFreeValue])] = Some((n, values))
    lazy val toStructStruct: Option[(Int, List[LetFreeValue])] = Some(
      (n, values)
    )
    lazy val toStructNewType: Option[(Int, List[LetFreeValue])] = Some(
      (n, values)
    )
  }

  def nvToLitValue(implicit
      extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => NormState[Option[LitValue]] = { lfv =>
    lfv.toValue.map(valueToLitValue)
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
  ) = lv.toLeaf.map {
    case Leaf.Struct(n, values, _)         => Some((n, values))
    case Leaf.Value(cv @ ComputedValue(_)) => cv.toStruct(df)
    // $COVERAGE-OFF$
    case other => sys.error(s"we should not get $other")
    // $COVERAGE-ON$
  }

  sealed abstract class Leaf {
    lazy val toValue = this match {
      case Leaf.Struct(enum, args, df)  => evaluateStruct(enum, args, df)
      case Leaf.Value(ComputedValue(v)) => v
      case Leaf.Lambda(lambda, scope, p) =>
        new Value.FnValue(
          LetFreeFnValue(lambda.expr, lambda.arg, scope, p)
        )
      case Leaf.Literal(TypedExpr.Literal(lit, _, _)) => Value.fromLit(lit)
    }
  }
  object Leaf {
    case class Struct(
        n: Int,
        values: List[LetFreeValue],
        df: DataFamily
    ) extends Leaf
    case class Lambda(
        lambda: TypedExpr.AnnotatedLambda[T],
        scope: Map[String, LetFreeValue],
        p: Package.Typed[T]
    ) extends Leaf
    case class Literal(expr: TypedExpr.Literal[T]) extends Leaf
    case class Value(value: ComputedValue) extends Leaf
    case class Constructor(constructor: NameKind.Constructor[T]) extends Leaf
  }

  def annotationToLeaf(
      a: TypedExpr.Annotation[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] =
    evalToLeaf(a.term, scope, p)

  def genericToLeaf(
      g: TypedExpr.Generic[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] =
    evalToLeaf(g.in, scope, p)

  def localToLeaf(
      v: TypedExpr.Local[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] = {
    scope(v.name.asString).toLeaf
  }

  private def norm(
      pack: Package.Typed[T],
      item: Identifier,
      scope: Map[String, LetFreeValue]
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
              scope
            )
          case c @ NameKind.Constructor(_, _, _, _) =>
            State.pure(Leaf.Constructor(c))
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            norm(pm.toMap(from.name), orig, Map.empty)

          case NameKind.ExternalDef(pn, n, defType) =>
            State.pure(Leaf.Value(ComputedValue(extEnv(n).value)))
        }
      case None =>
        sys.error(s"we didn't find an item $item in pack.name ${pack.name}")
    }

  def globalToLeaf(
      v: TypedExpr.Global[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
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

    norm(p, name, scope)
  }

  def annotatedLambdaToLeaf(
      al: TypedExpr.AnnotatedLambda[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] =
    State.pure(Leaf.Lambda(al, scope, p))

  def letToLeaf(
      l: TypedExpr.Let[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] =
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
  ): NormState[Leaf] = for {
    fn <- evalToLeaf(a.fn, scope, p)
    arg = LazyValue(a.arg, scope, p)
    leaf <- applyLeaf(fn, arg, p)
  } yield leaf

  def literalToLeaf(
      l: TypedExpr.Literal[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] = State.pure(Leaf.Literal(l))

  def matchToLeaf(
      m: TypedExpr.Match[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] = simplifyMatch(m, scope, p).toLeaf

  def simplifyMatch(
      mtch: TypedExpr.Match[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): LetFreeValue = {
    val (_, patEnv: Map[Bindable, LetFreeValue], result) = mtch.branches.toList
      .collectFirst(Function.unlift({ case (pat, result) =>
        LetFreeValueMaybeBind(pat, p)
          .apply(LazyValue(mtch.arg, scope, p), Map.empty) match {
          case Matches(env) => Some((pat, env, result))
          case NoMatch      => None
          // $COVERAGE-OFF$
          case NotProvable =>
            sys.error("For value we should never be NotProvable")
          // $COVERAGE-ON$
        }
      }))
      .get

    LazyValue(
      result,
      scope ++ (patEnv.map { case (k, v) => (k.asString, v) }),
      p
    )
  }

  private def nameKindLetToLeaf(
      name: Identifier.Bindable,
      recursive: RecursionKind,
      expr: TypedExpr[T],
      pack: Package.Typed[T],
      scope: Map[String, LetFreeValue]
  ): NormState[Leaf] =
    for {
      lookup <- State.inspect {
        lets: Map[(PackageName, Identifier), LetFreeValue] =>
          lets.get((pack.name, name))
      }
      outExpr <- lookup match {
        case Some(res) =>
          res.toLeaf
        case None =>
          recursive match {
            case RecursionKind.Recursive =>
              val v = LazyValue(expr, scope, pack)
              for {
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), LetFreeValue] =>
                    lets + ((pack.name, name) -> v)
                }
                res <- v.toLeaf
              } yield res
            case _ =>
              val v = LazyValue(expr, scope, pack)
              for {
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), LetFreeValue] =>
                    lets + ((pack.name, name) -> v)
                }
                res <- v.toLeaf
              } yield res
          }
      }
    } yield outExpr

  def evalToLeaf(
      expr: TypedExpr[T],
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): NormState[Leaf] =
    expr match {
      case a @ TypedExpr.Annotation(_, _, _) => annotationToLeaf(a, scope, p)
      case g @ TypedExpr.Generic(_, _)       => genericToLeaf(g, scope, p)
      case v @ TypedExpr.Local(_, _, _)      => localToLeaf(v, scope, p)
      case v @ TypedExpr.Global(_, _, _, _)  => globalToLeaf(v, scope, p)
      case al @ TypedExpr.AnnotatedLambda(_, _, _, _) =>
        annotatedLambdaToLeaf(al, scope, p)
      case a @ TypedExpr.App(_, _, _, _)    => appToLeaf(a, scope, p)
      case l @ TypedExpr.Let(_, _, _, _, _) => letToLeaf(l, scope, p)
      case l @ TypedExpr.Literal(_, _, _)   => literalToLeaf(l, scope, p)
      case m @ TypedExpr.Match(_, _, _)     => matchToLeaf(m, scope, p)

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
    lst =>
      @tailrec
      def loop(vals: List[LetFreeValue], acc: StructValue): StructValue =
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

  type Applyable = Either[Value, (LetFreeExpression.Lambda, List[LetFreeValue])]
  type ToLFV = Option[LetFreeValue => Future[Value]]

  case class ExprFnValue(toExprFn: LetFreeValue => Value)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v))
    }
  }

  case class LetFreeFnValue(
      lambda: TypedExpr[T],
      arg: Bindable,
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ) extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      evalToValue(lambda, scope + (arg.asString -> ComputedValue(v)), p)
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
  ): NormState[Leaf] = applyable match {
    case Leaf.Lambda(lambda, scope, _) => {
      // By evaluating when we add to the scope we don't have to overflow the stack later when we
      // need to use the value
      val _ = arg match {
        case lv @ LazyValue(_, _, _) => {
          lv.toValue
          ()
        }
        case _ => ()
      }
      val nextScope = scope + (lambda.arg.asString -> arg)
      LazyValue(lambda.expr, nextScope, p).toLeaf
    }
    case Leaf.Value(lfv) =>
      lfv.toValue.flatMap { v =>
        attemptExprFn(v) match {
          case Left(eFn) =>
            ComputedValue(eFn(arg)).toLeaf
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

  case class LetFreeValueMaybeBind(
      pat: org.bykn.bosatsu.Pattern[
        (PackageName, Identifier.Constructor),
        rankn.Type
      ],
      p: Package.Typed[T]
  ) extends MaybeBind[LetFreeValue](pat) {
    def toLitValue(t: LetFreeValue): NormState[Option[LitValue]] =
      nvToLitValue(extEnv, cache)(t)
    def toStruct(t: LetFreeValue, df: DataFamily) =
      nvToStruct(extEnv, cache)(t, df)
    def toList(t: LetFreeValue): Option[List[LetFreeValue]] =
      nvToList(extEnv, cache)(t)
    def fromList(lst: List[LetFreeValue]): LetFreeValue =
      nvFromList(p)(lst)
    def fromString(str: String): LetFreeValue =
      ComputedValue(Value.Str(str))
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
      scope: Map[String, LetFreeValue],
      p: Package.Typed[T]
  ): Value = LazyValue(ne, scope, p).toValue

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
