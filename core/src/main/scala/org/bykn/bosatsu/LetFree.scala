package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._
import rankn._
import scala.collection.immutable.SortedMap

import Identifier.Constructor
import org.bykn.bosatsu.LetFreePattern.ListPart
import scala.annotation.tailrec 
import scala.collection.immutable.IntMap

/*
 * The LetFreeExpression is an attempting to be a stripped down version of a bosatsu expression
 * to give a semantic representation of the output of the computation. It is not intended to represent
 * the computation that produces the output, even though it does contain enough information to be
 * evaluated. As such it can be used to sometimes tell if two expressions will produce the same 
 * outputs for cacheing purposes. It also makes functions inspectable which means it's possible
 * to translate them into code for extrenal computation (eg sql statements).
 * 
 * As the name implies, it expression eliminates Lets by inlining the values. This creates an issue
 * for how to represent a recursive Lets so we introduce a Recursion expression that is only valid
 * if it contains a Lambda, where the applying the argument is the recursive call. All variable names
 * are eliminated and replaced with De Bruijn indicies so that expressions get the same representation
 * regardless of variable name choice or nesting. Where possible Match and App are normalized,
 * even when it will produce a less efficient computation. No recurrsive functions or external functions
 * are applied as they are considered runtime concerns.
 */
sealed abstract class LetFreeExpression {

  /*
   * maxLambdaVar is to keep track of what is the largest de bruijn index
   * of lambda variables in the expression. This is useful because if this number
   * is positive then there are unbound variables and it should not be cached (unless
   * you want to be clever about cacheing values for when they are bound in an outer
   * scope). And when they are negative it implies there are eta reduction opportunities.
   * None essentially means -Infinity as either there are no linked expressions
   * or there are no lambda variables used in the linked expressions
   */
  def maxLambdaVar: Option[Int]

  def asString: String = {
    def escapeString(unescaped: String) = StringUtil.escape('\'', unescaped)

    this match {
      case LetFreeExpression.App(fn, arg) => s"App(${fn.asString},${arg.asString})"
      case LetFreeExpression.ExternalVar(pack, defName, tpe) => s"ExternalVar('${escapeString(pack.asString)}','${escapeString(defName.asString)}', '${escapeString(TypeRef.fromTypes(None, tpe :: Nil).apply(tpe).toDoc.render(100))}')"
      case LetFreeExpression.Match(arg, branches) => {
        val serBranches = branches.toList.map {case (lfp, lfe) => s"${lfp.asString},${lfe.asString}"}.mkString(",")
        s"Match(${arg.asString},$serBranches)"
      }
      case LetFreeExpression.LambdaVar(index) => s"LambdaVar($index)"
      case LetFreeExpression.Lambda(expr) => s"Lambda(${expr.asString})"
      case LetFreeExpression.Struct(enum, args, _) => s"Struct($enum,${args.map(_.asString).mkString(",")})"
      case LetFreeExpression.Literal(toLit) => toLit match {
        case Lit.Str(toStr) => s"Literal('${escapeString(toStr)}')"
        case Lit.Integer(bigInt) => s"Literal($bigInt)"
      }
      case LetFreeExpression.Recursion(lambda) => s"Recursion(${lambda.asString})"
    }
  }

  /*
   * varSet is the set of indices of variables referenced in the passed in environment
   */
  lazy val varSet: Set[Int] =
    this match {
      case LetFreeExpression.Lambda(expr) =>
        expr.varSet.collect { case n if n > 0 => n - 1 }
      case LetFreeExpression.App(fn, arg)         => fn.varSet.union(arg.varSet)
      case LetFreeExpression.ExternalVar(_, _, _) => Set.empty
      case LetFreeExpression.Match(arg, branches) =>
        branches
          .map { branch =>
            val varCount = LetFreePattern.varCount(branch._1)
            branch._2.varSet.map(_ - varCount).filter(_ >= 0)
          }
          .foldLeft(arg.varSet) { case (s1, s2) => s1.union(s2) }
      case LetFreeExpression.Struct(enum, args, _) =>
        args.foldLeft(Set[Int]()) { case (s, arg) => s ++ arg.varSet }
      case LetFreeExpression.Literal(_)        => Set.empty
      case LetFreeExpression.Recursion(lambda) => lambda.varSet
      case LetFreeExpression.LambdaVar(name)   => Set(name)
    }
}

object LetFreeExpression {
  case class App(fn: LetFreeExpression, arg: LetFreeExpression)
  extends LetFreeExpression {
    def maxLambdaVar = (fn.maxLambdaVar.toList ++ arg.maxLambdaVar.toList)
      .reduceLeftOption(Math.max)
  }
  case class ExternalVar(pack: PackageName, defName: Identifier, tpe: rankn.Type)
  extends LetFreeExpression {
    def maxLambdaVar = None
  }
  case class Match(arg: LetFreeExpression,
    branches: NonEmptyList[(LetFreePattern, LetFreeExpression)])
  extends LetFreeExpression {
    def maxLambdaVar =
      (arg.maxLambdaVar.toList ++ branches.toList.flatMap(_._2.maxLambdaVar))
        .reduceLeftOption(Math.max)
  }
  case class LambdaVar(index: Int) extends LetFreeExpression {
    def maxLambdaVar = Some(index)
  }
  /*
   * It is reasonable to ask how you can define a lambda without an identifier
   * for its argument in its expression. This is a benefit of de bruijn indexing.
   * When all lambdas have exactly one argument you identify the var by how many
   * lambdas out you have to travel.
   *
   * eg \x -> \y -> [y,x] would have normalization Lambda(Lambda(Apply(Apply(List, LambdaVar(1)), LambdaVar(0))))
   *
   * ref: https://en.wikipedia.org/wiki/De_Bruijn_index
   */
  case class Lambda(expr: LetFreeExpression) extends LetFreeExpression {
    def maxLambdaVar = expr.maxLambdaVar.map(_ - 1)
  }
  case class Struct(enum: Int, args: List[LetFreeExpression], dataFamily: DataFamily) extends LetFreeExpression {
    def maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends LetFreeExpression {
    def maxLambdaVar = None
  }
  case class Recursion(lambda: LetFreeExpression) extends LetFreeExpression {
    def maxLambdaVar = lambda.maxLambdaVar
  }

}

sealed abstract class LetFreePattern {
  def escapeString(unescaped: String) = StringUtil.escape('\'', unescaped)
  def asString: String =
    this match {
      case LetFreePattern.WildCard => "WildCard"
      case LetFreePattern.Literal(toLit) => toLit match {
        case Lit.Str(toStr) => s"Literal('${escapeString(toStr)}')"
        case Lit.Integer(bigInt) => s"Literal($bigInt)"
      }
      case LetFreePattern.Var(name) => s"Var($name)"
      case LetFreePattern.Named(name, pat) => s"Named($name,${pat.asString})"
      case LetFreePattern.ListPat(parts) => {
        val inside = parts.map {
          case Left(name) => s"Left(${name.map(_.toString).getOrElse("")})"
          case Right(pat) => s"Right(${pat.asString})"
        }.mkString(",")
        s"ListPat($inside)"
      }
      case LetFreePattern.PositionalStruct(name, params, df) => s"PositionalStruct(${name.map(_.toString).getOrElse("")},${params.map(_.asString).mkString(",")})"
      case LetFreePattern.Union(head, rest) => s"Union(${head.asString},${rest.toList.map(_.asString).mkString(",")})"
      case LetFreePattern.StrPat(parts) => {
        val inside = parts.map {
          case LetFreePattern.StrPart.WildStr => "WildStr"
          case LetFreePattern.StrPart.NamedStr(name) => s"NamedStr($name)"
          case LetFreePattern.StrPart.LitStr(toString) => s"LitStr($toString)"
        }.toList.mkString(",")
        s"StrPat($inside)"
      }
    }
}

object LetFreePattern {
  /*
   * varCount gets the number of variables assigned in the pattern by finding the largest index and adding one to it. floor is the candidate so far.
   */
  def varCount(pattern: LetFreePattern): Int = {
    @tailrec
    def loop(floor: Int, patterns: List[LetFreePattern]): Int =
      patterns match {
        case head :: rest =>
          head match {
            case LetFreePattern.WildCard   => loop(floor, rest)
            case LetFreePattern.Literal(_) => loop(0, rest)
            case LetFreePattern.Var(name)  => loop(floor.max(name + 1), rest)
            case LetFreePattern.Named(name, pat) =>
              loop(floor.max(name + 1), pat :: rest)
            case LetFreePattern.ListPat(parts) =>
              val result = parts.foldLeft((floor, rest)) {
                case ((fl, lst), Left(None)) => (fl, lst)
                case ((fl, lst), Left(Some(n))) => (fl.max(n + 1), lst)
                case ((fl, lst), Right(pat)) => (fl, pat :: lst)
              }
              loop(result._1, result._2)
            case LetFreePattern.PositionalStruct(None, params, df) => loop(floor, params ++ rest)
            case LetFreePattern.PositionalStruct(Some(name), params, df) => loop(floor.max(floor + 1), params ++ rest)
            case LetFreePattern.Union(uHead, _) => loop(floor, uHead :: rest)
            case LetFreePattern.StrPat(parts) =>
              val newFloor = (NonEmptyList.of(floor) ++ (parts.collect {
                case LetFreePattern.StrPart.NamedStr(name) => name + 1
              })).maximum
              loop(newFloor, rest)
          }
        case Nil => floor
      }
    loop(0, List(pattern))
  }

  case object WildCard extends LetFreePattern
  case class Literal(toLit: Lit) extends LetFreePattern
  case class Var(name: Int) extends LetFreePattern
  /**
   * Patterns like foo @ Some(_)
   * @ binds tighter than |, so use ( ) with groups you want to bind
   */
  case class Named(name: Int, pat: LetFreePattern) extends LetFreePattern

  type ListPart = Either[ListPart.Glob, LetFreePattern]
  object ListPart {
    type Glob = Option[Int]
  }

  case class ListPat(parts: List[ListPart]) extends LetFreePattern {
    lazy val toMatchList: (List[LetFreePattern], List[(ListPart.Glob, List[LetFreePattern])]) = {
      def loop(parts: List[ListPart]): (List[LetFreePattern], List[(ListPart.Glob, List[LetFreePattern])]) = parts match {
        case Nil => (Nil, Nil)
        case Left(None) :: Right(WildCard) :: tail => loop(Right(WildCard) :: Left(None) :: tail)
        case Left(glob) :: tail => loop(tail) match {
          case (prefix, rest) => (Nil, (glob, prefix) :: rest)
        }
        case Right(item) :: tail => loop(tail) match {
          case (prefix, rest) => (item :: prefix, rest)
        }
      }
      loop(parts)
    }     
  }
  case class PositionalStruct(name: Option[Int], params: List[LetFreePattern], dataFamily: DataFamily) extends LetFreePattern
  case class Union(head: LetFreePattern, rest: NonEmptyList[LetFreePattern]) extends LetFreePattern
  case class StrPat(parts: NonEmptyList[StrPart]) extends LetFreePattern

  sealed abstract class StrPart
  object StrPart {
    final case object WildStr extends StrPart
    final case class NamedStr(idx: Int) extends StrPart
    final case class LitStr(asString: String) extends StrPart
  }
}

object LetFreeConversion {
  case class ExpressionKeyTag[T](lfe: T, children: Set[T])
  type LetFreeExpressionTag = ExpressionKeyTag[LetFreeExpression]
  def LetFreeExpressionTag(lfe: LetFreeExpression, children: Set[LetFreeExpression]) = ExpressionKeyTag(lfe, children)
  type LetFreePM = PackageMap.Typed[(Declaration, LetFreeExpressionTag)]
  type LetFreePac = Package.Typed[(Declaration, LetFreeExpressionTag)]

  type PatternEnv[T] = IntMap[T]

  sealed trait PatternMatch[+A]
  case class Matches[A](env: A) extends PatternMatch[A]
  case object NoMatch extends PatternMatch[Nothing]
  case object NotProvable extends PatternMatch[Nothing]

  def noop[T]: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = { (_, env) => Matches(env) }
  def neverMatch[T]: (T, PatternEnv[T]) => PatternMatch[Nothing] = { (_, _) => NoMatch }

  /*
   * Takes bosatsu's List structure and converts it into a scala List, if it can.
   */
  def structListAsList(
      lfe: LetFreeExpression
  ): Option[List[LetFreeExpression]] = {
    lfe match {
      case LetFreeExpression.Struct(0, _, _) => Some(Nil)
      case LetFreeExpression.Struct(1, value :: tail :: NIl, _) =>
        structListAsList(tail).map(value :: _)
      case _ => None
    }
  }

  def listAsStructList(lst: List[LetFreeExpression]): LetFreeExpression =
    lst.foldRight(LetFreeExpression.Struct(0, Nil, DataFamily.Enum)) { case (lfe, acc) => LetFreeExpression.Struct(1, List(lfe, acc), DataFamily.Enum) }

  case class LitValue(toAny: Any) {
    def equivToLit(lit: Lit) = toAny == lit.unboxToAny
  }

  object LitValue {
    def fromLit(lit: Lit) = LitValue(lit.unboxToAny)
  }

  val neToLitValue: LetFreeExpression => Option[LitValue] = {
    case LetFreeExpression.Literal(lit) => Some(LitValue.fromLit(lit))
    case _ => None
  }
  val neToStruct: (LetFreeExpression, DataFamily) => Option[(Int, List[LetFreeExpression])] = {
    case (LetFreeExpression.Struct(enum, args, _), df) => Some((enum, args))
    case _ => None
  }
  val neToList: LetFreeExpression => Option[List[LetFreeExpression]] = structListAsList(_)
  val neFromList: List[LetFreeExpression] => LetFreeExpression = listAsStructList(_)

  /*
   * MaybeBind is an implementation of determining if a T satisfies a LetFreePattern and if it does it creates an environment
   * for right hand evaluation. This has been implemented for T as a LetFreeExpression, a Value, and a LetFreeValue. When
   * attempting to bind it result in a Matches, NoMatch, or NotProvable which means there's not enough present information to
   * determine what this would do at runtime (eg this may be running at compile time.)
   */
  abstract class MaybeBind[T](pat: LetFreePattern) {
    def toLitValue(t: T): Option[LitValue]
    def toStruct(t: T, df: DataFamily): Option[(Int, List[T])]
    def toList(t: T): Option[List[T]]
    def fromList(lst: List[T]): T
    def fromString(str: String): T
    def maybeBind(pat: LetFreePattern): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]
  
    val apply: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = pat match {
      case LetFreePattern.WildCard => noop
      case LetFreePattern.Literal(lit) =>
        { (v, env) => toLitValue(v) match {
          case Some(lv) => if (lv.equivToLit(lit)) Matches(env) else NoMatch
          case _ => NotProvable
        }}
      case LetFreePattern.Var(n) =>
        { (v, env) => Matches(env + (n ->  v)) }
      case LetFreePattern.Named(n, p) =>
        val inner = maybeBind(p)

        { (v, env) =>
          inner(v, env) match {
            case Matches(env1) => Matches(env1 + (n -> v))
            case notMatch => notMatch
          }
        }
      case lp@LetFreePattern.ListPat(_) => {
        sealed abstract class StructListResult
        case object Empty extends StructListResult
        case class Cons(h: T, t: StructList) extends StructListResult
        case class StructList(asT: T) {
          lazy val parts = {
            val strct = toStruct(asT, DataFamily.Enum)
            strct.map { 
              case (1, h :: t :: Nil) => Cons(h, StructList(t))
              case (0, Nil) => Empty
              case _ => sys.error("type checking should ensure this is always a bosatsu List")
            }
          }
        }

        def consumePrefix(prefix: List[LetFreePattern]):(StructList, PatternEnv[T]) => PatternMatch[(StructList, PatternEnv[T])] = { 
          val matchers = prefix.map(maybeBind(_))
          val initial: (StructList, PatternEnv[T]) => PatternMatch[(StructList, PatternEnv[T])] = {(x: StructList, env: PatternEnv[T]) => Matches((x, env))}
          matchers.foldRight[(StructList, PatternEnv[T]) => PatternMatch[(StructList, PatternEnv[T])]](initial) {
            case (fn, acc) => {(x: StructList, env: PatternEnv[T]) =>
              x.parts match {
                case None => NotProvable
                // Possible optimization below because we hit the end of the list and there's no point in going on
                case Some(Empty) => NoMatch
                case Some(Cons(head, tail)) => fn(head, env) match {
                  case Matches(nextEnv) => acc(tail, nextEnv)
                  case NotProvable => NotProvable
                  case NoMatch => NoMatch
                }
              }
            }
          }
        }
        def consumeMatch(glob: ListPart.Glob, suffix: List[LetFreePattern]): (StructList, PatternEnv[T]) => PatternMatch[(StructList, PatternEnv[T])] = {
          val suffixConsumer = consumePrefix(suffix)
          @tailrec
          def loop(v: StructList, env: PatternEnv[T], acc: List[T]): PatternMatch[(StructList, PatternEnv[T], List[T])] = suffixConsumer(v, env) match {
            case Matches((vRest, nextEnv)) => Matches((vRest, nextEnv, acc))
            case NotProvable => NotProvable
            case NoMatch => v.parts match {
              case None => NotProvable
              case Some(Empty) => NoMatch
              case Some(Cons(h, tail)) => loop(tail, env, h :: acc)
            }
          }
          {(x: StructList, env: PatternEnv[T]) => loop(x, env, Nil) match {
            case Matches((x, env, acc)) => glob match {
              case None => Matches((x, env))
              case Some(n) => Matches((x, env + (n -> fromList(acc.reverse))))
            }
            case NoMatch => NoMatch
            case NotProvable => NotProvable
          }}
        }

        def loop(matchList: NonEmptyList[(ListPart.Glob, List[LetFreePattern])]):
          (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = matchList match {
          case NonEmptyList((None, Nil), Nil) => {(v, env) => Matches(env)}
          case NonEmptyList((Some(n), Nil), Nil) => {(v, env) => Matches(env + (n -> v.asT))}
          case NonEmptyList((glob, suffix), Nil) => {(v, env) =>
            toList(v.asT) match {
              case None => NotProvable
              case Some(lst) => if (lst.length < suffix.length) NoMatch else {
                val (globList, tail) = lst.splitAt(lst.length - suffix.length)
                val globEnv = glob match {
                  case None => env
                  case Some(n) => env + (n -> fromList(globList))
                }
                tail.zip(suffix).foldLeft[PatternMatch[PatternEnv[T]]](Matches(globEnv)) {
                  case (Matches(env), (x, pat)) => maybeBind(pat).apply(x, env)
                  case (noMatch, _) => noMatch
                }
              } 
            }
          }
          case NonEmptyList((glob, suffix), h :: ltail) => {
            val tail = NonEmptyList(h, ltail)
            val tailMatcher: (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = loop(tail)
            val prefixConsumer: (StructList, PatternEnv[T]) => PatternMatch[(StructList, PatternEnv[T])] = consumeMatch(glob, suffix)
            val result: (StructList, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = { (v, env) => prefixConsumer(v, env) match {
              case Matches((remainder, nextEnv)) => tailMatcher(remainder, nextEnv)
              case NoMatch => NoMatch
              case NotProvable => NotProvable
            }}
            result
          }
        }
        val (prefix, matchList) = lp.toMatchList
        val prefixConsumer = consumePrefix(prefix)
        val matchesConsumer = NonEmptyList.fromList(matchList) match {
          // $COVERAGE-OFF$ this case is actually optimized away. If there are no globs the listpattern becomes a positionalstruct
          case None => { (v: StructList, env:PatternEnv[T]) => v.parts match {
            case None => NotProvable
            case Some(Empty) => Matches(env)
            case Some(_) => NoMatch
          }}
          // $COVERAGE-ON$
          case Some(lst) => loop(lst)
        }
        (v, env) => {
          val structList = StructList(v)
          prefixConsumer(structList, env) match {
            case Matches((remainder, env)) => matchesConsumer(remainder, env)
            case NoMatch => NoMatch
            case NotProvable => NotProvable
          }
        }
      }

      case LetFreePattern.Union(h, t) =>
        // we can just loop expanding these out:
        def loop(ps: List[LetFreePattern]): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
          ps match {
            case Nil => neverMatch
            case head :: tail =>
              val fnh = maybeBind(head)
              val fnt: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = loop(tail)
              val result: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = { case (arg, acc) =>
                fnh(arg, acc) match {
                  case NoMatch  => fnt(arg, acc)
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

        def processArgs(as: List[T], acc: PatternEnv[T]): PatternMatch[PatternEnv[T]] = {
          // manually write out foldM hoping for performance improvements
          @annotation.tailrec
          def loop(vs: List[T], fns: List[(T, PatternEnv[T]) => PatternMatch[PatternEnv[T]]], env: (PatternEnv[T], PatternMatch[PatternEnv[T]])): PatternMatch[PatternEnv[T]] =
            vs match {
              case Nil => env._2
              case vh :: vt =>
                fns match {
                  case fh :: ft =>
                    (fh(vh, env._1), env._2) match {
                      case (_, NoMatch) => NoMatch
                      case (NoMatch, _) => NoMatch
                      case (Matches(env1), Matches(_)) => loop(vt, ft, (env1, Matches(env1)))
                      case (Matches(env1), NotProvable) => loop(vt, ft, (env1, NotProvable))
                      case (NotProvable, _) => loop(vt, ft, (env._1, NotProvable))
                    }
                  case Nil => env._2 // mismatch in size, shouldn't happen statically
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
        case LetFreePattern.StrPat(parts) =>
          val listParts: List[LetFreePattern.ListPart] = parts.toList.flatMap {
            case LetFreePattern.StrPart.WildStr => List(Left(None))
            case LetFreePattern.StrPart.NamedStr(n) => List(Left(Some(n)))
            case LetFreePattern.StrPart.LitStr(str) => str.toList.map(c => Right(LetFreePattern.Literal(Lit.Str(c.toString))))
          }
          val listMaybeBind = maybeBind(LetFreePattern.ListPat(listParts))

          (v, env) => toLitValue(v) match {
              case None => NotProvable
              case Some(LitValue(str)) => listMaybeBind(fromList(str.asInstanceOf[String].toList.map(c => fromString(c.toString))), IntMap.empty) match {
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

  case class LetFreeExpressionMaybeBind(pat: LetFreePattern) extends MaybeBind[LetFreeExpression](pat) {
    def toLitValue(t: LetFreeExpression): Option[LitValue] = neToLitValue(t)
    def toStruct(t: LetFreeExpression, df: DataFamily): Option[(Int, List[LetFreeExpression])] = neToStruct(t, df)
    def toList(t: LetFreeExpression): Option[List[LetFreeExpression]] = neToList(t)
    def fromList(lst: List[LetFreeExpression]): LetFreeExpression = neFromList(lst)
    def fromString(str: String): LetFreeExpression = LetFreeExpression.Literal(Lit.Str(str))
    def maybeBind(pat: LetFreePattern): (LetFreeExpression, PatternEnv[LetFreeExpression]) => PatternMatch[PatternEnv[LetFreeExpression]] = LetFreeExpressionMaybeBind(pat).apply(_, _)
  }
  def findMatch(m: LetFreeExpression.Match) =
    m.branches.collectFirst(Function.unlift( { case (pat, result) =>
      LetFreeExpressionMaybeBind(pat).apply(m.arg, IntMap.empty) match {
        case Matches(env) => Some(Some((pat, env, result)))
        case NotProvable => Some(None)
        case NoMatch => None
      }
    })).get // Totallity of matches should ensure this will always find something unless something has gone terribly wrong

  def solveMatch(env: PatternEnv[LetFreeExpression], result: LetFreeExpression) =
    ((env.size - 1) to 0 by -1).map(env.get(_).get) // If this exceptions then somehow we didn't get enough names in the env
      .foldLeft(result) { case (lfe, arg) => LetFreeExpression.App(lfe, arg) }

  def normalOrderReduction(expr: LetFreeExpression): LetFreeExpression = {
    import LetFreeExpression._
    val res = headReduction(expr) match {
      case App(fn, arg) =>
        App(normalOrderReduction(fn), normalOrderReduction(arg))
      case extVar @ ExternalVar(_, _, _) => extVar
      // check for a match reduction opportunity (beta except for Match instead of lambda)
      case Match(arg, branches) =>
        Match(normalOrderReduction(arg), branches.map{ case (p, s) => (p, normalOrderReduction(s))})
      case lv @ LambdaVar(_)  => lv
      // check for eta reduction
      case Lambda(expr)       =>
        Lambda(normalOrderReduction(expr))
      case Struct(enum, args, df) => Struct(enum, args.map(normalOrderReduction(_)), df)
      case l @ Literal(_)     => l
      case Recursion(innerExpr) => Recursion(normalOrderReduction(innerExpr))
    }
    if (res != expr) {
      normalOrderReduction(res)
    } else {
      res
    }
  }

  @annotation.tailrec
  def headReduction(expr: LetFreeExpression): LetFreeExpression = {
    import LetFreeExpression._
    val nextExpr = expr match {
      // beta reduction
      case App(Lambda(nextExpr), arg) =>
        applyLambdaSubstituion(nextExpr, Some(arg), 0)
      // match reduction
      case m@Match(_, _) =>
        findMatch(m) match {
          case None => m
          case Some((pat, env, result)) =>
            solveMatch(env, result)
        }
      case Recursion(Lambda(innerExpr)) if(innerExpr.maxLambdaVar.map(_ < 0).getOrElse(true)) =>
        applyLambdaSubstituion(innerExpr, None, 0)
      // eta reduction
      case Lambda(App(innerExpr, LambdaVar(0))) if innerExpr.maxLambdaVar.map(_ < 0).getOrElse(true) =>
        applyLambdaSubstituion(innerExpr, Some(LambdaVar(0)), 0)
      case _ => expr
    }

    if (expr != nextExpr) {
      headReduction(nextExpr)
    } else {
      nextExpr
    }
  }

  private def applyLambdaSubstituion(expr: LetFreeExpression,
    subst: Option[LetFreeExpression],
    idx: Int): LetFreeExpression = {
      import LetFreeExpression._
      expr match {
        case App(fn, arg)                           =>
          App(applyLambdaSubstituion(fn, subst, idx),
            applyLambdaSubstituion(arg, subst, idx))
        case ext @ ExternalVar(_, _, _)                => ext
        case Match(arg, branches)                   =>
          Match(applyLambdaSubstituion(arg, subst, idx), branches.map {
            case (enum, expr) => (enum, applyLambdaSubstituion(expr, subst, idx))
          })
        case LambdaVar(varIndex) if varIndex == idx => subst.get
        case LambdaVar(varIndex) if varIndex > idx  => LambdaVar(varIndex - 1)
        case lv @ LambdaVar(_)                      => lv
        case Lambda(fn)                             => Lambda(applyLambdaSubstituion(fn, subst.map(incrementLambdaVars(_, 0)), idx + 1))
        case Struct(enum, args, df)                     =>
          Struct(enum, args.map(applyLambdaSubstituion(_, subst, idx)), df)
        case l @ Literal(_)                         => l
        case r @ Recursion(fn)                      => Recursion(applyLambdaSubstituion(fn, subst, idx))
      }
  }

  def incrementLambdaVars(expr: LetFreeExpression, lambdaDepth: Int): LetFreeExpression = {
    import LetFreeExpression._
    expr match {
      case App(fn, arg) =>
        App(incrementLambdaVars(fn, lambdaDepth),
          incrementLambdaVars(arg, lambdaDepth))
      case ext @ ExternalVar(_, _, _) => ext
      case Match(arg, branches) =>
        Match(incrementLambdaVars(arg, lambdaDepth), branches.map {
          case (enum, expr) => (enum, incrementLambdaVars(expr, lambdaDepth))
        })
      case LambdaVar(varIndex) if varIndex >= lambdaDepth => LambdaVar(varIndex + 1)
      case lv @ LambdaVar(_)                      => lv
      case Lambda(fn)                             => Lambda(incrementLambdaVars(fn, lambdaDepth + 1))
      case Struct(enum, args, df) =>
        Struct(enum, args.map(incrementLambdaVars(_, lambdaDepth)), df)
      case l @ Literal(_) => l
      case Recursion(fn) => Recursion(incrementLambdaVars(fn, lambdaDepth))
    }
  }
}

case class LetFreePackageMap(pm: PackageMap.Inferred) {
  import LetFreeConversion._
  import TypedExpr._

  val letFreePackageMap: LetFreePM = {
    val packs = pm.toMap.toList
    val normAll = packs.traverse { case (name, pack) =>
      letFreeConvertPackage(name, pack)
        .map((name, _))
    }
    PackageMap(SortedMap(normAll.run(Map.empty).value._2: _*))
  }

  def letFreeConvertExpr(expr: TypedExpr[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] = {
      expr match {
        case a@Annotation(_, _, _) => letFreeConvertAnnotation(a, env, p)
        case g@Generic(_, _, _) => letFreeConvertGeneric(g, env, p)
        case v@Local(_, _, _) => letFreeConvertLocal(v, env, p)
        case v@Global(_, _, _, _) => letFreeConvertGlobal(v, env, p)
        case al@AnnotatedLambda(_, _, _, _) => letFreeConvertAnnotatedLambda(al, env, p)
        case a@App(_, _, _, _) => letFreeConvertApp(a, env, p)
        case l@Let(_, _, _, _, _) => letFreeConvertLet(l, env, p)
        case l@Literal(_, _, _) => letFreeConvertLiteral(l, env, p)
        case m@Match(_, _, _) => letFreeConvertMatch(m, env, p)
      }
    }

  private def combineWithChildren(nt: LetFreeExpressionTag) = nt.children + nt.lfe

  def letFreeConvertAnnotation(a: Annotation[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      letFreeConvertExpr(a.term, env, p).map { term =>
        val lfeTag = term.tag._2
        val tag = (a.tag, lfeTag)
        a.copy(term=term, tag=tag)
      }

  def letFreeConvertGeneric(g: Generic[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      letFreeConvertExpr(g.in, env, p).map { in =>
        val lfeTag = in.tag._2
        val tag = (g.tag, lfeTag)
        g.copy(in=in, tag=tag)
      }

  def letFreeConvertLocal(v: Local[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] = {
      val lfeTag = env(v.name)
      State.pure(v.copy(tag=(v.tag, lfeTag)))
    }

  def letFreeConvertGlobal(v: Global[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      norm(p, v.name, v.tag, env).map { ne =>
        val lfeTag = getTag(ne)._2
        v.copy(tag=(v.tag, lfeTag))
      }
    
  def letFreeConvertAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] = {
      val nextEnv = (env - al.arg).mapValues { case ExpressionKeyTag(lfe, children) =>
        LetFreeExpressionTag(LetFreeConversion.incrementLambdaVars(lfe, 0), children)
      } + (al.arg -> LetFreeExpressionTag(LetFreeExpression.LambdaVar(0), Set.empty))

      for {
        eExpr <- letFreeConvertExpr(al.expr, nextEnv, p)
        ne = normalOrderReduction(LetFreeExpression.Lambda(eExpr.tag._2.lfe))
        children = combineWithChildren(eExpr.tag._2)
        lfeTag = LetFreeExpressionTag(ne, children)
      } yield al.copy(expr=eExpr, tag=(al.tag, lfeTag))
    }

  def letFreeConvertApp(a: App[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      for {
        efn <- letFreeConvertExpr(a.fn, env, p)
        earg <- letFreeConvertExpr(a.arg, env, p)
        ne = normalOrderReduction(LetFreeExpression.App(efn.tag._2.lfe, earg.tag._2.lfe))
        children = combineWithChildren(efn.tag._2) ++ combineWithChildren(earg.tag._2)
        lfeTag = LetFreeExpressionTag(ne, children)
      } yield a.copy(fn=efn, arg=earg, tag=(a.tag, lfeTag))

  def letFreeConvertLet(l: Let[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      l.recursive match {
        case RecursionKind.Recursive =>
          val nextEnv = (env - l.arg).mapValues { case ExpressionKeyTag(lfe, children) =>
            LetFreeExpressionTag(LetFreeConversion.incrementLambdaVars(lfe, 0), children)
          } + (l.arg -> LetFreeExpressionTag(LetFreeExpression.LambdaVar(0), Set.empty))

          val neWrapper = {ne: LetFreeExpression => normalOrderReduction(LetFreeExpression.Recursion(LetFreeExpression.Lambda(ne)))}
          val originalLambda = AnnotatedLambda(arg=l.arg, tpe=l.expr.getType, expr=l.in, tag=l.tag)
          for {
            ee <- letFreeConvertExpr(l.expr, nextEnv, p)
            eeNe = neWrapper(ee.tag._2.lfe)
            eeNeTag = LetFreeExpressionTag(eeNe, ee.tag._2.children)
            nextNextEnv: Env = env + (l.arg -> eeNeTag)
            eIn <- letFreeConvertExpr(l.in, nextNextEnv, p)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, eIn.tag._2))
        case _ =>
          for {
            ee <- letFreeConvertExpr(l.expr, env, p)
            nextEnv: Env = env + (l.arg -> ee.tag._2)
            eIn <- letFreeConvertExpr(l.in, nextEnv, p)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, eIn.tag._2))
      }

  def letFreeConvertLiteral(l: Literal[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, LetFreeExpressionTag)]] =
      State.pure(l.copy(tag=(l.tag, LetFreeExpressionTag(LetFreeExpression.Literal(l.lit), Set.empty))))

  def letFreeConvertMatch(m: Match[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, LetFreeExpressionTag)]] = for {
      arg <- letFreeConvertExpr(m.arg, env, p)
      branches <- (m.branches.map { case branch => letFreeConvertBranch(branch, env, p)}).sequence
      letFreeBranches = branches.map { case (p, e) => (letFreeConvertPattern(p), e.tag._2.lfe)}
      ne=normalOrderReduction(LetFreeExpression.Match(arg.tag._2.lfe, letFreeBranches))
      children=branches.foldLeft(combineWithChildren(arg.tag._2)) { case (tags, br) => tags ++ combineWithChildren(br._2.tag._2) }
      lfeTag = LetFreeExpressionTag(lfe=ne, children=children)
    } yield Match(arg=arg,
      branches=branches,
      tag=(m.tag, lfeTag))

  def letFreeConvertPattern(pat: Pattern[(PackageName, Constructor), Type]): LetFreePattern = {
    val names = pat.names
    def loop(pat: Pattern[(PackageName, Constructor), Type]): LetFreePattern =
      pat match {
        case Pattern.WildCard => LetFreePattern.WildCard
        case Pattern.Literal(lit) => LetFreePattern.Literal(lit)
        case Pattern.Var(v) => LetFreePattern.Var(names.indexOf(v))
        case Pattern.Named(n, p) => LetFreePattern.Named(names.indexOf(n), loop(p))
        case Pattern.StrPat(items) => LetFreePattern.StrPat(
          items.map {
            case Pattern.StrPart.WildStr => LetFreePattern.StrPart.WildStr
            case Pattern.StrPart.NamedStr(n) => LetFreePattern.StrPart.NamedStr(names.indexOf(n))
            case Pattern.StrPart.LitStr(asString) => LetFreePattern.StrPart.LitStr(asString)
          })
        case Pattern.ListPat(items) =>
          LetFreePattern.ListPat(items.map {
            case Pattern.ListPart.NamedList(n) =>Left(Some(names.indexOf(n)))
            case Pattern.ListPart.WildList => Left(None)
            case Pattern.ListPart.Item(p) => Right(loop(p))
          })
        case Pattern.Annotation(p, tpe) => loop(p)
        case Pattern.PositionalStruct(pc@(_, ctor), params) =>
          val dt = definedForCons(pc)
          val df = dt.dataFamily
          val name = if (dt.isStruct) None else Some(dt.constructors.indexWhere(_.name == ctor))
          LetFreePattern.PositionalStruct(name, params.map(loop(_)), df)
        case Pattern.Union(h, t) => LetFreePattern.Union(loop(h), t.map(loop(_)))
      }
    loop(pat)
  }

  def letFreeConvertBranch(b: (Pattern[(PackageName, Constructor), Type], TypedExpr[Declaration]), env: Env, p: Package.Inferred): NormState[
    (Pattern[(PackageName, Constructor), Type], TypedExpr[(Declaration, LetFreeExpressionTag)])] = {
    val (pattern, expr) = b
    val names = pattern.names.collect { case b: Identifier.Bindable => b }

    val lv = names.zipWithIndex
      .reverse
      .toMap
      .mapValues(idx => LetFreeExpressionTag(LetFreeExpression.LambdaVar(idx), Set[LetFreeExpression]()))

    val nextEnv = lv ++ ((env -- names).mapValues { case ExpressionKeyTag(lfe, children) =>
      LetFreeExpressionTag(
        names.foldLeft(lfe) { 
          (expr, _) => LetFreeConversion.incrementLambdaVars(expr, 0)
        } , children)
    })

    for {
      innerExpr <- letFreeConvertExpr(expr, nextEnv, p)
      normalExpr = names.foldLeft(innerExpr.tag._2.lfe) { case (expr, _) => LetFreeExpression.Lambda(expr) }
      finalExpression = innerExpr.updatedTag((innerExpr.tag._1, innerExpr.tag._2.copy(lfe=normalExpr)))
    } yield (pattern, finalExpression)
  }

  def letFreeConvertProgram(pkgName: PackageName, pack: Package.Inferred): NormState[
    Program[TypeEnv[Variance], TypedExpr[(Declaration, LetFreeConversion.LetFreeExpressionTag)], Any]] = {
    for {
      lets <- pack.program.lets.map {
        case (name, recursive, expr) => letFreeConvertNameKindLet(name, recursive, expr, pack, Map.empty).map((name, recursive, _))
      }.sequence
    } yield pack.program.copy(
      lets  = lets
    )
  }

  def letFreeConvertPackage(pkgName: PackageName, pack: Package.Inferred):
    NormState[LetFreePac] = for {
    program <- letFreeConvertProgram(pkgName, pack)
  } yield pack.copy(program = program)

  def getTag(ref: ResultingRef) = ref match {
    case Right(te) => te.tag
    case Left((_, t)) => t
  }

  private type Ref[T] =
    Either[(Identifier, T), TypedExpr[T]]

  private type SourceRef = Ref[Declaration]
  private type ResultingRef = Ref[(Declaration,  LetFreeExpressionTag)]
  private type Env = Map[Identifier, LetFreeExpressionTag]
  private type NormState[A] = State[Map[(PackageName, Identifier), TypedExpr[(Declaration, LetFreeExpressionTag)]], A]

  private def norm(pack: Package.Inferred, item: Identifier, t: Declaration, env: Env): NormState[ResultingRef] =
      NameKind(pack, item).get match { // this get should never fail due to type checking
        case NameKind.Let(name, recursive, expr) => letFreeConvertNameKindLet(
          name, recursive, expr, pack, env
          ).map(res => Right(res))
        case NameKind.Constructor(cn, _, dt, _) =>
          val lfeTag = LetFreeExpressionTag(constructor(cn, dt), Set.empty)
          State.pure(Left((item, (t, lfeTag))))
        case NameKind.Import(from, orig) =>
          // we reset the environment in the other package
          for {
            imported <- norm(pm.toMap(from.name), orig, t, Map.empty)
            lfeTag = getTag(imported)._2
          } yield Left((item, (t, lfeTag)))
        case NameKind.ExternalDef(pn, n, defType) =>
          val lfeTag = LetFreeExpressionTag(LetFreeExpression.ExternalVar(pn, n, defType), Set.empty)  
          State.pure(Left((item, (t, lfeTag))))
      }

  private def letFreeConvertNameKindLet(name: Identifier.Bindable, recursive: RecursionKind, expr: TypedExpr[Declaration], pack: Package.Inferred, env: Env):
    NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]] =
    for {
      lookup <- State.inspect {
        lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, LetFreeExpressionTag)]] =>
          lets.get((pack.name, name))
        }
      outExpr  <- lookup match {
        case Some(res) =>
          State.pure(res): NormState[TypedExpr[(Declaration, LetFreeExpressionTag)]]
        case None =>
          recursive match {
            case RecursionKind.Recursive =>
              val nextEnv = (env - name).mapValues { case ExpressionKeyTag(lfe, children) =>
                LetFreeExpressionTag(LetFreeConversion.incrementLambdaVars(lfe, 0), children)
              } + (name -> LetFreeExpressionTag(LetFreeExpression.LambdaVar(0), Set.empty))

              for {
                res <- letFreeConvertExpr(expr, nextEnv, pack)
                tag = res.tag
                wrappedNe = normalOrderReduction(LetFreeExpression.Recursion(LetFreeExpression.Lambda(tag._2.lfe)))
                children = tag._2.children
                finalRes = res.updatedTag((res.tag._1, LetFreeExpressionTag(wrappedNe, children)))
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, LetFreeExpressionTag)]] =>
                    lets + ((pack.name, name) -> finalRes)
                }
              } yield finalRes
            case _ =>
              for {
                res <- letFreeConvertExpr(expr, env, pack)
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, LetFreeExpressionTag)]] =>
                    lets + ((pack.name, name) -> res)
                }
              } yield res
          }
      }
    } yield outExpr

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): LetFreeExpression = {
      val (enum, arity) = dt.constructors
        .toList
        .iterator
        .zipWithIndex
        .collectFirst { case (cf, idx) if cf.name == c => (idx, cf.args.size) }
        .get

      def loop(params: Int, expr: LetFreeExpression): LetFreeExpression =
        if (params == 0) expr
        else loop(params - 1, LetFreeExpression.Lambda(expr))

        loop(arity, LetFreeExpression.Struct(enum, ((arity - 1) to 0 by -1).map(LetFreeExpression.LambdaVar(_)).toList, dt.dataFamily))
  }

  private def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
    pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2
}

