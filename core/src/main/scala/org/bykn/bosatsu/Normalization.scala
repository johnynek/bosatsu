package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._
import cats.Id
import rankn._

import Identifier.Constructor

sealed abstract class NormalExpression {
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

  def serialize: String = {
    def escapeString(unescaped: String) = StringUtil.escape('\'', unescaped)
    this match {
      case NormalExpression.App(fn, arg) => s"App(${fn.serialize},${arg.serialize})"
      case NormalExpression.ExternalVar(pack, defName) => s"ExternalVar('${escapeString(pack.asString)}','${escapeString(defName.asString)}')"
      case NormalExpression.Match(arg, branches) => {
        val serBranches = branches.toList.map {case (np, ne) => s"${np.serialize},${ne.serialize}"}.mkString(",")
        s"Match(${arg.serialize},$serBranches)"
      }
      case NormalExpression.LambdaVar(index) => s"LambdaVar($index)"
      case NormalExpression.Lambda(expr) => s"Lambda(${expr.serialize})"
      case NormalExpression.Struct(enum, args) => s"Struct($enum,${args.map(_.serialize).mkString(",")})"
      case NormalExpression.Literal(toLit) => toLit match {
        case Lit.Str(toStr) => s"Literal('${escapeString(toStr)}')"
        case Lit.Integer(bigInt) => s"Literal($bigInt)"
      }
      case NormalExpression.Recursion(lambda) => s"Recursion(${lambda.serialize})"
    }
  }
}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression)
  extends NormalExpression {
    def maxLambdaVar = (fn.maxLambdaVar.toList ++ arg.maxLambdaVar.toList)
      .reduceLeftOption(Math.max)
  }
  case class ExternalVar(pack: PackageName, defName: Identifier)
  extends NormalExpression {
    def maxLambdaVar = None
  }
  case class Match(arg: NormalExpression,
    branches: NonEmptyList[(NormalPattern, NormalExpression)])
  extends NormalExpression {
    def maxLambdaVar =
      (arg.maxLambdaVar.toList ++ branches.toList.flatMap(_._2.maxLambdaVar))
        .reduceLeftOption(Math.max)
  }
  case class LambdaVar(index: Int) extends NormalExpression {
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
  case class Lambda(expr: NormalExpression) extends NormalExpression {
    def maxLambdaVar = expr.maxLambdaVar.map(_ - 1)
  }
  case class Struct(enum: Int, args: List[NormalExpression]) extends NormalExpression {
    def maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends NormalExpression {
    def maxLambdaVar = None
  }
  case class Recursion(lambda: NormalExpression) extends NormalExpression {
    def maxLambdaVar = lambda.maxLambdaVar
  }
}

sealed abstract class NormalPattern {
  def escapeString(unescaped: String) = StringUtil.escape('\'', unescaped)
  def serialize: String = {
    this match {
      case NormalPattern.WildCard => "WildCard"
      case NormalPattern.Literal(toLit) => toLit match {
        case Lit.Str(toStr) => s"Literal('${escapeString(toStr)}')"
        case Lit.Integer(bigInt) => s"Literal($bigInt)"
      }
      case NormalPattern.Var(name) => s"Var($name)"
      case NormalPattern.Named(name, pat) => s"Named($name,${pat.serialize})"
      case NormalPattern.ListPat(parts) => {
        val inside = parts.map {
          case Left(name) => s"Left(${name.map(_.toString).getOrElse("")})"
          case Right(pat) => s"Right(${pat.serialize})"
        }.mkString(",")
        s"ListPat($inside)"
      }
      case NormalPattern.PositionalStruct(name, params) => s"PositionalStruct(${name.map(_.toString).getOrElse("")},${params.map(_.serialize).mkString(",")})"
      case NormalPattern.Union(head, rest) => s"Union(${head.serialize},${rest.toList.map(_.serialize).mkString(",")})"
      case NormalPattern.StrPat(parts) => {
        val inside = parts.map {
          case NormalPattern.StrPart.WildStr => "WildStr"
          case NormalPattern.StrPart.NamedStr(name) => s"NamedStr($name)"
          case NormalPattern.StrPart.LitStr(toString) => s"LitStr($toString)"
        }.toList.mkString(",")
        s"StrPat($inside)"
      }
    }
  }
}

object NormalPattern {
  case object WildCard extends NormalPattern
  case class Literal(toLit: Lit) extends NormalPattern
  case class Var(name: Int) extends NormalPattern
  /**
   * Patterns like foo @ Some(_)
   * @ binds tighter than |, so use ( ) with groups you want to bind
   */
  case class Named(name: Int, pat: NormalPattern) extends NormalPattern
  case class ListPat(parts: List[Either[Option[Int], NormalPattern]]) extends NormalPattern
  case class PositionalStruct(name: Option[Int], params: List[NormalPattern]) extends NormalPattern
  case class Union(head: NormalPattern, rest: NonEmptyList[NormalPattern]) extends NormalPattern
  case class StrPat(parts: NonEmptyList[StrPart]) extends NormalPattern

  sealed abstract class StrPart
  object StrPart {
    final case object WildStr extends StrPart
    final case class NamedStr(idx: Int) extends StrPart
    final case class LitStr(asString: String) extends StrPart
  }
}

object Normalization {
  case class ExpressionKeyTag[T](ne: T, children: Set[T])
  type NormalExpressionTag = ExpressionKeyTag[NormalExpression]
  def NormalExpressionTag(ne: NormalExpression, children: Set[NormalExpression]) = ExpressionKeyTag(ne, children)
  type NormalizedPM = PackageMap.Typed[(Declaration, NormalExpressionTag)]
  type NormalizedPac = Package.Typed[(Declaration, NormalExpressionTag)]

  type PatternEnv[T] = Map[Int, T]

  sealed trait PatternMatch[+A]
  case class Matches[A](env: A) extends PatternMatch[A]
  case object NoMatch extends PatternMatch[Nothing]
  case object NotProvable extends PatternMatch[Nothing]

  def noop[T]: (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] = { (_, env) => Matches(env) }
  def neverMatch[T]: (T, PatternEnv[T]) => PatternMatch[Nothing] = { (_, _) => NoMatch }

  def structListAsList(ne: NormalExpression): Option[List[NormalExpression]] = {
    ne match {
      case NormalExpression.Struct(0, _) => Some(Nil)
      case NormalExpression.Struct(1, List(value, tail)) => structListAsList(tail).map(value :: _)
      case _ => None
    }
  }

  def listAsStructList(lst: List[NormalExpression]): NormalExpression =
    lst.foldRight(NormalExpression.Struct(0, Nil)) { case (ne, acc) => NormalExpression.Struct(1, List(ne, acc)) }

  case class LitValue(toAny: Any)

  implicit val neToLitValue: NormalExpression => Option[LitValue] = {
    case NormalExpression.Literal(lit) => Some(LitValue(lit.toAny))
    case _ => None
  }
  implicit val neToStruct: NormalExpression => Option[(Int, List[NormalExpression])] = {
    case NormalExpression.Struct(enum, args) => Some((enum, args))
    case _ => None
  }
  implicit val neToList: NormalExpression => Option[List[NormalExpression]] = structListAsList(_)
  implicit val neFromList: List[NormalExpression] => NormalExpression = listAsStructList(_)

  def maybeBind[T](pat: NormalPattern)(implicit
    toLitValue: T => Option[LitValue],
    toStruct: T => Option[(Int, List[T])],
    toList: T => Option[List[T]],
    fromList: List[T] => T,
    ): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
    pat match {
      case NormalPattern.WildCard => noop
      case NormalPattern.Literal(lit) =>
        { (v, env) => toLitValue(v) match {
          case Some(LitValue(v)) => if (v == lit.toAny) Matches(env) else NoMatch
          case _ => NotProvable
        }}
      case NormalPattern.Var(n) =>
        { (v, env) => Matches(env + (n ->  v)) }
      case NormalPattern.Named(n, p) =>
        val inner = maybeBind[T](p)

        { (v, env) =>
          inner(v, env) match {
            case Matches(env1) => Matches(env1 + (n -> v))
            case notMatch => notMatch
          }
        }
      case NormalPattern.ListPat(items) =>
        items match {
          case Nil =>
            { (arg, acc) =>
              toStruct(arg) match {
                case Some((0, _)) => Matches(acc)
                case Some((1, _)) => NoMatch
                case _ => NotProvable
              }
            }
          case Right(ph) :: ptail =>
            // a right hand side pattern never matches the empty list
            val fnh = maybeBind[T](ph)
            val fnt = maybeBind[T](NormalPattern.ListPat(ptail))

            { (arg, acc) =>
              toStruct(arg) match {
                case Some((1, List(argHead, structTail))) =>
                  fnh(argHead, acc) match {
                    case NoMatch => NoMatch
                    case NotProvable => fnt(structTail, acc) match {
                      case NoMatch => NoMatch
                      case _ => NotProvable
                    }
                    case Matches(acc1) => fnt(structTail, acc1)
                  }
                case Some(_) => NoMatch
                case _ => NotProvable
              }
            }
          case Left(splice) :: Nil =>
            // this is the common and easy case: a total match of the tail
            // we don't need to match on it being a list, because we have
            // already type checked
            splice match {
              case Some(ident) =>
                { (v, env) => Matches(env + (ident -> v)) }
              case None =>
                noop
            }
          case Left(splice) :: ptail =>
            // this is more costly, since we have to match a non infinite tail.
            // we reverse the tails, do the match, and take the rest into
            // the splice
            val revPat = NormalPattern.ListPat(ptail.reverse)
            val fnMatchTail = maybeBind[T](revPat)
            val ptailSize = ptail.size

            { (arg, acc) =>
              // we only allow one splice, so we assume the rest of the patterns
              toList(arg) match {
                case None => NotProvable
                case Some(asList) =>
                  val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                  fnMatchTail(fromList(revArgTail), acc) match {
                    case m@Matches(acc1) => splice.map {nm =>
                      val rest = fromList(spliceVals.reverse)
                      Matches(acc1 + (nm -> rest))
                    }.getOrElse(m)
                    case notMatch => notMatch
                  }
              }
            }
        }
      case NormalPattern.Union(h, t) =>
        // we can just loop expanding these out:
        def loop(ps: List[NormalPattern]): (T, PatternEnv[T]) => PatternMatch[PatternEnv[T]] =
          ps match {
            case Nil => neverMatch
            case head :: tail =>
              val fnh = maybeBind[T](head)
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
      case NormalPattern.PositionalStruct(maybeIdx, items) =>
        // The type in question is not the outer dt, but the type associated
        // with this current constructor
        val itemFns = items.map(maybeBind[T](_))

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
              toStruct(arg) match {
                case Some((_, args)) =>
                  processArgs(args, acc)
                case _ =>
                  NotProvable
              }
            }

          case Some(idx) =>
            // we don't check if idx < 0, because if we compiled, it can't be
            val result = { (arg: T, acc: PatternEnv[T]) =>
              toStruct(arg) match {
                case Some((enumId, args)) =>
                  if (enumId == idx) processArgs(args, acc)
                  else NoMatch
                case _ =>
                  NotProvable
              }
            }
            result
        }
      case NormalPattern.StrPat(parts) => {(v, env) => NotProvable}
  }

  def findMatch(m: NormalExpression.Match) =
    m.branches.collectFirst(Function.unlift( { case (pat, result) =>
      maybeBind[NormalExpression](pat).apply(m.arg, Map()) match {
        case Matches(env) => Some(Some((pat, env, result)))
        case NotProvable => Some(None)
        case NoMatch => None
      }
    })).get // Totallity of matches should ensure this will always find something unless something has gone terribly wrong

  def solveMatch(env: PatternEnv[NormalExpression], result: NormalExpression) =
    ((env.size - 1) to 0 by -1).map(env.get(_).get) // If this exceptions then somehow we didn't get enough names in the env
      .foldLeft(result) { case (ne, arg) => NormalExpression.App(ne, arg) }

  def normalOrderReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
    val res = headReduction(expr) match {
      case App(fn, arg) =>
        App(normalOrderReduction(fn), normalOrderReduction(arg))
      case extVar @ ExternalVar(_, _) => extVar
      // check for a match reduction opportunity (beta except for Match instead of lambda)
      case Match(arg, branches) =>
        Match(normalOrderReduction(arg), branches.map{ case (p, s) => (p, normalOrderReduction(s))})
      case lv @ LambdaVar(_)  => lv
      // check for eta reduction
      case Lambda(expr)       =>
        Lambda(normalOrderReduction(expr))
      case Struct(enum, args) => Struct(enum, args.map(normalOrderReduction(_)))
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
  def headReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
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

  private def applyLambdaSubstituion(expr: NormalExpression,
    subst: Option[NormalExpression],
    idx: Int): NormalExpression = {
      import NormalExpression._
      expr match {
        case App(fn, arg)                           =>
          App(applyLambdaSubstituion(fn, subst, idx),
            applyLambdaSubstituion(arg, subst, idx))
        case ext @ ExternalVar(_, _)                => ext
        case Match(arg, branches)                   =>
          Match(applyLambdaSubstituion(arg, subst, idx), branches.map {
            case (enum, expr) => (enum, applyLambdaSubstituion(expr, subst, idx))
          })
        case LambdaVar(varIndex) if varIndex == idx => subst.get
        case LambdaVar(varIndex) if varIndex > idx  => LambdaVar(varIndex - 1)
        case lv @ LambdaVar(_)                      => lv
        case Lambda(fn)                             => Lambda(applyLambdaSubstituion(fn, subst.map(incrementLambdaVars(_, 0)), idx + 1))
        case Struct(enum, args)                     =>
          Struct(enum, args.map(applyLambdaSubstituion(_, subst, idx)))
        case l @ Literal(_)                         => l
        case r @ Recursion(fn)                      => Recursion(applyLambdaSubstituion(fn, subst, idx))
      }
  }

  private def incrementLambdaVars(expr: NormalExpression, lambdaDepth: Int): NormalExpression = {
    import NormalExpression._
    expr match {
      case App(fn, arg) =>
        App(incrementLambdaVars(fn, lambdaDepth),
          incrementLambdaVars(arg, lambdaDepth))
      case ext @ ExternalVar(_, _) => ext
      case Match(arg, branches) =>
        Match(incrementLambdaVars(arg, lambdaDepth), branches.map {
          case (enum, expr) => (enum, incrementLambdaVars(expr, lambdaDepth))
        })
      case LambdaVar(varIndex) if varIndex >= lambdaDepth => LambdaVar(varIndex + 1)
      case lv @ LambdaVar(_)                      => lv
      case Lambda(fn)                             => Lambda(incrementLambdaVars(fn, lambdaDepth + 1))
      case Struct(enum, args) =>
        Struct(enum, args.map(incrementLambdaVars(_, lambdaDepth)))
      case l @ Literal(_) => l
      case Recursion(fn) => Recursion(incrementLambdaVars(fn, lambdaDepth))
    }
  }
}

case class NormalizePackageMap(pm: PackageMap.Inferred) {
  import Normalization._
  import TypedExpr._

  val normalizePackageMap: NormalizedPM = {
    val packs = pm.toMap.toList
    val normAll = packs.traverse { case (name, pack) =>
      normalizePackage(name, pack)
        .map((name, _))
    }
    PackageMap(normAll.run(Map()).value._2.toMap)
  }

  def hashKey[T](fn: NormalExpression => T): PackageMap.Typed[(Declaration, ExpressionKeyTag[T])] = {
    val lst = normalizePackageMap.toMap.toList
      .map { case (packName, pack) =>
        val newLets = pack.program.lets.map { case (letsName, recursive, expr) =>
          val newExpr = expr.traverse[Id, (Declaration, ExpressionKeyTag[T])] {
            case (d, neT) => (d, ExpressionKeyTag(fn(neT.ne), neT.children.map(fn)))
          }
          (letsName, recursive, newExpr)
        }
        val newProgram = pack.program.copy(lets = newLets)
        val newPack = pack.copy(program = newProgram)
        (packName, newPack)
      }
    PackageMap(lst.toMap)
  }

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] = {
      expr match {
        case a@Annotation(_, _, _) => normalizeAnnotation(a, env, p)
        case g@Generic(_, _, _) => normalizeGeneric(g, env, p)
        case v@Var(_, _, _, _) => normalizeVar(v, env, p)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al, env, p)
        case a@App(_, _, _, _) => normalizeApp(a, env, p)
        case l@Let(_, _, _, _, _) => normalizeLet(l, env, p)
        case l@Literal(_, _, _) => normalizeLiteral(l, env, p)
        case m@Match(_, _, _) => normalizeMatch(m, env, p)
      }
    }

  private def combineWithChildren(nt: NormalExpressionTag) = nt.children + nt.ne

  def normalizeAnnotation(a: Annotation[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      normalizeExpr(a.term, env, p).map { term =>
        val neTag = term.tag._2
        val tag = (a.tag, neTag)
        a.copy(term=term, tag=tag)
      }

  def normalizeGeneric(g: Generic[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      normalizeExpr(g.in, env, p).map { in =>
        val neTag = in.tag._2
        val tag = (g.tag, neTag)
        g.copy(in=in, tag=tag)
      }

  def normalizeVar(v: Var[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      env._1.get(v.name) match {
        case None => norm(p, v.name, v.tag, env).map { ne =>
          val neTag = getTag(ne)._2
          v.copy(tag=(v.tag, neTag))
        }
        case Some(neTag) =>
          State.pure(v.copy(tag=(v.tag, neTag)))
      }

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] = {
      val lambdaVars = al.arg :: env._2
      val nextEnv: Env = (env._1 ++ lambdaVars.zipWithIndex
        .reverse
        .toMap
        .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
        lambdaVars)
      for {
        eExpr <- normalizeExpr(al.expr, nextEnv, p)
        ne = normalOrderReduction(NormalExpression.Lambda(eExpr.tag._2.ne))
        children = combineWithChildren(eExpr.tag._2)
        neTag = NormalExpressionTag(ne, children)
      } yield al.copy(expr=eExpr, tag=(al.tag, neTag))
    }

  def normalizeApp(a: App[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        efn <- normalizeExpr(a.fn, env, p)
        earg <- normalizeExpr(a.arg, env, p)
        ne = normalOrderReduction(NormalExpression.App(efn.tag._2.ne, earg.tag._2.ne))
        children = combineWithChildren(efn.tag._2) ++ combineWithChildren(earg.tag._2)
        neTag = NormalExpressionTag(ne, children)
      } yield a.copy(fn=efn, arg=earg, tag=(a.tag, neTag))

  def normalizeLet(l: Let[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      l.recursive match {
        case RecursionKind.Recursive =>
          val lambdaVars = l.arg :: env._2
          val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
            .reverse
            .toMap
            .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
            lambdaVars)
          val neWrapper = {ne: NormalExpression => normalOrderReduction(NormalExpression.Recursion(NormalExpression.Lambda(ne)))}
          val originalLambda = AnnotatedLambda(arg=l.arg, tpe=l.expr.getType, expr=l.in, tag=l.tag)
          for {
            ee <- normalizeExpr(l.expr, nextEnv, p)
            eeNe = neWrapper(ee.tag._2.ne)
            eeNeTag = NormalExpressionTag(eeNe, ee.tag._2.children)
            nextNextEnv: Env = (nextEnv._1 + (l.arg -> eeNeTag), nextEnv._2)
            eIn <- normalizeExpr(l.in, nextNextEnv, p)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, eIn.tag._2))
        case _ =>
          for {
            ee <- normalizeExpr(l.expr, env, p)
            nextEnv: Env = (env._1 + (l.arg -> ee.tag._2), env._2)
            eIn <- normalizeExpr(l.in, nextEnv, p)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, eIn.tag._2))
      }

  def normalizeLiteral(l: Literal[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      State.pure(l.copy(tag=(l.tag, NormalExpressionTag(NormalExpression.Literal(l.lit), Set()))))

  def normalizeMatch(m: Match[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, NormalExpressionTag)]] = for {
      arg <- normalizeExpr(m.arg, env, p)
      branches <- (m.branches.map { case branch => normalizeBranch(branch, env, p)}).sequence
      normalBranches = branches.map { case (p, e) => (normalizePattern(p), e.tag._2.ne)}
      ne=normalOrderReduction(NormalExpression.Match(arg.tag._2.ne, normalBranches))
      children=branches.foldLeft(combineWithChildren(arg.tag._2)) { case (tags, br) => tags ++ combineWithChildren(br._2.tag._2) }
      neTag = NormalExpressionTag(ne=ne, children=children)
    } yield Match(arg=arg,
      branches=branches,
      tag=(m.tag, neTag))

  def normalizePattern(pat: Pattern[(PackageName, Constructor), Type]): NormalPattern = {
    val names = pat.names
    def loop(pat: Pattern[(PackageName, Constructor), Type]): NormalPattern =
      pat match {
        case Pattern.WildCard => NormalPattern.WildCard
        case Pattern.Literal(lit) => NormalPattern.Literal(lit)
        case Pattern.Var(v) => NormalPattern.Var(names.indexOf(v))
        case Pattern.Named(n, p) => NormalPattern.Named(names.indexOf(n), loop(p))
        case Pattern.StrPat(items) => NormalPattern.StrPat(
          items.map {
            case Pattern.StrPart.WildStr => NormalPattern.StrPart.WildStr
            case Pattern.StrPart.NamedStr(n) => NormalPattern.StrPart.NamedStr(names.indexOf(n))
            case Pattern.StrPart.LitStr(asString) => NormalPattern.StrPart.LitStr(asString)
          })
        case Pattern.ListPat(items) =>
          NormalPattern.ListPat(items.map {
            case Pattern.ListPart.NamedList(n) =>Left(Some(names.indexOf(n)))
            case Pattern.ListPart.WildList => Left(None)
            case Pattern.ListPart.Item(p) => Right(loop(p))
          })
        case Pattern.Annotation(p, tpe) => loop(p)
        case Pattern.PositionalStruct(pc@(_, ctor), params) =>
          val dt = definedForCons(pc)
          val name = if (dt.isStruct) None else Some(dt.constructors.indexWhere(_.name == ctor))
          NormalPattern.PositionalStruct(name, params.map(loop(_)))
        case Pattern.Union(h, t) => NormalPattern.Union(loop(h), t.map(loop(_)))
      }
    loop(pat)
  }

  def normalizeBranch(b: (Pattern[(PackageName, Constructor), Type], TypedExpr[Declaration]), env: Env, p: Package.Inferred): NormState[
    (Pattern[(PackageName, Constructor), Type], TypedExpr[(Declaration, NormalExpressionTag)])] = {
    val (pattern, expr) = b
    val names = pattern.names.collect { case b: Identifier.Bindable => b }
    val lambdaVars = names ++ env._2
    val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
      .reverse
      .toMap
      .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
      lambdaVars)
    for {
      innerExpr <- normalizeExpr(expr, nextEnv, p)
      normalExpr = names.foldLeft(innerExpr.tag._2.ne) { case (expr, _) => NormalExpression.Lambda(expr) }
      finalExpression = innerExpr.updatedTag((innerExpr.tag._1, innerExpr.tag._2.copy(ne=normalExpr)))
    } yield (pattern, finalExpression)
  }

  def normalizeProgram(pkgName: PackageName, pack: Package.Inferred): NormState[
    Program[TypeEnv[Variance], TypedExpr[(Declaration, Normalization.NormalExpressionTag)], Any]] = {
    for {
      lets <- pack.program.lets.map {
        case (name, recursive, expr) => normalizeNameKindLet(name, recursive, expr, pack, (Map(), Nil)).map((name, recursive, _))
      }.sequence
    } yield pack.program.copy(
      lets  = lets
    )
  }

  def normalizePackage(pkgName: PackageName, pack: Package.Inferred):
    NormState[NormalizedPac] = for {
    program <- normalizeProgram(pkgName, pack)
  } yield pack.copy(program = program)

  def getTag(ref: ResultingRef) = ref match {
    case Right(te) => te.tag
    case Left((_, t)) => t
  }

  private type Ref[T] =
    Either[(Identifier, T), TypedExpr[T]]

  private type SourceRef = Ref[Declaration]
  private type ResultingRef = Ref[(Declaration,  NormalExpressionTag)]
  private type Env = (Map[Identifier, NormalExpressionTag], List[Identifier])
  private type NormState[A] = State[Map[(PackageName, Identifier), TypedExpr[(Declaration, NormalExpressionTag)]], A]

  private def norm(pack: Package.Inferred, item: Identifier, t: Declaration, env: Env): NormState[ResultingRef] =
      NameKind(pack, item).get match { // this get should never fail due to type checking
        case NameKind.Let(name, recursive, expr) => normalizeNameKindLet(
          name, recursive, expr, pack, env
          ).map(res => Right(res))
        case NameKind.Constructor(cn, _, dt, _) =>
          val neTag = NormalExpressionTag(constructor(cn, dt), Set())
          State.pure(Left((item, (t, neTag))))
        case NameKind.Import(from, orig) =>
          // we reset the environment in the other package
          for {
            imported <- norm(pm.toMap(from.name), orig, t, (Map.empty, Nil))
            neTag = getTag(imported)._2
          } yield Left((item, (t, neTag)))
        case NameKind.ExternalDef(pn, n, scheme) =>
          val neTag = NormalExpressionTag(NormalExpression.ExternalVar(pn, n), Set())
          State.pure(Left((item, (t, neTag))))
      }

  private def normalizeNameKindLet(name: Identifier.Bindable, recursive: RecursionKind, expr: TypedExpr[Declaration], pack: Package.Inferred, env: Env):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      lookup <- State.inspect {
        lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, NormalExpressionTag)]] =>
          lets.get((pack.name, name))
        }
      outExpr  <- lookup match {
        case Some(res) =>
          State.pure(res): NormState[TypedExpr[(Declaration, NormalExpressionTag)]]
        case None =>
          recursive match {
            case RecursionKind.Recursive =>
              val lambdaVars = name :: env._2
              val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
                .toMap
                .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
                lambdaVars)
              for {
                res <- normalizeExpr(expr, nextEnv, pack)
                tag = res.tag
                wrappedNe = normalOrderReduction(NormalExpression.Recursion(NormalExpression.Lambda(tag._2.ne)))
                children = tag._2.children
                finalRes = res.updatedTag((res.tag._1, NormalExpressionTag(wrappedNe, children)))
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, NormalExpressionTag)]] =>
                    lets + ((pack.name, name) -> finalRes)
                }
              } yield finalRes
            case _ =>
              for {
                res <- normalizeExpr(expr, env, pack)
                _ <- State.modify {
                  lets: Map[(PackageName, Identifier), TypedExpr[(Declaration, NormalExpressionTag)]] =>
                    lets + ((pack.name, name) -> res)
                }
              } yield res
          }
      }
    } yield outExpr

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): NormalExpression = {
      val (enum, arity) = dt.constructors
        .toList
        .iterator
        .zipWithIndex
        .collectFirst { case (cf, idx) if cf.name == c => (idx, cf.args.size) }
        .get

      def loop(params: Int, expr: NormalExpression): NormalExpression =
        if (params == 0) expr
        else loop(params - 1, NormalExpression.Lambda(expr))

        loop(arity, NormalExpression.Struct(enum, ((arity - 1) to 0 by -1).map(NormalExpression.LambdaVar(_)).toList))
  }

  private def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
    pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2
}

