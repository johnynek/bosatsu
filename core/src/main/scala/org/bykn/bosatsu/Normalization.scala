package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._
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
}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression)
  extends NormalExpression {
    val maxLambdaVar = (fn.maxLambdaVar.toList ++ arg.maxLambdaVar.toList)
      .reduceLeftOption(Math.max)
  }
  case class ExternalVar(pack: PackageName, defName: Identifier)
  extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Match(arg: NormalExpression,
    branches: NonEmptyList[(NormalPattern, NormalExpression)])
  extends NormalExpression {
    val maxLambdaVar =
      (arg.maxLambdaVar.toList ++ branches.toList.flatMap(_._2.maxLambdaVar))
        .reduceLeftOption(Math.max)
  }
  case class LambdaVar(index: Int) extends NormalExpression {
    val maxLambdaVar = Some(index)
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
    val maxLambdaVar = expr.maxLambdaVar.map(_ - 1)
  }
  case class Struct(enum: Int, args: List[NormalExpression]) extends NormalExpression {
    val maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Recursion(lambda: NormalExpression) extends NormalExpression {
    val maxLambdaVar = lambda.maxLambdaVar
  }
}

sealed abstract class NormalPattern {}

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
}

object Normalization {
  case class NormalExpressionTag(ne: NormalExpression, children: Set[NormalExpression])
  type NormalizedPM = PackageMap.Typed[(Declaration, Normalization.NormalExpressionTag)]
  type NormalizedPac = Package.Typed[(Declaration, Normalization.NormalExpressionTag)]

  type PatternEnv = Map[Int, NormalExpression]

  sealed trait PatternMatch[+A]
  case class Matches[A](env: A) extends PatternMatch[A]
  case object NoMatch extends PatternMatch[Nothing]
  case object NotProvable extends PatternMatch[Nothing]

  val noop: (NormalExpression, PatternEnv) => PatternMatch[PatternEnv] = { (_, _) => Matches(Map()) }
  val neverMatch: (NormalExpression, PatternEnv) => PatternMatch[Nothing] = { (_, _) => NoMatch }

  def structListAsList(ne: NormalExpression): PatternMatch[List[NormalExpression]] = {
    ne match {
      case NormalExpression.Struct(0, _) => Matches(Nil)
      case NormalExpression.Struct(1, List(value, tail)) => structListAsList(tail) match {
        case Matches(lst) => Matches(value :: lst)
        case notMatch => notMatch
      }
      case _ => NotProvable
    }
  }

  def listAsStructList(lst: List[NormalExpression]): NormalExpression =
    lst.foldRight(NormalExpression.Struct(0, Nil)) { case (ne, acc) => NormalExpression.Struct(1, List(ne, acc)) }

  private def maybeBind(pat: NormalPattern): (NormalExpression, PatternEnv) => PatternMatch[PatternEnv] =
    pat match {
      case NormalPattern.WildCard => noop
      case NormalPattern.Literal(lit) =>
        { (v, env) => v match {
          case NormalExpression.Literal(vlit) => if (vlit == lit) Matches(env) else NoMatch
          case _ => NotProvable
        }}
      case NormalPattern.Var(n) =>
        { (v, env) => Matches(env + (n ->  v)) }
      case NormalPattern.Named(n, p) =>
        val inner = maybeBind(p)

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
              arg match {
                case NormalExpression.Struct(0, List()) => Matches(acc)
                case NormalExpression.Struct(_, _) => NoMatch
                case _ => NotProvable
              }
            }
          case Right(ph) :: ptail =>
            // a right hand side pattern never matches the empty list
            val fnh = maybeBind(ph)
            val fnt = maybeBind(NormalPattern.ListPat(ptail))

            { (arg, acc) =>
              arg match {
                case NormalExpression.Struct(1, List(argHead, structTail)) =>
                  fnh(argHead, acc) match {
                    case NoMatch => NoMatch
                    case NotProvable => fnt(structTail, acc) match {
                      case NoMatch => NoMatch
                      case _ => NotProvable
                    }
                    case Matches(acc1) => fnt(structTail, acc1)
                  }
                  case NormalExpression.Struct(_, _) => NoMatch
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
            val fnMatchTail = maybeBind(revPat)
            val ptailSize = ptail.size

            { (arg, acc) =>
              arg match {
                case s@NormalExpression.Struct(_, _)  =>
                  // we only allow one splice, so we assume the rest of the patterns
                  structListAsList(s) match {
                    case NotProvable => NotProvable
                    case Matches(asList) =>
                      val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                      fnMatchTail(listAsStructList(revArgTail), acc) match {
                        case m@Matches(acc1) => splice.map {nm =>
                          val rest = listAsStructList(spliceVals.reverse)
                          Matches(acc1 + (nm -> rest))
                        }.getOrElse(m)
                        case notMatch => notMatch
                      }
                    case nomatch =>
                      // type checking will ensure this is either a list or a
                      // NormalExpression that will produce a list
                      // $COVERAGE-OFF$this should be unreachable
                      sys.error(s"ill typed in match: $nomatch")
                      // $COVERAGE-ON$
                  }
                case _ => NotProvable
              }
            }
        }
      case NormalPattern.Union(h, t) =>
        // we can just loop expanding these out:
        def loop(ps: List[NormalPattern]): (NormalExpression, PatternEnv) => PatternMatch[PatternEnv] =
          ps match {
            case Nil => neverMatch
            case head :: tail =>
              val fnh = maybeBind(head)
              val fnt: (NormalExpression, PatternEnv) => PatternMatch[PatternEnv] = loop(tail)
              val result: (NormalExpression, PatternEnv) => PatternMatch[PatternEnv] = { case (arg, acc) =>
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
        val itemFns = items.map(maybeBind(_))

        def processArgs(as: List[NormalExpression], acc: PatternEnv): PatternMatch[PatternEnv] = {
          // manually write out foldM hoping for performance improvements
          @annotation.tailrec
          def loop(vs: List[NormalExpression], fns: List[(NormalExpression, PatternEnv) => PatternMatch[PatternEnv]], env: (PatternEnv, PatternMatch[PatternEnv])): PatternMatch[PatternEnv] =
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
            { (arg: NormalExpression, acc: PatternEnv) =>
              arg match {
                case NormalExpression.Struct(_, args) =>
                  processArgs(args, acc)
                case _ =>
                  NotProvable
              }
            } 

          case Some(idx) =>
            // we don't check if idx < 0, because if we compiled, it can't be
            val result = { (arg: NormalExpression, acc: PatternEnv) =>
              arg match {
                case NormalExpression.Struct(enumId, args) =>
                  if (enumId == idx) processArgs(args, acc)
                  else NoMatch
                case _ =>
                  NotProvable
              }
            }
            result
        }
  }

  def findMatch(m: NormalExpression.Match) =
    m.branches.collectFirst(Function.unlift( { case (pat, result) =>
      maybeBind(pat).apply(m.arg, Map()) match {
        case Matches(env) => Some(Some((pat, env, result)))
        case NotProvable => Some(None)
        case NoMatch => None
      }
    })).get // Totallity of matches should ensure this will always find something unless something has gone terribly wrong

  def solveMatch(env: PatternEnv, result: NormalExpression) =
    (0 to (env.size - 1)).toList.map(env.get(_).get) // If this exceptions then somehow we didn't get enough names in the env
      .foldLeft(result) { case (ne, arg) => NormalExpression.App(ne, arg) }
 
  def normalOrderReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
    val nextExpr = expr match {
      case App(Lambda(nextExpr), arg) => {
        applyLambdaSubstituion(nextExpr, Some(arg), 0)
      }
      case m@Match(struct@Struct(enum, args), branches) =>
        findMatch(m) match {
          case None => m
          case Some((pat, env, result)) => solveMatch(env, result)
        }
      case Recursion(Lambda(innerExpr)) if(innerExpr.maxLambdaVar.map(_ < 0).getOrElse(false)) => {
        applyLambdaSubstituion(innerExpr, None, 0)
      }
      case Lambda(App(innerExpr, LambdaVar(0))) => {
        innerExpr
      }
      case _ => expr
    }
    nextExpr match {
      case al @ App(Lambda(_), _) => normalOrderReduction(al)
      case App(fn, arg) =>
        normalOrderReduction(fn) match {
          case l @ Lambda(_) => normalOrderReduction(App(l, arg))
          case nfn @ _       => App(nfn, normalOrderReduction(arg))
        }
      case extVar @ ExternalVar(_, _) => extVar
      case Match(arg, branches) =>
        val nextMatch = Match(normalOrderReduction(arg), branches)
        findMatch(nextMatch) match {
          case None => nextMatch
          case Some(_) => normalOrderReduction(nextMatch)
        }
      case lv @ LambdaVar(_)  => lv
      case Lambda(expr)       =>
        normalOrderReduction(expr) match {
          case a@App(innerExpr, LambdaVar(0)) => normalOrderReduction(Lambda(a))
          case na @ _ => Lambda(na)
        }
      case Struct(enum, args) => Struct(enum, args.map(normalOrderReduction(_)))
      case l @ Literal(_)     => l
      case r@Recursion(innerExpr) =>
        normalOrderReduction(innerExpr) match {
          case Lambda(lambdaExpr) if (lambdaExpr.maxLambdaVar.map(_ < 0).getOrElse(false)) =>
            normalOrderReduction(r)
          case _ => r
        }
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
      case Lambda(fn)                             => incrementLambdaVars(fn, lambdaDepth + 1)
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
        case None => norm((p, Left((v.name, v.tag)), env)).map {ne =>
          val neTag = getTag(ne)._2
          v.copy(tag=(v.tag, neTag))
        }
        case Some(neTag) =>
          State.pure(v.copy(tag=(v.tag, neTag)))
      }

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] = {
      val lambdaVars = Some(al.arg) :: env._2
      val nextEnv: Env = (env._1 ++ lambdaVars.zipWithIndex
        .collect { case (Some(n), i) => (n, i) }
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
        case RecursionKind.Recursive => {
          val lambdaVars = Some(l.arg) :: env._2
          val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
            .collect { case (Some(n), i) => (n, i) }
            .toMap
            .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
            lambdaVars)
          val neWrapper = {ne: NormalExpression => normalOrderReduction(NormalExpression.Recursion(NormalExpression.Lambda(ne)))}
          (nextEnv, neWrapper)
          val originalLambda = AnnotatedLambda(arg=l.arg, tpe=l.expr.getType, expr=l.in, tag=l.tag)
          for {
            ee <- normalizeExpr(l.expr, nextEnv, p)
            nextNextEnv: Env = (nextEnv._1 + (l.arg -> ee.tag._2), nextEnv._2)
            eIn <- normalizeExpr(l.in, nextEnv, p)
            ne = neWrapper(eIn.tag._2.ne)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, NormalExpressionTag(ne, eIn.tag._2.children)))
        }
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
      neTag = NormalExpressionTag(ne=ne, children=Set())
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
        case Pattern.ListPat(items) =>
          NormalPattern.ListPat(items.map {
            case Left(Some(n)) => Left(Some(names.indexOf(n)))
            case Left(None) => Left(None)
            case Right(p) => Right(loop(p))
          })
        case Pattern.Annotation(p, tpe) => loop(p)
        case Pattern.PositionalStruct(pc@(_, ctor), params) =>
          val dt = definedForCons(pc)
          val name = if (dt.isStruct) None else Some(dt.constructors.map(_._1).indexOf(ctor))
          NormalPattern.PositionalStruct(name, params.map(loop(_)))
        case Pattern.Union(h, t) => NormalPattern.Union(loop(h), t.map(loop(_)))
      }
    loop(pat)
  }

  def normalizeBranch(b: (Pattern[(PackageName, Constructor), Type], TypedExpr[Declaration]), env: Env, p: Package.Inferred): NormState[
    (Pattern[(PackageName, Constructor), Type], TypedExpr[(Declaration, NormalExpressionTag)])] = {
    val (pattern, expr) = b
    val names = pattern.names.collect { case b: Identifier.Bindable => Some(b)}
    val lambdaVars = names ++ env._2
    val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
      .collect { case (Some(n), i) => (n, i) }
      .toMap
      .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
      lambdaVars)
    for {
      innerExpr <- normalizeExpr(expr, nextEnv, p)
      normalExpr = names.foldLeft(innerExpr.tag._2.ne) { case (expr, _) => NormalExpression.Lambda(expr) }
      finalExpression = innerExpr.updatedTag((innerExpr.tag._1, innerExpr.tag._2.copy(ne=normalExpr)))
    } yield (pattern, finalExpression)
  }

  def normalizePackageLet(pkgName: PackageName, inferredExpr: (Identifier.Bindable, RecursionKind, TypedExpr[Declaration]), pack: Package.Inferred): 
    NormState[(Identifier.Bindable, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)])] =
    for {
      expr <- normalizeExpr(inferredExpr._3, (Map(), Nil), pack)
      _ <- State.modify { cache: Map[(PackageName, Identifier), ResultingRef] => cache + ((pkgName, inferredExpr._1) -> Right(expr))}
    }
    yield (inferredExpr._1, inferredExpr._2, expr)

  def normalizeProgram(pkgName: PackageName, pack: Package.Inferred): NormState[
    Program[TypeEnv[Variance], TypedExpr[(Declaration, Normalization.NormalExpressionTag)], Statement]] = {
    for { 
      lets <- pack.program.lets.map(normalizePackageLet(pkgName, _, pack)).sequence
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
  private type Env = (Map[Identifier, NormalExpressionTag], List[Option[Identifier]])
  private type NormState[A] = State[Map[(PackageName, Identifier), ResultingRef], A]

  private def norm(input: (Package.Inferred, SourceRef, Env)): NormState[ResultingRef] =
    input match {
      case ((pack, Right(expr), env)) =>
        for {
          expr <- normalizeExpr(expr, env, pack)
        } yield Right(expr)
      case (pack, Left((item, t)), env) => {
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(_, _, expr) =>
            for {
              lookup <- State.inspect {
                lets: Map[(PackageName, Identifier), ResultingRef] =>
                  lets.get((pack.name, item))
              }
              res <- lookup match {
                case Some(res) =>
                  State.pure(res): State[
                  Map[(PackageName, Identifier), ResultingRef],
                  ResultingRef]
                case None =>
                  for {
                    res <- norm((pack, Right(expr), env))
                    _ <- State.modify {
                      lets: Map[(PackageName, Identifier), ResultingRef] =>
                        lets + ((pack.name, item) -> res)
                    }
                  } yield res

              }
            } yield Left((item, getTag(res)))
                case NameKind.Constructor(cn, _, dt, _) => {
                  val neTag: NormalExpressionTag = NormalExpressionTag(constructor(cn, dt), Set())
                  State.pure(Left((item, (t, neTag))))
                }
                case NameKind.Import(from, orig) => {
                  // we reset the environment in the other package
                  for {
                    imported <- norm((pm.toMap(from.unfix.name), Left((orig, t)), (Map.empty, Nil)))
                    neTag = getTag(imported)._2
                  } yield Left((item, (t, neTag)))
                }
                case NameKind.ExternalDef(pn, n, scheme) => {
                  val neTag = NormalExpressionTag(NormalExpression.ExternalVar(pn, n), Set())
                  State.pure(Left((item, (t, neTag))))
                }
        }
        }
    }

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): NormalExpression = {
      val (enum, arity) = dt.constructors
        .toList
        .iterator
        .zipWithIndex
        .collectFirst { case ((ctor, params, resType), idx) if ctor == c => (idx, params.size) }
        .get

      def loop(params: Int, expr: NormalExpression): NormalExpression =
        if (params == 0) expr
        else loop(params - 1, NormalExpression.Lambda(expr))

        loop(arity, NormalExpression.Struct(enum, ((arity - 1) to 0 by -1).map(NormalExpression.LambdaVar(_)).toList))
  }

  private def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
    pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2
}

