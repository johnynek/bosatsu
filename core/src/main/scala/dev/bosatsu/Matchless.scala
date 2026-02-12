package dev.bosatsu

import cats.{Monad, Monoid, Order}
import cats.data.{Chain, NonEmptyList, WriterT}
import dev.bosatsu.pattern.StrPart
import dev.bosatsu.rankn.{DataRepr, Type, RefSpace}

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr[+A] derives CanEqual
  object Expr {
    private def exprTag[A](expr: Expr[A]): Int =
      expr match {
        case _: Lambda[?]               => 0
        case _: WhileExpr[?]            => 1
        case _: Global[?]               => 2
        case Local(_)                   => 3
        case ClosureSlot(_)             => 4
        case LocalAnon(_)               => 5
        case LocalAnonMut(_)            => 6
        case _: App[?]                  => 7
        case _: Let[?]                  => 8
        case _: LetMut[?]               => 9
        case _: If[?]                   => 10
        case _: Always[?]               => 11
        case _: GetEnumElement[?]       => 12
        case _: GetStructElement[?]     => 13
        case Literal(_)                 => 14
        case _: MakeEnum                => 15
        case _: MakeStruct              => 16
        case ZeroNat                    => 17
        case SuccNat                    => 18
        case _: PrevNat[?]              => 19
      }

    private given Order[LocalAnon] = Order.by(_.ident)
    private given Order[LocalAnonMut] = Order.by(_.ident)
    private given Order[Bindable] = Identifier.Bindable.bindableOrder
    private given Order[Either[LocalAnon, Bindable]] =
      Order[Either[LocalAnon, Bindable]]
    private given Order[Lit] = Order.fromOrdering(using Lit.litOrdering)

    given exprOrder[A: Order]: Order[Expr[A]] with {
      private def compareExpr(left: Expr[A], right: Expr[A]): Int =
        (left, right) match {
          case (
                Lambda(capturesL, recNameL, argsL, bodyL),
                Lambda(capturesR, recNameR, argsR, bodyR)
              ) =>
            val c1 = Order[List[Expr[A]]].compare(capturesL, capturesR)
            if (c1 != 0) c1
            else {
              val c2 = Order[Option[Bindable]].compare(recNameL, recNameR)
              if (c2 != 0) c2
              else {
                val c3 = Order[NonEmptyList[Bindable]].compare(argsL, argsR)
                if (c3 != 0) c3
                else compareExpr(bodyL, bodyR)
              }
            }

          case (
                WhileExpr(condL, effectExprL, resultL),
                WhileExpr(condR, effectExprR, resultR)
              ) =>
            val c1 = Order[BoolExpr[A]].compare(condL, condR)
            if (c1 != 0) c1
            else {
              val c2 = compareExpr(effectExprL, effectExprR)
              if (c2 != 0) c2
              else Order[LocalAnonMut].compare(resultL, resultR)
            }

          case (
                Global(fromL, packL, nameL),
                Global(fromR, packR, nameR)
              ) =>
            val c1 = Order[A].compare(fromL, fromR)
            if (c1 != 0) c1
            else {
              val c2 = Order[PackageName].compare(packL, packR)
              if (c2 != 0) c2
              else Order[Bindable].compare(nameL, nameR)
            }

          case (Local(left), Local(right)) =>
            Order[Bindable].compare(left, right)

          case (ClosureSlot(left), ClosureSlot(right)) =>
            java.lang.Integer.compare(left, right)

          case (LocalAnon(left), LocalAnon(right)) =>
            java.lang.Long.compare(left, right)

          case (LocalAnonMut(left), LocalAnonMut(right)) =>
            java.lang.Long.compare(left, right)

          case (App(fnL, argsL), App(fnR, argsR)) =>
            val c1 = compareExpr(fnL, fnR)
            if (c1 != 0) c1
            else Order[NonEmptyList[Expr[A]]].compare(argsL, argsR)

          case (Let(argL, valueL, inL), Let(argR, valueR, inR)) =>
            val c1 = Order[Either[LocalAnon, Bindable]].compare(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = compareExpr(valueL, valueR)
              if (c2 != 0) c2
              else compareExpr(inL, inR)
            }

          case (LetMut(nameL, spanL), LetMut(nameR, spanR)) =>
            val c1 = Order[LocalAnonMut].compare(nameL, nameR)
            if (c1 != 0) c1
            else compareExpr(spanL, spanR)

          case (If(condL, thenL, elseL), If(condR, thenR, elseR)) =>
            val c1 = Order[BoolExpr[A]].compare(condL, condR)
            if (c1 != 0) c1
            else {
              val c2 = compareExpr(thenL, thenR)
              if (c2 != 0) c2
              else compareExpr(elseL, elseR)
            }

          case (Always(condL, thenL), Always(condR, thenR)) =>
            val c1 = Order[BoolExpr[A]].compare(condL, condR)
            if (c1 != 0) c1
            else compareExpr(thenL, thenR)

          case (
                GetEnumElement(argL, variantL, indexL, sizeL),
                GetEnumElement(argR, variantR, indexR, sizeR)
              ) =>
            val c1 = compareExpr(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = java.lang.Integer.compare(variantL, variantR)
              if (c2 != 0) c2
              else {
                val c3 = java.lang.Integer.compare(indexL, indexR)
                if (c3 != 0) c3
                else java.lang.Integer.compare(sizeL, sizeR)
              }
            }

          case (GetStructElement(argL, indexL, sizeL), GetStructElement(argR, indexR, sizeR)) =>
            val c1 = compareExpr(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = java.lang.Integer.compare(indexL, indexR)
              if (c2 != 0) c2
              else java.lang.Integer.compare(sizeL, sizeR)
            }

          case (Literal(left), Literal(right)) =>
            Order[Lit].compare(left, right)

          case (MakeEnum(variantL, arityL, famAritiesL), MakeEnum(variantR, arityR, famAritiesR)) =>
            val c1 = java.lang.Integer.compare(variantL, variantR)
            if (c1 != 0) c1
            else {
              val c2 = java.lang.Integer.compare(arityL, arityR)
              if (c2 != 0) c2
              else Order[List[Int]].compare(famAritiesL, famAritiesR)
            }

          case (MakeStruct(arityL), MakeStruct(arityR)) =>
            java.lang.Integer.compare(arityL, arityR)

          case (ZeroNat, ZeroNat) =>
            0

          case (SuccNat, SuccNat) =>
            0

          case (PrevNat(ofL), PrevNat(ofR)) =>
            compareExpr(ofL, ofR)

          case _ =>
            java.lang.Integer.compare(exprTag(left), exprTag(right))
        }

      def compare(left: Expr[A], right: Expr[A]): Int =
        compareExpr(left, right)
    }
  }
  // these hold bindings either in the code, or temporary
  // local ones, note CheapExpr never trigger a side effect
  sealed trait CheapExpr[+A] extends Expr[A]

  // name is set for recursive (but not tail recursive) methods
  case class Lambda[+A](
      captures: List[Expr[A]],
      recursiveName: Option[Bindable],
      args: NonEmptyList[Bindable],
      body: Expr[A]
  ) extends Expr[A] {
    def recursionKind: RecursionKind =
      RecursionKind.recursive(recursiveName.isDefined)

    def arity: Int = args.length
  }

  // This is a while loop, the result of which is result and the body is evaluated
  // while cond is true
  case class WhileExpr[A](
      cond: BoolExpr[A],
      effectExpr: Expr[A],
      result: LocalAnonMut
  ) extends Expr[A]

  case class Global[A](from: A, pack: PackageName, name: Bindable)
      extends CheapExpr[A]

  // these are immutable (but can be shadowed)
  case class Local(arg: Bindable) extends CheapExpr[Nothing]
  case class ClosureSlot(idx: Int) extends CheapExpr[Nothing]
  // these are is a separate namespace from Expr
  case class LocalAnon(ident: Long) extends CheapExpr[Nothing]
  // these are mutable variables that can be updated while evaluating an BoolExpr
  case class LocalAnonMut(ident: Long) extends CheapExpr[Nothing]

  // we aggregate all the applications to potentially make dispatch more efficient
  // note fn is never an App
  case class App[A](fn: Expr[A], arg: NonEmptyList[Expr[A]]) extends Expr[A]
  case class Let[A](
      arg: Either[LocalAnon, Bindable],
      expr: Expr[A],
      in: Expr[A]
  ) extends Expr[A]

  object Let {
    def apply[A](arg: Bindable, expr: Expr[A], in: Expr[A]): Expr[A] =
      // don't create let x = y in x, just return y
      in match {
        case Local(a) if a == arg => expr
        case _                    => Let(Right(arg), expr, in)
      }

    def apply[A](arg: LocalAnon, expr: Expr[A], in: Expr[A]): Expr[A] =
      // don't create let x = y in x, just return y
      in match {
        case LocalAnon(id) if id == arg.ident => expr
        case _                                => Let(Left(arg), expr, in)
      }
  }

  private def allNamesMany[A](exprs: IterableOnce[Expr[A]]): Set[Bindable] =
    exprs.iterator.foldLeft(Set.empty[Bindable]) { case (acc, ex) =>
      acc ++ allNames(ex)
    }

  private def freshSyntheticNames(
      prefix: String,
      count: Int,
      usedNames: Set[Bindable]
  ): List[Bindable] =
    Iterator
      .from(0)
      .map(i => Identifier.synthetic(s"${prefix}_$i"))
      .filterNot(usedNames)
      .take(count)
      .toList

  private def substituteClosureSlots[A](
      slots: Vector[CheapExpr[A]],
      expr: Expr[A]
  ): Expr[A] = {
    def loopExpr(ex: Expr[A]): Expr[A] =
      ex match {
        case ClosureSlot(idx) =>
          slots.lift(idx) match {
            case Some(ch) => ch
            case None     =>
              // $COVERAGE-OFF$
              sys.error(
                s"missing closure slot $idx in expression: $expr"
              )
            // $COVERAGE-ON$
          }
        case Lambda(captures, recName, args, body) =>
          // ClosureSlot indices are local to each lambda body, so we only
          // rewrite captures and leave the inner body unchanged.
          Lambda(captures.map(loopExpr), recName, args, body)
        case App(fn, appArgs) =>
          applyArgs(loopExpr(fn), appArgs.map(loopExpr))
        case Let(arg, value, in) =>
          Let(arg, loopExpr(value), loopExpr(in))
        case LetMut(name, in) =>
          LetMut(name, loopExpr(in))
        case If(cond, thenExpr, elseExpr) =>
          If(loopBool(cond), loopExpr(thenExpr), loopExpr(elseExpr))
        case Always(cond, thenExpr) =>
          Always(loopBool(cond), loopExpr(thenExpr))
        case WhileExpr(cond, effectExpr, result) =>
          WhileExpr(loopBool(cond), loopExpr(effectExpr), result)
        case PrevNat(of) =>
          PrevNat(loopExpr(of))
        case ge: GetEnumElement[?] =>
          ge.copy(arg = loopCheap(ge.arg))
        case gs: GetStructElement[?] =>
          gs.copy(arg = loopCheap(gs.arg))
        case Local(_) | Global(_, _, _) | LocalAnon(_) | LocalAnonMut(_) |
            Literal(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
            ZeroNat =>
          ex
      }

    def loopCheap(ex: CheapExpr[A]): CheapExpr[A] =
      loopExpr(ex) match {
        case ch: CheapExpr[A] => ch
        case notCheap         =>
          // $COVERAGE-OFF$
          sys.error(
            s"invariant violation: expected cheap expression when rewriting closure slots, got: $notCheap"
          )
        // $COVERAGE-ON$
      }

    def loopBool(ex: BoolExpr[A]): BoolExpr[A] =
      ex match {
        case EqualsLit(arg, lit) =>
          EqualsLit(loopCheap(arg), lit)
        case EqualsNat(arg, nat) =>
          EqualsNat(loopCheap(arg), nat)
        case And(left, right) =>
          And(loopBool(left), loopBool(right))
        case CheckVariant(arg, expect, size, famArities) =>
          CheckVariant(loopCheap(arg), expect, size, famArities)
        case ms: MatchString[?] =>
          ms.copy(arg = loopCheap(ms.arg))
        case SetMut(target, value) =>
          SetMut(target, loopExpr(value))
        case LetBool(arg, value, in) =>
          LetBool(arg, loopExpr(value), loopBool(in))
        case LetMutBool(name, in) =>
          LetMutBool(name, loopBool(in))
        case TrueConst =>
          TrueConst
      }

    loopExpr(expr)
  }

  /** Apply args to an expression while pushing through branch structure and
    * reducing immediate lambda application into lets.
    */
  def applyArgs[A](fn: Expr[A], args: NonEmptyList[Expr[A]]): Expr[A] =
    {
      def betaReduce(lam: Lambda[A]): Expr[A] = {
        val Lambda(captures, recName, lamArgs, body) = lam
        val captureCount = captures.length
        val argCount = args.length
        val usedNames =
          allNames(body) |
            allNamesMany(captures.iterator ++ args.iterator) |
            recName.toSet ++
            lamArgs.iterator

        val tmpNames =
          freshSyntheticNames(
            prefix = "bsts_apply",
            count = captureCount + argCount,
            usedNames = usedNames
          )
        val (captureTmpNames, argTmpNames) = tmpNames.splitAt(captureCount)
        val captureTmpLocals: List[CheapExpr[A]] =
          captureTmpNames.map(Local(_))

        val bodyWithCaptures =
          substituteClosureSlots(captureTmpLocals.toVector, body)

        val bodyWithArgs =
          lamArgs.toList.zip(argTmpNames).foldRight(bodyWithCaptures) {
            case ((argName, argTmp), in) =>
              Let(argName, Local(argTmp), in)
          }

        val bodyWithRec =
          recName match {
            case Some(name) =>
              val recLam =
                Lambda(
                  captures = captureTmpLocals,
                  recursiveName = recName,
                  args = lamArgs,
                  body = body
                )
              Let(name, recLam, bodyWithArgs)
            case None =>
              bodyWithArgs
          }

        val withArgTmps =
          argTmpNames.zip(args.toList).foldRight(bodyWithRec) {
            case ((argTmp, argExpr), in) =>
              Let(argTmp, argExpr, in)
          }

        captureTmpNames.zip(captures).foldRight(withArgTmps) {
          case ((captureTmp, captureExpr), in) =>
            Let(captureTmp, captureExpr, in)
        }
      }

      def resolveAlias(
          expr: Expr[A],
          aliases: Map[Bindable, Lambda[A]],
          seen: Set[Bindable]
      ): Expr[A] =
        expr match {
          case Local(name) if !seen(name) =>
            aliases.get(name) match {
              case Some(lam) => resolveAlias(lam, aliases, seen + name)
              case None      => expr
            }
          case _ =>
            expr
        }

      def loop(ex: Expr[A], aliases: Map[Bindable, Lambda[A]]): Expr[A] =
        resolveAlias(ex, aliases, Set.empty) match {
          case lam: Lambda[A] if lam.arity == args.length =>
            betaReduce(lam)
          case If(cond, thenExpr, elseExpr) =>
            If(cond, loop(thenExpr, aliases), loop(elseExpr, aliases))
          case Always(cond, thenExpr) =>
            Always(cond, loop(thenExpr, aliases))
          case let @ Let(arg, expr, in) =>
            val canPushPastLet = arg match {
              case Left(_) =>
                true
              case Right(name) =>
                !args.exists(a => allNames(a)(name))
            }
            if (canPushPastLet) {
              val aliases1 =
                arg match {
                  case Right(name) =>
                    resolveAlias(expr, aliases, Set.empty) match {
                      case lam: Lambda[A] => aliases.updated(name, lam)
                      case _              => aliases - name
                    }
                  case Left(_) =>
                    aliases
                }
              Let(arg, expr, loop(in, aliases1))
            }
            // We stop when the let-bound name appears in any argument. Pushing would
            // risk changing which binder those argument references resolve to.
            else App(let, args)
          case other =>
            // No additional structure to push through; keep this as a regular call.
            App(other, args)
        }

      loop(fn, Map.empty)
    }

  def allNames[A](expr: Expr[A]): Set[Bindable] = {
    def loopExpr(ex: Expr[A], acc: Set[Bindable]): Set[Bindable] =
      ex match {
        case Lambda(captures, recName, args, body) =>
          val acc1 = recName.fold(acc)(acc + _)
          val acc2 = args.toList.foldLeft(acc1)(_ + _)
          val acc3 = captures.foldLeft(acc2) { case (accN, exN) =>
            loopExpr(exN, accN)
          }
          loopExpr(body, acc3)
        case WhileExpr(cond, effectExpr, _) =>
          loopExpr(effectExpr, loopBool(cond, acc))
        case App(fn, appArgs) =>
          appArgs.toList.foldLeft(loopExpr(fn, acc)) { case (accN, exN) =>
            loopExpr(exN, accN)
          }
        case Let(arg, value, in) =>
          val acc1 = arg match {
            case Right(name) => acc + name
            case _           => acc
          }
          loopExpr(in, loopExpr(value, acc1))
        case LetMut(_, span) =>
          loopExpr(span, acc)
        case If(cond, thenExpr, elseExpr) =>
          loopExpr(elseExpr, loopExpr(thenExpr, loopBool(cond, acc)))
        case Always(cond, thenExpr) =>
          loopExpr(thenExpr, loopBool(cond, acc))
        case PrevNat(of) =>
          loopExpr(of, acc)
        case Local(name) =>
          acc + name
        case Global(_, _, name) =>
          acc + name
        case ge: GetEnumElement[?] =>
          loopCheap(ge.arg, acc)
        case gs: GetStructElement[?] =>
          loopCheap(gs.arg, acc)
        case ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) | Literal(_) |
            MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat =>
          acc
      }

    def loopCheap(ex: CheapExpr[A], acc: Set[Bindable]): Set[Bindable] =
      loopExpr(ex, acc)

    def loopBool(ex: BoolExpr[A], acc: Set[Bindable]): Set[Bindable] =
      ex match {
        case EqualsLit(expr, _) =>
          loopCheap(expr, acc)
        case EqualsNat(expr, _) =>
          loopCheap(expr, acc)
        case And(left, right) =>
          loopBool(right, loopBool(left, acc))
        case CheckVariant(expr, _, _, _) =>
          loopCheap(expr, acc)
        case MatchString(arg, _, _, _) =>
          loopCheap(arg, acc)
        case SetMut(_, value) =>
          loopExpr(value, acc)
        case TrueConst =>
          acc
        case LetBool(arg, value, in) =>
          val acc1 = arg match {
            case Right(name) => acc + name
            case _           => acc
          }
          loopBool(in, loopExpr(value, acc1))
        case LetMutBool(_, in) =>
          loopBool(in, acc)
      }

    loopExpr(expr, Set.empty)
  }

  private def topLevelFunctionArity[A](expr: Expr[A]): Option[Int] = {
    def sameArity(left: Option[Int], right: Option[Int]): Option[Int] =
      (left, right) match {
        case (Some(l), Some(r)) if l == r => Some(l)
        case _                            => None
      }

    def loop(
        ex: Expr[A],
        bindableArities: Map[Bindable, Int],
        anonArities: Map[Long, Int]
    ): Option[Int] =
      ex match {
        case fn: Lambda[A] =>
          Some(fn.arity)
        case Local(name) =>
          bindableArities.get(name)
        case LocalAnon(id) =>
          anonArities.get(id)
        case Let(arg, value, in) =>
          val valueArity = loop(value, bindableArities, anonArities)
          arg match {
            case Right(name) =>
              val bindableArities1 =
                valueArity match {
                  case Some(ar) => bindableArities.updated(name, ar)
                  case None     => bindableArities - name
                }
              loop(in, bindableArities1, anonArities)
            case Left(anon) =>
              val anonArities1 =
                valueArity match {
                  case Some(ar) => anonArities.updated(anon.ident, ar)
                  case None     => anonArities - anon.ident
                }
              loop(in, bindableArities, anonArities1)
          }
        case LetMut(_, in) =>
          loop(in, bindableArities, anonArities)
        case Always(_, thenExpr) =>
          loop(thenExpr, bindableArities, anonArities)
        case If(_, thenExpr, els) =>
          sameArity(
            loop(thenExpr, bindableArities, anonArities),
            loop(els, bindableArities, anonArities)
          )
        case _ =>
          None
      }

    loop(expr, Map.empty, Map.empty)
  }

  /** Recover a top-level lambda when normalization pushes lambda creation into
    * control-flow branches.
    */
  def recoverTopLevelLambda[A](expr: Expr[A]): Expr[A] =
    expr match {
      case _: Lambda[A] => expr
      case other        =>
        topLevelFunctionArity(other) match {
          case None => other
          case Some(arity) =>
            val usedNames = allNames(other)
            val args: NonEmptyList[Bindable] =
              NonEmptyList.fromListUnsafe(
                freshSyntheticNames(
                  prefix = s"bsts_top$arity",
                  count = arity,
                  usedNames = usedNames
                )
              )
            val appArgs: NonEmptyList[Expr[A]] =
              args.map[Expr[A]](arg => Local(arg))
            Lambda(
              captures = Nil,
              recursiveName = None,
              args = args,
              body = applyArgs(other, appArgs)
            )
        }
    }

  private def letAnons[A](binds: List[(LocalAnon, Expr[A])], in: Expr[A]): Expr[A] =
    binds.foldRight(in) { case ((b, e), r) =>
      Let(Left(b), e, r)
    }

  // Constructors are pure. Reuse repeated constructor creation in a scope by
  // introducing local aliases. We stay conservative and only memoize constructor
  // applications whose arguments are immutable CheapExpr values.
  private[bosatsu] def reuseConstructors[A: Order](expr: Expr[A]): Expr[A] = {
    import scala.collection.immutable.{SortedMap, SortedSet}

    given Ordering[Expr[A]] = Expr.exprOrder[A].toOrdering

    case class CseState(nextAnon: Long) {
      def nextState: (LocalAnon, CseState) = {
        val anon = LocalAnon(nextAnon)
        (anon, copy(nextAnon = nextAnon + 1L))
      }
    }

    def maxAnonExpr(ex: Expr[A]): Long = {
      def loopExpr(e: Expr[A], curr: Long): Long =
        e match {
          case Lambda(captures, _, _, body) =>
            val c1 = captures.foldLeft(curr) { case (acc, c) => loopExpr(c, acc) }
            loopExpr(body, c1)
          case WhileExpr(cond, effectExpr, result) =>
            val c1 = curr.max(result.ident)
            loopExpr(effectExpr, loopBool(cond, c1))
          case App(fn, args) =>
            args.toList.foldLeft(loopExpr(fn, curr)) { case (acc, a) =>
              loopExpr(a, acc)
            }
          case Let(arg, value, in) =>
            val c1 = arg match {
              case Left(LocalAnon(id)) => curr.max(id)
              case Right(_)            => curr
            }
            loopExpr(in, loopExpr(value, c1))
          case LetMut(name, span) =>
            loopExpr(span, curr.max(name.ident))
          case If(cond, thenExpr, elseExpr) =>
            loopExpr(elseExpr, loopExpr(thenExpr, loopBool(cond, curr)))
          case Always(cond, thenExpr) =>
            loopExpr(thenExpr, loopBool(cond, curr))
          case PrevNat(of) =>
            loopExpr(of, curr)
          case LocalAnon(id) =>
            curr.max(id)
          case LocalAnonMut(id) =>
            curr.max(id)
          case ge: GetEnumElement[?] =>
            loopExpr(ge.arg, curr)
          case gs: GetStructElement[?] =>
            loopExpr(gs.arg, curr)
          case Local(_) | ClosureSlot(_) | Global(_, _, _) | Literal(_) |
              MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat =>
            curr
        }

      def loopBool(b: BoolExpr[A], curr: Long): Long =
        b match {
          case EqualsLit(expr, _) =>
            loopExpr(expr, curr)
          case EqualsNat(expr, _) =>
            loopExpr(expr, curr)
          case And(left, right) =>
            loopBool(right, loopBool(left, curr))
          case CheckVariant(expr, _, _, _) =>
            loopExpr(expr, curr)
          case MatchString(arg, _, binds, _) =>
            binds.foldLeft(loopExpr(arg, curr)) { case (acc, bnd) =>
              acc.max(bnd.ident)
            }
          case SetMut(target, value) =>
            loopExpr(value, curr.max(target.ident))
          case TrueConst =>
            curr
          case LetBool(arg, value, in) =>
            val c1 = arg match {
              case Left(LocalAnon(id)) => curr.max(id)
              case Right(_)            => curr
            }
            loopBool(in, loopExpr(value, c1))
          case LetMutBool(name, in) =>
            loopBool(in, curr.max(name.ident))
        }

      loopExpr(ex, 0L)
    }

    object CseState {
      def initFromExpr(ex: Expr[A]): CseState =
        CseState(maxAnonExpr(ex) + 1L)
    }

    def isImmutableCheap(ex: Expr[A]): Boolean =
      ex match {
        case Local(_) | ClosureSlot(_) | LocalAnon(_) | Global(_, _, _) |
            Literal(_) =>
          true
        case ge: GetEnumElement[?] =>
          isImmutableCheap(ge.arg)
        case gs: GetStructElement[?] =>
          isImmutableCheap(gs.arg)
        case LocalAnonMut(_) =>
          false
        case _ =>
          false
      }

    // Only share constructor creation with immutable cheap args.
    // Non-cheap args may need scheduling/evaluation this pass does not do.
    def isShareableCtor(ex: Expr[A]): Boolean =
      ex match {
        case App(_: ConsExpr, args) =>
          args.forall(isImmutableCheap)
        case cons: ConsExpr =>
          cons.arity == 0
        case _ =>
          false
      }

    def countExprs(exprs: List[Expr[A]]): SortedMap[Expr[A], Int] =
      exprs.foldLeft(SortedMap.empty[Expr[A], Int]) { case (acc, ex) =>
        acc.updated(ex, acc.getOrElse(ex, 0) + 1)
      }

    def distinctInOrder(exprs: List[Expr[A]]): List[Expr[A]] = {
      val (rev, _) =
        exprs.foldLeft((List.empty[Expr[A]], SortedSet.empty[Expr[A]])) {
          case ((acc, seen), ex) =>
            if (seen(ex)) (acc, seen)
            else (ex :: acc, seen + ex)
        }
      rev.reverse
    }

    // "Linear scope" traversal:
    // - traverse application / PrevNat trees
    // - stop at control-flow and binding boundaries (If/Let/Lambda/etc.)
    def collectLinearInOrder(ex: Expr[A]): List[Expr[A]] = {
      val b = List.newBuilder[Expr[A]]

      def loop(e: Expr[A]): Unit = {
        if (isShareableCtor(e)) b += e
        e match {
          case App(fn, args) =>
            loop(fn)
            args.toList.foreach(loop)
          case PrevNat(of) =>
            loop(of)
          case _ =>
            ()
        }
      }

      loop(ex)
      b.result()
    }

    def replaceLinear(ex: Expr[A], replace: SortedMap[Expr[A], LocalAnon]): Expr[A] =
      replace.get(ex) match {
        case Some(loc) =>
          loc
        case None =>
          ex match {
            case App(fn, args) =>
              App(replaceLinear(fn, replace), args.map(replaceLinear(_, replace)))
            case PrevNat(of) =>
              PrevNat(replaceLinear(of, replace))
            case _ =>
              ex
          }
      }

    def allocateBinds(
        keys: List[Expr[A]],
        st: CseState
    ): (List[(LocalAnon, Expr[A])], CseState) =
      keys.foldLeft((List.empty[(LocalAnon, Expr[A])], st)) {
        case ((acc, state), key) =>
          val (anon, state1) = state.nextState
          (((anon, key) :: acc), state1)
      } match {
        case (rev, st1) => (rev.reverse, st1)
      }

    def linearScopeCse(ex: Expr[A], st: CseState): (Expr[A], CseState) = {
      val occs = collectLinearInOrder(ex)
      val counts = countExprs(occs)
      val dupes =
        distinctInOrder(occs.filter(k => counts.getOrElse(k, 0) > 1))

      if (dupes.isEmpty) (ex, st)
      else {
        val (binds, st1) = allocateBinds(dupes, st)
        val replace = SortedMap.from(binds.map(_.swap))
        (letAnons(binds, replaceLinear(ex, replace)), st1)
      }
    }

    def ifBranchCse(
        cond: BoolExpr[A],
        thenExpr: Expr[A],
        elseExpr: Expr[A],
        st: CseState
    ): (Expr[A], CseState) = {
      def readsBlocked(
          ex: Expr[A],
          blockedBindables: Set[Bindable],
          blockedAnonIds: Set[Long]
      ): Boolean =
        ex match {
          case Local(name) =>
            blockedBindables(name)
          case LocalAnon(id) =>
            blockedAnonIds(id)
          case App(fn, args) =>
            readsBlocked(fn, blockedBindables, blockedAnonIds) ||
              args.exists(readsBlocked(_, blockedBindables, blockedAnonIds))
          case PrevNat(of) =>
            readsBlocked(of, blockedBindables, blockedAnonIds)
          case ge: GetEnumElement[?] =>
            readsBlocked(ge.arg, blockedBindables, blockedAnonIds)
          case gs: GetStructElement[?] =>
            readsBlocked(gs.arg, blockedBindables, blockedAnonIds)
          case _ =>
            false
        }

      def collectBranchInOrder(
          ex: Expr[A],
          blockedBindables: Set[Bindable],
          blockedAnonIds: Set[Long]
      ): List[Expr[A]] = {
        val b = List.newBuilder[Expr[A]]

        def loop(
            e: Expr[A],
            blockedBindables: Set[Bindable],
            blockedAnonIds: Set[Long]
        ): Unit = {
          if (isShareableCtor(e) && !readsBlocked(e, blockedBindables, blockedAnonIds))
            b += e
          e match {
            case App(fn, args) =>
              loop(fn, blockedBindables, blockedAnonIds)
              args.toList.foreach(loop(_, blockedBindables, blockedAnonIds))
            case PrevNat(of) =>
              loop(of, blockedBindables, blockedAnonIds)
            case Let(arg, value, in) =>
              loop(value, blockedBindables, blockedAnonIds)
              arg match {
                case Right(name) =>
                  loop(in, blockedBindables + name, blockedAnonIds)
                case Left(LocalAnon(id)) =>
                  loop(in, blockedBindables, blockedAnonIds + id)
              }
            case _ =>
              ()
          }
        }

        loop(ex, blockedBindables, blockedAnonIds)
        b.result()
      }

      def replaceBranch(
          ex: Expr[A],
          replace: SortedMap[Expr[A], LocalAnon],
          blockedBindables: Set[Bindable],
          blockedAnonIds: Set[Long]
      ): Expr[A] =
        replace.get(ex) match {
          case Some(loc)
              if !readsBlocked(ex, blockedBindables, blockedAnonIds) =>
            loc
          case _ =>
            ex match {
              case App(fn, args) =>
                App(
                  replaceBranch(fn, replace, blockedBindables, blockedAnonIds),
                  args.map(replaceBranch(_, replace, blockedBindables, blockedAnonIds))
                )
              case PrevNat(of) =>
                PrevNat(replaceBranch(of, replace, blockedBindables, blockedAnonIds))
              case Let(arg, value, in) =>
                val value1 =
                  replaceBranch(value, replace, blockedBindables, blockedAnonIds)
                arg match {
                  case Right(name) =>
                    Let(
                      arg,
                      value1,
                      replaceBranch(
                        in,
                        replace,
                        blockedBindables + name,
                        blockedAnonIds
                      )
                    )
                  case Left(LocalAnon(id)) =>
                    Let(
                      arg,
                      value1,
                      replaceBranch(
                        in,
                        replace,
                        blockedBindables,
                        blockedAnonIds + id
                      )
                    )
                }
              case _ =>
                ex
            }
        }

      val thenOccs = collectBranchInOrder(thenExpr, Set.empty, Set.empty)
      val elseOccs = collectBranchInOrder(elseExpr, Set.empty, Set.empty)
      val elseSet = SortedSet.from(elseOccs)
      val common = distinctInOrder(thenOccs.filter(elseSet))

      if (common.isEmpty) (If(cond, thenExpr, elseExpr), st)
      else {
        val (binds, st1) = allocateBinds(common, st)
        val replace = SortedMap.from(binds.map(_.swap))
        val then1 = replaceBranch(thenExpr, replace, Set.empty, Set.empty)
        val else1 = replaceBranch(elseExpr, replace, Set.empty, Set.empty)
        (letAnons(binds, If(cond, then1, else1)), st1)
      }
    }

    def mapExprs(
        exprs: List[Expr[A]],
        st: CseState
    ): (List[Expr[A]], CseState) =
      exprs.foldLeft((List.empty[Expr[A]], st)) { case ((acc, curr), ex) =>
        val (ex1, curr1) = recurExpr(ex, curr)
        (ex1 :: acc, curr1)
      } match {
        case (rev, st1) => (rev.reverse, st1)
      }

    def recurExprCheap(
        expr: CheapExpr[A],
        st: CseState
    ): (CheapExpr[A], CseState) =
      recurExpr(expr, st) match {
        case (expr1: CheapExpr[A], st1) =>
          (expr1, st1)
        case (other, _) =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected cheap expression in bool rewriting, found: $other"
          )
        // $COVERAGE-ON$
      }

    def recurBool(b: BoolExpr[A], st: CseState): (BoolExpr[A], CseState) =
      b match {
        case EqualsLit(expr, lit) =>
          recurExprCheap(expr, st) match {
            case (expr1, st1) => (EqualsLit(expr1, lit), st1)
          }
        case EqualsNat(expr, nat) =>
          recurExprCheap(expr, st) match {
            case (expr1, st1) => (EqualsNat(expr1, nat), st1)
          }
        case And(left, right) =>
          val (l1, st1) = recurBool(left, st)
          val (r1, st2) = recurBool(right, st1)
          (And(l1, r1), st2)
        case CheckVariant(expr, expect, size, famArities) =>
          recurExprCheap(expr, st) match {
            case (expr1, st1) =>
              (CheckVariant(expr1, expect, size, famArities), st1)
          }
        case MatchString(arg, parts, binds, mustMatch) =>
          recurExprCheap(arg, st) match {
            case (arg1, st1) =>
              (MatchString(arg1, parts, binds, mustMatch), st1)
          }
        case SetMut(target, expr) =>
          val (expr1, st1) = recurExpr(expr, st)
          (SetMut(target, expr1), st1)
        case LetBool(arg, expr, in) =>
          val (expr1, st1) = recurExpr(expr, st)
          val (in1, st2) = recurBool(in, st1)
          (LetBool(arg, expr1, in1), st2)
        case LetMutBool(name, in) =>
          val (in1, st1) = recurBool(in, st)
          (LetMutBool(name, in1), st1)
        case TrueConst =>
          (TrueConst, st)
      }

    def recurExpr(ex: Expr[A], st: CseState): (Expr[A], CseState) = {
      val (rewritten, st1) =
        ex match {
          case Lambda(captures, recursiveName, args, body) =>
            val (captures1, stCaptures) = mapExprs(captures, st)
            val (body1, stBody) = recurExpr(body, stCaptures)
            (Lambda(captures1, recursiveName, args, body1), stBody)
          case WhileExpr(cond, effectExpr, result) =>
            val (cond1, stCond) = recurBool(cond, st)
            val (effectExpr1, stEffect) = recurExpr(effectExpr, stCond)
            (WhileExpr(cond1, effectExpr1, result), stEffect)
          case App(fn, args) =>
            val (fn1, stFn) = recurExpr(fn, st)
            val (args1, stArgs) = mapExprs(args.toList, stFn)
            (App(fn1, NonEmptyList.fromListUnsafe(args1)), stArgs)
          case Let(arg, expr, in) =>
            val (expr1, stExpr) = recurExpr(expr, st)
            val (in1, stIn) = recurExpr(in, stExpr)
            (Let(arg, expr1, in1), stIn)
          case LetMut(name, span) =>
            val (span1, st1) = recurExpr(span, st)
            (LetMut(name, span1), st1)
          case If(cond, thenExpr, elseExpr) =>
            val (cond1, stCond) = recurBool(cond, st)
            val (then1, stThen) = recurExpr(thenExpr, stCond)
            val (else1, stElse) = recurExpr(elseExpr, stThen)
            ifBranchCse(cond1, then1, else1, stElse)
          case Always(cond, thenExpr) =>
            val (cond1, stCond) = recurBool(cond, st)
            val (then1, stThen) = recurExpr(thenExpr, stCond)
            (Always(cond1, then1), stThen)
          case PrevNat(of) =>
            val (of1, st1) = recurExpr(of, st)
            (PrevNat(of1), st1)
          case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
              Global(_, _, _) | Literal(_) | MakeEnum(_, _, _) | MakeStruct(_) |
              SuccNat | ZeroNat | GetEnumElement(_, _, _, _) |
              GetStructElement(_, _, _) =>
            (ex, st)
        }

      linearScopeCse(rewritten, st1)
    }

    val init = CseState.initFromExpr(expr)
    try recurExpr(expr, init)._1
    catch {
      case _: StackOverflowError =>
        // This pass is a pure optimization. If recursion gets too deep on a
        // pathological tree, keep semantics by returning the original expression.
        expr
    }
  }

  case class LetMut[A](name: LocalAnonMut, span: Expr[A]) extends Expr[A] {
    // often we have several LetMut at once, return all them
    def flatten: (NonEmptyList[LocalAnonMut], Expr[A]) =
      span match {
        case next @ LetMut(_, _) =>
          val (anons, expr) = next.flatten
          (name :: anons, expr)
        case notLetMut =>
          (NonEmptyList.one(name), notLetMut)
      }
  }
  case class Literal(lit: Lit) extends CheapExpr[Nothing]

  // these result in Int values which are also used as booleans
  // evaluating these CAN have side effects of mutating LocalAnon
  // variables.
  sealed abstract class BoolExpr[+A] derives CanEqual {
    final def &&[A1 >: A](that: BoolExpr[A1]): BoolExpr[A1] =
      (this, that) match {
        case (TrueConst, r) => r
        case (l, TrueConst) => l
        case _              => And(this, that)
      }
  }
  object BoolExpr {
    private def boolTag[A](boolExpr: BoolExpr[A]): Int =
      boolExpr match {
        case _: EqualsLit[?]    => 0
        case _: EqualsNat[?]    => 1
        case _: And[?]          => 2
        case _: CheckVariant[?] => 3
        case _: MatchString[?]  => 4
        case _: SetMut[?]       => 5
        case TrueConst          => 6
        case _: LetBool[?]      => 7
        case _: LetMutBool[?]   => 8
      }

    private given Order[LocalAnon] = Order.by(_.ident)
    private given Order[LocalAnonMut] = Order.by(_.ident)
    private given Order[Bindable] = Identifier.Bindable.bindableOrder
    private given Order[Either[LocalAnon, Bindable]] =
      Order[Either[LocalAnon, Bindable]]
    private given Order[Lit] = Order.fromOrdering(using Lit.litOrdering)
    private given Order[DataRepr.Nat] = Order.by {
      case DataRepr.ZeroNat => 0
      case DataRepr.SuccNat => 1
    }

    given [A: Order]: Order[BoolExpr[A]] with {
      def compare(left: BoolExpr[A], right: BoolExpr[A]): Int =
        (left, right) match {
          case (EqualsLit(exprL, litL), EqualsLit(exprR, litR)) =>
            val c1 = Order[Expr[A]].compare(exprL, exprR)
            if (c1 != 0) c1
            else Order[Lit].compare(litL, litR)

          case (EqualsNat(exprL, natL), EqualsNat(exprR, natR)) =>
            val c1 = Order[Expr[A]].compare(exprL, exprR)
            if (c1 != 0) c1
            else Order[DataRepr.Nat].compare(natL, natR)

          case (And(leftL, rightL), And(leftR, rightR)) =>
            val c1 = compare(leftL, leftR)
            if (c1 != 0) c1
            else compare(rightL, rightR)

          case (
                CheckVariant(exprL, expectL, sizeL, famAritiesL),
                CheckVariant(exprR, expectR, sizeR, famAritiesR)
              ) =>
            val c1 = Order[Expr[A]].compare(exprL, exprR)
            if (c1 != 0) c1
            else {
              val c2 = java.lang.Integer.compare(expectL, expectR)
              if (c2 != 0) c2
              else {
                val c3 = java.lang.Integer.compare(sizeL, sizeR)
                if (c3 != 0) c3
                else Order[List[Int]].compare(famAritiesL, famAritiesR)
              }
            }

          case (
                MatchString(argL, partsL, bindsL, mustMatchL),
                MatchString(argR, partsR, bindsR, mustMatchR)
              ) =>
            val c1 = Order[Expr[A]].compare(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = Order[List[StrPart]].compare(partsL, partsR)
              if (c2 != 0) c2
              else {
                val c3 = Order[List[LocalAnonMut]].compare(bindsL, bindsR)
                if (c3 != 0) c3
                else java.lang.Boolean.compare(mustMatchL, mustMatchR)
              }
            }

          case (SetMut(targetL, exprL), SetMut(targetR, exprR)) =>
            val c1 = Order[LocalAnonMut].compare(targetL, targetR)
            if (c1 != 0) c1
            else Order[Expr[A]].compare(exprL, exprR)

          case (TrueConst, TrueConst) =>
            0

          case (LetBool(argL, exprL, inL), LetBool(argR, exprR, inR)) =>
            val c1 = Order[Either[LocalAnon, Bindable]].compare(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = Order[Expr[A]].compare(exprL, exprR)
              if (c2 != 0) c2
              else compare(inL, inR)
            }

          case (LetMutBool(nameL, spanL), LetMutBool(nameR, spanR)) =>
            val c1 = Order[LocalAnonMut].compare(nameL, nameR)
            if (c1 != 0) c1
            else compare(spanL, spanR)

          case _ =>
            java.lang.Integer.compare(boolTag(left), boolTag(right))
        }
    }
  }
  // returns 1 if it does, else 0
  case class EqualsLit[A](expr: CheapExpr[A], lit: Lit) extends BoolExpr[A]
  case class EqualsNat[A](expr: CheapExpr[A], nat: DataRepr.Nat)
      extends BoolExpr[A]
  // 1 if both are > 0
  case class And[A](e1: BoolExpr[A], e2: BoolExpr[A]) extends BoolExpr[A]
  // checks if variant matches, and if so, writes to
  // a given mut
  case class CheckVariant[A](
      expr: CheapExpr[A],
      expect: Int,
      size: Int,
      famArities: List[Int]
  ) extends BoolExpr[A]
  // set the mutable variable to the given expr and return true
  // string matching is complex done at a lower level
  case class MatchString[A](
      arg: CheapExpr[A],
      parts: List[StrPart],
      binds: List[LocalAnonMut],
      mustMatch: Boolean
  ) extends BoolExpr[A]
  // set the mutable variable to the given expr and return true
  case class SetMut[A](target: LocalAnonMut, expr: Expr[A]) extends BoolExpr[A]
  case object TrueConst extends BoolExpr[Nothing]
  case class LetBool[A](
      arg: Either[LocalAnon, Bindable],
      expr: Expr[A],
      in: BoolExpr[A]
  ) extends BoolExpr[A]

  case class LetMutBool[A](name: LocalAnonMut, span: BoolExpr[A])
      extends BoolExpr[A]
  object LetMutBool {
    def apply[A](lst: List[LocalAnonMut], span: BoolExpr[A]): BoolExpr[A] =
      lst.foldRight(span)(LetMutBool(_, _))
  }

  def hasSideEffect(bx: BoolExpr[Any]): Boolean =
    bx match {
      case SetMut(_, _) => true
      case TrueConst | CheckVariant(_, _, _, _) | EqualsLit(_, _) |
          EqualsNat(_, _) =>
        false
      case MatchString(_, _, b, _) => b.nonEmpty
      case And(b1, b2)             => hasSideEffect(b1) || hasSideEffect(b2)
      case LetBool(_, x, b)        =>
        hasSideEffect(b) || hasSideEffect(x)
      case LetMutBool(_, b) => hasSideEffect(b)
    }

  def hasSideEffect(bx: Expr[Any]): Boolean =
    bx match {
      case _: CheapExpr[?] => false
      case Always(b, x)    => hasSideEffect(b) || hasSideEffect(x)
      case App(f, as)      =>
        (f :: as).exists(hasSideEffect(_))
      case If(c, t, f) =>
        hasSideEffect(c) || hasSideEffect(t) || hasSideEffect(f)
      case Let(_, x, b) =>
        hasSideEffect(b) || hasSideEffect(x)
      case LetMut(_, in) => hasSideEffect(in)
      case PrevNat(n)    => hasSideEffect(n)
      case MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat |
          Lambda(_, _, _, _) =>
        // making a lambda or const is a pure function
        false
      case WhileExpr(_, _, _) =>
        // not all while loops have side effects technically, but we assume yes
        // for now. We could have a list of all the known control mutables here
        // but it seems hard
        true
    }

  case class If[A](cond: BoolExpr[A], thenExpr: Expr[A], elseExpr: Expr[A])
      extends Expr[A] {
    def flatten: (NonEmptyList[(BoolExpr[A], Expr[A])], Expr[A]) = {
      def combine(expr: Expr[A]): (List[(BoolExpr[A], Expr[A])], Expr[A]) =
        expr match {
          case If(c1, t1, e1) =>
            val (ifs, e2) = combine(e1)
            (((c1, t1)) :: ifs, e2)
          case last => (Nil, last)
        }

      val (rest, last) = combine(elseExpr)
      (NonEmptyList((cond, thenExpr), rest), last)
    }
  }
  case class Always[A](cond: BoolExpr[A], thenExpr: Expr[A]) extends Expr[A]
  object Always {
    object SetChain {
      // a common pattern is Always(SetMut(m, e), r)
      def unapply[A](
          expr: Expr[A]
      ): Option[(NonEmptyList[(LocalAnonMut, Expr[A])], Expr[A])] =
        expr match {
          case Always(SetMut(mut, v), res) =>
            val pair = (mut, v)
            unapply(res) match {
              case None              => Some((NonEmptyList.one(pair), res))
              case Some((muts, res)) => Some((pair :: muts, res))
            }
          case _ => None
        }
    }
  }
  def always[A](cond: BoolExpr[A], thenExpr: Expr[A]): Expr[A] =
    if (hasSideEffect(cond)) Always(cond, thenExpr)
    else thenExpr

  /** These aren't really super cheap, but when we treat them cheap we check
    * that we will only call them one time
    */
  case class GetEnumElement[A](
      arg: CheapExpr[A],
      variant: Int,
      index: Int,
      size: Int
  ) extends CheapExpr[A]
  case class GetStructElement[A](arg: CheapExpr[A], index: Int, size: Int)
      extends CheapExpr[A]

  sealed abstract class ConsExpr extends Expr[Nothing] {
    def arity: Int
  }
  // we need to compile calls to constructors into these
  case class MakeEnum(variant: Int, arity: Int, famArities: List[Int])
      extends ConsExpr

  private val boolFamArities = 0 :: 0 :: Nil
  private val listFamArities = 0 :: 2 :: Nil
  val FalseExpr: Expr[Nothing] = MakeEnum(0, 0, boolFamArities)
  val TrueExpr: Expr[Nothing] = MakeEnum(1, 0, boolFamArities)
  val UnitExpr: Expr[Nothing] = MakeStruct(0)

  def isTrueExpr[A](e: CheapExpr[A]): BoolExpr[A] =
    CheckVariant(e, 1, 0, boolFamArities)

  object ListExpr {
    val Nil: Expr[Nothing] = MakeEnum(0, 0, listFamArities)
    private val consFn = MakeEnum(1, 2, listFamArities)

    def cons[A](h: Expr[A], t: Expr[A]): Expr[A] =
      applyArgs(consFn, NonEmptyList(h, t :: List.empty))

    def notNil[A](e: CheapExpr[A]): BoolExpr[A] =
      CheckVariant(e, 1, 2, listFamArities)

    def head[A](arg: CheapExpr[A]): CheapExpr[A] =
      GetEnumElement(arg, 1, 0, 2)

    def tail[A](arg: CheapExpr[A]): CheapExpr[A] =
      GetEnumElement(arg, 1, 1, 2)
  }
  case class MakeStruct(arity: Int) extends ConsExpr
  case object ZeroNat extends ConsExpr {
    def arity = 0
  }
  // this is the function Nat -> Nat
  case object SuccNat extends ConsExpr {
    def arity = 1
  }

  case class PrevNat[A](of: Expr[A]) extends Expr[A]

  private inline def maybeMemo[F[_]: Monad, A](
      arg: Expr[A],
      tmp: F[Long]
  )(inline fn: CheapExpr[A] => F[Expr[A]]): F[Expr[A]] =
    arg match {
      case c: CheapExpr[A] => fn(c)
      case _               =>
        for {
          nm <- tmp
          bound = LocalAnon(nm)
          res <- fn(bound)
        } yield Let(bound, arg, res)
    }

  private val empty = (PackageName.PredefName, Constructor("EmptyList"))
  private val cons = (PackageName.PredefName, Constructor("NonEmptyList"))
  private val revName = Identifier.Name("reverse")
  private def reverseFn[A](from: A) =
    Global[A](from, PackageName.PredefName, revName)

  // drop all items in the tail after the first time fn returns true
  // as a result, we have 0 or 1 items where fn is true in the result
  // and it is always the last if there is 1
  def stopAt[A](nel: NonEmptyList[A])(fn: A => Boolean): NonEmptyList[A] =
    nel match {
      case NonEmptyList(h, _) if fn(h) => NonEmptyList(h, Nil)
      case s @ NonEmptyList(_, Nil)    => s
      case NonEmptyList(h0, h1 :: t)   => h0 :: stopAt(NonEmptyList(h1, t))(fn)
    }

  // same as fromLet below, but uses RefSpace
  def fromLet[A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A]
  )(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr[B] =
    (for {
      c <- RefSpace.allocCounter
      expr <- fromLet(from, name, rec, te, variantOf, c)
    } yield expr).run.value

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLet[F[_]: Monad, A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long]
  ): F[Expr[B]] = {

    type UnionMatch =
      NonEmptyList[(List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])]

    val wildMatch: UnionMatch = NonEmptyList((Nil, TrueConst, Nil), Nil)
    def isWildMatch(um: UnionMatch): Boolean =
      um match {
        case NonEmptyList((Nil, TrueConst, Nil), Nil) => true
        case _                                        => false
      }

    val emptyExpr: Expr[B] =
      empty match {
        case (p, c) =>
          variantOf(p, c) match {
            case Some(DataRepr.Enum(v, s, f)) => MakeEnum(v, s, f)
            case other                        =>
              /* We assume the structure of Lists to be standard linked lists
               * Empty cannot be a struct
               */

              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"empty List should be an enum, found: $other"
              )
            // $COVERAGE-ON$
          }
      }

    case class LambdaState(
        name: Option[Bindable],
        slots: Map[Bindable, Expr[B]]
    ) {
      def names: Set[Bindable] =
        slots.keySet ++ name.iterator

      def unname: LambdaState = LambdaState(None, slots)

      def apply(b: Bindable): Expr[B] =
        slots.get(b) match {
          case Some(expr) => expr
          case None       => Local(b)
        }

      def lambdaFrees(frees: List[Bindable]): (LambdaState, List[Expr[B]]) =
        name match {
          case None =>
            val newSlots = frees.iterator.zipWithIndex.map { case (b, idx) =>
              (b, ClosureSlot(idx))
            }.toMap
            val captures = frees.map(apply(_))
            (copy(slots = newSlots), captures)
          case Some(n) =>
            val newSlots = frees.iterator
              .filterNot(_ == n)
              .zipWithIndex
              .map { case (b, idx) => (b, ClosureSlot(idx)) }
              .toMap
            val captures = frees.flatMap { f =>
              if (cats.Eq[Identifier].neqv(f, n)) (apply(f) :: Nil)
              else Nil
            }
            (copy(slots = newSlots), captures)
        }

      def inLet(b: Bindable): LambdaState = copy(name = Some(b))
    }

    def substituteLocalsBool(
        m: Map[Bindable, CheapExpr[B]],
        e: BoolExpr[B]
    ): BoolExpr[B] =
      e match {
        case SetMut(mut, e) => SetMut(mut, substituteLocals(m, e))
        case And(b1, b2)    =>
          And(substituteLocalsBool(m, b1), substituteLocalsBool(m, b2))
        case EqualsLit(x, l) =>
          EqualsLit(substituteLocalsCheap(m, x), l)
        case EqualsNat(x, n) =>
          EqualsNat(substituteLocalsCheap(m, x), n)
        case TrueConst                           => TrueConst
        case CheckVariant(expr, expect, sz, fam) =>
          CheckVariant(substituteLocalsCheap(m, expr), expect, sz, fam)
        case ms: MatchString[?] =>
          ms.copy(arg = substituteLocalsCheap(m, ms.arg))
        case LetBool(b, a, in) =>
          val m1 = b match {
            case Right(b) => m - b
            case _        => m
          }
          LetBool(b, substituteLocals(m, a), substituteLocalsBool(m1, in))
        case LetMutBool(b, in) =>
          LetMutBool(b, substituteLocalsBool(m, in))
      }

    def substituteLocals(m: Map[Bindable, CheapExpr[B]], e: Expr[B]): Expr[B] =
      e match {
        case App(fn, appArgs) =>
          applyArgs(substituteLocals(m, fn), appArgs.map(substituteLocals(m, _)))
        case If(c, tcase, fcase) =>
          If(
            substituteLocalsBool(m, c),
            substituteLocals(m, tcase),
            substituteLocals(m, fcase)
          )
        case Always(c, e) =>
          Always(substituteLocalsBool(m, c), substituteLocals(m, e))
        case LetMut(mut, e) =>
          LetMut(mut, substituteLocals(m, e))
        case Let(n, v, in) =>
          val m1 = n match {
            case Right(b) => m - b
            case _        => m
          }
          Let(n, substituteLocals(m, v), substituteLocals(m1, in))
        case Local(n) =>
          m.get(n) match {
            case Some(mut) => mut
            case None      => e
          }
        case PrevNat(n)            => PrevNat(substituteLocals(m, n))
        case ge: GetEnumElement[?] =>
          ge.copy(arg = substituteLocalsCheap(m, ge.arg))
        case gs: GetStructElement[?] =>
          gs.copy(arg = substituteLocalsCheap(m, gs.arg))
        case Lambda(c, r, as, b) =>
          val m1 = m -- as.toList
          val b1 = substituteLocals(m1, b)
          Lambda(c, r, as, b1)
        case WhileExpr(c, ef, r) =>
          WhileExpr(substituteLocalsBool(m, c), substituteLocals(m, ef), r)
        case ClosureSlot(_) | Global(_, _, _) | LocalAnon(_) | LocalAnonMut(_) |
            MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | Literal(_) |
            ZeroNat =>
          e
      }
    def substituteLocalsCheap(
        m: Map[Bindable, CheapExpr[B]],
        e: CheapExpr[B]
    ): CheapExpr[B] =
      substituteLocals(m, e) match {
        case ch: CheapExpr[B] => ch
        case notCheap         =>
          sys.error(
            s"invariant violation: substitution didn't maintain cheap: $e => $notCheap"
          )
      }

    // assign any results to result and set the condition to false
    // and replace any tail calls to nm(args) with assigning args to those values
    case class ArgRecord(
        name: Bindable,
        tmp: LocalAnon,
        loopVar: LocalAnonMut
    )

    def toWhileBody(
        name: Bindable,
        args: NonEmptyList[ArgRecord],
        cond: LocalAnonMut,
        result: LocalAnonMut,
        body: Expr[B]
    ): Expr[B] = {

      def returnValue(v: Expr[B]): Expr[B] =
        setAll((cond, FalseExpr) :: (result, v) :: Nil, UnitExpr)

      // return Some(e) if this expression can be rewritten into a tail call to name,
      // else None
      def loop(expr: Expr[B]): Option[Expr[B]] =
        expr match {
          case App(Local(fnName), appArgs) if fnName == name =>
            // this is a tail call
            // we know the length of appArgs must match args or the code wouldn't have compiled
            // we have to first assign to the temp variables, and then assign the temp variables
            // to the results to make sure we don't have any data dependency issues with the values;
            val tmpAssigns = appArgs.iterator
              .zip(args.iterator)
              .flatMap { case (appArg, argRecord) =>
                val isSelfAssign = appArg match {
                  case LocalAnonMut(id) => id == argRecord.loopVar.ident
                  case _                => false
                }
                if (isSelfAssign)
                  Iterator.empty
                else
                  // don't create self assignments
                  Iterator.single(
                    (
                      (argRecord.tmp, appArg),
                      (argRecord.loopVar, argRecord.tmp)
                    )
                  )
              }
              .toList

            // this can be empty if all assignments were identities
            Some(
              letAnons(
                tmpAssigns.map(_._1),
                setAll(tmpAssigns.map(_._2), UnitExpr)
              )
            )
          case If(c, tcase, fcase) =>
            // this can potentially have tail calls inside the branches
            (loop(tcase), loop(fcase)) match {
              case (Some(t), Some(f)) =>
                Some(If(c, t, f))
              case (None, Some(f)) =>
                Some(If(c, returnValue(tcase), f))
              case (Some(t), None) =>
                Some(If(c, t, returnValue(fcase)))
              case (None, None) => None
            }
          case Always(c, e) =>
            loop(e).map(Always(c, _))
          case LetMut(m, e) =>
            loop(e).map(LetMut(m, _))
          case Let(b, v, in) =>
            // in is in tail position
            loop(in).map(Let(b, v, _))
          // the rest cannot have a call in tail position
          case App(_, _) | ClosureSlot(_) | GetEnumElement(_, _, _, _) |
              GetStructElement(_, _, _) | Global(_, _, _) |
              Lambda(_, _, _, _) | Literal(_) | Local(_) | LocalAnon(_) |
              LocalAnonMut(_) | MakeEnum(_, _, _) | MakeStruct(_) |
              PrevNat(_) | SuccNat | WhileExpr(_, _, _) | ZeroNat =>
            None
        }

      val bodyTrans = substituteLocals(
        args.toList.map(a => (a.name, a.loopVar)).toMap,
        body
      )

      // No tail call means this loop executes once and returns bodyTrans.
      loop(bodyTrans).getOrElse(returnValue(bodyTrans))
    }

    def buildLoop(
        name: Bindable,
        args: NonEmptyList[Bindable],
        initArgs: NonEmptyList[Expr[B]],
        body: Expr[B]
    ): F[Expr[B]] = {
      val mut = makeAnon.map(LocalAnonMut(_))
      val anon = makeAnon.map(LocalAnon(_))
      for {
        cond <- mut
        result <- mut
        args1 <- args.traverse(b => (anon, mut).mapN(ArgRecord(b, _, _)))
        whileBody = toWhileBody(name, args1, cond, result, body)
        allMuts = cond :: result :: args1.toList.map(_.loopVar)
      } yield {
        val initSets = args1.toList.zip(initArgs.toList).map {
          case (argRec, initArg) =>
            (argRec.loopVar, initArg)
        }
        letMutAll(
          allMuts,
          setAll(
            initSets,
            Always(
              SetMut(cond, TrueExpr),
              WhileExpr(isTrueExpr(cond), whileBody, result)
            )
          )
        )
      }
    }

    def loopLetVal(
        name: Bindable,
        e: TypedExpr[A],
        rec: RecursionKind,
        slots: LambdaState
    ): F[Expr[B]] =
      rec match {
        case RecursionKind.Recursive =>
          val e0 = loop(e, slots.inLet(name))
          def letrec(expr: Expr[B]): Expr[B] =
            expr match {
              case fn: Lambda[B] if fn.recursiveName == Some(name) => fn
              case fn: Lambda[?]                                   =>
                // loops always have a function name
                sys.error(
                  s"expected ${fn.recursiveName} == Some($name) in ${e.repr.render(80)} which compiled to $fn"
                )
              case _ =>
                sys.error(
                  s"expected ${e.repr.render(80)} to compile to a function, but got: $expr"
                )
            }

          e0.map(letrec)
        case RecursionKind.NonRecursive => loop(e, slots)
      }

    def recurToSelfCall(
        loopName: Bindable,
        loopType: Type,
        te: TypedExpr[A],
        inNestedLoop: Boolean
    ): TypedExpr[A] =
      te match {
        case TypedExpr.Generic(q, in) =>
          TypedExpr.Generic(q, recurToSelfCall(loopName, loopType, in, inNestedLoop))
        case TypedExpr.Annotation(in, tpe) =>
          TypedExpr.Annotation(
            recurToSelfCall(loopName, loopType, in, inNestedLoop),
            tpe
          )
        case TypedExpr.AnnotatedLambda(args, body, tag) =>
          TypedExpr.AnnotatedLambda(
            args,
            recurToSelfCall(loopName, loopType, body, inNestedLoop),
            tag
          )
        case TypedExpr.App(fn, args, tpe, tag) =>
          TypedExpr.App(
            recurToSelfCall(loopName, loopType, fn, inNestedLoop),
            args.map(recurToSelfCall(loopName, loopType, _, inNestedLoop)),
            tpe,
            tag
          )
        case TypedExpr.Let(arg, expr, in, rec, tag) =>
          if (arg == loopName) {
            if (rec.isRecursive) {
              TypedExpr.Let(
                arg,
                expr,
                in,
                rec,
                tag
              )
            } else {
              TypedExpr.Let(
                arg,
                recurToSelfCall(loopName, loopType, expr, inNestedLoop),
                in,
                rec,
                tag
              )
            }
          } else {
            TypedExpr.Let(
              arg,
              recurToSelfCall(loopName, loopType, expr, inNestedLoop),
              recurToSelfCall(loopName, loopType, in, inNestedLoop),
              rec,
              tag
            )
          }
        case TypedExpr.Loop(args, body, tag) =>
          TypedExpr.Loop(
            args.map { case (n, expr) =>
              (n, recurToSelfCall(loopName, loopType, expr, inNestedLoop))
            },
            recurToSelfCall(loopName, loopType, body, inNestedLoop = true),
            tag
          )
        case TypedExpr.Recur(args, tpe, tag) =>
          val args1 = args.map(recurToSelfCall(loopName, loopType, _, inNestedLoop))
          if (inNestedLoop) TypedExpr.Recur(args1, tpe, tag)
          else {
            val fn = TypedExpr.Local(loopName, loopType, tag)
            TypedExpr.App(fn, args1, tpe, tag)
          }
        case TypedExpr.Match(arg, branches, tag) =>
          TypedExpr.Match(
            recurToSelfCall(loopName, loopType, arg, inNestedLoop),
            branches.map { branch =>
              branch.copy(
                guard =
                  branch.guard
                    .map(recurToSelfCall(loopName, loopType, _, inNestedLoop)),
                expr = recurToSelfCall(
                  loopName,
                  loopType,
                  branch.expr,
                  inNestedLoop
                )
              )
            },
            tag
          )
        case n @ (TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _)) =>
          n
      }

    def loop(te: TypedExpr[A], slots: LambdaState): F[Expr[B]] =
      te match {
        case TypedExpr.Generic(_, expr)              => loop(expr, slots)
        case TypedExpr.Annotation(term, _)           => loop(term, slots)
        case TypedExpr.AnnotatedLambda(args, res, _) =>
          val frees = TypedExpr.freeVars(te :: Nil)
          val (slots1, captures) = slots.lambdaFrees(frees)
          loop(res, slots1.unname).map(
            Lambda(captures, slots.name, args.map(_._1), _)
          )
        case TypedExpr.Global(pack, cons @ Constructor(_), _, _) =>
          Monad[F].pure(variantOf(pack, cons) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Enum(v, a, f) => MakeEnum(v, a, f)
                case DataRepr.Struct(a)     => MakeStruct(a)
                case DataRepr.NewType       => MakeStruct(1)
                case DataRepr.ZeroNat       => ZeroNat
                case DataRepr.SuccNat       => SuccNat
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          })
        case TypedExpr.Global(pack, notCons: Bindable, _, _) =>
          Monad[F].pure(Global(from, pack, notCons))
        case TypedExpr.Local(bind, _, _) =>
          Monad[F].pure(slots(bind))
        case TypedExpr.App(fn, as, _, _) =>
          (loop(fn, slots.unname), as.traverse(loop(_, slots.unname)))
            .mapN(applyArgs(_, _))
        case TypedExpr.Loop(args, body, _) =>
          val avoid: Set[Bindable] =
            TypedExpr.allVarsSet(body :: args.toList.map(_._2)) ++
              slots.names
          val loopName = freshSyntheticNames("loop", 1, avoid).head
          val loopArgs = args.map { case (n, arg) =>
            (n, arg.getType)
          }
          val loopType = Type.Fun(loopArgs.map(_._2), body.getType)
          val body1 = recurToSelfCall(loopName, loopType, body, inNestedLoop = false)
          (loop(body1, slots), args.traverse { case (_, init) => loop(init, slots) })
            .tupled
            .flatMap { case (bodyExpr, initVals) =>
              buildLoop(loopName, loopArgs.map(_._1), initVals, bodyExpr)
            }
        case TypedExpr.Let(a, e, in, r, _) =>
          (loopLetVal(a, e, r, slots.unname), loop(in, slots))
            .mapN(Let(a, _, _))
        case TypedExpr.Recur(_, _, _) =>
          // Loops should be lowered from TypedExpr.Loop and not escape raw Recur nodes.
          sys.error(s"unreachable raw recur in Matchless lowering: ${te.repr.render(80)}")
        case TypedExpr.Literal(lit, _, _)      => Monad[F].pure(Literal(lit))
        case TypedExpr.Match(arg, branches, _) =>
          (
            loop(arg, slots.unname),
            branches.traverse { branch =>
              (
                branch.guard.traverse(loop(_, slots.unname)),
                loop(branch.expr, slots.unname)
              ).mapN { (guard, te) =>
                MatchBranch(branch.pattern, guard, te)
              }
            }
          ).tupled
            .flatMap { case (a, b) => matchExpr(a, makeAnon, b) }
      }

    /*
     * A simple pattern is either:
     * 1. one that has no binding what-so-ever
     * 2. a total binding to a given name
     * 3. or we return None indicating not one of these
     */
    def maybeSimple(
        p: Pattern[(PackageName, Constructor), Type]
    ): Option[Either[Bindable, Unit]] =
      p match {
        case Pattern.WildCard   => Some(Right(()))
        case Pattern.Literal(_) =>
          // Literals are never total
          None
        case Pattern.Var(v)      => Some(Left(v))
        case Pattern.Named(v, p) =>
          maybeSimple(p) match {
            case Some(Right(_)) => Some(Left(v))
            case _              => None
          }
        case Pattern.StrPat(s) =>
          s match {
            case NonEmptyList(Pattern.StrPart.WildStr, Nil) => Some(Right(()))
            case NonEmptyList(Pattern.StrPart.NamedStr(n), Nil) => Some(Left(n))
            case _                                              => None
          }
        case Pattern.ListPat(l) =>
          l match {
            case Pattern.ListPart.WildList :: Nil     => Some(Right(()))
            case Pattern.ListPart.NamedList(n) :: Nil => Some(Left(n))
            case _                                    => None
          }
        case Pattern.Annotation(p, _)                    => maybeSimple(p)
        case Pattern.PositionalStruct((pack, cname), ps) =>
          // Only branch-free structs with no inner names are simple
          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(_) | DataRepr.NewType =>
                  ps.traverse(maybeSimple).flatMap { inners =>
                    if (inners.forall(_ == Right(()))) Some(Right(()))
                    else None
                  }
                case _ => None
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, t) =>
          (h :: t.toList).traverse(maybeSimple).flatMap { inners =>
            if (inners.forall(_ == Right(()))) Some(Right(()))
            else None
          }
      }

    // handle list matching, this is a while loop, that is evaluting
    // lst is initialized to init, leftAcc is initialized to empty
    // tail until it is true while mutating lst => lst.tail
    // this has the side-effect of mutating lst and leftAcc as well as any side effects that check has
    // which could have nested searches of its own
    def searchList(
        lst: LocalAnonMut,
        init: CheapExpr[B],
        check: BoolExpr[B],
        leftAcc: Option[LocalAnonMut]
    ): F[BoolExpr[B]] =
      (
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnon(_)),
        makeAnon.map(LocalAnonMut(_))
      )
        .mapN { (resMut, letBind, currentList) =>
          val initSets =
            (resMut, FalseExpr) ::
              (currentList, init) ::
              (leftAcc.toList.map { left =>
                (left, ListExpr.Nil)
              })

          val whileCheck = ListExpr.notNil(currentList)
          val effect: Expr[B] =
            setAll(
              (lst, currentList) :: Nil,
              If(
                check,
                setAll(
                  (currentList, ListExpr.Nil) ::
                    (resMut, TrueExpr) ::
                    Nil,
                  UnitExpr
                ),
                setAll(
                  (currentList, ListExpr.tail(currentList)) ::
                    leftAcc.toList.map { left =>
                      (left, ListExpr.cons(ListExpr.head(currentList), left))
                    },
                  UnitExpr
                )
              )
            )
          val searchLoop =
            setAll(initSets, WhileExpr(whileCheck, effect, resMut))

          LetMutBool(
            resMut :: currentList :: Nil,
            LetBool(Left(letBind), searchLoop, isTrueExpr(resMut))
          )
        }
      /*
            Dynamic { (scope: Scope) =>
              var res = false
              var currentList = initF(scope)
              var leftList = VList.VNil
              scope.updateMut(left, leftList)
              while (currentList ne null) {
                currentList match {
                  case nonempty @ VList.Cons(head, tail) =>
                    scope.updateMut(mutV, nonempty)
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else {
                      currentList = tail
                      leftList = VList.Cons(head, leftList)
                      scope.updateMut(left, leftList)
                    }
                  case _ =>
                    currentList = null
                  // we don't match empty lists
                }
              }
              res
            }
       */

    // return the check expression for the check we need to do, and the list of bindings
    // if must match is true, we know that the pattern must match, so we can potentially remove some checks
    def doesMatch(
        arg: CheapExpr[B],
        pat: Pattern[(PackageName, Constructor), Type],
        mustMatch: Boolean
    ): F[UnionMatch] = {
      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          Monad[F].pure(wildMatch)
        case Pattern.Literal(lit) =>
          Monad[F].pure(NonEmptyList((Nil, EqualsLit(arg, lit), Nil), Nil))
        case Pattern.Var(v) =>
          Monad[F].pure(NonEmptyList((Nil, TrueConst, (v, arg) :: Nil), Nil))
        case Pattern.Named(v, p) =>
          doesMatch(arg, p, mustMatch).map(_.map { case (l0, cond, bs) =>
            (l0, cond, (v, arg) :: bs)
          })
        case strPat @ Pattern.StrPat(items) =>
          strPat.simplify match {
            case Some(simpler) => doesMatch(arg, simpler, mustMatch)
            case None          =>
              val sbinds: List[Bindable] =
                items.toList
                  .collect {
                    // that each name is distinct
                    // should be checked in the SourceConverter/TotalityChecking code
                    case Pattern.StrPart.NamedStr(n)  => n
                    case Pattern.StrPart.NamedChar(n) => n
                  }

              val pat = items.toList.map {
                case Pattern.StrPart.NamedStr(_)  => StrPart.IndexStr
                case Pattern.StrPart.NamedChar(_) => StrPart.IndexChar
                case Pattern.StrPart.WildStr      => StrPart.WildStr
                case Pattern.StrPart.WildChar     => StrPart.WildChar
                case Pattern.StrPart.LitStr(s)    => StrPart.LitStr(s)
              }

              sbinds
                .traverse { b =>
                  makeAnon.map(LocalAnonMut(_)).map((b, _))
                }
                .map { binds =>
                  val ms = binds.map(_._2)

                  NonEmptyList.one(
                    (ms, MatchString(arg, pat, ms, mustMatch), binds)
                  )
                }
          }
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p) => doesMatch(arg, p, mustMatch)
            case Left(
                  (glob, right @ NonEmptyList(Pattern.ListPart.Item(_), _))
                ) =>
              // we have a non-trailing list pattern
              // to match, this becomes a search problem
              // we loop over all the matches of p in the list,
              // then we put the prefix on the glob, and the suffix against
              // the tail.
              //
              // we know all the bindings we will make, allocate
              // anons for them, do the loop, and then return
              // the boolean of did we match
              val leftF: F[Option[(LocalAnonMut, Bindable)]] =
                glob match {
                  case Pattern.ListPart.WildList =>
                    Monad[F].pure(None)
                  case Pattern.ListPart.NamedList(ln) =>
                    makeAnon.map(nm => Some((LocalAnonMut(nm), ln)))
                }

              (leftF, makeAnon).tupled
                .flatMap { case (optAnonLeft, tmpList) =>
                  val anonList = LocalAnonMut(tmpList)

                  doesMatch(anonList, Pattern.ListPat(right.toList), false)
                    .flatMap { cases =>
                      cases.traverse {
                        case (_, TrueConst, _) =>
                          // $COVERAGE-OFF$

                          // this shouldn't be possible, since there are no total list matches with
                          // one item since we recurse on a ListPat with the first item being Right
                          // which as we can see above always returns Some(_)
                          throw new IllegalStateException(
                            s"$right should not be a total match"
                          )
                        // $COVERAGE-ON$
                        case (preLet, expr, binds) =>
                          val letTail = anonList :: preLet

                          val (resLet, leftOpt, resBind) =
                            optAnonLeft match {
                              case Some((anonLeft, ln)) =>
                                val revList =
                                  applyArgs(
                                    reverseFn(from),
                                    NonEmptyList.one(anonLeft)
                                  )
                                (
                                  anonLeft :: letTail,
                                  Some(anonLeft),
                                  (ln, revList) :: binds
                                )
                              case None =>
                                (letTail, None, binds)
                            }

                          searchList(anonList, arg, expr, leftOpt)
                            .map(s => (resLet, s, resBind))
                      }
                    }
                }
            case Left(
                  (glob, right @ NonEmptyList(_: Pattern.ListPart.Glob, _))
                ) =>
              // we search on the right side, so the left will match nothing
              // this should be banned by SourceConverter/TotalityChecker because
              // it is confusing, but it can be handled

              // $COVERAGE-OFF$
              glob match {
                case Pattern.ListPart.WildList =>
                  // no binding on the let
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                case Pattern.ListPart.NamedList(ln) =>
                  // bind empty to ln
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                    .map { nel =>
                      nel.map { case (preLet, expr, binds) =>
                        (preLet, expr, (ln, emptyExpr) :: binds)
                      }
                    }
              }
            // $COVERAGE-ON$
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p, mustMatch)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          type Locals = Chain[(LocalAnonMut, Expr[B])]
          def asStruct(
              getter: Int => CheapExpr[B]
          ): WriterT[F, Locals, UnionMatch] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            def operate(
                pat: Pattern[(PackageName, Constructor), Type],
                idx: Int
            ): WriterT[F, Locals, UnionMatch] =
              maybeSimple(pat) match {
                case Some(Right(())) =>
                  // this is a total match
                  WriterT.value(wildMatch)
                case Some(Left(v)) =>
                  // this is just an alias
                  WriterT.value(
                    NonEmptyList((Nil, TrueConst, (v, getter(idx)) :: Nil), Nil)
                  )
                case None =>
                  // we make an anonymous variable and write to that:
                  for {
                    nm <- WriterT.valueT[F, Locals, Long](makeAnon)
                    lam = LocalAnonMut(nm)
                    um <- WriterT.valueT[F, Locals, UnionMatch](
                      doesMatch(lam, pat, mustMatch)
                    )
                    // if this is a total match, we don't need to do the getter at all
                    chain =
                      if (isWildMatch(um)) Chain.empty
                      else Chain.one((lam, getter(idx)))
                    _ <- WriterT.tell[F, Locals](chain)
                  } yield um
              }

            val ands: WriterT[F, Locals, List[UnionMatch]] =
              params.zipWithIndex
                .traverse { case (pati, i) => operate(pati, i) }

            ands.map(NonEmptyList.fromList(_) match {
              case None      => wildMatch
              case Some(nel) =>
                product(nel) { case ((l1, o1, b1), (l2, o2, b2)) =>
                  (l1 ::: l2, o1 && o2, b1 ::: b2)
                }
            })
          }

          def forStruct(size: Int) =
            asStruct(pos => GetStructElement(arg, pos, size)).run
              .map { case (anons, ums) =>
                ums.map { case (pre, cond, bind) =>
                  val pre1 = anons.foldLeft(pre) { case (pre, (a, _)) =>
                    a :: pre
                  }
                  // we have to set these variables before we can evaluate the condition
                  val cond1 = anons.foldLeft(cond) { case (c, (a, e)) =>
                    SetMut(a, e) && c
                  }
                  (pre1, cond1, bind)
                }
              }

          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(size)        => forStruct(size)
                case DataRepr.NewType             => forStruct(1)
                case DataRepr.Enum(vidx, size, f) =>
                  // if we match the variant, then treat it as a struct
                  val cv: BoolExpr[B] =
                    if (mustMatch) TrueConst
                    else CheckVariant(arg, vidx, size, f)
                  asStruct(pos => GetEnumElement(arg, vidx, pos, size)).run
                    .map { case (anons, ums) =>
                      if (isWildMatch(ums)) {
                        // we just need to check the variant
                        assert(
                          anons.isEmpty,
                          "anons must by construction always be empty on wildMatch"
                        )
                        NonEmptyList((Nil, cv, Nil), Nil)
                      } else {
                        // now we need to set up the binds if the variant is right
                        val cond1 = anons.foldLeft(cv) {
                          case (c, (mut, expr)) =>
                            c && SetMut(mut, expr)
                        }

                        ums.map { case (pre, cond, b) =>
                          val pre1 = anons.foldLeft(pre) {
                            case (pre, (mut, _)) => mut :: pre
                          }
                          (pre1, cond1 && cond, b)
                        }
                      }
                    }
                case DataRepr.ZeroNat =>
                  val cv: BoolExpr[B] =
                    if (mustMatch) TrueConst
                    else EqualsNat(arg, DataRepr.ZeroNat)
                  Monad[F].pure(NonEmptyList((Nil, cv, Nil), Nil))
                case DataRepr.SuccNat =>
                  params match {
                    case single :: Nil =>
                      // if we match, we recur on the inner pattern and prev of current
                      val check =
                        if (mustMatch) TrueConst
                        else EqualsNat(arg, DataRepr.SuccNat)

                      val ignoreInner = single match {
                        case Pattern.WildCard => true
                        case _                => false
                      }

                      if (!ignoreInner) {
                        for {
                          nm <- makeAnon
                          loc = LocalAnonMut(nm)
                          prev = PrevNat(arg)
                          rest <- doesMatch(loc, single, mustMatch)
                        } yield rest.map { case (preLets, cond, res) =>
                          (
                            loc :: preLets,
                            check && SetMut(loc, prev) && cond,
                            res
                          )
                        }
                      } else {
                        // we don't need to bind the prev
                        Monad[F].pure(wildMatch.map {
                          case (preLets, cond, res) =>
                            (
                              preLets,
                              check && cond,
                              res
                            )
                        })
                      }
                    case other =>
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        s"expected typechecked Nat to only have one param, found: $other in $pat"
                      )
                    // $COVERAGE-ON$
                  }
              }
            case None =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, ts) =>
          // note this list is exactly as long as h :: ts
          val unionMustMatch = NonEmptyList.fromListUnsafe(
            List.fill(ts.size)(false) ::: mustMatch :: Nil
          )
          ((h :: ts)
            .zip(unionMustMatch))
            .traverse { case (p, mm) => doesMatch(arg, p, mm) }
            .map { nene =>
              val nel = nene.flatten
              // at the first total match, we can stop
              stopAt(nel) {
                case (_, TrueConst, _) => true
                case _                 => false
              }
            }
      }
    }

    def lets(binds: List[(Bindable, Expr[B])], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case ((b, e), r) =>
        Let(b, e, r)
      }

    def letAnons(binds: List[(LocalAnon, Expr[B])], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case ((b, e), r) =>
        Let(Left(b), e, r)
      }

    def letMutAll(binds: List[LocalAnonMut], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case (anon, rest) =>
        // TODO: sometimes we generate code like
        // LetMut(x, Always(SetMut(x, y), f))
        // with no side effects in y or f
        // this would be better written as
        // Let(x, y, f)
        LetMut(anon, rest)
      }

    def setAll(ls: List[(LocalAnonMut, Expr[B])], ret: Expr[B]): Expr[B] =
      ls.foldRight(ret) { case ((l, e), r) =>
        Always(SetMut(l, e), r)
      }

    case class MatchBranch(
        pattern: Pattern[(PackageName, Constructor), Type],
        guard: Option[Expr[B]],
        rhs: Expr[B]
    )

    // A row in the pattern matrix: patterns for each occurrence,
    // a right-hand side, and the bindings accumulated so far.
    // pats may be empty after all columns are specialized away; in that
    // case the row is a total match for the remaining occurrences.
    case class MatchRow(
        pats: List[Pattern[(PackageName, Constructor), Type]],
        guard: Option[Expr[B]],
        rhs: Expr[B],
        binds: List[(Bindable, Expr[B])]
    ) {
      // Replace exactly one column's pattern with a new list of patterns.
      // We always patch with count == 1 because each column represents a
      // single occurrence; specialization removes or expands that one column.
      def patchPats(
          colIdx: Int,
          pats1: List[Pattern[(PackageName, Constructor), Type]]
      ): MatchRow =
        copy(pats = pats.patch(colIdx, pats1, 1))
    }
    object MatchRow {
      def fromBranch(branch: MatchBranch): MatchRow =
        MatchRow(branch.pattern :: Nil, branch.guard, branch.rhs, Nil)
    }

    // Head signatures represent refutable "shapes" we can branch on.
    // These correspond to Maranget's constructor/literal head cases.
    // See: https://compiler.club/compiling-pattern-matching/
    enum HeadSig derives CanEqual {
      case EnumSig(
          ctor: (PackageName, Constructor),
          variant: Int,
          arity: Int,
          famArities: List[Int]
      )
      case StructSig(ctor: (PackageName, Constructor), arity: Int)
          extends HeadSig
      case ZeroSig extends HeadSig
      case SuccSig extends HeadSig
      case LitSig(lit: Lit)
    }
    import HeadSig.*

    // Normalize patterns to shrink the grammar into cases the matrix
    // algorithm handles (e.g., collapse simple string/list patterns, remove Annotation).
    def normalizePattern(
        p: Pattern[(PackageName, Constructor), Type]
    ): Pattern[(PackageName, Constructor), Type] =
      p match {
        case Pattern.Annotation(p1, _) => normalizePattern(p1)
        case Pattern.Named(v, p1)      =>
          Pattern.Named(v, normalizePattern(p1))
        case Pattern.Var(_) | Pattern.WildCard | Pattern.Literal(_) => p
        case Pattern.PositionalStruct(n, ps)                        =>
          Pattern.PositionalStruct(n, ps.map(normalizePattern))
        case Pattern.Union(h, t) =>
          Pattern.Union(normalizePattern(h), t.map(normalizePattern))
        case sp @ Pattern.StrPat(_) =>
          sp.simplify match {
            case Some(p1) => normalizePattern(p1)
            case None     => sp
          }
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p1) => normalizePattern(p1)
            case Left(_)   =>
              val parts = lp.parts.map {
                case Pattern.ListPart.Item(p1) =>
                  Pattern.ListPart.Item(normalizePattern(p1))
                case g => g
              }
              Pattern.ListPat(parts)
          }
      }

    // Non-orthogonal patterns (string globs, non-trailing list globs) have
    // backtracking/search semantics and are not handled by the matrix compiler.
    def isNonOrthogonal(
        p: Pattern[(PackageName, Constructor), Type]
    ): Boolean =
      p match {
        case Pattern.Annotation(p1, _) => isNonOrthogonal(p1)
        case Pattern.Named(_, p1)      => isNonOrthogonal(p1)
        case Pattern.Var(_) | Pattern.WildCard | Pattern.Literal(_) =>
          false
        case sp @ Pattern.StrPat(_)  => sp.simplify.isEmpty
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p1) => isNonOrthogonal(p1)
            case Left(_)   => true
          }
        case Pattern.PositionalStruct(_, ps) =>
          ps.exists(isNonOrthogonal)
        case Pattern.Union(h, t) =>
          isNonOrthogonal(h) || t.exists(isNonOrthogonal)
      }

    // Pull off aliases and bindings that capture the whole occurrence.
    // This keeps the matrix focused on refutable structure.
    // Return patterns are normalized and never Named/Var/Annotation.
    // For matrix compilation, the remaining cases are:
    // WildCard, Literal, PositionalStruct, or Union.
    def peelPattern(
        p: Pattern[(PackageName, Constructor), Type],
        occ: CheapExpr[B]
    ): (List[(Bindable, Expr[B])], Pattern[(PackageName, Constructor), Type]) =
      p match {
        case Pattern.Named(v, inner) =>
          val (bs, core) = peelPattern(inner, occ)
          ((v, occ) :: bs, core)
        case Pattern.Var(v) =>
          ((v, occ) :: Nil, Pattern.WildCard)
        case Pattern.Annotation(inner, _) =>
          peelPattern(inner, occ)
        case Pattern.WildCard | Pattern.Literal(_) |
            Pattern.PositionalStruct(_, _) | Pattern.Union(_, _) =>
          (Nil, p)
        case Pattern.StrPat(_) | Pattern.ListPat(_) =>
          // normalizePattern converts orthogonal list/string patterns into
          // PositionalStruct/Literal/WildCard, and isNonOrthogonal filters out
          // the remaining list/string search patterns before matrix compilation.
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"unexpected non-orthogonal pattern in peelPattern: $p"
          )
        // $COVERAGE-ON$
      }

    // Expand all top-level unions in the row's pattern vector.
    // returns at liast as many rows as it takes in
    def expandRows(rows: List[MatchRow]): List[MatchRow] = {
      def expandPats(
          pats: List[Pattern[(PackageName, Constructor), Type]]
      ): NonEmptyList[List[Pattern[(PackageName, Constructor), Type]]] =
        pats match {
          case Nil       => NonEmptyList(Nil, Nil)
          case p :: tail =>
            for {
              p1 <- Pattern.flatten(p)
              rest <- expandPats(tail)
            } yield p1 :: rest
        }

      rows.flatMap { row =>
        expandPats(row.pats).toList.map(ps => row.copy(pats = ps))
      }
    }

    // Normalize a row by collapsing aliases into bindings and simplifying
    // patterns, without changing the left-to-right match semantics.
    // Invariant: row.pats.length == occs.length.
    def normalizeRow(
        row: MatchRow,
        occs: List[CheapExpr[B]]
    ): MatchRow = {
      val (bindsRev, patsRev) =
        row.pats.iterator
          .zip(occs.iterator)
          .foldLeft(
            (
              row.binds.reverse,
              List.empty[Pattern[(PackageName, Constructor), Type]]
            )
          ) { case ((bs, acc), (pat, occ)) =>
            val p1 = normalizePattern(pat)
            val (moreBinds, core) = peelPattern(p1, occ)
            // we prepend in reverse order so this isn't quadratic
            // otherwise prepending the bs accumulator would be quadratic
            (moreBinds.reverse ::: bs, core :: acc)
          }

      row.copy(pats = patsRev.reverse, binds = bindsRev.reverse)
    }

    // Drop columns that are all wildcards to keep the matrix small.
    // Invariant: every row has the same arity as occs, and that arity is
    // preserved in the returned rows/occs.
    def dropWildColumns(
        rows: List[MatchRow],
        occs: List[CheapExpr[B]]
    ): (List[MatchRow], List[CheapExpr[B]]) = {
      val keepIdxs = occs.indices.filter { idx =>
        rows.exists(r => r.pats(idx) != Pattern.WildCard)
      }.toList

      if (keepIdxs.length == occs.length) (rows, occs)
      else {
        val newRows = rows.map { r =>
          val ps = keepIdxs.map(r.pats(_))
          r.copy(pats = ps)
        }
        val newOccs = keepIdxs.map(occs(_))
        (newRows, newOccs)
      }
    }

    // Compute the head signature for a refutable pattern in a column.
    def headSig(
        pat: Pattern[(PackageName, Constructor), Type]
    ): Option[HeadSig] =
      pat match {
        case Pattern.WildCard     => None
        case Pattern.Literal(lit) =>
          Some(LitSig(lit))
        case Pattern.PositionalStruct((pack, cname), params) =>
          variantOf(pack, cname) match {
            case Some(DataRepr.Enum(v, s, f)) =>
              Some(EnumSig((pack, cname), v, s, f))
            case Some(DataRepr.Struct(s)) =>
              Some(StructSig((pack, cname), s))
            case Some(DataRepr.NewType) =>
              Some(StructSig((pack, cname), 1))
            case Some(DataRepr.ZeroNat) =>
              if (params.isEmpty) Some(ZeroSig)
              else {
                // $COVERAGE-OFF$
                throw new IllegalStateException(
                  s"ZeroNat should have 0 params, found: $params"
                )
                // $COVERAGE-ON$
              }
            case Some(DataRepr.SuccNat) =>
              if (params.length == 1) Some(SuccSig)
              else {
                // $COVERAGE-OFF$
                throw new IllegalStateException(
                  s"SuccNat should have 1 param, found: $params"
                )
                // $COVERAGE-ON$
              }
            case None =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"could not find $cname in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Annotation(_, _) | Pattern.Var(_) | Pattern.Named(_, _) |
            Pattern.StrPat(_) | Pattern.ListPat(_) | Pattern.Union(_, _) =>
          // headSig is only called on rows that have been normalized
          // (normalizeRow + peelPattern), had unions flattened
          // (expandRows), and are known to be orthogonal
          // (isNonOrthogonal == false). These cases should be eliminated
          // before headSig, so reaching them is an invariant violation.
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"unexpected pattern in headSig: $pat"
          )
        // $COVERAGE-ON$
      }

    // Stable de-duplication to keep constructor cases in source order.
    def distinctInOrder[A](as: List[A]): List[A] = {
      val seen = scala.collection.mutable.HashSet.empty[A]
      val bldr = scala.collection.mutable.ListBuffer.empty[A]
      as.foreach { a =>
        if (!seen(a)) {
          seen.add(a)
          bldr.append(a)
        }
      }
      bldr.toList
    }

    // Specialize the matrix for a given head signature (constructor/literal),
    // producing the submatrix for that case.
    // Invariant: colIdx is in-bounds and each row has the same arity.
    // returns a list of rows such that result.length <= rows.length
    def specializeRows(
        sig: HeadSig,
        rows: List[MatchRow],
        colIdx: Int,
        arity: Int
    ): List[MatchRow] = {
      lazy val wilds: List[Pattern[(PackageName, Constructor), Type]] =
        List.fill(arity)(Pattern.WildCard)

      rows.mapFilter { row =>
        val pat = row.pats(colIdx)

        pat match {
          case Pattern.WildCard     => Some(row.patchPats(colIdx, wilds))
          case Pattern.Literal(lit) =>
            sig match {
              case LitSig(l) if l === lit =>
                Some(row.patchPats(colIdx, Nil))
              case _ => None
            }
          case Pattern.PositionalStruct((pack, cname), params) =>
            variantOf(pack, cname) match {
              case Some(DataRepr.Enum(v, s, f)) =>
                sig match {
                  case EnumSig(_, v1, s1, f1)
                      if (v == v1) && (s == s1) && (f == f1) =>
                    Some(row.patchPats(colIdx, params))
                  case _ => None
                }
              case Some(DataRepr.Struct(s)) =>
                sig match {
                  case StructSig(_, s1) if s == s1 =>
                    Some(row.patchPats(colIdx, params))
                  case _ => None
                }
              case Some(DataRepr.NewType) =>
                sig match {
                  case StructSig(_, s1) if s1 == 1 =>
                    Some(row.patchPats(colIdx, params))
                  case _ => None
                }
              case Some(DataRepr.ZeroNat) =>
                sig match {
                  case ZeroSig if params.isEmpty =>
                    Some(row.patchPats(colIdx, Nil))
                  case _ => None
                }
              case Some(DataRepr.SuccNat) =>
                sig match {
                  case SuccSig if params.length == 1 =>
                    Some(row.patchPats(colIdx, params))
                  case _ => None
                }
              // $COVERAGE-OFF$
              case None =>
                throw new IllegalStateException(
                  s"could not find $cname in global data types"
                )
              // $COVERAGE-ON$
            }
          // $COVERAGE-OFF$
          case Pattern.Var(_) | Pattern.Named(_, _) | Pattern.Annotation(_, _) |
              Pattern.StrPat(_) | Pattern.ListPat(_) | Pattern.Union(_, _) =>
            // These cases are eliminated before specialization:
            // - normalizeRow peels Var/Named/Annotation into bindings/wildcards.
            // - normalizePattern strips Annotation and converts list/string
            //   patterns where possible.
            // - expandRows flattens Union into separate rows.
            // - isNonOrthogonal excludes list/string search patterns.
            // Hitting this means the matrix invariants were violated.
            throw new IllegalStateException(
              s"unexpected pattern in specializeRows: pat=$pat, sig=$sig, col=$colIdx, arity=$arity"
            )
          // $COVERAGE-ON$
        }
      }
    }

    def guardToBoolExpr(guardExpr: Expr[B]): F[BoolExpr[B]] =
      guardExpr match {
        case cheap: CheapExpr[B] =>
          Monad[F].pure(isTrueExpr(cheap))
        case notCheap =>
          makeAnon.map { tmp =>
            val guardLocal = LocalAnon(tmp)
            LetBool(Left(guardLocal), notCheap, isTrueExpr(guardLocal))
          }
      }

    // Legacy ordered compiler: compile each pattern into a BoolExpr and chain
    // Ifs. This preserves the semantics of non-orthogonal patterns.
    def matchExprOrderedCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[MatchBranch]
    ): F[Expr[B]] = {
      def recur(
          arg: CheapExpr[B],
          branches: NonEmptyList[MatchBranch]
      ): F[Expr[B]] = {
        val head = branches.head
        // Normalize to simplify list/string patterns while preserving
        // ordered semantics for non-orthogonal matches.
        val head1 = head.copy(pattern = normalizePattern(head.pattern))

        def loop(
            cbs: NonEmptyList[
              (List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])
            ]
        ): F[Expr[B]] =
          cbs match {
            case NonEmptyList((b0, cond, binds), others) =>
              val thisBranch = lets(binds, head1.rhs)

              val hasFallback = others.nonEmpty || branches.tail.nonEmpty
              val resF =
                if (hasFallback) {
                  val condF =
                    head1.guard match {
                      case None =>
                        Monad[F].pure(cond)
                      case Some(guard) =>
                        guardToBoolExpr(lets(binds, guard)).map(cond && _)
                    }
                  lazy val fallbackF: F[Expr[B]] =
                    others match {
                      case oh :: ot =>
                        loop(NonEmptyList(oh, ot))
                      case Nil =>
                        recur(arg, NonEmptyList.fromListUnsafe(branches.tail))
                    }
                  (condF, fallbackF).mapN { (cond1, fallback) =>
                    cond1 match {
                      case TrueConst => thisBranch
                      case _         => If(cond1, thisBranch, fallback)
                    }
                  }
                } else {
                  head1.guard match {
                    case None =>
                      // this must be total, but we still need
                      // to evaluate cond since it can have side effects.
                      Monad[F].pure(always(cond, thisBranch))
                    // $COVERAGE-OFF$
                    case Some(_) =>
                      // totality checking rejects a terminal guarded branch
                      // with no fallback.
                      Monad[F].pure(always(cond, thisBranch))
                    // $COVERAGE-ON$
                  }
                }

              resF.map(letMutAll(b0, _))
          }

        val mustMatchPattern = branches.tail.isEmpty && head1.guard.isEmpty
        doesMatch(arg, head1.pattern, mustMatchPattern).flatMap(loop)
      }

      recur(arg, branches)
    }

    // Compile matches with the Maranget pattern-matrix algorithm:
    // 1) Represent branches as a matrix of patterns (rows) over occurrences.
    // 2) Pick a column, branch on its head constructors/literals.
    // 3) Specialize the matrix per case, and recurse; default to wildcards.
    // This yields a shared decision tree and may add local aliases to
    // reuse projections.
    //
    // Efficiency notes: compared to the ordered chain, the matrix compiler
    // shares tests across branches and avoids re-checking the scrutinee, which
    // tends to reduce redundant work for larger matches. This approach is the
    // standard state-of-the-art in ML-family compilers, but it is not
    // globally optimal: finding the minimal decision tree is hard, and the
    // quality depends on the column-selection heuristic (we use leftmost).
    // See: https://compiler.club/compiling-pattern-matching/
    def matchExprMatrixCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[MatchBranch]
    ): F[Expr[B]] = {
      val rows0 = branches.toList.map(MatchRow.fromBranch)

      // Matches have already passed totality checking, so the initial matrix
      // must match. We thread this through recursive submatrices so the last
      // remaining case can skip redundant checks.
      val topMustMatch = true

      // Materialize projection occurrences once per matrix scope so recursive
      // specializations reuse the projected value instead of rebuilding nested
      // GetEnumElement/GetStructElement trees.
      def materializeOccs(
          occs: List[CheapExpr[B]]
      ): F[(List[(LocalAnon, Expr[B])], List[CheapExpr[B]])] = {
        case class State(
            letsRev: List[(LocalAnon, Expr[B])],
            seen: Map[CheapExpr[B], LocalAnon],
            occsRev: List[CheapExpr[B]]
        )

        def shouldMaterialize(occ: CheapExpr[B]): Boolean =
          occ match {
            case _: GetEnumElement[?] | _: GetStructElement[?] => true
            case _                                             => false
          }

        occs
          .foldLeftM(State(Nil, Map.empty, Nil)) { (st, occ) =>
            if (!shouldMaterialize(occ))
              Monad[F].pure(st.copy(occsRev = occ :: st.occsRev))
            else
              st.seen.get(occ) match {
                case Some(existing) =>
                  Monad[F].pure(st.copy(occsRev = existing :: st.occsRev))
                case None =>
                  makeAnon.map { nm =>
                    val local = LocalAnon(nm)
                    State(
                      (local, occ) :: st.letsRev,
                      st.seen.updated(occ, local),
                      local :: st.occsRev
                    )
                  }
              }
          }
          .map { st =>
            (st.letsRev.reverse, st.occsRev.reverse)
          }
      }

      // Compile a pattern matrix into Matchless Expr by building a decision tree.
      // rowsIn: matrix rows (each row has a pattern per column + rhs + binds)
      // occsIn: occurrences for each column (the scrutinee/projection expression)
      // Invariants: every row has the same arity, and
      // rowsIn.forall(r => r.pats.length == occsIn.length).
      // Also, rowsIn order matches source branch order (left-to-right).
      def compileRows(
          rowsIn: List[MatchRow],
          occsIn: List[CheapExpr[B]],
          mustMatch: Boolean
      ): F[Expr[B]] =
        materializeOccs(occsIn).flatMap { case (occLets, occsMemoed) =>
          val norm = rowsIn.map(normalizeRow(_, occsMemoed))
          val expanded = expandRows(norm)
          val (rows, occs) = dropWildColumns(expanded, occsMemoed)

          val compiled: F[Expr[B]] =
            rows match {
              case MatchRow(p0, g0, r0, b0) :: tail
                  if p0.forall(_ == Pattern.WildCard) =>
                val rhsExpr = lets(b0, r0)
                g0 match {
                  case None =>
                    Monad[F].pure(rhsExpr)
                  case Some(guard) =>
                    val guardExpr = lets(b0, guard)
                    guardToBoolExpr(guardExpr).flatMap { guardCond =>
                      if (tail.nonEmpty) {
                        compileRows(tail, occs, mustMatch = false).map { fallback =>
                          If(guardCond, rhsExpr, fallback)
                        }
                      } else {
                        // totality checking rejects this shape; keep defensive
                        // fallback behavior in case malformed TypedExpr reaches here.
                        Monad[F].pure(If(guardCond, rhsExpr, UnitExpr))
                      }
                    }
                }
              case Nil =>
                // this should be impossible in well-typed code
                Monad[F].pure(UnitExpr)
              case _ =>
                // Column selection: preserve left-to-right behavior by defaulting
                // to the leftmost column. (Heuristics can be added later.)
                val colIdx = 0

                val occ = occs(colIdx)

                val sigs = distinctInOrder(
                  rows.mapFilter(r => headSig(r.pats(colIdx)))
                )

                val defaultRows =
                  rows
                    .mapFilter(r =>
                      if r.pats(colIdx) == Pattern.WildCard then
                        Some(r.patchPats(colIdx, Nil))
                      else None
                    )

                val defaultOccs = occs.patch(colIdx, Nil, 1)

                // Build the Matchless condition + specialized submatrix for one case.
                def buildCase(
                    sig: HeadSig,
                    caseMustMatch: Boolean
                ): F[
                  (
                      BoolExpr[B],
                      List[LocalAnonMut],
                      List[MatchRow],
                      List[CheapExpr[B]]
                  )
                ] =
                  sig match {
                    case EnumSig(_, v, s, f) =>
                      val newRows = specializeRows(sig, rows, colIdx, s)
                      val fields =
                        (0 until s).toList.map(i => GetEnumElement(occ, v, i, s))
                      val newOccs = occs.patch(colIdx, fields, 1)
                      val cond =
                        if (caseMustMatch) TrueConst else CheckVariant(occ, v, s, f)
                      Monad[F].pure(
                        (
                          cond,
                          Nil,
                          newRows,
                          newOccs
                        )
                      )
                    case StructSig(_, s) =>
                      val newRows = specializeRows(sig, rows, colIdx, s)
                      val fields =
                        (0 until s).iterator
                          .map(i => GetStructElement(occ, i, s))
                          .toList
                      val newOccs = occs.patch(colIdx, fields, 1)
                      Monad[F].pure((TrueConst, Nil, newRows, newOccs))
                    case LitSig(lit) =>
                      val newRows = specializeRows(sig, rows, colIdx, 0)
                      val newOccs = occs.patch(colIdx, Nil, 1)
                      val cond =
                        if (caseMustMatch) TrueConst else EqualsLit(occ, lit)
                      Monad[F].pure((cond, Nil, newRows, newOccs))
                    case ZeroSig =>
                      val newRows = specializeRows(sig, rows, colIdx, 0)
                      val newOccs = occs.patch(colIdx, Nil, 1)
                      val cond =
                        if (caseMustMatch) TrueConst
                        else EqualsNat(occ, DataRepr.ZeroNat)
                      Monad[F].pure(
                        (cond, Nil, newRows, newOccs)
                      )
                    case SuccSig =>
                      makeAnon.map { nm =>
                        val mut = LocalAnonMut(nm)
                        val setPrev = SetMut(mut, PrevNat(occ))
                        val cond =
                          if (caseMustMatch) setPrev
                          else EqualsNat(occ, DataRepr.SuccNat) && setPrev
                        val newRows = specializeRows(sig, rows, colIdx, 1)
                        val newOccs = occs.patch(colIdx, mut :: Nil, 1)
                        (cond, mut :: Nil, newRows, newOccs)
                      }
                  }

                // Compile cases in order, with a default branch for wildcards.
                def compileCases(
                    sigs: List[HeadSig],
                    mustMatch: Boolean
                ): F[Expr[B]] =
                  sigs match {
                    case Nil =>
                      if (defaultRows.nonEmpty)
                        compileRows(defaultRows, defaultOccs, mustMatch)
                      else Monad[F].pure(UnitExpr)
                    case sig :: rest =>
                      val caseMustMatch =
                        mustMatch &&
                          rest.isEmpty &&
                          defaultRows.isEmpty &&
                          rows.forall(_.guard.isEmpty)
                      buildCase(sig, caseMustMatch).flatMap {
                        case (cond, preLets, newRows, newOccs) =>
                          if (newRows.isEmpty) compileCases(rest, mustMatch)
                          else {
                            val subMustMatch =
                              mustMatch && newRows.forall(_.guard.isEmpty)
                            compileRows(newRows, newOccs, subMustMatch).flatMap {
                              thenExpr =>
                                cond match {
                                  case TrueConst =>
                                    Monad[F].pure(letMutAll(preLets, thenExpr))
                                  case _ =>
                                    val hasElse =
                                      rest.nonEmpty || defaultRows.nonEmpty

                                    val branchF: F[Expr[B]] =
                                      if (!hasElse)
                                        if (mustMatch)
                                          Monad[F].pure(always(cond, thenExpr))
                                        else
                                          Monad[F].pure(
                                            If(cond, thenExpr, UnitExpr)
                                          )
                                      else
                                        compileCases(rest, mustMatch).map {
                                          elseExpr =>
                                            If(cond, thenExpr, elseExpr)
                                        }

                                    branchF.map(letMutAll(preLets, _))
                                }
                            }
                          }
                      }
                  }

                compileCases(sigs, mustMatch)
            }

          compiled.map(letAnons(occLets, _))
        }

      compileRows(rows0, arg :: Nil, topMustMatch)
    }

    // Use the matrix compiler for an orthogonal prefix and fall back to the
    // ordered compiler for a non-orthogonal suffix (string/list search
    // semantics). A small threshold avoids overhead on tiny matches.
    def exprWeight(expr: Expr[B]): Int = {
      def loopExpr(e: Expr[B]): Int =
        e match {
          case Lambda(captures, _, _, body) =>
            1 + captures.iterator.map(loopExpr).sum + loopExpr(body)
          case WhileExpr(cond, effectExpr, _) =>
            1 + loopBool(cond) + loopExpr(effectExpr)
          case App(fn, args) =>
            1 + loopExpr(fn) + args.iterator.map(loopExpr).sum
          case Let(_, value, in) =>
            1 + loopExpr(value) + loopExpr(in)
          case LetMut(_, in) =>
            1 + loopExpr(in)
          case If(cond, thenExpr, elseExpr) =>
            1 + loopBool(cond) + loopExpr(thenExpr) + loopExpr(elseExpr)
          case Always(cond, thenExpr) =>
            1 + loopBool(cond) + loopExpr(thenExpr)
          case PrevNat(of) =>
            1 + loopExpr(of)
          case _: CheapExpr[?] | MakeEnum(_, _, _) | MakeStruct(_) | ZeroNat | SuccNat =>
            1
        }

      def loopBool(b: BoolExpr[B]): Int =
        b match {
          case EqualsLit(expr, _) =>
            1 + loopExpr(expr)
          case EqualsNat(expr, _) =>
            1 + loopExpr(expr)
          case And(left, right) =>
            1 + loopBool(left) + loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            1 + loopExpr(expr)
          case MatchString(arg, _, _, _) =>
            1 + loopExpr(arg)
          case SetMut(_, expr) =>
            1 + loopExpr(expr)
          case LetBool(_, value, in) =>
            1 + loopExpr(value) + loopBool(in)
          case LetMutBool(_, in) =>
            1 + loopBool(in)
          case TrueConst =>
            1
        }

      loopExpr(expr)
    }

    def isTotalFallbackPattern(
        p: Pattern[(PackageName, Constructor), Type]
    ): Boolean =
      maybeSimple(normalizePattern(p)).nonEmpty

    def matrixFallbackMultiplicity(
        branches: NonEmptyList[MatchBranch]
    ): Int = {
      case class TaggedRow(row: MatchRow, isFallback: Boolean)

      def expandTaggedRows(rows: List[TaggedRow]): List[TaggedRow] = {
        def expandPats(
            pats: List[Pattern[(PackageName, Constructor), Type]]
        ): NonEmptyList[List[Pattern[(PackageName, Constructor), Type]]] =
          pats match {
            case Nil       => NonEmptyList(Nil, Nil)
            case p :: tail =>
              for {
                p1 <- Pattern.flatten(p)
                rest <- expandPats(tail)
              } yield p1 :: rest
          }

        rows.flatMap { tr =>
          expandPats(tr.row.pats).toList.map(ps => tr.copy(row = tr.row.copy(pats = ps)))
        }
      }

      def dropWildColumnsTagged(
          rows: List[TaggedRow],
          occs: List[CheapExpr[B]]
      ): (List[TaggedRow], List[CheapExpr[B]]) = {
        val keepIdxs = occs.indices.filter { idx =>
          rows.exists(tr => tr.row.pats(idx) != Pattern.WildCard)
        }.toList

        if (keepIdxs.length == occs.length) (rows, occs)
        else {
          val newRows = rows.map { tr =>
            val ps = keepIdxs.map(tr.row.pats(_))
            tr.copy(row = tr.row.copy(pats = ps))
          }
          val newOccs = keepIdxs.map(occs(_))
          (newRows, newOccs)
        }
      }

      def specializeTaggedRows(
          sig: HeadSig,
          rows: List[TaggedRow],
          colIdx: Int,
          arity: Int
      ): List[TaggedRow] =
        rows.flatMap { tr =>
          specializeRows(sig, tr.row :: Nil, colIdx, arity)
            .map(r => TaggedRow(r, tr.isFallback))
        }

      def arityOfSig(sig: HeadSig): Int =
        sig match {
          case EnumSig(_, _, s, _) => s
          case StructSig(_, s)     => s
          case LitSig(_)           => 0
          case ZeroSig             => 0
          case SuccSig             => 1
        }

      val dummyMut: LocalAnonMut = LocalAnonMut(Long.MinValue)

      def newOccsForSig(
          sig: HeadSig,
          occ: CheapExpr[B],
          occs: List[CheapExpr[B]],
          colIdx: Int
      ): List[CheapExpr[B]] =
        sig match {
          case EnumSig(_, v, s, _) =>
            val fields =
              (0 until s).toList.map(i => GetEnumElement(occ, v, i, s))
            occs.patch(colIdx, fields, 1)
          case StructSig(_, s)     =>
            val fields =
              (0 until s).toList.map(i => GetStructElement(occ, i, s))
            occs.patch(colIdx, fields, 1)
          case LitSig(_) | ZeroSig =>
            occs.patch(colIdx, Nil, 1)
          case SuccSig             =>
            occs.patch(colIdx, dummyMut :: Nil, 1)
        }

      def compileRowsCount(
          rowsIn: List[TaggedRow],
          occsIn: List[CheapExpr[B]],
          mustMatch: Boolean
      ): Int = {
        val norm = rowsIn.map(tr => tr.copy(row = normalizeRow(tr.row, occsIn)))
        val expanded = expandTaggedRows(norm)
        val (rows, occs) = dropWildColumnsTagged(expanded, occsIn)

        rows match {
          case TaggedRow(MatchRow(p0, g0, _, _), isFallback) :: tail
              if p0.forall(_ == Pattern.WildCard) =>
            val thisLeaf = if (isFallback) 1 else 0
            g0 match {
              case None =>
                thisLeaf
              case Some(_) =>
                if (tail.nonEmpty) thisLeaf + compileRowsCount(tail, occs, mustMatch = false)
                else thisLeaf
            }
          case Nil =>
            0
          case _ =>
            val colIdx = 0
            val occ = occs(colIdx)
            val sigs =
              distinctInOrder(rows.mapFilter(tr => headSig(tr.row.pats(colIdx))))
            val defaultRows =
              rows.mapFilter { tr =>
                if (tr.row.pats(colIdx) == Pattern.WildCard)
                  Some(tr.copy(row = tr.row.patchPats(colIdx, Nil)))
                else None
              }
            val defaultOccs = occs.patch(colIdx, Nil, 1)

            def compileCasesCount(
                sigs: List[HeadSig],
                mustMatch: Boolean
            ): Int =
              sigs match {
                case Nil =>
                  if (defaultRows.nonEmpty)
                    compileRowsCount(defaultRows, defaultOccs, mustMatch)
                  else 0
                case sig :: rest =>
                  val arity = arityOfSig(sig)
                  val newRows = specializeTaggedRows(sig, rows, colIdx, arity)
                  if (newRows.isEmpty) compileCasesCount(rest, mustMatch)
                  else {
                    val newOccs = newOccsForSig(sig, occ, occs, colIdx)
                    val subMustMatch = mustMatch && newRows.forall(_.row.guard.isEmpty)
                    val thenCount = compileRowsCount(newRows, newOccs, subMustMatch)
                    val hasElse = rest.nonEmpty || defaultRows.nonEmpty
                    if (hasElse) thenCount + compileCasesCount(rest, mustMatch)
                    else thenCount
                  }
              }

            compileCasesCount(sigs, mustMatch)
        }
      }

      val rows0 = branches.toList.zipWithIndex.map { case (branch, idx) =>
        TaggedRow(MatchRow.fromBranch(branch), idx == branches.length - 1)
      }
      compileRowsCount(rows0, Local(Identifier.Name("bsts_fallback_probe")) :: Nil, mustMatch = true)
    }

    def shouldPreferOrderedTerminalFallback(
        branches: NonEmptyList[MatchBranch]
    ): Boolean = {
      val MinRhsWeight = 20
      val hasTerminalFallback =
        branches.tail.nonEmpty &&
          branches.last.guard.isEmpty &&
          isTotalFallbackPattern(branches.last.pattern)

      hasTerminalFallback &&
      exprWeight(branches.last.rhs) >= MinRhsWeight &&
      matrixFallbackMultiplicity(branches) > 1
    }

    def matchExpr(
        arg: Expr[B],
        tmp: F[Long],
        branches: NonEmptyList[MatchBranch]
    ): F[Expr[B]] = {
      val (orthoPrefix, nonOrthoSuffix) =
        branches.toList.span(branch => !isNonOrthogonal(branch.pattern))
      val maybeNonOrthoSuffix = NonEmptyList.fromList(nonOrthoSuffix)
      // Heuristic: only pay the matrix setup cost if we can prune a few
      // orthogonal cases before falling back.
      val orthoThreshold = 4

      def maybeMatrix(
          arg: CheapExpr[B],
          branches: NonEmptyList[MatchBranch]
      ): F[Expr[B]] =
        if (shouldPreferOrderedTerminalFallback(branches))
          matchExprOrderedCheap(arg, branches)
        else
          matchExprMatrixCheap(arg, branches)

      maybeMemo(arg, tmp) { (arg: CheapExpr[B]) =>
        maybeNonOrthoSuffix match {
          case None =>
            maybeMatrix(arg, branches)
          case Some(suffixNel) if orthoPrefix.length >= orthoThreshold =>
            matchExprOrderedCheap(arg, suffixNel).flatMap { fallbackExpr =>
              val combinedNel = NonEmptyList.ofInitLast(
                orthoPrefix,
                MatchBranch(Pattern.WildCard, None, fallbackExpr)
              )
              matchExpr(arg, tmp, combinedNel)
            }
          case _ =>
            matchExprOrderedCheap(arg, branches)
        }
      }
    }

    loopLetVal(name, te, rec, LambdaState(None, Map.empty)).map(reuseConstructors(_))
  }

  // toy matcher to see the structure
  // Left means match any number of items, like *_
  def matchList[A, B: Monoid](
      items: List[A],
      pattern: List[Either[List[A] => B, A => Option[B]]]
  ): Option[B] =
    pattern match {
      case Nil =>
        if (items.isEmpty) Some(Monoid[B].empty)
        else None
      case Right(fn) :: pt =>
        items match {
          case ih :: it =>
            fn(ih) match {
              case None    => None
              case Some(b) => matchList(it, pt).map(Monoid[B].combine(b, _))
            }
          case Nil => None
        }

      case Left(lstFn) :: Nil =>
        Some(lstFn(items))

      case Left(lstFn) :: (pt @ (Left(_) :: _)) =>
        // it is ambiguous how much to absorb
        // so, just assume lstFn gets nothing
        matchList(items, pt)
          .map(Monoid.combine(lstFn(Nil), _))

      case Left(lstFn) :: (pt @ (Right(_) :: _)) =>
        var revLeft: List[A] = Nil
        var it = items
        var result: Option[B] = None
        // this cannot match an empty list
        while (result.isEmpty && it.nonEmpty) {
          matchList(it, pt) match {
            case Some(rb) =>
              val leftB = lstFn(revLeft.reverse)
              result = Some(Monoid[B].combine(leftB, rb))
            case None =>
              revLeft = it.head :: revLeft
              it = it.tail
          }
        }
        result
      /*
       * The above should be an imperative version
       * of this code. The imperative code
       * is easier to translate into low level
       * instructions
        items
          .to(LazyList)
          .mapWithIndex { (a, idx) => afn(a).map((_, idx)) }
          .collect { case Some(m) => m }
          .flatMap { case (b, idx) =>
            val left = items.take(idx)
            val leftB = Monoid[B].combine(lstFn(left), b)

            val right = items.drop(idx + 1)
            matchList(right, pt)
              .map { br =>
                Monoid[B].combine(leftB, br)
              }
          }
          .headOption
       */
    }

  /** return the expanded product of sums
    */
  def product[A1](
      sum: NonEmptyList[NonEmptyList[A1]]
  )(prod: (A1, A1) => A1): NonEmptyList[A1] =
    sum match {
      case NonEmptyList(h, Nil) =>
        // this (a1 + a2 + a3) case
        h
      case NonEmptyList(h0, h1 :: tail) =>
        val rightProd = product(NonEmptyList(h1, tail))(prod)
        // (a0 + a1 + ...) * rightProd
        // = a0 * rightProd + a1 * rightProd + ...
        for {
          ai <- h0
          r <- rightProd
        } yield prod(ai, r)
    }

}
