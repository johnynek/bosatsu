package dev.bosatsu

import cats.{Monad, Monoid, Order}
import cats.data.{Chain, NonEmptyList, WriterT}
import dev.bosatsu.pattern.StrPart
import dev.bosatsu.rankn.{DataRepr, Type, RefSpace}

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  private val PostLoweringCleanupMaxRounds = 4

  private[bosatsu] enum LocalPass(val cliName: String) derives CanEqual {
    case HoistInvariantLoopLets extends LocalPass("hoist-invariant-loop-lets")
    case ReuseConstructors extends LocalPass("reuse-constructors")
  }
  private[bosatsu] object LocalPass {
    val ordered: List[LocalPass] = values.toList
    val defaultSet: Set[LocalPass] = ordered.toSet

    def fromCliName(name: String): Option[LocalPass] =
      ordered.find(_.cliName == name)
  }

  private[bosatsu] enum Pass(val cliName: String) derives CanEqual {
    case HoistInvariantLoopLets extends Pass("hoist-invariant-loop-lets")
    case ReuseConstructors extends Pass("reuse-constructors")
    case GlobalInlining extends Pass("global-inlining")
  }
  private[bosatsu] object Pass {
    val ordered: List[Pass] = values.toList
    val defaultSet: Set[Pass] = ordered.toSet

    def fromCliName(name: String): Option[Pass] =
      ordered.find(_.cliName == name)
  }

  private[bosatsu] final case class LocalPassOptions(enabled: Set[LocalPass]) {
    def enables(pass: LocalPass): Boolean =
      enabled(pass)

    def enabledPasses: List[LocalPass] =
      LocalPass.ordered.filter(enabled)
  }
  private[bosatsu] object LocalPassOptions {
    val Default: LocalPassOptions =
      LocalPassOptions(LocalPass.defaultSet)
    val None: LocalPassOptions =
      LocalPassOptions(Set.empty)
  }

  private[bosatsu] final case class PassOptions(enabled: Set[Pass]) {
    def enables(pass: Pass): Boolean =
      enabled(pass)

    def enabledPasses: List[Pass] =
      Pass.ordered.filter(enabled)

    def localPassOptions: LocalPassOptions =
      LocalPassOptions(
        enabled.collect {
          case Pass.HoistInvariantLoopLets =>
            LocalPass.HoistInvariantLoopLets
          case Pass.ReuseConstructors      =>
            LocalPass.ReuseConstructors
        }
      )

    def enableGlobalInlining: Boolean =
      enables(Pass.GlobalInlining)
  }
  private[bosatsu] object PassOptions {
    val Default: PassOptions =
      PassOptions(Pass.defaultSet)
  }

  sealed abstract class Expr[+A] derives CanEqual
  object Expr {
    private def exprTag[A](expr: Expr[A]): Int =
      expr match {
        case _: Lambda[?]           => 0
        case _: WhileExpr[?]        => 1
        case _: Global[?]           => 2
        case Local(_)               => 3
        case ClosureSlot(_)         => 4
        case LocalAnon(_)           => 5
        case LocalAnonMut(_)        => 6
        case _: App[?]              => 7
        case _: Let[?]              => 8
        case _: LetMut[?]           => 9
        case _: If[?]               => 10
        case _: SwitchVariant[?]    => 11
        case _: Always[?]           => 12
        case _: GetEnumElement[?]   => 13
        case _: GetStructElement[?] => 14
        case Literal(_)             => 15
        case LitInt64(_)            => 16
        case _: MakeEnum            => 17
        case _: MakeStruct          => 18
        case ZeroNat                => 19
        case SuccNat                => 20
        case _: PrevNat[?]          => 21
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

          case (
                SwitchVariant(onL, famAritiesL, casesL, defaultL),
                SwitchVariant(onR, famAritiesR, casesR, defaultR)
              ) =>
            val c1 = compareExpr(onL, onR)
            if (c1 != 0) c1
            else {
              val c2 = Order[List[Int]].compare(famAritiesL, famAritiesR)
              if (c2 != 0) c2
              else {
                val c3 =
                  Order[NonEmptyList[(Int, Expr[A])]].compare(casesL, casesR)
                if (c3 != 0) c3
                else Order[Option[Expr[A]]].compare(defaultL, defaultR)
              }
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

          case (
                GetStructElement(argL, indexL, sizeL),
                GetStructElement(argR, indexR, sizeR)
              ) =>
            val c1 = compareExpr(argL, argR)
            if (c1 != 0) c1
            else {
              val c2 = java.lang.Integer.compare(indexL, indexR)
              if (c2 != 0) c2
              else java.lang.Integer.compare(sizeL, sizeR)
            }

          case (Literal(left), Literal(right)) =>
            Order[Lit].compare(left, right)

          case (LitInt64(left), LitInt64(right)) =>
            java.lang.Long.compare(left, right)

          case (
                MakeEnum(variantL, arityL, famAritiesL),
                MakeEnum(variantR, arityR, famAritiesR)
              ) =>
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

    def containsWhileExpr[A](expr: Expr[A]): Boolean = {
      def loopExpr(e: Expr[A]): Boolean =
        e match {
          case WhileExpr(_, _, _) =>
            true
          case Lambda(captures, _, _, body) =>
            captures.exists(loopExpr) || loopExpr(body)
          case App(fn, args) =>
            loopExpr(fn) || args.exists(loopExpr)
          case Let(_, value, in) =>
            loopExpr(value) || loopExpr(in)
          case LetMut(_, in) =>
            loopExpr(in)
          case If(cond, thenExpr, elseExpr) =>
            loopBool(cond) || loopExpr(thenExpr) || loopExpr(elseExpr)
          case SwitchVariant(on, _, cases, default) =>
            loopExpr(on) || cases.exists { case (_, branch) =>
              loopExpr(branch)
            } || default.exists(loopExpr)
          case Always(cond, thenExpr) =>
            loopBool(cond) || loopExpr(thenExpr)
          case PrevNat(of) =>
            loopExpr(of)
          case ge: GetEnumElement[?] =>
            loopExpr(ge.arg)
          case gs: GetStructElement[?] =>
            loopExpr(gs.arg)
          case Local(_) | Global(_, _, _) | ClosureSlot(_) | LocalAnon(_) |
              LocalAnonMut(_) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) |
              SuccNat | ZeroNat =>
            false
        }

      def loopBool(b: BoolExpr[A]): Boolean =
        b match {
          case CompareLit(expr, _, _) =>
            loopExpr(expr)
          case CompareInt(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case CompareInt64(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case CompareFloat64(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case EqualsNat(expr, _) =>
            loopExpr(expr)
          case And(left, right) =>
            loopBool(left) || loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            loopExpr(expr)
          case CheckVariantSet(expr, _, _, _) =>
            loopExpr(expr)
          case SetMut(_, value) =>
            loopExpr(value)
          case LetBool(_, value, in) =>
            loopExpr(value) || loopBool(in)
          case LetMutBool(_, in) =>
            loopBool(in)
          case TrueConst =>
            false
        }

      loopExpr(expr)
    }

    def readsMutable[A](expr: Expr[A]): Boolean = {
      def loopExpr(e: Expr[A]): Boolean =
        e match {
          case LocalAnonMut(_) =>
            true
          case Lambda(captures, _, _, body) =>
            captures.exists(loopExpr) || loopExpr(body)
          case WhileExpr(cond, effectExpr, _) =>
            loopBool(cond) || loopExpr(effectExpr)
          case App(fn, args) =>
            loopExpr(fn) || args.exists(loopExpr)
          case Let(_, value, in) =>
            loopExpr(value) || loopExpr(in)
          case LetMut(_, in) =>
            loopExpr(in)
          case If(cond, thenExpr, elseExpr) =>
            loopBool(cond) || loopExpr(thenExpr) || loopExpr(elseExpr)
          case SwitchVariant(on, _, cases, default) =>
            loopExpr(on) || cases.exists { case (_, branch) =>
              loopExpr(branch)
            } || default.exists(loopExpr)
          case Always(cond, thenExpr) =>
            loopBool(cond) || loopExpr(thenExpr)
          case PrevNat(of) =>
            loopExpr(of)
          case ge: GetEnumElement[?] =>
            loopExpr(ge.arg)
          case gs: GetStructElement[?] =>
            loopExpr(gs.arg)
          case Local(_) | Global(_, _, _) | ClosureSlot(_) | LocalAnon(_) |
              Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
              ZeroNat =>
            false
        }

      def loopBool(b: BoolExpr[A]): Boolean =
        b match {
          case CompareLit(expr, _, _) =>
            loopExpr(expr)
          case CompareInt(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case CompareInt64(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case CompareFloat64(left, _, right) =>
            loopExpr(left) || loopExpr(right)
          case EqualsNat(expr, _) =>
            loopExpr(expr)
          case And(left, right) =>
            loopBool(left) || loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            loopExpr(expr)
          case CheckVariantSet(expr, _, _, _) =>
            loopExpr(expr)
          case SetMut(_, value) =>
            loopExpr(value)
          case LetBool(_, value, in) =>
            loopExpr(value) || loopBool(in)
          case LetMutBool(_, in) =>
            loopBool(in)
          case TrueConst =>
            false
        }

      loopExpr(expr)
    }

    def referencesBindable[A](expr: Expr[A], target: Bindable): Boolean = {
      def checkExpr(expr: Expr[A]): Boolean =
        loopExpr(expr)

      @annotation.tailrec
      def loopExprList(todo: List[Expr[A]]): Boolean =
        todo match {
          case head :: tail =>
            if (checkExpr(head)) true
            else loopExprList(tail)
          case Nil          =>
            false
        }

      @annotation.tailrec
      def loopBranches(todo: List[(Int, Expr[A])]): Boolean =
        todo match {
          case (_, branch) :: tail =>
            if (checkExpr(branch)) true
            else loopBranches(tail)
          case Nil                 =>
            false
        }

      @annotation.tailrec
      def loopExpr(expr: Expr[A]): Boolean =
        expr match {
          case Local(name) =>
            name == target
          case Lambda(captures, recName, args, body) =>
            if (loopExprList(captures)) true
            else if (recName.contains(target) || args.exists(_ == target)) false
            else loopExpr(body)
          case WhileExpr(cond, effectExpr, _) =>
            if (BoolExpr.referencesBindable(cond, target)) true
            else loopExpr(effectExpr)
          case App(fn, args) =>
            if (checkExpr(fn)) true
            else loopExprList(args.toList)
          case Let(arg, value, in) =>
            if (checkExpr(value)) true
            else
              arg match {
                case Right(name) if name == target => false
                case _                             => loopExpr(in)
              }
          case LetMut(_, in) =>
            loopExpr(in)
          case If(cond, thenExpr, elseExpr) =>
            if (BoolExpr.referencesBindable(cond, target)) true
            else if (checkExpr(thenExpr)) true
            else loopExpr(elseExpr)
          case SwitchVariant(on, _, cases, default) =>
            if (checkExpr(on)) true
            else if (loopBranches(cases.toList)) true
            else
              default match {
                case Some(defaultExpr) => loopExpr(defaultExpr)
                case None              => false
              }
          case Always(cond, thenExpr) =>
            if (BoolExpr.referencesBindable(cond, target)) true
            else loopExpr(thenExpr)
          case PrevNat(of) =>
            loopExpr(of)
          case ge: GetEnumElement[?] =>
            loopExpr(ge.arg)
          case gs: GetStructElement[?] =>
            loopExpr(gs.arg)
          case ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
              Global(_, _, _) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) |
              MakeStruct(_) | SuccNat | ZeroNat =>
            false
        }

      loopExpr(expr)
    }

    def referencesLocalAnon[A](expr: Expr[A], target: Long): Boolean = {
      def checkExpr(expr: Expr[A]): Boolean =
        loopExpr(expr)

      @annotation.tailrec
      def loopExprList(todo: List[Expr[A]]): Boolean =
        todo match {
          case head :: tail =>
            if (checkExpr(head)) true
            else loopExprList(tail)
          case Nil          =>
            false
        }

      @annotation.tailrec
      def loopBranches(todo: List[(Int, Expr[A])]): Boolean =
        todo match {
          case (_, branch) :: tail =>
            if (checkExpr(branch)) true
            else loopBranches(tail)
          case Nil                 =>
            false
        }

      @annotation.tailrec
      def loopExpr(expr: Expr[A]): Boolean =
        expr match {
          case LocalAnon(id) =>
            id == target
          case Lambda(captures, _, _, body) =>
            if (loopExprList(captures)) true
            else loopExpr(body)
          case WhileExpr(cond, effectExpr, _) =>
            if (BoolExpr.referencesLocalAnon(cond, target)) true
            else loopExpr(effectExpr)
          case App(fn, args) =>
            if (checkExpr(fn)) true
            else loopExprList(args.toList)
          case Let(arg, value, in) =>
            if (checkExpr(value)) true
            else
              arg match {
                case Left(LocalAnon(id)) if id == target => false
                case _                                   => loopExpr(in)
              }
          case LetMut(_, in) =>
            loopExpr(in)
          case If(cond, thenExpr, elseExpr) =>
            if (BoolExpr.referencesLocalAnon(cond, target)) true
            else if (checkExpr(thenExpr)) true
            else loopExpr(elseExpr)
          case SwitchVariant(on, _, cases, default) =>
            if (checkExpr(on)) true
            else if (loopBranches(cases.toList)) true
            else
              default match {
                case Some(defaultExpr) => loopExpr(defaultExpr)
                case None              => false
              }
          case Always(cond, thenExpr) =>
            if (BoolExpr.referencesLocalAnon(cond, target)) true
            else loopExpr(thenExpr)
          case PrevNat(of) =>
            loopExpr(of)
          case ge: GetEnumElement[?] =>
            loopExpr(ge.arg)
          case gs: GetStructElement[?] =>
            loopExpr(gs.arg)
          case Local(_) | ClosureSlot(_) | LocalAnonMut(_) | Global(_, _, _) |
              Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
              ZeroNat =>
            false
        }

      loopExpr(expr)
    }

    def usesBinding[A](expr: Expr[A], arg: Either[LocalAnon, Bindable]): Boolean =
      arg match {
        case Right(name) =>
          referencesBindable(expr, name)
        case Left(LocalAnon(id)) =>
          referencesLocalAnon(expr, id)
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

    def bindAll[A](
        binds: List[(Either[LocalAnon, Bindable], Expr[A])],
        in: Expr[A]
    ): Expr[A] =
      binds.foldRight(in) { case ((arg, value), acc) =>
        Let(arg, value, acc)
      }

    def bindNamed[A](binds: List[(Bindable, Expr[A])], in: Expr[A]): Expr[A] =
      binds.foldRight(in) { case ((arg, value), acc) =>
        Let(arg, value, acc)
      }

    def bindAnons[A](binds: List[(LocalAnon, Expr[A])], in: Expr[A]): Expr[A] =
      binds.foldRight(in) { case ((arg, value), acc) =>
        Let(arg, value, acc)
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
    Identifier.Bindable
      .freshPrefixedSyntheticIterator(prefix, usedNames)
      .take(count)
      .toList

  private type ExprOrBool[A] = Expr[A] | BoolExpr[A]

  private final case class AnonRenameEnv(
      // Original anonymous binder id -> fresh binder id in the rewritten tree.
      anons: Map[Long, Long],
      // Original mutable-anon binder id -> fresh binder id in the rewritten tree.
      muts: Map[Long, Long]
  )

  private val EmptyAnonRenameEnv = AnonRenameEnv(Map.empty, Map.empty)

  private[bosatsu] def allLocalAnonIds[A](expr: Expr[A]): Set[Long] = {
    @annotation.tailrec
    def loop(todo: List[ExprOrBool[A]], acc: Set[Long]): Set[Long] =
      todo match {
        case head :: tail =>
          head match {
            case Lambda(captures, _, _, body) =>
              loop(body :: captures ::: tail, acc)
            case WhileExpr(cond, effectExpr, _) =>
              loop(cond :: effectExpr :: tail, acc)
            case App(fn, args) =>
              loop(fn :: args.toList ::: tail, acc)
            case Let(arg, value, in) =>
              val acc1 = arg match {
                case Left(LocalAnon(id)) => acc + id
                case Right(_)            => acc
              }
              loop(value :: in :: tail, acc1)
            case LetMut(_, in) =>
              loop(in :: tail, acc)
            case If(cond, thenExpr, elseExpr) =>
              loop(cond :: thenExpr :: elseExpr :: tail, acc)
            case SwitchVariant(on, _, cases, default) =>
              loop(on :: default.toList ::: cases.toList.map(_._2) ::: tail, acc)
            case Always(cond, thenExpr) =>
              loop(cond :: thenExpr :: tail, acc)
            case PrevNat(of) =>
              loop(of :: tail, acc)
            case LocalAnon(id) =>
              loop(tail, acc + id)
            case ge: GetEnumElement[?] =>
              loop(ge.arg :: tail, acc)
            case gs: GetStructElement[?] =>
              loop(gs.arg :: tail, acc)
            case CompareLit(arg, _, _) =>
              loop(arg :: tail, acc)
            case CompareInt(left, _, right) =>
              loop(left :: right :: tail, acc)
            case CompareInt64(left, _, right) =>
              loop(left :: right :: tail, acc)
            case CompareFloat64(left, _, right) =>
              loop(left :: right :: tail, acc)
            case EqualsNat(arg, _) =>
              loop(arg :: tail, acc)
            case And(left, right) =>
              loop(left :: right :: tail, acc)
            case CheckVariant(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case CheckVariantSet(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case SetMut(_, value) =>
              loop(value :: tail, acc)
            case LetBool(arg, value, in) =>
              val acc1 = arg match {
                case Left(LocalAnon(id)) => acc + id
                case Right(_)            => acc
              }
              loop(value :: in :: tail, acc1)
            case LetMutBool(_, in) =>
              loop(in :: tail, acc)
            case Local(_) | Global(_, _, _) | ClosureSlot(_) | LocalAnonMut(_) |
                Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | _: SuccNat.type |
                _: ZeroNat.type | _: TrueConst.type =>
              loop(tail, acc)
          }
        case _ =>
          acc
      }

    loop(expr :: Nil, Set.empty)
  }

  private[bosatsu] def allLocalAnonMutIds[A](expr: Expr[A]): Set[Long] = {
    @annotation.tailrec
    def loop(todo: List[ExprOrBool[A]], acc: Set[Long]): Set[Long] =
      todo match {
        case head :: tail =>
          head match {
            case Lambda(captures, _, _, body) =>
              loop(body :: captures ::: tail, acc)
            case WhileExpr(cond, effectExpr, result) =>
              loop(cond :: effectExpr :: tail, acc + result.ident)
            case App(fn, args) =>
              loop(fn :: args.toList ::: tail, acc)
            case Let(_, value, in) =>
              loop(value :: in :: tail, acc)
            case LetMut(name, in) =>
              loop(in :: tail, acc + name.ident)
            case If(cond, thenExpr, elseExpr) =>
              loop(cond :: thenExpr :: elseExpr :: tail, acc)
            case SwitchVariant(on, _, cases, default) =>
              loop(on :: default.toList ::: cases.toList.map(_._2) ::: tail, acc)
            case Always(cond, thenExpr) =>
              loop(cond :: thenExpr :: tail, acc)
            case PrevNat(of) =>
              loop(of :: tail, acc)
            case LocalAnonMut(id) =>
              loop(tail, acc + id)
            case ge: GetEnumElement[?] =>
              loop(ge.arg :: tail, acc)
            case gs: GetStructElement[?] =>
              loop(gs.arg :: tail, acc)
            case CompareLit(arg, _, _) =>
              loop(arg :: tail, acc)
            case CompareInt(left, _, right) =>
              loop(left :: right :: tail, acc)
            case CompareInt64(left, _, right) =>
              loop(left :: right :: tail, acc)
            case CompareFloat64(left, _, right) =>
              loop(left :: right :: tail, acc)
            case EqualsNat(arg, _) =>
              loop(arg :: tail, acc)
            case And(left, right) =>
              loop(left :: right :: tail, acc)
            case CheckVariant(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case CheckVariantSet(arg, _, _, _) =>
              loop(arg :: tail, acc)
            case SetMut(target, value) =>
              loop(value :: tail, acc + target.ident)
            case LetBool(_, value, in) =>
              loop(value :: in :: tail, acc)
            case LetMutBool(name, in) =>
              loop(in :: tail, acc + name.ident)
            case Local(_) | Global(_, _, _) | ClosureSlot(_) | LocalAnon(_) |
                Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | _: SuccNat.type |
                _: ZeroNat.type | _: TrueConst.type =>
              loop(tail, acc)
          }
        case _ =>
          acc
      }

    loop(expr :: Nil, Set.empty)
  }

  private def maxLocalAnonId[A](expr: Expr[A]): Long =
    allLocalAnonIds(expr).foldLeft(-1L)(_ max _)

  private def maxLocalAnonMutId[A](expr: Expr[A]): Long =
    allLocalAnonMutIds(expr).foldLeft(-1L)(_ max _)

  private[bosatsu] def refreshAnonBinders[A](expr: Expr[A]): Expr[A] = {
    case class RenameState(
        nextId: Long
    ) {
      def freshAnon: (LocalAnon, RenameState) = {
        val next = LocalAnon(nextId)
        (next, copy(nextId = nextId + 1L))
      }

      def freshMut: (LocalAnonMut, RenameState) = {
        val next = LocalAnonMut(nextId)
        (next, copy(nextId = nextId + 1L))
      }
    }

    def loopExpr(
        ex: Expr[A],
        env: AnonRenameEnv,
        st: RenameState
    ): (Expr[A], RenameState) =
      ex match {
        case LocalAnon(id) =>
          (env.anons.get(id).fold(ex: Expr[A])(LocalAnon(_)), st)
        case LocalAnonMut(id) =>
          (env.muts.get(id).fold(ex: Expr[A])(LocalAnonMut(_)), st)
        case Lambda(captures, recName, args, body) =>
          val (captures1Rev, st1) =
            captures.foldLeft((List.empty[Expr[A]], st)) { case ((acc, stN), capture) =>
              val (capture1, stN1) = loopExpr(capture, env, stN)
              (capture1 :: acc, stN1)
            }
          val (body1, st2) = loopExpr(body, env, st1)
          (Lambda(captures1Rev.reverse, recName, args, body1), st2)
        case WhileExpr(cond, effectExpr, result) =>
          val result1 =
            env.muts.get(result.ident).fold(result)(LocalAnonMut(_))
          val (cond1, st1) = loopBool(cond, env, st)
          val (effectExpr1, st2) = loopExpr(effectExpr, env, st1)
          (WhileExpr(cond1, effectExpr1, result1), st2)
        case App(fn, args) =>
          val (fn1, st1) = loopExpr(fn, env, st)
          val (args1Rev, st2) =
            args.toList.foldLeft((List.empty[Expr[A]], st1)) { case ((acc, stN), arg) =>
              val (arg1, stN1) = loopExpr(arg, env, stN)
              (arg1 :: acc, stN1)
            }
          (App(fn1, NonEmptyList.fromListUnsafe(args1Rev.reverse)), st2)
        case Let(arg, value, in) =>
          val (value1, st1) = loopExpr(value, env, st)
          arg match {
            case Right(_) =>
              val (in1, st2) = loopExpr(in, env, st1)
              (Let(arg, value1, in1), st2)
            case Left(anon) =>
              val (anon1, st2) = st1.freshAnon
              val env1 = env.copy(anons = env.anons.updated(anon.ident, anon1.ident))
              val (in1, st3) = loopExpr(in, env1, st2)
              (Let(Left(anon1), value1, in1), st3)
          }
        case LetMut(name, in) =>
          val (name1, st1) = st.freshMut
          val env1 = env.copy(muts = env.muts.updated(name.ident, name1.ident))
          val (in1, st2) = loopExpr(in, env1, st1)
          (LetMut(name1, in1), st2)
        case If(cond, thenExpr, elseExpr) =>
          val (cond1, st1) = loopBool(cond, env, st)
          val (then1, st2) = loopExpr(thenExpr, env, st1)
          val (else1, st3) = loopExpr(elseExpr, env, st2)
          (If(cond1, then1, else1), st3)
        case SwitchVariant(on, famArities, cases, default) =>
          val (on1, st1) = loopCheap(on, env, st)
          val (cases1Rev, st2) =
            cases.toList.foldLeft((List.empty[(Int, Expr[A])], st1)) {
              case ((acc, stN), (variant, branch)) =>
                val (branch1, stN1) = loopExpr(branch, env, stN)
                ((variant, branch1) :: acc, stN1)
            }
          val (default1, st3) =
            default match {
              case Some(defaultExpr) =>
                val (defaultExpr1, stN) = loopExpr(defaultExpr, env, st2)
                (Some(defaultExpr1), stN)
              case None =>
                (None, st2)
            }
          (
            SwitchVariant(
              on1,
              famArities,
              NonEmptyList.fromListUnsafe(cases1Rev.reverse),
              default1
            ),
            st3
          )
        case Always(cond, thenExpr) =>
          val (cond1, st1) = loopBool(cond, env, st)
          val (then1, st2) = loopExpr(thenExpr, env, st1)
          (Always(cond1, then1), st2)
        case PrevNat(of) =>
          val (of1, st1) = loopExpr(of, env, st)
          (PrevNat(of1), st1)
        case ge: GetEnumElement[?] =>
          val (arg1, st1) = loopCheap(ge.arg, env, st)
          (ge.copy(arg = arg1), st1)
        case gs: GetStructElement[?] =>
          val (arg1, st1) = loopCheap(gs.arg, env, st)
          (gs.copy(arg = arg1), st1)
        case Global(_, _, _) | Local(_) | ClosureSlot(_) | Literal(_) |
            LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat =>
          (ex, st)
      }

    def loopCheap(
        ex: CheapExpr[A],
        env: AnonRenameEnv,
        st: RenameState
    ): (CheapExpr[A], RenameState) =
      loopExpr(ex, env, st) match {
        case (cheap: CheapExpr[A], st1) => (cheap, st1)
        case (notCheap, _) =>
          // $COVERAGE-OFF$
          sys.error(
            s"invariant violation: expected cheap expression while refreshing anon binders, got: $notCheap"
          )
        // $COVERAGE-ON$
      }

    def loopBool(
        ex: BoolExpr[A],
        env: AnonRenameEnv,
        st: RenameState
    ): (BoolExpr[A], RenameState) =
      ex match {
        case CompareLit(arg, rel, lit) =>
          val (arg1, st1) = loopCheap(arg, env, st)
          (CompareLit(arg1, rel, lit), st1)
        case CompareInt(left, rel, right) =>
          val (left1, st1) = loopCheap(left, env, st)
          val (right1, st2) = loopCheap(right, env, st1)
          (CompareInt(left1, rel, right1), st2)
        case CompareInt64(left, rel, right) =>
          val (left1, st1) = loopCheap(left, env, st)
          val (right1, st2) = loopCheap(right, env, st1)
          (CompareInt64(left1, rel, right1), st2)
        case CompareFloat64(left, rel, right) =>
          val (left1, st1) = loopCheap(left, env, st)
          val (right1, st2) = loopCheap(right, env, st1)
          (CompareFloat64(left1, rel, right1), st2)
        case EqualsNat(arg, nat) =>
          val (arg1, st1) = loopCheap(arg, env, st)
          (EqualsNat(arg1, nat), st1)
        case And(left, right) =>
          val (left1, st1) = loopBool(left, env, st)
          val (right1, st2) = loopBool(right, env, st1)
          (And(left1, right1), st2)
        case CheckVariant(arg, expect, size, famArities) =>
          val (arg1, st1) = loopCheap(arg, env, st)
          (CheckVariant(arg1, expect, size, famArities), st1)
        case CheckVariantSet(arg, expect, size, famArities) =>
          val (arg1, st1) = loopCheap(arg, env, st)
          (CheckVariantSet(arg1, expect, size, famArities), st1)
        case SetMut(target, value) =>
          val target1 =
            env.muts.get(target.ident).fold(target)(LocalAnonMut(_))
          val (value1, st1) = loopExpr(value, env, st)
          (SetMut(target1, value1), st1)
        case LetBool(arg, value, in) =>
          val (value1, st1) = loopExpr(value, env, st)
          arg match {
            case Right(_) =>
              val (in1, st2) = loopBool(in, env, st1)
              (LetBool(arg, value1, in1), st2)
            case Left(anon) =>
              val (anon1, st2) = st1.freshAnon
              val env1 = env.copy(anons = env.anons.updated(anon.ident, anon1.ident))
              val (in1, st3) = loopBool(in, env1, st2)
              (LetBool(Left(anon1), value1, in1), st3)
          }
        case LetMutBool(name, in) =>
          val (name1, st1) = st.freshMut
          val env1 = env.copy(muts = env.muts.updated(name.ident, name1.ident))
          val (in1, st2) = loopBool(in, env1, st1)
          (LetMutBool(name1, in1), st2)
        case TrueConst =>
          (TrueConst, st)
      }

    val initialState =
      RenameState(
        nextId = (maxLocalAnonId(expr) max maxLocalAnonMutId(expr)) + 1L
      )

    loopExpr(expr, EmptyAnonRenameEnv, initialState)._1
  }

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
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            loopCheap(on),
            famArities,
            cases.map { case (variant, branch) =>
              (variant, loopExpr(branch))
            },
            default.map(loopExpr)
          )
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
            Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
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
        case CompareLit(arg, rel, lit) =>
          CompareLit(loopCheap(arg), rel, lit)
        case CompareInt(left, rel, right) =>
          CompareInt(loopCheap(left), rel, loopCheap(right))
        case CompareInt64(left, rel, right) =>
          CompareInt64(loopCheap(left), rel, loopCheap(right))
        case CompareFloat64(left, rel, right) =>
          CompareFloat64(loopCheap(left), rel, loopCheap(right))
        case EqualsNat(arg, nat) =>
          EqualsNat(loopCheap(arg), nat)
        case And(left, right) =>
          And(loopBool(left), loopBool(right))
        case CheckVariant(arg, expect, size, famArities) =>
          CheckVariant(loopCheap(arg), expect, size, famArities)
        case CheckVariantSet(arg, expect, size, famArities) =>
          CheckVariantSet(loopCheap(arg), expect, size, famArities)
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

  private def resolveAlias[A](
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

  private def substituteBindables[A](
      expr: Expr[A],
      subst: Map[Bindable, Expr[A]]
  ): Expr[A] =
    if (subst.isEmpty) expr
    else {
      def loopExpr(ex: Expr[A], env: Map[Bindable, Expr[A]]): Expr[A] = {
        ex match {
          case Local(name) =>
            env.getOrElse(name, ex)
          case Lambda(captures, recName, args, body) =>
            val bodyEnv = env -- recName.toSet -- args.toList
            if (bodyEnv.isEmpty) ex
            else
              Lambda(
                captures.map(loopExpr(_, env)),
                recName,
                args,
                loopExpr(body, bodyEnv)
              )
          case WhileExpr(cond, effectExpr, result) =>
            WhileExpr(loopBool(cond, env), loopExpr(effectExpr, env), result)
          case App(fn, appArgs) =>
            App(loopExpr(fn, env), appArgs.map(loopExpr(_, env)))
          case Let(arg, value, in) =>
            val value1 = loopExpr(value, env)
            val env1 =
              arg match {
                case Right(name) => env - name
                case Left(_)     => env
              }
            val in1 =
              if (env1.isEmpty) in
              else loopExpr(in, env1)
            Let(arg, value1, in1)
          case LetMut(name, in) =>
            LetMut(name, loopExpr(in, env))
          case If(cond, thenExpr, elseExpr) =>
            If(
              loopBool(cond, env),
              loopExpr(thenExpr, env),
              loopExpr(elseExpr, env)
            )
          case SwitchVariant(on, famArities, cases, default) =>
            SwitchVariant(
              loopCheap(on, env),
              famArities,
              cases.map { case (variant, branch) =>
                (variant, loopExpr(branch, env))
              },
              default.map(loopExpr(_, env))
            )
          case Always(cond, thenExpr) =>
            Always(loopBool(cond, env), loopExpr(thenExpr, env))
          case PrevNat(of) =>
            PrevNat(loopExpr(of, env))
          case ge: GetEnumElement[?] =>
            ge.copy(arg = loopCheap(ge.arg, env))
          case gs: GetStructElement[?] =>
            gs.copy(arg = loopCheap(gs.arg, env))
          case Global(_, _, _) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
              Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
              ZeroNat =>
            ex
        }
      }

      def loopCheap(ex: CheapExpr[A], env: Map[Bindable, Expr[A]]): CheapExpr[A] =
        loopExpr(ex, env) match {
          case ch: CheapExpr[A] => ch
          case notCheap         =>
            // $COVERAGE-OFF$
            sys.error(
              s"invariant violation: expected cheap expression during substitution, got: $notCheap"
            )
          // $COVERAGE-ON$
        }

      def loopBool(ex: BoolExpr[A], env: Map[Bindable, Expr[A]]): BoolExpr[A] = {
        ex match {
          case CompareLit(arg, rel, lit) =>
            CompareLit(loopCheap(arg, env), rel, lit)
          case CompareInt(left, rel, right) =>
            CompareInt(loopCheap(left, env), rel, loopCheap(right, env))
          case CompareInt64(left, rel, right) =>
            CompareInt64(loopCheap(left, env), rel, loopCheap(right, env))
          case CompareFloat64(left, rel, right) =>
            CompareFloat64(loopCheap(left, env), rel, loopCheap(right, env))
          case EqualsNat(arg, nat) =>
            EqualsNat(loopCheap(arg, env), nat)
          case And(left, right) =>
            And(loopBool(left, env), loopBool(right, env))
          case CheckVariant(arg, expect, size, famArities) =>
            CheckVariant(loopCheap(arg, env), expect, size, famArities)
          case CheckVariantSet(arg, expect, size, famArities) =>
            CheckVariantSet(loopCheap(arg, env), expect, size, famArities)
          case SetMut(target, value) =>
            SetMut(target, loopExpr(value, env))
          case LetBool(arg, value, in) =>
            val value1 = loopExpr(value, env)
            val env1 =
              arg match {
                case Right(name) => env - name
                case Left(_)     => env
              }
            val in1 =
              if (env1.isEmpty) in
              else loopBool(in, env1)
            LetBool(arg, value1, in1)
          case LetMutBool(name, in) =>
            LetMutBool(name, loopBool(in, env))
          case TrueConst =>
            TrueConst
        }
      }

      loopExpr(expr, subst)
    }

  private[bosatsu] def exprWeight[A](expr: Expr[A]): Int = {
    def loopExpr(e: Expr[A]): Int =
      e match {
        case Lambda(captures, _, _, body) =>
          1 + captures.iterator.map(loopExpr).sum + loopExpr(body)
        case WhileExpr(cond, effectExpr, _) =>
          2 + loopBool(cond) + loopExpr(effectExpr)
        case App(fn, args) =>
          2 + loopExpr(fn) + args.iterator.map(loopExpr).sum
        case Let(_, value, in) =>
          1 + loopExpr(value) + loopExpr(in)
        case LetMut(_, in) =>
          1 + loopExpr(in)
        case If(cond, thenExpr, elseExpr) =>
          1 + loopBool(cond) + loopExpr(thenExpr) + loopExpr(elseExpr)
        case SwitchVariant(on, _, cases, default) =>
          1 + loopExpr(on) + cases.iterator
            .map { case (_, branch) =>
              loopExpr(branch)
            }
            .sum + default.fold(0)(loopExpr)
        case Always(cond, thenExpr) =>
          1 + loopBool(cond) + loopExpr(thenExpr)
        case PrevNat(of) =>
          1 + loopExpr(of)
        case _: CheapExpr[?] | MakeEnum(_, _, _) | MakeStruct(_) | ZeroNat |
            SuccNat =>
          1
      }

    def loopBool(b: BoolExpr[A]): Int =
      b match {
        case CompareLit(expr, _, _) =>
          1 + loopExpr(expr)
        case CompareInt(left, _, right) =>
          1 + loopExpr(left) + loopExpr(right)
        case CompareInt64(left, _, right) =>
          1 + loopExpr(left) + loopExpr(right)
        case CompareFloat64(left, _, right) =>
          1 + loopExpr(left) + loopExpr(right)
        case EqualsNat(expr, _) =>
          1 + loopExpr(expr)
        case And(left, right) =>
          1 + loopBool(left) + loopBool(right)
        case CheckVariant(expr, _, _, _) =>
          1 + loopExpr(expr)
        case CheckVariantSet(expr, _, _, _) =>
          1 + loopExpr(expr)
        case SetMut(_, value) =>
          1 + loopExpr(value)
        case LetBool(_, value, in) =>
          1 + loopExpr(value) + loopBool(in)
        case LetMutBool(_, in) =>
          1 + loopBool(in)
        case TrueConst =>
          1
      }

    loopExpr(expr)
  }

  private[bosatsu] def isTriviallyCheap[A](value: Expr[A]): Boolean =
    value match {
      case _: CheapExpr[?] =>
        true
      case cons: ConsExpr =>
        cons.arity == 0
      case _ =>
        false
    }

  /** Apply args to an expression while pushing through branch structure and
    * reducing immediate lambda application into lets.
    */
  def applyArgs[A](fn: Expr[A], args: NonEmptyList[Expr[A]]): Expr[A] = {
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
        Let.bindNamed(
          lamArgs.toList.zip(argTmpNames.map(name => Local(name): Expr[A])),
          bodyWithCaptures
        )

      val bodyWithRec =
        recName match {
          case Some(name) =>
            // We still bind the recursive lambda once at the call site; the win
            // is that the actual arguments can now flow through surrounding
            // control flow instead of staying behind an outer App node.
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
        Let.bindNamed(argTmpNames.zip(args.toList), bodyWithRec)

      Let.bindNamed(captureTmpNames.zip(captures), withArgTmps)
    }

    def loop(ex: Expr[A], aliases: Map[Bindable, Lambda[A]]): Expr[A] =
      resolveAlias(ex, aliases, Set.empty) match {
        case lam: Lambda[A] if lam.arity == args.length =>
          betaReduce(lam)
        case If(cond, thenExpr, elseExpr) =>
          If(cond, loop(thenExpr, aliases), loop(elseExpr, aliases))
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            on,
            famArities,
            cases.map { case (variant, branch) =>
              (variant, loop(branch, aliases))
            },
            default.map(loop(_, aliases))
          )
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

  /** Apply args for inlining without introducing eager outer lets for the
    * actual arguments.
    */
  def inlineApplyArgs[A](fn: Expr[A], args: NonEmptyList[Expr[A]]): Expr[A] = {
    val argNames = allNamesMany(args.iterator)
    val argAnonIds = args.foldMap(allLocalAnonIds)
    val argMutIds = args.foldMap(allLocalAnonMutIds)

    case class RenameEnv(
        bindables: Map[Bindable, Bindable],
        ids: AnonRenameEnv
    )
    val emptyRenameEnv = RenameEnv(Map.empty, EmptyAnonRenameEnv)

    case class RenameState(
        usedNames: Set[Bindable],
        nextId: Long
    ) {
      def freshName(prefix: String): (Bindable, RenameState) = {
        val next =
          Identifier.Bindable
            .freshPrefixedSyntheticIterator(prefix, usedNames)
            .next()
        (next, copy(usedNames = usedNames + next))
      }

      def freshAnon: (LocalAnon, RenameState) = {
        val next = LocalAnon(nextId)
        (next, copy(nextId = nextId + 1L))
      }

      def freshMut: (LocalAnonMut, RenameState) = {
        val next = LocalAnonMut(nextId)
        (next, copy(nextId = nextId + 1L))
      }
    }

    // This is the Matchless equivalent of an unshadow pass: once we copy the
    // callee body into the caller, every binder that crosses the inline
    // boundary must be fresh before we substitute the actual arguments.
    def renameBinders(lam0: Lambda[A]): Lambda[A] = {
      def loopExpr(
          ex: Expr[A],
          env: RenameEnv,
          st: RenameState
      ): (Expr[A], RenameState) =
        ex match {
          case Local(name) =>
            (env.bindables.get(name).fold(ex: Expr[A])(Local(_)), st)
          case LocalAnon(id) =>
            (env.ids.anons.get(id).fold(ex: Expr[A])(LocalAnon(_)), st)
          case LocalAnonMut(id) =>
            (env.ids.muts.get(id).fold(ex: Expr[A])(LocalAnonMut(_)), st)
          case Lambda(captures, recName, lamArgs, body) =>
            val (captures1Rev, st1) =
              captures.foldLeft((List.empty[Expr[A]], st)) { case ((acc, stN), cap) =>
                val (cap1, stN1) = loopExpr(cap, env, stN)
                (cap1 :: acc, stN1)
              }
            val (recName1, st2, env1) =
              recName match {
                case Some(name) =>
                  val (name1, stN) = st1.freshName("bsts_inline_rec")
                  (Some(name1), stN, env.copy(bindables = env.bindables.updated(name, name1)))
                case None =>
                  (None, st1, env)
              }
            val (argsRev, st3, env2) =
              lamArgs.toList.foldLeft((List.empty[Bindable], st2, env1)) {
                case ((acc, stN, envN), arg) =>
                  val (arg1, stN1) = stN.freshName("bsts_inline_arg")
                  (
                    arg1 :: acc,
                    stN1,
                    envN.copy(bindables = envN.bindables.updated(arg, arg1))
                  )
              }
            val (body1, st4) = loopExpr(body, env2, st3)
            (
              Lambda(
                captures1Rev.reverse,
                recName1,
                NonEmptyList.fromListUnsafe(argsRev.reverse),
                body1
              ),
              st4
            )
          case WhileExpr(cond, effectExpr, result) =>
            val result1 =
              env.ids.muts.get(result.ident).fold(result)(LocalAnonMut(_))
            val (cond1, st1) = loopBool(cond, env, st)
            val (effect1, st2) = loopExpr(effectExpr, env, st1)
            (WhileExpr(cond1, effect1, result1), st2)
          case App(appFn, appArgs) =>
            val (fn1, st1) = loopExpr(appFn, env, st)
            val (args1Rev, st2) =
              appArgs.toList.foldLeft((List.empty[Expr[A]], st1)) {
                case ((acc, stN), arg) =>
                  val (arg1, stN1) = loopExpr(arg, env, stN)
                  (arg1 :: acc, stN1)
              }
            (App(fn1, NonEmptyList.fromListUnsafe(args1Rev.reverse)), st2)
          case Let(arg, value, in) =>
            val (value1, st1) = loopExpr(value, env, st)
            arg match {
              case Right(name) =>
                val (name1, st2) = st1.freshName("bsts_inline_let")
                val env1 = env.copy(bindables = env.bindables.updated(name, name1))
                val (in1, st3) = loopExpr(in, env1, st2)
                (Let(name1, value1, in1), st3)
              case Left(anon) =>
                val (anon1, st2) = st1.freshAnon
                val env1 =
                  env.copy(ids = env.ids.copy(anons = env.ids.anons.updated(anon.ident, anon1.ident)))
                val (in1, st3) = loopExpr(in, env1, st2)
                (Let(anon1, value1, in1), st3)
            }
          case LetMut(name, in) =>
            val (name1, st1) = st.freshMut
            val env1 =
              env.copy(ids = env.ids.copy(muts = env.ids.muts.updated(name.ident, name1.ident)))
            val (in1, st2) = loopExpr(in, env1, st1)
            (LetMut(name1, in1), st2)
          case If(cond, thenExpr, elseExpr) =>
            val (cond1, st1) = loopBool(cond, env, st)
            val (then1, st2) = loopExpr(thenExpr, env, st1)
            val (else1, st3) = loopExpr(elseExpr, env, st2)
            (If(cond1, then1, else1), st3)
          case SwitchVariant(on, famArities, cases, default) =>
            val (on1, st1) = loopCheap(on, env, st)
            val (cases1Rev, st2) =
              cases.toList.foldLeft((List.empty[(Int, Expr[A])], st1)) {
                case ((acc, stN), (variant, branch)) =>
                  val (branch1, stN1) = loopExpr(branch, env, stN)
                  ((variant, branch1) :: acc, stN1)
              }
            val (default1, st3) =
              default match {
                case Some(defaultExpr) =>
                  val (defaultExpr1, stN) = loopExpr(defaultExpr, env, st2)
                  (Some(defaultExpr1), stN)
                case None =>
                  (None, st2)
              }
            (
              SwitchVariant(
                on1,
                famArities,
                NonEmptyList.fromListUnsafe(cases1Rev.reverse),
                default1
              ),
              st3
            )
          case Always(cond, thenExpr) =>
            val (cond1, st1) = loopBool(cond, env, st)
            val (then1, st2) = loopExpr(thenExpr, env, st1)
            (Always(cond1, then1), st2)
          case PrevNat(of) =>
            val (of1, st1) = loopExpr(of, env, st)
            (PrevNat(of1), st1)
          case ge: GetEnumElement[?] =>
            val (arg1, st1) = loopCheap(ge.arg, env, st)
            (ge.copy(arg = arg1), st1)
          case gs: GetStructElement[?] =>
            val (arg1, st1) = loopCheap(gs.arg, env, st)
            (gs.copy(arg = arg1), st1)
          case Global(_, _, _) | ClosureSlot(_) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) |
              MakeStruct(_) | SuccNat | ZeroNat =>
            (ex, st)
        }

      def loopCheap(
          ex: CheapExpr[A],
          env: RenameEnv,
          st: RenameState
      ): (CheapExpr[A], RenameState) =
        loopExpr(ex, env, st) match {
          case (ch: CheapExpr[A], st1) => (ch, st1)
          case (notCheap, _)           =>
            // $COVERAGE-OFF$
            sys.error(
              s"invariant violation: expected cheap expression during inlining alpha-rename, got: $notCheap"
            )
          // $COVERAGE-ON$
        }

      def loopBool(
          ex: BoolExpr[A],
          env: RenameEnv,
          st: RenameState
      ): (BoolExpr[A], RenameState) =
        ex match {
          case CompareLit(arg, rel, lit) =>
            val (arg1, st1) = loopCheap(arg, env, st)
            (CompareLit(arg1, rel, lit), st1)
          case CompareInt(left, rel, right) =>
            val (left1, st1) = loopCheap(left, env, st)
            val (right1, st2) = loopCheap(right, env, st1)
            (CompareInt(left1, rel, right1), st2)
          case CompareInt64(left, rel, right) =>
            val (left1, st1) = loopCheap(left, env, st)
            val (right1, st2) = loopCheap(right, env, st1)
            (CompareInt64(left1, rel, right1), st2)
          case CompareFloat64(left, rel, right) =>
            val (left1, st1) = loopCheap(left, env, st)
            val (right1, st2) = loopCheap(right, env, st1)
            (CompareFloat64(left1, rel, right1), st2)
          case EqualsNat(arg, nat) =>
            val (arg1, st1) = loopCheap(arg, env, st)
            (EqualsNat(arg1, nat), st1)
          case And(left, right) =>
            val (left1, st1) = loopBool(left, env, st)
            val (right1, st2) = loopBool(right, env, st1)
            (And(left1, right1), st2)
          case CheckVariant(arg, expect, size, famArities) =>
            val (arg1, st1) = loopCheap(arg, env, st)
            (CheckVariant(arg1, expect, size, famArities), st1)
          case CheckVariantSet(arg, expect, size, famArities) =>
            val (arg1, st1) = loopCheap(arg, env, st)
            (CheckVariantSet(arg1, expect, size, famArities), st1)
          case SetMut(target, value) =>
            val target1 =
              env.ids.muts.get(target.ident).fold(target)(LocalAnonMut(_))
            val (value1, st1) = loopExpr(value, env, st)
            (SetMut(target1, value1), st1)
          case LetBool(arg, value, in) =>
            val (value1, st1) = loopExpr(value, env, st)
            arg match {
              case Right(name) =>
                val (name1, st2) = st1.freshName("bsts_inline_bool")
                val env1 = env.copy(bindables = env.bindables.updated(name, name1))
                val (in1, st3) = loopBool(in, env1, st2)
                (LetBool(Right(name1), value1, in1), st3)
              case Left(anon) =>
                val (anon1, st2) = st1.freshAnon
                val env1 =
                  env.copy(ids = env.ids.copy(anons = env.ids.anons.updated(anon.ident, anon1.ident)))
                val (in1, st3) = loopBool(in, env1, st2)
                (LetBool(Left(anon1), value1, in1), st3)
            }
          case LetMutBool(name, in) =>
            val (name1, st1) = st.freshMut
            val env1 =
              env.copy(ids = env.ids.copy(muts = env.ids.muts.updated(name.ident, name1.ident)))
            val (in1, st2) = loopBool(in, env1, st1)
            (LetMutBool(name1, in1), st2)
          case TrueConst =>
            (TrueConst, st)
        }

      val usedNames = allNames(lam0) | argNames
      val nextId =
        List(
          maxLocalAnonId(lam0),
          maxLocalAnonMutId(lam0),
          argAnonIds.foldLeft(-1L)(_ max _),
          argMutIds.foldLeft(-1L)(_ max _)
        ).max + 1L
      loopExpr(
        lam0,
        emptyRenameEnv,
        RenameState(usedNames, nextId)
      ) match {
        case (lam1: Lambda[A], _) => lam1
        case (other, _)           =>
          // $COVERAGE-OFF$
          sys.error(s"expected lambda after alpha-rename, got: $other")
        // $COVERAGE-ON$
      }
    }

    def betaInline(lam0: Lambda[A]): Expr[A] = {
      val Lambda(captures, recName, lamArgs, body) = renameBinders(lam0)
      val paramDemand = parameterDemandSummary(
        Lambda(captures, recName, lamArgs, body)
      )
      val baseUsedNames =
        allNames(body) |
          allNamesMany(captures.iterator ++ args.iterator) |
          recName.toSet ++
          lamArgs.iterator
      val captureTmpNames =
        freshSyntheticNames(
          prefix = "bsts_inline_capture",
          count = captures.length,
          usedNames = baseUsedNames
        )
      val captureTmpLocals: List[CheapExpr[A]] =
        captureTmpNames.map(Local(_))

      val bodyWithCaptures =
        substituteClosureSlots(captureTmpLocals.toVector, body)

      val argMemos = lamArgs.toList
        .zip(args.toList)
        .zip(paramDemand)
        .collect {
          case ((argName, argExpr), demand)
              if !demand.unused &&
                !argExpr.isInstanceOf[CheapExpr[?]] &&
                (
                  // Any eager use means the original call would have evaluated the
                  // argument before entering the helper, so binding it once at the
                  // call site preserves that behavior while avoiding duplication.
                  ((demand.eagerUses > 0) &&
                    ((demand.totalUses > 1) || (demand.cheapPositionUses > 0))) ||
                  // CheapExpr positions such as EqualsNat/GetStructElement cannot
                  // directly hold arbitrary constructor applications. Memoizing
                  // any non-cheap selector input preserves eager call semantics
                  // while giving substitution a cheap local to reference.
                  (demand.cheapPositionUses > 0)
                ) =>
            (argName, argExpr)
        }
      val argMemoNames =
        freshSyntheticNames(
          prefix = "bsts_inline_arg",
          count = argMemos.length,
          usedNames = baseUsedNames ++ captureTmpNames
        )
      val argMemoSubst =
        argMemos.iterator
          .map(_._1)
          .zip(argMemoNames.iterator.map(Local(_): Expr[A]))
          .toMap
      val directArgSubst =
        lamArgs.toList
          .zip(args.toList)
          .collect {
            case (argName, argExpr) if !argMemoSubst.contains(argName) =>
              (argName, argExpr)
          }
          .toMap
      val bodyWithArgs =
        substituteBindables(bodyWithCaptures, directArgSubst ++ argMemoSubst)

      val bodyWithRec =
        recName match {
          case Some(name) =>
            // Recursive calls still target a single lambda bound at the call
            // site; this keeps recursion intact while the non-recursive
            // arguments can still benefit from direct substitution.
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

      val withArgMemos =
        Let.bindNamed(argMemoNames.zip(argMemos.map(_._2)), bodyWithRec)

      Let.bindNamed(captureTmpNames.zip(captures), withArgMemos)
    }

    def loop(ex: Expr[A], aliases: Map[Bindable, Lambda[A]]): Expr[A] =
      resolveAlias(ex, aliases, Set.empty) match {
        case lam: Lambda[A] if lam.arity == args.length =>
          betaInline(lam)
        case If(cond, thenExpr, elseExpr) =>
          If(cond, loop(thenExpr, aliases), loop(elseExpr, aliases))
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            on,
            famArities,
            cases.map { case (variant, branch) =>
              (variant, loop(branch, aliases))
            },
            default.map(loop(_, aliases))
          )
        case Always(cond, thenExpr) =>
          Always(cond, loop(thenExpr, aliases))
        case let @ Let(arg, expr, in) =>
          val canPushPastLet =
            arg match {
              case Right(name) =>
                !argNames(name)
              case Left(anon)  =>
                !argAnonIds(anon.ident)
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
          } else App(let, args)
        case letMut @ LetMut(name, in) =>
          if (!argMutIds(name.ident)) LetMut(name, loop(in, aliases))
          else App(letMut, args)
        case other =>
          App(other, args)
      }

    loop(fn, Map.empty)
  }

  final case class ParamDemand(
      totalUses: Int,
      eagerUses: Int,
      branchOnlyUses: Int,
      directCalleeUses: Int,
      nonDirectCalleeUses: Int,
      cheapPositionUses: Int
  ) {
    def unused: Boolean = totalUses == 0
    def deferrable: Boolean =
      (branchOnlyUses > 0) && (eagerUses == 0)
    def lambdaCalleeOnly: Boolean =
      (totalUses > 0) && (nonDirectCalleeUses == 0)
  }

  private[bosatsu] def parameterDemandSummary[A](
      lambda: Lambda[A]
  ): Vector[ParamDemand] = {
    val args = lambda.args.toList
    val indexOf = args.zipWithIndex.toMap
    val totalUses = Array.fill(args.length)(0)
    val eagerUses = Array.fill(args.length)(0)
    val branchOnlyUses = Array.fill(args.length)(0)
    val directCalleeUses = Array.fill(args.length)(0)
    val nonDirectCalleeUses = Array.fill(args.length)(0)
    val cheapPositionUses = Array.fill(args.length)(0)

    inline def record(
        name: Bindable,
        branchOnly: Boolean,
        directCallee: Boolean,
        insideLambda: Boolean,
        cheapContext: Boolean
    ): Unit =
      indexOf.get(name) match {
        case Some(idx) =>
          totalUses(idx) += 1
          if (branchOnly && !insideLambda) branchOnlyUses(idx) += 1
          else eagerUses(idx) += 1
          if (directCallee && !insideLambda) directCalleeUses(idx) += 1
          else nonDirectCalleeUses(idx) += 1
          if (cheapContext) cheapPositionUses(idx) += 1
        case None =>
          ()
      }

    case class LoopState(
        node: ExprOrBool[A],
        branchOnly: Boolean,
        directCallee: Boolean,
        insideLambda: Boolean,
        cheapContext: Boolean,
        shadowed: Set[Bindable]
    )

    @annotation.tailrec
    def loop(todo: List[LoopState]): Unit =
      todo match {
        case LoopState(node, branchOnly, directCallee, insideLambda, cheapContext, shadowed) :: tail =>
          node match {
            case Local(name) if !shadowed(name) =>
              record(name, branchOnly, directCallee, insideLambda, cheapContext)
              loop(tail)
            case Local(_) =>
              loop(tail)
            case Lambda(captures, recName, lamArgs, body) =>
              val captureStates =
                captures.foldRight(tail) { (capture, acc) =>
                  LoopState(capture, branchOnly, false, insideLambda, cheapContext, shadowed) :: acc
                }
              loop(
                LoopState(
                  body,
                  branchOnly = false,
                  directCallee = false,
                  insideLambda = true,
                  cheapContext = false,
                  shadowed = shadowed ++ recName.toSet ++ lamArgs.toList
                ) :: captureStates
              )
            case WhileExpr(cond, effectExpr, _) =>
              loop(
                LoopState(cond, branchOnly, false, insideLambda, false, shadowed) ::
                  LoopState(effectExpr, branchOnly, false, insideLambda, false, shadowed) ::
                  tail
              )
            case App(appFn, appArgs) =>
              val argStates =
                appArgs.toList.foldRight(tail) { (arg, acc) =>
                  LoopState(arg, branchOnly, false, insideLambda, false, shadowed) :: acc
                }
              loop(
                LoopState(appFn, branchOnly, true, insideLambda, false, shadowed) :: argStates
              )
            case Let(arg, value, in) =>
              val shadowed1 =
                arg match {
                  case Right(name) => shadowed + name
                  case Left(_)     => shadowed
                }
              loop(
                LoopState(value, branchOnly, false, insideLambda, false, shadowed) ::
                  LoopState(in, branchOnly, false, insideLambda, cheapContext, shadowed1) ::
                  tail
              )
            case LetMut(_, in) =>
              loop(
                LoopState(in, branchOnly, false, insideLambda, cheapContext, shadowed) :: tail
              )
            case If(cond, thenExpr, elseExpr) =>
              loop(
                LoopState(cond, branchOnly, false, insideLambda, true, shadowed) ::
                  LoopState(thenExpr, true, false, insideLambda, false, shadowed) ::
                  LoopState(elseExpr, true, false, insideLambda, false, shadowed) ::
                  tail
              )
            case SwitchVariant(on, _, cases, default) =>
              val branchStates =
                cases.toList.foldRight(default.fold(tail) { branch =>
                  LoopState(branch, true, false, insideLambda, false, shadowed) :: tail
                }) { case ((_, branch), acc) =>
                  LoopState(branch, true, false, insideLambda, false, shadowed) :: acc
                }
              loop(
                LoopState(on, branchOnly, false, insideLambda, true, shadowed) :: branchStates
              )
            case Always(cond, thenExpr) =>
              loop(
                LoopState(cond, branchOnly, false, insideLambda, false, shadowed) ::
                  LoopState(thenExpr, branchOnly, false, insideLambda, false, shadowed) ::
                  tail
              )
            case PrevNat(of) =>
              loop(
                LoopState(of, branchOnly, false, insideLambda, false, shadowed) :: tail
              )
            case ge: GetEnumElement[?] =>
              loop(
                LoopState(ge.arg, branchOnly, false, insideLambda, true, shadowed) :: tail
              )
            case gs: GetStructElement[?] =>
              loop(
                LoopState(gs.arg, branchOnly, false, insideLambda, true, shadowed) :: tail
              )
            case CompareLit(arg, _, _) =>
              loop(LoopState(arg, branchOnly, false, insideLambda, true, shadowed) :: tail)
            case CompareInt(left, _, right) =>
              loop(
                LoopState(left, branchOnly, false, insideLambda, true, shadowed) ::
                  LoopState(right, branchOnly, false, insideLambda, true, shadowed) ::
                  tail
              )
            case CompareInt64(left, _, right) =>
              loop(
                LoopState(left, branchOnly, false, insideLambda, true, shadowed) ::
                  LoopState(right, branchOnly, false, insideLambda, true, shadowed) ::
                  tail
              )
            case CompareFloat64(left, _, right) =>
              loop(
                LoopState(left, branchOnly, false, insideLambda, true, shadowed) ::
                  LoopState(right, branchOnly, false, insideLambda, true, shadowed) ::
                  tail
              )
            case EqualsNat(arg, _) =>
              loop(LoopState(arg, branchOnly, false, insideLambda, true, shadowed) :: tail)
            case And(left, right) =>
              loop(
                LoopState(left, branchOnly, false, insideLambda, cheapContext, shadowed) ::
                  LoopState(right, branchOnly, false, insideLambda, cheapContext, shadowed) ::
                  tail
              )
            case CheckVariant(arg, _, _, _) =>
              loop(LoopState(arg, branchOnly, false, insideLambda, true, shadowed) :: tail)
            case CheckVariantSet(arg, _, _, _) =>
              loop(LoopState(arg, branchOnly, false, insideLambda, true, shadowed) :: tail)
            case SetMut(_, value) =>
              loop(
                LoopState(value, branchOnly, false, insideLambda, false, shadowed) :: tail
              )
            case LetBool(arg, value, in) =>
              val shadowed1 =
                arg match {
                  case Right(name) => shadowed + name
                  case Left(_)     => shadowed
                }
              loop(
                LoopState(value, branchOnly, false, insideLambda, cheapContext, shadowed) ::
                  LoopState(in, branchOnly, false, insideLambda, cheapContext, shadowed1) ::
                  tail
              )
            case LetMutBool(_, in) =>
              loop(
                LoopState(in, branchOnly, false, insideLambda, cheapContext, shadowed) :: tail
              )
            case Global(_, _, _) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
                Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | _: SuccNat.type |
                _: ZeroNat.type | _: TrueConst.type =>
              loop(tail)
          }
        case _ =>
          ()
      }

    loop(
      LoopState(
        lambda.body,
        branchOnly = false,
        directCallee = false,
        insideLambda = false,
        cheapContext = false,
        // The top-level lambda arguments are exactly the parameters we want to
        // measure, so only nested binders should shadow them during the walk.
        shadowed = lambda.recursiveName.toSet
      ) :: Nil
    )

    Vector.tabulate(args.length) { idx =>
      ParamDemand(
        totalUses(idx),
        eagerUses(idx),
        branchOnlyUses(idx),
        directCalleeUses(idx),
        nonDirectCalleeUses(idx),
        cheapPositionUses(idx)
      )
    }
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
        case SwitchVariant(on, _, cases, default) =>
          val acc0 = loopCheap(on, acc)
          val acc1 = default.fold(acc0)(loopExpr(_, acc0))
          cases.foldLeft(acc1) {
            case (accN, (_, branch)) =>
              loopExpr(branch, accN)
          }
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
            LitInt64(_) |
            MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat =>
          acc
      }

    def loopCheap(ex: CheapExpr[A], acc: Set[Bindable]): Set[Bindable] =
      loopExpr(ex, acc)

    def loopBool(ex: BoolExpr[A], acc: Set[Bindable]): Set[Bindable] =
      ex match {
        case CompareLit(expr, _, _) =>
          loopCheap(expr, acc)
        case CompareInt(left, _, right) =>
          loopCheap(right, loopCheap(left, acc))
        case CompareInt64(left, _, right) =>
          loopCheap(right, loopCheap(left, acc))
        case CompareFloat64(left, _, right) =>
          loopCheap(right, loopCheap(left, acc))
        case EqualsNat(expr, _) =>
          loopCheap(expr, acc)
        case And(left, right) =>
          loopBool(right, loopBool(left, acc))
        case CheckVariant(expr, _, _, _) =>
          loopCheap(expr, acc)
        case CheckVariantSet(expr, _, _, _) =>
          loopCheap(expr, acc)
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
        case SwitchVariant(_, _, cases, default) =>
          val defaultArity = default.flatMap(loop(_, bindableArities, anonArities))
          cases.foldLeft(defaultArity) {
            case (acc, (_, branch)) =>
              sameArity(acc, loop(branch, bindableArities, anonArities))
          }
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
          case None        => other
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

  private def letAnons[A](
      binds: List[(LocalAnon, Expr[A])],
      in: Expr[A]
  ): Expr[A] =
    Let.bindAnons(binds, in)

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
            val c1 = captures.foldLeft(curr) { case (acc, c) =>
              loopExpr(c, acc)
            }
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
          case SwitchVariant(on, _, cases, default) =>
            val curr1 = loopExpr(on, curr)
            val curr2 = default.fold(curr1)(loopExpr(_, curr1))
            cases.foldLeft(curr2) {
              case (acc, (_, branch)) =>
                loopExpr(branch, acc)
            }
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
              LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat =>
            curr
        }

      def loopBool(b: BoolExpr[A], curr: Long): Long =
        b match {
          case CompareLit(expr, _, _) =>
            loopExpr(expr, curr)
          case CompareInt(left, _, right) =>
            loopExpr(right, loopExpr(left, curr))
          case CompareInt64(left, _, right) =>
            loopExpr(right, loopExpr(left, curr))
          case CompareFloat64(left, _, right) =>
            loopExpr(right, loopExpr(left, curr))
          case EqualsNat(expr, _) =>
            loopExpr(expr, curr)
          case And(left, right) =>
            loopBool(right, loopBool(left, curr))
          case CheckVariant(expr, _, _, _) =>
            loopExpr(expr, curr)
          case CheckVariantSet(expr, _, _, _) =>
            loopExpr(expr, curr)
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
            Literal(_) | LitInt64(_) =>
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

    def replaceLinear(
        ex: Expr[A],
        replace: SortedMap[Expr[A], LocalAnon]
    ): Expr[A] =
      replace.get(ex) match {
        case Some(loc) =>
          loc
        case None =>
          ex match {
            case App(fn, args) =>
              App(
                replaceLinear(fn, replace),
                args.map(replaceLinear(_, replace))
              )
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
          if (
            isShareableCtor(e) && !readsBlocked(
              e,
              blockedBindables,
              blockedAnonIds
            )
          )
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
                  args.map(
                    replaceBranch(_, replace, blockedBindables, blockedAnonIds)
                  )
                )
              case PrevNat(of) =>
                PrevNat(
                  replaceBranch(of, replace, blockedBindables, blockedAnonIds)
                )
              case Let(arg, value, in) =>
                val value1 =
                  replaceBranch(
                    value,
                    replace,
                    blockedBindables,
                    blockedAnonIds
                  )
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
        case CompareLit(expr, rel, lit) =>
          recurExprCheap(expr, st) match {
            case (expr1, st1) => (CompareLit(expr1, rel, lit), st1)
          }
        case CompareInt(left, rel, right) =>
          val (left1, st1) = recurExprCheap(left, st)
          val (right1, st2) = recurExprCheap(right, st1)
          (CompareInt(left1, rel, right1), st2)
        case CompareInt64(left, rel, right) =>
          val (left1, st1) = recurExprCheap(left, st)
          val (right1, st2) = recurExprCheap(right, st1)
          (CompareInt64(left1, rel, right1), st2)
        case CompareFloat64(left, rel, right) =>
          val (left1, st1) = recurExprCheap(left, st)
          val (right1, st2) = recurExprCheap(right, st1)
          (CompareFloat64(left1, rel, right1), st2)
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
        case CheckVariantSet(expr, expect, size, famArities) =>
          recurExprCheap(expr, st) match {
            case (expr1, st1) =>
              (CheckVariantSet(expr1, expect, size, famArities), st1)
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
          case SwitchVariant(on, famArities, cases, default) =>
            val (on1, stOn) = recurExprCheap(on, st)
            val (casesRev, stCases) =
              cases.toList.foldLeft((List.empty[(Int, Expr[A])], stOn)) {
                case ((acc, currSt), (variant, branch)) =>
                  val (branch1, st1) = recurExpr(branch, currSt)
                  ((variant, branch1) :: acc, st1)
              }
            val (default1, stDefault) =
              default match {
                case Some(defaultExpr) =>
                  val (defaultExpr1, st1) = recurExpr(defaultExpr, stCases)
                  (Some(defaultExpr1), st1)
                case None              =>
                  (None, stCases)
              }
            (
              SwitchVariant(
                on1,
                famArities,
                NonEmptyList.fromListUnsafe(casesRev.reverse),
                default1
              ),
              stDefault
            )
          case Always(cond, thenExpr) =>
            val (cond1, stCond) = recurBool(cond, st)
            val (then1, stThen) = recurExpr(thenExpr, stCond)
            (Always(cond1, then1), stThen)
          case PrevNat(of) =>
            val (of1, st1) = recurExpr(of, st)
            (PrevNat(of1), st1)
          case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
              Global(_, _, _) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) |
              SuccNat | ZeroNat | GetEnumElement(_, _, _, _) |
              GetStructElement(_, _, _) =>
            (ex, st)
        }

      linearScopeCse(rewritten, st1)
    }

    val init = CseState.initFromExpr(expr)
    // This pass is a pure optimization. If recursion gets too deep, keep
    // semantics by returning the original expression.
    StackSafe.onStackOverflow(recurExpr(expr, init)._1)(expr)
  }

  private val HoistInvariantLoopLetsMinWeight: Int = 4

  // Hoist loop-invariant leading lets out of canonical recursion loops.
  private[bosatsu] def hoistInvariantLoopLets[A](expr: Expr[A]): Expr[A] = {
    def canHoist(
        loopCond: BoolExpr[A],
        arg: Either[LocalAnon, Bindable],
        value: Expr[A],
        in: Expr[A]
    ): Boolean = {
      val noShadowingHazard =
        arg match {
          case Right(name) =>
            !BoolExpr.referencesBindable(loopCond, name)
          case Left(_) =>
            true
        }

      noShadowingHazard &&
      Expr.usesBinding(in, arg) &&
      !hasSideEffect(value) &&
      !Expr.readsMutable(value) &&
      !Expr.containsWhileExpr(value) &&
      // Cheap bindings still extend live ranges and inflate the residual loop
      // body; require non-trivial work before we move them.
      !isTriviallyCheap(value) &&
      (exprWeight(value) >= HoistInvariantLoopLetsMinWeight)
    }

    @annotation.tailrec
    def collectHoistedPrefix(
        loopCond: BoolExpr[A],
        effectExpr: Expr[A],
        revAcc: List[(Either[LocalAnon, Bindable], Expr[A])]
    ): (List[(Either[LocalAnon, Bindable], Expr[A])], Expr[A]) =
      effectExpr match {
        case Let(arg, value, in) if canHoist(loopCond, arg, value, in) =>
          collectHoistedPrefix(loopCond, in, (arg, value) :: revAcc)
        case _ =>
          (revAcc.reverse, effectExpr)
      }

    def rewriteCanonicalRecursionLoop(
        condExpr: BoolExpr[A],
        thenExpr: Expr[A]
    ): Expr[A] =
      (condExpr, thenExpr) match {
        case (
              setCond @ SetMut(condMut, TrueExpr),
              WhileExpr(loopCond, effectExpr, result)
            ) if loopCond == isTrueExpr(condMut) =>
          val (hoisted, residualEffect) =
            collectHoistedPrefix(loopCond, effectExpr, Nil)
          if (hoisted.isEmpty) Always(setCond, thenExpr)
          else
            Let.bindAll(
              hoisted,
              Always(setCond, WhileExpr(loopCond, residualEffect, result))
            )
        case _ =>
          Always(condExpr, thenExpr)
      }

    def recurBool(b: BoolExpr[A]): BoolExpr[A] =
      b match {
        case CompareLit(expr, rel, lit) =>
          CompareLit(recurExprCheap(expr), rel, lit)
        case CompareInt(left, rel, right) =>
          CompareInt(recurExprCheap(left), rel, recurExprCheap(right))
        case CompareInt64(left, rel, right) =>
          CompareInt64(recurExprCheap(left), rel, recurExprCheap(right))
        case CompareFloat64(left, rel, right) =>
          CompareFloat64(recurExprCheap(left), rel, recurExprCheap(right))
        case EqualsNat(expr, nat) =>
          EqualsNat(recurExprCheap(expr), nat)
        case And(left, right) =>
          And(recurBool(left), recurBool(right))
        case CheckVariant(expr, expect, size, famArities) =>
          CheckVariant(recurExprCheap(expr), expect, size, famArities)
        case CheckVariantSet(expr, expect, size, famArities) =>
          CheckVariantSet(recurExprCheap(expr), expect, size, famArities)
        case SetMut(target, value) =>
          SetMut(target, recurExpr(value))
        case LetBool(arg, value, in) =>
          LetBool(arg, recurExpr(value), recurBool(in))
        case LetMutBool(name, in) =>
          LetMutBool(name, recurBool(in))
        case TrueConst =>
          TrueConst
      }

    def recurExprCheap(ex: CheapExpr[A]): CheapExpr[A] =
      recurExpr(ex) match {
        case ch: CheapExpr[A] => ch
        case notCheap         =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected cheap expression when hoisting loop lets, got: $notCheap"
          )
        // $COVERAGE-ON$
      }

    def recurExpr(ex: Expr[A]): Expr[A] =
      ex match {
        case Lambda(captures, recursiveName, args, body) =>
          Lambda(captures.map(recurExpr), recursiveName, args, recurExpr(body))
        case WhileExpr(cond, effectExpr, result) =>
          WhileExpr(recurBool(cond), recurExpr(effectExpr), result)
        case App(fn, args) =>
          App(recurExpr(fn), args.map(recurExpr))
        case Let(arg, value, in) =>
          Let(arg, recurExpr(value), recurExpr(in))
        case LetMut(name, span) =>
          LetMut(name, recurExpr(span))
        case If(cond, thenExpr, elseExpr) =>
          If(recurBool(cond), recurExpr(thenExpr), recurExpr(elseExpr))
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            recurExprCheap(on),
            famArities,
            cases.map { case (variant, branch) =>
              (variant, recurExpr(branch))
            },
            default.map(recurExpr)
          )
        case Always(cond, thenExpr) =>
          rewriteCanonicalRecursionLoop(recurBool(cond), recurExpr(thenExpr))
        case PrevNat(of) =>
          PrevNat(recurExpr(of))
        case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
            Global(_, _, _) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) |
            SuccNat | ZeroNat | GetEnumElement(_, _, _, _) |
            GetStructElement(_, _, _) =>
          ex
      }

    // This pass is a pure optimization. If recursion gets too deep, keep
    // semantics by returning the original expression.
    StackSafe.onStackOverflow(recurExpr(expr))(expr)
  }

  private def canSinkBranchOnlyLet[A](value: Expr[A]): Boolean =
    !hasSideEffect(value) &&
      !Expr.readsMutable(value) &&
      !Expr.containsWhileExpr(value)

  // Defer pure work until the branches that actually need it after inlining
  // has exposed control flow. This is the Matchless analogue of the existing
  // TypedExpr let-to-match sinking rewrite.
  private[bosatsu] def sinkBranchOnlyLets[A](expr: Expr[A]): Expr[A] = {
    def sinkIntoUsedBranch(
        arg: Either[LocalAnon, Bindable],
        value: Expr[A],
        branch: Expr[A],
        uses: Boolean
    ): Expr[A] =
      if (uses) loopExpr(Let(arg, value, branch))
      else branch

    def sinkBinding(
        arg: Either[LocalAnon, Bindable],
        value: Expr[A],
        in: Expr[A]
    ): Option[Expr[A]] =
      if (!canSinkBranchOnlyLet(value)) None
      else
        in match {
          case If(cond, thenExpr, elseExpr)
              if !BoolExpr.usesBinding(cond, arg) =>
            val thenUses = Expr.usesBinding(thenExpr, arg)
            val elseUses = Expr.usesBinding(elseExpr, arg)

            if (thenUses && elseUses) None
            else if (!thenUses && !elseUses) Some(in)
            else
              Some(
                If(
                  cond,
                  sinkIntoUsedBranch(arg, value, thenExpr, thenUses),
                  sinkIntoUsedBranch(arg, value, elseExpr, elseUses)
                )
              )

          case SwitchVariant(on, famArities, cases, default)
              if !Expr.usesBinding(on, arg) =>
            val caseUses = cases.map { case (_, branch) =>
              Expr.usesBinding(branch, arg)
            }
            val defaultUses = default.map(Expr.usesBinding(_, arg))
            val anyUses =
              caseUses.exists(identity) || defaultUses.contains(true)
            val allUses =
              caseUses.forall(identity) && defaultUses.forall(identity)

            if (allUses) None
            else if (!anyUses) Some(in)
            else {
              val cases1 =
                cases.zip(caseUses).map { case ((variant, branch), uses) =>
                  (variant, sinkIntoUsedBranch(arg, value, branch, uses))
                }
              val default1 =
                default.zip(defaultUses).map { case (branch, uses) =>
                  sinkIntoUsedBranch(arg, value, branch, uses)
                }

              Some(SwitchVariant(on, famArities, cases1, default1))
            }

          case _ =>
            None
        }

    def loopBool(boolExpr: BoolExpr[A]): BoolExpr[A] =
      boolExpr match {
        case CompareLit(expr, rel, lit) =>
          CompareLit(loopCheap(expr), rel, lit)
        case CompareInt(left, rel, right) =>
          CompareInt(loopCheap(left), rel, loopCheap(right))
        case CompareInt64(left, rel, right) =>
          CompareInt64(loopCheap(left), rel, loopCheap(right))
        case CompareFloat64(left, rel, right) =>
          CompareFloat64(loopCheap(left), rel, loopCheap(right))
        case EqualsNat(expr, nat) =>
          EqualsNat(loopCheap(expr), nat)
        case And(left, right) =>
          And(loopBool(left), loopBool(right))
        case CheckVariant(expr, expect, size, famArities) =>
          CheckVariant(loopCheap(expr), expect, size, famArities)
        case CheckVariantSet(expr, expect, size, famArities) =>
          CheckVariantSet(loopCheap(expr), expect, size, famArities)
        case SetMut(target, value) =>
          SetMut(target, loopExpr(value))
        case LetBool(arg, value, in) =>
          LetBool(arg, loopExpr(value), loopBool(in))
        case LetMutBool(name, in) =>
          LetMutBool(name, loopBool(in))
        case TrueConst =>
          TrueConst
      }

    def loopCheap(ex: CheapExpr[A]): CheapExpr[A] =
      loopExpr(ex) match {
        case ch: CheapExpr[A] => ch
        case notCheap         =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected cheap expression when sinking branch-only lets, got: $notCheap"
          )
        // $COVERAGE-ON$
      }

    def loopExpr(ex: Expr[A]): Expr[A] =
      ex match {
        case Lambda(captures, recursiveName, args, body) =>
          Lambda(captures.map(loopExpr), recursiveName, args, loopExpr(body))
        case WhileExpr(cond, effectExpr, result) =>
          WhileExpr(loopBool(cond), loopExpr(effectExpr), result)
        case App(fn, args) =>
          App(loopExpr(fn), args.map(loopExpr))
        case Let(arg, value, in) =>
          val value1 = loopExpr(value)
          val in1 = loopExpr(in)
          sinkBinding(arg, value1, in1).getOrElse(Let(arg, value1, in1))
        case LetMut(name, span) =>
          LetMut(name, loopExpr(span))
        case If(cond, thenExpr, elseExpr) =>
          If(loopBool(cond), loopExpr(thenExpr), loopExpr(elseExpr))
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            loopCheap(on),
            famArities,
            cases.map { case (variant, branch) =>
              (variant, loopExpr(branch))
            },
            default.map(loopExpr)
          )
        case Always(cond, thenExpr) =>
          Always(loopBool(cond), loopExpr(thenExpr))
        case PrevNat(of) =>
          PrevNat(loopExpr(of))
        case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
            Global(_, _, _) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) |
            SuccNat | ZeroNat | GetEnumElement(_, _, _, _) |
            GetStructElement(_, _, _) =>
          ex
      }

    StackSafe.onStackOverflow(loopExpr(expr))(expr)
  }

  // Evaluate selector-like tests against locally known pure values so inlining
  // can collapse branches such as `let x = False in if x then ... else ...`.
  private[bosatsu] def simplifyKnownConditions[A](expr: Expr[A]): Expr[A] = {
    case class KnownEnv(
        bindables: Map[Bindable, Expr[A]],
        anons: Map[Long, Expr[A]]
    )

    val emptyEnv = KnownEnv(Map.empty, Map.empty)

    def shadowBinding(
        env: KnownEnv,
        arg: Either[LocalAnon, Bindable]
    ): KnownEnv =
      arg match {
        case Right(name) =>
          env.copy(bindables = env.bindables - name)
        case Left(LocalAnon(id)) =>
          env.copy(anons = env.anons - id)
      }

    def knownValue(
        ex: Expr[A],
        env: KnownEnv,
        seenBindables: Set[Bindable],
        seenAnons: Set[Long]
    ): Option[Expr[A]] =
      ex match {
        case Local(name) if !seenBindables(name) =>
          env.bindables
            .get(name)
            .flatMap(knownValue(_, env, seenBindables + name, seenAnons))
        case LocalAnon(id) if !seenAnons(id) =>
          env.anons
            .get(id)
            .flatMap(knownValue(_, env, seenBindables, seenAnons + id))
        case lam: Lambda[A] if lam.recursiveName.isEmpty =>
          Some(lam)
        case global @ Global(_, _, _) =>
          Some(global)
        case lit @ (Literal(_) | LitInt64(_)) =>
          Some(lit)
        case enumExpr @ MakeEnum(_, 0, _) =>
          Some(enumExpr)
        case structExpr @ MakeStruct(0) =>
          Some(structExpr)
        case ZeroNat =>
          Some(ZeroNat)
        case App(cons @ MakeEnum(_, arity, _), args) if args.length == arity =>
          args.toList
            .traverse(knownValue(_, env, seenBindables, seenAnons))
            .map(args1 => App(cons, NonEmptyList.fromListUnsafe(args1)))
        case App(cons @ MakeStruct(arity), args) if args.length == arity =>
          args.toList
            .traverse(knownValue(_, env, seenBindables, seenAnons))
            .map(args1 => App(cons, NonEmptyList.fromListUnsafe(args1)))
        case App(SuccNat, NonEmptyList(arg, Nil)) =>
          knownValue(arg, env, seenBindables, seenAnons)
            .map(arg1 => App(SuccNat, NonEmptyList.one(arg1)))
        case PrevNat(of) =>
          knownValue(of, env, seenBindables, seenAnons).collect {
            case App(SuccNat, NonEmptyList(prev, Nil)) => prev
          }
        case GetEnumElement(arg, variant, index, size) =>
          knownValue(arg, env, seenBindables, seenAnons).flatMap {
            case App(MakeEnum(v, arity, _), args)
                if (v == variant) && (arity == size) =>
              args.toList.lift(index)
            case _ =>
              None
          }
        case GetStructElement(arg, index, size) =>
          knownValue(arg, env, seenBindables, seenAnons).flatMap {
            case App(MakeStruct(arity), args) if arity == size =>
              args.toList.lift(index)
            case value if (size == 1) && (index == 0) =>
              Some(value)
            case _ =>
              None
          }
        case _ =>
          None
      }

    def knownEnumTag(ex: Expr[A], env: KnownEnv): Option[(Int, Int, List[Int])] =
      knownValue(ex, env, Set.empty, Set.empty).flatMap {
        case MakeEnum(variant, 0, famArities) =>
          Some((variant, 0, famArities))
        case App(MakeEnum(variant, arity, famArities), args)
            if args.length == arity =>
          Some((variant, arity, famArities))
        case _ =>
          None
      }

    def knownNatTag(ex: Expr[A], env: KnownEnv): Option[DataRepr.Nat] =
      knownValue(ex, env, Set.empty, Set.empty).flatMap {
        case ZeroNat                            => Some(DataRepr.ZeroNat)
        case App(SuccNat, NonEmptyList(_, Nil)) => Some(DataRepr.SuccNat)
        case _                                  => None
      }

    def compareLitBoolValue(
        left: Expr[A],
        rel: CompareRel,
        right: Lit
    ): Option[Boolean] =
      left match {
        case Literal(found) =>
          compareLiteralValues(found, rel, right)
        case _ =>
          None
      }

    def compareIntBoolValue(
        left: Expr[A],
        rel: CompareRel,
        right: Expr[A]
    ): Option[Boolean] =
      (left, right) match {
        case (Literal(Lit.Integer(lhs)), Literal(Lit.Integer(rhs))) =>
          Some(compareRelHolds(rel, lhs.compareTo(rhs)))
        case _ =>
          None
      }

    def compareInt64BoolValue(
        left: Expr[A],
        rel: CompareRel,
        right: Expr[A]
    ): Option[Boolean] =
      (left, right) match {
        case (LitInt64(lhs), LitInt64(rhs)) =>
          Some(compareRelHolds(rel, java.lang.Long.compare(lhs, rhs)))
        case _ =>
          None
      }

    def compareFloat64BoolValue(
        left: Expr[A],
        rel: CompareRel,
        right: Expr[A]
    ): Option[Boolean] =
      (left, right) match {
        case (Literal(lhs: Lit.Float64), Literal(rhs: Lit.Float64)) =>
          Some(compareFloat64Values(lhs.toDouble, rel, rhs.toDouble))
        case _ =>
          None
      }

    def boolValue(ex: BoolExpr[A], env: KnownEnv): Option[Boolean] =
      ex match {
        case CompareLit(expr, rel, lit) =>
          knownValue(expr, env, Set.empty, Set.empty).flatMap(compareLitBoolValue(_, rel, lit))
        case CompareInt(left, rel, right) =>
          (
            knownValue(left, env, Set.empty, Set.empty),
            knownValue(right, env, Set.empty, Set.empty)
          ).flatMapN(compareIntBoolValue(_, rel, _))
        case CompareInt64(left, rel, right) =>
          (
            knownValue(left, env, Set.empty, Set.empty),
            knownValue(right, env, Set.empty, Set.empty)
          ).flatMapN(compareInt64BoolValue(_, rel, _))
        case CompareFloat64(left, rel, right) =>
          (
            knownValue(left, env, Set.empty, Set.empty),
            knownValue(right, env, Set.empty, Set.empty)
          ).flatMapN(compareFloat64BoolValue(_, rel, _))
        case EqualsNat(expr, nat) =>
          knownNatTag(expr, env).map(_ == nat)
        case And(left, right) =>
          (boolValue(left, env), boolValue(right, env)) match {
            case (Some(false), _) | (_, Some(false)) =>
              Some(false)
            case (Some(true), other) =>
              other
            case (other, Some(true)) =>
              other
            case _ =>
              None
          }
        case CheckVariant(expr, expect, size, famArities) =>
          knownEnumTag(expr, env).collect {
            case (variant, `size`, famArities1) if famArities1 == famArities =>
              variant == expect
          }
        case CheckVariantSet(expr, expect, size, famArities) =>
          knownEnumTag(expr, env).collect {
            case (variant, `size`, famArities1) if famArities1 == famArities =>
              expect.exists(_ == variant)
          }
        case LetBool(arg, value, in) =>
          boolValue(in, extendEnv(env, arg, value))
        case TrueConst =>
          Some(true)
        case SetMut(_, _) | LetMutBool(_, _) =>
          None
      }

    def extendEnv(
        env: KnownEnv,
        arg: Either[LocalAnon, Bindable],
        value: Expr[A]
    ): KnownEnv = {
      val base = shadowBinding(env, arg)
      knownValue(value, env, Set.empty, Set.empty) match {
        case Some(value1) =>
          arg match {
            case Right(name) =>
              base.copy(bindables = base.bindables.updated(name, value1))
            case Left(LocalAnon(id)) =>
              base.copy(anons = base.anons.updated(id, value1))
          }
        case None =>
          base
      }
    }

    def canDiscardBinding(value: Expr[A], env: KnownEnv): Boolean =
      knownValue(value, env, Set.empty, Set.empty).isDefined || (value match {
        case Local(_) | ClosureSlot(_) | LocalAnon(_) | Global(_, _, _) |
            Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) | SuccNat |
            ZeroNat | Lambda(_, _, _, _) =>
          true
        case _ =>
          false
      })

    def recurExprCheap(ex: CheapExpr[A], env: KnownEnv): CheapExpr[A] =
      recurExpr(ex, env) match {
        case ch: CheapExpr[A] => ch
        case notCheap         =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected cheap expression while simplifying known Matchless conditions, found: $notCheap"
          )
        // $COVERAGE-ON$
      }

    def recurBool(ex: BoolExpr[A], env: KnownEnv): BoolExpr[A] =
      ex match {
        case CompareLit(expr, rel, lit) =>
          CompareLit(recurExprCheap(expr, env), rel, lit)
        case CompareInt(left, rel, right) =>
          CompareInt(recurExprCheap(left, env), rel, recurExprCheap(right, env))
        case CompareInt64(left, rel, right) =>
          CompareInt64(recurExprCheap(left, env), rel, recurExprCheap(right, env))
        case CompareFloat64(left, rel, right) =>
          CompareFloat64(recurExprCheap(left, env), rel, recurExprCheap(right, env))
        case EqualsNat(expr, nat) =>
          EqualsNat(recurExprCheap(expr, env), nat)
        case And(left, right) =>
          (recurBool(left, env), recurBool(right, env)) match {
            case (TrueConst, right1) => right1
            case (left1, TrueConst)  => left1
            case (left1, right1)     => And(left1, right1)
          }
        case CheckVariant(expr, expect, size, famArities) =>
          CheckVariant(recurExprCheap(expr, env), expect, size, famArities)
        case CheckVariantSet(expr, expect, size, famArities) =>
          CheckVariantSet(
            recurExprCheap(expr, env),
            expect,
            size,
            famArities
          )
        case SetMut(target, value) =>
          SetMut(target, recurExpr(value, env))
        case LetBool(arg, value, in) =>
          val value1 = recurExpr(value, env)
          val env1 = extendEnv(env, arg, value1)
          val in1 = recurBool(in, env1)
          if (!BoolExpr.usesBinding(in1, arg) && canDiscardBinding(value1, env))
            in1
          else LetBool(arg, value1, in1)
        case LetMutBool(name, in) =>
          LetMutBool(name, recurBool(in, env))
        case TrueConst =>
          TrueConst
      }

    def recurExpr(ex: Expr[A], env: KnownEnv): Expr[A] =
      ex match {
        case Lambda(captures, recursiveName, args, body) =>
          val bodyEnv =
            env.copy(bindables = env.bindables -- recursiveName.toSet -- args.toList)
          Lambda(
            captures.map(recurExpr(_, env)),
            recursiveName,
            args,
            recurExpr(body, bodyEnv)
          )
        case WhileExpr(cond, effectExpr, result) =>
          WhileExpr(recurBool(cond, env), recurExpr(effectExpr, env), result)
        case App(fn, args) =>
          val fn1 = recurExpr(fn, env)
          val args1 = args.map(recurExpr(_, env))
          val resolvedFn =
            knownValue(fn1, env, Set.empty, Set.empty).getOrElse(fn1)

          recoverTopLevelLambda(resolvedFn) match {
            case lam: Lambda[A] if lam.arity == args1.length =>
              recurExpr(inlineApplyArgs(lam, args1), env)
            case _ =>
              App(resolvedFn, args1)
          }
        case Let(arg, value, in) =>
          val value1 = recurExpr(value, env)
          val env1 = extendEnv(env, arg, value1)
          val in1 = recurExpr(in, env1)
          if (!Expr.usesBinding(in1, arg) && canDiscardBinding(value1, env)) in1
          else Let(arg, value1, in1)
        case LetMut(name, in) =>
          LetMut(name, recurExpr(in, env))
        case If(cond, thenExpr, elseExpr) =>
          val cond1 = recurBool(cond, env)
          if (!hasSideEffect(cond1))
            boolValue(cond1, env) match {
              case Some(true) =>
                recurExpr(thenExpr, env)
              case Some(false) =>
                recurExpr(elseExpr, env)
              case None =>
                If(
                  cond1,
                  recurExpr(thenExpr, env),
                  recurExpr(elseExpr, env)
                )
            }
          else
            If(
              cond1,
              recurExpr(thenExpr, env),
              recurExpr(elseExpr, env)
            )
        case SwitchVariant(on, famArities, cases, default) =>
          val on1 = recurExprCheap(on, env)
          knownEnumTag(on1, env) match {
            case Some((variant, _, famArities1)) if famArities1 == famArities =>
              val branch =
                cases.collectFirst { case (`variant`, value) => value }
                  .orElse(default)
              branch match {
                case Some(expr1) => recurExpr(expr1, env)
                case None        =>
                  SwitchVariant(
                    on1,
                    famArities,
                    cases.map { case (caseVariant, branchExpr) =>
                      (caseVariant, recurExpr(branchExpr, env))
                    },
                    default.map(recurExpr(_, env))
                  )
              }
            case _ =>
              SwitchVariant(
                on1,
                famArities,
                cases.map { case (variant, branch) =>
                  (variant, recurExpr(branch, env))
                },
                default.map(recurExpr(_, env))
              )
          }
        case Always(cond, thenExpr) =>
          val cond1 = recurBool(cond, env)
          if (!hasSideEffect(cond1) && boolValue(cond1, env).contains(true))
            recurExpr(thenExpr, env)
          else
            Always(cond1, recurExpr(thenExpr, env))
        case PrevNat(of) =>
          PrevNat(recurExpr(of, env))
        case GetEnumElement(arg, variant, index, size) =>
          val rewritten = GetEnumElement(recurExprCheap(arg, env), variant, index, size)
          knownValue(rewritten, env, Set.empty, Set.empty).fold(rewritten: Expr[A])(recurExpr(_, env))
        case GetStructElement(arg, index, size) =>
          val rewritten = GetStructElement(recurExprCheap(arg, env), index, size)
          knownValue(rewritten, env, Set.empty, Set.empty).fold(rewritten: Expr[A])(recurExpr(_, env))
        case Local(_) | ClosureSlot(_) | LocalAnon(_) | LocalAnonMut(_) |
            Global(_, _, _) | Literal(_) | LitInt64(_) | MakeEnum(_, _, _) | MakeStruct(_) |
            SuccNat | ZeroNat =>
          ex
      }

    // This pass is a pure optimization. If recursion gets too deep, keep
    // semantics by returning the original expression.
    StackSafe.onStackOverflow(recurExpr(expr, emptyEnv))(expr)
  }

  case class LetMut[A](name: LocalAnonMut, span: Expr[A]) extends Expr[A] {
    // often we have several LetMut at once, return all them
    def flatten: (NonEmptyList[LocalAnonMut], Expr[A]) = {
      var reverseNames: List[LocalAnonMut] = name :: Nil
      var tailExpr: Expr[A] = span
      var done = false

      while (!done) {
        tailExpr match {
          case LetMut(nextName, nextTail) =>
            reverseNames = nextName :: reverseNames
            tailExpr = nextTail
          case _ =>
            done = true
        }
      }

      (
        NonEmptyList.fromListUnsafe(reverseNames.reverse),
        tailExpr
      )
    }
  }
  case class Literal(lit: Lit) extends CheapExpr[Nothing]
  case class LitInt64(value: Long) extends CheapExpr[Nothing]

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

  enum CompareRel derives CanEqual {
    case Eq, Ne, Lt, Lte, Gt, Gte
  }

  private[bosatsu] def compareRelHolds(rel: CompareRel, cmp: Int): Boolean =
    rel match {
      case CompareRel.Eq  => cmp == 0
      case CompareRel.Ne  => cmp != 0
      case CompareRel.Lt  => cmp < 0
      case CompareRel.Lte => cmp <= 0
      case CompareRel.Gt  => cmp > 0
      case CompareRel.Gte => cmp >= 0
    }

  private[bosatsu] def compareFloat64Values(
      left: Double,
      rel: CompareRel,
      right: Double
  ): Boolean =
    compareRelHolds(rel, PredefImpl.compareFloat64Total(left, right))

  private[bosatsu] def compareLiteralValues(
      left: Lit,
      rel: CompareRel,
      right: Lit
  ): Option[Boolean] =
    (left, right) match {
      case (Lit.Integer(lhs), Lit.Integer(rhs)) =>
        Some(compareRelHolds(rel, lhs.compareTo(rhs)))
      case (lhs: Lit.StringMatchResult, rhs: Lit.StringMatchResult) =>
        Some(compareRelHolds(rel, lhs.asStr.compareTo(rhs.asStr)))
      case (lhs: Lit.Float64, rhs: Lit.Float64) =>
        Some(compareFloat64Values(lhs.toDouble, rel, rhs.toDouble))
      case _ =>
        None
    }
  object BoolExpr {
    private def boolTag[A](boolExpr: BoolExpr[A]): Int =
      boolExpr match {
        case _: CompareLit[?]     => 0
        case _: CompareInt[?]     => 1
        case _: CompareInt64[?]   => 2
        case _: CompareFloat64[?] => 3
        case _: EqualsNat[?]      => 4
        case _: And[?]            => 5
        case _: CheckVariant[?]   => 6
        case _: CheckVariantSet[?] => 7
        case _: SetMut[?]          => 8
        case TrueConst             => 9
        case _: LetBool[?]         => 10
        case _: LetMutBool[?]      => 11
      }

    private given Order[LocalAnon] = Order.by(_.ident)
    private given Order[LocalAnonMut] = Order.by(_.ident)
    private given Order[Bindable] = Identifier.Bindable.bindableOrder
    private given Order[Either[LocalAnon, Bindable]] =
      Order[Either[LocalAnon, Bindable]]
    private given Order[Lit] = Order.fromOrdering(using Lit.litOrdering)
    private given Order[CompareRel] = Order.by {
      case CompareRel.Eq  => 0
      case CompareRel.Ne  => 1
      case CompareRel.Lt  => 2
      case CompareRel.Lte => 3
      case CompareRel.Gt  => 4
      case CompareRel.Gte => 5
    }
    private given Order[DataRepr.Nat] = Order.by {
      case DataRepr.ZeroNat => 0
      case DataRepr.SuccNat => 1
    }

    given [A: Order]: Order[BoolExpr[A]] with {
      def compare(left: BoolExpr[A], right: BoolExpr[A]): Int =
        (left, right) match {
          case (CompareLit(exprL, relL, litL), CompareLit(exprR, relR, litR)) =>
            val c1 = Order[Expr[A]].compare(exprL, exprR)
            if (c1 != 0) c1
            else {
              val c2 = Order[CompareRel].compare(relL, relR)
              if (c2 != 0) c2
              else Order[Lit].compare(litL, litR)
            }

          case (CompareInt(leftL, relL, rightL), CompareInt(leftR, relR, rightR)) =>
            val c1 = Order[Expr[A]].compare(leftL, leftR)
            if (c1 != 0) c1
            else {
              val c2 = Order[CompareRel].compare(relL, relR)
              if (c2 != 0) c2
              else Order[Expr[A]].compare(rightL, rightR)
            }

          case (
                CompareInt64(leftL, relL, rightL),
                CompareInt64(leftR, relR, rightR)
              ) =>
            val c1 = Order[Expr[A]].compare(leftL, leftR)
            if (c1 != 0) c1
            else {
              val c2 = Order[CompareRel].compare(relL, relR)
              if (c2 != 0) c2
              else Order[Expr[A]].compare(rightL, rightR)
            }

          case (
                CompareFloat64(leftL, relL, rightL),
                CompareFloat64(leftR, relR, rightR)
              ) =>
            val c1 = Order[Expr[A]].compare(leftL, leftR)
            if (c1 != 0) c1
            else {
              val c2 = Order[CompareRel].compare(relL, relR)
              if (c2 != 0) c2
              else Order[Expr[A]].compare(rightL, rightR)
            }

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
                CheckVariantSet(exprL, expectL, sizeL, famAritiesL),
                CheckVariantSet(exprR, expectR, sizeR, famAritiesR)
              ) =>
            val c1 = Order[Expr[A]].compare(exprL, exprR)
            if (c1 != 0) c1
            else {
              val c2 = Order[NonEmptyList[Int]].compare(expectL, expectR)
              if (c2 != 0) c2
              else {
                val c3 = java.lang.Integer.compare(sizeL, sizeR)
                if (c3 != 0) c3
                else Order[List[Int]].compare(famAritiesL, famAritiesR)
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

    def referencesBindable[A](boolExpr: BoolExpr[A], target: Bindable): Boolean = {
      def checkBool(boolExpr: BoolExpr[A]): Boolean =
        loopBool(boolExpr)

      @annotation.tailrec
      def loopBool(boolExpr: BoolExpr[A]): Boolean =
        boolExpr match {
          case CompareLit(expr, _, _) =>
            Expr.referencesBindable(expr, target)
          case CompareInt(left, _, right) =>
            Expr.referencesBindable(left, target) ||
              Expr.referencesBindable(right, target)
          case CompareInt64(left, _, right) =>
            Expr.referencesBindable(left, target) ||
              Expr.referencesBindable(right, target)
          case CompareFloat64(left, _, right) =>
            Expr.referencesBindable(left, target) ||
              Expr.referencesBindable(right, target)
          case EqualsNat(expr, _) =>
            Expr.referencesBindable(expr, target)
          case And(left, right) =>
            if (checkBool(left)) true
            else loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            Expr.referencesBindable(expr, target)
          case CheckVariantSet(expr, _, _, _) =>
            Expr.referencesBindable(expr, target)
          case SetMut(_, value) =>
            Expr.referencesBindable(value, target)
          case LetBool(arg, value, in) =>
            if (Expr.referencesBindable(value, target)) true
            else
              arg match {
                case Right(name) if name == target => false
                case _                             => loopBool(in)
              }
          case LetMutBool(_, in) =>
            loopBool(in)
          case TrueConst =>
            false
        }

      loopBool(boolExpr)
    }

    def referencesLocalAnon[A](boolExpr: BoolExpr[A], target: Long): Boolean = {
      def checkBool(boolExpr: BoolExpr[A]): Boolean =
        loopBool(boolExpr)

      @annotation.tailrec
      def loopBool(boolExpr: BoolExpr[A]): Boolean =
        boolExpr match {
          case CompareLit(expr, _, _) =>
            Expr.referencesLocalAnon(expr, target)
          case CompareInt(left, _, right) =>
            Expr.referencesLocalAnon(left, target) ||
              Expr.referencesLocalAnon(right, target)
          case CompareInt64(left, _, right) =>
            Expr.referencesLocalAnon(left, target) ||
              Expr.referencesLocalAnon(right, target)
          case CompareFloat64(left, _, right) =>
            Expr.referencesLocalAnon(left, target) ||
              Expr.referencesLocalAnon(right, target)
          case EqualsNat(expr, _) =>
            Expr.referencesLocalAnon(expr, target)
          case And(left, right) =>
            if (checkBool(left)) true
            else loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            Expr.referencesLocalAnon(expr, target)
          case CheckVariantSet(expr, _, _, _) =>
            Expr.referencesLocalAnon(expr, target)
          case SetMut(_, value) =>
            Expr.referencesLocalAnon(value, target)
          case LetBool(arg, value, in) =>
            if (Expr.referencesLocalAnon(value, target)) true
            else
              arg match {
                case Left(LocalAnon(id)) if id == target => false
                case _                                   => loopBool(in)
              }
          case LetMutBool(_, in) =>
            loopBool(in)
          case TrueConst =>
            false
        }

      loopBool(boolExpr)
    }

    def usesBinding[A](
        boolExpr: BoolExpr[A],
        arg: Either[LocalAnon, Bindable]
    ): Boolean =
      arg match {
        case Right(name) =>
          referencesBindable(boolExpr, name)
        case Left(LocalAnon(id)) =>
          referencesLocalAnon(boolExpr, id)
      }
  }
  // returns 1 if it does, else 0
  case class CompareLit[A](expr: CheapExpr[A], rel: CompareRel, lit: Lit)
      extends BoolExpr[A]
  case class CompareInt[A](
      left: CheapExpr[A],
      rel: CompareRel,
      right: CheapExpr[A]
  ) extends BoolExpr[A]
  case class CompareInt64[A](
      left: CheapExpr[A],
      rel: CompareRel,
      right: CheapExpr[A]
  ) extends BoolExpr[A]
  case class CompareFloat64[A](
      left: CheapExpr[A],
      rel: CompareRel,
      right: CheapExpr[A]
  ) extends BoolExpr[A]
  object EqualsLit {
    def apply[A](expr: CheapExpr[A], lit: Lit): BoolExpr[A] =
      CompareLit(expr, CompareRel.Eq, lit)

    def unapply[A](boolExpr: BoolExpr[A]): Option[(CheapExpr[A], Lit)] =
      boolExpr match {
        case CompareLit(expr, CompareRel.Eq, lit) =>
          Some((expr, lit))
        case _ =>
          None
      }
  }

  object LtEqLit {
    def apply[A](expr: CheapExpr[A], lit: Lit): BoolExpr[A] =
      CompareLit(expr, CompareRel.Lte, lit)

    def unapply[A](boolExpr: BoolExpr[A]): Option[(CheapExpr[A], Lit)] =
      boolExpr match {
        case CompareLit(expr, CompareRel.Lte, lit) =>
          Some((expr, lit))
        case _ =>
          None
      }
  }
  case class EqualsNat[A](expr: CheapExpr[A], nat: DataRepr.Nat)
      extends BoolExpr[A]
  // 1 if both are > 0
  case class And[A](e1: BoolExpr[A], e2: BoolExpr[A]) extends BoolExpr[A]
  // checks if variant matches, and if so, writes to
  // a given mut
  // Note: this remains distinct from CheckVariantSet(NonEmptyList.one(...)).
  // Several lowering/optimization paths produce singleton checks directly and
  // keeping this node avoids broader cross-pass churn in this PR.
  case class CheckVariant[A](
      expr: CheapExpr[A],
      expect: Int,
      size: Int,
      famArities: List[Int]
  ) extends BoolExpr[A]
  // checks if variant is in the provided sorted distinct set
  case class CheckVariantSet[A](
      expr: CheapExpr[A],
      expect: NonEmptyList[Int],
      size: Int,
      famArities: List[Int]
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
      extends BoolExpr[A] {
    // often we have several LetMutBool at once, return all them
    def flatten: (NonEmptyList[LocalAnonMut], BoolExpr[A]) = {
      var reverseNames: List[LocalAnonMut] = name :: Nil
      var tailBool: BoolExpr[A] = span
      var done = false

      while (!done) {
        tailBool match {
          case LetMutBool(nextName, nextTail) =>
            reverseNames = nextName :: reverseNames
            tailBool = nextTail
          case _ =>
            done = true
        }
      }

      (
        NonEmptyList.fromListUnsafe(reverseNames.reverse),
        tailBool
      )
    }
  }
  object LetMutBool {
    def apply[A](lst: List[LocalAnonMut], span: BoolExpr[A]): BoolExpr[A] =
      lst.foldRight(span)(LetMutBool(_, _))
  }

  def hasSideEffect(bx: BoolExpr[Any]): Boolean =
    bx match {
      case SetMut(_, _) => true
      case TrueConst | CheckVariant(_, _, _, _) | CheckVariantSet(_, _, _, _) |
          CompareLit(_, _, _) |
          CompareInt(_, _, _) |
          CompareInt64(_, _, _) |
          CompareFloat64(_, _, _) |
          EqualsNat(_, _) =>
        false
      case And(b1, b2)      => hasSideEffect(b1) || hasSideEffect(b2)
      case LetBool(_, x, b) =>
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
      case SwitchVariant(on, _, cases, default) =>
        hasSideEffect(on) ||
          cases.exists { case (_, branch) => hasSideEffect(branch) } ||
          default.exists(hasSideEffect)
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
  case class SwitchVariant[A](
      on: CheapExpr[A],
      famArities: List[Int],
      cases: NonEmptyList[(Int, Expr[A])],
      // Variants not listed in `cases` (for example via wildcard/default rows)
      // evaluate this branch.
      default: Option[Expr[A]]
  ) extends Expr[A] {
    private val caseVariants = cases.toList.map(_._1)
    private val familySize = famArities.length

    Require(cases.length >= 2, "SwitchVariant requires at least two cases")
    Require(
      caseVariants.distinct.length == caseVariants.length,
      s"SwitchVariant variants must be distinct, found: $caseVariants"
    )
    Require(
      caseVariants.forall(v => (0 <= v) && (v < familySize)),
      s"SwitchVariant variants must be in [0, $familySize), found: $caseVariants"
    )

    def toIfElse: Expr[A] = {
      val caseMap = cases.toList.toMap
      // A missing case without default is unreachable in well-typed lowered matches.
      // Keep this total by reusing one branch if such a gap remains.
      val fallbackExpr = default.getOrElse(cases.head._2)
      val variantBranches = Vector.tabulate(familySize) { variant =>
        caseMap.getOrElse(variant, fallbackExpr)
      }

      def sameExpr(from: Int, until: Int): Option[Expr[A]] = {
        val first = variantBranches(from)
        var idx = from + 1
        var isSame = true
        while (isSame && (idx < until)) {
          isSame = first.equals(variantBranches(idx))
          idx = idx + 1
        }
        if (isSame) Some(first) else None
      }

      def build(from: Int, until: Int): Expr[A] =
        sameExpr(from, until) match {
          case Some(single) =>
            single
          case None         =>
            val split = from + ((until - from) / 2)
            val left = build(from, split)
            val right = build(split, until)
            if (left.equals(right)) left
            else {
              val leftVariants =
                NonEmptyList.fromListUnsafe((from until split).toList)
              If(
                CheckVariantSet(on, leftVariants, 0, famArities),
                left,
                right
              )
            }
        }

      build(0, familySize)
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

  private val SwitchVariantMinCases: Int = 4
  private val LiteralTreeMinCases: Int = 4
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

    def listOf[A](items: List[Expr[A]]): Expr[A] =
      items.foldRight(Nil: Expr[A]) { case (h, t) =>
        cons(h, t)
      }

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

  private inline def maybeMemoWith[F[_]: Monad, A, R](
      arg: Expr[A],
      tmp: F[Long]
  )(
      inline fn: CheapExpr[A] => F[R]
  )(
      inline bind: (LocalAnon, Expr[A], R) => R
  ): F[R] =
    arg match {
      case c: CheapExpr[A] => fn(c)
      case _               =>
        for {
          nm <- tmp
          bound = LocalAnon(nm)
          res <- fn(bound)
        } yield bind(bound, arg, res)
    }

  private inline def maybeMemo[F[_]: Monad, A](
      arg: Expr[A],
      tmp: F[Long]
  )(inline fn: CheapExpr[A] => F[Expr[A]]): F[Expr[A]] =
    maybeMemoWith(arg, tmp)(fn) { (bound, inExpr, inBody) =>
      Let(bound, inExpr, inBody)
    }

  object StringMatcher {
    private val emptyStringLit: Lit = Lit.Str("")
    private val emptyStringExpr: Expr[Nothing] = Literal(emptyStringLit)
    private val concatStringName = Identifier.Name("concat_String")
    private val charToStringName = Identifier.Name("char_to_String")
    private val partitionStringName = Identifier.Name("partition_String")
    private val unconsStringName = Identifier.Name("uncons_String")
    private val tailOrEmptyStringName = Identifier.Name("tail_or_empty_String")
    private val someCons = Constructor("Some")
    private val tuple2Cons = Constructor("Tuple2")

    final private case class Ctx[F[_], A](
        from: A,
        mustMatch: Boolean,
        optionSomeData: (Int, Int, List[Int]),
        tuple2Arity: Int,
        newMut: F[LocalAnonMut],
        newConst: F[LocalAnon]
    ) {
      def newMutOpt(
          some: Boolean
      )(implicit F: Monad[F]): F[Option[LocalAnonMut]] =
        if (some) newMut.map(Some(_))
        else F.pure(None)

      def optionIsSome(opt: CheapExpr[A]): BoolExpr[A] = {
        val (variant, size, famArities) = optionSomeData
        CheckVariant(opt, variant, size, famArities)
      }

      def optionGetSome(opt: CheapExpr[A]): CheapExpr[A] = {
        val (variant, size, _) = optionSomeData
        GetEnumElement(opt, variant, 0, size)
      }

      def getFromTuple2(tuple: CheapExpr[A], idx: Int): CheapExpr[A] =
        GetStructElement(tuple, idx, tuple2Arity)
    }

    // Read Some's enum tag from DataRepr so string matching keeps working even if
    // predef refactors reorder Option variants (unlikely, but cheap to robustly handle).
    private def optionSomeData(
        variantOf: (PackageName, Constructor) => Option[DataRepr]
    ): (Int, Int, List[Int]) =
      variantOf(PackageName.PredefName, someCons) match {
        case Some(DataRepr.Enum(variant, size, famArities)) if size == 1 =>
          (variant, size, famArities)
        // $COVERAGE-OFF$
        case Some(other) =>
          throw new IllegalStateException(
            s"expected Option.Some to be Enum(_, 1, _), found: $other"
          )
        case None =>
          throw new IllegalStateException(
            "could not find Bosatsu/Predef.Some in global data types"
          )
        // $COVERAGE-ON$
      }

    // Defensive guard: Tuple2 should always be Struct(2). If this ever changes,
    // fail loudly because it would be a very surprising predef shape change.
    private def tuple2Arity(
        variantOf: (PackageName, Constructor) => Option[DataRepr]
    ): Int =
      variantOf(PackageName.PredefName, tuple2Cons) match {
        case Some(DataRepr.Struct(arity)) if arity == 2 => arity
        // $COVERAGE-OFF$
        case Some(other) =>
          throw new IllegalStateException(
            s"expected Tuple2 to be Struct(2), found: $other"
          )
        case None =>
          throw new IllegalStateException(
            "could not find Bosatsu/Predef.Tuple2 in global data types"
          )
        // $COVERAGE-ON$
      }

    private def predefFn[A](from: A, name: Identifier.Name): Expr[A] =
      Global(from, PackageName.PredefName, name)

    private def call1[A](
        from: A,
        name: Identifier.Name,
        arg: Expr[A]
    ): Expr[A] =
      applyArgs(predefFn(from, name), NonEmptyList.one(arg))

    private def call2[A](
        from: A,
        name: Identifier.Name,
        arg0: Expr[A],
        arg1: Expr[A]
    ): Expr[A] =
      applyArgs(predefFn(from, name), NonEmptyList(arg0, arg1 :: Nil))

    private def concatString[A](from: A, items: List[Expr[A]]): Expr[A] =
      items match {
        case Nil       => emptyStringExpr
        case h :: Nil  => h
        case nonSingle =>
          call1(from, concatStringName, ListExpr.listOf(nonSingle))
      }

    private inline def withCheap[F[_]: Monad, A](
        inline expr: Expr[A],
        newConst: F[LocalAnon]
    )(
        inline fn: CheapExpr[A] => F[BoolExpr[A]]
    ): F[BoolExpr[A]] =
      expr match {
        case cheap: CheapExpr[A] =>
          fn(cheap)
        case _ =>
          newConst.flatMap { tmp =>
            fn(tmp).map(LetBool(Left(tmp), expr, _))
          }
      }

    private inline def withCheapExpr[F[_]: Monad, A](
        inline expr: Expr[A],
        newConst: F[LocalAnon]
    )(
        inline fn: CheapExpr[A] => F[Expr[A]]
    ): F[Expr[A]] =
      expr match {
        case cheap: CheapExpr[A] =>
          fn(cheap)
        case _ =>
          for {
            tmp <- newConst
            body <- fn(tmp)
          } yield Let(Left(tmp), expr, body)
      }

    private def withSomeTuple2[F[_]: Monad, A](
        optionExpr: Expr[A],
        ctx: Ctx[F, A]
    )(
        fn: (CheapExpr[A], CheapExpr[A]) => F[BoolExpr[A]]
    ): F[BoolExpr[A]] =
      withCheap(optionExpr, ctx.newConst) { opt =>
        val hasSome = ctx.optionIsSome(opt)
        ctx.newConst.flatMap { tuple =>
          val left = ctx.getFromTuple2(tuple, 0)
          val right = ctx.getFromTuple2(tuple, 1)
          fn(left, right).map { onSome =>
            hasSome && LetBool(Left(tuple), ctx.optionGetSome(opt), onSome)
          }
        }
      }

    private def withSomeTuple2Expr[F[_]: Monad, A](
        optionExpr: Expr[A],
        ctx: Ctx[F, A]
    )(
        onSome: (CheapExpr[A], CheapExpr[A]) => F[Expr[A]],
        onNone: => F[Expr[A]]
    ): F[Expr[A]] =
      withCheapExpr(optionExpr, ctx.newConst) { opt =>
        for {
          tuple <- ctx.newConst
          left = ctx.getFromTuple2(tuple, 0)
          right = ctx.getFromTuple2(tuple, 1)
          someExpr <- onSome(left, right)
          noneExpr <- onNone
        } yield {
          val someBody = Let(Left(tuple), ctx.optionGetSome(opt), someExpr)
          If(ctx.optionIsSome(opt), someBody, noneExpr)
        }
      }

    private type ExactStrPart = StrPart.CharPart | StrPart.LitStr

    final private case class StringSearchSegment(
        glob: StrPart.Glob,
        parts: NonEmptyList[ExactStrPart]
    )

    final private case class StringSearchPlan(
        segments: NonEmptyList[StringSearchSegment],
        trailingGlob: Option[StrPart.Glob]
    )

    private def bindAt(
        bindTargets: IndexedSeq[LocalAnonMut],
        idx: Int
    ): LocalAnonMut =
      if ((0 <= idx) && (idx < bindTargets.length)) bindTargets(idx)
      else {
        // $COVERAGE-OFF$
        throw new IllegalStateException(
          s"string match bind index out of bounds: idx=$idx, binds=${bindTargets.length}"
        )
        // $COVERAGE-ON$
      }

    private def setAll[A](
        ls: List[(LocalAnonMut, Expr[A])],
        ret: Expr[A]
    ): Expr[A] =
      ls.foldRight(ret) { case ((l, e), r) =>
        Always(SetMut(l, e), r)
      }

    private def exactBindCount(parts: NonEmptyList[ExactStrPart]): Int =
      parts.toList.count {
        case StrPart.IndexChar => true
        case _                 => false
      }

    private def buildStringSearchPlan(
        parts: List[StrPart]
    ): Option[StringSearchPlan] =
        parts match {
        case (glob: StrPart.Glob) :: tail =>
          var currentGlob: StrPart.Glob = glob
          var currentItemsRev: List[ExactStrPart] = Nil
          var segmentsRev: List[StringSearchSegment] = Nil

          tail.foreach {
            case nextGlob: StrPart.Glob =>
              val currentItems =
                NonEmptyList.fromList(currentItemsRev.reverse).getOrElse {
                  throw new IllegalStateException(
                    s"expected exact string items before trailing glob in string search pattern: $parts"
                  )
                }
              segmentsRev = StringSearchSegment(currentGlob, currentItems) :: segmentsRev
              currentGlob = nextGlob
              currentItemsRev = Nil
            case part: StrPart.CharPart =>
              currentItemsRev = part :: currentItemsRev
            case part: StrPart.LitStr =>
              currentItemsRev = part :: currentItemsRev
          }

          val trailingGlob =
            NonEmptyList.fromList(currentItemsRev.reverse) match {
              case Some(lastItems) =>
                segmentsRev = StringSearchSegment(currentGlob, lastItems) :: segmentsRev
                None
              case None            =>
                Some(currentGlob)
            }

          NonEmptyList
            .fromList(segmentsRev.reverse)
            .map(StringSearchPlan(_, trailingGlob))
            .filter(_.segments.tail.nonEmpty)
        case _ =>
          None
      }

    private def ifExactStringParts[F[_]: Monad, A](
        arg: CheapExpr[A],
        parts: List[ExactStrPart],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        ctx: Ctx[F, A]
    )(
        onMatch: CheapExpr[A] => F[Expr[A]],
        onMiss: => F[Expr[A]]
    ): F[Expr[A]] =
      parts match {
        case Nil =>
          onMatch(arg)
        case StrPart.LitStr(expect) :: tail =>
          withSomeTuple2Expr(
            call2(ctx.from, partitionStringName, arg, Literal(Lit.Str(expect))),
            ctx
          )(
            (left, right) =>
              for {
                missExpr <- onMiss
                matchedExpr <- ifExactStringParts(right, tail, bindTargets, nextBind, ctx)(
                  onMatch,
                  onMiss
                )
              } yield If(CompareLit(left, CompareRel.Eq, emptyStringLit), matchedExpr, missExpr),
            onMiss
          )
        case (charPart: StrPart.CharPart) :: tail =>
          withSomeTuple2Expr(call1(ctx.from, unconsStringName, arg), ctx)(
            (head, rest) => {
              val nextBind1 =
                if (charPart.capture) nextBind + 1 else nextBind
              ifExactStringParts(rest, tail, bindTargets, nextBind1, ctx)(
                onMatch,
                onMiss
              ).map { matchedExpr =>
                if (charPart.capture) Always(SetMut(bindAt(bindTargets, nextBind), head), matchedExpr)
                else matchedExpr
              }
            },
            onMiss
          )
      }

    private def advanceCurrentByOneChar[F[_]: Monad, A](
        current: LocalAnonMut,
        prefixAcc: Option[LocalAnonMut],
        ctx: Ctx[F, A]
    ): F[Expr[A]] =
      (ctx.newConst, ctx.newConst).mapN { (splitTmp, tupleTmp) =>
        val splitCall = call1(ctx.from, unconsStringName, current)
        val hasSome = ctx.optionIsSome(splitTmp)
        val head = ctx.getFromTuple2(tupleTmp, 0)
        val tail = ctx.getFromTuple2(tupleTmp, 1)
        val headStr = call1(ctx.from, charToStringName, head)
        val updates =
          (current, tail) :: prefixAcc.toList.map { p =>
            (p, concatString(ctx.from, p :: headStr :: Nil))
          }
        val onSome =
          Let(
            Left(tupleTmp),
            ctx.optionGetSome(splitTmp),
            setAll(updates, UnitExpr)
          )
        val onNone =
          setAll((current, emptyStringExpr) :: Nil, UnitExpr)

        Let(Left(splitTmp), splitCall, If(hasSome, onSome, onNone))
      }

    final private case class StringSearchLoopState(
        runMut: LocalAnonMut,
        resMut: LocalAnonMut
    )

    private def matchedStringSearchExpr[A](
        state: StringSearchLoopState,
        updates: List[(LocalAnonMut, Expr[A])]
    ): Expr[A] =
      setAll(
        (state.runMut, FalseExpr) ::
          (state.resMut, TrueExpr) ::
          updates,
        UnitExpr
      )

    private def stopStringSearchExpr[A](
        state: StringSearchLoopState
    ): Expr[A] =
      setAll((state.runMut, FalseExpr) :: Nil, UnitExpr)

    private def bindGlob[A](
        glob: StrPart.Glob,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        value: Expr[A]
    ): List[(LocalAnonMut, Expr[A])] =
      if (glob.capture) (bindAt(bindTargets, nextBind), value) :: Nil
      else Nil

    private def compilePureStringSearchPlan[F[_]: Monad, A](
        arg: CheapExpr[A],
        plan: StringSearchPlan,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] =
      compileStringSearchPlan(
        arg,
        plan,
        bindTargets,
        nextBind,
        Monad[F].pure(TrueConst),
        ctx
      )

    private def compileLeadingStringSearchSegmentWithChars[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        restPlan: StringSearchPlan,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      val segmentBindStart =
        if (segment.glob.capture) nextBind + 1 else nextBind
      val afterSegmentBinds = segmentBindStart + exactBindCount(segment.parts)

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        prefixMutOpt <- ctx.newMutOpt(segment.glob.capture)
        loopRes <- ctx.newConst
        state = StringSearchLoopState(runMut, resMut)
        advance <- advanceCurrentByOneChar(currentMut, prefixMutOpt, ctx)
        effect <- ifExactStringParts(
          currentMut,
          segment.parts.toList,
          bindTargets,
          segmentBindStart,
          ctx
        )(
          right =>
            for {
              pureCheck <- compilePureStringSearchPlan(
                right,
                restPlan,
                bindTargets,
                afterSegmentBinds,
                ctx
              )
              successCheck <- onPureMatch
            } yield {
              val matched =
                matchedStringSearchExpr(
                  state,
                  prefixMutOpt.toList.map { prefix =>
                    (bindAt(bindTargets, nextBind), prefix: Expr[A])
                  }
                )
              If(
                pureCheck,
                If(successCheck, matched, advance),
                stopStringSearchExpr(state)
              )
            },
          Monad[F].pure(advance)
        )
      } yield {
        val loopEffect =
          If(
            CompareLit(currentMut, CompareRel.Eq, emptyStringLit),
            stopStringSearchExpr(state),
            effect
          )
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            prefixMutOpt.toList.map { prefix =>
              (prefix, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), loopEffect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: prefixMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(loopRes))
        )
      }
    }

    private def compileLeadingStringSearchSegmentWithLiteral[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        restPlan: StringSearchPlan,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      val expect =
        segment.parts.head match {
          case StrPart.LitStr(s) => s
          case _                 =>
            throw new IllegalStateException(
              s"expected literal-led string segment: ${segment.parts}"
            )
        }
      val exactTail = segment.parts.tail
      val segmentBindStart =
        if (segment.glob.capture) nextBind + 1 else nextBind
      val afterSegmentBinds = segmentBindStart + exactBindCount(segment.parts)
      val splitIdx = expect.offsetByCodePoints(0, 1)
      val expectHeadExpr = Literal(Lit.Str(expect.substring(0, splitIdx)))
      val expectTailExprOpt =
        if (splitIdx < expect.length)
          Some(Literal(Lit.Str(expect.substring(splitIdx))))
        else None

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        consumedMutOpt <- ctx.newMutOpt(segment.glob.capture)
        loopRes <- ctx.newConst
        state = StringSearchLoopState(runMut, resMut)
        effect <- withSomeTuple2Expr(
          call2(ctx.from, partitionStringName, currentMut, Literal(Lit.Str(expect))),
          ctx
        )(
          (left, right) => {
            val currentAdvance: Expr[A] =
              expectTailExprOpt match {
                case Some(tailExpr) =>
                  concatString(ctx.from, tailExpr :: right :: Nil)
                case None           =>
                  right
              }
            val onCandidateMiss =
              setAll(
                (currentMut, currentAdvance) ::
                  consumedMutOpt.toList.map { consumed =>
                    val consumed1 =
                      concatString(ctx.from, consumed :: left :: expectHeadExpr :: Nil)
                    (consumed, consumed1)
                  },
                UnitExpr
              )

            ifExactStringParts(right, exactTail, bindTargets, segmentBindStart, ctx)(
              remainder =>
                for {
                  pureCheck <- compilePureStringSearchPlan(
                    remainder,
                    restPlan,
                    bindTargets,
                    afterSegmentBinds,
                    ctx
                  )
                  successCheck <- onPureMatch
                } yield {
                  val prefixUpdates =
                    consumedMutOpt.toList.map { consumed =>
                      val prefix = concatString(ctx.from, consumed :: left :: Nil)
                      (bindAt(bindTargets, nextBind), prefix)
                    }
                  val matched =
                    matchedStringSearchExpr(state, prefixUpdates)
                  If(
                    pureCheck,
                    If(successCheck, matched, onCandidateMiss),
                    stopStringSearchExpr(state)
                  )
                },
              Monad[F].pure(onCandidateMiss)
            )
          },
          Monad[F].pure(stopStringSearchExpr(state))
        )
      } yield {
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            consumedMutOpt.toList.map { consumed =>
              (consumed, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), effect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: consumedMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(loopRes))
        )
      }
    }

    private def compileLeadingStringSearchSegment[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        restPlan: StringSearchPlan,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] =
      segment.parts.head match {
        case _: StrPart.CharPart =>
          compileLeadingStringSearchSegmentWithChars(
            arg,
            segment,
            restPlan,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
        case StrPart.LitStr(_)   =>
          compileLeadingStringSearchSegmentWithLiteral(
            arg,
            segment,
            restPlan,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
      }

    private def compileFinalStringSearchSegmentWithChars[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        trailingGlob: Option[StrPart.Glob],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      val segmentBindStart =
        if (segment.glob.capture) nextBind + 1 else nextBind
      val afterSegmentBinds = segmentBindStart + exactBindCount(segment.parts)

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        prefixMutOpt <- ctx.newMutOpt(segment.glob.capture)
        loopRes <- ctx.newConst
        state = StringSearchLoopState(runMut, resMut)
        advance <- advanceCurrentByOneChar(currentMut, prefixMutOpt, ctx)
        effect <- ifExactStringParts(
          currentMut,
          segment.parts.toList,
          bindTargets,
          segmentBindStart,
          ctx
        )(
          remainder =>
            onPureMatch.map { successCheck =>
              val matchUpdates =
                prefixMutOpt.toList.map { prefix =>
                  (bindAt(bindTargets, nextBind), prefix: Expr[A])
                } ::: trailingGlob.toList.flatMap { glob =>
                  bindGlob(glob, bindTargets, afterSegmentBinds, remainder)
                }
              val matched = matchedStringSearchExpr(state, matchUpdates)
              trailingGlob match {
                case Some(_) =>
                  If(successCheck, matched, advance)
                case None    =>
                  If(
                    CompareLit(remainder, CompareRel.Eq, emptyStringLit),
                    If(successCheck, matched, advance),
                    advance
                  )
              }
            },
          Monad[F].pure(advance)
        )
      } yield {
        val loopEffect =
          If(
            CompareLit(currentMut, CompareRel.Eq, emptyStringLit),
            stopStringSearchExpr(state),
            effect
          )
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            prefixMutOpt.toList.map { prefix =>
              (prefix, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), loopEffect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: prefixMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(loopRes))
        )
      }
    }

    private def compileFinalStringSearchSegmentWithLiteral[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        trailingGlob: Option[StrPart.Glob],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      val expect =
        segment.parts.head match {
          case StrPart.LitStr(s) => s
          case _                 =>
            throw new IllegalStateException(
              s"expected literal-led string segment: ${segment.parts}"
            )
        }
      val exactTail = segment.parts.tail
      val segmentBindStart =
        if (segment.glob.capture) nextBind + 1 else nextBind
      val afterSegmentBinds = segmentBindStart + exactBindCount(segment.parts)
      val splitIdx = expect.offsetByCodePoints(0, 1)
      val expectHeadExpr = Literal(Lit.Str(expect.substring(0, splitIdx)))
      val expectTailExprOpt =
        if (splitIdx < expect.length)
          Some(Literal(Lit.Str(expect.substring(splitIdx))))
        else None

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        consumedMutOpt <- ctx.newMutOpt(segment.glob.capture)
        loopRes <- ctx.newConst
        state = StringSearchLoopState(runMut, resMut)
        effect <- withSomeTuple2Expr(
          call2(ctx.from, partitionStringName, currentMut, Literal(Lit.Str(expect))),
          ctx
        )(
          (left, right) => {
            val currentAdvance: Expr[A] =
              expectTailExprOpt match {
                case Some(tailExpr) =>
                  concatString(ctx.from, tailExpr :: right :: Nil)
                case None           =>
                  right
              }
            val onCandidateMiss =
              setAll(
                (currentMut, currentAdvance) ::
                  consumedMutOpt.toList.map { consumed =>
                    val consumed1 =
                      concatString(ctx.from, consumed :: left :: expectHeadExpr :: Nil)
                    (consumed, consumed1)
                  },
                UnitExpr
              )

            ifExactStringParts(right, exactTail, bindTargets, segmentBindStart, ctx)(
              remainder =>
                onPureMatch.map { successCheck =>
                  val prefixUpdates =
                    consumedMutOpt.toList.map { consumed =>
                      val prefix = concatString(ctx.from, consumed :: left :: Nil)
                      (bindAt(bindTargets, nextBind), prefix)
                    }
                  val trailingUpdates =
                    trailingGlob.toList.flatMap { glob =>
                      bindGlob(glob, bindTargets, afterSegmentBinds, remainder)
                    }
                  val matched =
                    matchedStringSearchExpr(state, prefixUpdates ::: trailingUpdates)

                  trailingGlob match {
                    case Some(_) =>
                      If(successCheck, matched, onCandidateMiss)
                    case None    =>
                      If(
                        CompareLit(remainder, CompareRel.Eq, emptyStringLit),
                        If(successCheck, matched, onCandidateMiss),
                        onCandidateMiss
                      )
                  }
                },
              Monad[F].pure(onCandidateMiss)
            )
          },
          Monad[F].pure(stopStringSearchExpr(state))
        )
      } yield {
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            consumedMutOpt.toList.map { consumed =>
              (consumed, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), effect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: consumedMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(loopRes))
        )
      }
    }

    private def compileFinalStringSearchSegment[F[_]: Monad, A](
        arg: CheapExpr[A],
        segment: StringSearchSegment,
        trailingGlob: Option[StrPart.Glob],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] =
      segment.parts.head match {
        case _: StrPart.CharPart =>
          compileFinalStringSearchSegmentWithChars(
            arg,
            segment,
            trailingGlob,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
        case StrPart.LitStr(_)   =>
          compileFinalStringSearchSegmentWithLiteral(
            arg,
            segment,
            trailingGlob,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
      }

    private def compileStringSearchPlan[F[_]: Monad, A](
        arg: CheapExpr[A],
        plan: StringSearchPlan,
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        onPureMatch: F[BoolExpr[A]],
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] =
      plan.segments match {
        case NonEmptyList(segment, Nil) =>
          compileFinalStringSearchSegment(
            arg,
            segment,
            plan.trailingGlob,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
        case NonEmptyList(segment, next :: tail) =>
          compileLeadingStringSearchSegment(
            arg,
            segment,
            StringSearchPlan(NonEmptyList(next, tail), plan.trailingGlob),
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
      }

    private def searchStringWithCharRest[F[_]: Monad, A](
        arg: CheapExpr[A],
        rest: List[StrPart],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        capture: Boolean,
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      val restBind = if (capture) nextBind + 1 else nextBind

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        prefixMutOpt <- ctx.newMutOpt(capture)
        loopRes <- ctx.newConst
        check <- matchStringParts(currentMut, rest, bindTargets, restBind, ctx)
        advance <- advanceCurrentByOneChar(currentMut, prefixMutOpt, ctx)
      } yield {
        val onEmpty = setAll((runMut, FalseExpr) :: Nil, UnitExpr)
        val onMatch = {
          val updates =
            (runMut, FalseExpr) ::
              (resMut, TrueExpr) ::
              prefixMutOpt.toList.map { prefix =>
                (bindAt(bindTargets, nextBind), prefix: Expr[A])
              }
          setAll(updates, UnitExpr)
        }

        val effect =
          If(
            CompareLit(currentMut, CompareRel.Eq, emptyStringLit),
            onEmpty,
            If(check, onMatch, advance)
          )
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            prefixMutOpt.toList.map { prefix =>
              (prefix, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), effect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: prefixMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(resMut))
        )
      }
    }

    private def searchStringWithLiteralRest[F[_]: Monad, A](
        arg: CheapExpr[A],
        expect: String,
        tail2: List[StrPart],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        capture: Boolean,
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] = {
      if (!capture && (tail2 == (StrPart.WildStr :: Nil))) {
        withCheap(
          call2(ctx.from, partitionStringName, arg, Literal(Lit.Str(expect))),
          ctx.newConst
        ) { part =>
          Monad[F].pure(ctx.optionIsSome(part))
        }
      } else {
      val splitIdx = expect.offsetByCodePoints(0, 1)
      val expectHead = expect.substring(0, splitIdx)
      val expectTail = expect.substring(splitIdx)
      val expectLit = Literal(Lit.Str(expect))
      val expectHeadExpr = Literal(Lit.Str(expectHead))
      val expectTailExprOpt =
        if (expectTail.isEmpty) None
        else Some(Literal(Lit.Str(expectTail)))

      val restBind = if (capture) nextBind + 1 else nextBind

      for {
        runMut <- ctx.newMut
        resMut <- ctx.newMut
        currentMut <- ctx.newMut
        consumedMutOpt <- ctx.newMutOpt(capture)
        loopRes <- ctx.newConst
        partTmp <- ctx.newConst
        tupleTmp <- ctx.newConst
        check <- matchStringParts(
          ctx.getFromTuple2(tupleTmp, 1),
          tail2,
          bindTargets,
          restBind,
          ctx
        )
      } yield {
        val left = ctx.getFromTuple2(tupleTmp, 0)
        val right = ctx.getFromTuple2(tupleTmp, 1)
        val onNoCandidate = setAll((runMut, FalseExpr) :: Nil, UnitExpr)
        val currentAdvance: Expr[A] =
          expectTailExprOpt match {
            case None           => right
            case Some(tailExpr) =>
              concatString(ctx.from, tailExpr :: right :: Nil)
          }
        val onCandidateMiss = {
          val updates =
            (currentMut, currentAdvance) ::
              consumedMutOpt.toList.map { consumed =>
                val consumed1 =
                  concatString(
                    ctx.from,
                    consumed :: left :: expectHeadExpr :: Nil
                  )
                (consumed, consumed1)
              }
          setAll(updates, UnitExpr)
        }
        val onCandidateHit = {
          val updates =
            (runMut, FalseExpr) ::
              (resMut, TrueExpr) ::
              consumedMutOpt.toList.map { consumed =>
                val prefix = concatString(ctx.from, consumed :: left :: Nil)
                (bindAt(bindTargets, nextBind), prefix)
              }
          setAll(updates, UnitExpr)
        }
        val onSome = {
          val body = If(check, onCandidateHit, onCandidateMiss)
          Let(Left(tupleTmp), ctx.optionGetSome(partTmp), body)
        }
        val partCall =
          call2(ctx.from, partitionStringName, currentMut, expectLit)
        val stepExpr =
          Let(
            Left(partTmp),
            partCall,
            If(ctx.optionIsSome(partTmp), onSome, onNoCandidate)
          )
        val onEmpty = setAll((runMut, FalseExpr) :: Nil, UnitExpr)
        val effect =
          If(
            CompareLit(currentMut, CompareRel.Eq, emptyStringLit),
            onEmpty,
            stepExpr
          )
        val initSets =
          (runMut, TrueExpr) ::
            (resMut, FalseExpr) ::
            (currentMut, arg) ::
            consumedMutOpt.toList.map { consumed =>
              (consumed, emptyStringExpr)
            }
        val searchLoop =
          setAll(initSets, WhileExpr(isTrueExpr(runMut), effect, resMut))

        LetMutBool(
          runMut :: resMut :: currentMut :: consumedMutOpt.toList,
          LetBool(Left(loopRes), searchLoop, isTrueExpr(resMut))
        )
      }
      }
    }

    private def matchStringParts[F[_]: Monad, A](
        arg: CheapExpr[A],
        parts: List[StrPart],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        ctx: Ctx[F, A]
    ): F[BoolExpr[A]] =
      parts match {
        case Nil =>
          Monad[F].pure(CompareLit(arg, CompareRel.Eq, emptyStringLit))
        case StrPart.LitStr(expect) :: tail =>
          withSomeTuple2(
            call2(ctx.from, partitionStringName, arg, Literal(Lit.Str(expect))),
            ctx
          ) { (left, right) =>
            matchStringParts(right, tail, bindTargets, nextBind, ctx).map {
              rest =>
                CompareLit(left, CompareRel.Eq, emptyStringLit) && rest
            }
          }
        case (charPart: StrPart.CharPart) :: tail =>
          val nextBindIdx =
            if (charPart.capture) nextBind + 1 else nextBind
          if (!charPart.capture && ctx.mustMatch) {
            withCheap(
              call1(ctx.from, tailOrEmptyStringName, arg),
              ctx.newConst
            ) { rest =>
              matchStringParts(rest, tail, bindTargets, nextBindIdx, ctx)
            }
          } else {
            withSomeTuple2(call1(ctx.from, unconsStringName, arg), ctx) {
              (head, rest) =>
                matchStringParts(rest, tail, bindTargets, nextBindIdx, ctx).map {
                  next =>
                    if (charPart.capture)
                      SetMut(bindAt(bindTargets, nextBind), head) && next
                    else next
                }
            }
          }
        case (glob: StrPart.Glob) :: tail =>
          tail match {
            case Nil =>
              if (glob.capture)
                Monad[F].pure(SetMut(bindAt(bindTargets, nextBind), arg))
              else Monad[F].pure(TrueConst)
            case (_: StrPart.CharPart) :: _ =>
              searchStringWithCharRest(
                arg,
                tail,
                bindTargets,
                nextBind,
                glob.capture,
                ctx
              )
            case StrPart.LitStr(expect) :: tail2 =>
              searchStringWithLiteralRest(
                arg,
                expect,
                tail2,
                bindTargets,
                nextBind,
                glob.capture,
                ctx
              )
            case (_: StrPart.Glob) :: _ =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"invariant violation, adjacent string globs: $parts"
              )
            // $COVERAGE-ON$
          }
      }

    def apply[F[_]: Monad, A](
        arg: CheapExpr[A],
        parts: List[StrPart],
        bindTargets: IndexedSeq[LocalAnonMut],
        nextBind: Int,
        mustMatch: Boolean,
        onPureMatch: F[BoolExpr[A]],
        makeAnon: F[Long],
        from: A,
        variantOf: (PackageName, Constructor) => Option[DataRepr]
    ): F[BoolExpr[A]] = {
      val compactPat = StrPart.compact(parts)
      val captureCount =
        compactPat.count {
          case StrPart.IndexStr | StrPart.IndexChar => true
          case _                                    => false
        }

      if (captureCount + nextBind != bindTargets.length) {
        // $COVERAGE-OFF$
        throw new IllegalStateException(
          s"string capture count mismatch: parts=$compactPat captures=$captureCount binds=${bindTargets.length}, nextBind=$nextBind"
        )
        // $COVERAGE-ON$
      }

      val ctx = Ctx(
        from = from,
        mustMatch = mustMatch,
        optionSomeData = optionSomeData(variantOf),
        tuple2Arity = tuple2Arity(variantOf),
        newMut = makeAnon.map(LocalAnonMut(_)),
        newConst = makeAnon.map(LocalAnon(_))
      )
      buildStringSearchPlan(compactPat) match {
        case Some(plan) =>
          compileStringSearchPlan(
            arg,
            plan,
            bindTargets,
            nextBind,
            onPureMatch,
            ctx
          )
        case None       =>
          for {
            pureCheck <- matchStringParts(arg, compactPat, bindTargets, nextBind, ctx)
            successCheck <- onPureMatch
          } yield pureCheck && successCheck
      }
    }
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

  private def postLoweringCleanupRound[A: Order](
      expr: Expr[A],
      localPassOptions: LocalPassOptions
  ): Expr[A] = {
    val hoisted =
      if (localPassOptions.enables(LocalPass.HoistInvariantLoopLets))
        hoistInvariantLoopLets(expr)
      else
        expr
    val reused =
      if (localPassOptions.enables(LocalPass.ReuseConstructors))
        reuseConstructors(hoisted)
      else
        hoisted
    val branchSunk =
      sinkBranchOnlyLets(reused)

    simplifyKnownConditions(branchSunk)
  }

  private[bosatsu] def postLoweringCleanup[A: Order](
      expr: Expr[A],
      localPassOptions: LocalPassOptions
  ): Expr[A] = {
    val structuralOrder = summon[Order[Expr[A]]]
    val cleanupOrder: Order[Expr[A]] =
      Order.by((expr: Expr[A]) => (exprWeight(expr), expr))

    def canonical(expr: Expr[A]): Expr[A] =
      StackSafe.onStackOverflow(refreshAnonBinders(expr))(expr)

    def bestExpr(seen: NonEmptyList[Expr[A]]): Expr[A] =
      seen.tail.foldLeft(seen.head) { (best, next) =>
        if (cleanupOrder.lteqv(best, next)) best else next
      }

    def structurallyEqual(left: Expr[A], right: Expr[A]): Boolean =
      StackSafe.onStackOverflow(structuralOrder.eqv(left, right))(false)

    @annotation.tailrec
    def loop(
        current: Expr[A],
        roundsLeft: Int,
        seen: List[Expr[A]]
    ): Expr[A] =
      if (roundsLeft <= 0) current
      else {
        val next = canonical(postLoweringCleanupRound(current, localPassOptions))
        if (structurallyEqual(next, current)) current
        else {
          if (seen.exists(prev => structurallyEqual(prev, next)))
            bestExpr(NonEmptyList(next, seen))
          else loop(next, roundsLeft - 1, next :: seen)
        }
      }

    val start = canonical(expr)
    loop(start, PostLoweringCleanupMaxRounds, start :: Nil)
  }

  // Allocate anonymous ids with RefSpace, then run the shared post-lowering
  // cleanup pipeline over the raw Matchless tree.
  def fromLet[A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A]
  )(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr[B] =
    fromLet(
      from,
      name,
      rec,
      te,
      LocalPassOptions.Default
    )(variantOf)

  private[bosatsu] def fromLet[A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      localPassOptions: LocalPassOptions
  )(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr[B] =
    postLoweringCleanup((for {
      c <- RefSpace.allocCounter
      expr <- fromLetRaw(from, name, rec, te, variantOf, c)
    } yield expr).run.value, localPassOptions)

  def fromLetRaw[A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A]
  )(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr[B] =
    (for {
      c <- RefSpace.allocCounter
      expr <- fromLetRaw(from, name, rec, te, variantOf, c)
    } yield expr).run.value

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLet[F[_]: Monad, A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long]
  ): F[Expr[B]] =
    fromLet(
      from,
      name,
      rec,
      te,
      variantOf,
      makeAnon,
      LocalPassOptions.Default
    )

  // we need a TypeEnv to inline the creation of structs and variants
  private[bosatsu] def fromLet[F[_]: Monad, A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long],
      localPassOptions: LocalPassOptions
  ): F[Expr[B]] =
    fromLetRaw(from, name, rec, te, variantOf, makeAnon)
      .map(postLoweringCleanup(_, localPassOptions))

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLetRaw[F[_]: Monad, A, B: Order](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long]
  ): F[Expr[B]] = {

    type UnionMatch =
      NonEmptyList[(List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])]
    type CandidateGuard = List[(Bindable, Expr[B])] => F[BoolExpr[B]]

    val wildMatch: UnionMatch = NonEmptyList((Nil, TrueConst, Nil), Nil)
    def isWildMatch(um: UnionMatch): Boolean =
      um match {
        case NonEmptyList((Nil, TrueConst, Nil), Nil) => true
        case _                                        => false
      }

    def retainReferencedBinds(
        in: Expr[B],
        binds: List[(Bindable, Expr[B])]
    ): List[(Bindable, Expr[B])] =
      binds.filter { case (bindable, _) =>
        Expr.referencesBindable(in, bindable)
      }

    def applyCandidateGuard(
        cond: BoolExpr[B],
        binds: List[(Bindable, Expr[B])],
        candidateGuard: CandidateGuard
    ): F[BoolExpr[B]] =
      candidateGuard(binds).map(cond && _)

    def applyOptionalCandidateGuard(
        cond: BoolExpr[B],
        binds: List[(Bindable, Expr[B])],
        candidateGuard: Option[CandidateGuard]
    ): F[BoolExpr[B]] =
      candidateGuard match {
        case Some(guardFn) =>
          applyCandidateGuard(cond, binds, guardFn)
        case None          =>
          Monad[F].pure(cond)
      }

    def applyCandidateGuardToUnionMatch(
        um: UnionMatch,
        candidateGuard: Option[CandidateGuard]
    ): F[UnionMatch] =
      candidateGuard match {
        case Some(guardFn) =>
          um.traverse { case (preLets, cond, binds) =>
            applyCandidateGuard(cond, binds, guardFn).map { guardedCond =>
              (preLets, guardedCond, binds)
            }
          }
        case None =>
          Monad[F].pure(um)
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

    enum BuiltinCompareDomain derives CanEqual {
      case IntDomain, Int64Domain, Float64Domain
    }

    val int64PackageName = PackageName.parts("Bosatsu", "Num", "Int64")
    val eqIntName = Identifier.Name("eq_Int")
    val cmpIntName = Identifier.Name("cmp_Int")
    val eqFloat64Name = Identifier.Name("eq_Float64")
    val cmpFloat64Name = Identifier.Name("cmp_Float64")
    val eqInt64Name = Identifier.Name("eq_Int64")
    val cmpInt64Name = Identifier.Name("cmp_Int64")
    val intToInt64Name = Identifier.Name("int_to_Int64")
    val intLowBitsToInt64Name = Identifier.Name("int_low_bits_to_Int64")
    val someConstructor = Constructor("Some")
    val noneConstructor = Constructor("None")

    lazy val optionSomeData: (Int, Int, List[Int]) =
      variantOf(PackageName.PredefName, someConstructor) match {
        case Some(DataRepr.Enum(variant, arity, famArities)) =>
          (variant, arity, famArities)
        case other =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected Bosatsu/Predef.Some to be an enum, found: $other"
          )
        // $COVERAGE-ON$
      }

    lazy val optionNoneExpr: Expr[B] =
      variantOf(PackageName.PredefName, noneConstructor) match {
        case Some(DataRepr.Enum(variant, arity, famArities)) =>
          MakeEnum(variant, arity, famArities)
        case other =>
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"expected Bosatsu/Predef.None to be an enum, found: $other"
          )
        // $COVERAGE-ON$
      }

    def optionSomeExpr(value: Expr[B]): Expr[B] = {
      val (variant, arity, famArities) = optionSomeData
      applyArgs(MakeEnum(variant, arity, famArities), NonEmptyList.one(value))
    }

    def eqBuiltinDomain(
        pack: PackageName,
        fnName: Bindable
    ): Option[BuiltinCompareDomain] =
      if ((pack == PackageName.PredefName) && (fnName == eqIntName))
        Some(BuiltinCompareDomain.IntDomain)
      else if ((pack == PackageName.PredefName) && (fnName == eqFloat64Name))
        Some(BuiltinCompareDomain.Float64Domain)
      else if ((pack == int64PackageName) && (fnName == eqInt64Name))
        Some(BuiltinCompareDomain.Int64Domain)
      else None

    def cmpBuiltinDomain(
        pack: PackageName,
        fnName: Bindable
    ): Option[BuiltinCompareDomain] =
      if ((pack == PackageName.PredefName) && (fnName == cmpIntName))
        Some(BuiltinCompareDomain.IntDomain)
      else if ((pack == PackageName.PredefName) && (fnName == cmpFloat64Name))
        Some(BuiltinCompareDomain.Float64Domain)
      else if ((pack == int64PackageName) && (fnName == cmpInt64Name))
        Some(BuiltinCompareDomain.Int64Domain)
      else None

    def comparePredicate(
        domain: BuiltinCompareDomain,
        left: CheapExpr[B],
        rel: CompareRel,
        right: CheapExpr[B]
    ): BoolExpr[B] =
      domain match {
        case BuiltinCompareDomain.IntDomain =>
          CompareInt(left, rel, right)
        case BuiltinCompareDomain.Int64Domain =>
          CompareInt64(left, rel, right)
        case BuiltinCompareDomain.Float64Domain =>
          CompareFloat64(left, rel, right)
      }

    def memoizeBinaryExpr(
        left: Expr[B],
        right: Expr[B]
    )(
        build: (CheapExpr[B], CheapExpr[B]) => Expr[B]
    ): F[Expr[B]] =
      maybeMemoWith(left, makeAnon) { leftCheap =>
        maybeMemoWith(right, makeAnon) { rightCheap =>
          Monad[F].pure(build(leftCheap, rightCheap))
        } { (rightTmp, rightExpr, body) =>
          Let(rightTmp, rightExpr, body)
        }
      } { (leftTmp, leftExpr, body) =>
        Let(leftTmp, leftExpr, body)
      }

    def lowerBooleanCompare(
        domain: BuiltinCompareDomain,
        left: Expr[B],
        rel: CompareRel,
        right: Expr[B]
    ): F[Expr[B]] =
      memoizeBinaryExpr(left, right) { (leftCheap, rightCheap) =>
        If(comparePredicate(domain, leftCheap, rel, rightCheap), TrueExpr, FalseExpr)
      }

    def lowerConstantBoolWithOperands(
        left: Expr[B],
        right: Expr[B],
        value: Boolean
    ): F[Expr[B]] =
      memoizeBinaryExpr(left, right) { (_, _) =>
        if (value) TrueExpr else FalseExpr
      }

    val comparisonFamArities = 0 :: 0 :: 0 :: Nil
    val comparisonTrueVariants = Set(0, 1, 2)

    def comparisonObservation(
        trueVariants: Set[Int]
    ): Either[Boolean, CompareRel] =
      if (trueVariants.isEmpty) Left(false)
      else if (trueVariants == Set(0)) Right(CompareRel.Lt)
      else if (trueVariants == Set(0, 1)) Right(CompareRel.Lte)
      else if (trueVariants == Set(1)) Right(CompareRel.Eq)
      else if (trueVariants == Set(1, 2)) Right(CompareRel.Gte)
      else if (trueVariants == Set(2)) Right(CompareRel.Gt)
      else if (trueVariants == Set(0, 2)) Right(CompareRel.Ne)
      else if (trueVariants == comparisonTrueVariants) Left(true)
      else {
        // total boolean matches over Comparison only have these subsets
        // $COVERAGE-OFF$
        throw new IllegalStateException(
          s"unexpected Comparison observation: $trueVariants"
        )
        // $COVERAGE-ON$
      }

    def comparisonObservationFor(
        selector: CheapExpr[B],
        expr: Expr[B]
    ): Option[Either[Boolean, CompareRel]] =
      collectTrueVariants(
        expr,
        comparisonTrueVariants,
        (selector, 0, comparisonFamArities)
      ).map(comparisonObservation)

    def cmpBuiltinArgs(
        expr: Expr[B]
    ): Option[(BuiltinCompareDomain, Expr[B], Expr[B])] =
      expr match {
        case App(Global(_, pack, fnName), NonEmptyList(left, right :: Nil)) =>
          cmpBuiltinDomain(pack, fnName).map((_, left, right))
        case _ =>
          None
      }

    def maybeLowerComparisonObservationLet(
        arg: Either[LocalAnon, Bindable],
        value: Expr[B],
        in: Expr[B]
    ): Option[F[Expr[B]]] = {
      val selector: CheapExpr[B] =
        arg match {
          case Left(localAnon) => localAnon
          case Right(name)     => Local(name)
        }

      for {
        (domain, left, right) <- cmpBuiltinArgs(value)
        observation <- comparisonObservationFor(selector, in)
      } yield {
        observation match {
          case Left(boolValue) =>
            lowerConstantBoolWithOperands(left, right, boolValue)
          case Right(rel)      =>
            lowerBooleanCompare(domain, left, rel, right)
        }
      }
    }

    def maybeLiteralIntValue(expr: TypedExpr[A]): Option[java.math.BigInteger] =
      expr match {
        case TypedExpr.Literal(Lit.Integer(value), _, _) =>
          Some(value)
        case _ =>
          None
      }

    def maybeLiteralInt64App(
        fn: TypedExpr[A],
        args: List[TypedExpr[A]]
    ): Option[Expr[B]] =
      (fn, args) match {
        case (
              TypedExpr.Global(pack, fnName, _, _),
              literalArg :: Nil
            ) if (pack == int64PackageName) && (fnName == intLowBitsToInt64Name) =>
          maybeLiteralIntValue(literalArg).map(value => LitInt64(value.longValue))

        case (
              TypedExpr.Global(pack, fnName, _, _),
              literalArg :: Nil
            ) if (pack == int64PackageName) && (fnName == intToInt64Name) =>
          maybeLiteralIntValue(literalArg).map { value =>
            try optionSomeExpr(LitInt64(value.longValueExact()))
            catch {
              case _: ArithmeticException => optionNoneExpr
            }
          }

        case _ =>
          None
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
        case CompareLit(x, rel, l) =>
          CompareLit(substituteLocalsCheap(m, x), rel, l)
        case CompareInt(left, rel, right) =>
          CompareInt(
            substituteLocalsCheap(m, left),
            rel,
            substituteLocalsCheap(m, right)
          )
        case CompareInt64(left, rel, right) =>
          CompareInt64(
            substituteLocalsCheap(m, left),
            rel,
            substituteLocalsCheap(m, right)
          )
        case CompareFloat64(left, rel, right) =>
          CompareFloat64(
            substituteLocalsCheap(m, left),
            rel,
            substituteLocalsCheap(m, right)
          )
        case EqualsNat(x, n) =>
          EqualsNat(substituteLocalsCheap(m, x), n)
        case TrueConst                           => TrueConst
        case CheckVariant(expr, expect, sz, fam) =>
          CheckVariant(substituteLocalsCheap(m, expr), expect, sz, fam)
        case CheckVariantSet(expr, expect, sz, fam) =>
          CheckVariantSet(substituteLocalsCheap(m, expr), expect, sz, fam)
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
          applyArgs(
            substituteLocals(m, fn),
            appArgs.map(substituteLocals(m, _))
          )
        case If(c, tcase, fcase) =>
          If(
            substituteLocalsBool(m, c),
            substituteLocals(m, tcase),
            substituteLocals(m, fcase)
          )
        case SwitchVariant(on, famArities, cases, default) =>
          SwitchVariant(
            substituteLocalsCheap(m, on),
            famArities,
            cases.map { case (variant, branch) =>
              (variant, substituteLocals(m, branch))
            },
            default.map(substituteLocals(m, _))
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
          // Captures are evaluated at lambda creation site, so they see the
          // outer substitution map. The body additionally excludes lambda-bound
          // names (args and optional recursive name).
          val bodyMap = m -- as.toList -- r.toList
          val c1 = c.map(substituteLocals(m, _))
          val b1 = substituteLocals(bodyMap, b)
          Lambda(c1, r, as, b1)
        case WhileExpr(c, ef, r) =>
          WhileExpr(substituteLocalsBool(m, c), substituteLocals(m, ef), r)
        case ClosureSlot(_) | Global(_, _, _) | LocalAnon(_) | LocalAnonMut(_) |
            MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | Literal(_) | LitInt64(_) |
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
    ): (Expr[B], Boolean) = {

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
          case SwitchVariant(on, famArities, cases, default) =>
            val rewrittenCases = cases.map {
              case (variant, branch) =>
                (variant, loop(branch).getOrElse(returnValue(branch)))
            }
            val rewrittenDefault = default.map { defaultExpr =>
              loop(defaultExpr).getOrElse(returnValue(defaultExpr))
            }
            Some(SwitchVariant(on, famArities, rewrittenCases, rewrittenDefault))
          case Always(c, e) =>
            loop(e).map(Always(c, _))
          case LetMut(m, e) =>
            loop(e).map(LetMut(m, _))
          case Let(b, v, in) =>
            // in is in tail position
            loop(in).map(Let(b, v, _))
          // the rest cannot have a call in tail position
          case App(_, _) | ClosureSlot(_) | GetEnumElement(_, _, _, _) |
              GetStructElement(_, _, _) | Global(_, _, _) | Lambda(_, _, _, _) |
              Literal(_) | LitInt64(_) | Local(_) | LocalAnon(_) | LocalAnonMut(_) |
              MakeEnum(_, _, _) | MakeStruct(_) | PrevNat(_) | SuccNat |
              WhileExpr(_, _, _) | ZeroNat =>
            None
        }

      val bodyTrans = substituteLocals(
        args.toList.map(a => (a.name, a.loopVar)).toMap,
        body
      )

      // No tail call means this loop executes once and returns bodyTrans.
      loop(bodyTrans) match {
        case Some(rewritten) =>
          (rewritten, true)
        case None =>
          (returnValue(bodyTrans), false)
      }
    }

    def buildLoopExpr(
        name: Bindable,
        args: NonEmptyList[Bindable],
        initArgs: NonEmptyList[Expr[B]],
        body: Expr[B],
        onlyIfTailCall: Boolean
    ): F[Option[Expr[B]]] = {
      val mut = makeAnon.map(LocalAnonMut(_))
      val anon = makeAnon.map(LocalAnon(_))
      for {
        cond <- mut
        result <- mut
        args1 <- args.traverse(b => (anon, mut).mapN(ArgRecord(b, _, _)))
        (whileBody, rewroteTailCall) = toWhileBody(
          name,
          args1,
          cond,
          result,
          body
        )
        allMuts = cond :: result :: args1.toList.map(_.loopVar)
      } yield {
        if (onlyIfTailCall && !rewroteTailCall) None
        else {
          val initSets = args1.iterator
            .zip(initArgs.iterator)
            .map { case (argRec, initArg) => (argRec.loopVar, initArg) }
            .toList
          Some(
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
          )
        }
      }
    }

    def buildLoop(
        name: Bindable,
        args: NonEmptyList[Bindable],
        initArgs: NonEmptyList[Expr[B]],
        body: Expr[B]
    ): F[Expr[B]] =
      buildLoopExpr(name, args, initArgs, body, onlyIfTailCall = false).map {
        case Some(loopExpr) => loopExpr
        case None =>
          // unreachable: onlyIfTailCall = false always returns Some(...)
          sys.error(
            s"internal error: missing loop body for $name with onlyIfTailCall=false"
          )
      }

    def loopLetVal(
        name: Bindable,
        e: TypedExpr[A],
        rec: RecursionKind,
        slots: LambdaState
    ): F[Expr[B]] =
      rec match {
        case RecursionKind.Recursive =>
          def peelLeadingLets(
              expr: Expr[B],
              acc: List[(Either[LocalAnon, Bindable], Expr[B])]
          ): (List[(Either[LocalAnon, Bindable], Expr[B])], Expr[B]) =
            expr match {
              case Let(arg, value, in) =>
                peelLeadingLets(in, (arg, value) :: acc)
              case other =>
                (acc.reverse, other)
            }

          loop(e, slots.inLet(name)).flatMap {
            case compiled =>
              val (lets, tail) = peelLeadingLets(compiled, Nil)
              tail match {
                case fn @ Lambda(captures, Some(fnName), args, body)
                    if fnName == name =>
                  // TypedExpr lowering keeps polymorphic recursion in lambda form to
                  // preserve typed AST invariants. Matchless is type-erased, so we
                  // can lower tail self-calls here when available.
                  val initArgs = args.map(Local(_))
                  buildLoopExpr(
                    fnName,
                    args,
                    initArgs,
                    body,
                    onlyIfTailCall = true
                  ).map { maybeLoopBody =>
                    val loweredFn =
                      maybeLoopBody match {
                        case Some(loopBody) =>
                          Lambda(captures, Some(fnName), args, loopBody)
                        case None =>
                          fn
                      }
                    Let.bindAll(lets, loweredFn)
                  }
                case fn: Lambda[?] =>
                  // loops always have a function name
                  sys.error(
                    s"expected ${fn.recursiveName} == Some($name) in ${e.repr.render(80)} which compiled to $compiled"
                  )
                case _ =>
                  sys.error(
                    s"expected ${e.repr.render(80)} to compile to a function, but got: $compiled"
                  )
              }
          }
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
          TypedExpr.Generic(
            q,
            recurToSelfCall(loopName, loopType, in, inNestedLoop)
          )
        case TypedExpr.Annotation(in, tpe, qev) =>
          TypedExpr.Annotation(
            recurToSelfCall(loopName, loopType, in, inNestedLoop),
            tpe,
            qev
          )
        case TypedExpr.AnnotatedLambda(args, body, tag) =>
          TypedExpr.AnnotatedLambda(
            args,
            recurToSelfCall(loopName, loopType, body, inNestedLoop),
            tag
          )
        case app @ TypedExpr.App(fn, args, tpe, tag) =>
          TypedExpr.flattenApp2(app) match {
            case Some((steps, last)) =>
              var acc: TypedExpr[A] =
                recurToSelfCall(loopName, loopType, last, inNestedLoop)
              val rev = steps.toList.reverseIterator
              while (rev.hasNext) {
                val step = rev.next()
                acc =
                  TypedExpr.App(
                    recurToSelfCall(loopName, loopType, step.fn, inNestedLoop),
                    NonEmptyList.of(
                      recurToSelfCall(loopName, loopType, step.arg, inNestedLoop),
                      acc
                    ),
                    step.result,
                    step.tag
                  )
              }
              acc
            case None =>
              TypedExpr.App(
                recurToSelfCall(loopName, loopType, fn, inNestedLoop),
                args.map(recurToSelfCall(loopName, loopType, _, inNestedLoop)),
                tpe,
                tag
              )
          }
        case let @ TypedExpr.Let(
              arg,
              expr,
              in,
              RecursionKind.Recursive,
              tag
            ) =>
          if (arg == loopName) {
            let
          } else {
            TypedExpr.Let(
              arg,
              recurToSelfCall(loopName, loopType, expr, inNestedLoop),
              recurToSelfCall(loopName, loopType, in, inNestedLoop),
              RecursionKind.Recursive,
              tag
            )
          }
        case let @ TypedExpr.Let(_, _, _, RecursionKind.NonRecursive, _) =>
          val (lets, tail) = TypedExpr.flattenLets(let)
          var rewriteIn = true
          var changed = false
          val rebuilt = List.newBuilder[(Bindable, TypedExpr[A], A)]
          val it = lets.iterator
          while (it.hasNext) {
            val (n, rhs, letTag) = it.next()
            val rhs1 =
              if (rewriteIn) recurToSelfCall(loopName, loopType, rhs, inNestedLoop)
              else rhs
            if (!(rhs1 eq rhs)) changed = true
            rebuilt += ((n, rhs1, letTag))
            if (rewriteIn && (n == loopName)) rewriteIn = false
          }
          val tail1 =
            if (rewriteIn) recurToSelfCall(loopName, loopType, tail, inNestedLoop)
            else tail
          if (!(tail1 eq tail)) changed = true
          if (!changed) let
          else TypedExpr.letAllNonRecWithTags(rebuilt.result(), tail1)
        case TypedExpr.Loop(args, body, tag) =>
          TypedExpr.Loop(
            args.map { case (n, expr) =>
              (n, recurToSelfCall(loopName, loopType, expr, inNestedLoop))
            },
            recurToSelfCall(loopName, loopType, body, inNestedLoop = true),
            tag
          )
        case TypedExpr.Recur(args, tpe, tag) =>
          val args1 =
            args.map(recurToSelfCall(loopName, loopType, _, inNestedLoop))
          if (inNestedLoop) TypedExpr.Recur(args1, tpe, tag)
          else {
            val fn = TypedExpr.Local(loopName, loopType, tag)
            TypedExpr.App(fn, args1, tpe, tag)
          }
        case m @ TypedExpr.Match(arg, branches, tag) =>
          TypedExpr.Match(
            m.matchKind,
            recurToSelfCall(loopName, loopType, arg, inNestedLoop),
            branches.map { branch =>
              branch.copy(
                guard = branch.guard
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
        case TypedExpr.Annotation(term, _, _)        => loop(term, slots)
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
        case app @ TypedExpr.App(fn, as, _, _) =>
          val unnameSlots = slots.unname
          def fallbackApp: F[Expr[B]] =
            TypedExpr.flattenApp2(app) match {
              case Some((steps, last)) =>
                val compiledStepsF =
                  steps.toList.foldLeftM(List.empty[(Expr[B], Expr[B])]) {
                    case (acc, step) =>
                      (loop(step.fn, unnameSlots), loop(step.arg, unnameSlots))
                        .mapN { (fn1, arg1) =>
                          (fn1, arg1) :: acc
                        }
                  }
                (compiledStepsF, loop(last, unnameSlots)).mapN {
                  (compiledRev, lastExpr) =>
                    compiledRev.foldLeft(lastExpr) { case (rhsExpr, (fnExpr, argExpr)) =>
                      applyArgs(fnExpr, NonEmptyList.of(argExpr, rhsExpr))
                    }
                }
              case None =>
                (loop(fn, unnameSlots), as.traverse(loop(_, unnameSlots)))
                  .mapN(applyArgs(_, _))
            }

          maybeLiteralInt64App(fn, as.toList) match {
            case Some(expr) =>
              Monad[F].pure(expr)
            case None =>
              (fn, as) match {
                case (
                      TypedExpr.Global(pack, fnName: Bindable, _, _),
                      NonEmptyList(left, right :: Nil)
                    ) =>
                  eqBuiltinDomain(pack, fnName) match {
                    case Some(domain) =>
                      (loop(left, unnameSlots), loop(right, unnameSlots))
                        .tupled
                        .flatMap { case (leftExpr, rightExpr) =>
                          lowerBooleanCompare(
                            domain,
                            leftExpr,
                            CompareRel.Eq,
                            rightExpr
                          )
                        }
                    case None =>
                      fallbackApp
                  }
                case _ =>
                  fallbackApp
              }
          }
        case TypedExpr.Loop(args, body, _) =>
          val avoid: Set[Bindable] =
            TypedExpr.allVarsSet(body :: args.toList.map(_._2)) ++
              slots.names
          val loopName = freshSyntheticNames("loop", 1, avoid).head
          val loopArgs = args.map { case (n, arg) =>
            (n, arg.getType)
          }
          val loopType = Type.Fun(loopArgs.map(_._2), body.getType)
          val body1 =
            recurToSelfCall(loopName, loopType, body, inNestedLoop = false)
          (
            loop(body1, slots),
            args.traverse { case (_, init) => loop(init, slots) }
          ).tupled
            .flatMap { case (bodyExpr, initVals) =>
              buildLoop(loopName, loopArgs.map(_._1), initVals, bodyExpr)
            }
        case TypedExpr.Let(a, e, in, RecursionKind.Recursive, _) =>
          (loopLetVal(a, e, RecursionKind.Recursive, slots.unname), loop(in, slots))
            .mapN(Let(a, _, _))
        case let @ TypedExpr.Let(_, _, _, RecursionKind.NonRecursive, _) =>
          val (lets, tail) = TypedExpr.flattenLets(let)
          val bindsF = lets.traverse { case (arg, rhs, _) =>
            loopLetVal(arg, rhs, RecursionKind.NonRecursive, slots.unname)
              .map(v => (arg, v))
          }
          (bindsF, loop(tail, slots)).tupled.flatMap { case (binds, tailExpr) =>
            binds.reverse.foldLeftM(tailExpr) { case (acc, (arg, value)) =>
              maybeLowerComparisonObservationLet(Right(arg), value, acc)
                .getOrElse(Monad[F].pure(Let(arg, value, acc)))
            }
          }
        case TypedExpr.Recur(_, _, _) =>
          // Loops should be lowered from TypedExpr.Loop and not escape raw Recur nodes.
          sys.error(
            s"unreachable raw recur in Matchless lowering: ${te.repr.render(80)}"
          )
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

    case class ListSearchSegment(
        glob: Pattern.ListPart.Glob,
        items: NonEmptyList[Pattern[(PackageName, Constructor), Type]]
    )

    case class ListSearchPlan(
        segments: NonEmptyList[ListSearchSegment],
        trailingGlob: Option[Pattern.ListPart.Glob]
    )

    case class SearchLoopState(runMut: LocalAnonMut, resMut: LocalAnonMut)

    // This is only called from the sole caller below after both
    // exactMiddleItems(right) and exactTrailingItems(right) fail.
    // That means right contains at least one interior glob, which rules out
    // one-segment shapes like [items..., trailingGlob]. So the resulting plan
    // always has at least two exact segments.
    def buildListSearchPlan(
        glob: Pattern.ListPart.Glob,
        right: NonEmptyList[Pattern.ListPart[Pattern[(PackageName, Constructor), Type]]]
    ): ListSearchPlan = {
      var currentGlob: Pattern.ListPart.Glob = glob
      var currentItemsRev: List[Pattern[(PackageName, Constructor), Type]] = Nil
      var segmentsRev: List[ListSearchSegment] = Nil

      right.toList.foreach {
        case Pattern.ListPart.Item(p) =>
          currentItemsRev = p :: currentItemsRev
        case nextGlob: Pattern.ListPart.Glob =>
          val currentItems =
            NonEmptyList.fromList(currentItemsRev.reverse).getOrElse {
              throw new IllegalStateException(
                s"expected exact items before trailing glob in list search pattern: $right"
              )
            }
          segmentsRev = ListSearchSegment(currentGlob, currentItems) :: segmentsRev
          currentGlob = nextGlob
          currentItemsRev = Nil
      }

      val trailingGlob =
        NonEmptyList.fromList(currentItemsRev.reverse) match {
          case Some(lastItems) =>
            segmentsRev = ListSearchSegment(currentGlob, lastItems) :: segmentsRev
            None
          case None            =>
            Some(currentGlob)
        }

      ListSearchPlan(
        NonEmptyList.fromListUnsafe(segmentsRev.reverse),
        trailingGlob
      )
    }

    def exactTrailingItems(
        right: NonEmptyList[Pattern.ListPart[Pattern[(PackageName, Constructor), Type]]]
    ): Option[NonEmptyList[Pattern[(PackageName, Constructor), Type]]] =
      right.traverse {
        case Pattern.ListPart.Item(p) => Some(p)
        case _: Pattern.ListPart.Glob => None
      }

    def exactMiddleItems(
        right: NonEmptyList[Pattern.ListPart[Pattern[(PackageName, Constructor), Type]]]
    ): Option[
      (
          NonEmptyList[Pattern[(PackageName, Constructor), Type]],
          Pattern.ListPart.Glob
      )
    ] =
      right.toList match {
        case init :+ (glob: Pattern.ListPart.Glob) =>
          init
            .traverse {
              case Pattern.ListPart.Item(p) => Some(p)
              case _: Pattern.ListPart.Glob => None
            }
            .flatMap { items =>
              NonEmptyList.fromList(items).map((_, glob))
            }
        case _ =>
          None
      }

    def prepareLeftGlob(
        glob: Pattern.ListPart.Glob
    ): F[Option[(LocalAnonMut, Bindable)]] =
      glob match {
        case Pattern.ListPart.WildList =>
          Monad[F].pure(None)
        case Pattern.ListPart.NamedList(ln) =>
          makeAnon.map(nm => Some((LocalAnonMut(nm), ln)))
      }

    def advanceListBy(
        lead: LocalAnonMut,
        ok: LocalAnonMut,
        steps: Int,
        onSuccess: Expr[B]
    ): Expr[B] =
      if (steps <= 0) onSuccess
      else {
        val next = advanceListBy(lead, ok, steps - 1, onSuccess)
        If(
          ListExpr.notNil(lead),
          setAll((lead, ListExpr.tail(lead)) :: Nil, next),
          setAll((ok, FalseExpr) :: Nil, UnitExpr)
        )
      }

    // Position lag at the unique suffix start for a fixed-width trailing
    // pattern. This avoids re-checking the exact suffix at every tail.
    def positionTrailingExactItems(
        lag: LocalAnonMut,
        lead: LocalAnonMut,
        ok: LocalAnonMut,
        init: CheapExpr[B],
        suffixLen: Int,
        leftAcc: Option[LocalAnonMut]
    ): F[BoolExpr[B]] =
      makeAnon.map { loopResNm =>
        val loopRes = LocalAnon(loopResNm)
        val initSets =
          (lag, init) ::
            (lead, init) ::
            (ok, TrueExpr) ::
            leftAcc.toList.map { left =>
              (left, ListExpr.Nil)
            }
        val shiftUpdates =
          leftAcc.toList.map { left =>
            (left, ListExpr.cons(ListExpr.head(lag), left))
          } ::: (
            (lead, ListExpr.tail(lead)) ::
              (lag, ListExpr.tail(lag)) ::
              Nil
          )
        val shiftLoop =
          WhileExpr(
            ListExpr.notNil(lead),
            setAll(shiftUpdates, UnitExpr),
            ok
          )
        val positioned =
          setAll(
            initSets,
            advanceListBy(lead, ok, suffixLen, shiftLoop)
          )

        LetBool(Left(loopRes), positioned, isTrueExpr(ok))
      }

    def matchFixedListWindowItems(
        itemMuts: NonEmptyList[LocalAnonMut],
        exactItems: NonEmptyList[Pattern[(PackageName, Constructor), Type]]
    ): F[UnionMatch] =
      itemMuts.zip(exactItems).traverse { case (itemMut, pat) =>
        doesMatch(ListExpr.head(itemMut), pat, false, None, None)
      }.map { matchLists =>
        product(matchLists) { case ((l1, o1, b1), (l2, o2, b2)) =>
          (l1 ::: l2, o1 && o2, b1 ::: b2)
        }
      }

    def initializeFixedListWindow(
        init: CheapExpr[B],
        itemMuts: List[LocalAnonMut],
        leadMut: LocalAnonMut,
        runMut: LocalAnonMut,
        leftAcc: Option[LocalAnonMut],
        onFailure: Expr[B],
        onSuccess: Expr[B]
    ): Expr[B] = {
      def assignWindow(
          current: CheapExpr[B],
          remaining: List[LocalAnonMut]
      ): Expr[B] =
        remaining match {
          case Nil =>
            setAll((leadMut, current) :: Nil, onSuccess)
          case itemMut :: tail =>
            If(
              ListExpr.notNil(current),
              setAll((itemMut, current) :: Nil, assignWindow(ListExpr.tail(current), tail)),
              setAll((runMut, FalseExpr) :: Nil, onFailure)
            )
        }

      val initSets =
        (runMut, TrueExpr) ::
          leftAcc.toList.map { left =>
            (left, ListExpr.Nil)
          }

      setAll(initSets, assignWindow(init, itemMuts))
    }

    def shiftFixedListWindow(
        itemMuts: NonEmptyList[LocalAnonMut],
        leadMut: LocalAnonMut,
        leftAcc: Option[LocalAnonMut]
    ): Expr[B] = {
      val window = itemMuts.toList
      val lastMut = window.last
      val shiftItems =
        window.zip(window.drop(1)).map { case (current, next) =>
          (current, next: Expr[B])
        }
      val shiftUpdates =
        leftAcc.toList.map { left =>
          (left, ListExpr.cons(ListExpr.head(itemMuts.head), left))
        } ::: shiftItems ::: (
          (lastMut, leadMut: Expr[B]) ::
            (leadMut, ListExpr.tail(leadMut)) ::
            Nil
        )

      setAll(shiftUpdates, UnitExpr)
    }

    def matchedExpr(state: SearchLoopState): Expr[B] =
      setAll(
        (state.runMut, FalseExpr) ::
          (state.resMut, TrueExpr) ::
          Nil,
        UnitExpr
      )

    def stopFailExpr(state: SearchLoopState): Expr[B] =
      setAll((state.runMut, FalseExpr) :: Nil, UnitExpr)

    def continueExpr(
        state: SearchLoopState,
        itemMuts: NonEmptyList[LocalAnonMut],
        leadMut: LocalAnonMut,
        leftAcc: Option[LocalAnonMut]
    ): Expr[B] =
      If(
        ListExpr.notNil(leadMut),
        shiftFixedListWindow(itemMuts, leadMut, leftAcc),
        stopFailExpr(state)
      )

    def searchFixedListWindow(
        arg: CheapExpr[B],
        itemMuts: NonEmptyList[LocalAnonMut],
        leadMut: LocalAnonMut,
        check: BoolExpr[B],
        leftAcc: Option[LocalAnonMut]
    ): F[BoolExpr[B]] =
      (
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnon(_))
      ).mapN { (runMut, resMut, loopRes) =>
        val onNoCandidate = setAll((runMut, FalseExpr) :: Nil, UnitExpr)
        val onMatch =
          setAll(
            (runMut, FalseExpr) ::
              (resMut, TrueExpr) ::
              Nil,
            UnitExpr
          )
        val onMiss =
          If(
            ListExpr.notNil(leadMut),
            shiftFixedListWindow(itemMuts, leadMut, leftAcc),
            onNoCandidate
          )
        val effect = If(check, onMatch, onMiss)
        val searchLoop =
          initializeFixedListWindow(
            arg,
            itemMuts.toList,
            leadMut,
            runMut,
            leftAcc,
            resMut,
            WhileExpr(isTrueExpr(runMut), effect, resMut)
          )
        val initSets =
          (resMut, FalseExpr) :: Nil

        LetMutBool(
          runMut :: resMut :: Nil,
          LetBool(Left(loopRes), setAll(initSets, searchLoop), isTrueExpr(loopRes))
        )
      }

    def searchFixedListWindowEarlyStop(
        arg: CheapExpr[B],
        itemMuts: NonEmptyList[LocalAnonMut],
        leadMut: LocalAnonMut,
        exactCheck: BoolExpr[B],
        pureCheck: BoolExpr[B],
        onPureMatch: BoolExpr[B],
        leftAcc: Option[LocalAnonMut]
    ): F[BoolExpr[B]] =
      (
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnon(_))
      ).mapN { (runMut, resMut, loopRes) =>
        val state = SearchLoopState(runMut, resMut)
        val effect =
          If(
            exactCheck,
            If(
              pureCheck,
              If(
                onPureMatch,
                matchedExpr(state),
                continueExpr(state, itemMuts, leadMut, leftAcc)
              ),
              stopFailExpr(state)
            ),
            continueExpr(state, itemMuts, leadMut, leftAcc)
          )
        val searchLoop =
          initializeFixedListWindow(
            arg,
            itemMuts.toList,
            leadMut,
            runMut,
            leftAcc,
            resMut,
            WhileExpr(isTrueExpr(runMut), effect, resMut)
          )
        val initSets =
          (resMut, FalseExpr) :: Nil

        LetMutBool(
          runMut :: resMut :: Nil,
          LetBool(Left(loopRes), setAll(initSets, searchLoop), isTrueExpr(loopRes))
        )
      }

    def exactMiddleListSearch(
        arg: CheapExpr[B],
        leftGlob: Pattern.ListPart.Glob,
        middleItems: NonEmptyList[Pattern[(PackageName, Constructor), Type]],
        rightGlob: Pattern.ListPart.Glob,
        candidateGuard: Option[CandidateGuard]
    ): F[UnionMatch] = {
      val leftF = prepareLeftGlob(leftGlob)
      (
        leftF,
        middleItems.traverse(_ => makeAnon.map(LocalAnonMut(_))),
        makeAnon.map(LocalAnonMut(_))
      ).tupled.flatMap { case (optAnonLeft, itemMuts, leadMut) =>
        matchFixedListWindowItems(itemMuts, middleItems)
          .flatMap { cases =>
            cases.traverse { case (preLet, expr, binds) =>
              val letTail =
                optAnonLeft.map(_._1).toList ::: (leadMut :: itemMuts.toList) ::: preLet
              val leftBind =
                optAnonLeft.map { case (anonLeft, ln) =>
                  (
                    ln,
                    applyArgs(
                      reverseFn(from),
                      NonEmptyList.one(anonLeft)
                    )
                  )
                }
              val rightBind =
                rightGlob match {
                  case Pattern.ListPart.WildList =>
                    Nil
                  case Pattern.ListPart.NamedList(ln) =>
                    (ln, leadMut: Expr[B]) :: Nil
                }
              val allBinds = leftBind.toList ::: binds ::: rightBind

              applyOptionalCandidateGuard(
                expr,
                allBinds,
                candidateGuard
              ).flatMap {
                searchCheck =>
                  searchFixedListWindow(
                    arg,
                    itemMuts,
                    leadMut,
                    searchCheck,
                    optAnonLeft.map(_._1)
                  ).map { search =>
                    (letTail, search, allBinds)
                  }
              }
            }
          }
      }
    }

    def exactTrailingListSearch(
        arg: CheapExpr[B],
        glob: Pattern.ListPart.Glob,
        exactItems: NonEmptyList[Pattern[(PackageName, Constructor), Type]]
    ): F[UnionMatch] = {
      val exactPat = Pattern.ListPat(exactItems.toList.map(Pattern.ListPart.Item(_)))
      val leftF = prepareLeftGlob(glob)
      val anon = makeAnon.map(LocalAnonMut(_))

      for {
        optAnonLeft <- leftF
        lagMut <- anon
        leadMut <- anon
        okMut <- anon
        positioned <- positionTrailingExactItems(
          lagMut,
          leadMut,
          okMut,
          arg,
          exactItems.length,
          optAnonLeft.map(_._1)
        )
        cases <- doesMatch(lagMut, exactPat, false, None, None)
      } yield {
        cases.map { case (preLet, expr, binds) =>
          val searchLets =
            optAnonLeft.map(_._1).toList ::: (okMut :: leadMut :: lagMut :: Nil)
          val letTail = searchLets ::: preLet
          val resBind =
            optAnonLeft match {
              case Some((anonLeft, ln)) =>
                val revList =
                  applyArgs(
                    reverseFn(from),
                    NonEmptyList.one(anonLeft)
                  )
                (ln, revList) :: binds
              case None =>
                binds
            }

          (letTail, positioned && expr, resBind)
        }
        }
      }

    def compileLastListSearchSegment(
        arg: CheapExpr[B],
        segment: ListSearchSegment,
        trailingGlob: Option[Pattern.ListPart.Glob]
    ): F[UnionMatch] =
      trailingGlob match {
        case Some(rightGlob) =>
          exactMiddleListSearch(
            arg,
            segment.glob,
            segment.items,
            rightGlob,
            None
          )
        case None            =>
          exactTrailingListSearch(arg, segment.glob, segment.items)
      }

    def compileLeadingListSearchSegment(
        arg: CheapExpr[B],
        segment: ListSearchSegment,
        restPlan: ListSearchPlan,
        onPureMatch: List[(Bindable, Expr[B])] => F[BoolExpr[B]]
    ): F[UnionMatch] = {
      val leftF = prepareLeftGlob(segment.glob)
      (
        leftF,
        segment.items.traverse(_ => makeAnon.map(LocalAnonMut(_))),
        makeAnon.map(LocalAnonMut(_))
      ).flatMapN { (optAnonLeft, itemMuts, leadMut) =>
        matchFixedListWindowItems(itemMuts, segment.items).flatMap { exactCases =>
          compilePureListSearchPlan(leadMut, restPlan).flatMap { restCases =>
            val casePairs =
              for {
                exactCase <- exactCases
                restCase <- restCases
              } yield (exactCase, restCase)

            casePairs
              .traverse {
                case (
                      (exactPreLet, exactExpr, exactBinds),
                      (restPreLet, restExpr, restBinds)
                    ) =>
                  val letTail =
                    optAnonLeft.map(_._1).toList ::: (leadMut :: itemMuts.toList) ::: exactPreLet ::: restPreLet
                  val leftBind =
                    optAnonLeft.map { case (anonLeft, ln) =>
                      (
                        ln,
                        applyArgs(
                          reverseFn(from),
                          NonEmptyList.one(anonLeft)
                        )
                      )
                    }
                  val allBinds = leftBind.toList ::: exactBinds ::: restBinds

                  onPureMatch(allBinds).flatMap { successCheck =>
                    searchFixedListWindowEarlyStop(
                      arg,
                      itemMuts,
                      leadMut,
                      exactExpr,
                      restExpr,
                      successCheck,
                      optAnonLeft.map(_._1)
                    ).map { search =>
                      (letTail, search, allBinds)
                    }
                  }
              }
          }
        }
      }
    }

    def compilePureListSearchPlan(
        arg: CheapExpr[B],
        plan: ListSearchPlan
    ): F[UnionMatch] =
      plan.segments match {
        case NonEmptyList(segment, Nil) =>
          compileLastListSearchSegment(arg, segment, plan.trailingGlob)
        case NonEmptyList(segment, next :: tail) =>
          compileLeadingListSearchSegment(
            arg,
            segment,
            ListSearchPlan(NonEmptyList(next, tail), plan.trailingGlob),
            _ => Monad[F].pure(TrueConst)
          )
      }

    def compileListSearchPlan(
        arg: CheapExpr[B],
        plan: ListSearchPlan,
        candidateGuard: Option[CandidateGuard]
    ): F[UnionMatch] =
      candidateGuard match {
        case None =>
          compilePureListSearchPlan(arg, plan)
        case Some(guardFn) =>
          plan.segments match {
            case NonEmptyList(segment, tail) =>
              // The sole caller only reaches compileListSearchPlan after both
              // exactMiddleItems(right) and exactTrailingItems(right) fail.
              // That guarantees there is at least one interior glob after the
              // initial exact run, so a guarded plan here always has at least
              // two exact segments.
              val tailNel = NonEmptyList.fromListUnsafe(tail)
              compileLeadingListSearchSegment(
                arg,
                segment,
                ListSearchPlan(tailNel, plan.trailingGlob),
                guardFn(_)
              )
          }
      }

    case class InlinedStructRoot(fields: Vector[CheapExpr[B]]) {
      def toExpr: Expr[B] =
        applyArgs(
          MakeStruct(fields.length),
          NonEmptyList.fromListUnsafe(fields.toList)
        )

      def field(idx: Int, size: Int): CheapExpr[B] = {
        require((size == fields.length) && (idx >= 0) && (idx < size))
        fields(idx)
      }
    }

    def prepareInlinedStructRoot(
        arg: Expr[B]
    ): F[Option[(List[(LocalAnon, Expr[B])], InlinedStructRoot)]] =
      arg match {
        case App(MakeStruct(arity), args) if args.length == arity =>
          args.toList
            .foldLeftM(
              (List.empty[(LocalAnon, Expr[B])], List.empty[CheapExpr[B]])
            ) { case ((letsRev, fieldsRev), item) =>
              item match {
                case ch: CheapExpr[B] =>
                  Monad[F].pure((letsRev, ch :: fieldsRev))
                case expr =>
                  makeAnon.map { nm =>
                    val local = LocalAnon(nm)
                    ((local, expr) :: letsRev, local :: fieldsRev)
                  }
              }
            }
            .map { case (letsRev, fieldsRev) =>
              Some(
                (
                  letsRev.reverse,
                  InlinedStructRoot(fieldsRev.reverse.toVector)
                )
              )
            }
        case _ =>
          Monad[F].pure(None)
      }

    def bindOccurrenceValue(
        occ: CheapExpr[B],
        inlined: Option[InlinedStructRoot]
    ): Expr[B] =
      inlined match {
        case Some(root) => root.toExpr
        case None       => occ
      }

    def projectStructOccurrence(
        occ: CheapExpr[B],
        idx: Int,
        size: Int,
        inlined: Option[InlinedStructRoot]
    ): CheapExpr[B] =
      inlined match {
        case Some(root) => root.field(idx, size)
        case None       => GetStructElement(occ, idx, size)
      }

    def rootInlinedForOcc(
        occ: CheapExpr[B],
        rootOcc: CheapExpr[B],
        inlinedRoot: Option[InlinedStructRoot]
    ): Option[InlinedStructRoot] =
      inlinedRoot.filter(_ => occ.equals(rootOcc))

    // return the check expression for the check we need to do, and the list of bindings
    // if must match is true, we know that the pattern must match, so we can potentially remove some checks
    def doesMatch(
        arg: CheapExpr[B],
        pat: Pattern[(PackageName, Constructor), Type],
        mustMatch: Boolean,
        rootInlined: Option[InlinedStructRoot],
        candidateGuard: Option[CandidateGuard]
    ): F[UnionMatch] = {
      def finish(result: F[UnionMatch]): F[UnionMatch] =
        result.flatMap(applyCandidateGuardToUnionMatch(_, candidateGuard))

      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          finish(Monad[F].pure(wildMatch))
        case Pattern.Literal(lit) =>
          val cond =
            if (mustMatch) TrueConst
            else CompareLit(arg, CompareRel.Eq, lit)
          finish(Monad[F].pure(NonEmptyList((Nil, cond, Nil), Nil)))
        case Pattern.Var(v) =>
          finish(Monad[F].pure(
            NonEmptyList(
              (Nil, TrueConst, (v, bindOccurrenceValue(arg, rootInlined)) :: Nil),
              Nil
            )
          ))
        case Pattern.Named(v, p) =>
          finish(doesMatch(arg, p, mustMatch, rootInlined, None).map(_.map {
            case (l0, cond, bs) =>
              (l0, cond, (v, bindOccurrenceValue(arg, rootInlined)) :: bs)
          }))
        case strPat @ Pattern.StrPat(items) =>
          strPat.simplify match {
            case Some(simpler) =>
              doesMatch(arg, simpler, mustMatch, rootInlined, candidateGuard)
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

              for {
                binds <- sbinds.traverse { b =>
                  makeAnon.map(LocalAnonMut(_)).map((b, _))
                }
                ms = binds.map(_._2)
                onPureMatch =
                  applyOptionalCandidateGuard(TrueConst, binds, candidateGuard)
                cond <- StringMatcher(
                  arg,
                  pat,
                  ms.toIndexedSeq,
                  0,
                  mustMatch,
                  onPureMatch,
                  makeAnon,
                  from,
                  variantOf
                )
              } yield NonEmptyList.one((ms, cond, binds))
          }
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p) =>
              // rootInlined = None: list patterns match enum constructors, not root structs.
              doesMatch(arg, p, mustMatch, None, candidateGuard)
            case Left(
                  (glob, right @ NonEmptyList(Pattern.ListPart.Item(_), _))
                ) =>
              exactMiddleItems(right) match {
                case Some((middleItems, rightGlob)) =>
                  exactMiddleListSearch(
                    arg,
                    glob,
                    middleItems,
                    rightGlob,
                    candidateGuard
                  )
                case None =>
                  exactTrailingItems(right) match {
                    case Some(exactItems) =>
                      finish(exactTrailingListSearch(arg, glob, exactItems))
                    case None             =>
                      compileListSearchPlan(
                        arg,
                        buildListSearchPlan(glob, right),
                        candidateGuard
                      )
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
                  doesMatch(
                    arg,
                    Pattern.ListPat(right.toList),
                    mustMatch,
                    None,
                    candidateGuard
                  )
                case Pattern.ListPart.NamedList(ln) =>
                  // bind empty to ln
                  finish(
                    doesMatch(
                      arg,
                      Pattern.ListPat(right.toList),
                      mustMatch,
                      None,
                      None
                    )
                      .map { nel =>
                      nel.map { case (preLet, expr, binds) =>
                        (preLet, expr, (ln, emptyExpr) :: binds)
                      }
                      }
                  )
              }
            // $COVERAGE-ON$
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p, mustMatch, rootInlined, candidateGuard)
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
                      doesMatch(lam, pat, mustMatch, None, None)
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
            asStruct(pos =>
              projectStructOccurrence(arg, pos, size, rootInlined)
            ).run
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
                case DataRepr.Struct(size)        => finish(forStruct(size))
                case DataRepr.NewType             => finish(forStruct(1))
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
                    .flatMap(applyCandidateGuardToUnionMatch(_, candidateGuard))
                case DataRepr.ZeroNat =>
                  val cv: BoolExpr[B] =
                    if (mustMatch) TrueConst
                    else EqualsNat(arg, DataRepr.ZeroNat)
                  finish(Monad[F].pure(NonEmptyList((Nil, cv, Nil), Nil)))
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
                        (
                          for {
                            nm <- makeAnon
                            loc = LocalAnonMut(nm)
                            prev = PrevNat(arg)
                            // rootInlined = None: we match the predecessor occurrence, not the root.
                            rest <- doesMatch(loc, single, mustMatch, None, None)
                          } yield rest.map { case (preLets, cond, res) =>
                            (
                              loc :: preLets,
                              check && SetMut(loc, prev) && cond,
                              res
                            )
                          }
                        ).flatMap(
                          applyCandidateGuardToUnionMatch(_, candidateGuard)
                        )
                      } else {
                        // we don't need to bind the prev
                        finish(Monad[F].pure(wildMatch.map {
                          case (preLets, cond, res) =>
                            (
                              preLets,
                              check && cond,
                              res
                            )
                        }))
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
            .traverse { case (p, mm) =>
              doesMatch(arg, p, mm, rootInlined, None)
            }
            .map { nene =>
              val nel = nene.flatten
              // at the first total match, we can stop
              stopAt(nel) {
                case (_, TrueConst, _) => true
                case _                 => false
              }
            }
            .flatMap(applyCandidateGuardToUnionMatch(_, candidateGuard))
      }
    }

    def lets(binds: List[(Bindable, Expr[B])], in: Expr[B]): Expr[B] =
      Let.bindNamed(binds, in)

    def letAnons(binds: List[(LocalAnon, Expr[B])], in: Expr[B]): Expr[B] =
      Let.bindAnons(binds, in)

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

      def projectionArity: Int =
        this match {
          case EnumSig(_, _, arity, _) => arity
          case StructSig(_, arity)     => arity
          case SuccSig                 => 1
          case LitSig(_) | ZeroSig     => 0
        }
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
      p.isSearchPattern

    // Pull off aliases and bindings that capture the whole occurrence.
    // This keeps the matrix focused on refutable structure.
    // Return patterns are normalized and never Named/Var/Annotation.
    // For matrix compilation, the remaining cases are:
    // WildCard, Literal, PositionalStruct, or Union.
    def peelPattern(
        p: Pattern[(PackageName, Constructor), Type],
        occ: CheapExpr[B],
        inlined: Option[InlinedStructRoot]
    ): (List[(Bindable, Expr[B])], Pattern[(PackageName, Constructor), Type]) =
      p match {
        case Pattern.Named(v, inner) =>
          val (bs, core) = peelPattern(inner, occ, inlined)
          ((v, bindOccurrenceValue(occ, inlined)) :: bs, core)
        case Pattern.Var(v) =>
          ((v, bindOccurrenceValue(occ, inlined)) :: Nil, Pattern.WildCard)
        case Pattern.Annotation(inner, _) =>
          peelPattern(inner, occ, inlined)
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
        occs: List[CheapExpr[B]],
        rootOcc: CheapExpr[B],
        inlinedRoot: Option[InlinedStructRoot]
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
            val maybeInlined = rootInlinedForOcc(occ, rootOcc, inlinedRoot)
            val (moreBinds, core) = peelPattern(p1, occ, maybeInlined)
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
          seen.add(a): Unit
          bldr.append(a)
        }
      }
      bldr.toList
    }

    case class ColumnScore(
        colIdx: Int,
        distinctSigs: Int,
        refutableRows: Int,
        arityPenalty: Int,
        sigs: List[HeadSig]
    )

    // Rank columns with a cheap local heuristic:
    // 1) more distinct refutable heads first,
    // 2) then columns refuting more rows,
    // 3) then lower arity expansion cost,
    // 4) then leftmost for deterministic ties.
    def chooseColumnByScore(rows: List[MatchRow]): (Int, List[HeadSig]) = {
      val colCount = rows.headOption match {
        case Some(row) => row.pats.length
        case None      => 0
      }

      // $COVERAGE-OFF$
      if (colCount == 0)
        throw new IllegalStateException(
          "chooseColumnByScore called with no remaining columns"
        )
      // $COVERAGE-ON$
      else {
        def scoreCol(colIdx: Int): ColumnScore = {
          val sigs = distinctInOrder(rows.mapFilter(r => headSig(r.pats(colIdx))))
          // normalizeRow + peelPattern remove Var/Named/Annotation, so at this
          // stage non-refutable patterns are exactly WildCard.
          val refutableRows = rows.count(r => r.pats(colIdx) != Pattern.WildCard)
          // We split once per distinct signature, so sum those arities as a
          // cheap proxy for added projection pressure in this column.
          val arityPenalty = sigs.iterator.map(_.projectionArity).sum
          ColumnScore(
            colIdx = colIdx,
            distinctSigs = sigs.length,
            refutableRows = refutableRows,
            arityPenalty = arityPenalty,
            sigs = sigs
          )
        }

        def better(next: ColumnScore, best: ColumnScore): Boolean = {
          import java.lang.Integer.compare

          val cmpDistinct = compare(next.distinctSigs, best.distinctSigs)
          (cmpDistinct > 0) || {
            (cmpDistinct == 0) && {
              val cmpRefutable = compare(next.refutableRows, best.refutableRows)
              (cmpRefutable > 0) || {
                (cmpRefutable == 0) && {
                  val cmpPenalty =
                    compare(best.arityPenalty, next.arityPenalty)
                  (cmpPenalty > 0) || {
                    (cmpPenalty == 0) &&
                    (compare(best.colIdx, next.colIdx) > 0)
                  }
                }
              }
            }
          }
        }

        val best =
          (1 until colCount).iterator
            .map(scoreCol)
            .foldLeft(scoreCol(0)) { (currentBest, candidate) =>
              if (better(candidate, currentBest)) candidate else currentBest
            }

        (best.colIdx, best.sigs)
      }
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

    def specializedKeepOffsets(
        rows: List[MatchRow],
        colIdx: Int,
        arity: Int
    ): List[Int] =
      (0 until arity).iterator
        .filter(i => rows.exists(_.pats(colIdx + i) != Pattern.WildCard))
        .toList

    def trimSpecializedPats(
        pats: List[Pattern[(PackageName, Constructor), Type]],
        colIdx: Int,
        arity: Int,
        keepOffsets: List[Int]
    ): List[Pattern[(PackageName, Constructor), Type]] = {
      val prefix = pats.take(colIdx)
      val kept = keepOffsets.map(i => pats(colIdx + i))
      val suffix = pats.drop(colIdx + arity)
      prefix ::: kept ::: suffix
    }

    def minimizeAlreadySpecializedRows(
        rows: List[MatchRow],
        colIdx: Int,
        arity: Int
    ): (List[MatchRow], List[Int]) = {
      val keepOffsets = specializedKeepOffsets(rows, colIdx, arity)
      if (keepOffsets.length == arity) (rows, keepOffsets)
      else {
        val trimmedRows = rows.map { row =>
          row.copy(
            pats = trimSpecializedPats(row.pats, colIdx, arity, keepOffsets)
          )
        }
        (trimmedRows, keepOffsets)
      }
    }

    def minimizeSpecializedRows(
        sig: HeadSig,
        rows: List[MatchRow],
        colIdx: Int,
        arity: Int
    ): (List[MatchRow], List[Int]) =
      minimizeAlreadySpecializedRows(
        specializeRows(sig, rows, colIdx, arity),
        colIdx,
        arity
      )

    type VariantSelector = (CheapExpr[B], Int, List[Int])

    def boolLiteralValue(expr: Expr[B]): Option[Boolean] =
      expr match {
        case MakeEnum(variant, 0, famArities) if famArities == boolFamArities =>
          variant match {
            case 0 => Some(false)
            case 1 => Some(true)
            case _ => None
          }
        case _ =>
          None
      }

    def collectTrueVariants(
        expr: Expr[B],
        possible: Set[Int],
        selector: VariantSelector
    ): Option[Set[Int]] = {
      val (selectorArg, selectorSize, selectorFamArities) = selector
      boolLiteralValue(expr) match {
        case Some(true)  => Some(possible)
        case Some(false) => Some(Set.empty[Int])
        case None        =>
          expr match {
            case If(
                  CheckVariant(arg: CheapExpr[B], expect, size, famArities),
                  ifTrue,
                  ifFalse
                )
                if Order[Expr[B]].eqv(arg, selectorArg) &&
                  (size == selectorSize) &&
                  (famArities == selectorFamArities) &&
                  selectorFamArities.forall(_ == 0) =>
              val onTrue: Set[Int] =
                if (possible(expect)) Set(expect) else Set.empty[Int]
              val onFalse = possible - expect
              (
                collectTrueVariants(ifTrue, onTrue, selector),
                collectTrueVariants(ifFalse, onFalse, selector)
              ).mapN(_ union _)
            case If(
                  CheckVariantSet(arg: CheapExpr[B], expect, size, famArities),
                  ifTrue,
                  ifFalse
                )
                if Order[Expr[B]].eqv(arg, selectorArg) &&
                  (size == selectorSize) &&
                  (famArities == selectorFamArities) &&
                  selectorFamArities.forall(_ == 0) =>
              val expectSet = expect.toList.toSet
              val onTrue = possible intersect expectSet
              val onFalse = possible diff expectSet
              (
                collectTrueVariants(ifTrue, onTrue, selector),
                collectTrueVariants(ifFalse, onFalse, selector)
              ).mapN(_ union _)
            case SwitchVariant(
                  on: CheapExpr[B],
                  famArities,
                  cases,
                  default
                )
                if Order[Expr[B]].eqv(on, selectorArg) &&
                  (famArities == selectorFamArities) &&
                  selectorFamArities.forall(_ == 0) =>
              val explicitVariants = cases.map(_._1).toList.toSet
              val explicitTrue =
                cases.toList.foldLeft(Option(Set.empty[Int])) {
                  case (accOpt, (variant, branch))
                      if possible(variant) =>
                    (
                      accOpt,
                      collectTrueVariants(branch, Set(variant), selector)
                    ).mapN(_ union _)
                  case (accOpt, _) =>
                    accOpt
                }
              val defaultPossible = possible diff explicitVariants
              val defaultTrue =
                if (defaultPossible.isEmpty) Some(Set.empty[Int])
                else default.flatMap(collectTrueVariants(_, defaultPossible, selector))
              (explicitTrue, defaultTrue).mapN(_ union _)
            case _ =>
              None
          }
      }
    }

    def buildVariantSelectorBool(
        selector: VariantSelector,
        trueVariants: Set[Int]
    ): Option[BoolExpr[B]] = {
      val (selectorArg, selectorSize, selectorFamArities) = selector
      val allVariants = selectorFamArities.indices.toSet
      if (trueVariants.isEmpty) None
      else if (trueVariants.size == allVariants.size) Some(TrueConst)
      else if (trueVariants.size == 1)
        Some(
          CheckVariant(
            selectorArg,
            trueVariants.iterator.next(),
            selectorSize,
            selectorFamArities
          )
        )
      else
        Some(
          CheckVariantSet(
            selectorArg,
            NonEmptyList.fromListUnsafe(trueVariants.toList.sorted),
            selectorSize,
            selectorFamArities
          )
        )
    }

    def directGuardBoolExpr(expr: Expr[B]): Option[BoolExpr[B]] =
      expr match {
        case Let(arg, value, in) =>
          directGuardBoolExpr(in).map(LetBool(arg, value, _))
        case If(cond, TrueExpr, FalseExpr) =>
          Some(cond)
        case _ =>
          None
      }

    def selectorGuardToBoolExpr(expr: Expr[B]): Option[BoolExpr[B]] =
      expr match {
        case Let(arg, value, in) =>
          selectorGuardToBoolExpr(in).map(LetBool(arg, value, _))
        case selectorExpr @ If(cond, _, _) =>
          cond match {
            case CheckVariant(arg, _, size, famArities)
                if famArities.forall(_ == 0) =>
              val selector: VariantSelector = (arg, size, famArities)
              collectTrueVariants(selectorExpr, famArities.indices.toSet, selector)
                .flatMap(buildVariantSelectorBool(selector, _))
            case CheckVariantSet(arg, _, size, famArities)
                if famArities.forall(_ == 0) =>
              val selector: VariantSelector = (arg, size, famArities)
              collectTrueVariants(selectorExpr, famArities.indices.toSet, selector)
                .flatMap(buildVariantSelectorBool(selector, _))
            case _ =>
              None
          }
        case selectorExpr @ SwitchVariant(on, famArities, _, _)
            if famArities.forall(_ == 0) =>
          // all-zero enum families carry no payload, so selector checks use size=0
          val selector: VariantSelector = (on, 0, famArities)
          collectTrueVariants(selectorExpr, famArities.indices.toSet, selector)
            .flatMap(buildVariantSelectorBool(selector, _))
        case _ =>
          None
      }

    def guardToBoolExpr(guardExpr: Expr[B]): F[BoolExpr[B]] =
      directGuardBoolExpr(guardExpr)
        .orElse(selectorGuardToBoolExpr(guardExpr)) match {
        case Some(fastPath) =>
          Monad[F].pure(fastPath)
        case None           =>
          guardExpr match {
            case cheap: CheapExpr[B] =>
              Monad[F].pure(isTrueExpr(cheap))
            case notCheap =>
              makeAnon.map { tmp =>
                val guardLocal = LocalAnon(tmp)
                LetBool(Left(guardLocal), notCheap, isTrueExpr(guardLocal))
              }
          }
      }

    // Legacy ordered compiler: compile each pattern into a BoolExpr and chain
    // Ifs. This preserves the semantics of non-orthogonal patterns.
    def matchExprOrderedCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[MatchBranch],
        rootInlined: Option[InlinedStructRoot]
    ): F[Expr[B]] = {
      def recur(
          arg: CheapExpr[B],
          branches: NonEmptyList[MatchBranch]
      ): F[Expr[B]] = {
        val head = branches.head
        // Normalize to simplify list/string patterns while preserving
        // ordered semantics for non-orthogonal matches.
        val head1 = head.copy(pattern = normalizePattern(head.pattern))
        val candidateGuard =
          head1.guard.map { guard =>
            (binds: List[(Bindable, Expr[B])]) =>
              guardToBoolExpr(lets(retainReferencedBinds(guard, binds), guard))
          }

        def loop(
            cbs: NonEmptyList[
              (List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])
            ]
        ): F[Expr[B]] =
          cbs match {
            case NonEmptyList((b0, cond, binds), others) =>
              val thisBranch =
                lets(retainReferencedBinds(head1.rhs, binds), head1.rhs)

              val hasFallback = others.nonEmpty || branches.tail.nonEmpty
              val resF =
                if (hasFallback) {
                  lazy val fallbackF: F[Expr[B]] =
                    others match {
                      case oh :: ot =>
                        loop(NonEmptyList(oh, ot))
                      case Nil =>
                        recur(arg, NonEmptyList.fromListUnsafe(branches.tail))
                    }
                  fallbackF.map { fallback =>
                    cond match {
                      case TrueConst => thisBranch
                      case _         => If(cond, thisBranch, fallback)
                    }
                  }
                } else {
                  // this must be total, but we still need
                  // to evaluate cond since it can have side effects.
                  Monad[F].pure(always(cond, thisBranch))
                }

              resF.map(letMutAll(b0, _))
          }

        val mustMatchPattern = branches.tail.isEmpty && head1.guard.isEmpty
        doesMatch(
          arg,
          head1.pattern,
          mustMatchPattern,
          rootInlined,
          candidateGuard
        ).flatMap(loop)
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
    // quality depends on the column-selection heuristic.
    // See: https://compiler.club/compiling-pattern-matching/
    def matchExprMatrixCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[MatchBranch],
        inlinedRoot: Option[InlinedStructRoot]
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
          val norm = rowsIn.map(normalizeRow(_, occsMemoed, arg, inlinedRoot))
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
                        // A guarded wildcard row does not constrain the scrutinee shape;
                        // if this matrix must match, then when the guard is false the tail
                        // must still be total.
                        compileRows(tail, occs, mustMatch).map {
                          fallback =>
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
                val (colIdx, sigs) = chooseColumnByScore(rows)

                val occ = occs(colIdx)

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
                      val (newRows, keepOffsets) =
                        minimizeSpecializedRows(sig, rows, colIdx, s)
                      val fields = keepOffsets.map(i =>
                        GetEnumElement(occ, v, i, s)
                      )
                      val newOccs = occs.patch(colIdx, fields, 1)
                      val cond =
                        if (caseMustMatch) TrueConst
                        else CheckVariant(occ, v, s, f)
                      Monad[F].pure(
                        (
                          cond,
                          Nil,
                          newRows,
                          newOccs
                        )
                      )
                    case StructSig(_, s) =>
                      val (newRows, keepOffsets) =
                        minimizeSpecializedRows(sig, rows, colIdx, s)
                      val occInline =
                        rootInlinedForOcc(occ, arg, inlinedRoot)
                      val fields = keepOffsets.map(i =>
                        projectStructOccurrence(occ, i, s, occInline)
                      )
                      val newOccs = occs.patch(colIdx, fields, 1)
                      Monad[F].pure((TrueConst, Nil, newRows, newOccs))
                    case LitSig(lit) =>
                      val (newRows, _) =
                        minimizeSpecializedRows(sig, rows, colIdx, 0)
                      val newOccs = occs.patch(colIdx, Nil, 1)
                      val cond =
                        if (caseMustMatch) TrueConst else CompareLit(occ, CompareRel.Eq, lit)
                      Monad[F].pure((cond, Nil, newRows, newOccs))
                    case ZeroSig =>
                      val (newRows, _) =
                        minimizeSpecializedRows(sig, rows, colIdx, 0)
                      val newOccs = occs.patch(colIdx, Nil, 1)
                      val cond =
                        if (caseMustMatch) TrueConst
                        else EqualsNat(occ, DataRepr.ZeroNat)
                      Monad[F].pure(
                        (cond, Nil, newRows, newOccs)
                      )
                    case SuccSig =>
                      val (newRows, keepOffsets) =
                        minimizeSpecializedRows(sig, rows, colIdx, 1)
                      if (keepOffsets.nonEmpty)
                        makeAnon.map { nm =>
                          val mut = LocalAnonMut(nm)
                          val setPrev = SetMut(mut, PrevNat(occ))
                          val cond =
                            if (caseMustMatch) setPrev
                            else EqualsNat(occ, DataRepr.SuccNat) && setPrev
                          val newOccs = occs.patch(colIdx, mut :: Nil, 1)
                          (cond, mut :: Nil, newRows, newOccs)
                        }
                      else {
                        val cond =
                          if (caseMustMatch) TrueConst
                          else EqualsNat(occ, DataRepr.SuccNat)
                        Monad[F].pure((cond, Nil, newRows, occs.patch(colIdx, Nil, 1)))
                      }
                  }

                enum OrderedLiteralKind derives CanEqual {
                  case IntKind, CharKind
                }

                def orderedLiteralKind(lit: Lit): Option[OrderedLiteralKind] =
                  lit match {
                    case Lit.Integer(_) => Some(OrderedLiteralKind.IntKind)
                    case Lit.Chr(_)     => Some(OrderedLiteralKind.CharKind)
                    case _              => None
                  }

                def orderedLiteralCompare(
                    kind: OrderedLiteralKind,
                    left: Lit,
                    right: Lit
                ): Int =
                  kind match {
                    case OrderedLiteralKind.IntKind =>
                      (left, right) match {
                        case (Lit.Integer(l), Lit.Integer(r)) =>
                          l.compareTo(r)
                        case _ =>
                          // $COVERAGE-OFF$
                          throw new IllegalStateException(
                            s"unexpected Int literal comparison: left=$left, right=$right"
                          )
                        // $COVERAGE-ON$
                      }
                    case OrderedLiteralKind.CharKind =>
                      (left, right) match {
                        case (l: Lit.Chr, r: Lit.Chr) =>
                          java.lang.Integer.compare(l.toCodePoint, r.toCodePoint)
                        case _ =>
                          // $COVERAGE-OFF$
                          throw new IllegalStateException(
                            s"unexpected Char literal comparison: left=$left, right=$right"
                          )
                        // $COVERAGE-ON$
                      }
                  }

                def literalTreeLits(
                    sigs: List[HeadSig]
                ): Option[NonEmptyList[Lit]] =
                  if (sigs.length < LiteralTreeMinCases) None
                  else {
                    for {
                      literals <- sigs.traverse {
                        case LitSig(lit) => Some(lit)
                        case _           => None
                      }
                      nel <- NonEmptyList.fromList(literals)
                      kind <- orderedLiteralKind(nel.head)
                      if nel.forall(lit => orderedLiteralKind(lit).contains(kind))
                    } yield {
                      val sorted = nel.toList.sortWith { (left, right) =>
                        orderedLiteralCompare(kind, left, right) < 0
                      }
                      NonEmptyList.fromListUnsafe(sorted)
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
                          defaultRows.isEmpty
                      buildCase(sig, caseMustMatch).flatMap {
                        case (cond, preLets, newRows, newOccs) =>
                          if (newRows.isEmpty) compileCases(rest, mustMatch)
                          else {
                            val subMustMatch =
                              mustMatch
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

                def compileLiteralTreeCases(
                    sortedLits: NonEmptyList[Lit],
                    mustMatch: Boolean
                ): F[Expr[B]] = {
                  def compileLiteralCase(lit: Lit): F[(Lit, Expr[B])] = {
                    val (newRows, _) =
                      minimizeSpecializedRows(LitSig(lit), rows, colIdx, 0)
                    val newOccs = occs.patch(colIdx, Nil, 1)
                    // Sorting only changes test order; specialization by `lit`
                    // still selects the exact original rows/rhs for that literal.
                    if (newRows.isEmpty) {
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        s"expected literal specialization rows for $lit in $sigs"
                      )
                    // $COVERAGE-ON$
                    } else compileRows(newRows, newOccs, mustMatch).map((lit, _))
                  }

                  def fallbackExpr: F[Expr[B]] =
                    // Wildcard rows become `defaultRows` and represent `_`.
                    // We compile those once here as the shared fallback path.
                    if (defaultRows.nonEmpty)
                      compileRows(defaultRows, defaultOccs, mustMatch)
                    else Monad[F].pure(UnitExpr)

                  def buildTree(
                      lits: Vector[(Lit, Expr[B])],
                      fallback: Expr[B]
                  ): Expr[B] =
                    if (lits.isEmpty) fallback
                    else if (lits.length == 1) {
                      val (lit, thenExpr) = lits.head
                      If(CompareLit(occ, CompareRel.Eq, lit), thenExpr, fallback)
                    } else {
                      // Keep the left branch as all literals <= pivot.
                      val leftSize = (lits.length + 1) / 2
                      val pivot = lits(leftSize - 1)._1
                      val leftTree = buildTree(lits.take(leftSize), fallback)
                      val rightTree = buildTree(lits.drop(leftSize), fallback)
                      If(CompareLit(occ, CompareRel.Lte, pivot), leftTree, rightTree)
                    }

                  (sortedLits.traverse(compileLiteralCase), fallbackExpr).mapN {
                    (compiledLits, fallback) =>
                      buildTree(compiledLits.toList.toVector, fallback)
                  }
                }

                def enumSwitchData(
                    sigs: List[HeadSig]
                ): Option[(List[Int], NonEmptyList[HeadSig.EnumSig])] =
                  NonEmptyList.fromList(sigs).flatMap { nel =>
                    nel
                      .traverse {
                        case enumSig @ EnumSig(_, _, _, _) =>
                          Some(enumSig)
                        case _ =>
                          None
                      }
                      .flatMap { enumSigs =>
                        val famArities = enumSigs.head.famArities
                        if (
                          (enumSigs.length >= SwitchVariantMinCases) &&
                          enumSigs.forall(_.famArities == famArities)
                        ) Some((famArities, enumSigs))
                        else None
                      }
                  }

                def compileSwitchCases(
                    famArities: List[Int],
                    enumSigs: NonEmptyList[HeadSig.EnumSig],
                    mustMatch: Boolean
                ): F[Expr[B]] = {
                  def compileCase(
                      enumSig: HeadSig.EnumSig
                  ): F[(Int, Expr[B])] =
                    enumSig match {
                      case EnumSig(_, variant, arity, _) =>
                        val (newRows, keepOffsets) =
                          minimizeSpecializedRows(enumSig, rows, colIdx, arity)
                        val fields = keepOffsets.map(i =>
                          GetEnumElement(occ, variant, i, arity)
                        )
                        val newOccs = occs.patch(colIdx, fields, 1)

                        if (newRows.isEmpty) Monad[F].pure((variant, UnitExpr))
                        else {
                          val subMustMatch = mustMatch
                          compileRows(newRows, newOccs, subMustMatch).map {
                            (variant, _)
                          }
                        }
                    }

                  val hasAllVariants = enumSigs.length == famArities.length
                  val defaultExprF: F[Option[Expr[B]]] =
                    if (defaultRows.nonEmpty)
                      compileRows(defaultRows, defaultOccs, mustMatch).map(Some(_))
                    else if (!mustMatch && !hasAllVariants)
                      Monad[F].pure(Some(UnitExpr))
                    else Monad[F].pure(None)

                  (enumSigs.traverse(compileCase), defaultExprF).mapN {
                    (compiledCases, defaultExpr) =>
                      SwitchVariant(
                        occ,
                        famArities,
                        compiledCases,
                        defaultExpr
                      )
                  }
                }

                literalTreeLits(sigs) match {
                  case Some(sortedLits) =>
                    compileLiteralTreeCases(sortedLits, mustMatch)
                  case None             =>
                    enumSwitchData(sigs) match {
                      case Some((famArities, enumSigs)) =>
                        compileSwitchCases(famArities, enumSigs, mustMatch)
                      case None =>
                        compileCases(sigs, mustMatch)
                    }
                }
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
          case SwitchVariant(on, _, cases, default) =>
            1 + loopExpr(on) + cases.iterator
              .map { case (_, branch) =>
                loopExpr(branch)
              }
              .sum + default.fold(0)(loopExpr)
          case Always(cond, thenExpr) =>
            1 + loopBool(cond) + loopExpr(thenExpr)
          case PrevNat(of) =>
            1 + loopExpr(of)
          case _: CheapExpr[?] | MakeEnum(_, _, _) | MakeStruct(_) | ZeroNat |
              SuccNat =>
            1
        }

      def loopBool(b: BoolExpr[B]): Int =
        b match {
          case CompareLit(expr, _, _) =>
            1 + loopExpr(expr)
          case CompareInt(left, _, right) =>
            1 + loopExpr(left) + loopExpr(right)
          case CompareInt64(left, _, right) =>
            1 + loopExpr(left) + loopExpr(right)
          case CompareFloat64(left, _, right) =>
            1 + loopExpr(left) + loopExpr(right)
          case EqualsNat(expr, _) =>
            1 + loopExpr(expr)
          case And(left, right) =>
            1 + loopBool(left) + loopBool(right)
          case CheckVariant(expr, _, _, _) =>
            1 + loopExpr(expr)
          case CheckVariantSet(expr, _, _, _) =>
            1 + loopExpr(expr)
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
          expandPats(tr.row.pats).toList.map(ps =>
            tr.copy(row = tr.row.copy(pats = ps))
          )
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

      val dummyMut: LocalAnonMut = LocalAnonMut(Long.MinValue)

      def minimizeAlreadySpecializedTaggedRows(
          rows: List[TaggedRow],
          colIdx: Int,
          arity: Int
      ): (List[TaggedRow], List[Int]) = {
        val keepOffsets = specializedKeepOffsets(rows.map(_.row), colIdx, arity)
        if (keepOffsets.length == arity) (rows, keepOffsets)
        else {
          val trimmedRows = rows.map { tr =>
            tr.copy(
              row = tr.row.copy(
                pats = trimSpecializedPats(
                  tr.row.pats,
                  colIdx,
                  arity,
                  keepOffsets
                )
              )
            )
          }
          (trimmedRows, keepOffsets)
        }
      }

      def minimizeSpecializedTaggedRows(
          sig: HeadSig,
          rows: List[TaggedRow],
          colIdx: Int,
          arity: Int
      ): (List[TaggedRow], List[Int]) =
        minimizeAlreadySpecializedTaggedRows(
          specializeTaggedRows(sig, rows, colIdx, arity),
          colIdx,
          arity
        )

      def specializeTaggedCase(
          sig: HeadSig,
          rows: List[TaggedRow],
          occ: CheapExpr[B],
          occs: List[CheapExpr[B]],
          colIdx: Int
      ): (List[TaggedRow], List[CheapExpr[B]]) =
        sig match {
          case EnumSig(_, v, s, _) =>
            val (newRows, keepOffsets) =
              minimizeSpecializedTaggedRows(sig, rows, colIdx, s)
            val fields = keepOffsets.map(i => GetEnumElement(occ, v, i, s))
            (newRows, occs.patch(colIdx, fields, 1))
          case StructSig(_, s) =>
            val (newRows, keepOffsets) =
              minimizeSpecializedTaggedRows(sig, rows, colIdx, s)
            val fields = keepOffsets.map(i => GetStructElement(occ, i, s))
            (newRows, occs.patch(colIdx, fields, 1))
          case LitSig(_) | ZeroSig =>
            val newRows = specializeTaggedRows(sig, rows, colIdx, 0)
            (newRows, occs.patch(colIdx, Nil, 1))
          case SuccSig =>
            val newRows = specializeTaggedRows(sig, rows, colIdx, 1)
            (newRows, occs.patch(colIdx, dummyMut :: Nil, 1))
        }

      def compileRowsCount(
          rowsIn: List[TaggedRow],
          occsIn: List[CheapExpr[B]],
          mustMatch: Boolean
      ): Int = {
        val norm = rowsIn.map(tr =>
          tr.copy(row = normalizeRow(tr.row, occsIn, dummyMut, None))
        )
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
                if (tail.nonEmpty)
                  thisLeaf + compileRowsCount(tail, occs, mustMatch = false)
                else thisLeaf
            }
          case Nil =>
            0
          case _ =>
            val (colIdx, sigs) = chooseColumnByScore(rows.map(_.row))
            val occ = occs(colIdx)
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
                  val (newRows, newOccs) =
                    specializeTaggedCase(sig, rows, occ, occs, colIdx)
                  if (newRows.isEmpty) compileCasesCount(rest, mustMatch)
                  else {
                    val subMustMatch =
                      mustMatch && newRows.forall(_.row.guard.isEmpty)
                    val thenCount =
                      compileRowsCount(newRows, newOccs, subMustMatch)
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
      compileRowsCount(
        rows0,
        Local(Identifier.Name("bsts_fallback_probe")) :: Nil,
        mustMatch = true
      )
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
      def bindsWholeRoot(
          p: Pattern[(PackageName, Constructor), Type]
      ): Boolean =
        p match {
          case Pattern.Var(_) | Pattern.Named(_, _) => true
          case Pattern.Annotation(inner, _)          => bindsWholeRoot(inner)
          case Pattern.Union(h, tail) =>
            bindsWholeRoot(h) && tail.forall(bindsWholeRoot)
          case _                                    => false
        }

      val wholeRootBindBranches =
        branches.toList.count(branch => bindsWholeRoot(branch.pattern))

      // Heuristic: only pay the matrix setup cost if we can prune a few
      // orthogonal cases before falling back.
      val orthoThreshold = 4

      def boolSelectorBranches(
          bs: NonEmptyList[MatchBranch]
      ): Option[(Expr[B], Expr[B])] = {
        val truePat: Pattern[(PackageName, Constructor), Type] =
          Pattern.PositionalStruct(
            (PackageName.PredefName, Constructor("True")),
            Nil
          )
        val falsePat: Pattern[(PackageName, Constructor), Type] =
          Pattern.PositionalStruct(
            (PackageName.PredefName, Constructor("False")),
            Nil
          )

        def normalizedNoNames(
            p: Pattern[(PackageName, Constructor), Type]
        ): Option[Pattern[(PackageName, Constructor), Type]] = {
          val p1 = normalizePattern(p)
          if (p1.names.isEmpty) Some(p1)
          else None
        }

        def isTrue(p: Pattern[(PackageName, Constructor), Type]): Boolean =
          p == truePat

        def isFalseOrWild(
            p: Pattern[(PackageName, Constructor), Type]
        ): Boolean =
          (p == falsePat) || (p == Pattern.WildCard)

        bs.toList match {
          case MatchBranch(p1, None, ifTrue) :: MatchBranch(p2, None, ifFalse) :: Nil =>
            (normalizedNoNames(p1), normalizedNoNames(p2)) match {
              case (Some(np1), Some(np2)) if isTrue(np1) && isFalseOrWild(np2) =>
                Some((ifTrue, ifFalse))
              case (Some(np1), Some(np2)) if (np1 == falsePat) && (
                    (np2 == truePat) || (np2 == Pattern.WildCard)
                  ) =>
                Some((ifFalse, ifTrue))
              case _ =>
                None
            }
          case _ =>
            None
        }
      }

      def comparisonSelectorBranches(
          selectorExpr: Expr[B],
          bs: NonEmptyList[MatchBranch]
      ): Option[F[Expr[B]]] = {
        val ltPat: Pattern[(PackageName, Constructor), Type] =
          Pattern.PositionalStruct(
            (PackageName.PredefName, Constructor("LT")),
            Nil
          )
        val eqPat: Pattern[(PackageName, Constructor), Type] =
          Pattern.PositionalStruct(
            (PackageName.PredefName, Constructor("EQ")),
            Nil
          )
        val gtPat: Pattern[(PackageName, Constructor), Type] =
          Pattern.PositionalStruct(
            (PackageName.PredefName, Constructor("GT")),
            Nil
          )

        def normalizedNoNames(
            p: Pattern[(PackageName, Constructor), Type]
        ): Option[Pattern[(PackageName, Constructor), Type]] = {
          val p1 = normalizePattern(p)
          if (p1.names.isEmpty) Some(p1)
          else None
        }

        def patternVariants(
            p: Pattern[(PackageName, Constructor), Type]
        ): Option[Set[Int]] =
          p match {
            case Pattern.WildCard =>
              Some(comparisonTrueVariants)
            case `ltPat` =>
              Some(Set(0))
            case `eqPat` =>
              Some(Set(1))
            case `gtPat` =>
              Some(Set(2))
            case Pattern.Union(head, tail) =>
              (head :: tail.toList).traverse(patternVariants).map(_.foldLeft(Set.empty[Int])(_ union _))
            case _ =>
              None
          }

        def literalBoolValue(expr: Expr[B]): Option[Boolean] =
          expr match {
            case TrueExpr  => Some(true)
            case FalseExpr => Some(false)
            case _         => None
          }

        val trueVariantsOpt =
          bs.toList
            .foldLeft(Option((comparisonTrueVariants, Set.empty[Int]))) {
              case (accOpt, MatchBranch(pattern, None, rhs)) =>
                for {
                  acc <- accOpt
                  (remaining, trueVariants) = acc
                  normalized <- normalizedNoNames(pattern)
                  variants <- patternVariants(normalized)
                  branchValue <- literalBoolValue(rhs)
                } yield {
                  val covered = remaining intersect variants
                  val nextTrueVariants =
                    if (branchValue) trueVariants union covered else trueVariants
                  (remaining diff covered, nextTrueVariants)
                }
              case _ =>
                None
            }
            .collect { case (remaining, trueVariants) if remaining.isEmpty =>
              trueVariants
            }

        for {
          trueVariants <- trueVariantsOpt
          (domain, left, right) <- cmpBuiltinArgs(selectorExpr)
        } yield {
          comparisonObservation(trueVariants) match {
            case Left(value) =>
              lowerConstantBoolWithOperands(left, right, value)
            case Right(rel)  =>
              lowerBooleanCompare(domain, left, rel, right)
          }
        }
      }

      def maybeMatrix(
          arg: CheapExpr[B],
          branches: NonEmptyList[MatchBranch],
          rootInlined: Option[InlinedStructRoot]
      ): F[Expr[B]] =
        if (shouldPreferOrderedTerminalFallback(branches))
          matchExprOrderedCheap(arg, branches, rootInlined)
        else
          matchExprMatrixCheap(arg, branches, rootInlined)

      def compileWithCheapArg(
          arg: CheapExpr[B],
          branches: NonEmptyList[MatchBranch],
          rootInlined: Option[InlinedStructRoot]
      ): F[Expr[B]] = {
        boolSelectorBranches(branches) match {
          case Some((ifTrue, ifFalse)) =>
            guardToBoolExpr(arg).map(If(_, ifTrue, ifFalse))
          case None =>
            val (orthoPrefix, nonOrthoSuffix) =
              branches.toList.span(branch => !isNonOrthogonal(branch.pattern))
            val maybeNonOrthoSuffix = NonEmptyList.fromList(nonOrthoSuffix)

            maybeNonOrthoSuffix match {
              case None =>
                maybeMatrix(arg, branches, rootInlined)
              case Some(suffixNel) if orthoPrefix.length >= orthoThreshold =>
                matchExprOrderedCheap(arg, suffixNel, rootInlined).flatMap {
                  fallbackExpr =>
                    val combinedNel = NonEmptyList.ofInitLast(
                      orthoPrefix,
                      MatchBranch(Pattern.WildCard, None, fallbackExpr)
                    )
                    compileWithCheapArg(arg, combinedNel, rootInlined)
                }
              case _ =>
                matchExprOrderedCheap(arg, branches, rootInlined)
            }
        }
      }

      val maybeBoolBranches = boolSelectorBranches(branches)
      val maybeComparisonBranches = comparisonSelectorBranches(arg, branches)

      def compileWithoutInlining: F[Expr[B]] =
        maybeBoolBranches match {
          case Some((ifTrue, ifFalse)) =>
            guardToBoolExpr(arg).map(If(_, ifTrue, ifFalse))
          case None =>
            maybeComparisonBranches.getOrElse {
              maybeMemo(arg, tmp) { (arg: CheapExpr[B]) =>
                compileWithCheapArg(arg, branches, None)
              }
            }
        }

      // phase-1 policy: if multiple branches bind the whole root, keep eager root allocation
      if (wholeRootBindBranches > 1 || maybeBoolBranches.nonEmpty || maybeComparisonBranches.nonEmpty)
        compileWithoutInlining
      else {
        prepareInlinedStructRoot(arg).flatMap {
          case Some((argLets, inlinedRoot)) =>
            makeAnon
              .map(LocalAnon(_))
              .flatMap { rootOcc =>
                compileWithCheapArg(rootOcc, branches, Some(inlinedRoot))
              }
              .map(letAnons(argLets, _))
          case None =>
            compileWithoutInlining
        }
      }
    }

    loopLetVal(name, te, rec, LambdaState(None, Map.empty))
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
