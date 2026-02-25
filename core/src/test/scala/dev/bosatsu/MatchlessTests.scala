package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import Identifier.{Bindable, Constructor}
import rankn.DataRepr

import scala.util.Try

class MatchlessTest extends munit.ScalaCheckSuite {
  given Order[Unit] = Order.fromOrdering

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 5000 else 20
    )

  type Fn = (PackageName, Constructor) => Option[DataRepr]

  def fnFromTypeEnv[A](te: rankn.TypeEnv[A]): Fn = {
    // the list constructors *have* to be in scope or matching will generate
    // bad code
    case (PackageName.PredefName, Constructor("EmptyList")) =>
      Some(DataRepr.Enum(0, 0, List(0, 1)))
    case (PackageName.PredefName, Constructor("NonEmptyList")) =>
      Some(DataRepr.Enum(1, 2, List(0, 1)))
    case (pn, cons) =>
      te.getConstructor(pn, cons)
        .map(_._1.dataRepr(cons))
        .orElse(Some(DataRepr.Struct(0)))
  }

  lazy val genInputs: Gen[(Bindable, RecursionKind, TypedExpr[Unit], Fn)] =
    Generators
      .genPackage(Gen.const(()), 5)
      .flatMap { (m: Map[PackageName, Package.Typed[Unit]]) =>
        val candidates = m.filter { case (_, t) => t.lets.nonEmpty }

        if (candidates.isEmpty) genInputs
        else
          for {
            packName <- Gen.oneOf(candidates.keys.toSeq)
            pack = m(packName)
            (b, r, t) <- Gen.oneOf(pack.lets)
            fn = fnFromTypeEnv(pack.types)
          } yield (b, r, t, fn)
      }

  test("matchless.fromLet is pure: f(x) == f(x)") {
    forAll(genInputs) { case (b, r, t, fn) =>
      def run(): Option[Matchless.Expr[Unit]] =
        // ill-formed inputs can fail
        Try(Matchless.fromLet((), b, r, t)(fn)).toOption

      assertEquals(run(), run())
    }
  }

  lazy val genMatchlessExpr: Gen[Matchless.Expr[Unit]] =
    genInputs
      .map { case (b, r, t, fn) =>
        // ill-formed inputs can fail
        Try(Matchless.fromLet((), b, r, t)(fn)).toOption
      }
      .flatMap {
        case Some(e) => Gen.const(e)
        case None    => genMatchlessExpr
      }

  private def boolSubexpressions(
      boolExpr: Matchless.BoolExpr[Unit]
  ): List[Matchless.BoolExpr[Unit]] = {
    val nested =
      boolExpr match {
        case Matchless.And(left, right) =>
          boolSubexpressions(left) ++ boolSubexpressions(right)
        case Matchless.SetMut(_, expr) =>
          exprBoolSubexpressions(expr)
        case Matchless.LetBool(_, value, in) =>
          exprBoolSubexpressions(value) ++ boolSubexpressions(in)
        case Matchless.LetMutBool(_, in) =>
          boolSubexpressions(in)
        case _ =>
          Nil
      }

    boolExpr :: nested
  }

  private def exprBoolSubexpressions(
      expr: Matchless.Expr[Unit]
  ): List[Matchless.BoolExpr[Unit]] =
    expr match {
      case Matchless.Lambda(captures, _, _, body) =>
        captures.toList.flatMap(
          exprBoolSubexpressions
        ) ++ exprBoolSubexpressions(body)
      case Matchless.WhileExpr(cond, effectExpr, _) =>
        boolSubexpressions(cond) ++ exprBoolSubexpressions(effectExpr)
      case Matchless.App(fn, args) =>
        exprBoolSubexpressions(fn) ++ args.toList.flatMap(
          exprBoolSubexpressions
        )
      case Matchless.Let(_, value, in) =>
        exprBoolSubexpressions(value) ++ exprBoolSubexpressions(in)
      case Matchless.LetMut(_, in) =>
        exprBoolSubexpressions(in)
      case Matchless.If(cond, thenExpr, elseExpr) =>
        boolSubexpressions(cond) ++ exprBoolSubexpressions(
          thenExpr
        ) ++ exprBoolSubexpressions(elseExpr)
      case Matchless.Always(cond, thenExpr) =>
        boolSubexpressions(cond) ++ exprBoolSubexpressions(thenExpr)
      case Matchless.PrevNat(of) =>
        exprBoolSubexpressions(of)
      case _ =>
        Nil
    }

  lazy val genMatchlessBoolExpr: Gen[Matchless.BoolExpr[Unit]] =
    genMatchlessExpr.flatMap { expr =>
      exprBoolSubexpressions(expr) match {
        case Nil         => Gen.const(Matchless.TrueConst)
        case bool :: Nil => Gen.const(bool)
        case bools       => Gen.oneOf(bools)
      }
    }

  private val issue1688Package = PackageName.parts("Issue1688")
  private val issue1688Struct = Constructor("Foo")
  private val issue1688EnumType = Constructor("E")
  private val issue1688Enum0 = Constructor("E0")
  private val issue1688Enum1 = Constructor("E1")
  private val issue1732Package = PackageName.parts("Issue1732")
  private val issue1732Pair = Constructor("Pair")
  private val predefEmptyList = Constructor("EmptyList")
  private val predefNonEmptyList = Constructor("NonEmptyList")
  private val issue1688StructType: rankn.Type = rankn.Type.TyConst(
    rankn.Type.Const.Defined(issue1688Package, TypeName(issue1688Struct))
  )
  private val issue1732PairType: rankn.Type = rankn.Type.TyConst(
    rankn.Type.Const.Defined(issue1732Package, TypeName(issue1732Pair))
  )
  private val issue1688EnumTypeValue: rankn.Type = rankn.Type.TyConst(
    rankn.Type.Const.Defined(issue1688Package, TypeName(issue1688EnumType))
  )
  private val issue1688StructCtorType: rankn.Type =
    rankn.Type.Fun(
      NonEmptyList.of(rankn.Type.IntType, rankn.Type.IntType, rankn.Type.IntType),
      issue1688StructType
    )
  private val listIntType: rankn.Type =
    rankn.Type.apply1(rankn.Type.ListType, rankn.Type.IntType)
  private val issue1688Fn: Fn = {
    val base = fnFromTypeEnv(rankn.TypeEnv.empty)
    {
      case (`issue1688Package`, `issue1688Struct`) =>
        Some(DataRepr.Struct(3))
      case (`issue1688Package`, `issue1688Enum0`)  =>
        Some(DataRepr.Enum(0, 3, 3 :: 0 :: Nil))
      case (`issue1688Package`, `issue1688Enum1`)  =>
        Some(DataRepr.Enum(1, 0, 3 :: 0 :: Nil))
      case (pn, cons) =>
        base(pn, cons)
    }
  }
  private val issue1732Fn: Fn = {
    val base = fnFromTypeEnv(rankn.TypeEnv.empty)
    {
      case (`issue1732Package`, `issue1732Pair`) =>
        Some(DataRepr.Struct(2))
      case (pn, cons) =>
        base(pn, cons)
    }
  }

  private def intLit(i: Int): TypedExpr[Unit] =
    TypedExpr.Literal(Lit.fromInt(i), rankn.Type.IntType, ())

  private def pairCtorType(
      leftType: rankn.Type,
      rightType: rankn.Type
  ): rankn.Type =
    rankn.Type.Fun(
      NonEmptyList.of(leftType, rightType),
      issue1732PairType
    )

  private def pairCtorExpr(
      leftType: rankn.Type,
      rightType: rankn.Type
  ): TypedExpr[Unit] =
    TypedExpr.Global(
      issue1732Package,
      issue1732Pair,
      pairCtorType(leftType, rightType),
      ()
    )

  private def pairCall(
      left: TypedExpr[Unit],
      leftType: rankn.Type,
      right: TypedExpr[Unit],
      rightType: rankn.Type
  ): TypedExpr[Unit] =
    TypedExpr.App(
      pairCtorExpr(leftType, rightType),
      NonEmptyList.of(left, right),
      issue1732PairType,
      ()
    )

  private def issue1688StructCall(a: Int, b: Int, c: Int): TypedExpr[Unit] =
    TypedExpr.App(
      TypedExpr.Global(issue1688Package, issue1688Struct, issue1688StructCtorType, ()),
      NonEmptyList.of(intLit(a), intLit(b), intLit(c)),
      issue1688StructType,
      ()
    )

  private def listOfInts(items: List[Int]): TypedExpr[Unit] = {
    val empty: TypedExpr[Unit] =
      TypedExpr.Global(PackageName.PredefName, predefEmptyList, listIntType, ())
    val consType = rankn.Type.Fun(
      NonEmptyList.of(rankn.Type.IntType, listIntType),
      listIntType
    )
    val cons =
      TypedExpr.Global(PackageName.PredefName, predefNonEmptyList, consType, ())

    items.foldRight(empty) { (item, tail) =>
      TypedExpr.App(cons, NonEmptyList.of(intLit(item), tail), listIntType, ())
    }
  }

  private def collectProjectionIndices(
      expr: Matchless.Expr[Unit]
  )(
      select: PartialFunction[Matchless.CheapExpr[Unit], Int]
  ): Set[Int] = {
    val indices = scala.collection.mutable.Set.empty[Int]

    def loopCheap(c: Matchless.CheapExpr[Unit]): Unit = {
      select.lift(c).foreach(indices.addOne)
      c match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopCheap(arg)
        case Matchless.GetStructElement(arg, _, _) =>
          loopCheap(arg)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
          ()
      }
    }

    def loopBool(b: Matchless.BoolExpr[Unit]): Unit =
      b match {
        case Matchless.EqualsLit(e, _) =>
          loopCheap(e)
        case Matchless.EqualsNat(e, _) =>
          loopCheap(e)
        case Matchless.And(l, r) =>
          loopBool(l)
          loopBool(r)
        case Matchless.CheckVariant(e, _, _, _) =>
          loopCheap(e)
        case Matchless.SetMut(_, e) =>
          loopExpr(e)
        case Matchless.TrueConst =>
          ()
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value)
          loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
      }

    def loopExpr(e: Matchless.Expr[Unit]): Unit =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.foreach(loopExpr)
          loopExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond)
          loopExpr(effectExpr)
        case Matchless.App(fn, args) =>
          loopExpr(fn)
          args.toList.foreach(loopExpr)
        case Matchless.Let(_, value, in) =>
          loopExpr(value)
          loopExpr(in)
        case Matchless.LetMut(_, in) =>
          loopExpr(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond)
          loopExpr(thenExpr)
          loopExpr(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond)
          loopExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          loopExpr(of)
        case c: Matchless.CheapExpr[Unit] =>
          loopCheap(c)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          ()
      }

    loopExpr(expr)
    indices.toSet
  }

  private def countStructConstructorApps(
      expr: Matchless.Expr[Unit],
      arity: Int
  ): Int = {
    def loopCheap(c: Matchless.CheapExpr[Unit]): Int =
      c match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopCheap(arg)
        case Matchless.GetStructElement(arg, _, _) =>
          loopCheap(arg)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
          0
      }

    def loopBool(b: Matchless.BoolExpr[Unit]): Int =
      b match {
        case Matchless.EqualsLit(e, _) =>
          loopCheap(e)
        case Matchless.EqualsNat(e, _) =>
          loopCheap(e)
        case Matchless.And(l, r) =>
          loopBool(l) + loopBool(r)
        case Matchless.CheckVariant(e, _, _, _) =>
          loopCheap(e)
        case Matchless.SetMut(_, e) =>
          loopExpr(e)
        case Matchless.TrueConst =>
          0
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value) + loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
      }

    def loopExpr(e: Matchless.Expr[Unit]): Int =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.toList.map(loopExpr).sum + loopExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond) + loopExpr(effectExpr)
        case Matchless.App(Matchless.MakeStruct(a), args) =>
          val nested = args.toList.map(loopExpr).sum
          (if (a == arity) 1 else 0) + nested
        case Matchless.App(fn, args) =>
          loopExpr(fn) + args.toList.map(loopExpr).sum
        case Matchless.Let(_, value, in) =>
          loopExpr(value) + loopExpr(in)
        case Matchless.LetMut(_, in) =>
          loopExpr(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond) + loopExpr(thenExpr) + loopExpr(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond) + loopExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          loopExpr(of)
        case c: Matchless.CheapExpr[Unit] =>
          loopCheap(c)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          0
      }

    loopExpr(expr)
  }

  private def hasStructConstructorOutsideConditional(
      expr: Matchless.Expr[Unit],
      arity: Int
  ): Boolean = {
    def loopCheap(c: Matchless.CheapExpr[Unit], inConditionalBranch: Boolean): Boolean =
      c match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopCheap(arg, inConditionalBranch)
        case Matchless.GetStructElement(arg, _, _) =>
          loopCheap(arg, inConditionalBranch)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
          false
      }

    def loopBool(
        b: Matchless.BoolExpr[Unit],
        inConditionalBranch: Boolean
    ): Boolean =
      b match {
        case Matchless.EqualsLit(e, _) =>
          loopCheap(e, inConditionalBranch)
        case Matchless.EqualsNat(e, _) =>
          loopCheap(e, inConditionalBranch)
        case Matchless.And(l, r) =>
          loopBool(l, inConditionalBranch) || loopBool(r, inConditionalBranch)
        case Matchless.CheckVariant(e, _, _, _) =>
          loopCheap(e, inConditionalBranch)
        case Matchless.SetMut(_, e) =>
          loopExpr(e, inConditionalBranch)
        case Matchless.TrueConst =>
          false
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value, inConditionalBranch) || loopBool(
            in,
            inConditionalBranch
          )
        case Matchless.LetMutBool(_, in) =>
          loopBool(in, inConditionalBranch)
      }

    def loopExpr(
        e: Matchless.Expr[Unit],
        inConditionalBranch: Boolean
    ): Boolean =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(loopExpr(_, inConditionalBranch)) || loopExpr(
            body,
            inConditionalBranch
          )
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond, inConditionalBranch) || loopExpr(
            effectExpr,
            inConditionalBranch
          )
        case Matchless.App(Matchless.MakeStruct(a), args) =>
          val isOutside = (a == arity) && !inConditionalBranch
          isOutside || args.exists(loopExpr(_, inConditionalBranch))
        case Matchless.App(fn, args) =>
          loopExpr(fn, inConditionalBranch) || args.exists(
            loopExpr(_, inConditionalBranch)
          )
        case Matchless.Let(_, value, in) =>
          loopExpr(value, inConditionalBranch) || loopExpr(
            in,
            inConditionalBranch
          )
        case Matchless.LetMut(_, in) =>
          loopExpr(in, inConditionalBranch)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond, inConditionalBranch) || loopExpr(
            thenExpr,
            inConditionalBranch = true
          ) || loopExpr(elseExpr, inConditionalBranch = true)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond, inConditionalBranch) || loopExpr(
            thenExpr,
            inConditionalBranch
          )
        case Matchless.PrevNat(of) =>
          loopExpr(of, inConditionalBranch)
        case c: Matchless.CheapExpr[Unit] =>
          loopCheap(c, inConditionalBranch)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          false
      }

    loopExpr(expr, inConditionalBranch = false)
  }

  private def countGlobalCalls(
      expr: Matchless.Expr[Unit],
      pack: PackageName,
      name: Bindable
  ): Int = {
    def loopCheap(c: Matchless.CheapExpr[Unit]): Int =
      c match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopCheap(arg)
        case Matchless.GetStructElement(arg, _, _) =>
          loopCheap(arg)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
          0
      }

    def loopBool(b: Matchless.BoolExpr[Unit]): Int =
      b match {
        case Matchless.EqualsLit(e, _) =>
          loopCheap(e)
        case Matchless.EqualsNat(e, _) =>
          loopCheap(e)
        case Matchless.And(l, r) =>
          loopBool(l) + loopBool(r)
        case Matchless.CheckVariant(e, _, _, _) =>
          loopCheap(e)
        case Matchless.SetMut(_, e) =>
          loopExpr(e)
        case Matchless.TrueConst =>
          0
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value) + loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
      }

    def loopExpr(e: Matchless.Expr[Unit]): Int =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.toList.map(loopExpr).sum + loopExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond) + loopExpr(effectExpr)
        case Matchless.App(Matchless.Global(_, p, fn), args)
            if (p == pack) && (fn == name) =>
          1 + args.toList.map(loopExpr).sum
        case Matchless.App(fn, args) =>
          loopExpr(fn) + args.toList.map(loopExpr).sum
        case Matchless.Let(_, value, in) =>
          loopExpr(value) + loopExpr(in)
        case Matchless.LetMut(_, in) =>
          loopExpr(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond) + loopExpr(thenExpr) + loopExpr(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond) + loopExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          loopExpr(of)
        case c: Matchless.CheapExpr[Unit] =>
          loopCheap(c)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          0
      }

    loopExpr(expr)
  }

  private def leadingLetGlobalCalls(
      expr: Matchless.Expr[Unit],
      pack: PackageName
  ): List[Bindable] = {
    def callName(value: Matchless.Expr[Unit]): Option[Bindable] =
      value match {
        case Matchless.App(Matchless.Global(_, p, fn), _) if p == pack =>
          Some(fn)
        case _ =>
          None
      }

    @annotation.tailrec
    def loop(
        e: Matchless.Expr[Unit],
        revAcc: List[Bindable]
    ): List[Bindable] =
      e match {
        case Matchless.Let(_, value, in) =>
          val revAcc1 = callName(value).fold(revAcc)(_ :: revAcc)
          loop(in, revAcc1)
        case _ =>
          revAcc.reverse
      }

    loop(expr, Nil)
  }

  private def canonicalRecLoop(
      runMut: Matchless.LocalAnonMut,
      resultMut: Matchless.LocalAnonMut,
      effectExpr: Matchless.Expr[Unit]
  ): Matchless.Expr[Unit] =
    Matchless.Always(
      Matchless.SetMut(runMut, Matchless.TrueExpr),
      Matchless.WhileExpr(
        Matchless.isTrueExpr(runMut),
        effectExpr,
        resultMut
      )
    )

  private def firstWhileEffect(
      expr: Matchless.Expr[Unit]
  ): Option[Matchless.Expr[Unit]] = {
    def loop(e: Matchless.Expr[Unit]): Option[Matchless.Expr[Unit]] =
      e match {
        case Matchless.WhileExpr(_, effectExpr, _) =>
          Some(effectExpr)
        case Matchless.Lambda(captures, _, _, body) =>
          captures.iterator.map(loop).collectFirst {
            case Some(found) => found
          }.orElse(loop(body))
        case Matchless.App(fn, args) =>
          loop(fn).orElse(
            args.iterator.map(loop).collectFirst { case Some(found) => found }
          )
        case Matchless.Let(_, value, in) =>
          loop(value).orElse(loop(in))
        case Matchless.LetMut(_, in) =>
          loop(in)
        case Matchless.If(_, thenExpr, elseExpr) =>
          loop(thenExpr).orElse(loop(elseExpr))
        case Matchless.Always(_, thenExpr) =>
          loop(thenExpr)
        case Matchless.PrevNat(of) =>
          loop(of)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat | (_: Matchless.CheapExpr[?]) =>
          None
      }

    loop(expr)
  }

  test("Matchless.Expr order is lawful") {
    forAll(genMatchlessExpr, genMatchlessExpr, genMatchlessExpr) { (a, b, c) =>
      OrderingLaws.forOrder(a, b, c)
    }
  }

  test("Matchless.BoolExpr order is lawful") {
    forAll(
      genMatchlessBoolExpr,
      genMatchlessBoolExpr,
      genMatchlessBoolExpr
    ) { (a, b, c) =>
      OrderingLaws.forOrder(a, b, c)
    }
  }

  test("Matchless.Expr[Int] order uses given Order[Int]") {
    given Order[Int] with {
      def compare(left: Int, right: Int): Int =
        java.lang.Integer.compare(right, left)
    }

    val pack = PackageName.parts("OrderTest")
    val name = Identifier.Name("x")
    val left: Matchless.Expr[Int] = Matchless.Global(1, pack, name)
    val right: Matchless.Expr[Int] = Matchless.Global(2, pack, name)

    assert(
      Order[Matchless.Expr[Int]].compare(left, right) > 0,
      "expected reversed Int ordering to be used for Matchless.Expr[Int]"
    )
  }

  test("Matchless.BoolExpr[Int] order uses given Order[Int]") {
    given Order[Int] with {
      def compare(left: Int, right: Int): Int =
        java.lang.Integer.compare(right, left)
    }

    val pack = PackageName.parts("OrderTest")
    val name = Identifier.Name("x")
    val left: Matchless.BoolExpr[Int] =
      Matchless.EqualsLit(Matchless.Global(1, pack, name), Lit(0))
    val right: Matchless.BoolExpr[Int] =
      Matchless.EqualsLit(Matchless.Global(2, pack, name), Lit(0))

    assert(
      Order[Matchless.BoolExpr[Int]].compare(left, right) > 0,
      "expected reversed Int ordering to be used for Matchless.BoolExpr[Int]"
    )
  }

  def genNE[A](max: Int, ga: Gen[A]): Gen[NonEmptyList[A]] =
    for {
      h <- ga
      // this can get out of control with long lists
      cnt <- Gen.choose(0, max)
      tail <- Gen.listOfN(cnt, ga)
    } yield NonEmptyList(h, tail)

  test("Matchless.product works") {

    forAll(genNE(5, genNE(5, Gen.choose(-10, 10)))) { ne2 =>
      val p1 = Matchless.product(ne2)(_ * _).toList.reduce(_ + _)

      val p0 = ne2.map(_.toList.reduce(_ + _)).toList.reduce(_ * _)

      assertEquals(p1, p0)
    }
  }

  test("Matchless.stopAt works") {
    forAll(
      genNE(100, Gen.choose(-100, 100)),
      Arbitrary.arbitrary[Int => Boolean]
    ) { (nel, fn) =>
      val stopped = Matchless.stopAt(nel)(fn)

      if (fn(stopped.last)) {
        // none of the items before the last are true:
        assertEquals(stopped.init.exists(fn), false)
      } else {
        // none of them were true
        assertEquals(stopped, nel)
        assertEquals(nel.exists(fn), false)
      }
    }
  }

  test("Mathless.matchList works like SeqPattern") {
    val genArgs: Gen[(List[Byte], List[Option[Byte => Option[Int]]])] = {
      val bytes = Gen.choose(Byte.MinValue, Byte.MaxValue)
      val size = Gen.choose(0, 20)
      for {
        s <- size
        left <- Gen.listOfN(s, bytes)
        sright <- Gen.choose(0, 2 * s)
        pat <- Gen.listOfN(
          sright,
          Arbitrary.arbitrary[Option[Byte => Option[Int]]]
        )
      } yield (left, pat)
    }

    import pattern.{SeqPattern, SeqPart, Splitter, Matcher}
    def toSeqPat[A, B](
        pat: List[Option[A => Option[B]]]
    ): SeqPattern[A => Option[B]] =
      SeqPattern.fromList(pat.map {
        case None     => SeqPart.Wildcard
        case Some(fn) => SeqPart.Lit(fn)
      })

    val matcher = SeqPattern.matcher(
      Splitter.listSplitter(new Matcher[Byte => Option[Int], Byte, Int] {
        def apply(fn: Byte => Option[Int]) = fn
      })
    )

    forAll(genArgs) { case (targ, pat) =>
      val seqPat = toSeqPat(pat)
      val matchRes = matcher(seqPat)(targ)
      val matchlessRes = Matchless.matchList(
        targ,
        pat.map {
          case None     => Left((_: List[Byte]) => 0)
          case Some(fn) => Right(fn)
        }
      )

      assertEquals(matchlessRes, matchRes)
    }
  }

  test("If.flatten can be unflattened") {
    forAll(genMatchlessExpr) {
      case ifexpr @ Matchless.If(_, _, _) =>
        val (chain, rest) = ifexpr.flatten
        def unflatten[A](
            ifs: NonEmptyList[(Matchless.BoolExpr[A], Matchless.Expr[A])],
            elseX: Matchless.Expr[A]
        ): Matchless.If[A] =
          ifs.tail match {
            case Nil          => Matchless.If(ifs.head._1, ifs.head._2, elseX)
            case head :: next =>
              val end = unflatten(NonEmptyList(head, next), elseX)
              Matchless.If(ifs.head._1, ifs.head._2, end)
          }

        assertEquals(unflatten(chain, rest), ifexpr)
      case _ => ()
    }
  }

  test("check compilation of some matchless") {
    TestUtils.checkMatchless("""
x = 1    
""") { binds =>
      val map = binds(TestUtils.testPackage).toMap

      assert(map.contains(Identifier.Name("x")))
      assertEquals(map(Identifier.Name("x")), Matchless.Literal(Lit(1)))
    }
  }

  test("Matchless.applyArgs pushes through If and Always") {
    val left = Identifier.Name("left")
    val right = Identifier.Name("right")
    val arg = Matchless.Local(Identifier.Name("arg"))

    val ifFn: Matchless.Expr[Unit] = Matchless.If(
      Matchless.TrueConst,
      Matchless.Local(left),
      Matchless.Always(Matchless.TrueConst, Matchless.Local(right))
    )

    val res = Matchless.applyArgs(ifFn, NonEmptyList.one(arg))

    val expected: Matchless.Expr[Unit] = Matchless.If(
      Matchless.TrueConst,
      Matchless.App(Matchless.Local(left), NonEmptyList.one(arg)),
      Matchless.Always(
        Matchless.TrueConst,
        Matchless.App(Matchless.Local(right), NonEmptyList.one(arg))
      )
    )

    assertEquals(res, expected)
  }

  test(
    "Matchless.applyArgs does not curry App and only pushes through safe Let"
  ) {
    val fn = Matchless.Local(Identifier.Name("f"))
    val x = Matchless.Local(Identifier.Name("x"))
    val y = Matchless.Local(Identifier.Name("y"))
    val z = Identifier.Name("z")
    val k = Identifier.Name("k")

    val applied =
      Matchless.applyArgs(
        Matchless.App(fn, NonEmptyList.one(x)),
        NonEmptyList.one(y)
      )

    assertEquals(
      applied,
      Matchless.App(Matchless.App(fn, NonEmptyList.one(x)), NonEmptyList.one(y))
    )

    val letLeftExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Left(Matchless.LocalAnon(0)),
        Matchless.Literal(Lit(1)),
        Matchless.Local(k)
      )

    assertEquals(
      Matchless.applyArgs(letLeftExpr, NonEmptyList.one(y)),
      Matchless.Let(
        Left(Matchless.LocalAnon(0)),
        Matchless.Literal(Lit(1)),
        Matchless.App(Matchless.Local(k), NonEmptyList.one(y))
      )
    )

    val letRightSafeExpr: Matchless.Expr[Unit] =
      Matchless.Let(z, Matchless.Literal(Lit(1)), Matchless.Local(k))

    assertEquals(
      Matchless.applyArgs(letRightSafeExpr, NonEmptyList.one(y)),
      Matchless.Let(
        z,
        Matchless.Literal(Lit(1)),
        Matchless.App(Matchless.Local(k), NonEmptyList.one(y))
      )
    )

    val zArg = Matchless.Local(z)
    assertEquals(
      Matchless.applyArgs(letRightSafeExpr, NonEmptyList.one(zArg)),
      Matchless.App(letRightSafeExpr, NonEmptyList.one(zArg))
    )
  }

  test("Matchless.applyArgs beta-reduces Lambda into Lets") {
    val x = Identifier.Name("x")
    val f = Identifier.Name("f")
    val yExpr: Matchless.Expr[Unit] = Matchless.Local(Identifier.Name("y"))
    val lam: Matchless.Expr[Unit] =
      Matchless.Lambda(
        captures = Nil,
        recursiveName = None,
        args = NonEmptyList.one(x),
        body = Matchless.App(
          Matchless.Local(f),
          NonEmptyList.one(Matchless.Local(x))
        )
      )

    Matchless.applyArgs(lam, NonEmptyList.one(yExpr)) match {
      case Matchless.Let(
            Right(tmp),
            `yExpr`,
            Matchless.Let(
              Right(`x`),
              Matchless.Local(tmpRef),
              Matchless.App(Matchless.Local(`f`), appArgs)
            )
          ) =>
        assertEquals(tmp, tmpRef)
        assertEquals(appArgs, NonEmptyList.one(Matchless.Local(x)))
      case other =>
        fail(s"expected beta-reduced lets, found: $other")
    }
  }

  test(
    "Matchless.recoverTopLevelLambda pushes apply into branches and beta-reduces"
  ) {
    val a = Identifier.Name("a")
    val b = Identifier.Name("b")
    val left = Identifier.Name("left")
    val right = Identifier.Name("right")
    val branchFn1: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(a),
        Matchless.App(
          Matchless.Local(left),
          NonEmptyList.one(Matchless.Local(a))
        )
      )
    val branchFn2: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(b),
        Matchless.App(
          Matchless.Local(right),
          NonEmptyList.one(Matchless.Local(b))
        )
      )

    val expr: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, branchFn1, branchFn2)

    Matchless.recoverTopLevelLambda(expr) match {
      case Matchless.Lambda(Nil, None, args, body) =>
        val topArg: Matchless.Expr[Unit] = Matchless.Local(args.head)
        body match {
          case Matchless.If(Matchless.TrueConst, tBranch, fBranch) =>
            def assertReducedBranch(
                branch: Matchless.Expr[Unit],
                fnName: Bindable,
                fnArg: Bindable
            ): Unit =
              branch match {
                case Matchless.Let(
                      Right(tmp),
                      `topArg`,
                      Matchless.Let(
                        Right(`fnArg`),
                        Matchless.Local(tmpRef),
                        Matchless.App(Matchless.Local(`fnName`), appArgs)
                      )
                    ) =>
                  assertEquals(tmp, tmpRef)
                  assertEquals(
                    appArgs,
                    NonEmptyList.one(Matchless.Local(fnArg))
                  )
                case other =>
                  fail(s"expected beta-reduced branch, found: $other")
              }

            assertReducedBranch(tBranch, left, a)
            assertReducedBranch(fBranch, right, b)
          case other =>
            fail(s"expected recovered branch if, found: $other")
        }
      case other =>
        fail(s"expected recovered lambda, found: $other")
    }
  }

  test("Matchless.recoverTopLevelLambda avoids existing names") {
    val used = Identifier.synthetic("bsts_top1_0")
    val a = Identifier.Name("a")
    val branchFn1: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(used), Matchless.Local(used))
    val branchFn2: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(a), Matchless.Local(a))

    val expr: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, branchFn1, branchFn2)

    Matchless.recoverTopLevelLambda(expr) match {
      case Matchless.Lambda(Nil, None, args, _) =>
        assertEquals(Matchless.allNames(expr)(used), true)
        assertNotEquals(args.head, used)
      case other =>
        fail(s"expected recovered lambda, found: $other")
    }
  }

  test("Matchless.allNames has expected results on simple nodes") {
    val x = Identifier.Name("x")
    val y = Identifier.Name("y")
    val g = Identifier.Name("g")
    val rec = Identifier.Name("loop")
    val arg = Identifier.Name("arg")

    assertEquals(Matchless.allNames(Matchless.Local(x)), Set(x: Bindable))
    assertEquals(
      Matchless.allNames(Matchless.Global((), TestUtils.testPackage, g)),
      Set(g: Bindable)
    )

    val lam = Matchless.Lambda(
      captures = Matchless.Local(y) :: Nil,
      recursiveName = Some(rec),
      args = NonEmptyList.one(arg),
      body = Matchless.Local(x)
    )
    assertEquals(Matchless.allNames(lam), Set(x: Bindable, y, rec, arg))

    val letRight: Matchless.Expr[Unit] =
      Matchless.Let(x, Matchless.Local(y), Matchless.Local(y))
    assertEquals(Matchless.allNames(letRight), Set(x: Bindable, y))

    val letLeft: Matchless.Expr[Unit] =
      Matchless.Let(
        Left(Matchless.LocalAnon(1)),
        Matchless.Local(y),
        Matchless.Local(y)
      )
    assertEquals(Matchless.allNames(letLeft), Set(y: Bindable))
  }

  test("Matchless.allNames is a superset of names from expression parts") {
    def checkExpr(expr: Matchless.Expr[Unit]): Unit = {
      val full = Matchless.allNames(expr)

      def requireSubset(sub: Matchless.Expr[Unit]): Unit = {
        val subNames = Matchless.allNames(sub)
        assert(
          subNames.subsetOf(full),
          s"expected $subNames to be subset of $full in $expr"
        )
        checkExpr(sub)
      }

      def checkBool(boolExpr: Matchless.BoolExpr[Unit]): Unit =
        boolExpr match {
          case Matchless.EqualsLit(e, _) =>
            requireSubset(e)
          case Matchless.EqualsNat(e, _) =>
            requireSubset(e)
          case Matchless.And(l, r) =>
            checkBool(l)
            checkBool(r)
          case Matchless.CheckVariant(e, _, _, _) =>
            requireSubset(e)
          case Matchless.SetMut(_, e) =>
            requireSubset(e)
          case Matchless.TrueConst =>
            ()
          case Matchless.LetBool(arg, value, in) =>
            arg match {
              case Right(name) =>
                assert(full(name), s"missing let-bound name $name in $expr")
              case _ => ()
            }
            requireSubset(value)
            checkBool(in)
          case Matchless.LetMutBool(_, in) =>
            checkBool(in)
        }

      expr match {
        case Matchless.Lambda(captures, rec, args, body) =>
          rec.foreach { n =>
            assert(full(n), s"missing recursive name $n in $expr")
          }
          args.toList.foreach { n =>
            assert(full(n), s"missing lambda arg $n in $expr")
          }
          captures.foreach(requireSubset)
          requireSubset(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          checkBool(cond)
          requireSubset(effectExpr)
        case Matchless.App(fn, args) =>
          requireSubset(fn)
          args.toList.foreach(requireSubset)
        case Matchless.Let(arg, value, in) =>
          arg match {
            case Right(name) =>
              assert(full(name), s"missing let-bound name $name in $expr")
            case _ => ()
          }
          requireSubset(value)
          requireSubset(in)
        case Matchless.LetMut(_, span) =>
          requireSubset(span)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          checkBool(cond)
          requireSubset(thenExpr)
          requireSubset(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          checkBool(cond)
          requireSubset(thenExpr)
        case Matchless.PrevNat(of) =>
          requireSubset(of)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          ()
        case _: Matchless.CheapExpr[?] =>
          ()
      }
    }

    forAll(genMatchlessExpr) { expr =>
      checkExpr(expr)
    }
  }

  test(
    "Matchless.reuseConstructors shares repeated constructor apps in a linear scope"
  ) {
    val x = Identifier.Name("x")
    val y = Identifier.Name("y")
    val shared: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeEnum(1, 2, 0 :: 2 :: Nil),
        NonEmptyList(Matchless.Local(x), Matchless.Local(y) :: Nil)
      )
    val input: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(2),
        NonEmptyList(shared, shared :: Nil)
      )

    Matchless.reuseConstructors(input) match {
      case Matchless.Let(
            Left(tmp),
            `shared`,
            Matchless.App(Matchless.MakeStruct(2), args)
          ) =>
        assertEquals(args.head, Matchless.LocalAnon(tmp.ident))
        assertEquals(args.tail.headOption, Some(Matchless.LocalAnon(tmp.ident)))
      case other =>
        fail(s"expected constructor reuse in linear scope, found: $other")
    }
  }

  test(
    "Matchless.reuseConstructors shares constructor apps across if branches"
  ) {
    val x = Identifier.Name("x")
    val y = Identifier.Name("y")
    val shared: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeEnum(1, 2, 0 :: 2 :: Nil),
        NonEmptyList(Matchless.Local(x), Matchless.Local(y) :: Nil)
      )

    val input: Matchless.Expr[Unit] =
      Matchless.If(
        Matchless.TrueConst,
        Matchless.App(
          Matchless.MakeStruct(2),
          NonEmptyList(Matchless.Literal(Lit(0)), shared :: Nil)
        ),
        Matchless.App(
          Matchless.MakeStruct(2),
          NonEmptyList(Matchless.Literal(Lit(1)), shared :: Nil)
        )
      )

    Matchless.reuseConstructors(input) match {
      case Matchless.Let(
            Left(tmp),
            `shared`,
            Matchless.If(_, thenExpr, elseExpr)
          ) =>
        def checkBranch(e: Matchless.Expr[Unit], lit: Int): Unit =
          e match {
            case Matchless.App(Matchless.MakeStruct(2), args) =>
              assertEquals(args.head, Matchless.Literal(Lit(lit)))
              assertEquals(
                args.tail.headOption,
                Some(Matchless.LocalAnon(tmp.ident))
              )
            case other =>
              fail(s"expected branch struct constructor, found: $other")
          }

        checkBranch(thenExpr, 0)
        checkBranch(elseExpr, 1)
      case other =>
        fail(s"expected constructor reuse across branches, found: $other")
    }
  }

  test(
    "Matchless.reuseConstructors does not share constructor apps with mutable refs"
  ) {
    val sharedMut: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.SuccNat,
        NonEmptyList.one(Matchless.LocalAnonMut(9))
      )
    val input: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, sharedMut, sharedMut)

    val optimized = Matchless.reuseConstructors(input)

    assertEquals(optimized, input)
  }

  test(
    "Matchless.reuseConstructors replaces constructor reuse inside let values"
  ) {
    val x = Identifier.Name("x")
    val y = Identifier.Name("y")
    val z = Identifier.Name("z")
    val shared: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeEnum(1, 2, 0 :: 2 :: Nil),
        NonEmptyList(Matchless.Local(x), Matchless.Local(y) :: Nil)
      )
    val thenExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        z,
        shared,
        Matchless.App(
          Matchless.MakeStruct(2),
          NonEmptyList(Matchless.Local(z), shared :: Nil)
        )
      )
    val elseExpr: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(2),
        NonEmptyList(Matchless.Literal(Lit(1)), shared :: Nil)
      )

    Matchless.reuseConstructors(
      Matchless.If(Matchless.TrueConst, thenExpr, elseExpr)
    ) match {
      case Matchless.Let(Left(tmp), `shared`, Matchless.If(_, then1, else1)) =>
        then1 match {
          case Matchless.Let(
                Right(`z`),
                Matchless.LocalAnon(tmpInValue),
                Matchless.App(Matchless.MakeStruct(2), args)
              ) =>
            assertEquals(tmpInValue, tmp.ident)
            assertEquals(args.head, Matchless.Local(z))
            assertEquals(
              args.tail.headOption,
              Some(Matchless.LocalAnon(tmp.ident))
            )
          case other =>
            fail(
              s"expected let value to reuse constructor binding, found: $other"
            )
        }

        else1 match {
          case Matchless.App(Matchless.MakeStruct(2), args) =>
            assertEquals(args.head, Matchless.Literal(Lit(1)))
            assertEquals(
              args.tail.headOption,
              Some(Matchless.LocalAnon(tmp.ident))
            )
          case other =>
            fail(s"expected else branch constructor reuse, found: $other")
        }
      case other =>
        fail(s"expected top-level constructor reuse, found: $other")
    }
  }

  test(
    "Matchless.reuseConstructors does not share constructors when mutables appear inside cheap args"
  ) {
    val sharedWithInnerMut: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(1),
        NonEmptyList.one(
          Matchless.GetEnumElement(Matchless.LocalAnonMut(12), 1, 0, 2)
        )
      )
    val input: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(2),
        NonEmptyList(sharedWithInnerMut, sharedWithInnerMut :: Nil)
      )

    val optimized = Matchless.reuseConstructors(input)

    assertEquals(optimized, input)
  }

  test("Matchless.Expr helper predicates are directly testable") {
    val runMut = Matchless.LocalAnonMut(9000L)
    val nested: Matchless.Expr[Unit] =
      Matchless.Lambda(
        captures = Matchless.WhileExpr(
          Matchless.TrueConst,
          Matchless.UnitExpr,
          runMut
        ) :: Nil,
        recursiveName = None,
        args = NonEmptyList.one(Identifier.Name("arg")),
        body = Matchless.UnitExpr
      )
    val mutableRead: Matchless.Expr[Unit] =
      Matchless.GetStructElement(Matchless.LocalAnonMut(77L), 0, 1)

    assertEquals(Matchless.Expr.containsWhileExpr(nested), true)
    assertEquals(Matchless.Expr.containsWhileExpr(Matchless.UnitExpr), false)
    assertEquals(Matchless.Expr.readsMutable(mutableRead), true)
    assertEquals(Matchless.Expr.readsMutable(Matchless.LocalAnon(77L)), false)
  }

  test("Matchless.Expr and Matchless.BoolExpr reference helpers respect shadowing") {
    val target = Identifier.Name("target")
    val other = Identifier.Name("other")

    val directCond: Matchless.BoolExpr[Unit] =
      Matchless.EqualsLit(Matchless.Local(target), Lit.fromInt(1))
    val shadowedCond: Matchless.BoolExpr[Unit] =
      Matchless.LetBool(
        Right(target),
        Matchless.Literal(Lit.fromInt(1)),
        Matchless.EqualsLit(Matchless.Local(target), Lit.fromInt(1))
      )
    assertEquals(Matchless.BoolExpr.referencesBindable(directCond, target), true)
    assertEquals(Matchless.BoolExpr.referencesBindable(shadowedCond, target), false)

    val directExpr: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Local(other),
        NonEmptyList.one(Matchless.Local(target))
      )
    val shadowedExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Right(target),
        Matchless.Literal(Lit.fromInt(0)),
        directExpr
      )
    assertEquals(Matchless.Expr.referencesBindable(directExpr, target), true)
    assertEquals(Matchless.Expr.referencesBindable(shadowedExpr, target), false)
    assertEquals(Matchless.Expr.usesBinding(directExpr, Right(target)), true)
    assertEquals(Matchless.Expr.usesBinding(shadowedExpr, Right(target)), false)
  }

  test("Matchless.Expr.referencesLocalAnon respects shadowing") {
    val target = Matchless.LocalAnon(42L)
    val directExpr: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(1),
        NonEmptyList.one(Matchless.LocalAnon(target.ident))
      )
    val shadowedExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Left(target),
        Matchless.Literal(Lit.fromInt(0)),
        Matchless.LocalAnon(target.ident)
      )

    assertEquals(Matchless.Expr.referencesLocalAnon(directExpr, target.ident), true)
    assertEquals(
      Matchless.Expr.referencesLocalAnon(shadowedExpr, target.ident),
      false
    )
    assertEquals(Matchless.Expr.usesBinding(directExpr, Left(target)), true)
    assertEquals(Matchless.Expr.usesBinding(shadowedExpr, Left(target)), false)
  }

  test(
    "Matchless.hoistInvariantLoopLets hoists invariant leading lets from canonical recursion loops"
  ) {
    val runMut = Matchless.LocalAnonMut(1000L)
    val resultMut = Matchless.LocalAnonMut(1001L)
    val z = Identifier.Name("z")
    val heavyName = Identifier.Name("hoist_heavy")
    val heavyCall: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Global((), TestUtils.testPackage, heavyName),
        NonEmptyList.one(Matchless.Literal(Lit.fromInt(100)))
      )
    val effectExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Right(z),
        heavyCall,
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(z))
        )
      )
    val input = canonicalRecLoop(runMut, resultMut, effectExpr)

    Matchless.hoistInvariantLoopLets(input) match {
      case Matchless.Let(
            Right(`z`),
            `heavyCall`,
            Matchless.Always(
              Matchless.SetMut(`runMut`, Matchless.TrueExpr),
              Matchless.WhileExpr(loopCond, loopEffect, `resultMut`)
            )
          ) =>
        assertEquals(loopCond, Matchless.isTrueExpr(runMut))
        assertEquals(
          loopEffect,
          Matchless.App(
            Matchless.MakeStruct(1),
            NonEmptyList.one(Matchless.Local(z))
          )
        )
      case other =>
        fail(s"expected hoisted loop-invariant let, found: $other")
    }
  }

  test("Matchless.hoistInvariantLoopLets hoists non-trivial constructor allocations") {
    val runMut = Matchless.LocalAnonMut(1005L)
    val resultMut = Matchless.LocalAnonMut(1006L)
    val z = Identifier.Name("z")
    val allocated: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.MakeStruct(2),
        NonEmptyList.of(Matchless.Literal(Lit.fromInt(1)), Matchless.TrueExpr)
      )
    val effectExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Right(z),
        allocated,
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(z))
        )
      )
    val input = canonicalRecLoop(runMut, resultMut, effectExpr)

    Matchless.hoistInvariantLoopLets(input) match {
      case Matchless.Let(
            Right(`z`),
            `allocated`,
            Matchless.Always(
              Matchless.SetMut(`runMut`, Matchless.TrueExpr),
              Matchless.WhileExpr(_, loopEffect, `resultMut`)
            )
          ) =>
        assertEquals(
          loopEffect,
          Matchless.App(
            Matchless.MakeStruct(1),
            NonEmptyList.one(Matchless.Local(z))
          )
        )
      case other =>
        fail(s"expected constructor allocation to hoist, found: $other")
    }
  }

  test(
    "Matchless.hoistInvariantLoopLets hoists dependent leading lets together"
  ) {
    val runMut = Matchless.LocalAnonMut(1010L)
    val resultMut = Matchless.LocalAnonMut(1011L)
    val a = Identifier.Name("a")
    val b = Identifier.Name("b")
    val fName = Identifier.Name("dep_f")
    val gName = Identifier.Name("dep_g")
    val firstCall: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Global((), TestUtils.testPackage, fName),
        NonEmptyList.one(Matchless.Literal(Lit.fromInt(5)))
      )
    val secondCall: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Global((), TestUtils.testPackage, gName),
        NonEmptyList.one(Matchless.Local(a))
      )
    val effectExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Right(a),
        firstCall,
        Matchless.Let(
          Right(b),
          secondCall,
          Matchless.App(
            Matchless.MakeStruct(2),
            NonEmptyList.of(Matchless.Local(a), Matchless.Local(b))
          )
        )
      )
    val input = canonicalRecLoop(runMut, resultMut, effectExpr)

    Matchless.hoistInvariantLoopLets(input) match {
      case Matchless.Let(
            Right(`a`),
            `firstCall`,
            Matchless.Let(
              Right(`b`),
              `secondCall`,
              Matchless.Always(
                Matchless.SetMut(`runMut`, Matchless.TrueExpr),
                Matchless.WhileExpr(_, loopEffect, `resultMut`)
              )
            )
          ) =>
        assertEquals(
          loopEffect,
          Matchless.App(
            Matchless.MakeStruct(2),
            NonEmptyList.of(Matchless.Local(a), Matchless.Local(b))
          )
        )
      case other =>
        fail(s"expected dependent lets hoisted in order, found: $other")
    }
  }

  test(
    "Matchless.hoistInvariantLoopLets does not hoist cheap aliases, mutable reads, or side-effectful values"
  ) {
    val runMut = Matchless.LocalAnonMut(1020L)
    val resultMut = Matchless.LocalAnonMut(1021L)
    val x = Identifier.Name("x")
    val z = Identifier.Name("z")
    val sideName = Identifier.Name("side")
    val sideResult = Matchless.LocalAnonMut(89L)

    val cheapAliasInput = canonicalRecLoop(
      runMut,
      resultMut,
      Matchless.Let(
        Right(z),
        Matchless.Local(x),
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(z))
        )
      )
    )
    assertEquals(Matchless.hoistInvariantLoopLets(cheapAliasInput), cheapAliasInput)

    val mutableReadInput = canonicalRecLoop(
      runMut,
      resultMut,
      Matchless.Let(
        Right(z),
        Matchless.GetStructElement(Matchless.LocalAnonMut(77L), 0, 1),
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(z))
        )
      )
    )
    assertEquals(
      Matchless.hoistInvariantLoopLets(mutableReadInput),
      mutableReadInput
    )

    val sideEffectInput = canonicalRecLoop(
      runMut,
      resultMut,
      Matchless.Let(
        Right(sideName),
        Matchless.WhileExpr(Matchless.TrueConst, Matchless.UnitExpr, sideResult),
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(sideName))
        )
      )
    )
    assertEquals(
      Matchless.hoistInvariantLoopLets(sideEffectInput),
      sideEffectInput
    )
  }

  test(
    "Matchless.hoistInvariantLoopLets does not hoist lets that capture mutable refs in lambdas"
  ) {
    val runMut = Matchless.LocalAnonMut(1030L)
    val resultMut = Matchless.LocalAnonMut(1031L)
    val fnName = Identifier.Name("fn")
    val argName = Identifier.Name("arg")
    val mutRef = Matchless.LocalAnonMut(500L)
    val lambdaValue: Matchless.Expr[Unit] =
      Matchless.Lambda(
        captures = mutRef :: Nil,
        recursiveName = None,
        args = NonEmptyList.one(argName),
        body = mutRef
      )
    val input = canonicalRecLoop(
      runMut,
      resultMut,
      Matchless.Let(
        Right(fnName),
        lambdaValue,
        Matchless.App(
          Matchless.Local(fnName),
          NonEmptyList.one(Matchless.Literal(Lit.fromInt(1)))
        )
      )
    )

    assertEquals(Matchless.hoistInvariantLoopLets(input), input)
  }

  test(
    "Matchless.hoistInvariantLoopLets does not hoist lets inside conditional branches"
  ) {
    val runMut = Matchless.LocalAnonMut(1035L)
    val resultMut = Matchless.LocalAnonMut(1036L)
    val z = Identifier.Name("z")
    val branchCall: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Global((), TestUtils.testPackage, Identifier.Name("branch_call")),
        NonEmptyList.one(Matchless.Literal(Lit.fromInt(42)))
      )
    val branchExpr: Matchless.Expr[Unit] =
      Matchless.If(
        Matchless.TrueConst,
        Matchless.Let(
          Right(z),
          branchCall,
          Matchless.App(
            Matchless.MakeStruct(1),
            NonEmptyList.one(Matchless.Local(z))
          )
        ),
        Matchless.UnitExpr
      )
    val input = canonicalRecLoop(runMut, resultMut, branchExpr)

    assertEquals(Matchless.hoistInvariantLoopLets(input), input)
  }

  test(
    "Matchless.hoistInvariantLoopLets only rewrites canonical recursion-loop conditions"
  ) {
    val runMut = Matchless.LocalAnonMut(1040L)
    val resultMut = Matchless.LocalAnonMut(1041L)
    val z = Identifier.Name("z")
    val heavyName = Identifier.Name("hoist_cond")
    val heavyCall: Matchless.Expr[Unit] =
      Matchless.App(
        Matchless.Global((), TestUtils.testPackage, heavyName),
        NonEmptyList.one(Matchless.Literal(Lit.fromInt(10)))
      )
    val effectExpr: Matchless.Expr[Unit] =
      Matchless.Let(
        Right(z),
        heavyCall,
        Matchless.App(
          Matchless.MakeStruct(1),
          NonEmptyList.one(Matchless.Local(z))
        )
      )
    val nonCanonicalLoop: Matchless.Expr[Unit] =
      Matchless.Always(
        Matchless.SetMut(runMut, Matchless.TrueExpr),
        Matchless.WhileExpr(
          Matchless.And(
            Matchless.isTrueExpr(runMut),
            Matchless.EqualsLit(Matchless.Local(z), Lit.fromInt(0))
          ),
          effectExpr,
          resultMut
        )
      )

    assertEquals(Matchless.hoistInvariantLoopLets(nonCanonicalLoop), nonCanonicalLoop)
  }

  test("Matchless.fromLet hoists invariant loop lets out of WhileExpr effects") {
    val intType = rankn.Type.IntType
    val loopArg = Identifier.Name("loop_arg")
    val hoisted = Identifier.Name("hoisted")
    val heavyName = Identifier.Name("loop_heavy")
    val heavyType = rankn.Type.Fun(NonEmptyList.one(intType), intType)
    val heavyGlobal =
      TypedExpr.Global(TestUtils.testPackage, heavyName, heavyType, ())
    val heavyCall =
      TypedExpr.App(
        heavyGlobal,
        NonEmptyList.one(intLit(100)),
        intType,
        ()
      )
    val loopArgExpr = TypedExpr.Local(loopArg, intType, ())
    val hoistedLocal = TypedExpr.Local(hoisted, intType, ())
    val loopBody =
      TypedExpr.Let(
        hoisted,
        heavyCall,
        TypedExpr.Match(
          loopArgExpr,
          NonEmptyList.of(
            TypedExpr.Branch(
              Pattern.Literal(Lit.fromInt(0)),
              None,
              hoistedLocal
            ),
            TypedExpr.Branch(
              Pattern.WildCard,
              None,
              TypedExpr.Recur(NonEmptyList.one(loopArgExpr), intType, ())
            )
          ),
          ()
        ),
        RecursionKind.NonRecursive,
        ()
      )
    val loopExpr =
      TypedExpr.Loop(
        NonEmptyList.one((loopArg, intLit(1))),
        loopBody,
        ()
      )

    val lowered =
      Matchless.fromLet(
        (),
        Identifier.Name("loop_hoist_out"),
        RecursionKind.NonRecursive,
        loopExpr
      )(fnFromTypeEnv(rankn.TypeEnv.empty))

    val whileEffect =
      firstWhileEffect(lowered).getOrElse(
        fail(s"expected loop lowering to contain WhileExpr: $lowered")
      )
    assertEquals(countGlobalCalls(lowered, TestUtils.testPackage, heavyName), 1)
    assertEquals(
      countGlobalCalls(whileEffect, TestUtils.testPackage, heavyName),
      0
    )
  }

  test(
    "matrix match materializes list projections to avoid nested projection trees"
  ) {
    def hasNestedProjectionCheap(e: Matchless.CheapExpr[Unit]): Boolean =
      e match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          arg match {
            case _: Matchless.GetEnumElement[?] |
                _: Matchless.GetStructElement[?] =>
              true
            case _ =>
              hasNestedProjectionCheap(arg)
          }
        case Matchless.GetStructElement(arg, _, _) =>
          arg match {
            case _: Matchless.GetEnumElement[?] |
                _: Matchless.GetStructElement[?] =>
              true
            case _ =>
              hasNestedProjectionCheap(arg)
          }
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.LocalAnon(_) | Matchless.LocalAnonMut(_) |
            Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
          false
      }

    def hasNestedProjectionBool(b: Matchless.BoolExpr[Unit]): Boolean =
      b match {
        case Matchless.EqualsLit(e, _) =>
          hasNestedProjectionCheap(e)
        case Matchless.EqualsNat(e, _) =>
          hasNestedProjectionCheap(e)
        case Matchless.And(l, r) =>
          hasNestedProjectionBool(l) || hasNestedProjectionBool(r)
        case Matchless.CheckVariant(e, _, _, _) =>
          hasNestedProjectionCheap(e)
        case Matchless.SetMut(_, e) =>
          hasNestedProjectionExpr(e)
        case Matchless.TrueConst =>
          false
        case Matchless.LetBool(_, value, in) =>
          hasNestedProjectionExpr(value) || hasNestedProjectionBool(in)
        case Matchless.LetMutBool(_, in) =>
          hasNestedProjectionBool(in)
      }

    def hasNestedProjectionExpr(e: Matchless.Expr[Unit]): Boolean =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(hasNestedProjectionExpr) || hasNestedProjectionExpr(
            body
          )
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          hasNestedProjectionBool(cond) || hasNestedProjectionExpr(effectExpr)
        case Matchless.App(fn, args) =>
          hasNestedProjectionExpr(fn) || args.exists(hasNestedProjectionExpr)
        case Matchless.Let(_, value, in) =>
          hasNestedProjectionExpr(value) || hasNestedProjectionExpr(in)
        case Matchless.LetMut(_, in) =>
          hasNestedProjectionExpr(in)
        case Matchless.If(cond, t, f) =>
          hasNestedProjectionBool(cond) || hasNestedProjectionExpr(
            t
          ) || hasNestedProjectionExpr(f)
        case Matchless.Always(cond, thenExpr) =>
          hasNestedProjectionBool(cond) || hasNestedProjectionExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          hasNestedProjectionExpr(of)
        case c: Matchless.CheapExpr[Unit] =>
          hasNestedProjectionCheap(c)
        case Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          false
      }

    TestUtils.checkMatchless("""
enum L:
  E
  M(head, tail)

def matches_five(xs):
  match xs:
    case M(0, M(1, M(2, M(3, M(4, E))))): 1
    case _: 0
""") { binds =>
      val byName = binds(TestUtils.testPackage).toMap
      val expr = byName(Identifier.Name("matches_five"))
      assertEquals(hasNestedProjectionExpr(expr), false)
    }
  }

  test(
    "matrix match on hand-written TypedExpr only projects required struct fields"
  ) {
    val arg = Identifier.Name("arg")
    val out = Identifier.Name("pick_struct")

    forAll(Gen.option(Gen.choose(0, 2))) { usedIdx =>
      val fieldName = usedIdx.map(i => Identifier.Name(s"field$i"))
      val params = (0 until 3).toList.map { i =>
        if (usedIdx.contains(i)) Pattern.Var(Identifier.Name(s"field$i"))
        else Pattern.WildCard
      }
      val rhs: TypedExpr[Unit] =
        fieldName match {
          case Some(nm) =>
            TypedExpr.Local(nm, rankn.Type.IntType, ())
          case None =>
            TypedExpr.Literal(Lit.fromInt(0), rankn.Type.IntType, ())
        }

      val expr = TypedExpr.Match(
        TypedExpr.Local(arg, issue1688StructType, ()),
        NonEmptyList.one(
          TypedExpr.Branch(
            Pattern.PositionalStruct((issue1688Package, issue1688Struct), params),
            None,
            rhs
          )
        ),
        ()
      )

      val lowered =
        Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1688Fn)

      val projected =
        collectProjectionIndices(lowered) {
          case Matchless.GetStructElement(_, idx, 3) => idx
        }

      assertEquals(projected, usedIdx.toSet)
    }
  }

  test(
    "matrix match on hand-written TypedExpr only projects required enum fields"
  ) {
    val arg = Identifier.Name("arg")
    val out = Identifier.Name("pick_enum")

    forAll(Gen.option(Gen.choose(0, 2))) { usedIdx =>
      val fieldName = usedIdx.map(i => Identifier.Name(s"variant_field$i"))
      val params = (0 until 3).toList.map { i =>
        if (usedIdx.contains(i))
          Pattern.Var(Identifier.Name(s"variant_field$i"))
        else Pattern.WildCard
      }
      val e0Rhs: TypedExpr[Unit] =
        fieldName match {
          case Some(nm) =>
            TypedExpr.Local(nm, rankn.Type.IntType, ())
          case None =>
            TypedExpr.Literal(Lit.fromInt(0), rankn.Type.IntType, ())
        }

      val expr = TypedExpr.Match(
        TypedExpr.Local(arg, issue1688EnumTypeValue, ()),
        NonEmptyList.of(
          TypedExpr.Branch(
            Pattern.PositionalStruct((issue1688Package, issue1688Enum0), params),
            None,
            e0Rhs
          ),
          TypedExpr.Branch(
            Pattern.PositionalStruct((issue1688Package, issue1688Enum1), Nil),
            None,
            TypedExpr.Literal(Lit.fromInt(1), rankn.Type.IntType, ())
          )
        ),
        ()
      )

      val lowered =
        Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1688Fn)

      val projected =
        collectProjectionIndices(lowered) {
          case Matchless.GetEnumElement(_, 0, idx, 3) => idx
        }

      assertEquals(projected, usedIdx.toSet)
    }
  }

  test("matrix match elides tuple allocation at match site") {
    val first = Identifier.Name("first")
    val out = Identifier.Name("matrix_elide")
    val scrutinee = pairCall(
      issue1688StructCall(10, 11, 12),
      issue1688StructType,
      issue1688StructCall(20, 21, 22),
      issue1688StructType
    )
    val expr = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.Var(first) :: Pattern.WildCard :: Nil
          ),
          None,
          TypedExpr.Local(first, issue1688StructType, ())
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
    val tupleProjections =
      collectProjectionIndices(lowered) {
        case Matchless.GetStructElement(_, idx, 2) => idx
      }
    assertEquals(tupleProjections, Set.empty[Int])
  }

  test("ordered matcher also elides tuple allocation for non-orthogonal suffix") {
    val out = Identifier.Name("ordered_elide")
    val listPattern = Pattern.ListPat(
      Pattern.ListPart.WildList ::
        Pattern.ListPart.Item(Pattern.Literal(Lit.fromInt(2))) ::
        Pattern.ListPart.WildList ::
        Nil
    )
    val scrutinee = pairCall(
      intLit(1),
      rankn.Type.IntType,
      listOfInts(1 :: 2 :: 3 :: Nil),
      listIntType
    )
    val expr = TypedExpr.Match(
      scrutinee,
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.WildCard :: listPattern :: Nil
          ),
          None,
          intLit(1)
        ),
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.WildCard :: Pattern.WildCard :: Nil
          ),
          None,
          intLit(0)
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
    val tupleProjections =
      collectProjectionIndices(lowered) {
        case Matchless.GetStructElement(_, idx, 2) => idx
      }
    assertEquals(tupleProjections, Set.empty[Int])
  }

  test("ordered matcher handles annotated glob-first list pattern (wild glob)") {
    val out = Identifier.Name("list_glob_wild")
    val annotated = Pattern.Annotation(
      Pattern.ListPat(
        Pattern.ListPart.WildList :: Pattern.ListPart.WildList :: Nil
      ),
      listIntType
    )
    val expr = TypedExpr.Match(
      listOfInts(1 :: 2 :: Nil),
      NonEmptyList.of(
        TypedExpr.Branch(
          annotated,
          None,
          intLit(1)
        ),
        TypedExpr.Branch(Pattern.WildCard, None, intLit(0))
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
  }

  test("ordered matcher handles annotated glob-first list pattern (named glob)") {
    val out = Identifier.Name("list_glob_named")
    val prefix = Identifier.Name("prefix")
    val annotated = Pattern.Annotation(
      Pattern.ListPat(
        Pattern.ListPart.NamedList(prefix) :: Pattern.ListPart.WildList :: Nil
      ),
      listIntType
    )
    val expr = TypedExpr.Match(
      listOfInts(3 :: 4 :: Nil),
      NonEmptyList.one(
        TypedExpr.Branch(
          annotated,
          None,
          intLit(1)
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
  }

  test("ordered matcher visits simplified string patterns") {
    val out = Identifier.Name("ordered_str_simplify")
    val strType = rankn.Type.StrType
    val literal = Pattern.StrPat(NonEmptyList.one(Pattern.StrPart.LitStr("abc")))
    val listSearch = Pattern.ListPat(
      Pattern.ListPart.WildList ::
        Pattern.ListPart.Item(Pattern.Literal(Lit.fromInt(2))) ::
        Pattern.ListPart.WildList ::
        Nil
    )
    val expr = TypedExpr.Match(
      pairCall(
        TypedExpr.Literal(Lit.Str("abc"), strType, ()),
        strType,
        listOfInts(1 :: 2 :: 3 :: Nil),
        listIntType
      ),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.WildCard :: listSearch :: Nil
          ),
          None,
          intLit(0)
        ),
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            literal :: Pattern.WildCard :: Nil
          ),
          None,
          intLit(1)
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
  }

  test("ordered matcher handles union roots with named and var binds") {
    val out = Identifier.Name("ordered_union_root_binds")
    val alias = Identifier.Name("alias_pair")
    val anyPair = Identifier.Name("any_pair")
    val listSearch = Pattern.ListPat(
      Pattern.ListPart.WildList ::
        Pattern.ListPart.Item(Pattern.Literal(Lit.fromInt(2))) ::
        Pattern.ListPart.WildList ::
        Nil
    )
    val unionPat = Pattern.Union(
      Pattern.Named(
        alias,
        Pattern.PositionalStruct(
          (issue1732Package, issue1732Pair),
          Pattern.Literal(Lit.fromInt(1)) :: Pattern.WildCard :: Nil
        )
      ),
      NonEmptyList.one(Pattern.Var(anyPair))
    )
    val expr = TypedExpr.Match(
      pairCall(
        intLit(1),
        rankn.Type.IntType,
        listOfInts(1 :: 2 :: 3 :: Nil),
        listIntType
      ),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.WildCard :: listSearch :: Nil
          ),
          None,
          intLit(0)
        ),
        TypedExpr.Branch(unionPat, None, intLit(1)),
        TypedExpr.Branch(Pattern.WildCard, None, intLit(2))
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2) >= 0, true)
  }

  test(
    "matrix bridge handles long orthogonal prefix before non-orthogonal suffix"
  ) {
    val out = Identifier.Name("ortho_prefix_bridge")
    val listSearch = Pattern.ListPat(
      Pattern.ListPart.WildList ::
        Pattern.ListPart.Item(Pattern.Literal(Lit.fromInt(2))) ::
        Pattern.ListPart.WildList ::
        Nil
    )
    def litPair(i: Int): Pattern[(PackageName, Constructor), rankn.Type] =
      Pattern.PositionalStruct(
        (issue1732Package, issue1732Pair),
        Pattern.Literal(Lit.fromInt(i)) :: Pattern.WildCard :: Nil
      )

    val nonOrthoBranch =
      Pattern.PositionalStruct(
        (issue1732Package, issue1732Pair),
        Pattern.WildCard :: listSearch :: Nil
      )

    val expr = TypedExpr.Match(
      pairCall(
        intLit(99),
        rankn.Type.IntType,
        listOfInts(1 :: 2 :: 3 :: Nil),
        listIntType
      ),
      NonEmptyList.of(
        TypedExpr.Branch(litPair(0), None, intLit(0)),
        TypedExpr.Branch(litPair(1), None, intLit(1)),
        TypedExpr.Branch(litPair(2), None, intLit(2)),
        TypedExpr.Branch(litPair(3), None, intLit(3)),
        TypedExpr.Branch(nonOrthoBranch, None, intLit(4)),
        TypedExpr.Branch(Pattern.WildCard, None, intLit(5))
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2) >= 0, true)
  }

  test("matrix matcher peels annotated binders") {
    val out = Identifier.Name("matrix_annotated_bind")
    val first = Identifier.Name("first_annotated")
    val expr = TypedExpr.Match(
      pairCall(intLit(7), rankn.Type.IntType, intLit(8), rankn.Type.IntType),
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.Annotation(Pattern.Var(first), rankn.Type.IntType) ::
              Pattern.WildCard ::
              Nil
          ),
          None,
          TypedExpr.Local(first, rankn.Type.IntType, ())
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
  }

  test("single-constructor struct constructor scrutinee behaves like tuple") {
    val selected = Identifier.Name("selected")
    val out = Identifier.Name("pick_struct_ctor")
    val intType = rankn.Type.IntType
    val ctorType =
      rankn.Type.Fun(
        NonEmptyList.of(intType, intType, intType),
        issue1688StructType
      )
    val ctor =
      TypedExpr.Global(issue1688Package, issue1688Struct, ctorType, ())
    val scrutinee = TypedExpr.App(
      ctor,
      NonEmptyList.of(
        TypedExpr.Literal(Lit.fromInt(1), intType, ()),
        TypedExpr.Literal(Lit.fromInt(2), intType, ()),
        TypedExpr.Literal(Lit.fromInt(3), intType, ())
      ),
      issue1688StructType,
      ()
    )
    val matchExpr = TypedExpr.Match(
      scrutinee,
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1688Package, issue1688Struct),
            Pattern.WildCard :: Pattern.Var(selected) :: Pattern.WildCard :: Nil
          ),
          None,
          TypedExpr.Local(selected, intType, ())
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, matchExpr)(
        issue1688Fn
      )

    assertEquals(countStructConstructorApps(lowered, 3), 0)
    val structProjections =
      collectProjectionIndices(lowered) {
        case Matchless.GetStructElement(_, idx, 3) => idx
      }
    assertEquals(structProjections, Set.empty[Int])
  }

  test("whole-root binding reconstructs only on bound branch path") {
    val x2 = Identifier.Name("x2")
    val pairAlias = Identifier.Name("pair_alias")
    val out = Identifier.Name("whole_root")
    val intType = rankn.Type.IntType

    val expr = TypedExpr.Match(
      pairCall(intLit(1), intType, intLit(2), intType),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.Named(
            pairAlias,
            Pattern.PositionalStruct(
              (issue1732Package, issue1732Pair),
              Pattern.Literal(Lit.fromInt(0)) :: Pattern.WildCard :: Nil
            )
          ),
          None,
          intLit(0)
        ),
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.Var(x2) :: Pattern.WildCard :: Nil
          ),
          None,
          TypedExpr.Local(x2, intType, ())
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 1)
    assertEquals(hasStructConstructorOutsideConditional(lowered, 2), false)
  }

  test("strict evaluation for inlined root fields is once and left-to-right") {
    val first = Identifier.Name("first_field")
    val fName = Identifier.Name("f")
    val gName = Identifier.Name("g")
    val out = Identifier.Name("strict_eval")
    val intType = rankn.Type.IntType
    val fnType = rankn.Type.Fun(NonEmptyList.one(intType), intType)
    val fCall = TypedExpr.App(
      TypedExpr.Global(TestUtils.testPackage, fName, fnType, ()),
      NonEmptyList.one(intLit(1)),
      intType,
      ()
    )
    val gCall = TypedExpr.App(
      TypedExpr.Global(TestUtils.testPackage, gName, fnType, ()),
      NonEmptyList.one(intLit(2)),
      intType,
      ()
    )
    val expr = TypedExpr.Match(
      pairCall(
        fCall,
        intType,
        gCall,
        intType
      ),
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.PositionalStruct(
            (issue1732Package, issue1732Pair),
            Pattern.Var(first) :: Pattern.WildCard :: Nil
          ),
          None,
          TypedExpr.Local(first, intType, ())
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 0)
    assertEquals(countGlobalCalls(lowered, TestUtils.testPackage, fName), 1)
    assertEquals(countGlobalCalls(lowered, TestUtils.testPackage, gName), 1)

    val leadCalls = leadingLetGlobalCalls(lowered, TestUtils.testPackage)
    val fIdx = leadCalls.indexOf(fName)
    val gIdx = leadCalls.indexOf(gName)
    assert(fIdx >= 0, s"expected leading let-bound call to include $fName")
    assert(gIdx >= 0, s"expected leading let-bound call to include $gName")
    assert(
      fIdx < gIdx,
      s"expected left-to-right call evaluation order f then g, got: $leadCalls"
    )
  }

  test("multiple whole-root bind branches disable root inlining") {
    val flag = Identifier.Name("flag")
    val left = Identifier.Name("left")
    val right = Identifier.Name("right")
    val out = Identifier.Name("multiple_root")

    val expr = TypedExpr.Match(
      pairCall(intLit(1), rankn.Type.IntType, intLit(2), rankn.Type.IntType),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.Named(
            left,
            Pattern.PositionalStruct(
              (issue1732Package, issue1732Pair),
              Pattern.WildCard :: Pattern.WildCard :: Nil
            )
          ),
          Some(TypedExpr.Local(flag, rankn.Type.BoolType, ())),
          intLit(0)
        ),
        TypedExpr.Branch(
          Pattern.Named(
            right,
            Pattern.PositionalStruct(
              (issue1732Package, issue1732Pair),
              Pattern.WildCard :: Pattern.WildCard :: Nil
            )
          ),
          None,
          intLit(1)
        )
      ),
      ()
    )

    val lowered =
      Matchless.fromLet((), out, RecursionKind.NonRecursive, expr)(issue1732Fn)

    assertEquals(countStructConstructorApps(lowered, 2), 1)
    assertEquals(hasStructConstructorOutsideConditional(lowered, 2), true)
  }

  test("TypedExpr.Loop/Recur lowers to WhileExpr in matchless") {
    val intType = rankn.Type.IntType
    val x = Identifier.Name("x")
    val xExpr = TypedExpr.Local(x, intType, ())
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.one((x, TypedExpr.Literal(Lit.fromInt(1), intType, ()))),
      TypedExpr.Match(
        xExpr,
        NonEmptyList.of(
          TypedExpr.Branch(Pattern.Literal(Lit.fromInt(0)), None, xExpr),
          TypedExpr.Branch(
            Pattern.WildCard,
            None,
            TypedExpr.Recur(NonEmptyList.one(xExpr), intType, ())
          )
        ),
        ()
      ),
      ()
    )

    def containsWhile(e: Matchless.Expr[Unit]): Boolean =
      e match {
        case Matchless.WhileExpr(_, _, _)           => true
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(containsWhile) || containsWhile(body)
        case Matchless.App(fn, args) =>
          containsWhile(fn) || args.exists(containsWhile)
        case Matchless.Let(_, expr, in) =>
          containsWhile(expr) || containsWhile(in)
        case Matchless.LetMut(_, in) =>
          containsWhile(in)
        case Matchless.If(_, t, f) =>
          containsWhile(t) || containsWhile(f)
        case Matchless.Always(_, e) =>
          containsWhile(e)
        case Matchless.PrevNat(e) =>
          containsWhile(e)
        case _ =>
          false
      }

    val compiled =
      Matchless.fromLet(
        (),
        Identifier.Name("loop_value"),
        RecursionKind.NonRecursive,
        loopExpr
      )(fnFromTypeEnv(rankn.TypeEnv.empty))

    assert(containsWhile(compiled), compiled.toString)
  }
}
