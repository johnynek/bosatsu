package dev.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import Identifier.{Bindable, Constructor}
import rankn.DataRepr

import scala.util.Try

class MatchlessTest extends munit.ScalaCheckSuite {
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

  test("Matchless.applyArgs does not curry App and only pushes through safe Let") {
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
      Matchless.Let(Left(Matchless.LocalAnon(0)), Matchless.Literal(Lit(1)), Matchless.Local(k))

    assertEquals(
      Matchless.applyArgs(letLeftExpr, NonEmptyList.one(y)),
      Matchless.Let(Left(Matchless.LocalAnon(0)), Matchless.Literal(Lit(1)), Matchless.App(Matchless.Local(k), NonEmptyList.one(y)))
    )

    val letRightSafeExpr: Matchless.Expr[Unit] =
      Matchless.Let(z, Matchless.Literal(Lit(1)), Matchless.Local(k))

    assertEquals(
      Matchless.applyArgs(letRightSafeExpr, NonEmptyList.one(y)),
      Matchless.Let(z, Matchless.Literal(Lit(1)), Matchless.App(Matchless.Local(k), NonEmptyList.one(y)))
    )

    val zArg = Matchless.Local(z)
    assertEquals(
      Matchless.applyArgs(letRightSafeExpr, NonEmptyList.one(zArg)),
      Matchless.App(letRightSafeExpr, NonEmptyList.one(zArg))
    )
  }

  test("Matchless.recoverTopLevelLambda pushes apply into branches") {
    val a = Identifier.Name("a")
    val b = Identifier.Name("b")
    val branchFn1: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(a), Matchless.Local(a))
    val branchFn2: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(b), Matchless.Local(b))

    val expr: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, branchFn1, branchFn2)

    Matchless.recoverTopLevelLambda(expr) match {
      case Matchless.Lambda(Nil, None, args, body) =>
        val callArgs = args.map[Matchless.Expr[Unit]](Matchless.Local(_))
        val expectedBody: Matchless.Expr[Unit] =
          Matchless.If(
            Matchless.TrueConst,
            Matchless.App(branchFn1, callArgs),
            Matchless.App(branchFn2, callArgs)
          )
        assertEquals(body, expectedBody)
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

  test("TypedExpr.Loop/Recur lowers to WhileExpr in matchless") {
    val intType = rankn.Type.IntType
    val x = Identifier.Name("x")
    val xExpr = TypedExpr.Local(x, intType, ())
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.one((x, TypedExpr.Literal(Lit.fromInt(1), intType, ()))),
      TypedExpr.Match(
        xExpr,
        NonEmptyList.of(
          (Pattern.Literal(Lit.fromInt(0)), xExpr),
          (Pattern.WildCard, TypedExpr.Recur(NonEmptyList.one(xExpr), intType, ()))
        ),
        ()
      ),
      ()
    )

    def containsWhile(e: Matchless.Expr[Unit]): Boolean =
      e match {
        case Matchless.WhileExpr(_, _, _) => true
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
