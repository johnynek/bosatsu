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
        captures.toList.flatMap(exprBoolSubexpressions) ++ exprBoolSubexpressions(body)
      case Matchless.WhileExpr(cond, effectExpr, _) =>
        boolSubexpressions(cond) ++ exprBoolSubexpressions(effectExpr)
      case Matchless.App(fn, args) =>
        exprBoolSubexpressions(fn) ++ args.toList.flatMap(exprBoolSubexpressions)
      case Matchless.Let(_, value, in) =>
        exprBoolSubexpressions(value) ++ exprBoolSubexpressions(in)
      case Matchless.LetMut(_, in) =>
        exprBoolSubexpressions(in)
      case Matchless.If(cond, thenExpr, elseExpr) =>
        boolSubexpressions(cond) ++ exprBoolSubexpressions(thenExpr) ++ exprBoolSubexpressions(elseExpr)
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
        case Nil           => Gen.const(Matchless.TrueConst)
        case bool :: Nil   => Gen.const(bool)
        case bools         => Gen.oneOf(bools)
      }
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

  test("Matchless.recoverTopLevelLambda pushes apply into branches and beta-reduces") {
    val a = Identifier.Name("a")
    val b = Identifier.Name("b")
    val left = Identifier.Name("left")
    val right = Identifier.Name("right")
    val branchFn1: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(a),
        Matchless.App(Matchless.Local(left), NonEmptyList.one(Matchless.Local(a)))
      )
    val branchFn2: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(b),
        Matchless.App(Matchless.Local(right), NonEmptyList.one(Matchless.Local(b)))
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
                  assertEquals(appArgs, NonEmptyList.one(Matchless.Local(fnArg)))
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
      Matchless.Let(Left(Matchless.LocalAnon(1)), Matchless.Local(y), Matchless.Local(y))
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
              case Right(name) => assert(full(name), s"missing let-bound name $name in $expr")
              case _           => ()
            }
            requireSubset(value)
            checkBool(in)
          case Matchless.LetMutBool(_, in) =>
            checkBool(in)
        }

      expr match {
        case Matchless.Lambda(captures, rec, args, body) =>
          rec.foreach { n => assert(full(n), s"missing recursive name $n in $expr") }
          args.toList.foreach { n => assert(full(n), s"missing lambda arg $n in $expr") }
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
            case Right(name) => assert(full(name), s"missing let-bound name $name in $expr")
            case _           => ()
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

  test("Matchless.reuseConstructors shares repeated constructor apps in a linear scope") {
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

  test("Matchless.reuseConstructors shares constructor apps across if branches") {
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
      case Matchless.Let(Left(tmp), `shared`, Matchless.If(_, thenExpr, elseExpr)) =>
        def checkBranch(e: Matchless.Expr[Unit], lit: Int): Unit =
          e match {
            case Matchless.App(Matchless.MakeStruct(2), args) =>
              assertEquals(args.head, Matchless.Literal(Lit(lit)))
              assertEquals(args.tail.headOption, Some(Matchless.LocalAnon(tmp.ident)))
            case other =>
              fail(s"expected branch struct constructor, found: $other")
          }

        checkBranch(thenExpr, 0)
        checkBranch(elseExpr, 1)
      case other =>
        fail(s"expected constructor reuse across branches, found: $other")
    }
  }

  test("Matchless.reuseConstructors does not share constructor apps with mutable refs") {
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

  test("Matchless.reuseConstructors replaces constructor reuse inside let values") {
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

    Matchless.reuseConstructors(Matchless.If(Matchless.TrueConst, thenExpr, elseExpr)) match {
      case Matchless.Let(Left(tmp), `shared`, Matchless.If(_, then1, else1)) =>
        then1 match {
          case Matchless.Let(
                Right(`z`),
                Matchless.LocalAnon(tmpInValue),
                Matchless.App(Matchless.MakeStruct(2), args)
              ) =>
            assertEquals(tmpInValue, tmp.ident)
            assertEquals(args.head, Matchless.Local(z))
            assertEquals(args.tail.headOption, Some(Matchless.LocalAnon(tmp.ident)))
          case other =>
            fail(s"expected let value to reuse constructor binding, found: $other")
        }

        else1 match {
          case Matchless.App(Matchless.MakeStruct(2), args) =>
            assertEquals(args.head, Matchless.Literal(Lit(1)))
            assertEquals(args.tail.headOption, Some(Matchless.LocalAnon(tmp.ident)))
          case other =>
            fail(s"expected else branch constructor reuse, found: $other")
        }
      case other =>
        fail(s"expected top-level constructor reuse, found: $other")
    }
  }

  test("Matchless.reuseConstructors does not share constructors when mutables appear inside cheap args") {
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

  test("matrix match materializes list projections to avoid nested projection trees") {
    def hasNestedProjectionCheap(e: Matchless.CheapExpr[Unit]): Boolean =
      e match {
        case Matchless.GetEnumElement(arg, _, _, _) =>
          arg match {
            case _: Matchless.GetEnumElement[?] | _: Matchless.GetStructElement[?] =>
              true
            case _ =>
              hasNestedProjectionCheap(arg)
          }
        case Matchless.GetStructElement(arg, _, _) =>
          arg match {
            case _: Matchless.GetEnumElement[?] | _: Matchless.GetStructElement[?] =>
              true
            case _ =>
              hasNestedProjectionCheap(arg)
          }
        case Matchless.Local(_) | Matchless.Global(_, _, _) | Matchless.LocalAnon(_) |
            Matchless.LocalAnonMut(_) | Matchless.ClosureSlot(_) | Matchless.Literal(_) =>
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
          captures.exists(hasNestedProjectionExpr) || hasNestedProjectionExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          hasNestedProjectionBool(cond) || hasNestedProjectionExpr(effectExpr)
        case Matchless.App(fn, args) =>
          hasNestedProjectionExpr(fn) || args.exists(hasNestedProjectionExpr)
        case Matchless.Let(_, value, in) =>
          hasNestedProjectionExpr(value) || hasNestedProjectionExpr(in)
        case Matchless.LetMut(_, in) =>
          hasNestedProjectionExpr(in)
        case Matchless.If(cond, t, f) =>
          hasNestedProjectionBool(cond) || hasNestedProjectionExpr(t) || hasNestedProjectionExpr(f)
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
