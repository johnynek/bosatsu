package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import Identifier.{Bindable, Constructor}
import rankn.DataRepr

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Try

class MatchlessTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
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

      assert(run() == run())
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

      assert(p1 == p0)
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
        assert(stopped.init.exists(fn) == false)
      } else {
        // none of them were true
        assert(stopped == nel)
        assert(nel.exists(fn) == false)
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

      assert(matchlessRes == matchRes)
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

        assert(unflatten(chain, rest) == ifexpr)
      case _ => ()
    }
  }

  test("check compilation of some matchless") {
    TestUtils.checkMatchless("""
x = 1    
""") { binds =>
      val map = binds(TestUtils.testPackage).toMap

      assert(map.contains(Identifier.Name("x")))
      assert(map(Identifier.Name("x")) == Matchless.Literal(Lit(1)))
    }
  }
}
