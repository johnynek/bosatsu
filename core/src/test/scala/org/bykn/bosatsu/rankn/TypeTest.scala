package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.funsuite.AnyFunSuite

class TypeTest extends AnyFunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def parse(s: String): Type =
    Type.fullyResolvedParser.parseAll(s) match {
      case Right(t) => t
      case Left(err) => sys.error(err.toString)
    }

  test("free vars are not duplicated") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      val frees = Type.freeTyVars(ts)
      assert(frees.distinct == frees)
    }
  }

  test("types are well ordered") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03, NTypeGen.genDepth03) {
      org.bykn.bosatsu.OrderingLaws.law(_, _, _)
    }
  }

  test("test all binders") {
    assert(Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name) ==
      ("a" #:: Stream.iterate(0)(_ + 1).map { i => s"a$i" }).take(100))
  }

  test("tyVarBinders is identity for Bound") {
    forAll(Gen.listOf(NTypeGen.lowerIdent), NTypeGen.genDepth03) { (vs, t) =>
      val vsD = vs.distinct
      val bs = vsD.map(Type.Var.Bound(_))
      val fa = Type.forAll(bs, t)
      val binders = Type.tyVarBinders(List(fa))
      assert(bs.toSet.subsetOf(binders))
    }
  }

  test("if Type.hasNoVars then freeVars is empty") {
    forAll(NTypeGen.genDepth03) { t =>
      assert(Type.hasNoVars(t) == Type.freeTyVars(t :: Nil).isEmpty)
    }
  }

  test("hasNoVars fully recurses") {
    def allTypesIn(t: Type): List[Type] =
      t match {
        case f@Type.ForAll(bounds, in) =>
          // filter bounds out, since they are shadowed
          val boundSet = bounds.toList.toSet[Type.Var]
          f :: (allTypesIn(in).filterNot { it =>
            val frees = Type.freeTyVars(it :: Nil).toSet
            // if we intersect, this is not a legit type to consider
            (boundSet & frees).nonEmpty
          })
        case t@Type.TyApply(a, b) => t :: allTypesIn(a) ::: allTypesIn(b)
        case other => other :: Nil
      }

    def law(t: Type) = {
      val allT = allTypesIn(t)
      val hnv = Type.hasNoVars(t)

      if (hnv) assert(allT.forall(Type.hasNoVars), "hasNoVars == true")
      else assert(allT.exists { t => !Type.hasNoVars(t) }, "hasNoVars == false")
    }

    forAll(NTypeGen.genDepth03)(law _)

    val pastFails =
      List(
        Type.ForAll(NonEmptyList.of(Type.Var.Bound("x"), Type.Var.Bound("ogtumm"), Type.Var.Bound("t")),
          Type.TyVar(Type.Var.Bound("x"))),
        Type.ForAll(NonEmptyList.of(Type.Var.Bound("a")),Type.TyVar(Type.Var.Bound("a")))
        )

    pastFails.foreach(law)
  }

  test("Type.freeTyVars is empty for ForAll fully bound") {
    val fa = parse("forall a. a")
    assert(Type.freeTyVars(fa :: Nil).isEmpty)

    val fb = parse("forall a. a -> Bosatsu/Predef::Int")
    assert(Type.freeTyVars(fb :: Nil).isEmpty)
  }

  test("Type.freeTyVars is left to right for functions") {
    val ba = Type.Var.Bound("a")
    val bb = Type.Var.Bound("b")

    val ta = Type.TyVar(ba)
    val tb = Type.TyVar(bb)
    val fb = Type.Fun(ta, tb)
    assert(Type.freeTyVars(fb :: ta :: Nil) == List(ba, bb))
    assert(Type.freeTyVars(fb :: tb :: Nil) == List(ba, bb))
  }

  test("types are in order in freeTyVars") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03) { (t1, t2) =>
      val left = Type.freeTyVars(t1 :: Nil)
      val right = Type.freeTyVars(t2 :: Nil)
      val both = Type.freeTyVars(t1 :: t2 :: Nil)

      assert((left.toSet | right.toSet) == both.toSet)
      assert(left ::: (right.filterNot(left.toSet)) == both)
    }
  }

  def genSubs(depth: Int): Gen[Map[Type.Var, Type]] = {
    val pair = Gen.zip(
      NTypeGen.genBound,
      NTypeGen.genDepth(depth, Some(NTypeGen.genConst)))
    Gen.mapOf(pair)
  }

  test("substitute is a no-op if none of the substitutions are free") {
    forAll(NTypeGen.genDepth03, genSubs(3)) { (t, subs) =>
      val subs1 = subs -- Type.freeBoundTyVars(t :: Nil)
      assert(Type.substituteVar(t, subs1) == t)
    }
  }

  test("substitute is idempotent") {
    def law(t: Type, subs0: Map[Type.Var, Type]) = {
      /*
       * subs is only idempotent if none of
       * the free variables are also keys to the substitution
       */
      val allFrees = Type.freeTyVars(subs0.values.toList).toSet
      val subs = subs0.filterNot { case (k, _) => allFrees(k) }
      val t1 = Type.substituteVar(t, subs)
      val t2 = Type.substituteVar(t1, subs)
      assert(t2 == t1)
    }

    forAll(NTypeGen.genDepth03, genSubs(3))(law _)

    val ba: Type.Var = Type.Var.Bound("a")

    val pastFails = List(
      // this is an illegitmate substitution, but the law needs to be robust to it
      (Type.TyVar(ba), Map(ba -> Type.TyApply(Type.TyVar(ba), Type.TyVar(ba))))
    )

    pastFails.foreach { case (t, s) => law(t, s) }
  }

  test("after substitution, none of the keys are free") {
    def law(t: Type, subs: Map[Type.Var, Type]) = {
      // don't substitute back onto the keys
      val subs1 = subs.filter { case (_, v) =>
        (Type.freeBoundTyVars(v :: Nil).toSet & subs.keySet).isEmpty
      }
      // now subs1 has keys that can be completely removed, so
      // after substitution, those keys should be gone
      val t1 = Type.substituteVar(t, subs1)
      assert((Type.freeBoundTyVars(t1 :: Nil).toSet & subs1.keySet) == Set.empty)
    }

    forAll(NTypeGen.genDepth03, genSubs(3))(law _)
  }

  test("test Fun.uncurry") {
    def b(s: String) = Type.TyVar(Type.Var.Bound(s))

    import Type.Fun.{uncurry, curry}
    assert(uncurry(b("a")) == None)

    assert(uncurry(Type.Fun(b("a"), b("b"))) == Some((NonEmptyList.of(b("a")), b("b"))))
    assert(curry(NonEmptyList.of(b("a")), b("b")) == Type.Fun(b("a"), b("b")))

    assert(uncurry(Type.Fun(b("a"), Type.Fun(b("b"), b("c")))) == Some((NonEmptyList.of(b("a"), b("b")), b("c"))))
    assert(curry(NonEmptyList.of(b("a"), b("b")), b("c")) == Type.Fun(b("a"), Type.Fun(b("b"), b("c"))))

    forAll(NTypeGen.genDepth03) { t =>
      assert(Type.Fun.arity(t) >= 0)

      uncurry(t) match {
        case Some((a, r)) =>
          assert(Type.Fun.arity(t) == a.length)
          assert(curry(a, r) == t)
        case None =>
          ()
      }
    }
  }
}
