package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

class KindParseTest extends ParserTestBase {
  override def config: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 5000 else 100
    )

  val genVariance =
    Gen.oneOf(Variance.co, Variance.in, Variance.contra, Variance.phantom)

  val genKind: Gen[Kind] = {
    val recurse = Gen.lzy(genKind)
    Gen.frequency(
      (
        1,
        Gen.zip(genVariance, recurse, recurse).map { case (v, a, b) =>
          Kind.Cons(a.withVar(v), b)
        }
      ),
      (10, Gen.oneOf(Kind.allKinds.take(1000)))
    )
  }

  val genArg: Gen[Kind.Arg] =
    Gen.zip(genVariance, genKind).map { case (v, k) => Kind.Arg(v, k) }

  test("we can parse everything we generate") {
    forAll(genKind) { law(Kind.parser) }
  }

  test("test some examples") {
    import Kind.Type

    parseTestAll(Kind.parser, "*", Type)

    val kind1 = Kind(Type.in)
    parseTestAll(Kind.parser, "(*) -> *", kind1)
    parseTestAll(Kind.parser, "(* -> *)", kind1)
    parseTestAll(Kind.parser, "* -> (*)", kind1)

    parseTestAll(Kind.parser, "* -> *", kind1)
    parseTestAll(Kind.parser, "*->*", kind1)
    parseTestAll(Kind.parser, "+*->*", Kind(Type.co))
    parseTestAll(Kind.parser, "-* -> *", Kind(Type.contra))
    parseTestAll(Kind.parser, "👻* -> *", Kind(Type.phantom))

    parseTestAll(Kind.parser, "(* -> *) -> *", Kind(Kind(Type.in).in))
    parseTestAll(Kind.parser, "+(* -> *) -> *", Kind(Kind(Type.in).co))
    parseTestAll(Kind.parser, "* -> * -> *", Kind(Type.in, Type.in))
    parseTestAll(Kind.parser, "-* -> +* -> *", Kind(Type.contra, Type.co))
    parseTestAll(Kind.parser, "* -> (* -> *)", Kind(Type.in, Type.in))
  }

  test("order is consistent with isType") {
    forAll(genKind) { k =>
      assert((k.order == 0) == k.isType)
    }
  }

  test("order is consistent with isOrder1") {
    forAll(genKind) { k =>
      assert((k.order == 1) == k.isOrder1)
    }
  }

  test("order is consistent with isHigherKind") {
    forAll(genKind) { k =>
      assert((k.order > 1) == k.isHigherOrder)
    }
  }

  test("order is non-negative") {
    forAll(genKind) { k =>
      assert(k.order >= 0)
      assert(k.isType || k.isOrder1 || k.isHigherOrder)
    }
  }

  test("order composes correctly") {
    forAll(genKind, genKind) { (k0, k1) =>
      val ord0 = k0.order
      val ord1 = k1.order
      val ord2 = Kind.Cons(k0.in, k1).order
      assert(ord2 == scala.math.max(ord0 + 1, ord1))
    }
  }

  test("toArgs and Kind.apply are inverses") {
    forAll(genKind) { k =>
      assert(Kind(k.toArgs: _*) == k)
    }

    forAll(Gen.listOf(genArg)) { args =>
      assert(Kind(args: _*).toArgs == args)
    }
  }

  test("example orders") {
    def check(str: String, ord: Int) =
      Kind.parser.parseAll(str) match {
        case Right(k)  => assert(k.order == ord)
        case Left(err) => fail(err.toString)
      }

    check("*", 0)

    check("* -> *", 1)
    check("* -> * -> *", 1)
    check("* -> * -> * -> *", 1)

    check("(* -> *) -> *", 2)
    check("(* -> *) -> (* -> *) -> *", 2)
  }

  test("Kind.shapeMatch(a, b) == Kind.shapeMatch(b, a)") {
    forAll(genKind, genKind) { (a, b) =>
      val m = Kind.shapeMatch(a, b)
      assert(m === Kind.shapeMatch(b, a))
      if (m) {
        assert(a.order === b.order)
      }
      assert(Kind.shapeMatch(a, a))
    }
  }
  test("if Kind.leftSubsumesRight(a, b) then shapeMatch(a, b)") {
    forAll(genKind, genKind) { (a, b) =>
      if (Kind.leftSubsumesRight(a, b)) {
        assert(Kind.shapeMatch(a, b))
      }

      assert(Kind.leftSubsumesRight(a, a))
    }
  }

  test("Kind.allSubkinds(k).forall(Kind.leftSubsumesRight(k, _))") {
    forAll(genKind, genKind) { (k1, k2) =>
      val subs = Kind.allSubKinds(k1)
      if (Kind.leftSubsumesRight(k1, k2)) {
        assert(subs.indexOf(k2) >= 0)
      }
      assert(subs.indexOf(k1) >= 0)
      subs.foreach { k =>
        assert(Kind.leftSubsumesRight(k1, k))
        assert(Kind.shapeMatch(k, k1))
        assert(Kind.shapeMatch(k1, k))
        assert(k.order === k1.order)
      }
    }
  }

  test("Kind.allSuperkinds(k).forall(Kind.leftSubsumesRight(_, k))") {
    forAll(genKind, genKind) { (k1, k2) =>
      val sups = Kind.allSuperKinds(k1)
      if (Kind.leftSubsumesRight(k2, k1)) {
        assert(sups.indexOf(k2) >= 0)
      }
      assert(sups.indexOf(k1) >= 0)
      sups.foreach { k =>
        assert(Kind.leftSubsumesRight(k, k1))
        assert(Kind.shapeMatch(k, k1))
        assert(Kind.shapeMatch(k1, k))
        assert(k.order === k1.order)
      }
    }
  }

  test("some subsume examples") {
    def check(str1: String, str2: String, matches: Boolean = true) = {
      (Kind.parser.parseAll(str1), Kind.parser.parseAll(str2)) match {
        case (Right(k1), Right(k2)) =>
          assert(Kind.leftSubsumesRight(k1, k2) === matches)
        case err => fail(err.toString)
      }
    }

    check("* -> *", "+* -> *")
    check("* -> *", "-* -> *")
    check("* -> *", Kind(Kind.Type.phantom).toDoc.render(80))

    check("+* -> *", Kind(Kind.Type.phantom).toDoc.render(80))
    check("-* -> *", Kind(Kind.Type.phantom).toDoc.render(80))
    // this is tricky:
    // if left is like trait Foo[G[+_]]
    // and right is like trait Bar[G[_]]
    // can we use the kind of Bar where we require kind of Foo?
    // yes, because we will only pass covariant G's to Bar, but Bar
    // doesn't care if they are covariant.
    check("(+* -> *) -> *", "(* -> *) -> *")

    check("(* -> *) -> *", "(+* -> *) -> *", false)
    check("(* -> *) -> *", "(-* -> *) -> *", false)

    check("+* -> *", "-* -> *", false)
    check("-* -> *", "+* -> *", false)
    // Some scala agreement
    // if we try to pass a kind, it has to be a subkind to accept
    // (+* -> *) -> *
    trait CoMonad[F[+_]]
    // (-* -> *) -> *
    trait ContraMonad[F[-_]]
    // (* -> *) -> *
    trait Monad[F[_]]
    // ((+* -> *) -> *) -> *
    trait CoMonadTrans[F[_[+_]]]
    trait ContraMonadTrans[F[_[-_]]]
    // ((* -> *) -> *) -> *
    trait MonadTrans[F[_[_]]]

    new MonadTrans[Monad] {}

    // this does not compile
    // new MonadTrans[CoMonad] {}
    check("(* -> *) -> *", "(+* -> *) -> *", false)
    // this does not compile
    // new MonadTrans[ContraMonad] {}
    check("(* -> *) -> *", "(-* -> *) -> *", false)

    new CoMonadTrans[CoMonad] {}
    // CoMonadTrans accepts Monad
    new CoMonadTrans[Monad] {}
    check("(+* -> *) -> *", "(* -> *) -> *")
    new ContraMonadTrans[Monad] {}
    check("(-* -> *) -> *", "(* -> *) -> *")
    new ContraMonadTrans[ContraMonad] {}
    check("(-* -> *) -> *", "(-* -> *) -> *")
    // this does not compile
    // new CoMonadTrans[ContraMonad] {}
    check("(+* -> *) -> *", "(-* -> *) -> *", false)
  }

  test("we can find all kinds in the allKinds list") {
    // cache this
    val all = Kind.allKinds

    // if order gets too crazy we will blow up the memory
    assert(all.take(10000).toSet.size == 10000)

    def check(k: Kind) = assert(all.indexOf(k) >= 0)

    import Kind.Type
    check(Kind())
    check(Kind(Type.withVar(Variance.phantom)))
    check(Kind(Type.withVar(Variance.co)))
    check(Kind(Type.withVar(Variance.contra)))
    check(Kind(Type.withVar(Variance.in)))
    check(Kind(Type.withVar(Variance.co), Type.withVar(Variance.co)))
  }
}
