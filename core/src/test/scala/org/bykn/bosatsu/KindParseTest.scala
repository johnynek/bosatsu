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
      (2, Gen.const(Kind.Type)),
      (
        1,
        Gen.zip(genVariance, recurse, recurse).map { case (v, a, b) =>
          Kind.Cons(a.withVar(v), b)
        }
      )
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
}
