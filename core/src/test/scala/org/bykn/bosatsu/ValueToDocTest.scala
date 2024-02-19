package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}

import rankn.{NTypeGen, Type, TypeEnv}
import TestUtils.typeEnvOf
import org.scalatest.funsuite.AnyFunSuite

class ValueToDocTest extends AnyFunSuite {

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful =
      if (Platform.isScalaJvm) 1000 else 20
    )

  test("never throw when converting to doc") {
    val tegen = Generators.typeEnvGen(PackageName.parts("Foo"), Gen.const(()))

    val withType: Gen[(TypeEnv[Any], Type)] =
      tegen.flatMap { te =>
        val tyconsts =
          te.allDefinedTypes.map(_.toTypeConst)
        val theseTypes = NTypeGen.genDepth(
          4,
          if (tyconsts.isEmpty) None else Some(Gen.oneOf(tyconsts))
        )

        theseTypes.map((te, _))
      }

    forAll(withType, GenValue.genValue) { case ((te, t), v) =>
      val vd = ValueToDoc(te.toDefinedType(_))
      vd.toDoc(t)(v)
      succeed
    }
  }

  test("some hand written cases round trip") {
    val te = typeEnvOf(
      PackageName.parts("Test"),
      """

struct MyUnit
# wrappers are removed
struct MyWrapper(item)
struct MyPair(fst, snd)

enum MyEither: L(left), R(right)

enum MyNat: Z, S(prev: MyNat)
"""
    )
    val conv = ValueToDoc(te.toDefinedType(_))

    def stringToType(t: String): Type = {
      val tr = Parser.unsafeParse(TypeRef.parser, t)

      TypeRefConverter[cats.Id](tr) { cons =>
        te.referencedPackages.toList
          .flatMap { pack =>
            val const = Type.Const.Defined(pack, TypeName(cons))
            te.toDefinedType(const).map(_ => const)
          }
          .headOption
          .getOrElse(Type.Const.predef(cons.asString))
      }
    }

    def law(tpe: String, v: Value, str: String) = {
      val t = stringToType(tpe)
      val toDoc = conv.toDoc(t)

      toDoc(v) match {
        case Right(doc) => assert(doc.render(80) == str)
        case Left(err)  => fail(s"could not handle to Value: $tpe, $v, $err")
      }
    }

    law("Int", Value.VInt(42), "42")
    law("String", Value.Str("hello world"), "'hello world'")
    law("MyUnit", Value.UnitValue, "MyUnit")
    law("MyWrapper[MyUnit]", Value.UnitValue, "MyWrapper { item: MyUnit }")
    law(
      "MyWrapper[MyWrapper[MyUnit]]",
      Value.UnitValue,
      "MyWrapper { item: MyWrapper { item: MyUnit } }"
    )
    law(
      "MyPair[MyUnit, MyUnit]",
      Value.ProductValue.fromList(List(Value.UnitValue, Value.UnitValue)),
      "MyPair { fst: MyUnit, snd: MyUnit }"
    )
    law(
      "MyEither[MyUnit, MyUnit]",
      Value.SumValue(0, Value.ProductValue.fromList(List(Value.UnitValue))),
      "L { left: MyUnit }"
    )
    law(
      "MyEither[MyUnit, MyUnit]",
      Value.SumValue(1, Value.ProductValue.fromList(List(Value.UnitValue))),
      "R { right: MyUnit }"
    )
  }
}
