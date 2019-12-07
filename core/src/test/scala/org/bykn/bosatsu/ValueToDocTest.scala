package org.bykn.bosatsu

import org.scalatest.prop.PropertyChecks.{forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

import rankn.{NTypeGen, Type}
import TestUtils.typeEnvOf

class ValueToDocTest extends FunSuite {

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000)

  test("never throw when converting to doc") {
    val vd = ValueToDoc({ _ => None})

    forAll(NTypeGen.genPredefType, GenValue.genValue) { (t, v) =>
      vd.toDoc(t)(v)
      succeed
    }
  }

  test("some hand written cases round trip") {
    val te = typeEnvOf(PackageName.parts("Test"), """

struct MyUnit
# wrappers are removed
struct MyWrapper(item)
struct MyPair(fst, snd)

enum MyEither: L(left), R(right)

enum MyNat: Z, S(prev: MyNat)
""")
    val conv = ValueToDoc(te.toDefinedType(_))

    def stringToType(t: String): Type = {
      val tr = TypeRef.parser.parse(t) match {
        case fastparse.all.Parsed.Success(tr, l) if l == t.length => tr
        case other => sys.error(s"could not parse: $t, $other")
      }

      TypeRefConverter[cats.Id](tr) { cons =>
        te.referencedPackages.toList.flatMap { pack =>
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
        case Left(err) => fail(s"could not handle to Value: $tpe, $v, $err")
      }
    }

    law("Int", Value.VInt(42), "42")
    law("String", Value.Str("hello world"), "'hello world'")
    law("MyUnit", Value.UnitValue, "MyUnit")
    law("MyWrapper[MyUnit]", Value.UnitValue, "MyWrapper { item: MyUnit }")
    law("MyWrapper[MyWrapper[MyUnit]]", Value.UnitValue, "MyWrapper { item: MyWrapper { item: MyUnit } }")
    law("MyPair[MyUnit, MyUnit]", Value.ProductValue.fromList(List(Value.UnitValue, Value.UnitValue)),
        "MyPair { fst: MyUnit, snd: MyUnit }")
    law("MyEither[MyUnit, MyUnit]", Value.SumValue(0, Value.ProductValue.fromList(List(Value.UnitValue))), "L { left: MyUnit }")
    law("MyEither[MyUnit, MyUnit]", Value.SumValue(1, Value.ProductValue.fromList(List(Value.UnitValue))), "R { right: MyUnit }")
  }
}
