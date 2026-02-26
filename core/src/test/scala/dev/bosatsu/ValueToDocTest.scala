package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import rankn.{NTypeGen, Type, TypeEnv}
import TestUtils.typeEnvOf

class ValueToDocTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
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
      vd.toDoc(t)(v): Unit
      ()
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
        case Right(doc) => assertEquals(doc.render(80), str)
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

  test("render Char external values") {
    val conv = ValueToDoc(_ => None)
    val toDoc = conv.toDoc(Type.CharType)

    toDoc(Value.Str("a")) match {
      case Right(doc) => assertEquals(doc.render(80), ".'a'")
      case Left(err)  => fail(s"failed to render char: $err")
    }
  }

  test("char rendering reports ill-typed runtime values") {
    val conv = ValueToDoc(_ => None)
    val toDoc = conv.toDoc(Type.CharType)

    toDoc(Value.Str("ab")) match {
      case Left(_)    => ()
      case Right(doc) =>
        fail(s"expected ill-typed char rendering failure, got ${doc.render(80)}")
    }
  }

  test("render Array external values") {
    val conv = ValueToDoc(_ => None)
    val arrayType = Type.TyApply(
      Type.const(
        PackageName.parts("Bosatsu", "Collection", "Array"),
        TypeName("Array")
      ),
      Type.IntType
    )
    val arr = Value.ExternalValue(
      PredefImpl.ArrayValue(Array(Value.VInt(1), Value.VInt(2)), 0, 2)
    )

    conv.toDoc(arrayType)(arr) match {
      case Right(doc) => assertEquals(doc.render(80), "[1, 2]")
      case Left(err)  => fail(s"failed to render array: $err")
    }
  }

  test("array rendering reports ill-typed runtime values") {
    val conv = ValueToDoc(_ => None)
    val arrayType = Type.TyApply(
      Type.const(
        PackageName.parts("Bosatsu", "Collection", "Array"),
        TypeName("Array")
      ),
      Type.IntType
    )

    conv.toDoc(arrayType)(Value.UnitValue) match {
      case Left(_)    => ()
      case Right(doc) =>
        fail(
          s"expected ill-typed array rendering failure, got ${doc.render(80)}"
        )
    }
  }

  test("render Bytes external values") {
    val conv = ValueToDoc(_ => None)
    val bytesType =
      Type.const(PackageName.parts("Bosatsu", "IO", "Bytes"), TypeName("Bytes"))
    val bytes = Value.ExternalValue(
      PredefImpl.BytesValue(Array[Byte](0.toByte, 10.toByte, 255.toByte), 0, 3)
    )

    conv.toDoc(bytesType)(bytes) match {
      case Right(doc) => assertEquals(doc.render(80), "[0, 10, 255]")
      case Left(err)  => fail(s"failed to render bytes: $err")
    }
  }

  test("bytes rendering reports ill-typed runtime values") {
    val conv = ValueToDoc(_ => None)
    val bytesType =
      Type.const(PackageName.parts("Bosatsu", "IO", "Bytes"), TypeName("Bytes"))

    conv.toDoc(bytesType)(Value.UnitValue) match {
      case Left(_)    => ()
      case Right(doc) =>
        fail(
          s"expected ill-typed bytes rendering failure, got ${doc.render(80)}"
        )
    }
  }

  test("render Prog as opaque value") {
    val conv = ValueToDoc(_ => None)
    val progType = Type.TyApply(
      Type.TyApply(
        Type.TyApply(
          Type.const(PackageName.parts("Bosatsu", "Prog"), TypeName("Prog")),
          Type.IntType
        ),
        Type.IntType
      ),
      Type.UnitType
    )
    val progValue = Value.SumValue(0, Value.ProductValue.single(Value.UnitValue))

    conv.toDoc(progType)(progValue) match {
      case Right(doc) => assertEquals(doc.render(80), "Prog(...)")
      case Left(err)  => fail(s"failed to render Prog: $err")
    }
  }

  test("Prog rendering reports ill-typed runtime values") {
    val conv = ValueToDoc(_ => None)
    val progType = Type.TyApply(
      Type.TyApply(
        Type.TyApply(
          Type.const(PackageName.parts("Bosatsu", "Prog"), TypeName("Prog")),
          Type.IntType
        ),
        Type.IntType
      ),
      Type.UnitType
    )

    conv.toDoc(progType)(Value.UnitValue) match {
      case Left(_)    => ()
      case Right(doc) =>
        fail(
          s"expected ill-typed Prog rendering failure, got ${doc.render(80)}"
        )
    }
  }
}
