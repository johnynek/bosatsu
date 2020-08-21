package org.bykn.bosatsu

import cats.Eq
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks.{forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import TestUtils.typeEnvOf

import rankn.{NTypeGen, Type, TypeEnv}

import GenJson._

class JsonTest extends FunSuite {

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000)

  def assertParser(str: String): Json =
    Json.parser.parse(str) match {
      case fastparse.all.Parsed.Success(j1, len) if len == str.length => j1
      case other =>
        fail(s"failed to parse:\n\n$str\n\nerror: $other")
        Json.JNull
    }

  def law(j: Json) = {
    assert(assertParser(j.render) == j)
  }

  val withType: Gen[(TypeEnv[Unit], Type)] =
    Generators
      .typeEnvGen(PackageName.parts("Foo"), Gen.const(()))
      .flatMap { te =>
        val tyconsts =
          te.allDefinedTypes.map(_.toTypeConst)
        val theseTypes = NTypeGen.genDepth(4, if (tyconsts.isEmpty) None else Some(Gen.oneOf(tyconsts)))

        theseTypes.map((te, _))
      }

  val optTE: Gen[(Option[TypeEnv[Unit]], Type)] =
    for {
      (te, tpe) <- withType
      none <- Gen.oneOf(true, false)
      optTE = if (none) None else Some(te)
    } yield (optTE, tpe)


  test("we can parse all the json we generate") {
    forAll { j: Json => law(j) }
  }

  test("we can parse hard Json numbers") {
    forAll(genJsonNumber)(law(_))

    forAll(genJsonNumber) { num =>
      Parser.JsonNumber.partsParser.parse(num.asString) match {
        case fastparse.all.Parsed.Success(parts, _) =>
          assert(parts.asString == num.asString)
        case other =>
          fail(s"failed to parse:\n\n${num.asString}\n\nerror: $other")
      }
    }

    val regressions = List(
      Json.JNumberStr("2E9"),
      Json.JNumberStr("-9E+19"))

    regressions.foreach { n =>
      law(n)
    }
  }

  test("we can decode and encode json in the same cases") {

    def law(te: Option[TypeEnv[Any]], t: Type, j: Json) = {
      val jsonCodec = te match {
        case None => ValueToJson(_ => None)
        case Some(te) => ValueToJson(te.toDefinedType(_))
      }
      val toJson = jsonCodec.toJson(t)
      val fromJson = jsonCodec.toValue(t)

      assert(toJson.isRight == fromJson.isRight)
      val ej1 = for {
        f12 <- fromJson.product(toJson)
        (fn1, fn2) = f12
        v1 <- fn1(j)
        j <- fn2(v1)
      } yield j

      ej1 match {
        case Right(j1) => assert(Eq[Json].eqv(j1, j), s"$j1 != $j")
        case Left(_) => ()
      }
    }

    forAll(optTE, GenJson.arbJson.arbitrary) { case ((ote, tpe), json) => law(ote, tpe, json) }

    val regressions = List((None, Type.TyApply(Type.OptionType, Type.BoolType), Json.JBool.False))

    regressions.foreach { case (te, t, j) => law(te, t, j) }
  }

  test("if valueToToJson gives 0 arity, it is not a function type") {

    forAll(withType) { case (te, t) =>
      val jsonCodec = ValueToJson(te.toDefinedType(_))
      jsonCodec.valueFnToJsonFn(t) match {
        case Left(_) => ()
        case Right((arity, _)) =>
          val fnUn = Type.Fun.unapply(t)
          assert(arity >= 0)
          if (arity == 0) assert(fnUn.isEmpty)
          else assert(fnUn.isDefined)
      }
    }
  }

  test("we can decode and encode value in the same cases") {

    def law(ote: Option[TypeEnv[Unit]], t: Type, v: Value) = {
      val jsonCodec = ote match {
        case None => ValueToJson(_ => None)
        case Some(te) => ValueToJson(te.toDefinedType(_))
      }
      val toJson = jsonCodec.toJson(t)
      val fromJson = jsonCodec.toValue(t)

      assert(toJson.isRight == fromJson.isRight)
      val ej1 = for {
        f12 <- fromJson.product(toJson)
        (fn1, fn2) = f12
        j <- fn2(v)
        v1 <- fn1(j)
      } yield v1

      ej1 match {
        case Right(v1) => assert(v1 == v, s"$v1 != $v")
        case Left(_) => ()
      }
    }

    forAll(optTE, GenValue.genValue) { case ((ote, t), v) => law(ote, t, v) }

    val regressions: List[(Option[TypeEnv[Unit]], Type, Value)] =
      List()

    regressions.foreach { case (ote, t, v) => law(ote, t, v) }
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
    val jsonConv = ValueToJson(te.toDefinedType(_))

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

    def stringToJson(s: String): Json =
      Json.parser.parse(s) match {
        case fastparse.all.Parsed.Success(j, l) if l == s.length => j
        case other => sys.error(s"could not parse: $s, $other")
      }

    def law(tpe: String, json: String) = {
      val t = stringToType(tpe)
      val toV = jsonConv.toValue(t)
      val toJ = jsonConv.toJson(t)

      toV match {
        case Right(toV) =>
          jsonConv.toJson(t) match {
            case Right(toJ) =>
              val j = stringToJson(json)
              toV(j).flatMap(toJ) match {
                case Right(j1) => assert(Eq[Json].eqv(j1, j), s"$j1 != $j")
                case Left(err) => fail(err.toString)
              }
            case Left(err) => fail(s"could not handle to Json: $tpe, $t, $toV")
          }
        case Left(err) => fail(s"could not handle to Value: $tpe, $t, $toJ")
      }
    }

    def unsupported(tpe: String) = {
      val t = stringToType(tpe)
      jsonConv.supported(t) match {
        case Right(_) => fail(s"expected $tpe to be unsupported")
        case Left(_) => succeed
      }
    }

    def illTypedJson(tpe: String, json: String) = {
      val t = stringToType(tpe)
      val toV = jsonConv.toValue(t)
      val toJ = jsonConv.toJson(t)

      toV match {
        case Right(toV) =>
          val j = stringToJson(json)
          toV(j) match {
            case Left(_) => succeed
            case Right(v) => fail(s"expected $json to be ill-typed: $v")
          }
        case Left(err) => fail(s"could not handle to Value: $tpe, $t, $err")
      }
    }

    law("Int", "42")
    law("String", "\"hello world\"")
    law("Option[Int]", "null")
    law("Option[Int]", "42")
    law("Dict[String, Int]", "{ \"foo\": 42 }")
    law("Dict[String, Int]", "{ \"foo\": 42, \"bar\": 24 }")
    law("Dict[String, Int]", "{ \"bar\": 42, \"foo\": 24 }")
    law("List[Int]", "[1, 2, 3, 4]")
    law("(Int, String)", "[1, \"2\"]")
    law("(Int, String, Bool)", "[1, \"2\", true]")
    law("Option[Bool]", "true")
    law("Option[Bool]", "false")
    law("Option[Bool]", "null")
    law("Option[Option[Bool]]", "[true]")
    law("Option[Option[Bool]]", "[false]")
    law("Option[Option[Bool]]", "[null]")
    law("Option[Option[Bool]]", "[]")
    law("MyUnit", "{}")
    law("MyWrapper[MyUnit]", "{}")
    law("MyWrapper[MyWrapper[MyUnit]]", "{}")
    law("MyPair[MyUnit, MyUnit]", "{\"fst\": {}, \"snd\": {}}")
    law("MyWrapper[MyPair[MyUnit, MyUnit]]", "{\"fst\": {}, \"snd\": {}}")
    law("MyEither[MyUnit, MyUnit]", "{\"left\": {}}")
    law("MyEither[MyUnit, MyUnit]", "{\"right\": {}}")
    law("MyNat", "0")
    law("MyNat", "42")

    // here are some examples of unsupported types
    unsupported("Int -> Int")
    unsupported("MyWrapper[Int -> String]")
    unsupported("forall a. MyWrapper[a]")

    illTypedJson("MyNat", "{}")
    illTypedJson("(Int, String)", "[1, 2]")
    illTypedJson("(Int, String, Bool)", "[1, \"2\", null]")
    illTypedJson("List[Int]", "[1, \"2\", 3]")
    illTypedJson("String", "1")
    illTypedJson("MyPair[MyUnit, MyUnit]", "0")
    illTypedJson("List[Unit]", "[null, 1]")
  }
}
