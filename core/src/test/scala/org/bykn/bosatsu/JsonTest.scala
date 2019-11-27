package org.bykn.bosatsu

import cats.implicits._
import org.scalatest.prop.PropertyChecks.{forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import TestUtils.typeEnvOf

import rankn.{NTypeGen, Type}

import GenJson._

class JsonTest extends FunSuite {

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 5000)

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

  // NumberKind is the lowest possible kind
  def kindLaw(num: Json.JNumberStr) = {
    import Json.NumberKind
    import NumberKind._

    val k = num.numberKind
    assert(assertParser(k.asString).asInstanceOf[Json.JNumberStr].numberKind == k)

    def opt(n: => NumberKind): Option[NumberKind] =
      try Some(n)
      catch {
        case (_: NumberFormatException) => None
      }

    def lower(nk: NumberKind): Option[NumberKind] =
      nk match {
        case IntKind(i) => None
        case LongKind(l) => opt(IntKind(l.toString.toInt))
        case BigIntKind(b) => opt(LongKind(b.toString.toLong))
        case DoubleKind(d) => opt(BigIntKind(new java.math.BigInteger(d.toString)))
        case BigDecimalKind(b) => opt(DoubleKind(b.toString.toDouble))
        case GiantDecimalKind(d) => opt(BigDecimalKind(new java.math.BigDecimal(d)))
      }

    lower(k) match {
      case None => ()
      case Some(lk) => assert(NumberKind(lk.asString) != k)
    }
  }

  test("we can parse all the json we generate") {
    forAll { j: Json => law(j) }
  }

  test("we can parse hard Json numbers") {
    forAll(genJsonNumber)(law(_))

    forAll(genJsonNumber) { num =>
      kindLaw(num)
    }
  }

  test("we can decode and encode json in the same cases") {
    val jsonCodec = ValueToJson({ _ => None})

    def law(t: Type, j: Json) = {
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
        case Right(j1) => assert(j1.render == j.render)
        case Left(_) => ()
      }
    }

    forAll(NTypeGen.genPredefType, GenJson.arbJson.arbitrary)(law(_, _))

    val regressions = List((Type.TyApply(Type.OptionType, Type.BoolType), Json.JBool.False))

    regressions.foreach { case (t, j) => law(t, j) }
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
              assert(toV(j).flatMap(toJ) == Right(j))
            case Left(err) => fail(s"could not handle to Json: $tpe, $t, $toV")
          }
        case Left(err) => fail(s"could not handle to Value: $tpe, $t, $toJ")
      }
    }

    law("Int", "42")
    law("String", "\"hello world\"")
    law("Option[Int]", "null")
    law("Option[Int]", "42")
    law("Dict[String, Int]", "{ \"foo\": 42 }")
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
  }
}
