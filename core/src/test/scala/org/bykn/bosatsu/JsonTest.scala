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

  test("we can parse all the json we generate") {
    forAll { (j: Json) =>
      val str = j.render
      Json.parser.parse(str) match {
        case fastparse.all.Parsed.Success(j1, _) =>
          // JNumber and JNumberStr can be confused by parsing
          // since we prefer JNumber on parse if we don't lose
          // precision
          assert(j1.render == str)
        case other => fail(s"failed to parse:\n\n$str\n\n$j\n\nerror: $other")
      }
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

struct Foo(foo1: Int, foo2: String)

""")
    val jsonConv = ValueToJson(te.toDefinedType(_))

    def stringToType(t: String): Type = {
      val tr = TypeRef.parser.parse(t) match {
        case fastparse.all.Parsed.Success(tr, l) if l == t.length => tr
        case other => sys.error(s"could not parse: $t, $other")
      }

      TypeRefConverter[cats.Id](tr) { cons => Type.Const.predef(cons.asString) }
    }

    def stringToJson(s: String): Json =
      Json.parser.parse(s) match {
        case fastparse.all.Parsed.Success(j, l) if l == s.length => j
        case other => sys.error(s"could not parse: $s, $other")
      }

    def law(tpe: String, json: String) = {
      val t = stringToType(tpe)
      jsonConv.toValue(t) match {
        case Right(toV) =>
          jsonConv.toJson(t) match {
            case Right(toJ) =>
              val j = stringToJson(json)
              assert(toV(j).flatMap(toJ) == Right(j))
            case Left(err) => fail(s"could not handle to Json: $tpe, $t")
          }
        case Left(err) => fail(s"could not handle to Value: $tpe, $t")
      }
    }

    law("Int", "42")
    law("String", "\"hello world\"")
    law("Option[Int]", "null")
    law("Option[Int]", "42")
    law("Dict[String, Int]", "{ \"foo\": 42 }")
    law("List[Int]", "[1, 2, 3, 4]")
    law("Option[Bool]", "true")
    law("Option[Bool]", "false")
    law("Option[Bool]", "null")
    law("Option[Option[Bool]]", "[true]")
    law("Option[Option[Bool]]", "[false]")
    law("Option[Option[Bool]]", "[null]")
    law("Option[Option[Bool]]", "[]")
  }
}
