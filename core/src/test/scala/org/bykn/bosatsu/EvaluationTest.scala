package org.bykn.bosatsu

import cats.data.Validated
import cats.implicits._
import org.scalatest.FunSuite

import Evaluation.Value._

class EvaluationTest extends FunSuite {
  def evalTest(packages: List[String], mainPackS: String, expected: Any, extern: Externals = Externals.empty) =
    evalTestEither(packages, mainPackS, Left(expected), extern)

  def evalTestJson(packages: List[String], mainPackS: String, expected: Json, extern: Externals = Externals.empty) =
    evalTestEither(packages, mainPackS, Right(expected), extern)

  def evalTestEither(packages: List[String], mainPackS: String, expected: Either[Any, Json], extern: Externals = Externals.empty) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          p.showContext.foreach(System.err.println)
        }
        sys.error(errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths)) match {
      case (dups, Validated.Valid(packMap)) if dups.isEmpty =>
        val ev = Evaluation(packMap, Predef.jvmExternals ++ extern)
        ev.evaluateLast(mainPack) match {
          case None => fail("found no main expression")
          case Some((eval, schm)) =>
            expected match {
              case Left(exp) => assert(eval.value == exp)
              case Right(json) =>
                assert(ev.toJson(eval.value, schm) === Some(json))
            }
        }

      case (other, Validated.Invalid(errs)) =>
        val tes = errs.toList.collect {
          case PackageError.TypeErrorIn(te, _) =>
            te.message
        }
        .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }
  }

  def evalFail(packages: List[String], mainPackS: String, extern: Externals = Externals.empty) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        sys.error(errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths)) match {
      case (_, Validated.Valid(_)) =>
        fail("expected to fail type checking")

      case (_, Validated.Invalid(errs)) if errs.collect { case PackageError.TypeErrorIn(_, _) => () }.nonEmpty =>
        assert(true)
      case (_, Validated.Invalid(errs)) =>
          fail(s"failed, but no type errors: $errs")
    }
  }

  test("simple evaluation") {
    evalTest(
      List("""
package Foo

x = 1
"""), "Foo", VInt(1))
  }

  test("test if/else") {
    evalTest(
      List("""
package Foo

x = 1

z = match x.cmp_Int(1):
  EQ:
    "foo"
  _:
    "bar"
"""), "Foo", Str("foo"))

    evalTest(
      List("""
package Foo

x = 1

# here if the single expression python style
z = "foo" if x.eq_Int(2) else "bar"
"""), "Foo", Str("bar"))
  }

  test("exercise option from predef") {
    evalTest(
      List("""
package Foo

x = Some(1)

z = match x:
  Some(v):
    add(v, 10)
  None:
    0
"""), "Foo", VInt(11))
  }

  test("do a fold") {
    evalTest(
      List("""
package Foo

three = NonEmptyList(1, NonEmptyList(2, EmptyList))

def sum(ls):
  ls.foldLeft(0, add)

sum0 = sum(three)
sum1 = three.foldLeft(0, \x, y -> add(x, y))

same = sum0.eq_Int(sum1)
"""), "Foo", True)

    evalTest(
      List("""
package Foo

three = NonEmptyList(1, NonEmptyList(2, EmptyList))

sum0 = three.foldLeft(0, add)
sum1 = three.foldLeft(0, \x, y -> add(x, y))

same = sum0.eq_Int(sum1)
"""), "Foo", True)

  }

  test("test Int functions") {
    evalTest(
      List("""
package Foo

main = 6.mod_Int(4)
"""), "Foo", VInt(2))
  }

  test("use range") {
    evalTest(
      List("""
package Foo

three = NonEmptyList(0, NonEmptyList(1, EmptyList))
# exercise the built-in range function (not implementable in bosatsu)
threer = range(3)

def reverse(ls):
  # note foldLeft is also built in, and not implementable
  ls.foldLeft(EmptyList, \tail, h -> NonEmptyList(h, tail))

struct Pair(fst, sec)

def zip(as: List[a], bs: List[b]) -> List[Pair[a, b]]:
  def cons(pair, item):
    match pair:
      Pair(acc, EmptyList):
        Pair(acc, EmptyList)
      Pair(acc, NonEmptyList(h, tail)):
        Pair(NonEmptyList(Pair(item, h), acc), tail)

  rev = as.foldLeft(Pair(EmptyList, bs), cons)
  match rev:
    Pair(res, _):
      reverse(res)

def and(a, b):
  b if a else False

def same_items(items, eq):
  def test(p):
    match trace("in same items", p):
      Pair(a, b):
        eq(a, b)

  items.foldLeft(True, \res, t -> and(res, test(t)))

def eq_list(a, b, fn):
  same_items(zip(a, b), fn)

same = eq_list(three, threer)(eq_Int)
"""), "Foo", True)

evalTest(
  List("""
package Foo

def reverse(ls):
  # note foldLeft is also built in, and not implementable
  ls.foldLeft(EmptyList, \tail, h -> NonEmptyList(h, tail))

struct Pair(fst, sec)

def zip(as: List[a], bs: List[b]) -> List[Pair[a, b]]:
  def cons(pair: Pair[List[Pair[a, b]], List[b]], item: a) -> Pair[List[Pair[a, b]], List[b]]:
    match pair:
      Pair(acc, EmptyList):
        Pair(acc, EmptyList)
      Pair(acc, NonEmptyList(h, tail)):
        Pair(NonEmptyList(Pair(item, h), acc), tail)

  rev = as.foldLeft(Pair(EmptyList, bs), cons)
  match rev:
    Pair(res, _):
      reverse(res)

main = 1
"""), "Foo", VInt(1))

  }

  test("test generics in defs") {
    evalTest(
      List("""
package Foo

def id(x: a) -> a:
  x

main = id(1)
"""), "Foo", VInt(1))
  }

  test("exercise struct creation") {
    evalTest(
      List("""
package Foo

struct Bar(a: Int)

main = Bar(1)
"""), "Foo",
  ConsValue(VInt(1), UnitValue))

    evalTestJson(
      List("""
package Foo

struct Bar(a: Int, s: String)

main = Bar(1, "foo")
"""), "Foo", Json.JObject(Map("a" -> Json.JNumberStr("1"), "s" -> Json.JString("foo"))))
  }

  test("test some type errors") {
    evalFail(
      List("""
package Foo

main = if True:
  1
else:
  "1"
"""), "Foo")
  }
}
