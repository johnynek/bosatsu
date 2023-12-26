package org.bykn.bosatsu

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.bykn.bosatsu.rankn.NTypeGen
import org.bykn.bosatsu.TestUtils.checkLast
import org.bykn.bosatsu.Identifier.Name

class SelfCallKindTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  def gen[A](g: Gen[A]): Gen[TypedExpr[A]] =
    Generators.genTypedExpr(g, 3, NTypeGen.genDepth03)

  val genTypedExpr: Gen[TypedExpr[Unit]] =
    gen(Gen.const(()))

  test("test selfCallKind") {
    import SelfCallKind.{NoCall, NonTailCall, TailCall, apply as selfCallKind}

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list, acc):
  recur list:
    case E: acc
    case NE(_, t): list_len(t, S(acc))
""") { te => assert(selfCallKind(Name("list_len"), te) == TailCall) }

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list):
  recur list:
    case E: Z
    case NE(_, t): S(list_len(t))
""") { te => assert(selfCallKind(Name("list_len"), te) == NonTailCall) }

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])

def list_len(list):
  match list:
    case E: 0
    case NE(_, _): 1
""") { te => assert(selfCallKind(Name("list_len"), te) == NoCall) }

  }

  test("for_all example") {
    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])
enum B: T, F

def for_all(xs: List[a], fn: a -> B) -> B:
  recur xs:
    case E: T
    case NE(head, tail):
      match fn(head):
        case T: for_all(tail, fn)
        case F: F
""") { te => assert(SelfCallKind(Name("for_all"), te) == SelfCallKind.TailCall, s"${te.repr}") }
  }

  test("TypedExpr.Let.selfCallKind terminates and doesn't throw") {
    // pretty weak test, but just to make sure nothing ever blows up
    forAll(Generators.bindIdentGen, genTypedExpr) { (b, te) =>
      assert(SelfCallKind(b, te) ne null)
    }
  }

  test("SelfCallKind forms a lattice") {
    import SelfCallKind.*
    val scs = List(NoCall, TailCall, NonTailCall)

    for {
      a <- scs
      b <- scs
      c <- scs
    } {
      assert(a.merge(b) == b.merge(a))
      assert(a.merge(b.merge(c)) == a.merge(b).merge(c))
    }

    scs.foreach { a =>
      assert(a.merge(a) == a)
      assert((a.ifNoCallThen(null) eq null) == (a == NoCall))
      assert(NoCall.merge(a) == a)
      assert(NonTailCall.merge(a) == NonTailCall)
      assert(a.callNotTail != TailCall)
      assert((a.callNotTail == NoCall) == (a == NoCall))
    }
  }

}