package dev.bosatsu

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import dev.bosatsu.rankn.NTypeGen
import dev.bosatsu.TestUtils.checkLast
import dev.bosatsu.Identifier.Name

class SelfCallKindTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)

  def gen[A](g: Gen[A]): Gen[TypedExpr[A]] =
    Generators.genTypedExpr(g, 3, NTypeGen.genDepth03)

  val genTypedExpr: Gen[TypedExpr[Unit]] =
    gen(Gen.const(()))

  test("test selfCallKind") {
    import SelfCallKind.{NoCall, NonTailCall, TailCall, apply => selfCallKind}

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list, acc):
  recur list:
    case E: acc
    case NE(_, t): list_len(t, S(acc))
""")(te => assertEquals(selfCallKind(Name("list_len"), te), TailCall))

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list):
  recur list:
    case E: Z
    case NE(_, t): S(list_len(t))
""")(te => assertEquals(selfCallKind(Name("list_len"), te), NonTailCall))

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])

def list_len(list):
  match list:
    case E: 0
    case NE(_, _): 1
""")(te => assertEquals(selfCallKind(Name("list_len"), te), NoCall))

  }

  test("for_all example") {
    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])
enum B: T, F

def for_all(xs: List[a], fn: a -> B) -> B:
  recur xs:
    case E: T
    case NE(head, tail):
      match fn(head):
        case T: for_all(tail, fn)
        case F: F
""") { te =>
      assertEquals(
        SelfCallKind(Name("for_all"), te),
        SelfCallKind.TailCall,
        s"${te.repr}"
      )
    }
  }

  test("TypedExpr.Let.selfCallKind terminates and doesn't throw") {
    // pretty weak test, but just to make sure nothing ever blows up
    forAll(Generators.bindIdentGen, genTypedExpr) { (b, te) =>
      assert(SelfCallKind(b, te) ne null)
    }
  }

  test("SelfCallKind forms a lattice") {
    import SelfCallKind._
    val scs = List(NoCall, TailCall, NonTailCall)

    for {
      a <- scs
      b <- scs
      c <- scs
    } {
      assertEquals(a.merge(b), b.merge(a))
      assertEquals(a.merge(b.merge(c)), a.merge(b).merge(c))
    }

    scs.foreach { a =>
      assertEquals(a.merge(a), a)
      assertEquals(
        scs.forall { other => (a.ifNoCallThen(other) eq other) }, (a == NoCall))
      assertEquals(NoCall.merge(a), a)
      assertEquals(NonTailCall.merge(a), NonTailCall)
      assert(a.callNotTail != TailCall)
      assertEquals((a.callNotTail == NoCall), (a == NoCall))
    }
  }

}
