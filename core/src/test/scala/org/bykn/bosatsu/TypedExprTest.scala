package org.bykn.bosatsu

import cats.data.Writer
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import scala.collection.immutable.SortedSet

import TestUtils.checkLast

import Identifier.{Bindable, Name}
import rankn.{Type, NTypeGen}

class TypedExprTest extends FunSuite {

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  def allVars[A](te: TypedExpr[A]): Set[Bindable] = {
    type W[B] = Writer[Set[Bindable], B]

    te.traverseUp[W] {
      case v@TypedExpr.Local(ident, _, _) => Writer(Set(ident), v)
      case notVar => Writer(Set.empty, notVar)
    }.run._1
  }

  test("freeVarsSet is a subset of allVars") {
    def law[A](te: TypedExpr[A]) = {
      val frees = TypedExpr.freeVarsSet(te :: Nil).toSet
      val av = allVars(te)
      val missing = frees -- av
      assert(missing.isEmpty, s"expression:\n\n${te.repr}\n\nallVars: $av\n\nfrees: $frees")
    }

    forAll(genTypedExpr)(law _)

    checkLast("""
enum AB: A, B(x)
x = match B(100):
  A: 10
  B(b): b
""")(law)
  }

  test("freeVars on top level TypedExpr is Nil") {
    // all of the names are local to a TypedExpr, nothing can be free
    // id and x are fully resolved to top level
    checkLast("""#
x = 1
def id(x): x
y = id(x)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
x = 1
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(1, 2)
y = match x:
  Tup2(a, _): a
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }
  }

  val intTpe = Type.IntType

  def lit(l: Lit): TypedExpr[Unit] =
    TypedExpr.Literal(l, Type.getTypeOf(l), ())

  def int(i: Int): TypedExpr[Unit] =
    lit(Lit.fromInt(i))

  def varTE(n: String, tpe: Type): TypedExpr[Unit] =
    TypedExpr.Local(Identifier.Name(n), tpe, ())

  def let(n: String, ex1: TypedExpr[Unit], ex2: TypedExpr[Unit]): TypedExpr[Unit] =
    TypedExpr.Let(Identifier.Name(n), ex1, ex2, RecursionKind.NonRecursive, ())

  def letrec(n: String, ex1: TypedExpr[Unit], ex2: TypedExpr[Unit]): TypedExpr[Unit] =
    TypedExpr.Let(Identifier.Name(n), ex1, ex2, RecursionKind.Recursive, ())

  def app(fn: TypedExpr[Unit], arg: TypedExpr[Unit], tpe: Type): TypedExpr[Unit] =
    TypedExpr.App(fn, arg, tpe, ())

  test("test let substitution") {
    {
      // substitution in let
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assert(TypedExpr.substitute(Identifier.Name("x"), int(2), let1) ==
        Some(let("y", int(2), varTE("y", intTpe))))
    }

    {
      // substitution in let with a masking
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assert(TypedExpr.substitute(Identifier.Name("x"), varTE("y", intTpe), let1) ==
        None)
    }

    {
      // substitution in let with a shadowing in result
      val let1 = let("y", varTE("y", intTpe), varTE("y", intTpe))
      assert(TypedExpr.substitute(Identifier.Name("y"), int(42), let1) ==
        Some(let("y", int(42), varTE("y", intTpe))))
    }

    {
      // substitution in letrec with a shadowing in bind and result
      val let1 = letrec("y", varTE("y", intTpe), varTE("y", intTpe))
      assert(TypedExpr.substitute(Identifier.Name("y"), int(42), let1) ==
        Some(let1))
    }
  }

  lazy val genNonFree: Gen[TypedExpr[Unit]] =
   genTypedExpr.flatMap { te =>
     if (TypedExpr.freeVars(te :: Nil).isEmpty) Gen.const(te)
     else genNonFree
   }

  test("after substitution, a variable is no longer free") {
    forAll(genTypedExpr, genNonFree) { (te0, te1) =>
      TypedExpr.freeVars(te0 :: Nil) match {
        case Nil => ()
        case b :: _ =>
          TypedExpr.substitute(b, te1, te0) match {
            case None =>
              // te1 has no free variables, this shouldn't fail
              assert(false)

            case Some(te0sub) =>
              assert(!TypedExpr.freeVarsSet(te0sub :: Nil)(b))
          }
      }
    }
  }

  test("substituting a non-free variable is identity") {
    def genNotFree[A](te: TypedExpr[A]): Gen[Bindable] = {
      val frees = TypedExpr.freeVarsSet(te :: Nil).toSet
      lazy val nf: Gen[Bindable] =
        Generators.bindIdentGen.flatMap {
          case isfree if frees(isfree) => nf
          case notfree => Gen.const(notfree)
        }

      nf
    }

    val pair = for {
      te <- genTypedExpr
      nf <- genNotFree(te)
    } yield (nf, te)

    forAll(pair, genNonFree) { case ((b, te0), te1) =>
        TypedExpr.substitute(b, te1, te0) match {
          case None =>
            // te1 has no free variables, this shouldn't fail
            assert(false)

          case Some(te0sub) => assert(te0sub == te0)
        }
    }
  }


  test("test some basic normalizations") {
    // inline lets of vars
    assert(TypedExpr.normalize(let("x", varTE("y", intTpe), varTE("x", intTpe))) ==
      Some(varTE("y", intTpe)))

    // we can't inline a shadow
    // x = y
    // y = z(43)
    // x(y, y)
    val normalLet =
      let("x", varTE("y", intTpe),
        let("y", app(varTE("z", intTpe), int(43), intTpe),
           app(app(varTE("x", intTpe), varTE("y", intTpe), intTpe),
             varTE("y", intTpe), intTpe)))

    assert(TypedExpr.normalize(normalLet) == None)

    // if w doesn't have x free:
    // (app (let x y z) w) == let x y (app z w)
    assert(TypedExpr.normalize(app(normalLet, varTE("w", intTpe), intTpe)) ==
      Some(
        let("x", varTE("y", intTpe),
          let("y", app(varTE("z", intTpe), int(43), intTpe),
             app(app(app(varTE("x", intTpe), varTE("y", intTpe), intTpe),
               varTE("y", intTpe), intTpe),
               varTE("w", intTpe), intTpe)))))
  }

  val genTypedExpr = Generators.genTypedExpr(Gen.const(()), 3, NTypeGen.genDepth03)

  test("TypedExpr.substituteTypeVar of identity is identity") {
    forAll(genTypedExpr, Gen.listOf(NTypeGen.genBound)) { (te, bounds) =>
      val identMap: Map[Type.Var, Type] = bounds.map { b => (b, Type.TyVar(b)) }.toMap
      assert(TypedExpr.substituteTypeVar(te, identMap) == te)
    }
  }

  test("TypedExpr.substituteTypeVar is idempotent") {
    forAll(genTypedExpr, Gen.listOf(NTypeGen.genBound)) { (te, bounds) =>
      val tpes = te.allTypes
      val avoid = tpes.toSet | bounds.map(Type.TyVar(_)).toSet
      val replacements = Type.allBinders.iterator.filterNot { t => avoid(Type.TyVar(t)) }
      val identMap: Map[Type.Var, Type] =
        bounds.iterator.zip(replacements)
          .map { case (b, v) => (b, Type.TyVar(v)) }
          .toMap
      val te1 = TypedExpr.substituteTypeVar(te, identMap)
      val te2 = TypedExpr.substituteTypeVar(te1, identMap)
      assert(te2 == te1)
    }
  }

  test("TypedExpr.Let.selfCallKind terminates and doesn't throw") {
    // pretty weak test, but just to make sure nothing ever blows up
    forAll(Generators.bindIdentGen, genTypedExpr) { (b, te) =>
      assert(TypedExpr.selfCallKind(b, te) ne null)
    }
  }

  test("SelfCallKind forms a lattice") {
    import TypedExpr.SelfCallKind._
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

  test("TypedExpr.substituteTypeVar is not an identity function") {
    // if we replace all the current types with some bound types, things won't be the same
    forAll(genTypedExpr) { te =>
      val tpes: Set[Type.Var] = te.allTypes.iterator.collect { case Type.TyVar(b) => b }.toSet

      implicit def setM[A: Ordering]: cats.Monoid[SortedSet[A]] =
        new cats.Monoid[SortedSet[A]] {
          def empty = SortedSet.empty
          def combine(a: SortedSet[A], b: SortedSet[A]) = a ++ b
      }

      // All the vars that are used in bounds
      val bounds: Set[Type.Var] = te.traverseType { t: Type =>
        t match {
          case Type.ForAll(ps, _) => Writer(SortedSet[Type.Var](ps.toList: _*), t)
          case _ => Writer(SortedSet[Type.Var](), t)
        }
      }.run._1.toSet[Type.Var]

      val replacements = Type.allBinders.iterator.filterNot(tpes)
      val identMap: Map[Type.Var, Type] =
        tpes.filterNot(bounds)
          .iterator
          .zip(replacements)
          .map { case (b, v) => (b, Type.TyVar(v)) }
          .toMap

      if (identMap.nonEmpty) {
        val te1 = TypedExpr.substituteTypeVar(te, identMap)
        assert(te1 != te, s"mapping: $identMap, $bounds")
      }
    }
  }

  test("TypedExpr.allTypes contains the type") {
    forAll(genTypedExpr) { te =>
      assert(te.allTypes.contains(te.getType))
    }
  }

  def count[A](te: TypedExpr[A])(fn: PartialFunction[TypedExpr[A], Boolean]): Int = {
    type W[B] = Writer[Int, B]
    val (count, _) =
      te.traverseUp[W] { inner =>
        val c = if (fn.isDefinedAt(inner) && fn(inner)) 1 else 0
        Writer(c, inner)
      }.run

    count
  }

  def countMatch[A](te: TypedExpr[A]) = count(te) { case TypedExpr.Match(_, _, _) => true }
  def countLet[A](te: TypedExpr[A]) = count(te) { case TypedExpr.Let(_, _, _, _, _) => true }

  test("test match removed from some examples") {
    checkLast(
      """
x = \_ -> 1
""") { te => assert(countMatch(te) == 0) }

    checkLast(
      """
x = 10
y = match x:
  z: z
""") { te => assert(countMatch(te) == 0) }

    checkLast(
      """
x = 10
y = match x:
  _: 20
""") { te => assert(countMatch(te) == 0) }
  }

  test("test let removed from some examples") {
    // this should turn into `y = 20` as the last expression
    checkLast(
      """
x = 10
y = match x:
  _: 20
""") { te => assert(countLet(te) == 0) }

    checkLast(
      """
def foo:
  x = 1
  _ = x
  42
""") { te => assert(countLet(te) == 0) }
  }

  test("toArgsBody always terminates") {
    forAll(Gen.choose(0, 10), genTypedExpr) { (arity, te) =>
      // this is a pretty weak test.
      assert(TypedExpr.toArgsBody(arity, te) ne null)
    }
  }

  test("test selfCallKind") {
    import TypedExpr.SelfCallKind.{NoCall, NonTailCall, TailCall}

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list, acc):
  recur list:
    E: acc
    NE(_, t): list_len(t, S(acc))
""") { te => assert(TypedExpr.selfCallKind(Name("list_len"), te) == TailCall) }

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list):
  recur list:
    E: Z
    NE(_, t): S(list_len(t))
""") { te => assert(TypedExpr.selfCallKind(Name("list_len"), te) == NonTailCall) }

    checkLast(
      """
enum List[a]: E, NE(head: a, tail: List[a])

def list_len(list):
  match list:
    E: 0
    NE(_, _): 1
""") { te => assert(TypedExpr.selfCallKind(Name("list_len"), te) == NoCall) }

  }
}
