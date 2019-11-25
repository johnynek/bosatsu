package org.bykn.bosatsu

import cats.data.Writer
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import scala.collection.immutable.SortedSet

import TestUtils.checkLast

import Identifier.Name
import rankn.{Type, NTypeGen}

class TypedExprTest extends FunSuite {

  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  def allVars[A](te: TypedExpr[A]): Set[Identifier] = {
    type W[B] = Writer[Set[Identifier], B]

    te.traverseUp[W] {
      case v@TypedExpr.Var(None, ident, _, _) => Writer(Set(ident), v)
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

  test("freeVars on simple cases works") {
    checkLast("""#
x = 1
def id(x): x
y = id(x)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Name("id"), Name("x"))) }

    checkLast("""#
x = 1
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Identifier.Constructor("Tup2"))) }

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
y = match x:
  Tup2(a, _): a
""") { te => assert(TypedExpr.freeVars(te :: Nil) == List(Name("x"))) }
  }

  val intTpe = Type.IntType

  def lit(l: Lit): TypedExpr[Unit] =
    TypedExpr.Literal(l, Type.getTypeOf(l), ())

  def int(i: Int): TypedExpr[Unit] =
    lit(Lit.fromInt(i))

  def varTE(n: String, tpe: Type): TypedExpr[Unit] =
    TypedExpr.Var(None, Identifier.Name(n), tpe, ())

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
}
