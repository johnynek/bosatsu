package org.bykn.bosatsu

import org.scalatest.FunSuite

import TestUtils.checkLast

import Identifier.Name
import rankn.Type

class TypedExprTest extends FunSuite {
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
}
