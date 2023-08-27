package org.bykn.bosatsu

import cats.data.{State, Writer}
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import scala.collection.immutable.SortedSet

import Arbitrary.arbitrary
import Identifier.{Bindable, Name}
import TestUtils.checkLast
import rankn.{Type, NTypeGen}

class TypedExprTest extends AnyFunSuite {

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)

  def allVars[A](te: TypedExpr[A]): Set[Bindable] = {
    type W[B] = Writer[Set[Bindable], B]

    te.traverseUp[W] {
      case v @ TypedExpr.Local(ident, _, _) => Writer(Set(ident), v)
      case notVar                           => Writer(Set.empty, notVar)
    }.run
      ._1
  }

  /** Assert two bits of code normalize to the same thing
    */
  def normSame(s1: String, s2: String) =
    checkLast(s1) { t1 =>
      checkLast(s2) { t2 =>
        assert(t1.void == t2.void)
      }
    }

  test("freeVarsSet is a subset of allVars") {
    def law[A](te: TypedExpr[A]) = {
      val frees = TypedExpr.freeVarsSet(te :: Nil).toSet
      val av = allVars(te)
      val missing = frees -- av
      assert(
        missing.isEmpty,
        s"expression:\n\n${te.repr}\n\nallVars: $av\n\nfrees: $frees"
      )
    }

    forAll(genTypedExpr)(law _)

    checkLast("""
enum AB: A, B(x)
x = match B(100):
  case A: 10
  case B(b): b
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
  case Tup2(a, _): a
""") { te => assert(TypedExpr.freeVars(te :: Nil) == Nil) }
  }

  test("we can inline struct/destruct") {
    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(1, 2)
y = match x:
  case Tup2(a, _): a
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    normSame(
      """#
struct Tup2(a, b)

x = 23
x = Tup2(1, 2)
y = match x:
  case Tup2(a, _): a
""",
      """#
y = 1
"""
    )

    checkLast("""#
struct Tup2(a, b)

inner = (
  z = 1
  fn = Tup2
  x = fn(z, 2)
  match x:
    case Tup2(a, _): a
)
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

inner = (
  x = Tup2(1, 2)
  match x:
    case Tup2(a, _): a
)

y = inner
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
cons = Tup2
x = cons(1, 2)
y = match x:
  case Tup2(a, _): a
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
cons = Tup2
x = cons(1, 2)
y = match x:
  case Tup2(_, _) as t:
    Tup2(a, _) = t
    a
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(Tup2(1, 3), 2)
y = match x:
  case Tup2(Tup2(a, _), _): a
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(0, Tup2(1, 2))
y = match x:
  case Tup2(_, Tup2(a, _)): a
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
enum E: Left(l), Right(r)

x = 23
x = Left(1)
y = match x:
  case Left(l): l
  case Right(r): r
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
enum E: Left(l), Right(r)

inner = (
  x = Left(1)
  z = 1
  match x:
    case Left(_): z
    case Right(r):
      match z:
        case 1: 1
        case _: r
)
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
x = 23
y = match x:
  case 42: 0
  case 23 as y: y
  case _: -1
""") {
      case TypedExpr.Literal(lit, _, _) => assert(lit == Lit.fromInt(23))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }
  }

  test("we can lift a match above a lambda") {
    normSame(
      """#
struct Tup2(a, b)

y = Tup2(1, 2)

def inner_match(x):
  match y:
    case Tup2(a, _): Tup2(a, x)
""",
      """#
struct Tup2(a, b)
inner_match = x -> Tup2(1, x)
"""
    )

    normSame(
      """#
struct Tup2(a, b)
enum Eith: L(left), R(right)

def run(y):
  def inner_match(x):
    match y:
      case L(a): Tup2(a, x)
      case R(b): Tup2(x, b)

  inner_match
""",
      """#
struct Tup2(a, b)
enum Eith: L(left), R(right)

def run(y):
  match y:
    case L(a): x -> Tup2(a, x)
    case R(b): x -> Tup2(x, b)
"""
    )
  }

  test("we can push lets into match") {
    normSame(
      """#
struct Tup2(a, b)
enum Eith: L(left), R(right)

def run(y, x):
  z = y
  match x:
    L(_): z
    R(r): r
""",
      """#
struct Tup2(a, b)
enum Eith: L(left), R(right)

def run(y, x):
  match x:
    L(_): y
    R(r): r
"""
    )
  }

  test("we can evaluate constant matches") {
    normSame(
      """#
x = match 1:
  case (1 | 2) as x: x
  case _: -1
""",
      """#
x = 1
"""
    )

    normSame(
      """#
x = match 1:
  case _: -1
""",
      """#
x = -1
"""
    )

    normSame(
      """#
y = 21

def foo(_):
  match y:
    case 42: 0
    case x: x
""",
      """#
foo = _ -> 21
"""
    )

    /*
     * This does not yet work
    normSame("""#
struct Tup2(a, b)

def foo(_):
  y = 1
  z = Tup2(y, y)
  y = 0
  match z:
    case Tup2(0, 0): y
    case Tup2(1, 1): 1
    case _: 2
""", """#
foo = _ -> 1
""")
     */
  }

  val intTpe = Type.IntType

  def lit(l: Lit): TypedExpr[Unit] =
    TypedExpr.Literal(l, Type.getTypeOf(l), ())

  def int(i: Int): TypedExpr[Unit] =
    lit(Lit.fromInt(i))

  def varTE(n: String, tpe: Type): TypedExpr[Unit] =
    TypedExpr.Local(Identifier.Name(n), tpe, ())

  def let(
      n: String,
      ex1: TypedExpr[Unit],
      ex2: TypedExpr[Unit]
  ): TypedExpr[Unit] =
    TypedExpr.Let(Identifier.Name(n), ex1, ex2, RecursionKind.NonRecursive, ())

  def letrec(
      n: String,
      ex1: TypedExpr[Unit],
      ex2: TypedExpr[Unit]
  ): TypedExpr[Unit] =
    TypedExpr.Let(Identifier.Name(n), ex1, ex2, RecursionKind.Recursive, ())

  def app(
      fn: TypedExpr[Unit],
      arg: TypedExpr[Unit],
      tpe: Type
  ): TypedExpr[Unit] =
    TypedExpr.App(fn, arg, tpe, ())

  def lam(n: String, nt: Type, res: TypedExpr[Unit]): TypedExpr[Unit] =
    TypedExpr.AnnotatedLambda(Identifier.Name(n), nt, res, ())

  test("test let substitution") {
    {
      // substitution in let
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assert(
        TypedExpr.substitute(Identifier.Name("x"), int(2), let1) ==
          Some(let("y", int(2), varTE("y", intTpe)))
      )
    }

    {
      // substitution in let with a masking
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assert(
        TypedExpr.substitute(Identifier.Name("x"), varTE("y", intTpe), let1) ==
          None
      )
    }

    {
      // substitution in let with a shadowing in result
      val let1 = let("y", varTE("y", intTpe), varTE("y", intTpe))
      assert(
        TypedExpr.substitute(Identifier.Name("y"), int(42), let1) ==
          Some(let("y", int(42), varTE("y", intTpe)))
      )
    }

    {
      // substitution in letrec with a shadowing in bind and result
      val let1 = letrec("y", varTE("y", intTpe), varTE("y", intTpe))
      assert(
        TypedExpr.substitute(Identifier.Name("y"), int(42), let1) ==
          Some(let1)
      )
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
          case notfree                 => Gen.const(notfree)
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

  test("let x = y in x == y") {
    // inline lets of vars
    assert(
      TypedExprNormalization.normalize(
        let("x", varTE("y", intTpe), varTE("x", intTpe))
      ) ==
        Some(varTE("y", intTpe))
    )
  }

  val normalLet =
    let(
      "x",
      varTE("y", intTpe),
      let(
        "y",
        app(varTE("z", intTpe), int(43), intTpe),
        app(
          app(varTE("x", intTpe), varTE("y", intTpe), intTpe),
          varTE("y", intTpe),
          intTpe
        )
      )
    )

  test("we can't inline using a shadow: let x = y in let y = z in x(y, y)") {
    // we can't inline a shadow
    // x = y
    // y = z(43)
    // x(y, y)
    assert(TypedExprNormalization.normalize(normalLet) == None)
  }

  test("if w doesn't have x free: (app (let x y z) w) == let x y (app z w)") {
    assert(
      TypedExprNormalization.normalize(
        app(normalLet, varTE("w", intTpe), intTpe)
      ) ==
        Some(
          let(
            "x",
            varTE("y", intTpe),
            let(
              "y",
              app(varTE("z", intTpe), int(43), intTpe),
              app(
                app(
                  app(varTE("x", intTpe), varTE("y", intTpe), intTpe),
                  varTE("y", intTpe),
                  intTpe
                ),
                varTE("w", intTpe),
                intTpe
              )
            )
          )
        )
    )

  }

  test("x -> f(x) == f") {
    val f = varTE("f", Type.Fun(intTpe, intTpe))
    val left = lam("x", intTpe, app(f, varTE("x", intTpe), intTpe))

    assert(TypedExprNormalization.normalize(left) == Some(f))

    checkLast("""
struct Foo(a)
x = (y) -> Foo(y)
g = a -> x(a)
    """) { te1 =>
      checkLast("""
struct Foo(a)
x = Foo
      """) { te2 =>
        assert(te1.void == te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }

    checkLast("""
struct Foo(a)
x = Foo
    """) {
      case TypedExpr.Global(_, Identifier.Constructor("Foo"), _, _) =>
        assert(true)
      case notNorm =>
        fail(notNorm.repr)
    }
  }

  test("(x -> f(x, z))(y) == f(y, z)") {
    val int2int = Type.Fun(intTpe, intTpe)
    val f = varTE("f", Type.Fun(intTpe, int2int))
    val z = varTE("z", intTpe)
    val lamf =
      lam("x", intTpe, app(app(f, varTE("x", intTpe), int2int), z, intTpe))
    val y = varTE("y", intTpe)
    val left = app(lamf, y, intTpe)
    val right = app(app(f, y, int2int), z, intTpe)
    val res = TypedExprNormalization.normalize(left)

    assert(res == Some(right), s"${res.map(_.repr)} != Some(${right.repr}")

    checkLast("""
f = (_, y) -> y
z = 1
res = y -> (x -> f(x, z))(y)
""") { te1 =>
      checkLast("""
f = (_, y) -> y
res = y -> f(y, 1)
      """) { te2 =>
        assert(te1.void == te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }
  }

  test("lift let above lambda") {
    checkLast("""
enum FooBar: Foo, Bar

z = Foo
fn = (
  x -> (
    y = z
    match y:
      case Foo: x
      case Bar: y
  )
)
""") { te1 =>
      checkLast("""
enum FooBar: Foo, Bar

fn = (x: FooBar) -> x
    """) { te2 =>
        assert(te1.void == te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }
  }

  test("we destructure known structs") {
    checkLast("""
struct Data(a, b, c)
enum FooBar: Foo, Bar

x = (
  Data(_, _, c) = Data(Foo, Bar, Foo)
  c
)
""") { te1 =>
      checkLast("""
enum FooBar: Foo, Bar

x = Foo
    """) { te2 =>
        assert(te1.void == te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }
  }

  def gen[A](g: Gen[A]): Gen[TypedExpr[A]] =
    Generators.genTypedExpr(g, 3, NTypeGen.genDepth03)

  val genTypedExpr: Gen[TypedExpr[Unit]] =
    gen(Gen.const(()))

  val genTypedExprInt: Gen[TypedExpr[Int]] =
    gen(Gen.choose(Int.MinValue, Int.MaxValue))

  val genTypedExprChar: Gen[TypedExpr[Char]] =
    gen(Gen.choose('a', 'z'))

  test("TypedExpr.substituteTypeVar of identity is identity") {
    forAll(genTypedExpr, Gen.listOf(NTypeGen.genBound)) { (te, bounds) =>
      val identMap: Map[Type.Var, Type] = bounds.map { b =>
        (b, Type.TyVar(b))
      }.toMap
      assert(TypedExpr.substituteTypeVar(te, identMap) == te)
    }
  }

  test("TypedExpr.substituteTypeVar is idempotent") {
    forAll(genTypedExpr, Gen.listOf(NTypeGen.genBound)) { (te, bounds) =>
      val tpes = te.allTypes
      val avoid = tpes.toSet | bounds.map(Type.TyVar(_)).toSet
      val replacements = Type.allBinders.iterator.filterNot { t =>
        avoid(Type.TyVar(t))
      }
      val identMap: Map[Type.Var, Type] =
        bounds.iterator
          .zip(replacements)
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
      val tpes: Set[Type.Var] = te.allTypes.iterator.collect {
        case Type.TyVar(b) => b
      }.toSet

      implicit def setM[A: Ordering]: cats.Monoid[SortedSet[A]] =
        new cats.Monoid[SortedSet[A]] {
          def empty = SortedSet.empty
          def combine(a: SortedSet[A], b: SortedSet[A]) = a ++ b
        }

      // All the vars that are used in bounds
      val bounds: Set[Type.Var] = te
        .traverseType { (t: Type) =>
          t match {
            case Type.ForAll(ps, _) =>
              Writer(SortedSet[Type.Var](ps.toList.map(_._1): _*), t)
            case _ => Writer(SortedSet[Type.Var](), t)
          }
        }
        .run
        ._1
        .toSet[Type.Var]

      val replacements = Type.allBinders.iterator.filterNot(tpes)
      val identMap: Map[Type.Var, Type] =
        tpes
          .filterNot(bounds)
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

  def count[A](
      te: TypedExpr[A]
  )(fn: PartialFunction[TypedExpr[A], Boolean]): Int = {
    type W[B] = Writer[Int, B]
    val (count, _) =
      te.traverseUp[W] { inner =>
        val c = if (fn.isDefinedAt(inner) && fn(inner)) 1 else 0
        Writer(c, inner)
      }.run

    count
  }

  def countMatch[A](te: TypedExpr[A]) = count(te) {
    case TypedExpr.Match(_, _, _) => true
  }
  def countLet[A](te: TypedExpr[A]) = count(te) {
    case TypedExpr.Let(_, _, _, _, _) => true
  }

  test("test match removed from some examples") {
    checkLast("""
x = _ -> 1
""") { te => assert(countMatch(te) == 0) }

    checkLast("""
x = 10
y = match x:
  case z: z
""") { te => assert(countMatch(te) == 0) }

    checkLast("""
x = 10
y = match x:
  case _: 20
""") { te => assert(countMatch(te) == 0) }
  }

  test("test let removed from some examples") {
    // this should turn into `y = 20` as the last expression
    checkLast("""
x = 10
y = match x:
  case _: 20
""") { te => assert(countLet(te) == 0) }

    checkLast("""
foo = (
  x = 1
  _ = x
  42
)
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

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list, acc):
  recur list:
    case E: acc
    case NE(_, t): list_len(t, S(acc))
""") { te => assert(TypedExpr.selfCallKind(Name("list_len"), te) == TailCall) }

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])
enum N: Z, S(prev: N)

def list_len(list):
  recur list:
    case E: Z
    case NE(_, t): S(list_len(t))
""") { te =>
      assert(TypedExpr.selfCallKind(Name("list_len"), te) == NonTailCall)
    }

    checkLast("""
enum List[a]: E, NE(head: a, tail: List[a])

def list_len(list):
  match list:
    case E: 0
    case NE(_, _): 1
""") { te => assert(TypedExpr.selfCallKind(Name("list_len"), te) == NoCall) }

  }

  test("TypedExpr.fold matches traverse") {
    def law[A, B](init: A, te: TypedExpr[B])(fn: (A, B) => A) = {
      val viaFold = te.foldLeft(init)(fn)
      val viaTraverse = te.traverse_[State[A, *], Unit] { b =>
        for {
          i <- State.get[A]
          i1 = fn(i, b)
          _ <- State.set(i1)
        } yield ()
      }

      assert(viaFold == viaTraverse.runS(init).value, s"${te.repr}")
    }

    def lawR[A, B](te: TypedExpr[B], a: A)(fn: (B, A) => A) = {
      val viaFold = te
        .foldRight(cats.Eval.now(a)) { (b, r) => r.map { j => fn(b, j) } }
        .value
      val viaTraverse: State[A, Unit] = te.traverse_[State[A, *], Unit] { b =>
        for {
          i <- State.get[A]
          i1 = fn(b, i)
          _ <- State.set(i1)
        } yield ()
      }

      assert(viaFold == viaTraverse.runS(a).value, s"${te.repr}")
    }

    forAll(genTypedExprInt, Gen.choose(0, 1000)) { (te, init) =>
      // make a commutative int function
      law(init, te) { (a, b) => (a + 1) * b }
      lawR(te, init) { (a, b) => (a + 1) + b }
      law(init, te) { (a, b) => a + b }
    }
  }

  test("foldMap from traverse matches foldMap") {
    import cats.data.Const
    import cats.Monoid

    def law[A, B: Monoid](te: TypedExpr[A])(fn: A => B) = {
      val viaFold = te.foldMap(fn)
      val viaTraverse: Const[B, Unit] = te
        .traverse[Const[B, *], Unit] { b =>
          Const[B, Unit](fn(b))
        }
        .void

      assert(viaFold == viaTraverse.getConst, s"${te.repr}")
    }

    forAll(genTypedExprChar, arbitrary[Char => Int])(law(_)(_))
    // non-commutative
    forAll(genTypedExprChar, arbitrary[Char => String])(law(_)(_))

    val lamconst: TypedExpr[String] =
      TypedExpr.AnnotatedLambda(
        Identifier.Name("x"),
        intTpe,
        int(1).as("a"),
        "b"
      )

    assert(lamconst.foldMap(identity) == "ab")
    assert(lamconst.traverse { a => Const[String, Unit](a) }.getConst == "ab")
  }

  test("TypedExpr.traverse.void matches traverse_") {
    import cats.data.Const
    forAll(genTypedExprInt, arbitrary[Int => String]) { (te, fn) =>
      assert(
        te.traverse { i => Const[String, Unit](fn(i)) }.void ==
          te.traverse_ { i => Const[String, Unit](fn(i)) }
      )
    }
  }

  test("TypedExpr.foldRight matches foldRight for commutative funs") {
    forAll(genTypedExprInt, Gen.choose(0, 1000)) { (te, init) =>
      val right =
        te.foldRight(cats.Eval.now(init)) { (i, ej) => ej.map(_ + i) }.value
      val left = te.foldLeft(init)(_ + _)
      assert(right == left)
    }
  }

  test("TypedExpr.foldRight matches foldRight for non-commutative funs") {
    forAll(genTypedExprInt) { te =>
      val right = te
        .foldRight(cats.Eval.now("")) { (i, ej) =>
          ej.map { j => i.toString + j }
        }
        .value
      val left = te.foldLeft("") { (i, j) => i + j.toString }
      assert(right == left)
    }
  }

  test("TypedExpr.map matches traverse with Id") {
    forAll(genTypedExprInt, arbitrary[Int => Int]) { (te, fn) =>
      assert(te.map(fn) == te.traverse[cats.Id, Int](fn))
    }
  }
}
