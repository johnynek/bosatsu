package dev.bosatsu

import cats.data.{NonEmptyList, State, Writer}
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scala.collection.immutable.{SortedMap, SortedSet}

import Arbitrary.arbitrary
import Identifier.{Bindable, Constructor}
import TestUtils.checkLast
import rankn.{Type, NTypeGen, RefSpace}

class TypedExprTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)

  /** Assert two bits of code normalize to the same thing
    */
  def normSame(s1: String, s2: String) =
    checkLast(s1) { t1 =>
      checkLast(s2) { t2 =>
        assertEquals(t1.void, t2.void)
      }
    }

  test("freeVarsSet is a subset of allVars") {
    def law[A](te: TypedExpr[A]) = {
      val frees = TypedExpr.freeVarsSet(te :: Nil).toSet
      val av = TypedExpr.allVarsSet(te :: Nil).toSet
      val missing = frees -- av
      assert(
        missing.isEmpty,
        s"expression:\n\n${te.repr}\n\nallVars: $av\n\nfrees: $frees"
      )
    }

    val allVarsProp = forAll(genTypedExpr)(law)

    checkLast("""
enum AB: A, B(x)
x = match B(100):
  case A: 10
  case B(b): b
""")(law)
    allVarsProp
  }

  test("allVars includes binders from lambdas, lets, and matches") {
    val tag = ()
    val x = Identifier.Name("x")
    val y = Identifier.Name("y")
    val z = Identifier.Name("z")
    val intT = Type.IntType

    val pat: Pattern[(PackageName, Constructor), Type] = Pattern.Var(y)
    val matchExpr =
      TypedExpr.Match(
        TypedExpr.Local(x, intT, tag),
        NonEmptyList(
          TypedExpr.Branch(pat, None, TypedExpr.Local(y, intT, tag)),
          Nil
        ),
        tag
      )
    val letExpr =
      TypedExpr.Let(
        z,
        TypedExpr.Literal(Lit.fromInt(1), intT, tag),
        matchExpr,
        RecursionKind.NonRecursive,
        tag
      )
    val te =
      TypedExpr.AnnotatedLambda(
        NonEmptyList((x, intT), Nil),
        letExpr,
        tag
      )

    val av = TypedExpr.allVarsSet(te :: Nil)
    assert(av.contains(x))
    assert(av.contains(y))
    assert(av.contains(z))

  }

  test("freeVars on top level TypedExpr is Nil") {
    // all of the names are local to a TypedExpr, nothing can be free
    // id and x are fully resolved to top level
    checkLast("""#
x = 1
def id(x): x
y = id(x)
""")(te => assertEquals(TypedExpr.freeVars(te :: Nil), Nil))

    checkLast("""#
x = 1
""")(te => assertEquals(TypedExpr.freeVars(te :: Nil), Nil))

    checkLast("""#
struct Tup2(a, b)

x = Tup2(1, 2)
""")(te => assertEquals(TypedExpr.freeVars(te :: Nil), Nil))

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(1, 2)
y = match x:
  case Tup2(a, _): a
""")(te => assertEquals(TypedExpr.freeVars(te :: Nil), Nil))
  }

  test("we can inline struct/destruct") {
    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(1, 2)
y = match x:
  case Tup2(a, _): a
""") {
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(Tup2(1, 3), 2)
y = match x:
  case Tup2(Tup2(a, _), _): a
""") {
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
struct Tup2(a, b)

x = 23
x = Tup2(0, Tup2(1, 2))
y = match x:
  case Tup2(_, Tup2(a, _)): a
""") {
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
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
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(1))
      case notLit => fail(s"expected Literal got: ${notLit.repr}")
    }

    checkLast("""#
x = 23
y = match x:
  case 42: 0
  case 23 as y: y
  case _: -1
""") {
      case TypedExpr.Literal(lit, _, _) => assertEquals(lit, Lit.fromInt(23))
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
    case L(_): z
    case R(r): r
""",
      """#
struct Tup2(a, b)
enum Eith: L(left), R(right)

def run(y, x):
  match x:
    case L(_): y
    case R(r): r
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
  val boolTpe = Type.BoolType

  def lit(l: Lit): TypedExpr[Unit] =
    TypedExpr.Literal(l, Type.getTypeOf(l), ())

  def int(i: Int): TypedExpr[Unit] =
    lit(Lit.fromInt(i))

  def bool(b: Boolean): TypedExpr[Unit] =
    TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Constructor(if (b) "True" else "False"),
      boolTpe,
      ()
    )

  def varTE(n: String, tpe: Type): TypedExpr[Unit] =
    TypedExpr.Local(Identifier.Name(n), tpe, ())

  val PredefAdd: TypedExpr[Unit] =
    TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("add"),
      Type.Fun(NonEmptyList.of(Type.IntType, Type.IntType), Type.IntType),
      ()
    )

  val PredefMul: TypedExpr[Unit] =
    TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("mul"),
      Type.Fun(NonEmptyList.of(Type.IntType, Type.IntType), Type.IntType),
      ()
    )

  val PredefSub: TypedExpr[Unit] =
    TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("sub"),
      Type.Fun(NonEmptyList.of(Type.IntType, Type.IntType), Type.IntType),
      ()
    )

  val PredefEqInt: TypedExpr[Unit] =
    TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("eq_Int"),
      Type.Fun(NonEmptyList.of(Type.IntType, Type.IntType), boolTpe),
      ()
    )

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
    TypedExpr.App(fn, NonEmptyList.one(arg), tpe, ())

  def lam(n: String, nt: Type, res: TypedExpr[Unit]): TypedExpr[Unit] =
    TypedExpr.AnnotatedLambda(
      NonEmptyList.one((Identifier.Name(n), nt)),
      res,
      ()
    )

  def branch(
      p: Pattern[(PackageName, Constructor), Type],
      e: TypedExpr[Unit]
  ): TypedExpr.Branch[Unit] =
    TypedExpr.Branch(p, None, e)

  def hasLoop(te: TypedExpr[Unit]): Boolean =
    te match {
      case TypedExpr.Loop(_, _, _) =>
        true
      case TypedExpr.Generic(_, in) =>
        hasLoop(in)
      case TypedExpr.Annotation(in, _, _) =>
        hasLoop(in)
      case TypedExpr.AnnotatedLambda(_, in, _) =>
        hasLoop(in)
      case TypedExpr.App(fn, args, _, _) =>
        hasLoop(fn) || args.exists(hasLoop)
      case TypedExpr.Let(_, expr, in, _, _) =>
        hasLoop(expr) || hasLoop(in)
      case TypedExpr.Recur(args, _, _) =>
        args.exists(hasLoop)
      case TypedExpr.Match(arg, branches, _) =>
        hasLoop(arg) || branches.exists { case TypedExpr.Branch(_, guard, b) =>
          guard.exists(hasLoop) || hasLoop(b)
        }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        false
    }

  def hasRecursiveLet(te: TypedExpr[Unit]): Boolean =
    te match {
      case TypedExpr.Let(_, expr, in, rec, _) =>
        rec.isRecursive || hasRecursiveLet(expr) || hasRecursiveLet(in)
      case TypedExpr.Generic(_, in) =>
        hasRecursiveLet(in)
      case TypedExpr.Annotation(in, _, _) =>
        hasRecursiveLet(in)
      case TypedExpr.AnnotatedLambda(_, in, _) =>
        hasRecursiveLet(in)
      case TypedExpr.App(fn, args, _, _) =>
        hasRecursiveLet(fn) || args.exists(hasRecursiveLet)
      case TypedExpr.Loop(args, body, _) =>
        args.exists { case (_, init) =>
          hasRecursiveLet(init)
        } || hasRecursiveLet(body)
      case TypedExpr.Recur(args, _, _) =>
        args.exists(hasRecursiveLet)
      case TypedExpr.Match(arg, branches, _) =>
        hasRecursiveLet(arg) || branches.exists {
          case TypedExpr.Branch(_, guard, b) =>
            guard.exists(hasRecursiveLet) || hasRecursiveLet(b)
        }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        false
    }

  def lowerAndNormalize(te: TypedExpr[Unit]): TypedExpr[Unit] = {
    val lowered = TypedExprLoopRecurLowering.lower(te).getOrElse(te)
    TypedExprNormalization.normalize(lowered).getOrElse(lowered)
  }

  def countExpr[A](te: TypedExpr[A], target: TypedExpr[?]): Int =
    te match {
      case t if t.void === target.void         => 1
      case TypedExpr.Generic(_, in)            => countExpr(in, target)
      case TypedExpr.Annotation(in, _, _)      => countExpr(in, target)
      case TypedExpr.AnnotatedLambda(_, in, _) => countExpr(in, target)
      case TypedExpr.App(fn, args, _, _)       =>
        countExpr(fn, target) + args.toList.map(countExpr(_, target)).sum
      case TypedExpr.Let(_, ex, in, _, _) =>
        countExpr(ex, target) + countExpr(in, target)
      case TypedExpr.Loop(args, body, _) =>
        args.toList.map { case (_, init) =>
          countExpr(init, target)
        }.sum + countExpr(
          body,
          target
        )
      case TypedExpr.Recur(args, _, _) =>
        args.toList.map(countExpr(_, target)).sum
      case TypedExpr.Match(arg, branches, _) =>
        countExpr(arg, target) + branches.toList.map {
          case TypedExpr.Branch(_, guard, b) =>
            guard.fold(0)(countExpr(_, target)) + countExpr(b, target)
        }.sum
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        0
    }

  test("test let substitution") {
    {
      // substitution in let
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assertEquals(
        TypedExpr.substitute(Identifier.Name("x"), int(2), let1),
        Some(let("y", int(2), varTE("y", intTpe)))
      )
    }

    {
      // substitution in let with a masking
      val let1 = let("y", varTE("x", intTpe), varTE("y", intTpe))
      assertEquals(
        TypedExpr
          .substitute(Identifier.Name("x"), varTE("y", intTpe), let1)
          .map(_.reprString),
        Some(
          "(let y0 (var y Bosatsu/Predef::Int) (var y0 Bosatsu/Predef::Int))"
        )
      )
    }

    {
      // substitution in let with a shadowing in result
      val let1 = let("y", varTE("y", intTpe), varTE("y", intTpe))
      assertEquals(
        TypedExpr.substitute(Identifier.Name("y"), int(42), let1),
        Some(let("y", int(42), varTE("y", intTpe)))
      )
    }

    {
      // substitution in letrec with a shadowing in bind and result
      val let1 = letrec("y", varTE("y", intTpe), varTE("y", intTpe))
      assertEquals(
        TypedExpr.substitute(Identifier.Name("y"), int(42), let1),
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
        case Nil    => ()
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

        case Some(te0sub) => assertEquals(te0sub, te0)
      }
    }
  }

  test("let x = y in x == y") {
    // inline lets of vars
    assertEquals(
      TypedExprNormalization.normalize(
        let("x", varTE("y", intTpe), varTE("x", intTpe))
      ),
      Some(varTE("y", intTpe))
    )
  }

  test("we can normalize addition") {
    val three = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), int(2)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(three), Some(int(3)))
  }

  test("we can normalize multiplication") {
    val six = TypedExpr.App(
      PredefMul,
      NonEmptyList.of(int(2), int(3)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(six), Some(int(6)))

    val zeroR = TypedExpr.App(
      PredefMul,
      NonEmptyList.of(varTE("x", intTpe), int(0)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(zeroR), Some(int(0)))

    val zeroL = TypedExpr.App(
      PredefMul,
      NonEmptyList.of(int(0), varTE("x", intTpe)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(zeroL), Some(int(0)))
  }

  test("we can normalize subtraction") {
    val minusOne = TypedExpr.App(
      PredefSub,
      NonEmptyList.of(int(1), int(2)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(minusOne), Some(int(-1)))

    val minusTwo = TypedExpr.App(
      PredefSub,
      NonEmptyList.of(int(0), int(2)),
      Type.IntType,
      ()
    )
    assertEquals(TypedExprNormalization.normalize(minusTwo), Some(int(-2)))
  }

  test("normalization rewrites opaque arithmetic with identity elements") {
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val opaque = app(varTE("f", fnType), int(1), intTpe)
    val expr =
      TypedExpr.App(PredefMul, NonEmptyList.of(opaque, int(1)), intTpe, ())

    val normalized = TypedExprNormalization.normalize(expr)
    assert(normalized.nonEmpty)
    val norm = normalized.get
    assertEquals(norm.void, opaque.void)
    assert(countExpr(norm, opaque) <= 1, norm.reprString)
  }

  test("normalization shares repeated immutable values in scope") {
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val opaque = app(varTE("f", fnType), int(1), intTpe)
    val expr =
      TypedExpr.App(PredefAdd, NonEmptyList.of(opaque, opaque), intTpe, ())

    val normalized = TypedExprNormalization.normalize(expr)
    assert(normalized.nonEmpty)
    val norm = normalized.get
    assert(countExpr(norm, opaque) <= 1, norm.reprString)
    norm match {
      case TypedExpr.Let(_, bound, _, RecursionKind.NonRecursive, _) =>
        assertEquals(bound.void, opaque.void)
      case other =>
        fail(s"expected shared let binding, got: ${other.reprString}")
    }
  }

  test(
    "normalization shares branch-independent immutable values across match branches"
  ) {
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val opaque = app(varTE("f", fnType), int(1), intTpe)
    val expr = TypedExpr.Match(
      varTE("x", intTpe),
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), opaque),
        branch(Pattern.WildCard, opaque)
      ),
      ()
    )

    val normalized = TypedExprNormalization.normalize(expr)
    assert(normalized.nonEmpty)
    val norm = normalized.get
    assert(countExpr(norm, opaque) <= 1, norm.reprString)
    norm match {
      case TypedExpr.Let(
            _,
            bound,
            TypedExpr.Match(_, _, _),
            RecursionKind.NonRecursive,
            _
          ) =>
        assertEquals(bound.void, opaque.void)
      case other =>
        fail(s"expected let hoisted around match, got: ${other.reprString}")
    }
  }

  test(
    "normalization does not introduce sharing lets for simple repeated values"
  ) {
    val x = varTE("x", intTpe)
    val expr = TypedExpr.App(PredefAdd, NonEmptyList.of(x, x), intTpe, ())
    assertEquals(TypedExprNormalization.normalize(expr), None)
  }

  test(
    "normalization does not hoist shared values above generic type binders"
  ) {
    val a = Type.Var.Bound("a")
    val aTy = Type.TyVar(a)
    val x = Identifier.Name("x")
    val polyId = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((x, aTy)),
      TypedExpr.Local(x, aTy, ()),
      ()
    )
    val polyFnTy = Type.Fun(
      NonEmptyList.of(
        Type.Fun(NonEmptyList.one(aTy), aTy),
        Type.Fun(NonEmptyList.one(aTy), aTy)
      ),
      boolTpe
    )
    val opaquePolyFn = TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("opaque_poly_fn"),
      polyFnTy,
      ()
    )
    val genericExpr = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      TypedExpr.App(opaquePolyFn, NonEmptyList.of(polyId, polyId), boolTpe, ())
    )

    val norm =
      TypedExprNormalization.normalize(genericExpr).getOrElse(genericExpr)
    norm match {
      case TypedExpr.Let(_, _, TypedExpr.Generic(_, _), _, _) =>
        fail(
          s"unexpected sharing let hoisted above Generic: ${norm.reprString}"
        )
      case _ =>
        assert(true)
    }
  }

  test(
    "normalization can hoist shared values above generic when type-independent"
  ) {
    val a = Type.Var.Bound("a")
    val monoFnTy = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val opaqueMonoFn = TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("opaque_mono_fn"),
      monoFnTy,
      ()
    )
    val mono = app(opaqueMonoFn, int(1), intTpe)
    val genericExpr = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      TypedExpr.App(PredefEqInt, NonEmptyList.of(mono, mono), boolTpe, ())
    )

    val normalized = TypedExprNormalization.normalize(genericExpr)
    assert(normalized.nonEmpty)
    val norm = normalized.get
    assert(countExpr(norm, mono) <= 1, norm.reprString)
    norm match {
      case TypedExpr.Let(_, bound, _, RecursionKind.NonRecursive, _) =>
        assertEquals(bound.void, mono.void)
      case other =>
        fail(
          s"expected shared let for generic-independent value, got: ${other.reprString}"
        )
    }
  }

  test(
    "normalization evaluates constructor matches with named, annotation, and union patterns"
  ) {
    type Pat = Pattern[(PackageName, Constructor), Type]

    def cons(name: String): TypedExpr[Unit] =
      TypedExpr.Global(
        PackageName.PredefName,
        Identifier.Constructor(name),
        Type.Fun(NonEmptyList.one(intTpe), intTpe),
        ()
      )

    def consPat(name: String, argPat: Pat): Pat =
      Pattern.PositionalStruct(
        (PackageName.PredefName, Identifier.Constructor(name)),
        argPat :: Nil
      )

    val vName = Identifier.Name("v")
    val wholeName = Identifier.Name("whole")
    val cExpr = TypedExpr.App(cons("C"), NonEmptyList.one(int(1)), intTpe, ())
    val cPat = consPat("C", Pattern.Var(vName))
    val dPat = consPat("D", Pattern.Var(vName))
    val unionPat = Pattern.union(cPat, dPat :: Nil)
    val namedPat: Pat =
      Pattern.Named(wholeName, Pattern.Annotation(unionPat, intTpe))

    val matchExpr = TypedExpr.Match(
      cExpr,
      NonEmptyList.of(
        branch(namedPat, TypedExpr.Local(vName, intTpe, ())),
        branch(Pattern.WildCard, int(0))
      ),
      ()
    )

    assertEquals(TypedExprNormalization.normalize(matchExpr), Some(int(1)))
  }

  test(
    "normalization prunes impossible constructor branches while keeping runtime matches"
  ) {
    type Pat = Pattern[(PackageName, Constructor), Type]

    def cons(name: String): TypedExpr[Unit] =
      TypedExpr.Global(
        PackageName.PredefName,
        Identifier.Constructor(name),
        Type.Fun(NonEmptyList.one(intTpe), intTpe),
        ()
      )

    def consPat(name: String, argPat: Pat): Pat =
      Pattern.PositionalStruct(
        (PackageName.PredefName, Identifier.Constructor(name)),
        argPat :: Nil
      )

    val cExpr = TypedExpr.App(cons("C"), NonEmptyList.one(int(1)), intTpe, ())
    val noMatchPat = consPat("D", Pattern.WildCard)
    val exactPat = consPat("C", Pattern.Literal(Lit.fromInt(1)))

    val matchExpr = TypedExpr.Match(
      cExpr,
      NonEmptyList.of(
        branch(noMatchPat, int(-1)),
        branch(exactPat, int(1)),
        branch(Pattern.WildCard, int(0))
      ),
      ()
    )

    TypedExprNormalization.normalize(matchExpr) match {
      case Some(TypedExpr.Match(_, branches, _)) =>
        assertEquals(branches.length, 2)
        assertEquals(branches.head.pattern, exactPat)
      case other =>
        fail(s"expected branch-pruned match, got: $other")
    }
  }

  test("normalization folds guard True and drops guard False branches") {
    val x = varTE("x", intTpe)

    val guardTrue = TypedExpr.Match(
      x,
      NonEmptyList.of(
        TypedExpr
          .Branch(Pattern.Literal(Lit.fromInt(1)), Some(bool(true)), int(1)),
        TypedExpr.Branch(Pattern.WildCard, None, int(0))
      ),
      ()
    )

    TypedExprNormalization.normalize(guardTrue) match {
      case Some(TypedExpr.Match(_, branches, _)) =>
        assertEquals(branches.head.guard, None)
      case other =>
        fail(s"expected normalized match, got: $other")
    }

    val guardFalse = TypedExpr.Match(
      x,
      NonEmptyList.of(
        TypedExpr
          .Branch(Pattern.Literal(Lit.fromInt(1)), Some(bool(false)), int(1)),
        TypedExpr.Branch(Pattern.WildCard, None, int(0))
      ),
      ()
    )

    assertEquals(TypedExprNormalization.normalize(guardFalse), Some(int(0)))
  }

  test(
    "normalization rewrites let substitutions in guard and branch body consistently"
  ) {
    val xName = Identifier.Name("x")
    val xExpr = TypedExpr.Local(xName, intTpe, ())
    val yExpr = varTE("y", intTpe)
    val guardExpr = TypedExpr.App(
      PredefEqInt,
      NonEmptyList.of(xExpr, int(1)),
      boolTpe,
      ()
    )
    val matchExpr = TypedExpr.Match(
      yExpr,
      NonEmptyList.of(
        TypedExpr.Branch(Pattern.WildCard, Some(guardExpr), xExpr),
        TypedExpr.Branch(Pattern.WildCard, None, int(0))
      ),
      ()
    )
    val root = let("x", int(1), matchExpr)

    val normalized = TypedExprNormalization.normalize(root).getOrElse(root)
    assertEquals(
      TypedExpr.freeVarsSet(normalized :: Nil).contains(xName),
      false
    )
    normalized match {
      case TypedExpr.Match(_, branches, _) =>
        assert(branches.head.guard.nonEmpty)
        assert(branches.head.guard.forall(_.notFree(xName)))
        assertEquals(branches.head.expr, int(1))
      case other =>
        fail(s"expected normalized match expression, got: $other")
    }
  }

  test(
    "normalization can lift non-simple lets outside lambdas when binders do not shadow args"
  ) {
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val unaryInt = Type.Fun(NonEmptyList.one(intTpe), intTpe)

    val boundExpr = app(varTE("opaque", unaryInt), int(1), intTpe)
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val body = TypedExpr.Let(
      yName,
      boundExpr,
      TypedExpr.App(PredefAdd, NonEmptyList.of(yVar, yVar), intTpe, ()),
      RecursionKind.NonRecursive,
      ()
    )
    val lamExpr =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ())

    TypedExprNormalization.normalize(lamExpr) match {
      case Some(norm) =>
        assert(countLet(norm) > 0, norm.reprString)
      case None =>
        fail("expected normalized result for non-simple lambda let body")
    }
  }

  test(
    "normalization keeps lambda-match shape when branch guards depend on lambda args"
  ) {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val unaryInt = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val opaqueApp = app(varTE("opaque", unaryInt), int(1), intTpe)
    val guardExpr = TypedExpr.App(
      PredefEqInt,
      NonEmptyList.of(xVar, int(0)),
      boolTpe,
      ()
    )
    val body = TypedExpr.Match(
      TypedExpr.Annotation(opaqueApp, intTpe, None),
      NonEmptyList.one(
        TypedExpr.Branch(Pattern.WildCard, Some(guardExpr), int(1))
      ),
      ()
    )
    val lamExpr =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ())

    TypedExprNormalization.normalize(lamExpr) match {
      case Some(TypedExpr.AnnotatedLambda(_, TypedExpr.Match(_, branches, _), _)) =>
        assertEquals(branches.length, 1)
        assert(branches.head.guard.nonEmpty)
      case other =>
        fail(s"expected lambda preserving guarded match, got: $other")
    }
  }

  test(
    "normalization leaves lambda guarded matches unchanged when lifting is blocked"
  ) {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val guardExpr = TypedExpr.App(
      PredefEqInt,
      NonEmptyList.of(xVar, int(0)),
      boolTpe,
      ()
    )
    val body = TypedExpr.Match(
      int(1),
      NonEmptyList.of(
        TypedExpr.Branch(Pattern.WildCard, Some(guardExpr), int(1)),
        TypedExpr.Branch(Pattern.WildCard, None, int(2))
      ),
      ()
    )
    val lamExpr =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ())

    assertEquals(TypedExprNormalization.normalize(lamExpr), None)
  }

  test("normalization can evaluate guarded constructor matches to constants") {
    val normalized = Par.withEC {
      var out: Option[TypedExpr[Unit]] = None
      TestUtils.testInferred(
        List("""
package Test

main = match Some(1):
  case Some(v) if v matches 0: 0
  case Some(v) if v matches 1: 7
  case Some(v): v
  case None: -1
"""),
        "Test",
        { (pm, mainPack) =>
          val pack = pm.toMap(mainPack)
          val mainExpr = pack.lets.find(_._1 == Identifier.Name("main")) match {
            case Some((_, _, te)) => te
            case None             =>
              fail(s"missing let main in ${pack.lets.map(_._1)}")
          }
          out = Some(
            TypedExprNormalization.normalize(mainExpr).getOrElse(mainExpr).void
          )
        }
      )
      out.getOrElse(fail("failed to infer normalized expression for main"))
    }

    normalized match {
      case TypedExpr.Literal(lit, _, _) =>
        assertEquals(lit, Lit.fromInt(7))
      case other =>
        fail(s"expected normalized literal, got: ${other.repr}")
    }
  }

  test("normalization can inline a tail-recursive function via Loop") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())

    val fBody = TypedExpr.Match(
      xVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, app(fVar, int(0), intTpe))
      ),
      ()
    )
    val fDef =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), fBody, ())
    val useOnce =
      lam("g", fnType, app(varTE("g", fnType), int(1), intTpe))
    val root = letrec("f", fDef, app(useOnce, fVar, intTpe))

    val norm = lowerAndNormalize(root)
    assert(hasLoop(norm), norm.reprString)
    assertEquals(hasRecursiveLet(norm), false)
    assertEquals(TypedExpr.allVarsSet(norm :: Nil).contains(fName), false)
  }

  test("normalization does not lower recursive lets to loop/recur") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())

    val fBody = TypedExpr.Match(
      xVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, app(fVar, int(0), intTpe))
      ),
      ()
    )
    val fDef =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), fBody, ())
    val root = letrec("f", fDef, fVar)

    val normalizedOnly = TypedExprNormalization.normalize(root).getOrElse(root)
    assertEquals(hasLoop(normalizedOnly), false, normalizedOnly.reprString)

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
  }

  test("loop/recur lowering keeps non-tail self-calls and lowers tail ones") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())

    val tailSelfCall = app(fVar, int(0), intTpe)
    val nonTailSelfCall = app(fVar, int(1), intTpe)
    val withNonTail = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), nonTailSelfCall),
      intTpe,
      ()
    )
    val fBody = TypedExpr.Match(
      xVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), tailSelfCall),
        branch(Pattern.WildCard, withNonTail)
      ),
      ()
    )
    val fDef =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), fBody, ())
    val root = letrec("f", fDef, fVar)

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)

    def hasRecur(te: TypedExpr[Unit]): Boolean =
      te match {
        case TypedExpr.Recur(_, _, _) =>
          true
        case TypedExpr.Generic(_, in) =>
          hasRecur(in)
        case TypedExpr.Annotation(in, _, _) =>
          hasRecur(in)
        case TypedExpr.AnnotatedLambda(_, in, _) =>
          hasRecur(in)
        case TypedExpr.App(fn, args, _, _) =>
          hasRecur(fn) || args.exists(hasRecur)
        case TypedExpr.Let(_, ex, in, _, _) =>
          hasRecur(ex) || hasRecur(in)
        case TypedExpr.Loop(args, body, _) =>
          args.exists { case (_, init) => hasRecur(init) } || hasRecur(body)
        case TypedExpr.Match(arg, branches, _) =>
          hasRecur(arg) || branches.exists { case TypedExpr.Branch(_, g, b) =>
            g.exists(hasRecur) || hasRecur(b)
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          false
      }

    def hasDirectSelfCall(te: TypedExpr[Unit]): Boolean =
      te match {
        case TypedExpr.App(TypedExpr.Local(`fName`, _, _), _, _, _) =>
          true
        case TypedExpr.Generic(_, in) =>
          hasDirectSelfCall(in)
        case TypedExpr.Annotation(in, _, _) =>
          hasDirectSelfCall(in)
        case TypedExpr.AnnotatedLambda(_, in, _) =>
          hasDirectSelfCall(in)
        case TypedExpr.App(fn, args, _, _) =>
          hasDirectSelfCall(fn) || args.exists(hasDirectSelfCall)
        case TypedExpr.Let(_, ex, in, _, _) =>
          hasDirectSelfCall(ex) || hasDirectSelfCall(in)
        case TypedExpr.Loop(args, body, _) =>
          args.exists { case (_, init) => hasDirectSelfCall(init) } ||
            hasDirectSelfCall(body)
        case TypedExpr.Recur(args, _, _) =>
          args.exists(hasDirectSelfCall)
        case TypedExpr.Match(arg, branches, _) =>
          hasDirectSelfCall(arg) || branches.exists {
            case TypedExpr.Branch(_, g, b) =>
              g.exists(hasDirectSelfCall) || hasDirectSelfCall(b)
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          false
      }

    assert(hasRecur(lowered), lowered.reprString)
    assert(hasDirectSelfCall(lowered), lowered.reprString)
    assert(hasRecursiveLet(lowered), lowered.reprString)
  }

  test("loop/recur lowering skips recursive lets without tail self-calls") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())

    val onlyNonTail = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), app(fVar, xVar, intTpe)),
      intTpe,
      ()
    )
    val fDef =
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((xName, intTpe)),
        onlyNonTail,
        ()
      )
    val root = letrec("f", fDef, fVar)

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering handles monomorphic recursive defs with wrappers") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val a = Type.Var.Bound("a")
    val aTy = Type.TyVar(a)
    val fnType = Type.Fun(NonEmptyList.one(aTy), aTy)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, aTy, ())

    val lam =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, aTy)), app(fVar, xVar, aTy), ())
    val ann = TypedExpr.Annotation(lam, fnType, None)
    val polyDef = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      ann
    )
    val root = TypedExpr.Let(
      fName,
      polyDef,
      TypedExpr.Literal(Lit.fromInt(0), intTpe, ()),
      RecursionKind.Recursive,
      ()
    )

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    TestUtils.assertValid(lowered)
  }

  test(
    "loop/recur lowering rewrites grouped recursive calls with changing prefix args"
  ) {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())

    def groupedCall(prefix: TypedExpr[Unit], inner: TypedExpr[Unit]) =
      TypedExpr.App(
        TypedExpr.App(fVar, NonEmptyList.one(prefix), innerFnType, ()),
        NonEmptyList.one(inner),
        intTpe,
        ()
      )

    val recurse = groupedCall(TypedExpr.App(PredefAdd, NonEmptyList.of(fnVar, int(1)), intTpe, ()), yVar)
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, recurse)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test("loop/recur lowering handles issue 1727 eta-expanded grouped calls") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val etaArgName = Identifier.Name("eta")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val etaArgVar = TypedExpr.Local(etaArgName, intTpe, ())

    val etaBodyCall = TypedExpr.App(
      TypedExpr.App(
        fVar,
        NonEmptyList.one(fnVar),
        innerFnType,
        ()
      ),
      NonEmptyList.one(etaArgVar),
      intTpe,
      ()
    )
    val etaCall = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((etaArgName, intTpe)),
        etaBodyCall,
        ()
      ),
      NonEmptyList.one(
        TypedExpr.App(
          PredefSub,
          NonEmptyList.of(yVar, int(1)),
          intTpe,
          ()
        )
      ),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, etaCall)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test("loop/recur lowering handles monomorphic grouped recursive defs") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val a = Type.Var.Bound("a")
    val aTy = Type.TyVar(a)

    val innerFnType = Type.Fun(NonEmptyList.one(aTy), aTy)
    val fType = Type.Fun(NonEmptyList.one(aTy), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, aTy, ())
    val yVar = TypedExpr.Local(yName, aTy, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      aTy,
      ()
    )

    val monoShape = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, aTy)),
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((yName, aTy)),
        groupedSelf,
        ()
      ),
      ()
    )
    val polyDef = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      TypedExpr.Annotation(monoShape, fType, None)
    )
    val root = TypedExpr.Let(
      fName,
      polyDef,
      TypedExpr.Literal(Lit.fromInt(0), intTpe, ()),
      RecursionKind.Recursive,
      ()
    )

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
    TestUtils.assertValid(lowered)
  }

  test("loop/recur lowering supports lets between grouped lambda args") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val tmpName = Identifier.Name("tmp")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())

    val recurse = TypedExpr.App(
      TypedExpr.App(
        fVar,
        NonEmptyList.one(fnVar),
        innerFnType,
        ()
      ),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val innerBody = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, recurse)
      ),
      ()
    )
    val innerLam =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), innerBody, ())
    val withLetBetween =
      TypedExpr.Let(tmpName, int(2), innerLam, RecursionKind.NonRecursive, ())
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      withLetBetween,
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test("loop/recur lowering skips grouped rewrite for non-tail grouped calls") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val nonTail = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), groupedSelf),
      intTpe,
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), nonTail, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering skips grouped rewrite for partial grouped calls") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val partialName = Identifier.Name("partial")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())

    val partialSelf =
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ())
    val innerBody =
      TypedExpr.Let(partialName, partialSelf, int(0), RecursionKind.NonRecursive, ())
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), innerBody, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering skips grouped rewrite when group binders collide") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(int(1)), innerFnType, ()),
      NonEmptyList.one(xVar),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      xVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, groupedSelf)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering skips grouped rewrite when loop/recur already appear") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val zName = Identifier.Name("z")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())

    val loopBody = TypedExpr.Loop(
      NonEmptyList.one((zName, int(0))),
      TypedExpr.Recur(NonEmptyList.one(int(1)), intTpe, ()),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), loopBody, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test(
    "loop/recur lowering supports grouped wrappers and wrapped grouped self heads"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val a = Type.Var.Bound("a")
    val q = TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type)))

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val wrappedHead = TypedExpr.Generic(q, TypedExpr.Annotation(fVar, fType, None))
    val groupedSelf = TypedExpr.App(
      TypedExpr.App(wrappedHead, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val innerBody = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, groupedSelf)
      ),
      ()
    )
    val innerLam =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), innerBody, ())
    val wrappedInner = TypedExpr.Generic(
      q,
      TypedExpr.Annotation(innerLam, innerFnType, None)
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      wrappedInner,
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test(
    "loop/recur lowering traverses grouped terminal wrappers, lambdas, and app fallbacks"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val lamArgName = Identifier.Name("z")
    val a = Type.Var.Bound("a")
    val q = TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type)))

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val lamArgVar = TypedExpr.Local(lamArgName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val genericSelf = TypedExpr.Generic(q, groupedSelf)
    val annotationSelf = TypedExpr.Annotation(groupedSelf, intTpe, None)

    val lambdaApp = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((lamArgName, intTpe)),
        lamArgVar,
        ()
      ),
      NonEmptyList.one(int(7)),
      intTpe,
      ()
    )
    val fallbackApp = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), int(2)),
      intTpe,
      ()
    )

    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), genericSelf),
        branch(Pattern.Literal(Lit.fromInt(1)), annotationSelf),
        branch(Pattern.Literal(Lit.fromInt(2)), lambdaApp),
        branch(Pattern.WildCard, fallbackApp)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test(
    "loop/recur lowering handles unchanged grouped generic and annotation branches"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val a = Type.Var.Bound("a")
    val q = TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type)))

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val unchangedGeneric = TypedExpr.Generic(q, int(0))
    val unchangedAnnotation = TypedExpr.Annotation(int(1), intTpe, None)
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), unchangedGeneric),
        branch(Pattern.Literal(Lit.fromInt(1)), unchangedAnnotation),
        branch(Pattern.WildCard, groupedSelf)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test(
    "loop/recur lowering traverses unchanged grouped matches inside let bindings"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val wName = Identifier.Name("w")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val unchangedMatch = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, int(1))
      ),
      ()
    )
    val body = TypedExpr.Let(
      wName,
      unchangedMatch,
      groupedSelf,
      RecursionKind.NonRecursive,
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test("loop/recur lowerAll traverses grouped recursive top-level definitions") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(xVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, groupedSelf)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )

    val lowered = TypedExprLoopRecurLowering.lowerAll(
      List((fName, RecursionKind.Recursive, expr))
    )
    assertEquals(lowered.length, 1)
    val loweredExpr = lowered.head._3
    assert(hasLoop(loweredExpr), loweredExpr.reprString)
    assertEquals(SelfCallKind(fName, loweredExpr), SelfCallKind.NoCall)
  }

  test("loop/recur lowering skips eta-expanded grouped calls with no inner groups") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val etaArgName = Identifier.Name("eta")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val etaCall = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((etaArgName, intTpe)),
        TypedExpr.Local(fName, intTpe, ()),
        ()
      ),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, etaCall)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test(
    "loop/recur lowering skips eta-expanded grouped calls with non-local final args"
  ) {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val etaArgName = Identifier.Name("eta")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val etaBody = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ()),
      NonEmptyList.one(int(1)),
      intTpe,
      ()
    )
    val etaCall = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((etaArgName, intTpe)),
        etaBody,
        ()
      ),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, etaCall)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test(
    "loop/recur lowering skips eta-expanded grouped calls when outer group arity mismatches"
  ) {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val etaArgName = Identifier.Name("eta")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val etaBodyCall = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ()),
      NonEmptyList.one(TypedExpr.Local(etaArgName, intTpe, ())),
      intTpe,
      ()
    )
    val etaCall = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((etaArgName, intTpe)),
        etaBodyCall,
        ()
      ),
      NonEmptyList.of(yVar, int(1)),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, etaCall)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering skips non-tail eta-expanded grouped calls") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val etaArgName = Identifier.Name("eta")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val etaArgVar = TypedExpr.Local(etaArgName, intTpe, ())

    val etaBodyCall = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ()),
      NonEmptyList.one(etaArgVar),
      intTpe,
      ()
    )
    val etaCall = TypedExpr.App(
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((etaArgName, intTpe)),
        etaBodyCall,
        ()
      ),
      NonEmptyList.one(
        TypedExpr.App(PredefSub, NonEmptyList.of(yVar, int(1)), intTpe, ())
      ),
      intTpe,
      ()
    )
    val nonTail = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(int(1), etaCall),
      intTpe,
      ()
    )
    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, nonTail)
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering handles grouped let branch shadowing combinations") {
    val fName = Identifier.Name("f")
    val fnArgName = Identifier.Name("fn")
    val yName = Identifier.Name("y")
    val wName = Identifier.Name("w")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val fnVar = TypedExpr.Local(fnArgName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val groupedSelf = TypedExpr.App(
      TypedExpr.App(fVar, NonEmptyList.one(fnVar), innerFnType, ()),
      NonEmptyList.one(yVar),
      intTpe,
      ()
    )

    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(
          Pattern.Literal(Lit.fromInt(0)),
          TypedExpr.Let(
            fName,
            int(1),
            int(2),
            RecursionKind.Recursive,
            ()
          )
        ),
        branch(
          Pattern.Literal(Lit.fromInt(1)),
          TypedExpr.Let(
            fName,
            int(3),
            int(4),
            RecursionKind.NonRecursive,
            ()
          )
        ),
        branch(
          Pattern.WildCard,
          TypedExpr.Let(
            wName,
            int(5),
            groupedSelf,
            RecursionKind.NonRecursive,
            ()
          )
        )
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((fnArgName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assertEquals(SelfCallKind(fName, lowered), SelfCallKind.NoCall)
  }

  test("loop/recur lowering skips grouped rewrite on bare grouped self references") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")

    val innerFnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fType = Type.Fun(NonEmptyList.one(intTpe), innerFnType)
    val fVar = TypedExpr.Local(fName, fType, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())

    val body = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), int(0)),
        branch(Pattern.WildCard, TypedExpr.Local(fName, intTpe, ()))
      ),
      ()
    )
    val expr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((xName, intTpe)),
      TypedExpr.AnnotatedLambda(NonEmptyList.one((yName, intTpe)), body, ()),
      ()
    )
    val root = TypedExpr.Let(fName, expr, fVar, RecursionKind.Recursive, ())

    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test(
    "loop/recur lowering handles shadowing, guards, and explicit loop/recur nodes"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val zName = Identifier.Name("z")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val zVar = TypedExpr.Local(zName, intTpe, ())
    val idVar = TypedExpr.Local(Identifier.Name("id"), fnType, ())
    val a = Type.Var.Bound("a")
    val q = TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type)))

    val tailSelf = TypedExpr.App(
      TypedExpr.Generic(q, fVar),
      NonEmptyList.one(xVar),
      intTpe,
      ()
    )

    val nestedUnchanged = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((zName, intTpe)),
      TypedExpr.App(idVar, NonEmptyList.one(zVar), intTpe, ()),
      ()
    )
    val nestedChanged = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((zName, intTpe)),
      TypedExpr.Generic(q, zVar),
      ()
    )

    val shadowRec = TypedExpr.Let(
      fName,
      int(1),
      int(2),
      RecursionKind.Recursive,
      ()
    )
    val shadowNonRec = TypedExpr.Let(
      fName,
      int(3),
      int(4),
      RecursionKind.NonRecursive,
      ()
    )
    val nonShadow = TypedExpr.Let(
      Identifier.Name("w"),
      int(2),
      int(3),
      RecursionKind.NonRecursive,
      ()
    )

    val loopUnchanged = TypedExpr.Loop(
      NonEmptyList.one((fName, int(1))),
      TypedExpr.Recur(NonEmptyList.one(int(2)), intTpe, ()),
      ()
    )
    val loopChanged = TypedExpr.Loop(
      NonEmptyList.one((Identifier.Name("acc"), TypedExpr.Generic(q, int(4)))),
      TypedExpr.Recur(
        NonEmptyList.one(TypedExpr.Generic(q, int(5))),
        intTpe,
        ()
      ),
      ()
    )

    val unchangedMatch = TypedExpr.Match(
      int(1),
      NonEmptyList.one(TypedExpr.Branch(Pattern.WildCard, None, int(1))),
      ()
    )
    val guardedMatch = TypedExpr.Match(
      int(0),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.WildCard,
          Some(bool(true)),
          tailSelf
        ),
        TypedExpr.Branch(Pattern.WildCard, None, nonShadow)
      ),
      ()
    )

    val body =
      TypedExpr.Let(
        Identifier.Name("a"),
        nestedUnchanged,
        TypedExpr.Let(
          Identifier.Name("b"),
          nestedChanged,
          TypedExpr.Let(
            Identifier.Name("c"),
            shadowRec,
            TypedExpr.Let(
              Identifier.Name("d"),
              shadowNonRec,
              TypedExpr.Let(
                Identifier.Name("e"),
                loopUnchanged,
                TypedExpr.Let(
                  Identifier.Name("g"),
                  loopChanged,
                  TypedExpr.Let(
                    Identifier.Name("h"),
                    unchangedMatch,
                    guardedMatch,
                    RecursionKind.NonRecursive,
                    ()
                  ),
                  RecursionKind.NonRecursive,
                  ()
                ),
                RecursionKind.NonRecursive,
                ()
              ),
              RecursionKind.NonRecursive,
              ()
            ),
            RecursionKind.NonRecursive,
            ()
          ),
          RecursionKind.NonRecursive,
          ()
        ),
        RecursionKind.NonRecursive,
        ()
      )

    val fDef = TypedExpr.Generic(
      q,
      TypedExpr.Annotation(
        TypedExpr.AnnotatedLambda(
          NonEmptyList.one((xName, intTpe)),
          body,
          ()
        ),
        fnType,
        None
      )
    )
    val root = TypedExpr.Let(fName, fDef, fVar, RecursionKind.Recursive, ())

    def hasRecur(te: TypedExpr[Unit]): Boolean =
      te match {
        case TypedExpr.Recur(_, _, _) =>
          true
        case TypedExpr.Generic(_, in) =>
          hasRecur(in)
        case TypedExpr.Annotation(in, _, _) =>
          hasRecur(in)
        case TypedExpr.AnnotatedLambda(_, in, _) =>
          hasRecur(in)
        case TypedExpr.App(fn, args, _, _) =>
          hasRecur(fn) || args.exists(hasRecur)
        case TypedExpr.Let(_, ex, in, _, _) =>
          hasRecur(ex) || hasRecur(in)
        case TypedExpr.Loop(args, in, _) =>
          args.exists { case (_, init) => hasRecur(init) } || hasRecur(in)
        case TypedExpr.Match(arg, branches, _) =>
          hasRecur(arg) || branches.exists { case TypedExpr.Branch(_, g, b) =>
            g.exists(hasRecur) || hasRecur(b)
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          false
      }

    val lowered = TypedExprLoopRecurLowering.lower(root).getOrElse(root)
    assert(hasLoop(lowered), lowered.reprString)
    assert(hasRecur(lowered), lowered.reprString)
  }

  test("loop/recur lowering skips recursive non-lambda bindings") {
    val root = TypedExpr.Let(
      Identifier.Name("f"),
      int(1),
      int(2),
      RecursionKind.Recursive,
      ()
    )
    assertEquals(TypedExprLoopRecurLowering.lower(root), None)
  }

  test("loop/recur lowering rewrites changed shadow lets and guards") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnType, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val a = Type.Var.Bound("a")
    val q = TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type)))

    val shadowRecChanged = TypedExpr.Let(
      fName,
      TypedExpr.Generic(q, int(1)),
      int(2),
      RecursionKind.Recursive,
      ()
    )
    val shadowNonRecChanged = TypedExpr.Let(
      fName,
      TypedExpr.Generic(q, int(3)),
      int(4),
      RecursionKind.NonRecursive,
      ()
    )
    val guarded = TypedExpr.Match(
      int(0),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.WildCard,
          Some(TypedExpr.Generic(q, bool(true))),
          app(fVar, xVar, intTpe)
        ),
        TypedExpr.Branch(Pattern.WildCard, None, int(0))
      ),
      ()
    )
    val body = TypedExpr.Let(
      Identifier.Name("a"),
      shadowRecChanged,
      TypedExpr.Let(
        Identifier.Name("b"),
        shadowNonRecChanged,
        guarded,
        RecursionKind.NonRecursive,
        ()
      ),
      RecursionKind.NonRecursive,
      ()
    )
    val root = TypedExpr.Let(
      fName,
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ()),
      fVar,
      RecursionKind.Recursive,
      ()
    )

    val lowered = TypedExprLoopRecurLowering.lower(root)
    assert(lowered.nonEmpty)
    assert(hasLoop(lowered.getOrElse(root)), lowered.get.reprString)
  }

  test("loop/recur lowering traverses app/loop/recur containers") {
    val hName = Identifier.Name("h")
    val uName = Identifier.Name("u")
    val hType = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val hVar = TypedExpr.Local(hName, hType, ())
    val uVar = TypedExpr.Local(uName, intTpe, ())
    val hDef =
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((uName, intTpe)),
        app(hVar, uVar, intTpe),
        ()
      )
    val innerRec = TypedExpr.Let(hName, hDef, hVar, RecursionKind.Recursive, ())

    val useTy = Type.Fun(NonEmptyList.one(hType), intTpe)
    val appContainer = TypedExpr.App(
      TypedExpr.Local(Identifier.Name("use"), useTy, ()),
      NonEmptyList.one(innerRec),
      intTpe,
      ()
    )
    assert(TypedExprLoopRecurLowering.lower(appContainer).nonEmpty)

    val unchangedLoop = TypedExpr.Loop(
      NonEmptyList.one((Identifier.Name("x0"), int(1))),
      int(2),
      ()
    )
    assertEquals(TypedExprLoopRecurLowering.lower(unchangedLoop), None)
    val changedLoop = TypedExpr.Loop(
      NonEmptyList.one((Identifier.Name("x1"), innerRec)),
      int(0),
      ()
    )
    assert(TypedExprLoopRecurLowering.lower(changedLoop).nonEmpty)

    val unchangedRecur =
      TypedExpr.Recur(NonEmptyList.one(int(1)), intTpe, ())
    assertEquals(TypedExprLoopRecurLowering.lower(unchangedRecur), None)
    val changedRecur =
      TypedExpr.Recur(NonEmptyList.one(innerRec), intTpe, ())
    assert(TypedExprLoopRecurLowering.lower(changedRecur).nonEmpty)
  }

  test("lowerAll lowers top-level monomorphic generic len recursion") {
    TestUtils.checkPackageMap("""
enum List[a]: EmptyList, NonEmptyList(head: a, tail: List[a])
enum Nat: Z, S(prev: Nat)

def inc(n: Nat) -> Nat: S(n)

def len[a](list: List[a], acc: Nat) -> Nat:
  recur list:
    case EmptyList: acc
    case NonEmptyList(_, tail): len(tail, inc(acc))
    """) { pm =>
      val pack = pm.toMap(TestUtils.testPackage)
      val loweredLets = TypedExprLoopRecurLowering.lowerAll(
        pack.lets.map { case (n, rec, te) => (n, rec, te.void) }
      )
      val (name, _, te) =
        loweredLets.find(_._1 == Identifier.Name("len")).get
      assertEquals(name, Identifier.Name("len"))
      assert(hasLoop(te), te.reprString)
      TestUtils.assertValid(te)
    }
  }

  test("lowerAll skips top-level type-changing polymorphic recursion") {
    TestUtils.checkPackageMap("""
enum Nat: Z, S(prev: Nat)
enum Box[a]: Box(value: a)

def poly[a](n: Nat, x: a) -> Nat:
  recur n:
    case Z: Z
    case S(prev): poly(prev, Box(x))
    """) { pm =>
      val pack = pm.toMap(TestUtils.testPackage)
      val loweredLets = TypedExprLoopRecurLowering.lowerAll(
        pack.lets.map { case (n, rec, te) => (n, rec, te.void) }
      )
      val (name, _, te) =
        loweredLets.find(_._1 == Identifier.Name("poly")).get
      assertEquals(name, Identifier.Name("poly"))
      assertEquals(hasLoop(te), false, te.reprString)
      TestUtils.assertValid(te)
    }
  }

  test("normalization removes Loop when Recur is normalized away") {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val loopBody = TypedExpr.Match(
      int(0),
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), xVar),
        branch(
          Pattern.WildCard,
          TypedExpr.Recur(NonEmptyList.one(xVar), intTpe, ())
        )
      ),
      ()
    )
    val loopExpr =
      TypedExpr.Loop(NonEmptyList.one((xName, int(1))), loopBody, ())
    assertEquals(TypedExprNormalization.normalize(loopExpr), Some(int(1)))
  }

  test("normalization lifts invariant loop arguments into let bindings") {
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val yPlusOne = TypedExpr.App(
      PredefAdd,
      NonEmptyList.of(yVar, int(1)),
      intTpe,
      ()
    )
    val loopBody = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), xVar),
        branch(
          Pattern.WildCard,
          TypedExpr.Recur(NonEmptyList.of(xVar, yPlusOne), intTpe, ())
        )
      ),
      ()
    )
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.of((xName, int(10)), (yName, int(1))),
      loopBody,
      ()
    )

    val normalized = TypedExprNormalization.normalize(loopExpr)
    val maybeLoop = normalized match {
      case Some(TypedExpr.Loop(loopArgs, normBody, _)) =>
        Some((loopArgs, normBody))
      case Some(
            TypedExpr.Let(
              `xName`,
              _,
              TypedExpr.Loop(loopArgs, normBody, _),
              RecursionKind.NonRecursive,
              _
            )
          ) =>
        Some((loopArgs, normBody))
      case _ =>
        None
    }

    maybeLoop match {
      case Some((loopArgs, normBody)) =>
        assertEquals(loopArgs.toList.map(_._1), List(yName))
        normBody match {
          case TypedExpr.Match(_, branches, _) =>
            val recurArgs = branches.toList.collect {
              case TypedExpr.Branch(_, _, TypedExpr.Recur(args, _, _)) => args
            }
            assertEquals(recurArgs.map(_.length), List(1))
            assert(!recurArgs.exists(_.exists(_.freeVarsDup.contains(xName))))
          case other =>
            fail(s"expected normalized loop body match, got: ${other.repr}")
        }
      case None =>
        fail(s"expected invariant lift into single-arg loop, got: $normalized")
    }
  }

  test("normalization keeps final loop argument even when invariant") {
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val yVar = TypedExpr.Local(yName, intTpe, ())
    val loopBody = TypedExpr.Match(
      yVar,
      NonEmptyList.of(
        branch(Pattern.Literal(Lit.fromInt(0)), xVar),
        branch(
          Pattern.WildCard,
          TypedExpr.Recur(NonEmptyList.of(xVar, yVar), intTpe, ())
        )
      ),
      ()
    )
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.of((xName, int(10)), (yName, int(1))),
      loopBody,
      ()
    )

    val normalized = TypedExprNormalization.normalize(loopExpr)
    val maybeLoop = normalized match {
      case Some(TypedExpr.Loop(loopArgs, normBody, _)) =>
        Some((loopArgs, normBody))
      case Some(
            TypedExpr.Let(
              `xName`,
              _,
              TypedExpr.Loop(loopArgs, normBody, _),
              RecursionKind.NonRecursive,
              _
            )
          ) =>
        Some((loopArgs, normBody))
      case _ =>
        None
    }

    maybeLoop match {
      case Some((loopArgs, normBody)) =>
        // Even when every recur argument is invariant, we retain one slot.
        assertEquals(loopArgs.toList.map(_._1), List(yName))
        normBody match {
          case TypedExpr.Match(_, branches, _) =>
            val recurArities = branches.toList.collect {
              case TypedExpr.Branch(_, _, TypedExpr.Recur(args, _, _)) =>
                args.length
            }
            assertEquals(recurArities, List(1))
          case other =>
            fail(s"expected normalized loop body match, got: ${other.repr}")
        }
      case None =>
        fail(
          s"expected lifted invariant with retained final loop arg, got: $normalized"
        )
    }
  }

  test("normalization removes non-recursive identity let bindings") {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val wrappedX = TypedExpr.Annotation(xVar, intTpe, None)
    val body =
      TypedExpr.App(PredefAdd, NonEmptyList.of(xVar, int(1)), intTpe, ())
    val identLet =
      TypedExpr.Let(xName, wrappedX, body, RecursionKind.NonRecursive, ())

    assertEquals(
      TypedExprNormalization.normalize(identLet),
      TypedExprNormalization.normalize(body)
    )
  }

  test("normalization removes direct non-recursive identity let bindings") {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val body =
      TypedExpr.App(PredefAdd, NonEmptyList.of(xVar, int(1)), intTpe, ())
    val identLet =
      TypedExpr.Let(xName, xVar, body, RecursionKind.NonRecursive, ())

    assertEquals(
      TypedExprNormalization.normalize(identLet),
      TypedExprNormalization.normalize(body)
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

  test("we can inline using a shadow: let x = y in let y = z(43) in x(y)(y)") {
    // we can inline a shadow by unshadowing y to be y1
    // x = y
    // y = z(43)
    // x(y, y)
    val normed = TypedExprNormalization.normalize(normalLet)
    assertEquals(
      normed.map(_.repr.render(80)),
      Some("""(let
    y0
    (ap
        (var z Bosatsu/Predef::Int)
        (lit 43 Bosatsu/Predef::Int)
        Bosatsu/Predef::Int)
    (ap
        (ap
            (var y Bosatsu/Predef::Int)
            (var y0 Bosatsu/Predef::Int)
            Bosatsu/Predef::Int)
        (var y0 Bosatsu/Predef::Int)
        Bosatsu/Predef::Int))""")
    )
  }

  test("if w doesn't have x free: (app (let x y z) w) == let x y (app z w)") {
    assertEquals(
      TypedExprNormalization
        .normalize(
          app(normalLet, varTE("w", intTpe), intTpe)
        )
        .map(_.reprString),
      Some(
        let(
          "y0",
          app(varTE("z", intTpe), int(43), intTpe),
          app(
            app(
              app(varTE("y", intTpe), varTE("y0", intTpe), intTpe),
              varTE("y0", intTpe),
              intTpe
            ),
            varTE("w", intTpe),
            intTpe
          )
        ).reprString
      )
    )

  }

  test("x -> f(x) == f") {
    val f = varTE("f", Type.Fun(intTpe, intTpe))
    val left = lam("x", intTpe, app(f, varTE("x", intTpe), intTpe))

    assertEquals(TypedExprNormalization.normalize(left), Some(f))

    checkLast("""
struct Foo(a)
x = (y) -> Foo(y)
g = a -> x(a)
    """) { te1 =>
      checkLast("""
struct Foo(a)
x = Foo
      """) { te2 =>
        assertEquals(te1.void, te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }

    checkLast("""
struct Foo(a)
x = Foo
    """) {
      case TypedExpr.Global(_, Identifier.Constructor("Foo"), _, _) =>
        assert(true)
      case notNorm =>
        fail(notNorm.repr.render(80))
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

    assertEquals(res, Some(right), s"${res.map(_.repr)} != Some(${right.repr}")

    checkLast("""
res = (
  f = (_, y) -> y
  z = 1
  y -> (x -> f(x, z))(y)
)
""") { te1 =>
      checkLast("""
res = _ -> 1
      """) { te2 =>
        assertEquals(
          te1.void,
          te2.void,
          s"${te1.repr.render(80)} != ${te2.repr.render(80)}"
        )
      }
    }

    checkLast("""
f = (_, y) -> y
z = 1
res = y -> (x -> f(x, z))(y)
""") { te1 =>
      checkLast("""
res = _ -> 1
      """) { te2 =>
        assertEquals(te1.void, te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }
  }

  test("let de-nesting") {
    checkLast("""
struct Tup(a, b)
def f(x): x
res = (
   x = (
    y = f(1)
    Tup(y, y)
   ) 
   Tup(x, x)
)
""") { te1 =>
      checkLast("""
struct Tup(a, b)
def f(x): x
res = (
   y = f(1)
   x = Tup(y, y)
   Tup(x, x)
)
      """) { te2 =>
        assertEquals(te1.void, te2.void, s"${te1.repr} != ${te2.repr}")
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
        assertEquals(te1.void, te2.void, s"${te1.repr} != ${te2.repr}")
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
        assertEquals(te1.void, te2.void, s"${te1.repr} != ${te2.repr}")
      }
    }
  }

  test("coerceFn does not rewrite pattern-bound local types") {
    val xName = Identifier.Name("x")
    val yName = Identifier.Name("y")
    val yLocal = TypedExpr.Local(yName, intTpe, ())
    val body = TypedExpr.Match(
      yLocal,
      NonEmptyList.one(
        branch(
          Pattern.Var(xName): Pattern[(PackageName, Constructor), Type],
          TypedExpr.Local(xName, intTpe, ())
        )
      ),
      ()
    )
    val lam =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ())
    val coerced =
      TypedExpr.coerceFn(
        NonEmptyList.one(Type.StrType),
        Type.IntType,
        TypedExpr.coerceRho(Type.IntType, _ => None),
        _ => None
      )(lam)

    def localTypesOf(te: TypedExpr[Unit], target: Bindable): List[Type] =
      te match {
        case TypedExpr.Local(n, tpe, _) if n == target =>
          tpe :: Nil
        case TypedExpr.Generic(_, in) =>
          localTypesOf(in, target)
        case TypedExpr.Annotation(in, _, _) =>
          localTypesOf(in, target)
        case TypedExpr.AnnotatedLambda(_, in, _) =>
          localTypesOf(in, target)
        case TypedExpr.App(fn, args, _, _) =>
          localTypesOf(fn, target) ::: args.toList.flatMap(
            localTypesOf(_, target)
          )
        case TypedExpr.Let(_, ex, in, _, _) =>
          localTypesOf(ex, target) ::: localTypesOf(in, target)
        case TypedExpr.Loop(args, in, _) =>
          args.toList.flatMap { case (_, init) =>
            localTypesOf(init, target)
          } ::: localTypesOf(
            in,
            target
          )
        case TypedExpr.Recur(args, _, _) =>
          args.toList.flatMap(localTypesOf(_, target))
        case TypedExpr.Match(arg, branches, _) =>
          localTypesOf(arg, target) ::: branches.toList.flatMap {
            case TypedExpr.Branch(_, guard, in) =>
              guard.fold(List.empty[Type])(
                localTypesOf(_, target)
              ) ::: localTypesOf(
                in,
                target
              )
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          Nil
      }

    val xTypes = localTypesOf(coerced, xName).toSet
    assert(xTypes.nonEmpty)
    assertEquals(xTypes, Set[Type](intTpe))
  }

  def gen[A](g: Gen[A]): Gen[TypedExpr[A]] =
    Generators.genTypedExpr(g, 3, NTypeGen.genDepth03)

  val genTypedExpr: Gen[TypedExpr[Unit]] =
    gen(Gen.const(()))

  val genTypedExprInt: Gen[TypedExpr[Int]] =
    gen(Gen.choose(Int.MinValue, Int.MaxValue))

  val genTypedExprChar: Gen[TypedExpr[Char]] =
    gen(Gen.choose('a', 'z'))

  private def allTypesViaTraverseType[A](te: TypedExpr[A]): SortedSet[Type] =
    te.traverseType(t => Writer[SortedSet[Type], Type](SortedSet(t), t)).run._1

  private def allBoundViaTraverseType[A](
      te: TypedExpr[A]
  ): SortedSet[Type.Var.Bound] = {
    val tpes = allTypesViaTraverseType(te).toList
    val free = Type.freeBoundTyVars(tpes).toSet
    SortedSet.from(Type.tyVarBinders(tpes) ++ free)
  }

  private def allPatternTypesViaTraverseType[N](
      p: Pattern[N, Type]
  ): SortedSet[Type] =
    p.traverseType(t => Writer[SortedSet[Type], Type](SortedSet(t), t)).run._1

  private def freeTyVarsReference[A](te: TypedExpr[A]): List[Type.Var] = {
    def qevTypes(qev: TypedExpr.QuantifierEvidence): List[Type] =
      qev.sourceAtSolve ::
        qev.targetAtSolve ::
        qev.forallSolved.valuesIterator.map(_._2).toList :::
        qev.existsHidden.valuesIterator.map(_._2).toList

    def loop(self: TypedExpr[A]): Set[Type.Var] =
      self match {
        case TypedExpr.Generic(quant, expr) =>
          loop(expr) -- quant.vars.iterator.map(_._1)
        case TypedExpr.Annotation(of, tpe, qev) =>
          loop(of) ++ Type.freeTyVars(tpe :: qev.toList.flatMap(qevTypes))
        case TypedExpr.AnnotatedLambda(args, res, _) =>
          loop(res) ++ Type.freeTyVars(args.toList.map { case (_, t) => t })
        case TypedExpr.Local(_, tpe, _) =>
          Type.freeTyVars(tpe :: Nil).toSet
        case TypedExpr.Global(_, _, tpe, _) =>
          Type.freeTyVars(tpe :: Nil).toSet
        case TypedExpr.App(f, args, tpe, _) =>
          args.foldLeft(loop(f))(_ | loop(_)) ++ Type.freeTyVars(tpe :: Nil)
        case TypedExpr.Let(_, exp, in, _, _) =>
          loop(exp) | loop(in)
        case TypedExpr.Loop(args, body, _) =>
          args.foldLeft(loop(body)) { case (acc, (_, expr)) =>
            acc | loop(expr)
          }
        case TypedExpr.Recur(args, tpe, _) =>
          args.foldLeft(Type.freeTyVars(tpe :: Nil).toSet)(_ | loop(_))
        case TypedExpr.Literal(_, tpe, _) =>
          Type.freeTyVars(tpe :: Nil).toSet
        case TypedExpr.Match(expr, branches, _) =>
          branches.foldLeft(loop(expr)) { case (acc, branch) =>
            val acc1 = (acc | loop(branch.expr)) | branch.guard.fold(
              Set.empty[Type.Var]
            )(loop)
            acc1 ++ allPatternTypesViaTraverseType(branch.pattern).iterator.collect {
              case Type.TyVar(v) => v
            }
          }
      }

    loop(te).toList.sorted
  }

  test("TypedExpr.substituteTypeVar of identity is identity") {
    forAll(genTypedExpr, Gen.listOf(NTypeGen.genBound)) { (te, bounds) =>
      val identMap: Map[Type.Var, Type] = bounds.map { b =>
        (b, Type.TyVar(b))
      }.toMap
      assertEquals(TypedExpr.substituteTypeVar(te, identMap), te)
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
      assertEquals(te2, te1)
    }
  }

  test(
    "TypedExpr.substituteTypeVar can be identity for shadowed generic binders"
  ) {
    val a = Type.Var.Bound("a")
    val b = Type.Var.Bound("b")
    val te: TypedExpr[Unit] =
      TypedExpr.Generic(
        TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
        TypedExpr.Literal(Lit.fromInt(1), Type.IntType, ())
      )

    val tpes: Set[Type.Var] = te.allTypes.iterator.collect {
      case Type.TyVar(v) => v
    }.toSet

    // This mirrors the old property logic: it can miss Generic binders because
    // te.getType is normalized for vacuous quantifiers.
    val boundsFromQuantifiedTypes: Set[Type.Var] = te
      .traverseType { (t: Type) =>
        Writer(SortedSet[Type.Var](Type.quantVars(t).map(_._1)*), t)
      }
      .run
      ._1
      .toSet[Type.Var]

    val identMap: Map[Type.Var, Type] =
      tpes
        .filterNot(boundsFromQuantifiedTypes)
        .iterator
        .zip((b :: Nil).iterator)
        .map { case (v, rep) => (v, Type.TyVar(rep)) }
        .toMap

    assertEquals(identMap, Map[Type.Var, Type](a -> Type.TyVar(b)))
    assertEquals(TypedExpr.substituteTypeVar(te, identMap), te)
  }

  test("TypedExpr.substituteTypeVar is not an identity function") {
    // if we replace all the current types with some bound types, things won't be the same
    forAll(genTypedExpr) { te =>
      val tpes: Set[Type.Var] = te.allTypes.iterator.collect {
        case Type.TyVar(b) => b
      }.toSet

      // Type vars bound by Generic/type quantification should not be chosen as
      // replacement candidates since substitution is shadowed under binders.
      val bounds: Set[Type.Var] = te.allBound.toSet[Type.Var]

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
        assert(te1 =!= te, s"mapping: $identMap, $bounds")
      }
    }
  }

  test("TypedExpr.allTypes contains the type") {
    forAll(genTypedExpr) { te =>
      assert(te.allTypes.contains(te.getType))
    }
  }

  test("TypedExpr.allTypes matches traverseType oracle") {
    forAll(genTypedExpr) { te =>
      assertEquals(te.allTypes, allTypesViaTraverseType(te))
    }
  }

  test("TypedExpr.allBound matches traverseType oracle") {
    forAll(genTypedExpr) { te =>
      assertEquals(te.allBound, allBoundViaTraverseType(te))
    }
  }

  test("TypedExpr.freeTyVars matches reference oracle") {
    forAll(genTypedExpr) { te =>
      assertEquals(te.freeTyVars, freeTyVarsReference(te))
    }
  }

  test("TypedExpr.Quantification.fromLists and concat cover all constructors") {
    val a = (Type.Var.Bound("a"), Kind.Type)
    val b = (Type.Var.Bound("b"), Kind.Type)
    val c = (Type.Var.Bound("c"), Kind.Type)

    val fa = TypedExpr.Quantification.fromLists(List(a), Nil)
    val ex = TypedExpr.Quantification.fromLists(Nil, List(b))
    val du = TypedExpr.Quantification.fromLists(List(a), List(b))

    assertEquals(
      fa,
      Some(TypedExpr.Quantification.ForAll(NonEmptyList.one(a)))
    )
    assertEquals(
      ex,
      Some(TypedExpr.Quantification.Exists(NonEmptyList.one(b)))
    )
    assertEquals(
      du,
      Some(
        TypedExpr.Quantification.Dual(
          NonEmptyList.one(a),
          NonEmptyList.one(b)
        )
      )
    )

    val faq = TypedExpr.Quantification.ForAll(NonEmptyList.one(a))
    val exq = TypedExpr.Quantification.Exists(NonEmptyList.one(b))
    val duq =
      TypedExpr.Quantification.Dual(NonEmptyList.one(a), NonEmptyList.one(c))

    assertEquals(
      faq.concat(exq),
      TypedExpr.Quantification.Dual(NonEmptyList.one(a), NonEmptyList.one(b))
    )
    assertEquals(
      exq.concat(faq),
      TypedExpr.Quantification.Dual(NonEmptyList.one(a), NonEmptyList.one(b))
    )
    assertEquals(
      duq.concat(exq),
      TypedExpr.Quantification.Dual(
        NonEmptyList.one(a),
        NonEmptyList.of(c, b)
      )
    )
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
  def countLambdaCapturing[A](te: TypedExpr[A], name: Bindable): Int =
    count(te) { case lam @ TypedExpr.AnnotatedLambda(_, _, _) =>
      TypedExpr.freeVars(lam :: Nil).contains(name)
    }

  def inferUnoptimizedAndNormalizedExpr(
      statement: String,
      letName: String
  ): (TypedExpr[Declaration], TypedExpr[Declaration]) = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    val (fullTypeEnv, unoptProgram) =
      Package.inferBodyUnopt(TestUtils.testPackage, Nil, stmts) match {
        case cats.data.Ior.Right(res) =>
          res
        case cats.data.Ior.Both(errs, _) =>
          fail(s"inference failure:\n${errs.toList.mkString("\n")}")
        case cats.data.Ior.Left(errs) =>
          fail(s"inference failure:\n${errs.toList.mkString("\n")}")
      }

    val targetName = Identifier.Name(letName)
    val unoptimizedExpr = unoptProgram.lets.find(_._1 == targetName) match {
      case Some((_, _, te)) => te
      case None             =>
        fail(s"missing let: $letName in ${unoptProgram.lets.map(_._1)}")
    }
    val loweredLets = TypedExprLoopRecurLowering.lowerAll(unoptProgram.lets)
    val normalizedLets =
      TypedExprNormalization.normalizeAll(
        TestUtils.testPackage,
        loweredLets,
        fullTypeEnv
      )
    val normalizedExpr = normalizedLets.find(_._1 == targetName) match {
      case Some((_, _, te)) => te
      case None             =>
        fail(s"missing normalized let: $letName in ${normalizedLets.map(_._1)}")
    }
    (unoptimizedExpr, normalizedExpr)
  }

  test("if matches normalizes to same code as equivalent match") {
    def normalizedFromPackage(packSrc: String): TypedExpr[Unit] =
      Par.withEC {
        var out: Option[TypedExpr[Unit]] = None
        TestUtils.testInferred(
          List(packSrc),
          "Test",
          { (pm, mainPack) =>
            val pack = pm.toMap(mainPack)
            val fExpr = pack.lets.find(_._1 == Identifier.Name("f")) match {
              case Some((_, _, te)) => te
              case None => fail(s"missing let f in ${pack.lets.map(_._1)}")
            }
            val normalized =
              TypedExprNormalization.normalize(fExpr).getOrElse(fExpr).void
            out = Some(normalized)
          }
        )
        out.getOrElse(fail("failed to infer normalized expression for f"))
      }

    val ifNormalized = normalizedFromPackage(
      """
package Test

enum E: Left(l), Right(r)

def f(x):
  if x matches Left(_): 1
  else: -1
"""
    )

    val matchNormalized = normalizedFromPackage(
      """
package Test

enum E: Left(l), Right(r)

def f(x):
  match x:
    case Left(_): 1
    case _: -1
"""
    )

    assertEquals(ifNormalized, matchNormalized)
  }

  test("test match removed from some examples") {
    checkLast("""
x = _ -> 1
""")(te => assertEquals(countMatch(te), 0))

    checkLast("""
x = 10
y = match x:
  case z: z
""")(te => assertEquals(countMatch(te), 0))

    checkLast("""
x = 10
y = match x:
  case _: 20
""")(te => assertEquals(countMatch(te), 0))
  }

  test("test let removed from some examples") {
    // this should turn into `y = 20` as the last expression
    checkLast("""
x = 10
y = match x:
  case _: 20
""")(te => assertEquals(countLet(te), 0))

    checkLast("""
foo = (
  x = 1
  _ = x
  42
)
""")(te => assertEquals(countLet(te), 0))
  }

  test("test normalization let shadowing bug in lambda") {
    checkLast("""
enum L[a]: E, NE(head: a, tail: L[a])

x = (
  def go(y, z):
    def loop(z):
      recur z:
        case E: y
        case NE(_, t): loop(t)

    loop(z)

  fn1 = z -> go(1, z)
  fn1(NE(1, NE(2, E)))
)
""") { te1 =>
      checkLast("""
enum L[a]: E, NE(head: a, tail: L[a])

x = (
  def go(y, z):
    def loop(z1):
      recur z1:
        case E: y
        case NE(_, t): loop(t)

    loop(z)

  fn1 = z -> go(1, z)
  fn1(NE(1, NE(2, E)))
)
    """) { te2 =>
        TestUtils.assertValid(te1)
        TestUtils.assertValid(te2)
        assert(
          te1.getType.sameAs(te2.getType),
          s"type mismatch:\n${te1.getType}\n\n!=\n\n${te2.getType}"
        )
        // `void` still includes bindable names, so alpha-equivalent locals
        // (`z` vs `z1`) remain unequal. Normalize that one binder spelling.
        assertEquals(
          te1.reprString.replace("z1", "z"),
          te2.reprString.replace("z1", "z"),
          s"\n${te1.reprString}\n\n!=\n\n${te2.reprString}"
        )
      }
    }
  }

  test("normalization rewrites non-escaping recursive local loop closures") {
    val fnName = Identifier.Name("fn")
    val (unoptimizedExpr, normalizedExpr) = inferUnoptimizedAndNormalizedExpr(
      """
enum L[a]: E, NE(head: a, tail: L[a])

x = (
  def go(y, z, fn):
    def loop(lst):
      recur lst:
        case E: y
        case NE(_, t): fn(loop(t))
    loop(z)

  go(1, NE(2, E), x -> x)
)
""",
      "x"
    )

    assert(
      countLambdaCapturing(unoptimizedExpr, fnName) > 0,
      unoptimizedExpr.reprString
    )
    assertEquals(
      countLambdaCapturing(normalizedExpr, fnName),
      0,
      normalizedExpr.reprString
    )
  }

  test("normalization keeps closures when recursive local loops escape") {
    val fnName = Identifier.Name("fn")
    val (unoptimizedExpr, normalizedExpr) = inferUnoptimizedAndNormalizedExpr(
      """
enum L[a]: E, NE(head: a, tail: L[a])

def makeLoop(fn):
  def loop(lst):
    recur lst:
      case E: 1
      case NE(_, t): fn(loop(t))
  loop
""",
      "makeLoop"
    )

    assert(
      countLambdaCapturing(unoptimizedExpr, fnName) > 0,
      unoptimizedExpr.reprString
    )
    assert(
      countLambdaCapturing(normalizedExpr, fnName) > 0,
      normalizedExpr.reprString
    )
  }

  test(
    "closure rewrite traverses lets loops and recur nodes in recursive bindings"
  ) {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val aName = Identifier.Name("a")
    val iName = Identifier.Name("i")
    val yName = Identifier.Name("y")
    val zName = Identifier.Name("z")
    val wName = Identifier.Name("w")
    val keepName = Identifier.Name("keep")
    val recName = Identifier.Name("r")
    val fnTpe = Type.Fun(NonEmptyList.one(intTpe), intTpe)

    val fVar = TypedExpr.Local(fName, fnTpe, ())
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val aVar = TypedExpr.Local(aName, intTpe, ())
    val iVar = TypedExpr.Local(iName, intTpe, ())

    val callFOnA = TypedExpr.App(fVar, NonEmptyList.one(aVar), intTpe, ())
    val callFOnI = TypedExpr.App(fVar, NonEmptyList.one(iVar), intTpe, ())
    val callFOnOne = TypedExpr.App(fVar, NonEmptyList.one(int(1)), intTpe, ())

    val defLoop = TypedExpr.Loop(
      NonEmptyList.one((iName, xVar)),
      TypedExpr.Recur(NonEmptyList.one(callFOnI), intTpe, ()),
      ()
    )

    val defBody = TypedExpr.Let(
      keepName,
      xVar,
      TypedExpr.Let(
        recName,
        TypedExpr.Local(recName, intTpe, ()),
        defLoop,
        RecursionKind.Recursive,
        ()
      ),
      RecursionKind.NonRecursive,
      ()
    )

    val defExpr = TypedExpr.Annotation(
      TypedExpr.AnnotatedLambda(NonEmptyList.one((aName, intTpe)), defBody, ()),
      fnTpe,
      None
    )

    val shadowNoChange =
      TypedExpr.Let(fName, int(5), int(6), RecursionKind.NonRecursive, ())
    val shadowChanged =
      TypedExpr.Let(fName, callFOnA, int(7), RecursionKind.NonRecursive, ())
    val shadowRecursive =
      TypedExpr.Let(fName, int(8), int(9), RecursionKind.Recursive, ())

    val annotationNoChange = TypedExpr.Annotation(int(10), intTpe, None)
    val genericNoChange = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(
        NonEmptyList.one((Type.Var.Bound("qa"), Kind.Type))
      ),
      int(11)
    )
    val lambdaNoChange =
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((Identifier.Name("u"), intTpe)),
        int(12),
        ()
      )
    val matchNoChange =
      TypedExpr.Match(
        int(0),
        NonEmptyList.one(branch(Pattern.WildCard, int(0))),
        ()
      )

    val inLoop = TypedExpr.Loop(
      NonEmptyList.one((iName, callFOnOne)),
      TypedExpr.Recur(NonEmptyList.one(callFOnI), intTpe, ()),
      ()
    )

    val inExpr = TypedExpr.Let(
      yName,
      shadowNoChange,
      TypedExpr.Let(
        zName,
        shadowChanged,
        TypedExpr.Let(
          wName,
          shadowRecursive,
          TypedExpr.Let(
            Identifier.Name("ann"),
            annotationNoChange,
            TypedExpr.Let(
              Identifier.Name("gen"),
              genericNoChange,
              TypedExpr.Let(
                Identifier.Name("lam"),
                lambdaNoChange,
                TypedExpr.Let(
                  Identifier.Name("mat"),
                  matchNoChange,
                  inLoop,
                  RecursionKind.NonRecursive,
                  ()
                ),
                RecursionKind.NonRecursive,
                ()
              ),
              RecursionKind.NonRecursive,
              ()
            ),
            RecursionKind.NonRecursive,
            ()
          ),
          RecursionKind.NonRecursive,
          ()
        ),
        RecursionKind.NonRecursive,
        ()
      ),
      RecursionKind.NonRecursive,
      ()
    )

    val root =
      TypedExpr.Let(fName, defExpr, inExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assertEquals(
      countLambdaCapturing(normalized.get, xName),
      0,
      normalized.get.reprString
    )
  }

  test("closure rewrite detects escaping references in loop initializers") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val aName = Identifier.Name("a")
    val iName = Identifier.Name("i")
    val fnTpe = Type.Fun(NonEmptyList.one(intTpe), intTpe)

    val fVar = TypedExpr.Local(fName, fnTpe, ())
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.one((iName, fVar)),
      TypedExpr
        .Recur(NonEmptyList.one(TypedExpr.Local(iName, fnTpe, ())), fnTpe, ()),
      ()
    )
    val recExpr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((aName, intTpe)),
      TypedExpr.Local(xName, intTpe, ()),
      ()
    )
    val root =
      TypedExpr.Let(fName, recExpr, loopExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assert(
      countLambdaCapturing(normalized.get, xName) > 0,
      normalized.get.reprString
    )
  }

  test("closure rewrite detects escaping references in recur arguments") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val aName = Identifier.Name("a")
    val iName = Identifier.Name("i")
    val fnTpe = Type.Fun(NonEmptyList.one(intTpe), intTpe)

    val fVar = TypedExpr.Local(fName, fnTpe, ())
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.one((iName, int(0))),
      TypedExpr.Recur(NonEmptyList.one(fVar), fnTpe, ()),
      ()
    )
    val recExpr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((aName, intTpe)),
      TypedExpr.Local(xName, intTpe, ()),
      ()
    )
    val root =
      TypedExpr.Let(fName, recExpr, loopExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assert(
      countLambdaCapturing(normalized.get, xName) > 0,
      normalized.get.reprString
    )
  }

  test("closure rewrite falls back when recursive binding is not a lambda") {
    val fName = Identifier.Name("f")
    val xName = Identifier.Name("x")
    val fnTpe = Type.Fun(NonEmptyList.one(intTpe), intTpe)
    val fVar = TypedExpr.Local(fName, fnTpe, ())
    val root = TypedExpr.Let(
      fName,
      TypedExpr.Local(xName, fnTpe, ()),
      TypedExpr.App(fVar, NonEmptyList.one(int(1)), intTpe, ()),
      RecursionKind.Recursive,
      ()
    )
    assert(TypedExprNormalization.normalize(root).nonEmpty)
  }

  test("toArgsBody always terminates") {
    forAll(Gen.choose(0, 10), genTypedExpr) { (arity, te) =>
      // this is a pretty weak test.
      assert(TypedExpr.toArgsBody(arity, te) ne null)
    }
  }

  test("TypedExpr.fold matches traverse") {
    def law[A, B](init: A, te: TypedExpr[B])(fn: (A, B) => A) = {
      type StateA[X] = State[A, X]
      val viaFold = te.foldLeft(init)(fn)
      val viaTraverse = te.traverse_[StateA, Unit] { b =>
        for {
          i <- State.get[A]
          i1 = fn(i, b)
          _ <- State.set(i1)
        } yield ()
      }

      assertEquals(viaFold, viaTraverse.runS(init).value, s"${te.repr}")
    }

    def lawR[A, B](te: TypedExpr[B], a: A)(fn: (B, A) => A) = {
      type StateA[X] = State[A, X]
      val viaFold = te
        .foldRight(cats.Eval.now(a))((b, r) => r.map(j => fn(b, j)))
        .value
      val viaTraverse: State[A, Unit] = te.traverse_[StateA, Unit] { b =>
        for {
          i <- State.get[A]
          i1 = fn(b, i)
          _ <- State.set(i1)
        } yield ()
      }

      assertEquals(viaFold, viaTraverse.runS(a).value, s"${te.repr}")
    }

    forAll(genTypedExprInt, Gen.choose(0, 1000)) { (te, init) =>
      // make a commutative int function
      law(init, te)((a, b) => (a + 1) * b)
      lawR(te, init)((a, b) => (a + 1) + b)
      law(init, te)((a, b) => a + b)
    }
  }

  test("foldMap from traverse matches foldMap") {
    import cats.data.Const
    import cats.Monoid

    def law[A, B: Monoid](te: TypedExpr[A])(fn: A => B) = {
      type ConstB[X] = Const[B, X]
      val viaFold = te.foldMap(fn)
      val viaTraverse: Const[B, Unit] = te
        .traverse[ConstB, Unit] { b =>
          Const[B, Unit](fn(b))
        }
        .void

      assertEquals(viaFold, viaTraverse.getConst, s"${te.repr}")
    }

    val propInt = forAll(genTypedExprChar, arbitrary[Char => Int])(law(_)(_))
    // non-commutative
    val propString =
      forAll(genTypedExprChar, arbitrary[Char => String])(law(_)(_))

    val lamconst: TypedExpr[String] =
      TypedExpr.AnnotatedLambda(
        NonEmptyList.one((Identifier.Name("x"), intTpe)),
        int(1).as("a"),
        "b"
      )

    assertEquals(lamconst.foldMap(identity), "ab")
    assertEquals(lamconst.traverse(a => Const[String, Unit](a)).getConst, "ab")
    Prop.all(propInt, propString)
  }

  test("TypedExpr.traverse.void matches traverse_") {
    import cats.data.Const
    forAll(genTypedExprInt, arbitrary[Int => String]) { (te, fn) =>
      assertEquals(
        te.traverse(i => Const[String, Unit](fn(i))).void,
        te.traverse_(i => Const[String, Unit](fn(i)))
      )
    }
  }

  test("TypedExpr.foldRight matches foldRight for commutative funs") {
    forAll(genTypedExprInt, Gen.choose(0, 1000)) { (te, init) =>
      val right =
        te.foldRight(cats.Eval.now(init))((i, ej) => ej.map(_ + i)).value
      val left = te.foldLeft(init)(_ + _)
      assertEquals(right, left)
    }
  }

  test("TypedExpr.foldRight matches foldRight for non-commutative funs") {
    forAll(genTypedExprInt) { te =>
      val right = te
        .foldRight(cats.Eval.now("")) { (i, ej) =>
          ej.map(j => i.toString + j)
        }
        .value
      val left = te.foldLeft("")((i, j) => i + j.toString)
      assertEquals(right, left)
    }
  }

  test("TypedExpr.map matches traverse with Id") {
    forAll(genTypedExprInt, arbitrary[Int => Int]) { (te, fn) =>
      assertEquals(te.map(fn), te.traverse[cats.Id, Int](fn))
    }
  }

  test("freeTyVars is a superset of the frees in the outer type") {
    forAll(genTypedExpr) { te =>
      assert(
        Type
          .freeTyVars(te.getType :: Nil)
          .toSet
          .subsetOf(
            te.freeTyVars.toSet
          )
      )
    }
  }

  test("TypedExpr.zonkMeta zonks quantifier evidence types") {
    val k = Kind.Type
    val m1 = Type.Meta(k, 1001L, existential = false, RefSpace.constRef(None))
    val m2 = Type.Meta(k, 1002L, existential = true, RefSpace.constRef(None))
    val a = Type.Var.Bound("a")
    val e = Type.Var.Bound("e")
    val qev = TypedExpr.QuantifierEvidence(
      sourceAtSolve = Type.TyMeta(m1),
      targetAtSolve = Type.TyMeta(m2),
      forallSolved = SortedMap(a -> (k, Type.TyMeta(m1))),
      existsHidden = SortedMap(e -> (k, Type.TyMeta(m2)))
    )
    val te: TypedExpr[Unit] = TypedExpr.Annotation(
      TypedExpr.Local(Identifier.Name("x"), Type.TyMeta(m1), ()),
      Type.TyMeta(m2),
      Some(qev)
    )

    val zonked = TypedExpr.zonkMeta[cats.Id, Unit](te) { m =>
      if (m.id == m1.id) Some(Type.Tau(Type.IntType))
      else if (m.id == m2.id) Some(Type.Tau(Type.StrType))
      else None
    }

    zonked match {
      case TypedExpr.Annotation(
            TypedExpr.Local(_, localTpe, _),
            coerce,
            Some(qev1)
          ) =>
        assertEquals(localTpe, Type.IntType)
        assertEquals(coerce, Type.StrType)
        assertEquals(qev1.sourceAtSolve, Type.IntType)
        assertEquals(qev1.targetAtSolve, Type.StrType)
        assertEquals(qev1.forallSolved.valuesIterator.map(_._2).toList, Type.IntType :: Nil)
        assertEquals(qev1.existsHidden.valuesIterator.map(_._2).toList, Type.StrType :: Nil)
      case other =>
        fail(s"expected annotation with evidence, got: ${other.reprString}")
    }

    assertEquals(Type.metaTvs(zonked.allTypes.toList), SortedSet.empty[Type.Meta])
  }

  test("TypedExpr.quantify handles metas and skolems inside quantifier evidence") {
    type S[A] = State[Map[Type.Meta, Type.Tau], A]
    val k = Kind.Type
    val mForall = Type.Meta(k, 2001L, existential = false, RefSpace.constRef(None))
    val mExists = Type.Meta(k, 2002L, existential = true, RefSpace.constRef(None))
    val exSkolem = Type.Var.Skolem("ex", k, existential = true, id = 99L)
    val qev = TypedExpr.QuantifierEvidence(
      sourceAtSolve = Type.TyMeta(mForall),
      targetAtSolve = Type.TyVar(exSkolem),
      forallSolved = SortedMap(Type.Var.Bound("fa") -> (k, Type.TyMeta(mForall))),
      existsHidden = SortedMap(
        Type.Var.Bound("ex") -> (k, Type.TyApply(Type.TyMeta(mExists), Type.TyVar(exSkolem)))
      )
    )
    val rho: TypedExpr.Rho[Unit] = TypedExpr.Rho.assertRho(
      TypedExpr.Annotation(
        TypedExpr.Literal(Lit.fromInt(1), Type.IntType, ()),
        Type.IntType,
        Some(qev)
      )
    )

    val readFn: Type.Meta => S[Option[Type.Tau]] = m => State.inspect(_.get(m))
    val writeFn: (Type.Meta, Type.Tau) => S[Unit] =
      (m, t) => State.modify(_.updated(m, t))

    val quantified =
      TypedExpr
        .quantify[S, Unit](
          Map.empty,
          rho,
          readFn,
          writeFn
        )
        .runA(Map.empty)
        .value

    assertEquals(Type.metaTvs(quantified.allTypes.toList), SortedSet.empty[Type.Meta])
    assert(
      !Type.freeTyVars(quantified.allTypes.toList).contains(exSkolem),
      quantified.reprString
    )

    quantified match {
      case TypedExpr.Generic(quant, _) =>
        assert(quant.forallList.nonEmpty)
        assert(quant.existList.nonEmpty)
      case other =>
        fail(s"expected quantified result, got: ${other.reprString}")
    }
  }

  test("instantiateTo reuses matching quantifier evidence") {
    val a = Type.Var.Bound("a")
    val b = Type.Var.Bound("b")
    val fromBody = Type.Tuple(List(Type.TyVar(a), Type.TyVar(b)))
    val target = Type.Tuple(List(Type.IntType, Type.IntType))
    val gen: TypedExpr.Generic[Unit] = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(
        NonEmptyList.of((a, Kind.Type), (b, Kind.Type))
      ),
      TypedExpr.Local(Identifier.Name("x"), fromBody, ())
    )

    val evidence = TypedExpr.QuantifierEvidence(
      sourceAtSolve = gen.getType,
      targetAtSolve = target,
      forallSolved = SortedMap(a -> (Kind.Type, Type.IntType)),
      existsHidden = SortedMap.empty
    )

    val instantiated =
      TypedExpr.instantiateTo(gen, target, _ => None, Some(evidence))

    instantiated match {
      case TypedExpr.Annotation(
            TypedExpr.Generic(quant, TypedExpr.Local(_, innerTpe, _)),
            coerce,
            Some(ev1)
          ) =>
        assertEquals(quant.forallList.map(_._1), List(b))
        assertEquals(
          innerTpe,
          Type.Tuple(List(Type.IntType, Type.TyVar(b)))
        )
        assertEquals(coerce, target)
        assertEquals(ev1, evidence)
      case other =>
        fail(s"expected annotation with reused evidence, got: ${other.reprString}")
    }
  }

  test("instantiateTo ignores non-matching evidence and solves directly") {
    val a = Type.Var.Bound("a")
    val gen: TypedExpr.Generic[Unit] = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      TypedExpr.Local(Identifier.Name("x"), Type.TyVar(a), ())
    )

    val wrongEvidence = TypedExpr.QuantifierEvidence(
      sourceAtSolve = Type.StrType,
      targetAtSolve = Type.IntType,
      forallSolved = SortedMap(a -> (Kind.Type, Type.StrType)),
      existsHidden = SortedMap.empty
    )

    val instantiated =
      TypedExpr.instantiateTo(gen, Type.IntType, _ => None, Some(wrongEvidence))

    instantiated match {
      case TypedExpr.Local(_, tpe, _) =>
        assertEquals(tpe, Type.IntType)
      case other =>
        fail(s"expected direct solved local, got: ${other.reprString}")
    }
  }

  test("instantiateTo falls back to annotation when no instantiation exists") {
    val a = Type.Var.Bound("a")
    val fnType = Type.Fun(Type.TyVar(a), Type.TyVar(a))
    val gen: TypedExpr.Generic[Unit] = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(NonEmptyList.one((a, Kind.Type))),
      TypedExpr.Local(Identifier.Name("f"), fnType, ())
    )

    val instantiated = TypedExpr.instantiateTo(gen, Type.IntType, _ => None, None)

    instantiated match {
      case TypedExpr.Annotation(term @ TypedExpr.Generic(_, _), coerce, None) =>
        assertEquals(coerce, Type.IntType)
        assert(term.getType.sameAs(gen.getType))
      case other =>
        fail(s"expected fallback annotation, got: ${other.reprString}")
    }
  }

  test("TestUtils.assertValid enforces annotation evidence targetAtSolve") {
    val qev = TypedExpr.QuantifierEvidence(
      sourceAtSolve = Type.IntType,
      targetAtSolve = Type.StrType,
      forallSolved = SortedMap.empty,
      existsHidden = SortedMap.empty
    )
    val te: TypedExpr[Unit] = TypedExpr.Annotation(
      TypedExpr.Literal(Lit.fromInt(1), Type.IntType, ()),
      Type.IntType,
      Some(qev)
    )

    val err = intercept[IllegalArgumentException] {
      TestUtils.assertValid(te)
    }
    assert(err.getMessage.contains("quantifier evidence invariant violated"))
  }

  test("TypedExpr.liftQuantification makes all args Rho types") {

    forAll(
      Generators.smallNonEmptyList(genTypedExpr, 10),
      Gen.containerOf[Set, Type.Var.Bound](NTypeGen.genBound)
    ) { (tes, avoid) =>
      val (optQuant, args) = TypedExpr.liftQuantification(tes, avoid)
      args.toList.foreach { te =>
        te.getType match {
          case _: Type.Rho => ()
          case notRho => fail(s"expected: $te to have rho type, got: $notRho")
        }
      }
      val allRhos = tes.forall(_.getType.isInstanceOf[Type.Rho])
      assertEquals(allRhos, optQuant.isEmpty)
    }
  }
}
