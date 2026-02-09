package dev.bosatsu

import cats.data.{NonEmptyList, State, Writer}
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scala.collection.immutable.SortedSet

import Arbitrary.arbitrary
import Identifier.{Bindable, Constructor}
import TestUtils.checkLast
import rankn.{Type, NTypeGen}

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

    forAll(genTypedExpr)(law)

    checkLast("""
enum AB: A, B(x)
x = match B(100):
  case A: 10
  case B(b): b
""")(law)
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
          (pat, TypedExpr.Local(y, intT, tag)),
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

  def lit(l: Lit): TypedExpr[Unit] =
    TypedExpr.Literal(l, Type.getTypeOf(l), ())

  def int(i: Int): TypedExpr[Unit] =
    lit(Lit.fromInt(i))

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

  def hasLoop(te: TypedExpr[Unit]): Boolean =
    te match {
      case TypedExpr.Loop(_, _, _) =>
        true
      case TypedExpr.Generic(_, in) =>
        hasLoop(in)
      case TypedExpr.Annotation(in, _) =>
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
        hasLoop(arg) || branches.exists { case (_, b) => hasLoop(b) }
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
      case TypedExpr.Annotation(in, _) =>
        hasRecursiveLet(in)
      case TypedExpr.AnnotatedLambda(_, in, _) =>
        hasRecursiveLet(in)
      case TypedExpr.App(fn, args, _, _) =>
        hasRecursiveLet(fn) || args.exists(hasRecursiveLet)
      case TypedExpr.Loop(args, body, _) =>
        args.exists { case (_, init) => hasRecursiveLet(init) } || hasRecursiveLet(body)
      case TypedExpr.Recur(args, _, _) =>
        args.exists(hasRecursiveLet)
      case TypedExpr.Match(arg, branches, _) =>
        hasRecursiveLet(arg) || branches.exists { case (_, b) =>
          hasRecursiveLet(b)
        }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        false
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
    val expr = TypedExpr.App(PredefMul, NonEmptyList.of(opaque, int(1)), intTpe, ())

    def countExpr(te: TypedExpr[Unit], target: TypedExpr[Unit]): Int =
      te match {
        case t if t.void === target.void => 1
        case TypedExpr.Generic(_, in)   => countExpr(in, target)
        case TypedExpr.Annotation(in, _) => countExpr(in, target)
        case TypedExpr.AnnotatedLambda(_, in, _) => countExpr(in, target)
        case TypedExpr.App(fn, args, _, _) =>
          countExpr(fn, target) + args.toList.map(countExpr(_, target)).sum
        case TypedExpr.Let(_, ex, in, _, _) =>
          countExpr(ex, target) + countExpr(in, target)
        case TypedExpr.Loop(args, body, _) =>
          args.toList.map { case (_, init) => countExpr(init, target) }.sum + countExpr(
            body,
            target
          )
        case TypedExpr.Recur(args, _, _) =>
          args.toList.map(countExpr(_, target)).sum
        case TypedExpr.Match(arg, branches, _) =>
          countExpr(arg, target) + branches.toList.map { case (_, b) =>
            countExpr(b, target)
          }.sum
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) | TypedExpr.Literal(_, _, _) =>
          0
      }

    val normalized = TypedExprNormalization.normalize(expr)
    assert(normalized.nonEmpty)
    val norm = normalized.get
    assertEquals(norm.void, opaque.void)
    assert(countExpr(norm, opaque) <= 1, norm.reprString)
  }

  test("normalization evaluates constructor matches with named, annotation, and union patterns") {
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
        (namedPat, TypedExpr.Local(vName, intTpe, ())),
        (Pattern.WildCard, int(0))
      ),
      ()
    )

    assertEquals(TypedExprNormalization.normalize(matchExpr), Some(int(1)))
  }

  test("normalization prunes impossible constructor branches while keeping runtime matches") {
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
        (noMatchPat, int(-1)),
        (exactPat, int(1)),
        (Pattern.WildCard, int(0))
      ),
      ()
    )

    TypedExprNormalization.normalize(matchExpr) match {
      case Some(TypedExpr.Match(_, branches, _)) =>
        assertEquals(branches.length, 2)
        assertEquals(branches.head._1, exactPat)
      case other =>
        fail(s"expected branch-pruned match, got: $other")
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
        (Pattern.Literal(Lit.fromInt(0)), int(0)),
        (Pattern.WildCard, app(fVar, int(0), intTpe))
      ),
      ()
    )
    val fDef = TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), fBody, ())
    val useOnce =
      lam("g", fnType, app(varTE("g", fnType), int(1), intTpe))
    val root = letrec("f", fDef, app(useOnce, fVar, intTpe))

    val normed = TypedExprNormalization.normalize(root)
    assert(normed.isDefined)
    val norm = normed.get
    assert(hasLoop(norm), norm.reprString)
    assertEquals(hasRecursiveLet(norm), false)
    assertEquals(TypedExpr.allVarsSet(norm :: Nil).contains(fName), false)
  }

  test("normalizeAll lowers top-level recur defs to Loop and drops recursive kind") {
    TestUtils.checkPackageMap("""
enum List[a]: E, NE(head: a, tail: List[a])
enum B: T, F

def for_all(xs: List[a], fn: a -> B) -> B:
  recur xs:
    case E: T
    case NE(head, tail):
      match fn(head):
        case T: for_all(tail, fn)
        case F: F
    """) { pm =>
      val pack = pm.toMap(TestUtils.testPackage)
      val (name, rec, te) = pack.lets.find(_._1 == Identifier.Name("for_all")).get
      assertEquals(name, Identifier.Name("for_all"))
      assertEquals(rec, RecursionKind.NonRecursive)
      assert(hasLoop(te.void), te.reprString)
      assertEquals(hasRecursiveLet(te.void), false, te.reprString)
    }
  }

  test("normalization removes Loop when Recur is normalized away") {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val loopBody = TypedExpr.Match(
      int(0),
      NonEmptyList.of(
        (Pattern.Literal(Lit.fromInt(0)), xVar),
        (Pattern.WildCard, TypedExpr.Recur(NonEmptyList.one(xVar), intTpe, ()))
      ),
      ()
    )
    val loopExpr = TypedExpr.Loop(NonEmptyList.one((xName, int(1))), loopBody, ())
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
        (Pattern.Literal(Lit.fromInt(0)), xVar),
        (Pattern.WildCard, TypedExpr.Recur(NonEmptyList.of(xVar, yPlusOne), intTpe, ()))
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
              case (_, TypedExpr.Recur(args, _, _)) => args
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
        (Pattern.Literal(Lit.fromInt(0)), xVar),
        (Pattern.WildCard, TypedExpr.Recur(NonEmptyList.of(xVar, yVar), intTpe, ()))
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
              case (_, TypedExpr.Recur(args, _, _)) => args.length
            }
            assertEquals(recurArities, List(1))
          case other =>
            fail(s"expected normalized loop body match, got: ${other.repr}")
        }
      case None =>
        fail(s"expected lifted invariant with retained final loop arg, got: $normalized")
    }
  }

  test("normalization removes non-recursive identity let bindings") {
    val xName = Identifier.Name("x")
    val xVar = TypedExpr.Local(xName, intTpe, ())
    val wrappedX = TypedExpr.Annotation(xVar, intTpe)
    val body = TypedExpr.App(PredefAdd, NonEmptyList.of(xVar, int(1)), intTpe, ())
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
    val body = TypedExpr.App(PredefAdd, NonEmptyList.of(xVar, int(1)), intTpe, ())
    val identLet = TypedExpr.Let(xName, xVar, body, RecursionKind.NonRecursive, ())

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
        (
          Pattern.Var(xName): Pattern[(PackageName, Constructor), Type],
          TypedExpr.Local(xName, intTpe, ())
        )
      ),
      ()
    )
    val lam = TypedExpr.AnnotatedLambda(NonEmptyList.one((xName, intTpe)), body, ())
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
        case TypedExpr.Annotation(in, _) =>
          localTypesOf(in, target)
        case TypedExpr.AnnotatedLambda(_, in, _) =>
          localTypesOf(in, target)
        case TypedExpr.App(fn, args, _, _) =>
          localTypesOf(fn, target) ::: args.toList.flatMap(localTypesOf(_, target))
        case TypedExpr.Let(_, ex, in, _, _) =>
          localTypesOf(ex, target) ::: localTypesOf(in, target)
        case TypedExpr.Loop(args, in, _) =>
          args.toList.flatMap { case (_, init) => localTypesOf(init, target) } ::: localTypesOf(
            in,
            target
          )
        case TypedExpr.Recur(args, _, _) =>
          args.toList.flatMap(localTypesOf(_, target))
        case TypedExpr.Match(arg, branches, _) =>
          localTypesOf(arg, target) ::: branches.toList.flatMap { case (_, in) =>
            localTypesOf(in, target)
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) | TypedExpr.Literal(_, _, _) =>
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

  test("TypedExpr.substituteTypeVar is not an identity function") {
    // if we replace all the current types with some bound types, things won't be the same
    forAll(genTypedExpr) { te =>
      val tpes: Set[Type.Var] = te.allTypes.iterator.collect {
        case Type.TyVar(b) => b
      }.toSet

      // All the vars that are used in bounds
      val bounds: Set[Type.Var] = te
        .traverseType { (t: Type) =>
          Writer(SortedSet[Type.Var](Type.quantVars(t).map(_._1)*), t)
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
        assert(te1 =!= te, s"mapping: $identMap, $bounds")
      }
    }
  }

  test("TypedExpr.allTypes contains the type") {
    forAll(genTypedExpr) { te =>
      assert(te.allTypes.contains(te.getType))
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
    val duq = TypedExpr.Quantification.Dual(NonEmptyList.one(a), NonEmptyList.one(c))

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
    count(te) {
      case lam @ TypedExpr.AnnotatedLambda(_, _, _) =>
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
      case None =>
        fail(s"missing let: $letName in ${unoptProgram.lets.map(_._1)}")
    }
    val normalizedLets =
      TypedExprNormalization.normalizeAll(
        TestUtils.testPackage,
        unoptProgram.lets,
        fullTypeEnv
      )
    val normalizedExpr = normalizedLets.find(_._1 == targetName) match {
      case Some((_, _, te)) => te
      case None             =>
        fail(s"missing normalized let: $letName in ${normalizedLets.map(_._1)}")
    }
    (unoptimizedExpr, normalizedExpr)
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
        assertEquals(
          te1.void,
          te2.void,
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

    assert(countLambdaCapturing(unoptimizedExpr, fnName) > 0, unoptimizedExpr.reprString)
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

    assert(countLambdaCapturing(unoptimizedExpr, fnName) > 0, unoptimizedExpr.reprString)
    assert(countLambdaCapturing(normalizedExpr, fnName) > 0, normalizedExpr.reprString)
  }

  test("closure rewrite traverses lets loops and recur nodes in recursive bindings") {
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
      fnTpe
    )

    val shadowNoChange =
      TypedExpr.Let(fName, int(5), int(6), RecursionKind.NonRecursive, ())
    val shadowChanged =
      TypedExpr.Let(fName, callFOnA, int(7), RecursionKind.NonRecursive, ())
    val shadowRecursive =
      TypedExpr.Let(fName, int(8), int(9), RecursionKind.Recursive, ())

    val annotationNoChange = TypedExpr.Annotation(int(10), intTpe)
    val genericNoChange = TypedExpr.Generic(
      TypedExpr.Quantification.ForAll(
        NonEmptyList.one((Type.Var.Bound("qa"), Kind.Type))
      ),
      int(11)
    )
    val lambdaNoChange =
      TypedExpr.AnnotatedLambda(NonEmptyList.one((Identifier.Name("u"), intTpe)), int(12), ())
    val matchNoChange =
      TypedExpr.Match(int(0), NonEmptyList.one((Pattern.WildCard, int(0))), ())

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

    val root = TypedExpr.Let(fName, defExpr, inExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assertEquals(countLambdaCapturing(normalized.get, xName), 0, normalized.get.reprString)
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
      TypedExpr.Recur(NonEmptyList.one(TypedExpr.Local(iName, fnTpe, ())), fnTpe, ()),
      ()
    )
    val recExpr = TypedExpr.AnnotatedLambda(
      NonEmptyList.one((aName, intTpe)),
      TypedExpr.Local(xName, intTpe, ()),
      ()
    )
    val root = TypedExpr.Let(fName, recExpr, loopExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assert(countLambdaCapturing(normalized.get, xName) > 0, normalized.get.reprString)
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
    val root = TypedExpr.Let(fName, recExpr, loopExpr, RecursionKind.Recursive, ())
    val normalized = TypedExprNormalization.normalize(root)
    assert(normalized.nonEmpty)
    assert(countLambdaCapturing(normalized.get, xName) > 0, normalized.get.reprString)
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
    org.scalacheck.Prop.all(propInt, propString)
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
