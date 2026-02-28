package dev.bosatsu.smt

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class SmtExprRenderTest extends munit.ScalaCheckSuite {
  import SmtExpr._

  private val firstSymbolChars: Vector[Char] =
    (('a' to 'z') ++
      ('A' to 'Z') ++
      Seq('_', '~', '!', '@', '$', '%', '^', '&', '*', '+', '=', '<', '>', '.', '?', '/', '-'))
      .toVector

  private val restSymbolChars: Vector[Char] =
    (firstSymbolChars ++ ('0' to '9')).toVector

  private val escapedSymbolChars: Vector[Char] =
    (('a' to 'z') ++ ('0' to '9') ++ Seq(' ', ':', '#', ',', '[', ']', '(', ')', '|')).toVector

  private val bareSymbolGen: Gen[String] =
    for {
      head <- Gen.oneOf(firstSymbolChars)
      tailSize <- Gen.choose(0, 10)
      tail <- Gen.listOfN(tailSize, Gen.oneOf(restSymbolChars))
    } yield (head +: tail).mkString

  private val escapedSymbolGen: Gen[String] =
    for {
      size <- Gen.choose(1, 12)
      chars <- Gen.listOfN(size, Gen.oneOf(escapedSymbolChars))
      str = chars.mkString.trim
      if str.nonEmpty
    } yield str

  private val symbolGen: Gen[String] =
    Gen.frequency(
      4 -> bareSymbolGen,
      1 -> escapedSymbolGen
    )

  private def vectorGen[A](min: Int, max: Int, gen: Gen[A]): Gen[Vector[A]] =
    for {
      size <- Gen.choose(min, max)
      items <- Gen.listOfN(size, gen)
    } yield items.toVector

  private val intConstGen: Gen[IntExpr] =
    Gen.chooseNum(-1000000L, 1000000L).map(v => IntConst(BigInt(v)))

  private val boolConstGen: Gen[BoolExpr] =
    Gen.oneOf(true, false).map(BoolConst(_))

  private val intVarGen: Gen[IntExpr] =
    symbolGen.map(Var[SmtSort.IntSort](_))

  private val boolVarGen: Gen[BoolExpr] =
    symbolGen.map(Var[SmtSort.BoolSort](_))

  private val intAppLeafGen: Gen[IntExpr] =
    symbolGen.map(name => App[SmtSort.IntSort](name, Vector.empty))

  private val boolAppLeafGen: Gen[BoolExpr] =
    symbolGen.map(name => App[SmtSort.BoolSort](name, Vector.empty))

  private def widen[S <: SmtSort](gen: Gen[SmtExpr[S]]): Gen[SmtExpr[?]] =
    gen.map(expr => expr: SmtExpr[?])

  private def anyExprGen(depth: Int): Gen[SmtExpr[?]] =
    Gen.oneOf(widen(intExprGen(depth)), widen(boolExprGen(depth)))

  private def intExprGen(depth: Int): Gen[IntExpr] = {
    val leaf = Gen.frequency(
      4 -> intConstGen,
      3 -> intVarGen,
      1 -> intAppLeafGen
    )

    if (depth <= 0) leaf
    else {
      val intRec = intExprGen(depth - 1)
      val boolRec = boolExprGen(depth - 1)
      val anyRec = anyExprGen(depth - 1)

      val addGen = vectorGen(0, 4, intRec).map(Add(_))
      val subGen = vectorGen(0, 4, intRec).map(Sub(_))
      val mulGen = vectorGen(0, 4, intRec).map(Mul(_))
      val divGen = for {
        num <- intRec
        den <- intRec
      } yield Div(num, den)
      val modGen = for {
        num <- intRec
        den <- intRec
      } yield Mod(num, den)
      val iteGen = for {
        cond <- boolRec
        ifTrue <- intRec
        ifFalse <- intRec
      } yield Ite(cond, ifTrue, ifFalse)
      val appGen = for {
        name <- symbolGen
        args <- vectorGen(0, 3, anyRec)
      } yield App[SmtSort.IntSort](name, args)

      Gen.frequency(
        8 -> leaf,
        2 -> addGen,
        2 -> subGen,
        2 -> mulGen,
        1 -> divGen,
        1 -> modGen,
        2 -> iteGen,
        2 -> appGen
      )
    }
  }

  private def boolExprGen(depth: Int): Gen[BoolExpr] = {
    val leaf = Gen.frequency(
      4 -> boolConstGen,
      3 -> boolVarGen,
      1 -> boolAppLeafGen
    )

    if (depth <= 0) leaf
    else {
      val intRec = intExprGen(depth - 1)
      val boolRec = boolExprGen(depth - 1)
      val anyRec = anyExprGen(depth - 1)

      val ltGen = for {
        left <- intRec
        right <- intRec
      } yield Lt(left, right)
      val lteGen = for {
        left <- intRec
        right <- intRec
      } yield Lte(left, right)
      val gtGen = for {
        left <- intRec
        right <- intRec
      } yield Gt(left, right)
      val gteGen = for {
        left <- intRec
        right <- intRec
      } yield Gte(left, right)
      val eqIntGen = for {
        left <- intRec
        right <- intRec
      } yield EqInt(left, right)
      val eqBoolGen = for {
        left <- boolRec
        right <- boolRec
      } yield EqBool(left, right)

      val cmpGen = Gen.oneOf(ltGen, lteGen, gtGen, gteGen, eqIntGen, eqBoolGen)

      val notGen = boolRec.map(Not(_))
      val andGen = vectorGen(0, 4, boolRec).map(And(_))
      val orGen = vectorGen(0, 4, boolRec).map(Or(_))
      val xorGen = for {
        left <- boolRec
        right <- boolRec
      } yield Xor(left, right)
      val impliesGen = for {
        left <- boolRec
        right <- boolRec
      } yield Implies(left, right)
      val iteGen = for {
        cond <- boolRec
        ifTrue <- boolRec
        ifFalse <- boolRec
      } yield Ite(cond, ifTrue, ifFalse)
      val appGen = for {
        name <- symbolGen
        args <- vectorGen(0, 3, anyRec)
      } yield App[SmtSort.BoolSort](name, args)

      Gen.frequency(
        8 -> leaf,
        3 -> cmpGen,
        2 -> notGen,
        2 -> andGen,
        2 -> orGen,
        1 -> xorGen,
        1 -> impliesGen,
        2 -> iteGen,
        2 -> appGen
      )
    }
  }

  private def assertRoundTrip[S <: SmtSort](expr: SmtExpr[S]): Unit = {
    val direct = SmtToSExpr.expr(expr)
    val rendered = SmtLibRender.renderExpr(expr).render(120)
    assertEquals(SExprParser.parseAll(rendered), Right(Vector(direct)))
  }

  test("int expressions: SmtExpr=>SExpr equals Doc=>String=>SExpr") {
    forAll(intExprGen(5)) { expr =>
      assertRoundTrip(expr)
    }
  }

  test("bool expressions: SmtExpr=>SExpr equals Doc=>String=>SExpr") {
    forAll(boolExprGen(5)) { expr =>
      assertRoundTrip(expr)
    }
  }
}
