package dev.bosatsu.smt

import dev.bosatsu.PredefImpl
import dev.bosatsu.scalawasiz3.{Z3Platform, Z3Result}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

/** `SmtExpr.bosatsuDiv` and `bosatsuMod` must mean exactly Bosatsu's `div`
  * and `mod_Int` under SMT-LIB semantics, or the recursion check could prove
  * a false termination goal.
  */
class BosatsuDivModTest extends munit.ScalaCheckSuite {
  import SmtExpr.*
  import LinearBoundsGens.{Env, bigIntGen, byZeroChoices, evalInt}

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (dev.bosatsu.Platform.isJvm) 2000 else 300
    )

  // Bosatsu's documented semantics, stated independently of PredefImpl:
  // Python's floor division, with x / 0 == 0 and x % 0 == x
  private def specDiv(x: BigInt, d: BigInt): BigInt =
    if (d == 0) BigInt(0)
    else {
      val q = x / d // truncates toward zero
      if ((x % d != 0) && ((x < 0) != (d < 0))) q - 1 else q
    }

  private def specMod(x: BigInt, d: BigInt): BigInt =
    if (d == 0) x else x - d * specDiv(x, d)

  private def predefDiv(x: BigInt, d: BigInt): BigInt =
    BigInt(PredefImpl.divBigInteger(x.bigInteger, d.bigInteger))

  private def predefMod(x: BigInt, d: BigInt): BigInt =
    BigInt(PredefImpl.modBigInteger(x.bigInteger, d.bigInteger))

  private val x = Var[SmtSort.IntSort]("x")
  private val d = Var[SmtSort.IntSort]("d")

  // the numerator and divisor as variables or as constants, in every
  // combination, since the lowering treats a constant divisor specially
  private def shapes(xv: BigInt, dv: BigInt): List[(IntExpr, IntExpr)] =
    for {
      num <- List[IntExpr](x, IntConst(xv))
      den <- List[IntExpr](d, IntConst(dv))
    } yield (num, den)

  private def mismatches(xv: BigInt, dv: BigInt): List[String] = {
    val env0 = Map("x" -> xv, "d" -> dv)
    for {
      (num, den) <- shapes(xv, dv)
      bz <- byZeroChoices
      env = Env(env0, bz)
      (name, term, expected) <- List(
        ("div", bosatsuDiv(num, den), predefDiv(xv, dv)),
        ("mod_Int", bosatsuMod(num, den), predefMod(xv, dv))
      )
      got = evalInt(term, env)
      if got != expected
    } yield s"$xv $name $dv: expected $expected, SMT-LIB term $term gives $got"
  }

  test("PredefImpl div and mod_Int are Python's floor division, with x / 0 == 0 and x % 0 == x") {
    forAll(bigIntGen, bigIntGen) { (xv, dv) =>
      Prop(predefDiv(xv, dv) == specDiv(xv, dv) && predefMod(xv, dv) == specMod(xv, dv)) :|
        s"$xv, $dv: ${predefDiv(xv, dv)}, ${predefMod(xv, dv)}"
    }
  }

  test("the spec matches Python on fixed cases") {
    // values from Python 3: (x // d, x % d)
    val cases = List(
      (7, 3, 2, 1),
      (-7, 3, -3, 2),
      (7, -3, -3, -2),
      (-7, -3, 2, -1),
      (5, -3, -2, -1),
      (6, -3, -2, 0),
      (-6, 3, -2, 0),
      (0, -5, 0, 0),
      (1, -1, -1, 0),
      (-1, 2, -1, 1)
    )
    cases.foreach { case (xv, dv, q, r) =>
      assertEquals(specDiv(xv, dv), BigInt(q), s"$xv // $dv")
      assertEquals(specMod(xv, dv), BigInt(r), s"$xv % $dv")
    }
    assertEquals(specDiv(5, 0), BigInt(0))
    assertEquals(specMod(5, 0), BigInt(5))
  }

  property("bosatsuDiv and bosatsuMod evaluate to Bosatsu's div and mod_Int") {
    forAll(bigIntGen, bigIntGen) { (xv, dv) =>
      val bad = mismatches(xv, dv)
      Prop(bad.isEmpty) :| bad.mkString("\n")
    }
  }

  test("bosatsuDiv and bosatsuMod agree with Bosatsu on every small numerator and divisor") {
    for {
      xv <- -30 to 30
      dv <- -9 to 9
    } assertEquals(mismatches(xv, dv), Nil)
  }

  private def divisors(e: SmtExpr[?]): List[IntExpr] =
    e match {
      case Div(n, den)  => den :: divisors(n) ::: divisors(den)
      case Mod(n, den)  => den :: divisors(n) ::: divisors(den)
      case Add(args)    => args.toList.flatMap(divisors)
      case Sub(args)    => args.toList.flatMap(divisors)
      case Mul(args)    => args.toList.flatMap(divisors)
      case Ite(c, a, b) => divisors(c) ::: divisors(a) ::: divisors(b)
      case EqInt(a, b)  => divisors(a) ::: divisors(b)
      case Gt(a, b)     => divisors(a) ::: divisors(b)
      case _            => Nil
    }

  private def hasIte(e: SmtExpr[?]): Boolean =
    e match {
      case Ite(_, _, _) => true
      case Div(n, den)  => hasIte(n) || hasIte(den)
      case Mod(n, den)  => hasIte(n) || hasIte(den)
      case Add(args)    => args.exists(hasIte)
      case Sub(args)    => args.exists(hasIte)
      case Mul(args)    => args.exists(hasIte)
      case _            => false
    }

  property("a constant divisor gives no ite, and SMT-LIB div and mod only by a positive constant") {
    forAll(bigIntGen) { dv =>
      val terms = List(bosatsuDiv(x, IntConst(dv)), bosatsuMod(x, IntConst(dv)))
      Prop(terms.forall { t =>
        !hasIte(t) && divisors(t).forall {
          case IntConst(k) => k > 0
          case _           => false
        }
      }) :| terms.mkString("\n")
    }
  }

  test("a variable divisor only reaches SMT-LIB div and mod as d or -d, under a guard") {
    List(bosatsuDiv(x, d), bosatsuMod(x, d)).foreach { t =>
      t match {
        case Ite(EqInt(`d`, IntConst(zero)), _, Ite(Gt(`d`, IntConst(zero1)), pos, neg)) =>
          assertEquals(zero, BigInt(0))
          assertEquals(zero1, BigInt(0))
          assertEquals(divisors(pos), List(d))
          assertEquals(divisors(neg), List(Sub(Vector(IntConst(BigInt(0)), d))))
        case other =>
          fail(s"unexpected shape: $other")
      }
    }
  }

  // An independent check of the SMT-LIB side, with the real solver instead of
  // the test evaluator.
  private val z3Solver = Z3Platform.create()

  private def z3Unsat(decls: List[String], assertion: BoolExpr): Unit = {
    val script = SmtScript(
      Vector(SmtCommand.SetLogic.QF_LIA) ++
        decls.map(SmtCommand.DeclareConst(_, SmtSort.IntS)) ++
        Vector(SmtCommand.Assert(assertion), SmtCommand.CheckSat)
    )
    val smt2 = SmtLibRender.renderScript(script)
    z3Solver.runSmt2(smt2) match {
      case Z3Result.Success(stdout, _, _) =>
        assertEquals(stdout.trim, "unsat", smt2)
      case Z3Result.Failure(msg, _, stdout, stderr, _) =>
        fail(s"z3 failed: $msg\n$stdout\n$stderr\n$smt2")
    }
  }

  test("z3 agrees with Bosatsu on every small constant numerator and divisor") {
    val mismatches =
      for {
        xv <- (-12 to 12).toVector
        dv <- (-6 to 6).toVector
        (term, expected) <- Vector(
          bosatsuDiv(IntConst(xv), IntConst(dv)) -> predefDiv(xv, dv),
          bosatsuMod(IntConst(xv), IntConst(dv)) -> predefMod(xv, dv)
        )
      } yield Not(EqInt(term, IntConst(expected)))
    z3Unsat(Nil, Or(mismatches))
  }

  test("z3 proves bosatsuDiv and bosatsuMod by a constant are floor division, for every x") {
    // q and r are floor division of x by k exactly when x == k * q + r and r
    // is between 0 and k, on k's side of 0
    for (k <- (-9 to 9) if k != 0) {
      val q = bosatsuDiv(x, IntConst(k))
      val r = bosatsuMod(x, IntConst(k))
      val rInRange =
        if (k > 0) And(Vector(Gte(r, IntConst(0)), Lt(r, IntConst(k))))
        else And(Vector(Lte(r, IntConst(0)), Gt(r, IntConst(k))))
      val defn = And(Vector(EqInt(x, Add(Vector(Mul(Vector(IntConst(k), q)), r))), rInRange))
      z3Unsat(List("x"), Not(defn))
    }
    // and by 0, x div 0 == 0 and x mod_Int 0 == x
    assertEquals(bosatsuDiv(x, IntConst(0)), IntConst(BigInt(0)))
    assertEquals(bosatsuMod(x, IntConst(0)), x)
  }
}
