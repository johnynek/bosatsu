package dev.bosatsu.smt

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class SmtExprNormalizeAndPathImpliesTest extends munit.ScalaCheckSuite {
  import SmtExpr.*

  private val varNames: Vector[String] =
    Vector("x", "y", "z", "a", "b", "u", "v")

  private val intVarGen: Gen[IntExpr] =
    Gen.oneOf(varNames).map(Var[SmtSort.IntSort](_))

  private val intConstGen: Gen[IntExpr] =
    Gen.chooseNum(-30, 30).map(n => IntConst(BigInt(n)))

  private val boolConstGen: Gen[BoolExpr] =
    Gen.oneOf(true, false).map(BoolConst(_))

  private def intExprGen(depth: Int): Gen[IntExpr] = {
    val leaf = Gen.frequency(
      4 -> intConstGen,
      3 -> intVarGen
    )

    if (depth <= 0) leaf
    else {
      val recInt = intExprGen(depth - 1)
      val recBool = boolExprGen(depth - 1)

      val add2 = for {
        left <- recInt
        right <- recInt
      } yield Add(Vector(left, right))

      val addN = for {
        size <- Gen.choose(0, 4)
        args <- Gen.listOfN(size, recInt)
      } yield Add(args.toVector)

      val sub2 = for {
        left <- recInt
        right <- recInt
      } yield Sub(Vector(left, right))

      val mul2 = for {
        left <- recInt
        right <- recInt
      } yield Mul(Vector(left, right))

      val negMul = recInt.map(expr => Mul(Vector(IntConst(BigInt(-1)), expr)))

      val ite = for {
        cond <- recBool
        ifTrue <- recInt
        ifFalse <- recInt
      } yield Ite(cond, ifTrue, ifFalse)

      Gen.frequency(
        8 -> leaf,
        4 -> add2,
        2 -> addN,
        3 -> sub2,
        3 -> mul2,
        2 -> negMul,
        2 -> ite
      )
    }
  }

  private def boolExprGen(depth: Int): Gen[BoolExpr] = {
    val leaf = boolConstGen

    if (depth <= 0) leaf
    else {
      val recInt = intExprGen(depth - 1)
      val recBool = boolExprGen(depth - 1)

      val cmp = Gen.oneOf(
        for {
          l <- recInt
          r <- recInt
        } yield Lt(l, r),
        for {
          l <- recInt
          r <- recInt
        } yield Lte(l, r),
        for {
          l <- recInt
          r <- recInt
        } yield Gt(l, r),
        for {
          l <- recInt
          r <- recInt
        } yield Gte(l, r),
        for {
          l <- recInt
          r <- recInt
        } yield EqInt(l, r),
        for {
          l <- recBool
          r <- recBool
        } yield EqBool(l, r)
      )

      val andExpr = for {
        size <- Gen.choose(0, 4)
        args <- Gen.listOfN(size, recBool)
      } yield And(args.toVector)

      val orExpr = for {
        size <- Gen.choose(0, 4)
        args <- Gen.listOfN(size, recBool)
      } yield Or(args.toVector)

      val xorExpr = for {
        l <- recBool
        r <- recBool
      } yield Xor(l, r)

      val impliesExpr = for {
        l <- recBool
        r <- recBool
      } yield Implies(l, r)

      val iteExpr = for {
        c <- recBool
        t <- recBool
        f <- recBool
      } yield Ite(c, t, f)

      Gen.frequency(
        4 -> leaf,
        4 -> cmp,
        2 -> recBool.map(Not(_)),
        2 -> andExpr,
        2 -> orExpr,
        1 -> xorExpr,
        1 -> impliesExpr,
        1 -> iteExpr
      )
    }
  }

  private val envGen: Gen[Map[String, BigInt]] =
    Gen
      .listOfN(varNames.size, Gen.chooseNum(-20, 20))
      .map(values => varNames.iterator.zip(values.iterator.map(BigInt(_))).toMap)

  private def evalInt(expr: IntExpr, env: Map[String, BigInt]): BigInt =
    expr match {
      case IntConst(v) => v
      case Var(name)   => env.getOrElse(name, BigInt(0))
      case Add(args)   => args.iterator.map(evalInt(_, env)).sum
      case Sub(args)   =>
        args.toList match {
          case Nil          => BigInt(0)
          case head :: Nil  => -evalInt(head, env)
          case head :: tail => tail.foldLeft(evalInt(head, env))(_ - evalInt(_, env))
        }
      case Mul(args) =>
        args.toList match {
          case Nil  => BigInt(1)
          case list => list.map(evalInt(_, env)).product
        }
      case Div(_, _) | Mod(_, _) =>
        throw new IllegalArgumentException("generator excludes Div/Mod")
      case Ite(cond, ifTrue, ifFalse) =>
        if (evalBool(cond, env)) evalInt(ifTrue, env)
        else evalInt(ifFalse, env)
      case App(name, _) =>
        throw new IllegalArgumentException(s"generator excludes App: $name")
    }

  private def evalBool(expr: BoolExpr, env: Map[String, BigInt]): Boolean =
    expr match {
      case BoolConst(v)     => v
      case EqInt(l, r)      => evalInt(l, env) == evalInt(r, env)
      case EqBool(l, r)     => evalBool(l, env) == evalBool(r, env)
      case Lt(l, r)         => evalInt(l, env) < evalInt(r, env)
      case Lte(l, r)        => evalInt(l, env) <= evalInt(r, env)
      case Gt(l, r)         => evalInt(l, env) > evalInt(r, env)
      case Gte(l, r)        => evalInt(l, env) >= evalInt(r, env)
      case Not(inner)       => !evalBool(inner, env)
      case And(args)        => args.forall(evalBool(_, env))
      case Or(args)         => args.exists(evalBool(_, env))
      case Xor(l, r)        => evalBool(l, env) ^ evalBool(r, env)
      case Implies(l, r)    => !evalBool(l, env) || evalBool(r, env)
      case Ite(c, t, f)     => if (evalBool(c, env)) evalBool(t, env) else evalBool(f, env)
      case Var(name)        =>
        throw new IllegalArgumentException(s"unexpected bool var: $name")
      case App(name, _)     =>
        throw new IllegalArgumentException(s"generator excludes bool App: $name")
    }

  private def isNegatedTerm(expr: IntExpr): Boolean =
    expr match {
      case Mul(args) =>
        args.toList match {
          case IntConst(n) :: _ :: Nil if n == BigInt(-1) => true
          case _                                           => false
        }
      case IntConst(n) if n < 0 =>
        true
      case _ =>
        false
    }

  private def hasNegAddPair(expr: IntExpr): Boolean =
    expr match {
      case Add(args) =>
        val atNode =
          args.toList match {
            case left :: right :: Nil =>
              isNegatedTerm(left) || isNegatedTerm(right)
            case _ =>
              false
          }
        atNode || args.exists(hasNegAddPair)
      case Sub(args) =>
        args.exists(hasNegAddPair)
      case Mul(args) =>
        args.exists(hasNegAddPair)
      case Div(num, den) =>
        hasNegAddPair(num) || hasNegAddPair(den)
      case Mod(num, den) =>
        hasNegAddPair(num) || hasNegAddPair(den)
      case Ite(cond, ifTrue, ifFalse) =>
        hasNegAddPairInBool(cond) || hasNegAddPair(ifTrue) || hasNegAddPair(ifFalse)
      case _ =>
        false
    }

  private def hasNegAddPairInBool(expr: BoolExpr): Boolean =
    expr match {
      case EqInt(l, r)   => hasNegAddPair(l) || hasNegAddPair(r)
      case EqBool(l, r)  => hasNegAddPairInBool(l) || hasNegAddPairInBool(r)
      case Lt(l, r)      => hasNegAddPair(l) || hasNegAddPair(r)
      case Lte(l, r)     => hasNegAddPair(l) || hasNegAddPair(r)
      case Gt(l, r)      => hasNegAddPair(l) || hasNegAddPair(r)
      case Gte(l, r)     => hasNegAddPair(l) || hasNegAddPair(r)
      case Not(inner)    => hasNegAddPairInBool(inner)
      case And(args)     => args.exists(hasNegAddPairInBool)
      case Or(args)      => args.exists(hasNegAddPairInBool)
      case Xor(l, r)     => hasNegAddPairInBool(l) || hasNegAddPairInBool(r)
      case Implies(l, r) => hasNegAddPairInBool(l) || hasNegAddPairInBool(r)
      case Ite(c, t, f)  =>
        hasNegAddPairInBool(c) || hasNegAddPairInBool(t) || hasNegAddPairInBool(f)
      case _             => false
    }

  test("normalizeIntForSolver is idempotent") {
    forAll(intExprGen(5)) { expr =>
      val once = normalizeIntForSolver(expr)
      val twice = normalizeIntForSolver(once)
      assertEquals(twice, once)
    }
  }

  test("normalizeBoolForSolver is idempotent") {
    forAll(boolExprGen(5)) { expr =>
      val once = normalizeBoolForSolver(expr)
      val twice = normalizeBoolForSolver(once)
      assertEquals(twice, once)
    }
  }

  test("normalizeIntForSolver removes two-term add-with-negative patterns") {
    forAll(intExprGen(5)) { expr =>
      val normalized = normalizeIntForSolver(expr)
      assert(!hasNegAddPair(normalized))
    }
  }

  test("normalizeBoolForSolver removes two-term add-with-negative patterns from Int subterms") {
    forAll(boolExprGen(5)) { expr =>
      val normalized = normalizeBoolForSolver(expr)
      assert(!hasNegAddPairInBool(normalized))
    }
  }

  test("normalizeIntForSolver preserves evaluation semantics on generated Int expressions") {
    forAll(intExprGen(5), envGen) { (expr, env) =>
      val normalized = normalizeIntForSolver(expr)
      assertEquals(evalInt(normalized, env), evalInt(expr, env))
    }
  }

  test("normalizeBoolForSolver preserves evaluation semantics on generated Bool expressions") {
    forAll(boolExprGen(5), envGen) { (expr, env) =>
      val normalized = normalizeBoolForSolver(expr)
      assertEquals(evalBool(normalized, env), evalBool(expr, env))
    }
  }

  test("pathImplies holds when the goal fact is present") {
    forAll(boolExprGen(5), Gen.listOf(boolExprGen(4))) { (goal, extra) =>
      assert(pathImplies(goal, goal :: extra))
    }
  }

  test("pathImplies accepts stronger direct comparisons") {
    forAll(Gen.oneOf(varNames), Gen.chooseNum(-20, 20)) { (name, c) =>
      val left = Var[SmtSort.IntSort](name)
      val right = IntConst(BigInt(c))
      assert(pathImplies(Gte(left, right), List(Gt(left, right))))
      assert(pathImplies(Gte(left, right), List(EqInt(left, right))))
      assert(pathImplies(Lte(left, right), List(Lt(left, right))))
      assert(pathImplies(Lte(left, right), List(EqInt(left, right))))
    }
  }

  test("pathImplies proves decrement non-negativity from lower bounds for both canonical and legacy forms") {
    forAll(Gen.oneOf(varNames), Gen.chooseNum(1, 20)) { (name, by) =>
      val base = Var[SmtSort.IntSort](name)
      val byConst = IntConst(BigInt(by))
      val facts = List(Gte(base, byConst))
      val canonicalGoal = Gte(Sub(Vector(base, byConst)), IntConst(BigInt(0)))
      val legacyGoal =
        Gte(
          Add(
            Vector(
              base,
              Mul(Vector(IntConst(BigInt(-1)), byConst))
            )
          ),
          IntConst(BigInt(0))
        )
      assert(pathImplies(canonicalGoal, facts))
      assert(pathImplies(legacyGoal, facts))
    }
  }

  test("pathImplies proves strict decrease for positive constant decrements") {
    forAll(Gen.oneOf(varNames), Gen.chooseNum(1, 20)) { (name, by) =>
      val base = Var[SmtSort.IntSort](name)
      val byConst = IntConst(BigInt(by))
      assert(pathImplies(Lt(Sub(Vector(base, byConst)), base), Nil))
      assert(
        pathImplies(
          Lt(
            Add(Vector(base, Mul(Vector(IntConst(BigInt(-1)), byConst)))),
            base
          ),
          Nil
        )
      )
    }
  }

  test("pathImplies does not over-claim when the lower bound is insufficient") {
    forAll(Gen.oneOf(varNames), Gen.chooseNum(0, 20)) { (name, by) =>
      val base = Var[SmtSort.IntSort](name)
      val byConst = IntConst(BigInt(by))
      val facts = List(Gte(base, byConst))
      val tooLarge = IntConst(BigInt(by + 1))
      val goal = Gte(Sub(Vector(base, tooLarge)), IntConst(BigInt(0)))
      assert(!pathImplies(goal, facts))
    }
  }

  test("pathImplies is monotonic with respect to added facts") {
    forAll(boolExprGen(5), Gen.listOf(boolExprGen(4)), boolExprGen(4)) {
      (goal, facts, extraFact) =>
        val before = pathImplies(goal, facts)
        val after = pathImplies(goal, facts :+ extraFact)
        assert(!before || after)
    }
  }
}
