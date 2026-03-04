package dev.bosatsu.smt

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class SmtScriptScopeTest extends munit.ScalaCheckSuite {
  import SmtCommand.*
  import SmtExpr.*

  private val intNames: Vector[String] = Vector("i0", "i1", "i2", "i3")
  private val boolNames: Vector[String] = Vector("b0", "b1", "b2", "b3")

  private val intVarGen: Gen[IntExpr] =
    Gen.oneOf(intNames).map(Var[SmtSort.IntSort](_))

  private val boolVarGen: Gen[BoolExpr] =
    Gen.oneOf(boolNames).map(Var[SmtSort.BoolSort](_))

  private val intConstGen: Gen[IntExpr] =
    Gen.chooseNum(-20, 20).map(n => IntConst(BigInt(n)))

  private val boolConstGen: Gen[BoolExpr] =
    Gen.oneOf(true, false).map(BoolConst(_))

  private def intExprGen(depth: Int): Gen[IntExpr] = {
    val leaf = Gen.frequency(3 -> intConstGen, 2 -> intVarGen)
    if (depth <= 0) leaf
    else {
      val recI = intExprGen(depth - 1)
      val recB = boolExprGen(depth - 1)
      Gen.frequency(
        5 -> leaf,
        2 -> Gen.zip(recI, recI).map { case (l, r) => Add(Vector(l, r)) },
        2 -> Gen.zip(recI, recI).map { case (l, r) => Sub(Vector(l, r)) },
        1 -> Gen.zip(recI, recI).map { case (l, r) => Mul(Vector(l, r)) },
        1 -> Gen.zip(recI, recI).map { case (l, r) => Div(l, r) },
        1 -> Gen.zip(recI, recI).map { case (l, r) => Mod(l, r) },
        1 -> Gen.zip(recB, Gen.zip(recI, recI)).map { case (c, (t, f)) => Ite(c, t, f) }
      )
    }
  }

  private def boolExprGen(depth: Int): Gen[BoolExpr] = {
    val leaf = Gen.frequency(3 -> boolConstGen, 2 -> boolVarGen)
    if (depth <= 0) leaf
    else {
      val recI = intExprGen(depth - 1)
      val recB = boolExprGen(depth - 1)
      Gen.frequency(
        5 -> leaf,
        2 -> Gen.zip(recI, recI).map { case (l, r) => Lt(l, r) },
        2 -> Gen.zip(recI, recI).map { case (l, r) => Gte(l, r) },
        2 -> Gen.zip(recI, recI).map { case (l, r) => EqInt(l, r) },
        1 -> Gen.zip(recB, recB).map { case (l, r) => EqBool(l, r) },
        1 -> recB.map(Not(_)),
        1 -> Gen.zip(recB, recB).map { case (l, r) => And(Vector(l, r)) },
        1 -> Gen.zip(recB, recB).map { case (l, r) => Or(Vector(l, r)) },
        1 -> Gen.zip(recB, recB).map { case (l, r) => Implies(l, r) },
        1 -> Gen.zip(recB, Gen.zip(recB, recB)).map { case (c, (t, f)) => Ite(c, t, f) }
      )
    }
  }

  private case class UsedVars(ints: Set[String], bools: Set[String]) {
    def ++(that: UsedVars): UsedVars =
      UsedVars(ints ++ that.ints, bools ++ that.bools)
  }
  private object UsedVars {
    val empty: UsedVars = UsedVars(Set.empty, Set.empty)
  }

  private def varsInExpr(expr: SmtExpr[?]): UsedVars =
    expr match {
      case IntConst(_) | BoolConst(_) =>
        UsedVars.empty
      case Var(name) =>
        if (intNames.contains(name)) UsedVars(Set(name), Set.empty)
        else if (boolNames.contains(name)) UsedVars(Set.empty, Set(name))
        else UsedVars(Set(name), Set.empty)
      case App(_, args)     => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Ite(c, t, f)     => varsInExpr(c) ++ varsInExpr(t) ++ varsInExpr(f)
      case Add(args)        => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Sub(args)        => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Mul(args)        => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Div(num, den)    => varsInExpr(num) ++ varsInExpr(den)
      case Mod(num, den)    => varsInExpr(num) ++ varsInExpr(den)
      case EqInt(l, r)      => varsInExpr(l) ++ varsInExpr(r)
      case EqBool(l, r)     => varsInExpr(l) ++ varsInExpr(r)
      case Lt(l, r)         => varsInExpr(l) ++ varsInExpr(r)
      case Lte(l, r)        => varsInExpr(l) ++ varsInExpr(r)
      case Gt(l, r)         => varsInExpr(l) ++ varsInExpr(r)
      case Gte(l, r)        => varsInExpr(l) ++ varsInExpr(r)
      case Not(inner)       => varsInExpr(inner)
      case And(args)        => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Or(args)         => args.iterator.foldLeft(UsedVars.empty)((acc, e) => acc ++ varsInExpr(e))
      case Xor(l, r)        => varsInExpr(l) ++ varsInExpr(r)
      case Implies(l, r)    => varsInExpr(l) ++ varsInExpr(r)
    }

  private def varsInBool(expr: BoolExpr): UsedVars =
    varsInExpr(expr)

  test("recognizes local define-fun arguments as in scope") {
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        DefineFun(
          "inc",
          Vector("x" -> SmtSort.IntS),
          SmtSort.IntS,
          Add(Vector(Var[SmtSort.IntSort]("x"), IntConst(BigInt(1))))
        ),
        Assert(
          EqInt(
            App[SmtSort.IntSort]("inc", Vector(IntConst(BigInt(1)))),
            IntConst(BigInt(2))
          )
        ),
        CheckSat
      )
    )

    assertEquals(SmtScriptScope.undeclaredVars(script), Set.empty)
  }

  test("reports undeclared variables used before declaration order-wise") {
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        Assert(Gt(Var[SmtSort.IntSort]("x"), IntConst(BigInt(0)))),
        DeclareConst("x", SmtSort.IntS),
        CheckSat
      )
    )

    assertEquals(SmtScriptScope.undeclaredVars(script), Set("x"))
  }

  test("all declared vars in generated assertions produce empty undeclared set") {
    forAll(boolExprGen(4)) { expr =>
      val vars = varsInBool(expr)
      val iVars = vars.ints.toList.sorted
      val bVars = vars.bools.toList.sorted
      val script = SmtScript(
        Vector(SetLogic("QF_LIA")) ++
          iVars.map(DeclareConst(_, SmtSort.IntS)) ++
          bVars.map(DeclareConst(_, SmtSort.BoolS)) ++
          Vector(Assert(expr), CheckSat)
      )

      assertEquals(SmtScriptScope.undeclaredVars(script), Set.empty)
    }
  }

  test("dropping one declaration in generated assertions is detected") {
    forAll(boolExprGen(4)) { expr =>
      val vars = varsInBool(expr)
      val iVars = vars.ints
      val bVars = vars.bools
      val all = (iVars ++ bVars).toList.sorted
      val dropped = all.headOption

      val script =
        dropped match {
          case Some(droppedName) =>
            val iDecls =
              iVars.toList.sorted
                .filterNot(_ == droppedName)
                .map(DeclareConst(_, SmtSort.IntS))
            val bDecls =
              bVars.toList.sorted
                .filterNot(_ == droppedName)
                .map(DeclareConst(_, SmtSort.BoolS))
            SmtScript(
              Vector(SetLogic("QF_LIA")) ++
                iDecls ++
                bDecls ++
                Vector(Assert(expr), CheckSat)
            )
          case None       =>
            SmtScript(Vector(SetLogic("QF_LIA"), Assert(expr), CheckSat))
        }

      val actual = SmtScriptScope.undeclaredVars(script)
      dropped match {
        case Some(droppedName) => assert(actual.contains(droppedName))
        case None       => assertEquals(actual, Set.empty)
      }
    }
  }
}
