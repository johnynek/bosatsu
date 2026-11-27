package dev.bosatsu.smt

import dev.bosatsu.Predef

import IntDecide.Result

/** Replays real obligations of the recursion check (see the headers of the
  * files in core/src/test/resources/int_obligations for where they come from)
  * and checks that IntDecide and z3 agree on every one.
  *
  * IntDecide replaced z3 in the compiler, so a goal z3 proves that IntDecide
  * doesn't is a program that compiled before and is rejected now.
  */
class IntObligationCorpusTest extends munit.FunSuite {
  import IntObligationReader.Obligation

  private val testWorkspace: String =
    Predef.loadFileInCompile(
      "core/src/test/resources/int_obligations/test_workspace.smt2"
    )
  private val recursionCheckTest: String =
    Predef.loadFileInCompile(
      "core/src/test/resources/int_obligations/recursion_check_test.smt2"
    )

  private def obligations(
      text: String
  )(using munit.Location): Vector[Obligation] =
    IntObligationReader.parseAll(text) match {
      case Right(os) => os
      case Left(err) => fail(err)
    }

  private def check(name: String, text: String, expectedCount: Int)(using
      munit.Location
  ): Unit = {
    val os = obligations(text)
    // a reader that dropped lines would make this pass vacuously
    assertEquals(os.size, expectedCount, s"obligations read from $name")
    os.foreach { o =>
      val decided =
        IntDecide.implies(o.goal, o.facts, IntDecide.DefaultWorkLimit)
      val z3 = Z3Oracle.implies(o.goal, o.facts, o.ints, o.bools)
      val clue =
        s"$name\nfacts: ${o.facts.map(SmtLibRender.renderExpr(_).render(120))}\n" +
          s"goal: ${SmtLibRender.renderExpr(o.goal).render(120)}\nIntDecide: $decided\nz3: $z3"
      (decided, z3) match {
        case (Result.Proved, Right(Some(true)))      => ()
        case (Result.Refuted(m), Right(Some(false))) =>
          assert(o.facts.forall(IntDecide.evalBool(_, m).contains(true)), clue)
          assertEquals(IntDecide.evalBool(o.goal, m), Some(false), clue)
        // z3 can't decide division by a variable, which some obligations
        // have; IntDecide reads it as an unknown, so its proofs stay sound
        case (Result.Proved | Result.Unknown(_), Right(None) | Left(_)) => ()
        case _ => fail(s"IntDecide and z3 disagree\n$clue")
      }
    }
  }

  test("IntDecide and z3 agree on every obligation from test_workspace") {
    check("test_workspace", testWorkspace, 28)
  }

  test("IntDecide proves every obligation from test_workspace, which compiles") {
    obligations(testWorkspace).foreach { o =>
      assertEquals(
        IntDecide.implies(o.goal, o.facts, IntDecide.DefaultWorkLimit),
        Result.Proved,
        SmtLibRender.renderExpr(o.goal).render(120)
      )
    }
  }

  test(
    "IntDecide and z3 agree on every obligation from TypedExprRecursionCheckTest"
  ) {
    check("TypedExprRecursionCheckTest", recursionCheckTest, 163)
  }

  test("the reader gives back what SmtToSExpr renders") {
    (testWorkspace + "\n" + recursionCheckTest).linesIterator
      .filter(_.startsWith("(obligation"))
      .foreach { line =>
        val o = obligations(line).head
        val rendered =
          s"(facts${o.facts.map(f => " " + SmtLibRender.renderExpr(f).render(100000)).mkString}) " +
            s"(goal ${SmtLibRender.renderExpr(o.goal).render(100000)}))"
        assert(line.endsWith(rendered), s"$line\n$rendered")
      }
  }
}
