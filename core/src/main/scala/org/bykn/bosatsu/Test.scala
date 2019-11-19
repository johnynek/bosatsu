package org.bykn.bosatsu

import org.typelevel.paiges.Doc

sealed abstract class Test {
  def assertions: Int =
    Test.assertions(this)

  def failures: Option[Test] =
    this match {
      case Test.Assertion(true, _) => None
      case f@Test.Assertion(false, _) => Some(f)
      case Test.Suite(nm, ts) => {
        val innerFails = ts.flatMap(_.failures.toList)
        if (innerFails.isEmpty) None
        else Some(Test.Suite(nm, innerFails))
      }
    }

  def failureCount: Int = failures.fold(0)(_.assertions)
}

object Test {
  case class Assertion(value: Boolean, message: String) extends Test
  case class Suite(name: String, tests: List[Test]) extends Test

  def assertions(t: Test): Int = {
    @annotation.tailrec
    def go(ts: List[Test], acc: Int): Int =
      ts match {
        case Nil => acc
        case Assertion(_, _) :: tail =>
          go(tail, acc + 1)
        case Suite(_, test) :: tail =>
          go(test ::: tail, acc)
      }

    go(t :: Nil, 0)
  }

  private[this] val colonSpace = Doc.text(": ")
  private[this] val passed = Doc.text(" passed")
  private[this] val failed = Doc.text(" failed")
  private[this] val oneTest = Doc.text("1 test, ")

  def summary(passes: Int, fails: Int, c: LocationMap.Colorize): Doc = {
    @inline def failMsg = Doc.str(fails) + failed
    val total = passes + fails
    val tMsg = if (total == 1) oneTest else Doc.text(s"$total tests, ")
    tMsg + c.green(Doc.str(passes) + passed) + Doc.space +
      (if (fails > 0) c.red(failMsg) else failMsg)
  }

  def report(t: Test, c: LocationMap.Colorize): (Int, Int, Doc) = {

    val passDoc = c.green(Doc.text("pass"))
    val failDoc = c.red(Doc.text("fail"))

    def init(t: List[Test]): (Int, Int, Doc) =
      loop(t, None, 0, 0, Doc.empty)

    @annotation.tailrec
    def loop(ts: List[Test], lastSuite: Option[(Int, Int)], passes: Int, fails: Int, front: Doc): (Int, Int, Doc) =
      ts match {
        case Nil =>
          val sumDoc =
            lastSuite match {
              case Some((p, f)) if (p == passes) && (f == fails) => Doc.empty
              case _ =>
                Doc.line + summary(passes, fails, c)
            }
          (passes, fails, front + sumDoc)
        case Assertion(true, _) :: rest =>
          loop(rest, lastSuite, passes + 1, fails, front)
        case Assertion(false, label) :: rest =>
          loop(rest, lastSuite, passes, fails + 1, front + (Doc.line + Doc.text(label) + colonSpace + failDoc))
        case Suite(label, rest) :: tail =>
          val (p, f, d) = init(rest)
          val res = Doc.line + Doc.text(label) + Doc.char(':') + (Doc.lineOrSpace + d).nested(2)
          loop(tail, Some((p, f)), passes + p, fails + f, front + res)
      }

    init(t :: Nil)
  }
}
