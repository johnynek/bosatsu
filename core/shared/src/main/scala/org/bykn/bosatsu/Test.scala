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

  def report(t: Test): (Int, Int, Doc) = {

    val colonSpace = Doc.text(": ")
    val passDoc = green("pass")
    val failDoc = red("fail")

    def init(t: List[Test]): (Int, Int, Doc) =
      loop(t, 0, 0, Doc.empty)

    def summary(passes: Int, fails: Int): Doc = {
      val failMsg = s"$fails failed"
      val total = passes + fails
      val tMsg = if (total == 1) "1 test, " else s"$total tests, "
      Doc.text(tMsg) + green(s"$passes passed") + Doc.space +
        (if (fails > 0) red(failMsg) else Doc.text(failMsg))
    }

    @annotation.tailrec
    def loop(ts: List[Test], passes: Int, fails: Int, front: Doc): (Int, Int, Doc) =
      ts match {
        case Nil => (passes, fails, front + Doc.line + summary(passes, fails))
        case Assertion(true, _) :: rest =>
          loop(rest, passes + 1, fails, front)
        case Assertion(false, label) :: rest =>
          loop(rest, passes, fails + 1, front + (Doc.line + Doc.text(label) + colonSpace + failDoc))
        case Suite(label, rest) :: tail =>
          // Now we have at least two tests
          val (p, f, d) = init(rest)
          val res = Doc.line + Doc.text(label) + Doc.char(':') + (Doc.lineOrSpace + d).nested(2)
          loop(tail, passes + p, fails + f, front + res)
      }

    init(t :: Nil)
  }

  private[this] val greenDoc = Doc.text(Console.GREEN)
  private[this] val redDoc = Doc.text(Console.RED)
  private[this] val resetDoc = Doc.text(Console.RESET)

  private def green(s: String): Doc =
    greenDoc + Doc.text(s) + resetDoc

  private def red(s: String): Doc =
    redDoc + Doc.text(s) + resetDoc
}
