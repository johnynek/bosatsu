package org.bykn.bosatsu

import org.typelevel.paiges.Doc

sealed abstract class Test
object Test {
  case class Assert(value: Boolean) extends Test
  case class TestList(tests: List[Test]) extends Test
  case class Label(label: String, test: Test) extends Test

  def assertions(t: Test): Int = {
    @annotation.tailrec
    def go(ts: List[Test], acc: Int): Int =
      ts match {
        case Nil => acc
        case Assert(_) :: tail =>
          go(tail, acc + 1)
        case TestList(rest) :: tail =>
          go(rest ::: tail, acc)
        case Label(_, test) :: tail =>
          go(test :: tail, acc)
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
        case Assert(true) :: tail =>
          loop(tail, passes + 1, fails, front)
        case Assert(false) :: tail =>
          loop(tail, passes, fails + 1, front)
        case TestList(rest) :: tail =>
          loop(rest ::: tail, passes, fails, front)
        case Label(label, Assert(true)) :: rest =>
          loop(rest, passes + 1, fails, front + (Doc.line + Doc.text(label) + colonSpace + passDoc))
        case Label(label, Assert(false)) :: rest =>
          loop(rest, passes, fails + 1, front + (Doc.line + Doc.text(label) + colonSpace + failDoc))
        case Label(label, Label(_, test)) :: rest =>
          // we are overwriting a label
          loop(Label(label, test) :: rest, passes, fails, front)
        case Label(label, TestList(t :: Nil)) :: tail =>
          loop(Label(label, t) :: tail, passes, fails, front)
        case Label(label, TestList(rest)) :: tail =>
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
