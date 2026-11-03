package dev.bosatsu

class ExprTraverseTypeStackOverflowReproTest extends munit.FunSuite {

  private def sourceWithManyDefs(defCount: Int, assertionCount: Int): String = {
    val defs =
      (0 until defCount).iterator
        .map(i => s"def f$i(x): x")
        .mkString("\n\n")

    val assertions =
      List.fill(assertionCount)("Assertion(True, \"ok\")").mkString(", ")

    s"""#
$defs

tests = TestSuite(\"repro\", [$assertions])
"""
  }

  private def runSourceConversionOnSmallStack(
      source: String,
      stackBytes: Long
  ): Option[Throwable] = {
    @volatile var failure: Option[Throwable] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            TestUtils.sourceConvertedProgramOf(source): Unit
            ()
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "source-converter-small-stack",
      stackBytes
    )

    thread.start()
    thread.join()
    failure
  }

  Platform.onJvm(
    test(
      "source conversion should not stack overflow on a moderately large list literal"
    ) {
      val defCount = sys.props.get("repro.defCount").fold(0)(_.toInt)
      val assertionCount =
        sys.props.get("repro.assertionCount").fold(200)(_.toInt)
      val stackBytes =
        sys.props.get("repro.stackBytes").fold(256L * 1024L)(_.toLong)

      val source =
        sourceWithManyDefs(defCount = defCount, assertionCount = assertionCount)
      val failure = runSourceConversionOnSmallStack(source, stackBytes)

      failure match {
        case Some(_: StackOverflowError) =>
          val trace = failure.get.getStackTrace
          val top =
            trace.iterator.take(20).map(_.toString).mkString("\n")
          val firstSourceConverterFrame =
            trace
              .find(_.getClassName.contains("SourceConverter"))
              .map(_.toString)
              .getOrElse("<none>")
          fail(
            s"SourceConverter overflowed while traversing expression types.\nTop stack:\n$top\nFirst SourceConverter frame: $firstSourceConverterFrame"
          )
        case Some(other) =>
          throw other
        case None =>
          ()
      }
    }
  )
}
