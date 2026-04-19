package dev.bosatsu

class SourceConverterBindingStackSafetyTest extends munit.FunSuite {
  private val region: Region = Region.empty

  private def deepBindingBlock(depth: Int): Declaration.NonBinding = {
    def nameAt(idx: Int): Identifier.Bindable =
      Identifier.Name(s"x$idx")

    var current: Declaration = Declaration.Var(nameAt(depth - 1))(using region)
    var idx = depth - 1

    while (idx >= 0) {
      val value: Declaration.NonBinding =
        if (idx == 0) Declaration.Literal(Lit.fromInt(0))(using region)
        else Declaration.Var(nameAt(idx - 1))(using region)

      current = Declaration.Binding(
        BindingStatement(
          Pattern.Var(nameAt(idx)),
          value,
          Padding(1, current)
        )
      )(using region)

      idx = idx - 1
    }

    Declaration.Parens(current)(using region)
  }

  private def runSourceConversionOnSmallStack(
      depth: Int,
      stackBytes: Long
  ): Option[Throwable] = {
    val topLevel = Statement.Bind(
      BindingStatement(
        Pattern.Var(Identifier.Name("result")),
        deepBindingBlock(depth),
        ()
      )
    )(region)

    @volatile var failure: Option[Throwable] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            SourceConverter.toProgram(
              TestUtils.testPackage,
              Nil,
              topLevel :: Nil
            ): Unit
            ()
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "source-converter-binding-chain-small-stack",
      stackBytes
    )

    thread.start()
    thread.join()
    failure
  }

  Platform.onJvm(
    test("source conversion should not stack overflow on deep binding chains") {
      val depth = sys.props.get("repro.sourceBindingDepth").fold(800)(_.toInt)
      val stackBytes =
        sys.props.get("repro.stackBytes").fold(192L * 1024L)(_.toLong)

      val failure =
        runSourceConversionOnSmallStack(depth, stackBytes)

      failure match {
        case Some(so: StackOverflowError) =>
          val trace = so.getStackTrace.iterator.take(40).mkString("\n")
          fail(
            s"SourceConverter overflowed on a deep binding chain (depth=$depth, stackBytes=$stackBytes)\n$trace"
          )
        case Some(other) =>
          val trace = other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          ()
      }
    }
  )
}
