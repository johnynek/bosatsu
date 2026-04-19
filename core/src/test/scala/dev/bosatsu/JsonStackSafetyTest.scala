package dev.bosatsu

class JsonStackSafetyTest extends munit.FunSuite {

  private def deeplyNestedArray(depth: Int): Json = {
    var json: Json = Json.JNull
    var idx = 0
    while (idx < depth) {
      json = Json.JArray(Vector(json))
      idx += 1
    }
    json
  }

  Platform.onJvm(
    test("Json.render should not stack overflow on deeply nested arrays") {
      val depth = sys.props.get("repro.jsonDepth").fold(1200)(_.toInt)
      val stackBytes = sys.props.get("repro.stackBytes").fold(96L * 1024L)(_.toLong)
      // Build parser-related state on a normal thread stack.
      // This test is focused on render stack-safety for deep Json values.
      val _ = Json.parser
      val json = deeplyNestedArray(depth)

      @volatile var failure: Option[Throwable] = None
      @volatile var rendered: Option[String] = None

      val thread = new Thread(
        null,
        new Runnable {
          def run(): Unit =
            try {
              rendered = Some(json.render)
            } catch {
              case t: Throwable =>
                failure = Some(t)
            }
        },
        "json-render-small-stack",
        stackBytes
      )

      thread.start()
      thread.join()

      failure match {
        case Some(so: StackOverflowError) =>
          val trace = so.getStackTrace.iterator.take(40).mkString("\n")
          fail(
            s"Json.render overflowed on nested array depth=$depth, stackBytes=$stackBytes\n$trace"
          )
        case Some(other) =>
          val trace = other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          assert(rendered.exists(_.nonEmpty))
      }
    }
  )
}
