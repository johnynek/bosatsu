package org.bykn.bosatsu

import scala.concurrent.ExecutionContext

object DirectEC {
  implicit val directEC: ExecutionContext =
    new ExecutionContext {
      def execute(r: Runnable) = r.run()
      def reportFailure(t: Throwable): Unit = throw t
    }
}
